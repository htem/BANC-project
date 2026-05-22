#!/usr/bin/env python3
"""Upload the 277 BANC all-to-all influence shards to the Dataverse as
individual files (one Dataverse file per chunk_NNNN.parquet).

The shards live at
gs://lee-lab_brain-and-nerve-cord-fly-connectome/compiled_data/banc_888/influence/all_to_all/.
Each is ~1 GB. The shape and the algorithm are documented at
documentation/influence_all_to_all.md; THIS script applies the same
metadata to every shard, with a per-shard description suffix
("chunk N of 277").

Workflow per shard
------------------

  gsutil cp gs://.../chunk_NNNN.parquet /tmp/scratch/
  curl POST /api/datasets/:persistentId/add  (file + jsonData)
  rm /tmp/scratch/chunk_NNNN.parquet
  log success → upload_log.csv  (so a re-run resumes from the last failure)

Disk footprint is bounded to ~1 GB while running.

Usage
-----

    python3 upload_all_to_all.py --dry-run       # list shards; do not upload
    python3 upload_all_to_all.py --max 5         # upload only the first 5 shards
    python3 upload_all_to_all.py --skip-existing # resume from log
    python3 upload_all_to_all.py                 # the real deal — ~5 h at 1 GB / 5 s

API key & dataset come from the same conventions as upload.py.
"""
from __future__ import annotations
import argparse, csv, hashlib, json, os, re, subprocess, sys, tempfile, time
from datetime import datetime, timezone
from pathlib import Path
from urllib.parse import quote

import yaml

# Reuse the direct-S3 path from upload.py so this script benefits from the
# same battle-tested multipart + register-with-addFiles flow. Direct-S3 is
# the only Dataverse upload route that guarantees atomic, byte-for-byte
# storage (no archive extraction, no tabular ingest, no MIME sniffing on
# the server side) — see the "/add" failure case documented in the
# direct-S3 commit and in DATAVERSE_TODO.md.
from upload import upload_direct_s3

HERE = Path(__file__).parent
DOC  = HERE / "documentation" / "influence_all_to_all.md"
LOG  = HERE / "upload_log.csv"
DEFAULT_PERSISTENT_ID = "doi:10.7910/DVN/7WTH1N"
DEFAULT_SERVER = "https://dataverse.harvard.edu"
GCS_PREFIX = "gs://lee-lab_brain-and-nerve-cord-fly-connectome/compiled_data/banc_888/influence/all_to_all/"
DEFAULT_DIRLABEL = "influence/all_to_all"

API_FIELDS = ("description", "categories", "directoryLabel", "restrict", "tabIngest")


def log(*a, **kw): print(*a, **kw, file=sys.stderr, flush=True)


def parse_frontmatter(path: Path) -> dict:
    raw = path.read_text()
    parts = re.split(r"(?m)^---\s*$", raw, maxsplit=2)
    if len(parts) < 3:
        raise ValueError(f"{path.name}: no frontmatter")
    return yaml.safe_load(parts[1])


def list_shards(gcs_prefix: str) -> list[tuple[str, str]]:
    """Return list of (gcs_url, basename) for every shard."""
    out = subprocess.check_output(["gsutil", "ls", gcs_prefix], text=True)
    shards = []
    for line in out.splitlines():
        line = line.strip()
        if line.endswith(".parquet"):
            shards.append((line, line.split("/")[-1]))
    shards.sort()
    return shards


def per_shard_jsondata(base_fm: dict, basename: str, idx: int, total: int) -> str:
    base_desc = base_fm["description"]
    # Tail-truncate the long all_to_all description so the per-shard one is
    # still informative without duplicating the full prose 277 times.
    short = (base_desc[:400].rsplit(" ", 1)[0] + " ...") if len(base_desc) > 400 else base_desc
    per_shard = (f"{short} (Shard {idx} of {total}: {basename}.) "
                 "Schema: upstream_id, downstream_id, raw_influence — "
                 "as documented in documentation/influence_all_to_all.md.")
    api = {
        "description": per_shard,
        "categories": base_fm.get("categories", ["Data", "Influence"]),
        "directoryLabel": base_fm.get("directoryLabel", DEFAULT_DIRLABEL),
        "restrict": "false",
        "tabIngest": "false",
    }
    return json.dumps(api)


def md5_of(p: Path) -> str:
    h = hashlib.md5()
    with p.open("rb") as f:
        for chunk in iter(lambda: f.read(1 << 20), b""):
            h.update(chunk)
    return h.hexdigest()


def read_log() -> set[str]:
    """Return the set of deposit filenames marked OK in upload_log.csv."""
    if not LOG.exists(): return set()
    with LOG.open() as f:
        return {r["deposit_filename"] for r in csv.DictReader(f)
                if r["status"] == "OK"}


def live_filenames(key: str, server: str, persistent_id: str) -> set[str]:
    """Filenames currently in the draft (any directoryLabel). Used as a
    second-line resume check on top of upload_log.csv — protects against
    log/draft divergence (manual deletes, log truncation, etc.)."""
    pid_q = quote(persistent_id, safe=":/")
    cmd = [
        "curl", "-sS",
        "-H", f"X-Dataverse-key:{key}",
        "-H", "User-Agent: Mozilla/5.0 (BANC-Project all-to-all uploader)",
        f"{server}/api/datasets/:persistentId/versions/:draft/files?persistentId={pid_q}",
    ]
    try:
        raw = subprocess.check_output(cmd, text=True, timeout=60)
        d = json.loads(raw)
    except Exception as e:
        log(f"  [live-check] could not fetch draft file list: {e}")
        return set()
    out = set()
    for f in d.get("data", []):
        df = f.get("dataFile", {})
        if df.get("filename"):
            out.add(df["filename"])
    return out


LOG_FIELDS = ("when", "doc_md", "deposit_filename", "md5", "size_bytes",
              "dataset_doi", "data_file_id", "status")


def append_log(row: dict):
    exists = LOG.exists()
    with LOG.open("a", newline="") as f:
        w = csv.DictWriter(f, fieldnames=LOG_FIELDS)
        if not exists: w.writeheader()
        w.writerow(row)


def upload_shard(gcs_url: str, basename: str, jsonData: str,
                 key: str, server: str, persistent_id: str,
                 scratch: Path, dry_run: bool) -> dict:
    """Stream one shard GCS → local scratch → direct-S3 → register, then
    delete the local copy. Memory footprint is bounded to one part of the
    multipart upload at a time (Dataverse hands out 1 GiB parts by
    default; our shards average 1.04 GiB so most are 2-part multipart),
    plus the local on-disk copy in `scratch` (~1 GiB). Peak RAM per
    shard ≈ 1 GiB; peak disk ≈ 1 GiB."""
    if dry_run:
        log(f"  [dry-run] {basename}")
        return {"data_file_id": "", "status": "dry-run", "md5": "",
                "size_bytes": 0}

    local = scratch / basename
    log(f"  gsutil cp ...")
    subprocess.check_call(["gsutil", "-q", "cp", gcs_url, str(local)])
    sz = local.stat().st_size
    md5 = md5_of(local)
    log(f"  uploading ({sz/1e6:.0f} MB md5={md5[:8]})")

    # The per-shard JSON encodes the API fields (description,
    # categories, etc.) — parse it back into a dict for upload_direct_s3,
    # which expects a frontmatter-style mapping. Force content_type to
    # application/octet-stream so the register step records the file as
    # binary, not as a parquet that some Dataverse instance might try to
    # peek at.
    fm = json.loads(jsonData)
    fm["content_type"] = "application/octet-stream"

    try:
        res = upload_direct_s3(
            fm=fm,
            doc_name=DOC.name,
            src_path=local,
            deposit_name=basename,
            size_bytes=sz,
            md5_hex=md5,
            key=key,
            server=server,
            persistent_id=persistent_id,
            scratch=scratch,
        )
    finally:
        # Always reclaim disk so the script never wedges from full /tmp.
        try:
            local.unlink()
        except FileNotFoundError:
            pass

    return {"data_file_id": res.get("data_file_id", ""),
            "status": "OK", "md5": md5, "size_bytes": sz}


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--dry-run", action="store_true")
    ap.add_argument("--max", type=int,
                    help="upload only the first N shards (smoke testing)")
    ap.add_argument("--start-at", type=int, default=1,
                    help="1-indexed shard to start from (default 1)")
    ap.add_argument("--skip-existing", action="store_true",
                    help="skip shards already in upload_log.csv")
    ap.add_argument("--persistent-id", default=DEFAULT_PERSISTENT_ID)
    ap.add_argument("--server", default=DEFAULT_SERVER)
    ap.add_argument("--key-file", default=os.path.expanduser("~/.dataverse_api_key"))
    ap.add_argument("--scratch", default="/tmp/dataverse_upload_a2a")
    args = ap.parse_args()

    key = os.environ.get("DATAVERSE_API_KEY")
    if not key:
        kp = Path(args.key_file)
        if not kp.exists():
            log(f"no API key at {kp}"); sys.exit(2)
        key = kp.read_text().strip()

    scratch = Path(args.scratch)
    scratch.mkdir(parents=True, exist_ok=True)

    base_fm = parse_frontmatter(DOC)
    shards = list_shards(GCS_PREFIX)
    total = len(shards)
    log(f"[a2a] {total} shards at {GCS_PREFIX}")

    # Two independent resume checks: the upload_log.csv journal (fast,
    # local) and the live draft file list (slow, network). The script
    # ALWAYS does the live check on startup so a manually-pruned log
    # never produces duplicate Dataverse files; --skip-existing makes
    # the journal a second gate.
    done = read_log() if args.skip_existing else set()
    log(f"[a2a] {len(done)} OK rows in upload_log.csv")
    live = live_filenames(key, args.server, args.persistent_id)
    log(f"[a2a] {len(live)} files currently in the live draft")

    chosen = shards[args.start_at - 1:]
    if args.max:
        chosen = chosen[:args.max]
    log(f"[a2a] uploading up to {len(chosen)} shards "
        f"(start_at={args.start_at}, max={args.max})")

    t_session = time.monotonic()
    sz_session = 0
    for i, (gcs_url, basename) in enumerate(chosen, args.start_at):
        log(f"[{i:>3}/{total}] {basename}")
        jsonData = per_shard_jsondata(base_fm, basename, i, total)
        if basename in done:
            log("  skip (already in log)"); continue
        if basename in live:
            log("  skip (already in live draft)"); continue
        t0 = time.monotonic()
        try:
            res = upload_shard(gcs_url, basename, jsonData,
                               key, args.server, args.persistent_id,
                               scratch, args.dry_run)
        except Exception as e:
            log(f"  ERROR: {type(e).__name__}: {e}")
            if not args.dry_run:
                append_log({
                    "when": datetime.now(timezone.utc).isoformat(),
                    "doc_md": DOC.name,
                    "deposit_filename": basename,
                    "md5": "", "size_bytes": "",
                    "dataset_doi": args.persistent_id,
                    "data_file_id": "",
                    "status": f"error: {type(e).__name__}",
                })
            continue
        dt = time.monotonic() - t0
        sz_session += int(res["size_bytes"]) if not args.dry_run else 0
        log(f"  → OK ({res['data_file_id']})  {dt:.0f}s "
            f"({(res['size_bytes'] or 0)/1024**2/dt:.0f} MB/s)")
        if not args.dry_run:
            append_log({
                "when": datetime.now(timezone.utc).isoformat(),
                "doc_md": DOC.name,
                "deposit_filename": basename,
                "md5": res["md5"],
                "size_bytes": res["size_bytes"],
                "dataset_doi": args.persistent_id,
                "data_file_id": res["data_file_id"],
                "status": "OK",
            })

    # Session-level summary.
    dt = time.monotonic() - t_session
    if sz_session:
        log(f"[a2a] session done: {sz_session/1024**3:.1f} GiB in "
            f"{dt/3600:.2f} h ({sz_session/1024**2/dt:.0f} MB/s)")


if __name__ == "__main__":
    main()
