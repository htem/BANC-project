#!/usr/bin/env python3
"""Drive Dataverse uploads from the per-file documentation in documentation/.

For each .md in documentation/, parse the YAML frontmatter, fetch the file
bytes (from local_path or gcs_path), and POST it to the Dataverse `add`
endpoint with the API metadata in jsonData. The Dataverse dataset is
identified by `--persistent-id` (default: doi:10.7910/DVN/7WTH1N).

Usage
-----

    # Dry-run: print what would be uploaded, but do not POST.
    python3 upload.py --dry-run

    # Upload one file by doc name (smoke test):
    python3 upload.py --only acknowledgements.md

    # Upload all files in a given directoryLabel:
    python3 upload.py --dir compiled_data

    # Upload everything not already in the dataset (skips files whose
    # md5 already matches a deposit entry).
    python3 upload.py --skip-existing

API key
-------

Read from `~/.dataverse_api_key` (chmod 600). NEVER paste into the repo.
Override with `--key-file PATH` or env var `DATAVERSE_API_KEY`.

Upload mechanics
----------------

The upload uses curl invoked as a subprocess (the urllib path gets
403-blocked by the Dataverse WAF; `requests` works but adds a dep).
jsonData is written to a tempfile to avoid shell-quoting issues with
nested quotes in descriptions.

For files > 100 MB the upload still goes through the standard /add
endpoint; Harvard Dataverse handles ~3 GB through this path. If you
need to upload very large files (the all-to-all influence shards or
the mesh ZIPs > 5 GB), switch to direct-S3 upload (TODO: see
`upload_direct_s3` below — stub for now).

Resumability
------------

Successful uploads are recorded in `upload_log.csv` alongside this script.
Re-running with the same arguments will skip any doc whose
(deposit_filename, file_hash, dataset_doi) matches a previous entry.
"""
from __future__ import annotations
import argparse, csv, hashlib, json, os, re, shutil, subprocess, sys, tempfile
import time
from datetime import datetime, timezone
from pathlib import Path
from urllib.parse import quote

import yaml

HERE = Path(__file__).parent
DOCS = HERE / "documentation"
LOG  = HERE / "upload_log.csv"
DEFAULT_PERSISTENT_ID = "doi:10.7910/DVN/7WTH1N"
DEFAULT_SERVER = "https://dataverse.harvard.edu"

API_FIELDS = ("description", "categories", "directoryLabel", "restrict", "tabIngest")


def log(*a, **kw):
    print(*a, **kw, file=sys.stderr, flush=True)


# --- frontmatter -----------------------------------------------------------

def parse_frontmatter(path: Path) -> dict:
    raw = path.read_text()
    parts = re.split(r"(?m)^---\s*$", raw, maxsplit=2)
    if len(parts) < 3:
        raise ValueError(f"{path.name}: no YAML frontmatter")
    fm = yaml.safe_load(parts[1])
    if not isinstance(fm, dict):
        raise ValueError(f"{path.name}: frontmatter is not a mapping")
    return fm


def build_jsondata(fm: dict) -> str:
    api = {}
    for k in API_FIELDS:
        if k not in fm:
            raise ValueError(f"missing API field: {k}")
        v = fm[k]
        if k in ("restrict", "tabIngest") and isinstance(v, bool):
            v = "true" if v else "false"
        api[k] = v
    return json.dumps(api)


# --- file source resolution ------------------------------------------------

def stage_file(fm: dict, doc_name: str, scratch: Path,
               fetch: bool = True,
               staged_dir: Path | None = None) -> tuple[Path | None, str, int | None]:
    """Return (local-path-to-bytes-or-None, deposit-filename, size_bytes-or-None).

    Resolves a file from the doc's frontmatter:

      - `local_path:`         → use directly
      - `gcs_path:` (gs://…)  → `gsutil cp` to scratch when fetch=True;
                                otherwise `gsutil stat` to confirm reachable
                                and return (None, name, size_bytes)
      - `dataverse_filename:` → preferred name on Dataverse; else use basename
    """
    local = fm.get("local_path")
    gcs = fm.get("gcs_path")
    deposit_name = fm.get("dataverse_filename") or fm.get("filename")
    if not deposit_name:
        if local:
            deposit_name = Path(local).name
        elif gcs:
            deposit_name = Path(gcs).name
        else:
            raise ValueError(f"{doc_name}: no filename / local_path / gcs_path")
    # Prefer a pre-packaged file in the staging dir (produced by package.py)
    if staged_dir:
        staged = staged_dir / deposit_name
        if staged.exists() and staged.is_file():
            return staged, deposit_name, staged.stat().st_size
    if local and Path(local).exists() and Path(local).is_file():
        return Path(local), deposit_name, Path(local).stat().st_size
    if gcs:
        gs_url = gcs if gcs.startswith("gs://") else f"gs://{gcs}"
        is_dir = gs_url.endswith("/")
        if not fetch:
            # Dry-run reachability check. `gsutil stat` only works on
            # objects, not directory prefixes (2026-05-21 bug fix — it
            # threw FileNotFoundError on every legitimate directory like
            # influence/all_to_all/, neuron_meshes/, registrations/...).
            # We now use `gsutil ls -d` which works for both cases:
            # an existing object echoes its own URL; an existing prefix
            # echoes the prefix URL.
            try:
                out = subprocess.check_output(
                    ["gsutil", "ls", "-d", gs_url], text=True,
                    stderr=subprocess.STDOUT, timeout=30,
                )
            except subprocess.CalledProcessError as e:
                raise FileNotFoundError(
                    f"{doc_name}: gsutil ls failed: {e.output[:200]}"
                )
            except subprocess.TimeoutExpired:
                raise FileNotFoundError(f"{doc_name}: gsutil ls timed out")
            if not out.strip():
                raise FileNotFoundError(
                    f"{doc_name}: gsutil ls returned nothing for {gs_url}"
                )
            # Object: try `gsutil stat` for the byte size; directory: rely
            # on `size_bytes` in the frontmatter (the doc author has
            # authority for directory totals).
            if is_dir:
                return None, deposit_name, fm.get("size_bytes")
            try:
                stat_out = subprocess.check_output(
                    ["gsutil", "stat", gs_url], text=True,
                    stderr=subprocess.STDOUT, timeout=30,
                )
                m = re.search(r"Content-Length:\s*(\d+)", stat_out)
                return None, deposit_name, int(m.group(1)) if m else fm.get("size_bytes")
            except (subprocess.CalledProcessError, subprocess.TimeoutExpired):
                return None, deposit_name, fm.get("size_bytes")
        if is_dir:
            # Directory deposits (mesh ZIPs, all_to_all shards) are
            # produced by package.py and consumed via the staged_dir
            # path. If we reach here in fetch mode there's nothing to
            # gsutil-copy from a prefix as a single file.
            raise FileNotFoundError(
                f"{doc_name}: gcs_path is a directory ({gs_url}); build a "
                f"deposit ZIP via package.py or supply a staged file."
            )
        out_path = scratch / deposit_name
        log(f"  fetching {gs_url}")
        subprocess.check_call(["gsutil", "-q", "cp", gs_url, str(out_path)])
        return out_path, deposit_name, out_path.stat().st_size
    # Directory-only deposits (e.g. the all_to_all shard set) are not
    # uploaded by this script — they need per-shard iteration.
    if fm.get("nshards"):
        return None, deposit_name, fm.get("size_bytes")
    raise FileNotFoundError(f"{doc_name}: cannot stage — no resolvable source")


def file_md5(path: Path) -> str:
    h = hashlib.md5()
    with path.open("rb") as f:
        for chunk in iter(lambda: f.read(1 << 20), b""):
            h.update(chunk)
    return h.hexdigest()


# --- log / resumability ----------------------------------------------------

LOG_FIELDS = ("when", "doc_md", "deposit_filename", "md5", "size_bytes",
              "dataset_doi", "data_file_id", "status")


def read_log() -> list[dict]:
    if not LOG.exists():
        return []
    with LOG.open() as f:
        return list(csv.DictReader(f))


def write_log_row(row: dict):
    exists = LOG.exists()
    with LOG.open("a", newline="") as f:
        w = csv.DictWriter(f, fieldnames=LOG_FIELDS)
        if not exists:
            w.writeheader()
        w.writerow(row)


# --- upload ----------------------------------------------------------------

def upload_one(fm: dict, doc_name: str, key: str, server: str,
               persistent_id: str, scratch: Path, dry_run: bool,
               staged_dir: Path | None = None,
               direct_s3_threshold: int | None = None) -> dict:
    jsonData = build_jsondata(fm)
    src, deposit_name, sz = stage_file(fm, doc_name, scratch,
                                       fetch=not dry_run,
                                       staged_dir=staged_dir)
    md5 = fm.get("md5") or (file_md5(src) if src is not None else "")

    if dry_run:
        sz_str = f"{sz/1e6:.1f} MB" if sz else "(unknown)"
        log(f"  [dry-run] {doc_name} → {deposit_name} ({sz_str})")
        log(f"            jsonData: {jsonData[:160]}{'…' if len(jsonData)>160 else ''}")
        return {"data_file_id": "", "status": "dry-run", "md5": md5,
                "deposit_filename": deposit_name, "size_bytes": sz or 0}

    # Dispatch to direct-S3 when the file is over `direct_s3_threshold`.
    # The standard /add path can balk above ~3 GB on Harvard Dataverse;
    # the direct-S3 path uploads to AWS S3 via a presigned URL (or a
    # multipart set of URLs) and then registers the file with the
    # dataset, avoiding the WAF and the streaming-through-the-app
    # bottleneck.
    if (direct_s3_threshold is not None
            and src is not None and sz is not None
            and sz >= direct_s3_threshold):
        log(f"  size {sz/1024**3:.2f} GB >= threshold "
            f"{direct_s3_threshold/1024**3:.2f} GB → direct-S3 upload")
        return upload_direct_s3(fm, doc_name, src, deposit_name, sz, md5,
                                 key, server, persistent_id, scratch)

    # Write jsonData to a tempfile to avoid shell-quoting headaches.
    with tempfile.NamedTemporaryFile("w", suffix=".json", delete=False,
                                     dir=str(scratch)) as tf:
        tf.write(jsonData)
        json_path = tf.name

    url = (f"{server}/api/datasets/:persistentId/add"
           f"?persistentId={quote(persistent_id, safe=':/')}")
    # NOTE: the /add endpoint extracts .zip / .tar.gz on the server side
    # regardless of the curl multipart Content-Type or jsonData mimeType
    # (Dataverse sniffs file bytes with libmagic). The only reliable way
    # to upload an archive ATOMICALLY is direct-S3 — see upload_direct_s3
    # below. The caller can force direct-S3 for every upload by passing
    # --direct-s3-threshold 1.
    cmd = ["curl", "-sS", "-H", f"X-Dataverse-key:{key}", "-X", "POST",
           "-F", f"file=@{src};filename={deposit_name}",
           "-F", f"jsonData=<{json_path}", url]
    out = subprocess.check_output(cmd, text=True)
    try:
        parsed = json.loads(out)
    except json.JSONDecodeError:
        raise RuntimeError(f"non-JSON response from Dataverse: {out[:400]}")

    if parsed.get("status") != "OK":
        raise RuntimeError(f"upload failed: {parsed.get('message')!s}")
    dfid = parsed["data"]["files"][0]["dataFile"]["id"]
    return {"data_file_id": str(dfid), "status": "OK", "md5": md5,
            "deposit_filename": deposit_name, "size_bytes": sz}


# --- direct-S3 upload (for files > /add can handle, ~3 GB+) ----------------
#
# Reference: https://guides.dataverse.org/en/latest/developers/s3-direct-upload-api.html
#
# Three-phase flow:
#   1. POST /api/datasets/:persistentId/uploadurls?size=N&persistentId=...
#      with X-Dataverse-key header.
#      Response shape (single-part, size < partSize):
#          {"status":"OK","data":{
#             "url":"<presigned PUT URL>",
#             "partSize":...,
#             "storageIdentifier":"s3://<bucket>:<key>"}}
#      Response shape (multipart, size >= partSize):
#          {"status":"OK","data":{
#             "urls":{"1":"<URL>","2":"<URL>",...},
#             "partSize":...,
#             "abort":"/api/datasets/mpupload/abort?storageIdentifier=...",
#             "complete":"/api/datasets/mpupload/complete?storageIdentifier=...",
#             "storageIdentifier":"s3://<bucket>:<key>"}}
#
#   2. PUT bytes to each presigned URL (single PUT for small; chunk-PUT
#      for multipart, collecting the ETag header per part).
#      For multipart, after all parts succeed, PUT the JSON map of
#      {"1":"<etag1>","2":"<etag2>",...} to the `complete` endpoint
#      (the X-Dataverse-key header is required). On failure, DELETE the
#      `abort` endpoint to free the S3 multipart upload state.
#
#   3. POST /api/datasets/:persistentId/addFiles?persistentId=... with
#      jsonData = JSON array of file descriptors. Each descriptor is:
#          {"description":..., "directoryLabel":...,
#           "categories":[...], "restrict":bool,
#           "storageIdentifier": <from step 1>,
#           "fileName": <deposit_name>,
#           "mimeType": <content_type>,
#           "checksum": {"@type":"MD5", "@value": <md5 hex>}}
#      This registers the S3 object with the Dataverse dataset and
#      returns the dataFile ID, mirroring the /add response.

def _http_request(method: str, url: str, key: str | None = None,
                  data: bytes | None = None,
                  headers: dict | None = None,
                  timeout: int = 300) -> tuple[int, dict, bytes]:
    """Thin urllib wrapper that returns (status, headers, body)."""
    import urllib.request, urllib.error
    h = dict(headers or {})
    if key is not None:
        h.setdefault("X-Dataverse-key", key)
    req = urllib.request.Request(url, data=data, method=method, headers=h)
    try:
        with urllib.request.urlopen(req, timeout=timeout) as resp:
            return resp.status, dict(resp.headers), resp.read()
    except urllib.error.HTTPError as e:
        return e.code, dict(e.headers or {}), (e.read() or b"")


def upload_direct_s3(fm: dict, doc_name: str, src_path: Path,
                     deposit_name: str, size_bytes: int, md5_hex: str,
                     key: str, server: str, persistent_id: str,
                     scratch: Path) -> dict:
    """Upload a large file via Harvard Dataverse's S3 direct-upload API.

    Implementation follows the three-phase flow above; multipart chunks
    are PUT sequentially (Dataverse hands out per-part presigned URLs,
    so concurrency would require parallel HTTP — kept sequential here
    for simplicity and predictable failure semantics).
    """
    import urllib.parse
    pid_q = quote(persistent_id, safe=":/")

    # ---- Phase 1: request upload URL(s) ----
    qs = urllib.parse.urlencode({
        "persistentId": persistent_id,
        "size": str(size_bytes),
    })
    status, hdrs, body = _http_request(
        "POST",
        f"{server}/api/datasets/:persistentId/uploadurls?{qs}",
        key=key, timeout=60,
    )
    if status not in (200, 201):
        raise RuntimeError(
            f"{doc_name}: uploadurls HTTP {status}: {body[:400].decode(errors='replace')}"
        )
    payload = json.loads(body).get("data") or {}
    storage_id = payload.get("storageIdentifier")
    if not storage_id:
        raise RuntimeError(f"{doc_name}: no storageIdentifier in uploadurls response")
    part_size = int(payload.get("partSize") or 0)

    # ---- Phase 2: PUT bytes ----
    if "url" in payload:
        # Single-part upload (size < partSize).
        log(f"  [direct-s3] single-part PUT ({size_bytes/1024**3:.2f} GB)")
        with src_path.open("rb") as f:
            data = f.read()
        status, _, body = _http_request(
            "PUT", payload["url"], data=data,
            headers={"x-amz-tagging": "dv-state=temp"},
            timeout=3600,
        )
        if status not in (200, 201, 204):
            raise RuntimeError(
                f"{doc_name}: single-part PUT HTTP {status}: {body[:400].decode(errors='replace')}"
            )
    else:
        # Multipart upload.
        part_urls = payload.get("urls") or {}
        abort_path = payload.get("abort") or payload.get("abortUrl")
        complete_path = payload.get("complete") or payload.get("completeUrl")
        if not part_urls or not complete_path:
            raise RuntimeError(
                f"{doc_name}: multipart response missing urls/complete: {payload}"
            )
        log(f"  [direct-s3] multipart upload, {len(part_urls)} parts × "
            f"{part_size/1024**2:.1f} MB ({size_bytes/1024**3:.2f} GB total)")
        etags: dict[str, str] = {}
        try:
            with src_path.open("rb") as f:
                for part_num_s, put_url in sorted(
                        part_urls.items(), key=lambda kv: int(kv[0])):
                    part_num = int(part_num_s)
                    f.seek((part_num - 1) * part_size)
                    chunk = f.read(part_size)
                    status, hdrs, body = _http_request(
                        "PUT", put_url, data=chunk, timeout=3600,
                    )
                    if status not in (200, 201, 204):
                        raise RuntimeError(
                            f"{doc_name}: multipart part {part_num} PUT HTTP {status}: "
                            f"{body[:200].decode(errors='replace')}"
                        )
                    etag = (hdrs.get("ETag") or hdrs.get("etag") or "").strip('"')
                    if not etag:
                        raise RuntimeError(
                            f"{doc_name}: multipart part {part_num} returned no ETag"
                        )
                    etags[part_num_s] = etag
                    if part_num % 10 == 0 or part_num == len(part_urls):
                        log(f"  [direct-s3] uploaded part {part_num}/{len(part_urls)}")
            # Phase 2b: complete the multipart upload.
            status, _, body = _http_request(
                "PUT", f"{server}{complete_path}",
                key=key,
                data=json.dumps(etags).encode(),
                headers={"Content-Type": "application/json"},
                timeout=300,
            )
            if status not in (200, 201, 204):
                raise RuntimeError(
                    f"{doc_name}: multipart complete HTTP {status}: "
                    f"{body[:400].decode(errors='replace')}"
                )
        except Exception:
            # Best-effort: abort the S3 multipart upload to release state.
            if abort_path:
                try:
                    _http_request(
                        "DELETE", f"{server}{abort_path}", key=key, timeout=60,
                    )
                except Exception:
                    pass
            raise

    # ---- Phase 3: register the uploaded object with the dataset ----
    categories = fm.get("categories", [])
    if isinstance(categories, str):
        categories = [c.strip() for c in categories.split(",") if c.strip()]
    file_desc = {
        "description":     fm.get("description", "")[:1000],
        "directoryLabel":  fm.get("directoryLabel", ""),
        "categories":      categories,
        "restrict":        bool(fm.get("restrict", False)),
        "storageIdentifier": storage_id,
        "fileName":        deposit_name,
        "mimeType":        fm.get("content_type", "application/octet-stream"),
        "checksum":        {"@type": "MD5", "@value": md5_hex},
    }
    add_payload = json.dumps([file_desc])
    with tempfile.NamedTemporaryFile("w", suffix=".json", delete=False,
                                     dir=str(scratch)) as tf:
        tf.write(add_payload)
        json_path = tf.name

    add_url = (f"{server}/api/datasets/:persistentId/addFiles"
               f"?persistentId={pid_q}")
    cmd = ["curl", "-sS", "-H", f"X-Dataverse-key:{key}", "-X", "POST",
           "-F", f"jsonData=<{json_path}", add_url]
    out = subprocess.check_output(cmd, text=True)
    Path(json_path).unlink(missing_ok=True)
    try:
        parsed = json.loads(out)
    except json.JSONDecodeError:
        raise RuntimeError(f"{doc_name}: non-JSON addFiles response: {out[:400]}")
    if parsed.get("status") != "OK":
        raise RuntimeError(f"{doc_name}: addFiles failed: {parsed.get('message')!s}")
    files = parsed.get("data", {}).get("Files") or parsed.get("data", {}).get("files") or []
    dfid = ""
    if files and isinstance(files, list):
        first = files[0]
        dfid = str(first.get("dataFile", {}).get("id")
                   or first.get("data_file_id") or "")
    return {"data_file_id": dfid, "status": "OK", "md5": md5_hex,
            "deposit_filename": deposit_name, "size_bytes": size_bytes}


# --- main ------------------------------------------------------------------

def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--dry-run", action="store_true",
                    help="print what would be uploaded; do not POST")
    ap.add_argument("--only", help="upload one doc only (basename, e.g. "
                                   "acknowledgements.md)")
    ap.add_argument("--dir", help="upload all docs with this directoryLabel")
    ap.add_argument("--skip-existing", action="store_true",
                    help="skip docs whose md5 + name already appear in upload_log.csv")
    ap.add_argument("--persistent-id", default=DEFAULT_PERSISTENT_ID,
                    help="Dataverse persistentId (default: %(default)s)")
    ap.add_argument("--server", default=DEFAULT_SERVER)
    ap.add_argument("--key-file", default=os.path.expanduser("~/.dataverse_api_key"))
    ap.add_argument("--scratch", default="/tmp/dataverse_upload",
                    help="local scratch dir for staging GCS files")
    ap.add_argument("--use-staged", default="/tmp/dataverse_staging",
                    help="directory of pre-packaged ZIPs from package.py "
                         "(default: %(default)s; pass empty string to disable)")
    ap.add_argument("--direct-s3-threshold", type=int, default=None,
                    help="size in bytes above which to use direct-S3 "
                         "upload (default: disabled; Harvard Dataverse "
                         "/add endpoint balks above ~3 GB, so a typical "
                         "value is 3000000000)")
    args = ap.parse_args()

    key = os.environ.get("DATAVERSE_API_KEY")
    if not key:
        kp = Path(args.key_file)
        if not kp.exists():
            log(f"no API key at {kp} and DATAVERSE_API_KEY unset")
            sys.exit(2)
        key = kp.read_text().strip()

    scratch = Path(args.scratch)
    scratch.mkdir(parents=True, exist_ok=True)
    staged_dir = Path(args.use_staged) if args.use_staged else None
    if staged_dir and not staged_dir.exists():
        log(f"warning: --use-staged dir {staged_dir} does not exist; ignoring")
        staged_dir = None

    docs = sorted(DOCS.glob("*.md"))
    if args.only:
        docs = [d for d in docs if d.name == args.only]
        if not docs:
            log(f"no doc named {args.only}"); sys.exit(2)
    if args.dir:
        docs = [d for d in docs if (parse_frontmatter(d).get("directoryLabel") == args.dir)]

    prev = read_log()
    prev_keys = {(r["deposit_filename"], r["md5"]) for r in prev if r["status"] == "OK"}

    log(f"[upload] target dataset: {args.persistent_id}")
    log(f"[upload] {len(docs)} docs to process; "
        f"{len(prev_keys)} previous successes on record")

    for i, dpath in enumerate(docs, 1):
        log(f"[{i:>3}/{len(docs)}] {dpath.name}")
        try:
            fm = parse_frontmatter(dpath)
            res = upload_one(fm, dpath.name, key, args.server,
                             args.persistent_id, scratch, args.dry_run,
                             staged_dir=staged_dir,
                             direct_s3_threshold=args.direct_s3_threshold)
            if args.skip_existing and (res["deposit_filename"], res["md5"]) in prev_keys:
                log(f"  skipped (already in log)")
                continue
            row = {
                "when": datetime.now(timezone.utc).isoformat(),
                "doc_md": dpath.name,
                "deposit_filename": res["deposit_filename"],
                "md5": res["md5"],
                "size_bytes": res["size_bytes"],
                "dataset_doi": args.persistent_id,
                "data_file_id": res["data_file_id"],
                "status": res["status"],
            }
            if not args.dry_run:
                write_log_row(row)
            log(f"  → OK ({res['data_file_id']})")
        except Exception as e:
            log(f"  → ERROR: {type(e).__name__}: {e}")
            row = {
                "when": datetime.now(timezone.utc).isoformat(),
                "doc_md": dpath.name,
                "deposit_filename": "",
                "md5": "",
                "size_bytes": "",
                "dataset_doi": args.persistent_id,
                "data_file_id": "",
                "status": f"error: {type(e).__name__}",
            }
            if not args.dry_run:
                write_log_row(row)


if __name__ == "__main__":
    main()
