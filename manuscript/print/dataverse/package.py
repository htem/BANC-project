#!/usr/bin/env python3
"""Produce upload-ready ZIPs/tarballs for the Dataverse deposit.

Each doc in documentation/ either points at a single file on GCS or
local disk (handled by upload.py directly), at a GitHub repo
(`upstream_source` URL — needs a tarball fetch), or at a directory
(`gcs_path` ending without a parquet/feather/csv suffix — needs a ZIP).

This script handles the latter two. Output lands under a staging
directory (default `/tmp/dataverse_staging/`) keyed by the deposit
filename declared in the doc's frontmatter (e.g. `bancr_archive.zip`).

After running this script, run `upload.py` with `--use-staged STAGING_DIR`
to consult the staging dir before going to source.

Per-item handling
-----------------

- **GitHub tarballs.** Use `gh api repos/OWNER/NAME/zipball/main` (works
  for both public and private repos given `gh auth status` is OK) and
  save to `STAGING/<deposit_filename>`. Record the commit SHA from the
  HTTP `Etag` header for the per-file doc.

- **GCS directories.** Use `gsutil -m rsync` to mirror to a local
  scratch, then `zip -r` to produce the deposit ZIP. Skip if the dir's
  total size exceeds `--max-local-bytes` (default 2 GB) — that
  threshold is meant to keep this script local-disk-friendly. For
  larger dirs, see `--list-too-large` to print a separate work plan
  that would need to run on a host with disk.

- **All-to-all influence shards.** Special case: no packaging, just
  emits a manifest of the 277 shard URLs for direct-from-GCS upload.

Usage
-----

    python3 package.py                    # do everything that fits
    python3 package.py --only bancr_archive.zip
    python3 package.py --list-too-large
    python3 package.py --skip-existing    # don't re-fetch / re-zip
"""
from __future__ import annotations
import argparse, os, re, shutil, subprocess, sys
from pathlib import Path
import yaml

HERE = Path(__file__).parent
DOCS = HERE / "documentation"
DEFAULT_STAGE = Path("/tmp/dataverse_staging")
DEFAULT_MAX_LOCAL_BYTES = 2 * 1024**3   # 2 GB

def log(*a, **kw): print(*a, **kw, file=sys.stderr, flush=True)

def parse_fm(p: Path) -> dict:
    raw = p.read_text()
    parts = re.split(r"(?m)^---\s*$", raw, maxsplit=2)
    if len(parts) < 3: return {}
    fm = yaml.safe_load(parts[1])
    return fm if isinstance(fm, dict) else {}


# --- classifiers -----------------------------------------------------------

def classify(fm: dict) -> str:
    """Return 'github' | 'pypi' | 'gcs_dir' | 'local_dir' | 'all_to_all' | 'single'."""
    if fm.get("filename") and "influence/all_to_all" in str(fm.get("gcs_path", "")):
        return "all_to_all"
    upstream = fm.get("upstream_source") or fm.get("upstream_url") or ""
    if "pypi.org/project/" in upstream:
        return "pypi"
    if upstream and "github.com" in upstream:
        return "github"
    gcs = fm.get("gcs_path", "")
    if gcs:
        # Heuristic: GCS path with no file suffix → directory
        if "." not in Path(gcs).name:
            return "gcs_dir"
        return "single"
    local = fm.get("local_path", "")
    if local and Path(local).exists() and Path(local).is_dir():
        return "local_dir"
    return "single"


# --- handlers --------------------------------------------------------------

def parse_github_url(url: str) -> tuple[str, str]:
    """https://github.com/OWNER/NAME/  →  (OWNER, NAME)"""
    m = re.match(r"https?://github\.com/([^/]+)/([^/]+?)/?$", url)
    if not m:
        raise ValueError(f"can't parse github url: {url}")
    return m.group(1), m.group(2)


def fetch_github(fm: dict, doc: Path, stage: Path) -> Path:
    deposit_name = fm.get("filename") or f"{doc.stem}.zip"
    out = stage / deposit_name
    if out.exists():
        log(f"  exists, skipping: {out.name}")
        return out
    github_url = fm.get("upstream_source") or fm.get("upstream_url")
    owner, name = parse_github_url(github_url)
    # Prefer the frontmatter's pinned_commit (reproducible deposit);
    # fall back to default_branch, then to "main". This matches the
    # commitments documented in each archive's frontmatter (e.g.
    # `pinned_commit: c594c91`, `pinned_date: 2026-05-13`).
    ref = (fm.get("pinned_commit") or fm.get("default_branch") or "main").strip()
    log(f"  gh zipball: {owner}/{name} @ {ref}")
    subprocess.check_call(
        ["gh", "api", f"repos/{owner}/{name}/zipball/{ref}",
         "--method", "GET", "-H", "Accept: application/vnd.github+json",
         "-i"],  # include headers so we can record commit SHA
        stdout=open(out.with_suffix(".raw"), "wb"),
    )
    # The -i output is HTTP/2 headers + body. Split.
    raw = out.with_suffix(".raw").read_bytes()
    head, _, body = raw.partition(b"\r\n\r\n")
    head_text = head.decode("utf-8", errors="replace")
    out.write_bytes(body)
    out.with_suffix(".raw").unlink()
    # Pull etag for sha
    m = re.search(r"^etag:\s*\"?([^\s\"]+)", head_text, re.I | re.M)
    if m:
        log(f"  commit sha (etag): {m.group(1)}")
    log(f"  wrote {out} ({out.stat().st_size/1e6:.1f} MB)")
    return out


PYPI_NAME_RE = re.compile(r"https?://pypi\.org/project/([^/]+)/?")
PYPI_VERSIONED_NAME_RE = re.compile(r"^([A-Za-z0-9_.-]+)-([0-9][0-9A-Za-z.+-]*)\.tar\.gz$")


def parse_pypi_url(url: str) -> str:
    """https://pypi.org/project/banc/  →  'banc'"""
    m = PYPI_NAME_RE.match(url)
    if not m:
        raise ValueError(f"can't parse pypi url: {url}")
    return m.group(1)


def fetch_pypi(fm: dict, doc: Path, stage: Path) -> Path:
    """Download a PyPI sdist (.tar.gz) via `pip download --no-deps`.

    The expected `filename` is `<project>-<version>.tar.gz`; if a `release_tag`
    or explicit version is present in frontmatter we pin that, otherwise we
    take the latest release at fetch time and rename the resulting tarball
    into `stage/<deposit_filename>` so the manifest stays stable.
    """
    deposit_name = fm.get("filename")
    if not deposit_name:
        raise ValueError(f"{doc.name}: pypi entry needs a `filename` in frontmatter")
    out = stage / deposit_name
    if out.exists():
        log(f"  exists, skipping: {out.name}")
        return out
    upstream = fm.get("upstream_source") or fm.get("upstream_url")
    project = parse_pypi_url(upstream)

    # Prefer the version declared in the deposit filename
    # (e.g. `banc-0.6.1.tar.gz` → `banc==0.6.1`).
    m = PYPI_VERSIONED_NAME_RE.match(deposit_name)
    if m:
        spec = f"{m.group(1)}=={m.group(2)}"
    else:
        # fall back to release_tag (strip leading `v`) or latest
        tag = (fm.get("release_tag") or "").lstrip("v").strip()
        spec = f"{project}=={tag}" if tag and tag != "(none — pinned to main HEAD)" else project

    tmp_dir = stage / f".pip_{doc.stem}"
    if tmp_dir.exists():
        shutil.rmtree(tmp_dir)
    tmp_dir.mkdir(parents=True, exist_ok=True)
    log(f"  pip download (sdist only): {spec}")
    subprocess.check_call(
        ["pip", "download", spec,
         "--no-deps", "--no-binary", ":all:",
         "--dest", str(tmp_dir)],
    )
    candidates = sorted(tmp_dir.glob("*.tar.gz"))
    if not candidates:
        # pip 25+ may emit .zip sdists for some projects; fall back.
        candidates = sorted(tmp_dir.glob("*.zip"))
    if not candidates:
        raise RuntimeError(f"pip download produced no sdist in {tmp_dir}")
    src = candidates[0]
    if src.name != deposit_name:
        log(f"  renaming {src.name} → {deposit_name}")
    shutil.move(str(src), str(out))
    shutil.rmtree(tmp_dir)
    log(f"  wrote {out} ({out.stat().st_size/1e6:.1f} MB)")
    return out


def gcs_dir_total_bytes(gcs_path: str) -> int:
    p = gcs_path.rstrip("/")
    if not p.startswith("gs://"):
        p = "gs://" + p
    out = subprocess.check_output(["gsutil", "du", "-s", p], text=True)
    return int(out.split()[0])


def package_gcs_dir(fm: dict, doc: Path, stage: Path,
                     max_bytes: int) -> Path | None:
    deposit_name = fm.get("filename") or f"{doc.stem}.zip"
    out = stage / deposit_name
    if out.exists():
        log(f"  exists, skipping: {out.name}")
        return out
    gcs = fm["gcs_path"].rstrip("/")
    if not gcs.startswith("gs://"):
        gcs = "gs://" + gcs
    sz = gcs_dir_total_bytes(gcs)
    log(f"  source size: {sz/1024**3:.2f} GB")
    if sz > max_bytes:
        log(f"  SKIP (> {max_bytes/1024**3:.1f} GB local cap)")
        return None
    mirror = stage / f".mirror_{doc.stem}"
    mirror.mkdir(parents=True, exist_ok=True)
    log(f"  gsutil rsync → {mirror}")
    subprocess.check_call(["gsutil", "-m", "rsync", "-r", gcs, str(mirror)])
    log(f"  zip → {out}")
    subprocess.check_call(
        ["zip", "-r", "-q", str(out), "."], cwd=mirror
    )
    shutil.rmtree(mirror)
    log(f"  wrote {out} ({out.stat().st_size/1e6:.1f} MB)")
    return out


def package_local_dir(fm: dict, doc: Path, stage: Path,
                       max_bytes: int) -> Path | None:
    deposit_name = fm.get("filename") or f"{doc.stem}.zip"
    out = stage / deposit_name
    if out.exists():
        log(f"  exists, skipping: {out.name}")
        return out
    src = Path(fm["local_path"])
    sz = sum(f.stat().st_size for f in src.rglob("*") if f.is_file())
    log(f"  source size: {sz/1024**3:.2f} GB")
    if sz > max_bytes:
        log(f"  SKIP (> {max_bytes/1024**3:.1f} GB local cap)")
        return None
    log(f"  zip → {out}")
    subprocess.check_call(
        ["zip", "-r", "-q", str(out), "."], cwd=src
    )
    log(f"  wrote {out} ({out.stat().st_size/1e6:.1f} MB)")
    return out


# --- main ------------------------------------------------------------------

def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--stage", default=str(DEFAULT_STAGE),
                    help="staging directory (default: %(default)s)")
    ap.add_argument("--max-local-bytes", type=int, default=DEFAULT_MAX_LOCAL_BYTES,
                    help="skip dirs whose source size exceeds this (default 2 GB)")
    ap.add_argument("--only", help="package just this doc (basename .md)")
    ap.add_argument("--list-too-large", action="store_true",
                    help="just list items that exceed --max-local-bytes")
    args = ap.parse_args()

    stage = Path(args.stage)
    stage.mkdir(parents=True, exist_ok=True)

    docs = sorted(DOCS.glob("*.md"))
    if args.only:
        docs = [d for d in docs if d.name == args.only]

    too_large = []
    for d in docs:
        fm = parse_fm(d)
        kind = classify(fm)
        if kind == "single":
            continue
        log(f"[{kind:>10s}] {d.name}")
        try:
            if kind == "github":
                fetch_github(fm, d, stage)
            elif kind == "pypi":
                fetch_pypi(fm, d, stage)
            elif kind == "gcs_dir":
                res = package_gcs_dir(fm, d, stage, args.max_local_bytes)
                if res is None: too_large.append(d.name)
            elif kind == "local_dir":
                res = package_local_dir(fm, d, stage, args.max_local_bytes)
                if res is None: too_large.append(d.name)
            elif kind == "all_to_all":
                log(f"  (no packaging — upload as 277 individual shards)")
        except subprocess.CalledProcessError as e:
            log(f"  ERROR: {e}")
        except Exception as e:
            log(f"  ERROR: {type(e).__name__}: {e}")

    if too_large:
        log(f"\n{len(too_large)} items exceeded --max-local-bytes "
            f"= {args.max_local_bytes/1024**3:.1f} GB:")
        for n in too_large:
            log(f"  {n}")
        log("These need a host with more disk, direct-S3 from GCS, or")
        log("a streaming gsutil-cat | zip | curl-PUT pipeline.")

if __name__ == "__main__":
    main()
