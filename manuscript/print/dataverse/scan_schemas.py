#!/usr/bin/env python3
"""Stream schema + size + rowcount for every parquet/feather file we plan to
upload to the Dataverse. Reads `banc_data_locations.md` for the target list,
inspects each via pyarrow over GCS (or local FS), and writes a JSON dump plus
a markdown summary.

Run with no args; emits `gcs_schemas.json` and `sizes.csv` alongside this script.
"""
from __future__ import annotations
import json, re, sys, os, csv, time
from pathlib import Path
import pyarrow as pa
import pyarrow.ipc as ipc
import pyarrow.parquet as pq
import pyarrow.fs as pafs

HERE = Path(__file__).parent
LOCATIONS = HERE.parent / "banc_data_locations.md"
OUT_JSON = HERE / "gcs_schemas.json"
OUT_CSV  = HERE / "sizes.csv"
BUCKET   = "lee-lab_brain-and-nerve-cord-fly-connectome"

gcs = pafs.GcsFileSystem(anonymous=False)
local = pafs.LocalFileSystem()

def parse_locations(text: str):
    """Return list of (heading, path_or_url) tuples."""
    items = []
    cur_h = None
    for line in text.splitlines():
        m_h = re.match(r"^#{3,4}\s+(.*?)\s*$", line)
        if m_h:
            cur_h = m_h.group(1)
            continue
        s = line.strip()
        if not s or s.startswith("#"):
            continue
        # Skip pure prose under "Plan." etc.
        if s.startswith(("We ", "The ", "Order ", "In some")):
            continue
        items.append((cur_h, s))
    return items

def normalise(path: str) -> tuple[str, str]:
    """Return (fs_kind, resolved_path) where fs_kind in {gcs,local,gh,unknown}."""
    if path.startswith("gs://"):
        return "gcs", path[len("gs://"):]
    if path.startswith("/"):
        return "local", path
    if path.startswith(("https://", "http://")):
        return "url", path
    if path.startswith("lee-lab_brain-and-nerve-cord-fly-connectome/"):
        return "gcs", path
    return "unknown", path

def inspect_one(fs_kind: str, p: str):
    """Return dict with size_bytes, schema (cols), nrows where applicable."""
    out = {"path": p, "fs": fs_kind}
    if fs_kind not in ("gcs", "local"):
        out["note"] = "non-blob source (web/github); skip"
        return out
    fs = gcs if fs_kind == "gcs" else local
    try:
        info = fs.get_file_info(p)
    except Exception as e:
        out["error"] = f"get_file_info: {e}"
        return out
    if info.type == pafs.FileType.NotFound:
        out["error"] = "not found"
        return out
    if info.type == pafs.FileType.Directory:
        out["kind"] = "directory"
        # Skip recursive size for huge dirs (e.g. neuron_meshes)
        try:
            entries = fs.get_file_info(pafs.FileSelector(p, recursive=False))
            out["entries_n"] = len(entries)
            out["entries_sample"] = [e.path for e in entries[:10]]
        except Exception as e:
            out["dir_err"] = str(e)
        return out
    out["size_bytes"] = info.size
    suf = Path(p).suffix.lower()
    out["kind"] = "file"
    try:
        if suf in (".feather", ".arrow"):
            with fs.open_input_file(p) as f:
                r = ipc.open_file(f)
                out["format"] = "feather"
                out["nrows"] = sum(r.get_batch(i).num_rows for i in range(r.num_record_batches))
                out["schema"] = [{"name": n, "type": str(t)} for n, t in zip(r.schema.names, r.schema.types)]
        elif suf == ".parquet":
            with fs.open_input_file(p) as f:
                pf = pq.ParquetFile(f)
                out["format"] = "parquet"
                out["nrows"] = pf.metadata.num_rows
                out["num_row_groups"] = pf.num_row_groups
                out["schema"] = [{"name": n, "type": str(t)} for n, t in zip(pf.schema_arrow.names, pf.schema_arrow.types)]
        elif suf in (".csv", ".tsv"):
            out["format"] = "csv"
            # Just read the first line to get column names
            with fs.open_input_stream(p) as f:
                first = f.read(8192).decode("utf-8", errors="replace")
            head = first.splitlines()[0] if first else ""
            sep = "\t" if suf == ".tsv" else ","
            out["columns"] = [c.strip() for c in head.split(sep)]
        else:
            out["format"] = suf.lstrip(".") or "unknown"
    except Exception as e:
        out["schema_err"] = f"{type(e).__name__}: {e}"
    return out

def main():
    text = LOCATIONS.read_text()
    items = parse_locations(text)
    print(f"[scan] {len(items)} items in {LOCATIONS.name}", file=sys.stderr)
    results = []
    for i, (heading, path) in enumerate(items, 1):
        fs_kind, p = normalise(path)
        t0 = time.monotonic()
        info = {"index": i, "heading": heading, "raw": path}
        info.update(inspect_one(fs_kind, p))
        info["elapsed_s"] = round(time.monotonic() - t0, 2)
        results.append(info)
        sz = info.get("size_bytes")
        szs = f"{sz/(1024**3):.2f} GB" if sz and sz >= 1024**3 else (f"{sz/1024**2:.1f} MB" if sz and sz >= 1024**2 else (f"{sz} B" if sz else "—"))
        print(f"  [{i:>2}/{len(items)}] {heading or '(no heading)'}: {szs}  ({info.get('format','?')})", file=sys.stderr)

    OUT_JSON.write_text(json.dumps(results, indent=2))

    # Sizes CSV: only items with a numeric size
    with OUT_CSV.open("w") as f:
        w = csv.writer(f)
        w.writerow(["index","heading","path","fs","kind","format","size_bytes","size_gb","nrows","ncols"])
        cum = 0
        for r in results:
            sz = r.get("size_bytes")
            cum += sz or 0
            w.writerow([
                r["index"], r.get("heading",""), r.get("raw",""), r.get("fs",""),
                r.get("kind",""), r.get("format",""),
                sz or "", round((sz or 0)/(1024**3), 4) if sz else "",
                r.get("nrows",""),
                len(r.get("schema") or r.get("columns") or []) or ""
            ])
    print(f"[scan] total summed size: {cum/(1024**3):.2f} GB", file=sys.stderr)
    print(f"[scan] wrote {OUT_JSON} and {OUT_CSV}", file=sys.stderr)

if __name__ == "__main__":
    main()
