#!/usr/bin/env python3
"""Build a Dataverse upload manifest CSV by walking dataverse/documentation/*.md
and parsing the YAML frontmatter of each. Emits one row per file with the
local-bookkeeping fields and the API metadata fields the upload script needs.
"""
from __future__ import annotations
import csv, re, json, sys
from pathlib import Path
import yaml

HERE = Path(__file__).parent
DOCS = HERE / "documentation"
OUT  = HERE / "manifest.csv"

FIELDS = [
    "doc_md", "filename", "local_path", "gcs_path", "dataverse_filename",
    "size_bytes", "size_human", "nrows", "ncols",
    "directoryLabel", "categories", "restrict", "tabIngest",
    "description_chars",
]

def parse(path: Path) -> dict | None:
    raw = path.read_text()
    parts = re.split(r"(?m)^---\s*$", raw, maxsplit=2)
    if len(parts) < 3:
        return None
    fm = yaml.safe_load(parts[1])
    if not isinstance(fm, dict):
        return None
    fm["doc_md"] = path.name
    fm["description_chars"] = len(fm.get("description", "") or "")
    # Some ZIP entries use unzipped_size_bytes; normalize for the budget column
    if "size_bytes" not in fm and "unzipped_size_bytes" in fm:
        fm["size_bytes"] = fm["unzipped_size_bytes"]
    if isinstance(fm.get("categories"), list):
        fm["categories"] = ",".join(fm["categories"])
    return fm

def main():
    rows = []
    for p in sorted(DOCS.glob("*.md")):
        fm = parse(p)
        if not fm:
            print(f"[skip] no frontmatter: {p.name}", file=sys.stderr)
            continue
        row = {k: fm.get(k, "") for k in FIELDS}
        rows.append(row)
    with OUT.open("w") as f:
        w = csv.DictWriter(f, fieldnames=FIELDS)
        w.writeheader()
        for r in rows:
            w.writerow(r)
    # Budget summary
    total = sum(int(r["size_bytes"] or 0) for r in rows)
    print(f"[manifest] {len(rows)} rows -> {OUT}", file=sys.stderr)
    print(f"[budget]   total size: {total:,} B = {total/(1024**3):.2f} GB"
          f" ({total/(2*1024**4)*100:.2f}% of 2 TB)", file=sys.stderr)

if __name__ == "__main__":
    main()
