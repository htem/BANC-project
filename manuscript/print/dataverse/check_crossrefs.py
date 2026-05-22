#!/usr/bin/env python3
"""Cross-reference symmetry check across the per-file documentation.

For every per-file .md in documentation/, extract the 'Related files'
section and look for mentions of other deposit files (by basename or by
GCS path). Build a directed graph and report asymmetric edges (A → B
without B → A) so we can patch the missing back-pointers.
"""
from __future__ import annotations
import re, sys
from pathlib import Path
from collections import defaultdict

HERE = Path(__file__).parent
DOCS = HERE / "documentation"

# Pull filenames the upload will use, plus the doc_md → deposit-name mapping
# from frontmatter (some docs have local_path or gcs_path with different
# basename than the .md filename).
import yaml

def parse_frontmatter(path: Path) -> dict | None:
    raw = path.read_text()
    parts = re.split(r"(?m)^---\s*$", raw, maxsplit=2)
    if len(parts) < 3:
        return None
    fm = yaml.safe_load(parts[1])
    return fm if isinstance(fm, dict) else None

# Map each doc to the set of "search terms" — substrings that, if another doc
# mentions them, count as a reference. Use the basename of the local_path / gcs_path
# AND the doc .md stem, since some docs are referenced by their pretty name.
def search_terms_for(path: Path, fm: dict) -> list[str]:
    terms = set()
    terms.add(path.stem)  # e.g. banc_888_meta
    for key in ("filename", "dataverse_filename", "local_path", "gcs_path"):
        v = fm.get(key)
        if v and isinstance(v, str):
            terms.add(Path(v).name)  # basename
            terms.add(Path(v).stem)
    return sorted(t for t in terms if t and len(t) > 4)

def find_related_section(text: str) -> str:
    m = re.search(r"^## Related files\b(.*?)(?=^##\s|\Z)", text, flags=re.M|re.S)
    return m.group(1) if m else ""

def main():
    docs = {}  # doc_md -> {fm, body, related_section, search_terms}
    for p in sorted(DOCS.glob("*.md")):
        fm = parse_frontmatter(p)
        if not fm: continue
        body = p.read_text()
        docs[p.name] = {
            "path": p,
            "fm": fm,
            "body": body,
            "rel": find_related_section(body),
            "terms": search_terms_for(p, fm),
        }

    # Build directed graph: A -> B if A's related-files section mentions one
    # of B's search terms.
    edges = defaultdict(set)
    for a, da in docs.items():
        rel_text = da["rel"]
        if not rel_text:
            continue
        for b, db in docs.items():
            if a == b: continue
            for t in db["terms"]:
                if t in rel_text:
                    edges[a].add(b)
                    break

    # Asymmetric pairs: A→B without B→A
    asym = []
    for a in edges:
        for b in edges[a]:
            if a not in edges.get(b, set()):
                asym.append((a, b))

    # Sort by source then target
    asym.sort()
    print(f"== Docs:           {len(docs)}")
    print(f"== Directed edges: {sum(len(v) for v in edges.values())}")
    print(f"== Asymmetric:     {len(asym)}\n")

    # Group by target (which doc is missing back-pointers from many sources)
    missing_pointers = defaultdict(list)
    for a, b in asym:
        missing_pointers[b].append(a)
    print("== Most-mentioned docs missing back-pointers (top 25):")
    for b in sorted(missing_pointers, key=lambda x: -len(missing_pointers[x]))[:25]:
        print(f"  {b}: {len(missing_pointers[b])} sources point to it")
        for a in sorted(missing_pointers[b])[:6]:
            print(f"    ← {a}")
        if len(missing_pointers[b]) > 6:
            print(f"    ... and {len(missing_pointers[b]) - 6} more")

    # Also list orphans (no incoming edges)
    incoming = defaultdict(set)
    for a in edges:
        for b in edges[a]:
            incoming[b].add(a)
    orphans = [d for d in docs if d not in incoming]
    print(f"\n== Orphan docs (no incoming refs): {len(orphans)}")
    for o in sorted(orphans):
        print(f"  {o}")

if __name__ == "__main__":
    main()
