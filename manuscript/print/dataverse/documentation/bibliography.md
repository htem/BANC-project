---
filename: bibliography.bib
local_path: /Users/papers/BANC-project/manuscript/print/bibliography.bib
size_bytes: 646143
size_human: 631 KiB
nrecords: 316
content_type: text/x-bibtex

# --- Fields posted to Dataverse ---
description: >-
  BibTeX bibliography for the BANC paper covering every reference cited
  in the published manuscript, supplements, and Methods, exported from
  the project's Paperpile library. Useful for reviewers and readers who
  want a programmatic citation manifest, and for downstream reuse where
  citing this dataset alongside the works it builds on is required.
  Single `.bib` file in standard BibTeX format; UTF-8.
categories:
  - Documentation
directoryLabel: documentation
restrict: false
tabIngest: false
---

# bibliography.bib

## Purpose

The full BibTeX bibliography for the BANC paper. Provides a programmatic,
plain-text listing of every cited work — useful when:

- Citing the paper together with its primary literature dependencies
  (e.g. for a meta-analysis).
- Cross-checking the paper's reference list.
- Importing the BANC references into a reference manager (`bibtex`,
  `biblatex`, or any reference manager that ingests `.bib`).

## Provenance

Exported from the project's shared Paperpile library after the manuscript
was finalized for resubmission. The export is a single `.bib` file
covering every citation in the main text and Methods.

## Contents

Standard BibTeX. Each entry starts with `@ENTRY_TYPE{citekey, ...}`
and carries fields such as `title`, `author`, `journal`, `year`,
`doi`, `pages`, `volume`, `number`, `publisher`. Citekeys follow the
Paperpile convention `Surname<Year>-XXxx` (e.g. `Taylor1989-ox`).

The Paperpile export style places successive entries on adjacent
lines separated by `}\n,\n@TYPE{...}` rather than a blank line; this
parses fine in `bibtex` / `biber` / `RefManageR` but means tools that
expect entries to start at column 0 after a newline may need a quick
pre-pass (e.g. `tr ',' '\n'` or a BibTeX-aware parser) to enumerate
them.

## Usage

```bash
# Re-render the paper with this bibliography:
biber --tool --output-file=bibliography_clean.bib bibliography.bib
# or simply pass to LaTeX:
%   \bibliography{bibliography}
```

In R via `RefManageR`:

```r
library(RefManageR)
bib <- ReadBib("bibliography.bib")
```

## Related files

- `acknowledgements.md` — the front-matter content (authors, funding,
  consortium, acknowledgements) that the bibliography supports.
- The paper PDF and Extended Data — not deposited here; consult the
  publisher of record.

## Notes

- The bibliography is a snapshot at the time of upload; the canonical
  reference list in the published paper is authoritative if the two
  disagree.
- Some entries contain unicode characters in author names; ensure your
  consumer handles UTF-8 correctly.
- No DOIs are missing from the entries deliberately; if a DOI is absent,
  the cited work does not have one indexed by Paperpile.
