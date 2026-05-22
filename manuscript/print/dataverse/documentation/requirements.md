---
# API metadata sent with the file in a single POST to /api/datasets/:persistentId/add
# (description, categories, directoryLabel, restrict, tabIngest are the recognized
# per-file fields in jsonData; everything else here is local bookkeeping.)
filename: requirements.txt
local_path: /Users/papers/BANC-project/requirements.txt
size_bytes: 7719
md5: c92b8948715c1566b24360c4b866590e
content_type: text/plain

# --- Fields posted to Dataverse ---
description: >-
  Runtime requirements for the BANC-Project codebase that produced the
  paper "Distributed control circuits across a brain-and-cord connectome"
  (Bates, Phelps, Kim, Yang et al., 2026). Lists the R and Python
  interpreter versions used, every R library and Python package called
  during execution (with installed versions), and an install recipe that
  walks through (a) the R toolchain and CRAN packages, (b) the natverse
  + lab-fork packages installed from source, (c) the Python conda
  environment with the scientific stack + connectomics infra
  (caveclient, cloud-volume, seatable-api, leidenalg, umap-learn,
  gcsfs), and (d) the one-time reticulate::use_python() pointer plus a
  smoke test. The pipeline is driven from R; Python is called under the
  hood via reticulate for the influence calculator, spectral
  clustering, cascade model, betweenness centrality, GCS-backed feather
  reads, and SeaTable I/O.
categories:
  - Documentation
directoryLabel: documentation
restrict: false
tabIngest: false
---

# requirements.txt

## Purpose

Single-source-of-truth pin file for the BANC analysis pipeline. Records the
exact R / Python interpreter versions and every library/package that the
codebase calls during a normal run, so reviewers, data users, and future
maintainers can reproduce the environment that generated the figures and
numbers in the paper.

## What's included

- R 4.2.1 (interpreter), pinned versions for ~80 R packages used either as
  `library()` calls in scripts or as `pkg::fn()` namespaced calls.
- Python 3.12.2 (interpreter), pinned versions for the scientific stack
  (numpy, pandas, scipy, matplotlib, scikit-learn) plus connectomics infra
  (caveclient, cloud-volume), graph + clustering (python-igraph, leidenalg,
  umap-learn), SeaTable client (seatable-api) and GCS (gcsfs).
- Source URLs for the GitHub-only natverse + lab-fork R packages (`nat`,
  `nat.flybrains`, `nat.nblast`, `nat.templatebrains`, `nat.ggplot`,
  `fafbseg`, `malevnc`, `influencer`, `bancr`).
- An install recipe at the bottom: CRAN install, natverse install via
  `devtools::install_github()`, conda env for Python, and the one-time
  `reticulate::use_python(...)` step to wire R ↔ Python.

## Columns / fields

Not tabular — pin file in pip-style `name version` format, with
section-header comments for grouping. The trailing install-recipe block is
shell + R commands wrapped in `#` comment lines (so the whole file is still
parseable as a pip requirements file if you strip the non-CRAN entries).

## Related files in this dataset

- `acknowledgements.md` — front-matter (authors, funding, contributions).
- `bibliography.bib` — BibTeX bibliography for every paper cited.
- The BANC-Project codebase itself is hosted at the GitHub repo named in
  the paper's Data Availability statement.
