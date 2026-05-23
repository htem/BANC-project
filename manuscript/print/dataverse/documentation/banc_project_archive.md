---
filename: banc_project_archive.zip
upstream_url: https://github.com/htem/BANC-project
default_branch: main
pinned_commit: 2885e4b
pinned_date: '2026-05-22'
release_tag: (none — pinned to main HEAD)
license: GPL-3.0
language: R
content_type: application/zip
description: '**Canonical citation:** the authoritative archive for this software is on Zenodo (DOI https://doi.org/10.5281/zenodo.20350641). Cite the Zenodo DOI rather than this Dataverse copy — the Dataverse archive is a byte-for-byte snapshot pinned to the main-branch HEAD on the upload date, mirrored here for one-stop replication. Snapshot of BANC-project, the R analysis code and Adobe Illustrator plot files that together produced every figure in the BANC paper. Each figure of the manuscript has a matched directory under figures/figure_N/ holding the assembled .ai file(s), source PDF / PNG panels under links/, supplementary panels under links/supplement/, and exploratory or dark-mode variants under links/extra/. The R scripts that generate those panels live under R/figures/ (one panel_*.R or panels_*.R script per figure or per coherent set of panels), with shared infrastructure under R/startup/ (banc-startup.R, banc-meta.R, banc-edgelist.R, banc-functions.R) and text-side helpers under R/text/ (numbers.R, ngl_links.R) that produce the paper-cited number table and the Neuroglancer link manifest. The repository depends on bancr, influencer and the natverse stack. Developed at Harvard Medical School in the Wilson and Lee labs by Alexander Bates and collaborators. Distributed here as a ZIP pinned to commit e27bfa8 on main (24 February 2026); the live repository continues to evolve at the GitHub URL above.'
categories:
- Code
directoryLabel: code
restrict: false
tabIngest: false
---
# banc_project_archive.zip

## Purpose

BANC-project is the figure-and-prose codebase for the BANC paper. It
takes the compiled-data products deposited elsewhere in this
Dataverse — `banc_888_meta.feather`, the edgelists, synapses,
influence parquets, NBLAST feathers — and turns them into the
specific panels, stats summaries and Neuroglancer links that appear
in the manuscript. Users who want to reproduce a specific figure, or
who want to lift a methodological recipe (clustering, statistics,
panel layout) from the paper, should consult this archive.

## Provenance

Developed at Harvard Medical School in the Wilson and Lee labs.
Lead figure scripts authored by Alexander Bates; analysis
infrastructure (`R/startup/`) shared across the lab. The repository
sits downstream of bancpipeline (which produces the compiled data
products) and depends on bancr (data access), influencer (influence
computations) and the natverse stack (3D and 2D neuron plotting).

## Repository contents

- `R/figures/` — one panel script per figure or per coherent set of panels (e.g. `panels_an_dn_umap.R`, `panels_cns_network_analyses.R`, `panels_sensory_motor.R`, `panels_efferent_umap.R`, `panels_transmitter_predictions.R`, `panels_pre_effector_influence.R`, `panels_mbx_cx_control.R`, `panels_body_parts.R`, `panels_cell_type_blowouts.R`, `panels_an_dn_neuroanatomy.R`).
- `R/startup/` — shared infrastructure: `banc-startup.R` loads every library and metadata table; `banc-meta.R`, `banc-edgelist.R`, `banc-functions.R` implement the canonical metadata join, edgelist load and statistical helpers; `banc-cluster-update.R` is the SeaTable mutation surface (run by hand, not auto-sourced).
- `R/text/` — `numbers.R` (writes every figure-cited number to a numbers CSV and to a paired Google Sheet); `ngl_links.R` (builds the Neuroglancer-link manifest for the paper).
- `figures/` — per-figure folders holding `.ai` master files and `links/` subdirectories of source panels.
- `data/` — small derived CSV / feather files used by the figure scripts (cluster assignments, neck-functional-class tables, determined thresholds).
- `settings/` — colour palettes (`paper_colours_lacroix.csv`) and other plot defaults.
- `python/`, `matlab/` — companion analysis scripts (betweenness, spectral clustering and other non-R helpers).
- `images/`, `citations/`, `submission/`, `acknowledgements.md`, `LICENSE`, `README.md` — supporting documents and project metadata.

## Usage

```r
# From the repository root, with bancr + influencer installed:
source("R/startup/banc-startup.R")
source("R/figures/panels_cns_network_analyses.R")
```

Each figure script writes its output panels into
`figures/figure_N/links/` (and `links/supplement/` for extended-data
panels). The figure asset organisation conventions are documented
at the top of `CLAUDE.md` in this repository.

To regenerate the per-figure-cited numbers:

```r
source("R/text/numbers.R")
```

## Related files

- `bancr_archive.zip` — used by every figure script for data access.
- `influencer_archive.zip` — used wherever
  `query_influence(... count_thresh = 5)` is called.
- `bancpipeline_archive.zip` — upstream of every compiled-data file
  this repository reads.
- `banc_888_meta.feather`, the edgelists, synapse parquets, NBLAST
  feathers and influence parquets — the data layer this repository
  consumes.

## Notes

- This Dataverse copy is a point-in-time snapshot pinned to commit `e27bfa8` on the `main` branch (24 February 2026). For citation and for any new work, prefer the live GitHub URL plus a commit SHA over the Dataverse archive — the live repository has continued to evolve, notably for the v888 data migration.
- Licensed under GPL-3.0; redistribution and derivative works are permitted under those terms.
- The repository assumes a working bancr installation (which in turn requires CAVE credentials for some live queries) and access to the compiled-data files in this Dataverse. Running the figure scripts end-to-end against live CAVE is slow (hours to a day); most users will instead point them at the local copies of the feathers and parquets from this deposit.
- `panels_an_dn_neuroanatomy.R` is by far the heaviest script (~12-20 hours for full mesh rendering, ~23 GB peak memory) and must be run on its own; see the script header for the staged procedure.
