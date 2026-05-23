---
filename: bancr_archive.zip
upstream_url: https://github.com/natverse/bancr
default_branch: main
pinned_commit: 9139f94
pinned_date: '2026-05-22'
release_tag: (none — pinned to main HEAD past v0.2.1; DESCRIPTION reports 0.3.0)
license: GPL-3.0
language: R
content_type: application/zip
description: '**Canonical citation:** the authoritative archive for this software is on Zenodo (DOI https://doi.org/10.5281/zenodo.20350647). Cite the Zenodo DOI rather than this Dataverse copy — the Dataverse archive is a byte-for-byte snapshot pinned to the main-branch HEAD on the upload date, mirrored here for one-stop replication. Snapshot of the bancr R package, the primary user-facing client for the BANC (Brain And Nerve Cord) connectome of an adult female Drosophila melanogaster. bancr wraps CAVE, SeaTable, GCS and synapse-query endpoints behind a small set of R functions that return tibbles, neuron objects and ggplot scenes. Headline calls used throughout the paper include banc_meta() for the per-neuron metadata table, banc_partners() and banc_edgelist() for connectivity, banc_influence() for pre-computed adjusted-influence pairs, banc_nblast_matches() for cross-dataset morphological matches, banc_read_neuron_meshes() and banc_read_l2skel() for 3D morphology, banc_to_JRC2018F() and banc_mirror() for template-space transforms, and banc_view() / banc_front_view() / banc_vnc_view() for canonical scene angles. bancr depends on the natverse stack (nat, fafbseg, nat.jrcbrains, hemibrainr) and uses reticulate to reach the fafbseg-py / CAVEclient Python layer. Initialised from Greg Jefferis''s fancr package and developed at Harvard by Alexander Bates with the Wilson and Lee labs. Distributed here as a ZIP pinned to commit c594c91 on main; the live package continues to evolve at the GitHub URL above.'
categories:
- Code
directoryLabel: code
restrict: false
tabIngest: false
---
# bancr_archive.zip

## Purpose

bancr is the R client through which almost every figure in the BANC
paper reaches its underlying data. It exposes the BANC connectome — the
v888 metadata table, edgelists, synapse queries, influence scores, NBLAST
matches and 3D morphology — as plain R objects, and provides the small
set of plotting and template-space helpers used to render BANC neurons
in figure panels. Users who want to reproduce or extend the paper's
analyses should install bancr first and use it as the entry point to
every other file in this deposit.

## Provenance

Developed at Harvard Medical School in the Wilson and Lee labs, with
acknowledgement to Greg Jefferis (MRC LMB) whose fancr package
(`https://github.com/natverse/fancr`) served as the structural
template. bancr authenticates against CAVE, the BANC SeaTable, and
Google Cloud Storage; it is the same package returned by
`remotes::install_github("natverse/bancr")`.

## Repository contents

- `R/` — package source, with files covering CAVE / SeaTable access, synapse and partner queries, influence wrappers, NBLAST-match lookups, plotting and template-space transforms.
- `data-raw/` — controlled-vocabulary CSVs and the glossary that documents the metadata columns surfaced by `banc_meta()`.
- `data/` — packaged R objects (e.g. `banc.surf`, `banc_volumes.df`, lookup tables).
- `man/` — roxygen-generated function reference.
- `inst/` — non-R assets (logos, sample data).
- `tests/` — testthat suite.
- `DESCRIPTION`, `NAMESPACE`, `NEWS.md`, `LICENSE.md`, `_pkgdown.yml`, `README.md` — package metadata and the upstream pkgdown site.

## Usage

```r
remotes::install_github("natverse/bancr")
library(bancr)
m <- banc_meta()                       # per-neuron metadata table
e <- banc_edgelist()                   # neuron-to-neuron edgelist
inf <- banc_influence(some_root_ids)   # adjusted-influence pairs
n <- banc_read_l2skel("720575941521131930")
plot3d(n)
```

The full column-level documentation for `banc_meta()`, `banc_edgelist()`,
`banc_influence()`, `banc_nblast_matches()` and the per-synapse query
functions lives in the package roxygen and in
`data-raw/banc_codex_annotations_system.md`.

## Related files

- `banc_888_meta.feather` — the table returned by `banc_meta()`.
- `banc_888_edgelist_simple_v[23].feather` — returned by
  `banc_edgelist()`.
- `influence_*.parquet` — pre-aggregated tables read by
  `banc_influence_to_effectors()` and related helpers.
- `influencer_archive.zip` — the influence-calculator package called
  by bancr's `banc_influence_loop()`.
- `bancpipeline_archive.zip` — the upstream pipeline that produces every
  feather / parquet bancr consumes.
- `nat_ggplot_archive.zip` — natverse's ggplot2 neuron-plotting layer
  used by bancr's `plot()` methods.
- `fly_connectome_data_tutorial_archive.zip` — R + Python tutorial
  using bancr alongside the FAFB / MANC / maleCNS / Hemibrain clients,
  and the third-party documentation source for BANC tables.

## Notes

- This Dataverse copy is a point-in-time snapshot pinned to commit `c594c91` on the `main` branch (13 May 2026), which sits past the latest release tag (`v0.2.1`, 22 August 2025; the in-tree `DESCRIPTION` reports development version 0.3.0). For citation and for any new work, prefer the live GitHub URL plus a commit SHA over the Dataverse archive.
- Licensed under GPL-3.0; redistribution and derivative works are permitted under those terms.
- bancr is the R-side counterpart to Jasper Phelps's Python toolkit (`the-BANC-fly-connectome`, also deposited here). The two libraries are independent and target different downstream workflows.
