---
filename: influencer_archive.zip
upstream_url: https://github.com/natverse/influencer
default_branch: main
pinned_commit: 2519c57
pinned_date: 2026-04-13
release_tag: (none — pinned to main HEAD)
license: GPL-3.0
language: R
content_type: application/zip

# --- Fields posted to Dataverse ---
description: >-
  Snapshot of the influencer R package, an R port and wrapper of the Drugowitsch lab's ConnectomeInfluenceCalculator (Zenodo DOI 10.5281/zenodo.15999930). Provides two parallel implementations of the linear-dynamical-systems influence model used throughout the BANC paper: a native R backend in influence_calculator_r() built on Matrix and RSpectra, and a Python backend in influence_calculator_py() that calls PETSc / SLEPc through reticulate. Each calculator object exposes a calculate_influence() method (with calculate_influence_py() as the Python-backed wrapper) that returns the per-seed steady-state response, plus a data.table-based adjust_influence() that groups targets and recomputes per-group adjusted scores (~10-50x faster than the pure-R equivalent on the BANC edgelist). influencer caches the eigenvalue decomposition between seeds, yielding a large speedup on subsequent seed queries against the same connectome. It is the engine behind every adjusted-influence number cited in the paper and behind the precomputed parquet tables in this Dataverse. Citable via Zenodo DOI 10.5281/zenodo.15999929 (https://doi.org/10.5281/zenodo.15999929). Distributed here as a ZIP pinned to the main-branch HEAD at upload date; the package continues to evolve at the GitHub URL above.
categories:
  - Code
directoryLabel: code
restrict: false
tabIngest: false
---

# influencer_archive.zip

## Purpose

The influencer package is the in-R home of the influence calculation
used throughout the BANC paper. Influence here is the steady-state
response of a target neuron to a sustained signal injected at a seed,
derived from the synaptic weight matrix through a linear dynamical
system (Bates et al. 2020; Drugowitsch lab formulation). influencer
provides both a self-contained R implementation and a thin wrapper
around the Python PETSc / SLEPc backend, lets the user pick whichever
fits the size of the problem, and provides the data.table-based
aggregation that the paper's figure scripts rely on.

## Provenance

Developed by Alexander Bates with the natverse organisation, building
on Zaki Ajabi and Jan Drugowitsch's
ConnectomeInfluenceCalculator (deposited separately here as
`connectome_influence_calculator_archive.zip`) and on guidance from
Rachel Wilson. The package matured during the BANC paper effort; the
v888 figure pipeline calls `influence_calculator_py(count_thresh = 5)`
explicitly at every site.

## Repository contents

- `R/` — package source: `influence-calculator-r.R` (R6 R backend with eigenvalue cache and sparse-matrix path), `python-wrapper.R` (PETSc / SLEPc wrapper invoked through reticulate), `install-python.R` (one-shot installer for the Python dependencies), `utils.R` (including the data.table `adjust_influence()` aggregator), `influencer-package.R`, `zzz.R`.
- `man/` — roxygen function reference.
- `tests/` — testthat coverage of the R and Python backends, including cross-implementation correlation checks on the BANC edgelist.
- `inst/` — bundled images and example assets.
- `vignettes/` — pkgdown article(s) walking through a BANC analysis.
- `DESCRIPTION`, `NAMESPACE`, `README.md`, `LICENSE.md`, `_pkgdown.yml` — package metadata and the upstream pkgdown site.

## Usage

```r
remotes::install_github("natverse/influencer")
library(influencer)
ic <- influence_calculator_py(edgelist_simple = banc_edges,
                              meta = banc_meta,
                              count_thresh = 5)
infl <- calculate_influence_py(ic = ic, seed_ids)
adj  <- adjust_influence(infl, group_by = "cell_sub_class")
```

For the BANC paper the Python backend is preferred — it is faster on the full BANC edgelist and is the path actually used by bancpipeline.

## Related files

- `connectome_influence_calculator_archive.zip` — Python original;
  influencer's `influence_calculator_py()` calls into it.
- `bancr_archive.zip` — bancr's `banc_influence()` /
  `banc_influence_loop()` are the BANC-specific wrappers over
  influencer.
- `influence_all_to_effector_subclass.parquet`,
  `influence_sensory_subclass_to_all.parquet`,
  `influence/all_to_all/` — pre-computed influence outputs produced by
  bancpipeline using this package.

## Notes

- This Dataverse copy is a point-in-time snapshot pinned to commit `2519c57` on the `main` branch (13 April 2026). The package has no formal release tag at upload time; the upstream README carries a Zenodo DOI badge that can be cited as a stable handle. For new work, prefer the GitHub URL plus a commit SHA.
- Licensed under GPL-3.0; redistribution and derivative works are permitted under those terms.
- The Python backend requires a working PETSc / SLEPc build. `install_python_influence_calculator()` in the package handles this for the r-reticulate conda environment (Homebrew or conda-forge installs of PETSc / SLEPc + their petsc4py / slepc4py wrappers, with a fallback to local source builds). The pure-R backend has no such requirement but is slower on very large edgelists.
- `count_thresh = 5` is the canonical filter used in the BANC paper. The package default is `0` so that any filtering of edges is visible at the call site.
