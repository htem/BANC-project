---
filename: nat_ggplot_archive.zip
upstream_url: https://github.com/natverse/nat.ggplot
default_branch: main
pinned_commit: 6112a74
pinned_date: 2026-02-16
release_tag: (none — pinned to main HEAD)
license: MIT
language: R
content_type: application/zip

# --- Fields posted to Dataverse ---
description: >-
  Snapshot of nat.ggplot, the natverse helper package for publication-quality 2D renderings of neurons and brain meshes using ggplot2. Where natverse's stock plotting functions target rgl / plotly for 3D scenes, nat.ggplot projects skeletons, meshes, synaptic sites and compartment labels into 2D and exposes them as ggplot2 layers. Headline calls: ggneuron() for a complete 2D neuron-with-mesh scene with sensible defaults, geom_neuron() as a ggplot2 geometry layer for assembling custom panels, and gganat as the base ggplot object that supplies the projection state. The package works with any neuroanatomy that can be expressed as a neuron / neuronlist / mesh3d (its README uses BANC sample data, but the same code path handles FAFB, MANC, Hemibrain or any organism). nat.ggplot is the rendering backend behind the great majority of 2D neuron panels in the BANC paper figures — neuron silhouettes, AN / DN body-part summaries, cluster-level morphological galleries — and pairs naturally with bancr's banc_to_JRC2018F() to render BANC neurons in any of the shared template spaces. Authored by Alexander Bates in Rachel Wilson's lab at Harvard Medical School. Distributed as a ZIP pinned to the main-branch HEAD at upload date; the package continues to evolve at the GitHub URL above.
categories:
  - Code
directoryLabel: code
restrict: false
tabIngest: false
---

# nat_ggplot_archive.zip

## Purpose

nat.ggplot makes it possible to plot a neuron the way the paper
displays it — in 2D, in a chosen anatomical orientation, in the same
visual idiom as every other panel in the manuscript. It bridges
natverse's 3D neuron and mesh objects with ggplot2's layered grammar,
letting figures combine neuron skeletons, neuropil silhouettes,
synaptic sites and compartment annotations with arbitrary ggplot
geometries (text annotations, scale bars, statistical overlays).

## Provenance

Authored by Alexander Bates during his time in Rachel Wilson's lab at
Harvard Medical School and developed as part of the natverse
ecosystem (`https://natverse.org`). nat.ggplot grew out of the
plotting needs of the BANC paper and the projects that preceded it.

## Repository contents

- `R/` — package source with the high-level scene helper (`ggneuron()`), the ggplot2 geometry layer (`geom_neuron()`), the base ggplot object (`gganat`) and the projection helpers.
- `man/` — roxygen function reference.
- `data/`, `data-raw/` — example neuron / mesh objects bundled with the package (used in the README and vignettes).
- `inst/` — example data and reference images.
- `vignettes/`, `docs/` — pkgdown article sources and the rendered site.
- `tests/` — testthat coverage of the projection and geom paths.
- `DESCRIPTION`, `NAMESPACE`, `README.md`, `LICENSE`, `LICENSE.md`, `_pkgdown.yml` — package metadata.

## Usage

```r
remotes::install_github("natverse/nat.ggplot")
library(nat.ggplot); library(bancr)
n <- banc_read_l2skel("720575941521131930")
ggneuron(n, view = "front")
```

To assemble a custom panel:

```r
library(ggplot2)
ggplot() +
  geom_neuron(neurons, color = "grey20") +
  geom_neuron(highlight, color = "tomato") +
  theme_void()
```

## Related files

- `bancr_archive.zip` — bancr's `banc_view()`, `banc_front_view()`,
  `banc_vnc_view()` configure nat.ggplot scenes with the canonical
  BANC camera angles used in the paper.
- `banc_888_meta.feather`, `banc_swc_skeletons.zip` — the metadata
  and morphology consumed by nat.ggplot's neuron-plotting layers.
- `banc_project_archive.zip` — figure scripts in `R/figures/`
  produce all paper-bound 2D panels through nat.ggplot.

## Notes

- This Dataverse copy is a point-in-time snapshot pinned to commit `6112a74` on the `main` branch (16 February 2026). The package has no formal release tag at upload time; prefer the GitHub URL plus a commit SHA for citation.
- Licensed under MIT (`LICENSE.md`); the natverse ecosystem standardises on MIT and the upstream repository ships both `LICENSE` and `LICENSE.md` carrying the same terms. Redistribution is permissive under MIT.
- nat.ggplot relies on a recent ggplot2 and on the natverse core (`nat`, `nat.flybrains`). For 3D scenes, use natverse's rgl / plotly paths instead; nat.ggplot is intentionally 2D-only.
