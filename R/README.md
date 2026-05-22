# R/ — analysis pipeline for the BANC paper

The R code in this directory produces every figure panel and statistical
output reported in *Distributed control circuits across a brain-and-cord
connectome* (Bates, Phelps, Kim, Yang et al., 2026). Each subdirectory has a
single responsibility:

- **[`startup/`](startup/)** — configuration + helpers + data loaders.
  Sourced by every figure script. Includes the snapshot/live dispatcher
  for `banc.meta` and `franken.meta`, the GCS / arrow utilities, the
  private-key loader (`load-keys.R`), and the canonical paper colours +
  ordering vectors.
- **[`figures/`](figures/)** — one `panels_*.R` script per figure or per
  closely-related panel group. Reads `banc.meta` and (where needed)
  `banc.edgelist.simple`, then writes the per-panel PDFs and statistical
  sidecar `.txt` files into `figures/figure_N/links/`. The script name
  describes the analysis, not the figure number — see the table in
  [`figures/README.md`](figures/README.md) for the figure → script
  mapping.
- **[`text/`](text/)** — non-figure outputs: `numbers.R` compiles the
  in-text statistics into `manuscript/print/numbers.csv` and refreshes
  the corresponding Google Sheet; `supplemental_data.R` writes the ten
  supplementary CSV tables; `nblast_top_match_correct.R` recomputes the
  cross-dataset NBLAST match accuracy; `ngl_links.R` regenerates the
  Neuroglancer state URLs cited in figure legends.
- **[`annotations/`](annotations/)** — scripts that **write live** to
  SeaTable or rebuild the interactive cluster tools. Not auto-loaded.
  Run by hand, as documented in [`annotations/README.md`](annotations/README.md).

## How a figure is produced

Every figure script in `R/figures/` follows the same skeleton:

```r
# 1. Load configuration + data
source("R/startup/banc-startup.R")     # paths, env vars, helpers
source("R/startup/banc-meta.R")        # banc.meta + derived per-class subsets
source("R/startup/banc-edgelist.R")    # banc.edgelist.simple (if needed)

# 2. Build the panel
# ... ggplot / pheatmap / etc. ...

# 3. Save next to the .ai file that links it
ggsave(file.path(banc.fig2.path, "panel_name.pdf"), p, ...)
```

`banc.fig1.path`, `banc.fig2.path`, … are defined in `banc-startup.R` and
point at `figures/figure_N/links/`. Supplementary panels go to
`banc.figN.supp.path` (which is `links/supplement/`). Overflow analyses
go to `banc.figN.extra.path` (`links/extra/`).

## Reproducing a specific figure

The single-line recipe per figure is:

```bash
BANC_NCORES=1 Rscript R/figures/panels_<analysis>.R
```

For example:

```bash
BANC_NCORES=1 Rscript R/figures/panels_inventory.R              # Figure 1
BANC_NCORES=1 Rscript R/figures/panels_influence_validation.R   # Figure 2c, ED 4b
BANC_NCORES=1 Rscript R/figures/panels_betweenness_layers.R     # Figure 3a
BANC_NCORES=1 Rscript R/figures/panels_an_dn_umap.R             # Figure 3d
BANC_NCORES=1 Rscript R/figures/panels_cluster_sensory_correlations.R  # Figure 4a
BANC_NCORES=1 Rscript R/figures/panels_cns_networks.R           # Figure 6
```

`BANC_NCORES=1` forces sequential mode for the parallel influence
calculator (PSOCK workers accumulate memory on this machine; sequential
mode is faster and predictable in practice).

## Live-data refresh

Analyses default to the committed parquet snapshot at
`data/meta/banc_888_meta_<YYYYMMDD>.parquet`. To pull fresh annotations
from SeaTable + GCS (and refresh the snapshot):

```bash
BANC_LIVE=1 Rscript R/startup/banc-meta-live.R
```

This writes a freshly-dated snapshot under `data/meta/`. The dispatcher
at `R/startup/banc-meta.R` then loads the newest snapshot on next
source. The live SeaTable + GCS code is also the reference for
**what** went into the snapshot — see comments in `banc-meta-live.R`
for the column-coalescing priority.

> ⚠️ **SeaTable access is restricted to the BANC core team.** The live
> refresh authenticates against the internal `cns_meta` SeaTable
> workspace via `bancr::banctable_query()`, which requires a SeaTable
> API token in `~/.bancr.config.yml`. External users who clone this
> repo do **not** need this — the committed parquet snapshot in
> `data/meta/` is the canonical metadata for paper reproduction, and
> every script in `R/figures/` loads from it by default. The
> `BANC_LIVE=1` path (and everything in [`annotations/`](annotations/))
> is only useful if you can authenticate to BANC's internal SeaTable.

## Upstream pipeline

The cached connectivity feathers, per-neuron metrics, betweenness CSVs,
spectral-clustering CSVs, cascade pickles, and NBLAST similarity
matrices that the R scripts here consume are **produced by
[`bancpipeline`](https://github.com/htem/bancpipeline)** running on the
HMS O2 cluster and published to GCS under
`gs://lee-lab_brain-and-nerve-cord-fly-connectome/compiled_data/banc_<NNN>/`.

Each `data/<subdir>/README.md` cross-references the producing
bancpipeline script for that subdir. The pattern is:

```
bancpipeline (HMS O2 / SLURM) ─ produces ─▶ GCS feathers / parquets
                                                │
                                                ▼
                            BANC-project (this repo) ─ consumes ─▶ figures / numbers
```

If a `data/<subdir>/` artefact looks stale or wrong, the fix lives in
bancpipeline, not here. The R-side scripts re-pull from GCS on next
session source (or via `BANC_LIVE=1` for the metadata layer).
