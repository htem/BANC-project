# R/annotations/ — live writers

> **These scripts modify live data sources.** They are **not** auto-loaded
> by the figure pipeline and never run as part of the reproduce-a-figure
> recipe. Run them by hand, with intent.

The figure scripts in `R/figures/` are strictly read-only against SeaTable
and CAVE. The scripts here are different — they push annotations back to
SeaTable, rebuild the cluster-tool HTML widgets, or write the spreadsheet
of allowed annotation terms. Keeping them in a separate folder makes it
clear at a glance that running one of these will change shared state.

## Scripts

- **`banc-cluster-update.R`** — pushes the curated AN/DN cluster /
  super_cluster mappings (`ct_mp_to_super`, the canonical paper
  vocabulary) to SeaTable. The mapping lives at the top of this file
  and is the source of truth for the `super_cluster` column users see
  in Codex.
- **`banc-spectral-cluster-update.R`** — pushes the per-neuron CNS
  network labels (from spectral clustering on the v2 graph) to
  SeaTable.
- **`rebuild_interactive_tools.R`** — rebuilds the HTML widget bundles
  for the AN/DN, EFF, and CNS-network UMAP browsers from the current
  SeaTable annotations. Outputs to `figures/figure_3/links/extra/` and
  related extras dirs; the published widgets are versions of these.
- **`make_annotation_terms_spreadsheet.R`** — produces the spreadsheet
  of allowed values for each annotation column (super_class, cell_class,
  hemilineage, neuropil, etc.). Used to keep SeaTable column vocabulary
  consistent across annotators.

## Authentication

All four scripts authenticate to SeaTable via `bancr::banctable_query()`
(read) and `bancr::banctable_update_rows()` (write). The credentials are
in `~/.bancr.config.yml` (per user; not in the repo). If you do not
have write access to BANC's SeaTable workspace these scripts will fail
fast at the write step.
