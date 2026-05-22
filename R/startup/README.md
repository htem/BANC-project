# R/startup/ — configuration, helpers, and data loaders

Sourced by every figure-panel script. Splits into three small kinds of
file:

## Configuration + helpers (always sourced first)

- `banc-startup.R` — paths, environment variables, version selection,
  small helpers. The single entry point — every figure script begins
  with `source("R/startup/banc-startup.R")`.
- `banc-functions.R` — general helpers used across panels.
- `gcs-helpers.R` — GCS + arrow utility wrappers.
- `load-keys.R` — reads `data/private/keys.csv` into `banc.keys` (Google
  Doc / Sheet / Drive IDs). The CSV is gitignored.

## Data loaders (sourced on demand)

- `banc-meta.R` — dispatcher. Loads `banc.meta` from the committed
  parquet snapshot at `data/meta/banc_888_meta_<YYYYMMDD>.parquet`.
  Sources `banc-meta-live.R` (which guards its live block) so the
  derived per-class subsets always run regardless of path.
- `banc-meta-live.R` — the live SeaTable + GCS pipeline that produces
  the snapshot. Skipped when `banc.meta` is already loaded; run
  end-to-end when `BANC_LIVE=1`. At the end of a live run it writes a
  freshly-dated parquet under `data/meta/`. Also contains the derived
  per-class data frames (banc.an.meta, banc.dn.meta, banc.eff.meta,
  banc.sens.meta, banc.vpn.meta, banc.neck.meta) and the canonical
  ordering vectors (super.clust.order, cns.network.order,
  eff.super.order).
- `franken-meta.R` / `franken-meta-live.R` — same dispatcher pattern
  for the cross-dataset matching layer. Snapshot at
  `data/meta/franken_meta_<YYYYMMDD>.parquet`.
- `banc-edgelist.R` — neuron-to-neuron edgelist loader. Prefers
  the local cache under `data/cache/`; falls back to the GCS
  `compiled_data/banc_888/banc_888_edgelist_simple_v2.feather` and
  caches the result locally.
- `banc-distances.R` — cascade / signal-propagation distance loader
  (the `frankenbrain_v1.6` PKLs). Used by
  `R/figures/panels_influence_validation.R`.
- `banc_an_dn_data.R` — AN/DN-specific derived frames (cluster
  assignments, UMAP coords) used by Fig 3 + Fig 4 scripts.

## Where the cached data comes from

The feathers and parquets that the data loaders here pull from GCS are
**produced by [`bancpipeline`](https://github.com/htem/bancpipeline)**
running on the HMS O2 cluster. The GCS prefix is
`gs://lee-lab_brain-and-nerve-cord-fly-connectome/compiled_data/banc_<NNN>/`.

Mapping from loader to producing bancpipeline script:

| Loader | Reads | Produced by |
| --- | --- | --- |
| `banc-meta-live.R`    | `banc_<NNN>_meta.feather`               | `bancpipeline/banc/meta/banc-data.R` |
| `franken-meta-live.R` | `franken_meta.feather`                  | `bancpipeline/banc/franken/` |
| `banc-edgelist.R`     | `banc_<NNN>_edgelist_simple_v2.feather` | `bancpipeline/banc/metrics/banc-calculate-connectivity.R` |
| `banc-distances.R`    | `frankenbrain_v1.6/*.pkl`               | `bancpipeline/analysis/python/cascade_model.py` |

If a cached file looks stale or wrong, fix it in bancpipeline (not in
this repo); the loaders here re-pull on next session source.

## Conventions

- Loaders write a one-line `message()` describing what they loaded so a
  failing source is easy to trace from a script's stdout.
- All loaders are idempotent: re-sourcing them in the same R session
  reloads from the snapshot (no double-fetch from SeaTable).
- The `BANC_VERSION` env var selects the materialization (default
  `banc_888`); use `BANC_VERSION=banc_NNN` to point at a different one.
