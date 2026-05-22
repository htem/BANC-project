# `data/meta/` — bundled metadata snapshot + auxiliary lookups

The committed BANC metadata **snapshot** that lets the repo be cloned
and run with no live SeaTable / GCS access, plus a couple of auxiliary
lookup tables.

## Layout

| File | Tracked? | What it is |
| --- | --- | --- |
| `banc_888_meta_<YYYYMMDD>.parquet` | ✅ | The committed BANC metadata snapshot (one row per neuron, all ~165 columns). Only **the latest snapshot** is retained — older dated snapshots are deleted when a new one is written. The dispatcher at `R/startup/banc-meta.R` loads this file by default. |
| `banc_neck_inclusion.csv` | ✅ | Triaged neck-connective inclusion list — which AN/DN candidates pass the morphology checks for the PCA-UMAP clustering. Hand-curated; columns: `root_id`, `in_group`, plus QC notes. |
| `banc_sensor_neck_inclusion.csv` | ✅ | (Currently empty placeholder; will hold the equivalent triage list for sensory ANs once their roster is finalised.) |
| `bc_orig_cache.feather` | ❌ (gitignored) | Local cache of the most-recent SeaTable pull, written by `R/startup/banc-meta-live.R`. Auto-managed. |
| `fafb_schlegel_et_al_2024_meta.tsv`, `hemibrain_nt_meta_2024-02-01.csv`, `manc_v.1.2_meta.csv` | ❌ (gitignored) | Local copies of external metadata feathers — pulled on demand, safe to delete. |

## Snapshot vs live

`R/startup/banc-meta.R` is a thin dispatcher. By default it loads the
latest parquet snapshot from this directory. Set `BANC_LIVE=1` to force
a fresh SeaTable + GCS pull via `R/startup/banc-meta-live.R`; on a
successful live pull, that script writes a fresh snapshot here and
deletes the previous one.

## Coalesce priority

When the live loader merges SeaTable's manual curations with the GCS
segmentation-properties feather and frankenbrain's cross-dataset matches,
column priority is:

| Column kind | Priority |
| --- | --- |
| Manual curations (`cell_type`, `cluster`, `super_cluster`, `cell_function`, `body_part_*`, `super_class`, …) | SeaTable > GCS > franken |
| `proofread` (segmentation property, not a curation) | GCS > SeaTable > franken |

This is load-bearing — changing it would silently re-classify thousands
of neurons. See `R/startup/banc-meta-live.R` for the implementation.

## Consumers

Every analysis script. `banc.meta` is the canonical handle (post-load).
The derived per-class dataframes (`banc.an.meta`, `banc.dn.meta`,
`banc.eff.meta`, `banc.sens.meta`, `banc.vpn.meta`, `banc.neck.meta`)
are built at the tail of `R/startup/banc-meta-live.R` and run in both
the snapshot and live paths.
