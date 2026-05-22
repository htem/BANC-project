---
filename: synapse_neuropil_lookup_v2.parquet
gcs_path: gs://lee-lab_brain-and-nerve-cord-fly-connectome/synapses/v2.0/synapse_neuropil_lookup_v2.parquet
size_bytes: 2093290134
size_human: 1.95 GB
nrows: 213762397
ncols: 4
content_type: application/x-parquet

# --- Fields posted to Dataverse ---
description: >-
  Per-synapse neuropil-and-region lookup for the v2 synapse snapshot.
  213,762,397 rows by 4 columns: synapse `id`, FlyWire-style `neuropil`
  label, coarse CNS `region`, and `side`. Built by overlaying every v2
  synapse coordinate against the BANC neuropil mesh segmentation, so
  that downstream tables can label synapses by their anatomical
  location without re-running the spatial join. Joined by synapse `id`
  into banc_888_synapses_v2_enriched.parquet, where it populates the
  `neuropil`, `region` and `side` columns. Deposited as a standalone
  file so that users who want to re-derive a different join (for
  example against a custom neuropil-mesh segmentation or under a
  coarser regional partition) can do so without rebuilding the
  enriched master table.
categories:
  - Data
  - Synapses
directoryLabel: synapses/v2.0
restrict: false
tabIngest: false
---

# synapse_neuropil_lookup_v2.parquet

## Purpose

This is the **per-synapse neuropil and region lookup** for the v2
synapse snapshot. For every v2 synapse detection (one row per synapse,
keyed by CAVE synapse `id`), the file records the FlyWire-style
neuropil mesh the synapse falls inside, the coarser CNS region, and
the laterality. Synapses that fail the alpha-shape point-in-surface
test against every named mesh carry `neuropil = "outside"` (assigned
by a nearest-mesh fallback) but still receive a `region` and `side`.

The table is the source of the `neuropil`, `region` and `side` columns
in `banc_888_synapses_v2_enriched.parquet`, which is built by joining
this file to the v2 synapse table by `id`. It is deposited as a
standalone file because the spatial join is expensive enough that
re-running it for a custom partition (e.g. a coarser regional
grouping, or a different mesh release) is the usual reason a user
would want the underlying lookup rather than the enriched master
table.

## Provenance

Built by **bancpipeline** (`banc/metrics/banc-calculate-neuropil-inclusion.R`,
called from `banc/meta/banc-data.R:404-599`) by alpha-shape
point-in-surface tests of every v2 synapse centroid against the BANC
neuropil meshes (`banc_brain_neuropils.surf` and
`banc_vnc_neuropils.surf`). The neuropil meshes themselves are in
`region_outlines/` and use the FlyWire neuropil naming convention
(for example `LH`, `AVLP`, `MB_CA`, `LAL` for brain neuropils;
`T1L`, `T2R` and similar for VNC neuromeres). A `pointsnearby_banc`
nearest-mesh fallback handles synapses that fail the alpha-shape test.

## Schema

| column | dtype | description |
|---|---|---|
| `id` | string | Synapse identifier from the CAVE v2 synapse table. Joins to `id` in `banc_888_synapses_v2_enriched.parquet` and to `id` in `banc_nt_prediction_w_sizethresh_5_11102025.parquet` (cast `int64` to `string` or vice versa as needed). |
| `neuropil` | string | FlyWire-style neuropil mesh label (for example `LH`, `AVLP`, `MB_CA`, `LAL`, `T1L`); `outside` for synapses that fail the alpha-shape test. Comma-joined when a synapse lies inside more than one named mesh. |
| `region` | large_string | Coarse CNS region. Observed values include `central_brain`, `optic_lobe`, `optic_lobes`, `outside`, `outside_optic_lobes`, `ventral_nerve_cord`, `vnc`. The vocabulary is not normalised here — downstream code should map to the paper's preferred partition (typically `central_brain` / `optic_lobe` / `vnc`) on read. |
| `side` | string | Laterality: `left`, `right`. Sided from `bancr:::banc_lr_position()` (positive = right). |

## Usage

Joins are cheap as long as you filter first. From R via arrow:

```r
library(arrow); library(dplyr)
np <- open_dataset("synapse_neuropil_lookup_v2.parquet") %>%
  filter(neuropil == "MB_CA", side == "right") %>%
  collect()
```

From Python:

```python
import pyarrow.dataset as ds
np = ds.dataset("synapse_neuropil_lookup_v2.parquet").to_table(
    filter=(ds.field("region") == "ventral_nerve_cord")
).to_pandas()
```

For most downstream uses the enriched synapse table already carries
`neuropil` / `region` / `side`; pull this file directly only when you
need the lookup independent of the synapse table.

## Related files

- `banc_888_synapses_v2_enriched.parquet` — the master per-synapse v2
  table, which already incorporates this lookup as its `neuropil`,
  `region` and `side` columns.
- `synapses/v3.0/synapse_neuropil_lookup_v3.parquet` — the v3 counterpart.
- `synapses/v2.0/banc_nt_prediction_w_sizethresh_5_11102025.parquet` —
  per-synapse neurotransmitter classifier, joinable by the same `id`.
- `region_outlines/` — the source neuropil-mesh segmentation used for
  the spatial overlay.

## Notes

- **`id` dtype** is `string` here, versus `int64` in
  `banc_nt_prediction_w_sizethresh_5_11102025.parquet`. Cast on join.
- **Row count versus enriched-table row count.** This lookup is built
  before the size-threshold filter that the enriched master table
  applies, so it has ~213 M rows versus the enriched table's ~169 M.
  Filtering this file to the same set of `id`s as the enriched table
  recovers the row alignment.
- **`region` vocabulary is not deduplicated** — `optic_lobe` and
  `optic_lobes` both appear, as do `ventral_nerve_cord` and `vnc`.
  Normalise on read if you need a clean partition.
- **`outside` neuropil** marks synapses outside every named mesh —
  fiber tracts, the cervical connective and other unsegmented white
  matter — assigned by the `pointsnearby_banc` nearest-mesh fallback.
- **Coordinates are not stored here**; rejoin to
  `banc_888_synapses_v2_enriched.parquet` if you need `(X, Y, Z)`.
