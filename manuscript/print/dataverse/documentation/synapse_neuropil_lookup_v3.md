---
filename: synapse_neuropil_lookup_v3.parquet
gcs_path: gs://lee-lab_brain-and-nerve-cord-fly-connectome/synapses/v3.0/synapse_neuropil_lookup_v3.parquet
size_bytes: 2386313119
size_human: 2.22 GB
nrows: 259393451
ncols: 4
content_type: application/x-parquet

# --- Fields posted to Dataverse ---
description: >-
  Per-synapse neuropil-and-region lookup for the v3 synapse snapshot.
  259,393,451 rows by 4 columns: synapse `id`, FlyWire-style `neuropil`
  label, coarse CNS `region`, and `side`. Built by overlaying every v3
  synapse coordinate against the BANC neuropil mesh segmentation — the
  same spatial join used for the v2 lookup but applied to the larger
  v3 detection set. Joined by synapse `id` into
  banc_888_synapses_v3_enriched.parquet, where it populates the
  `neuropil`, `region` and `side` columns. Deposited as a standalone
  file for users who want to re-derive a custom join against the v3
  synapse universe.
categories:
  - Data
  - Synapses
directoryLabel: synapses/v3.0
restrict: false
tabIngest: false
---

# synapse_neuropil_lookup_v3.parquet

## Purpose

This is the **per-synapse neuropil and region lookup** for the v3
synapse snapshot. For every v3 synapse detection (one row per synapse,
keyed by CAVE synapse `id`), the file records the FlyWire-style
neuropil mesh the synapse falls inside, the coarser CNS region, and
the laterality. The schema mirrors the v2 lookup, but the synapse `id`
universe is independent, the row count is larger, and the `region`
vocabulary is broader: in addition to the `central_brain` / `optic_lobes` /
`vnc` partition, v3 introduces sub-partitions `neck` and `sez` (and a
`brain` catchall for the brain-without-optic-lobes wedge), discovered
at neuropil boundaries by the synapse-location overlap. These are finer
than the neuron-level `region` in `banc_888_meta.feather` — downstream
code should normalise on read if cross-table consistency is needed.

The table is the source of the `neuropil`, `region` and `side` columns
in `banc_888_synapses_v3_enriched.parquet`. It is deposited as a
standalone file for users who want to re-run the spatial join under a
different regional or neuropil partition.

## Provenance

Built by **bancpipeline**
(`banc/metrics/banc-synapses-v3-optimised.R`, surfaced through
`banc/meta/banc-data.R`) by alpha-shape point-in-surface tests of
every v3 synapse centroid against the BANC neuropil meshes
(`banc_brain_neuropils.surf`, `banc_vnc_neuropils.surf`). Neuropil
meshes live in `region_outlines/` and use the FlyWire neuropil naming
convention. A `pointsnearby_banc` nearest-mesh fallback handles
centroids that fail the alpha-shape test.

## Schema

| column | dtype | description |
|---|---|---|
| `id` | large_string | Synapse identifier from the CAVE v3 synapse table. Joins to `id` in `banc_888_synapses_v3_enriched.parquet` and to `id` in `banc_nt_prediction_v3_w_sizethresh_10_05042026.parquet` (cast as needed; the NT parquet stores `id` as `int64`). |
| `neuropil` | large_string | FlyWire-style neuropil mesh label; `outside` for centroids that fail the alpha-shape test. |
| `region` | large_string | Coarse CNS region. Observed values include `brain`, `central_brain`, `neck`, `optic_lobes`, `outside`, `sez`, `vnc`. The vocabulary is not normalised here — downstream code should map to the paper's preferred partition (typically `central_brain` / `optic_lobe` / `vnc`) on read. |
| `side` | string | Laterality: `left`, `right`. Sided from `bancr:::banc_lr_position()` (positive = right). |

## Usage

Joins are cheap with predicate pushdown. From R via arrow:

```r
library(arrow); library(dplyr)
np <- open_dataset("synapse_neuropil_lookup_v3.parquet") %>%
  filter(neuropil == "MB_CA", side == "right") %>%
  collect()
```

From Python:

```python
import pyarrow.dataset as ds
np = ds.dataset("synapse_neuropil_lookup_v3.parquet").to_table(
    filter=(ds.field("region") == "central_brain")
).to_pandas()
```

For most downstream uses the enriched v3 synapse table already carries
`neuropil` / `region` / `side`; pull this file directly only when you
need the lookup independent of the synapse table.

## Related files

- `banc_888_synapses_v3_enriched.parquet` — the master per-synapse v3
  table; receives this lookup as its `neuropil`, `region` and `side`
  columns.
- `synapses/v2.0/synapse_neuropil_lookup_v2.parquet` — the v2 counterpart.
- `synapses/v3.0/banc_nt_prediction_v3_w_sizethresh_10_05042026.parquet`
  — per-synapse neurotransmitter classifier for v3, joinable by the
  same `id`.
- `region_outlines/` — the source neuropil-mesh segmentation used for
  the spatial overlay.

## Notes

- **`id` dtype** is `large_string` here, versus `int64` in
  `banc_nt_prediction_v3_w_sizethresh_10_05042026.parquet`. Cast on join.
- **v2 versus v3.** The v3 lookup contains roughly 21% more rows than
  the v2 lookup, reflecting the larger v3 detection set. v2 and v3
  synapse `id`s are not interchangeable — they index different
  synapse universes.
- **`outside` neuropil** marks synapses outside every named mesh —
  fiber tracts, the cervical connective and other unsegmented white
  matter — assigned by the `pointsnearby_banc` nearest-mesh fallback.
- **Coordinates are not stored here**; rejoin to
  `banc_888_synapses_v3_enriched.parquet` if you need `(X, Y, Z)`.
