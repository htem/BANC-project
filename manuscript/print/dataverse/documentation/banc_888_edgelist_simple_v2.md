---
filename: banc_888_edgelist_simple_v2.feather
gcs_path: gs://lee-lab_brain-and-nerve-cord-fly-connectome/compiled_data/banc_888/banc_888_edgelist_simple_v2.feather
size_bytes: 298465650
size_human: 284.6 MB
nrows: 11510975
ncols: 6
content_type: application/x-arrow

# --- Fields posted to Dataverse ---
description: >-
  Neuron-to-neuron edgelist for the BANC connectome at materialization
  v888, rolled up from the v2 synapse set at `size >= 5` voxels. One row
  per directed pre → post pair; 11 510 975 rows × 6 columns. Carries the
  raw synapse count, a per-pair normalized weight (`count / post_count` =
  fraction of the target's total input from this source), and per-neuron
  pre- and post-synapse totals. Joins onto `banc_888_meta.feather` on both
  `pre` ↔ `banc_888_id` and `post` ↔ `banc_888_id`. This is the primary
  connectivity table used by Bates, Phelps, Kim, Yang et al., 2026, and
  the table from which all downstream influence parquets are derived. Autapses are excluded at
  build time (the paper reports they account for about 2.1 % of all
  connections). No global count threshold is applied — apply `count >= 5`
  at read time for the Codex-style connectivity threshold. The v3
  counterpart (`banc_888_edgelist_simple_v3.feather`) shares the same
  schema but uses the v3 synapse model at the stricter `size >= 10`
  threshold.
categories:
  - Data
  - Connectivity
directoryLabel: compiled_data
restrict: false
tabIngest: false
---

# banc_888_edgelist_simple_v2.feather

## Purpose

`banc_888_edgelist_simple_v2.feather` is the neuron-to-neuron edgelist for
BANC at materialization v888, summing per-synapse calls from the v2
synapse-prediction snapshot into one row per directed pair. Each row
records the number of synapses from a presynaptic neuron `pre` to a
postsynaptic neuron `post`, the fraction of the target neuron's total
input that those synapses represent, and the two per-neuron totals.

This is the connectivity table used by the BANC paper figures. It is
small enough to fit comfortably in memory (11.5 M rows ≈ 285 MB on
disk) and joins onto `banc_888_meta.feather` on both `pre` ↔ `banc_888_id`
and `post` ↔ `banc_888_id`.

## Provenance

Built by **bancpipeline** (`banc/metrics/banc-calculate-connectivity.R`,
synapse filter at line 234-236, edgelist roll-up + save at lines 240-287)
and copied into the versioned distribution directory by
`banc/meta/banc-data.R`, Section 4 at lines 605-640. The pipeline:

1. Reads the v2 CAVE synapse table.
2. Filters synapses to `size >= 5` voxels (`banc.size.threshold = 5` in
   `banc-calculate-connectivity.R:57`).
3. Drops autapses and rows where neither end is a member of the v888
   neuron set.
4. Groups by `(pre_root_id, post_root_id)` and counts.
5. Computes per-target and per-source totals, and the normalized weight
   `count / post_count` (rounded to 4 significant figures).

No global count threshold is applied at write time — the filter is purely
on synapse `size`. Per-script connectivity thresholds (e.g. `count >= 10`
for hop-tracing in `panels_pre_effector_influence.R`) are applied
downstream in the figure scripts.

## Schema

| column | dtype | description |
|---|---|---|
| `pre` | string | Presynaptic neuron root ID at v888. Joins to `banc_888_meta$banc_888_id`. |
| `post` | string | Postsynaptic neuron root ID at v888. |
| `count` | int32 | Number of synapses from `pre` to `post` (after the `size ≥ 5` filter). |
| `norm` | double | Normalized connection weight: `count / post_count`, i.e. the fraction of the target's total input that comes from `pre`. Rounded to 4 significant figures. |
| `post_count` | int32 | Total synaptic input to `post` from all sources in the dataset. |
| `pre_count` | int32 | Total synaptic output from `pre` to all targets in the dataset. |

## Usage

In R via arrow:

```r
library(arrow); library(dplyr)
el <- read_feather("banc_888_edgelist_simple_v2.feather")
# Top-10 upstream partners of a single neuron
el %>% filter(post == "720575941521131930") %>%
  arrange(desc(count)) %>% head(10)
```

In Python via pyarrow:

```python
import pyarrow.feather as feather
el = feather.read_table("banc_888_edgelist_simple_v2.feather").to_pandas()
top = el[el["post"] == "720575941521131930"].nlargest(10, "count")
```

bancr exposes `banc_partners()` and `banc_partner_summary()` over this
table; users who only need an ID-level partner list should use those
rather than reading the feather directly.

## Related files

- `banc_888_edgelist_simple_v3.feather` — same schema applied to the v3
  synapse-prediction snapshot at the stricter `size ≥ 10` threshold.
- `banc_888_edgelist_split_v2.feather` — compartment-to-compartment
  (axon/dendrite/primary_neurite/…) edgelist derived from the same v2
  synapse source.
- `banc_888_synapses_v2_enriched.parquet` — the per-synapse table this
  edgelist is rolled up from.
- `banc_888_meta.feather` — per-neuron metadata, joins on either `pre`
  or `post` ↔ `banc_888_id`.
- Influence parquets under `compiled_data/banc_888/influence/` — derived
  from this edgelist (see paper Methods, "Influence").
- `banc_888_cns_network_spectral_clustering_v2.csv` — spectral
  clustering output computed from this edgelist (see paper Methods,
  "Spectral clustering").
- `banc_888_betweenness_all_to_all_v2.csv` and
  `banc_888_betweenness_afferent_to_efferent_v2.csv` — Brandes
  betweenness centralities computed from this edgelist (paper Methods,
  "Betweenness centrality").

## Notes

- **Autapses are excluded at build time** (`pre != post` is enforced
  during the synapse filter).
- **`norm` is row-normalized by target.** Per-row values sum to 1 within
  each `post` (modulo rounding). For source-normalized weights, compute
  `count / pre_count` yourself.
- `pre` and `post` are 18-19 digit integers stored as strings to avoid
  64-bit-float precision loss in readers that promote unfamiliar integer
  types.
- The v2 threshold is `size >= 5`. The v3 threshold (`size >= 10`) is
  more conservative; v3 edges typically have slightly smaller `count`
  per pair but a different set of pairs survives, since the v3 model
  also detects synapses that v2 missed.
- Per the paper, "connected at >= 5 synapses" is the canonical
  Codex-style connectivity threshold — apply `count >= 5` at read time.
