---
filename: banc_888_edgelist_simple_v3.feather
gcs_path: gs://lee-lab_brain-and-nerve-cord-fly-connectome/compiled_data/banc_888/banc_888_edgelist_simple_v3.feather
size_bytes: 352129906
size_human: 335.8 MB
nrows: 13507098
ncols: 6
content_type: application/x-arrow

# --- Fields posted to Dataverse ---
description: >-
  Neuron-to-neuron edgelist for the BANC connectome at materialization
  v888, rolled up from the v3 synapse set at `size >= 10` voxels. One row
  per directed pre → post pair; 13 507 098 rows × 6 columns — about 17 %
  more pairs than the v2 edgelist because the v3 detection model finds
  synapses that v2 missed, even after the higher size threshold drops
  some marginal v2 calls. Schema matches the v2 edgelist (`pre`, `post`,
  `count`, `norm`, `post_count`, `pre_count`); the differences are the
  synapse-detection model and the size cutoff used to roll the synapses
  up. Joins onto `banc_888_meta.feather` on both `pre` ↔ `banc_888_id`
  and `post` ↔ `banc_888_id`. Autapses are excluded at build time. v3 is
  preferred for new work; Bates, Phelps, Kim, Yang et al., 2026 use v2
  throughout every figure and quantitative analysis. About 75 % of
  v2 directed pairs are also present in v3; the remainder is split
  between v2-only edges lost to the size cutoff and v3-only edges added
  by the new detection model.
categories:
  - Data
  - Connectivity
directoryLabel: compiled_data
restrict: false
tabIngest: false
---

# banc_888_edgelist_simple_v3.feather

## Purpose

`banc_888_edgelist_simple_v3.feather` is the neuron-to-neuron edgelist for
BANC at materialization v888, summing per-synapse calls from the v3
synapse-prediction snapshot into one row per directed pair. Each row
records the number of synapses from a presynaptic neuron `pre` to a
postsynaptic neuron `post`, the fraction of the target neuron's total
input from that source, and the two per-neuron totals.

The schema matches `banc_888_edgelist_simple_v2.feather` exactly. The
difference is upstream: v3 uses an updated synapse-detection model and a
stricter `size >= 10` cutoff. The result has about 17 % more directed
pairs than v2 (13.5 M vs 11.5 M), driven by the new model detecting
synapses that v2 missed, partially offset by the stricter size cutoff
dropping marginal v2 calls.

## Provenance

Built by **bancpipeline** (`banc/metrics/banc-calculate-connectivity.R`,
v3 branch; synapse filter + edgelist roll-up at lines 234-287) and copied
into the versioned distribution directory by `banc/meta/banc-data.R`,
Section 4 at lines 605-640. The v3 branch:

1. Reads the v3 CAVE synapse export (`pre_root_id` / `post_root_id`
   already resolved against v888).
2. Filters synapses to `size >= 10` voxels.
3. Drops autapses and rows where neither end is a member of the v888
   neuron set.
4. Groups by `(pre_root_id, post_root_id)` and counts.
5. Computes per-target and per-source totals, and the normalized weight
   `count / post_count` (rounded to 4 significant figures).

## Schema

| column | dtype | description |
|---|---|---|
| `pre` | string | Presynaptic neuron root ID at v888. |
| `post` | string | Postsynaptic neuron root ID at v888. |
| `count` | int32 | Number of synapses from `pre` to `post` (after `size ≥ 10`). |
| `norm` | double | Normalized weight: `count / post_count`. Rounded to 4 significant figures. |
| `post_count` | int32 | Total synaptic input to `post` across all v3 sources. |
| `pre_count` | int32 | Total synaptic output from `pre` across all v3 targets. |

## Usage

In R via arrow:

```r
library(arrow); library(dplyr)
el <- read_feather("banc_888_edgelist_simple_v3.feather")
el %>% filter(post == "720575941521131930", count >= 5) %>%
  arrange(desc(count))
```

In Python:

```python
import pyarrow.feather as feather
el = feather.read_table("banc_888_edgelist_simple_v3.feather").to_pandas()
top = el[(el["post"] == "720575941521131930") & (el["count"] >= 5)] \
        .nlargest(10, "count")
```

## Related files

- `banc_888_edgelist_simple_v2.feather` — the v2 counterpart at
  `size ≥ 5`. Used throughout the BANC paper figures.
- `banc_888_edgelist_split_v2.feather` — compartment-resolved edgelist.
  No `_v3` compartment edgelist is currently deposited because the
  flow-centrality compartment splits were computed against the v2
  synapse set.
- `banc_888_synapses_v3_enriched.parquet` — the per-synapse table this
  edgelist is rolled up from.
- `banc_888_meta.feather` — per-neuron metadata, joins on `pre` or
  `post` ↔ `banc_888_id`.
- `banc_888_cns_network_spectral_clustering_v3.csv` — spectral
  clustering output computed from this edgelist (sibling of the `_v2`
  result used in the paper).
- `banc_888_betweenness_all_to_all_v3.csv`,
  `banc_888_betweenness_afferent_to_efferent_v3.csv` — Brandes
  betweenness centralities computed from this edgelist.

## Notes

- **v3 is not row-aligned with v2.** Synapse IDs differ between detection
  runs, and edge presence depends both on the new model's recall and on
  the higher size threshold. Roughly 75 % of v2 directed pairs are also
  present in v3; the remainder is split between v2-only edges (lost to
  the size cutoff) and v3-only edges (added by the new model).
- **Autapses are excluded at build time.**
- `norm` is row-normalized by target — values sum to 1 within each
  `post` (modulo rounding).
- For the Codex-style "connected at >= 5 synapses" threshold, apply
  `count >= 5` at read time. With the higher v3 size cutoff this is a
  stricter filter than the same expression on the v2 edgelist.
