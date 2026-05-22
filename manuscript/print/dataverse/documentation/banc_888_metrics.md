---
filename: banc_888_metrics.feather
gcs_path: gs://lee-lab_brain-and-nerve-cord-fly-connectome/compiled_data/banc_888/banc_888_metrics.feather
size_bytes: 7787498
size_human: 7.43 MB
nrows: 188259
ncols: 12
content_type: application/x-arrow

# --- Fields posted to Dataverse ---
description: >-
  Per-neuron quantitative metrics for the BANC connectome at materialization
  v888, one row per segment (n = 188 259) and 12 columns. Carries the
  morphological measurements that also appear in `banc_888_meta.feather` —
  L2 node count, skeletal cable length (micrometers), segmentation volume
  (cubic nanometers), input and output synapse counts on the v2 size > 5
  set, left/right side bias for incoming and outgoing synapses,
  mitochondrion count and total mitochondrial volume, primary-dendrite
  width, and the axon/dendrite segregation index — split out as a small
  standalone file (7.5 MB). Use this in preference to the meta feather
  when an analysis only needs morphology: it loads about six times faster
  and joins to the rest of the metadata on `banc_888_id`. Compiled by
  bancpipeline from per-metric calculators (`banc-calculate-l2-metrics.R`,
  `banc-calculate-volumes.R`, `banc-calculate-synapses.R`) whose outputs
  are pushed to SeaTable and read back at compile time. Numbers are
  identical to those in the metrics columns of `banc_888_meta.feather`.
categories:
  - Data
directoryLabel: compiled_data
restrict: false
tabIngest: false
---

# banc_888_metrics.feather

## Purpose

`banc_888_metrics.feather` is the per-neuron morphological-metrics table for
the BANC connectome at CAVE materialization v888. It holds the small set of
scalar measurements that every neuron carries — skeletal cable length,
segmentation volume, synapse counts, side bias, mitochondrion load, and the
two flow-centrality split metrics — in a slim file that loads quickly when
the rest of the metadata is not needed.

Every row in `banc_888_meta.feather` has a corresponding row here, and the
numeric columns are byte-for-byte the same. The metrics file is the
practical join target for workflows that only need geometry (e.g. filtering
by `l2_cable_length_um` before pulling synapses, or computing volume
distributions per region).

## Provenance

Built by **bancpipeline** (`banc/meta/banc-data.R`, Section 2 at lines
383-402), which selects the metrics columns from the already-compiled meta
table. Upstream, each column is computed by its own calculator script and
pushed to BANC SeaTable, then read back when this file is written:

- `l2_nodes`, `l2_cable_length_um`, `pd_width`, `segregation_index` —
  `banc/metrics/banc-calculate-l2-metrics.R`, which reads per-neuron L2
  skeleton CSVs and the flow-centrality axon/dendrite split.
- `volume_nm3` — `banc/metrics/banc-calculate-volumes.R`, which sums CAVE
  L2 chunk volumes via `banc_neuron_volume()`.
- `input_connections`, `output_connections`, `input_side_index`,
  `output_side_index`, `mitochondria`, `mitochondria_volume` —
  `banc/metrics/banc-calculate-synapses.R`, which counts pre- and
  postsynaptic sites on the v2 synapse set and queries the CAVE
  `mitochondria` table.

The per-metric outputs are `full_join`ed on `root_id`, refined where
SeaTable holds curated overrides, and round-tripped through SeaTable so the
meta table and this file always agree.

## Schema

| column | dtype | description |
|---|---|---|
| `banc_888_id` | string | Primary key — the neuron's root identifier at v888. Same value as `root_id` in bancr. |
| `l2_nodes` | double | Number of L2 chunked-graph nodes in the reconstruction. |
| `l2_cable_length_um` | double | Total skeletal cable length in micrometers, from the L2 skeleton. |
| `input_connections` | double | Number of postsynaptic sites belonging to this neuron (incoming synapses, v2 `size > 5`). |
| `output_connections` | double | Number of presynaptic sites (outgoing synapses, v2 `size > 5`). |
| `input_side_index` | double | Laterality index for incoming synapses in `[-1, 1]`; negative = predominantly left, positive = predominantly right. |
| `output_side_index` | double | Laterality index for outgoing synapses, same convention. |
| `mitochondria` | double | Mitochondrion count inside the segmentation (CAVE `mitochondria` table). |
| `mitochondria_volume` | double | Summed mitochondrial volume in cubic nanometers. |
| `pd_width` | double | Primary-dendrite width derived from the flow-centrality split; used in optic-lobe cell typing. |
| `segregation_index` | double | Axon/dendrite segregation index in `[0, 1]` (1 = fully polarized). |
| `volume_nm3` | double | Total segmentation volume in cubic nanometers. |

## Usage

In R via arrow:

```r
library(arrow); library(dplyr)
m <- read_feather("banc_888_metrics.feather")
m %>% filter(l2_cable_length_um > 100, segregation_index > 0.8) %>%
  arrange(desc(volume_nm3))
```

In Python via pyarrow:

```python
import pyarrow.feather as feather
m = feather.read_table("banc_888_metrics.feather").to_pandas()
m = m[(m["l2_cable_length_um"] > 100) & (m["segregation_index"] > 0.8)]
```

To join onto annotations from the meta table:

```r
library(bancr)
meta <- banc_meta()                                       # or read_feather()
m <- read_feather("banc_888_metrics.feather") %>%
  left_join(select(meta, banc_888_id, super_class, super_cluster),
            by = "banc_888_id")
```

## Related files

- `banc_888_meta.feather` — the full per-neuron metadata table; carries
  the same 11 metric columns plus identity, taxonomy, neurochemistry, and
  cross-dataset matches.
- `banc_888_edgelist_simple_v2.feather` / `banc_888_edgelist_simple_v3.feather` —
  neuron-to-neuron edgelists. `input_connections` / `output_connections`
  here are close to (but not exactly) row-wise sums of the v2 edgelist's
  `count` column, because the metrics calculator uses `size > 5` while the
  edgelist uses `size >= 5`.
- `banc_888_neurotransmitter_prediction_v2.csv` — per-neuron NT
  prediction summary, joinable on `banc_888_id` ↔ `root_id`.

## Notes

- `banc_888_id` is stored as a string. Cast to `int64` only if you must —
  the column was kept as string to avoid silent precision loss in readers
  that promote unfamiliar integer types to 64-bit floats.
- The synapse-count columns are derived from the **v2** synapse set with
  `size > 5`. Counts derived from the stricter v3 set (`size >= 10`) are
  about 10-15 % lower; the metrics file does not ship v3 counts.
- `mitochondria` and `mitochondria_volume` cover only the segmentation
  proper — disconnected mitochondrial fragments that were not assigned to
  the neuron are not counted.
- Row count (188 259) is slightly larger than `banc_888_meta.feather`
  (188 162) because a small number of segments produce metric rows but
  have no curated metadata; treat these as unannotated background.
