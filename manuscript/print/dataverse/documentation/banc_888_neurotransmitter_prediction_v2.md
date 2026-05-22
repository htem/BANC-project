---
filename: banc_888_neurotransmitter_prediction_v2.csv
gcs_path: gs://lee-lab_brain-and-nerve-cord-fly-connectome/compiled_data/banc_888/banc_888_neurotransmitter_prediction_v2.csv
size_bytes: 22125000
size_human: 21.1 MB
nrows: 188199
ncols: 17
content_type: text/csv

# --- Fields posted to Dataverse ---
description: >-
  Per-neuron neurotransmitter prediction summary for the BANC connectome at
  materialization v888, derived from the v2 per-synapse classifier (size
  threshold 5). One row per presynaptic neuron, 17 columns. For each
  neuron the table records per-NT presynapse counts across eight candidate
  transmitters (acetylcholine, GABA, glutamate, dopamine, serotonin,
  octopamine, tyramine, histamine), the argmax prediction, the prediction
  confidence in `[0, 1]`, the total NT-classified presynapse count, a
  supervoxel anchor and soma position, the curated cell-type label, and a
  per-`cell_type` consensus prediction with its own confidence.
  Complementary to the per-synapse table
  `banc_888_synapses_v2_enriched.parquet`, which carries the same
  classifier output at single-synapse resolution. The two summary fields
  (`neurotransmitter_predicted`, `neurotransmitter_score`) also appear in
  `banc_888_meta.feather`; this file is the standalone form for users who
  want the per-neuron NT call without pulling the full meta table, plus
  the per-NT presynapse counts that go into the argmax.
categories:
  - Data
directoryLabel: compiled_data
restrict: false
tabIngest: false
---

# banc_888_neurotransmitter_prediction_v2.csv

## Purpose

`banc_888_neurotransmitter_prediction_v2.csv` is the per-neuron
neurotransmitter prediction summary at v888 materialization, computed from
the v2 per-synapse classifier output. Each row aggregates the eight
per-synapse probabilities across all classified presynaptic sites of a
single neuron, then takes the argmax to produce a per-neuron predicted
neurotransmitter and a confidence score.

The same two summary fields (`neurotransmitter_predicted`,
`neurotransmitter_score`) are carried in `banc_888_meta.feather`; this
file additionally exposes the per-NT presynapse counts that go into the
argmax, the total classified-presynapse count, and a per-`cell_type`
consensus that is useful when individual neurons of a cell type fail
classification but the type as a whole has a clear NT.

The file uses the **v2** synapse set with `size ≥ 5` voxels. A v3
counterpart, derived from the stricter v3 size-≥-10 synapse set with the
updated classifier, will be deposited separately when it is available.

## Provenance

Built by **bancpipeline** (`banc/metrics/banc-calculate-ntpred.R`,
computation lines ~50-183, CSV write + GCS copy at lines 195-201). The
script:

1. Reads the per-synapse NT-classifier output
   (`synapses/v2.0/banc_nt_prediction_w_sizethresh_5_11102025.parquet`,
   Drugowitsch lab; size threshold 5).
2. Joins onto the v2 BANC synapse table by synapse `id`, keeping rows with
   a non-NA `predicted_nt`.
3. Per presynaptic neuron, sums per-synapse probability across each of
   the eight candidate transmitters and picks the argmax NT.
4. Computes a per-neuron confidence as `max_score / total_score`, scaled
   to `[0, 1]`.
5. Pivots the per-NT presynapse counts (number of synapses whose own
   argmax matches that NT) into eight wide columns.
6. Joins SeaTable metadata (`supervoxel_id`, `position`, `cell_type`),
   then computes a per-`cell_type` consensus NT and confidence by
   weighting each member neuron's `neurotransmitter_score` by its
   `count`.

The same per-neuron prediction is also pushed to SeaTable and copied into
`banc_888_meta.feather`; the `cell_type_neurotransmitter_predicted` and
`cell_type_neurotransmitter_score` columns appear only in this CSV.

## Schema

| column | dtype | description |
|---|---|---|
| `root_id` | string | Root ID of the presynaptic neuron at v888. Synonymous with `banc_888_id` in the meta table. |
| `acetylcholine` | int | Count of this neuron's presynapses whose per-synapse argmax was acetylcholine. |
| `dopamine` | int | Per-NT presynapse count for dopamine. |
| `gaba` | int | Per-NT presynapse count for GABA. |
| `glutamate` | int | Per-NT presynapse count for glutamate. |
| `histamine` | int | Per-NT presynapse count for histamine. |
| `octopamine` | int | Per-NT presynapse count for octopamine. |
| `serotonin` | int | Per-NT presynapse count for serotonin. |
| `tyramine` | int | Per-NT presynapse count for tyramine. |
| `neurotransmitter_predicted` | string | Per-neuron predicted neurotransmitter (argmax over the eight probability sums). One of the eight NTs above; absent for neurons with no classified presynapses. |
| `neurotransmitter_score` | double | Per-neuron prediction confidence in `[0, 1]` (max-NT sum divided by the total NT sum). |
| `count` | int | Total presynapses with a non-NA classifier output for this neuron. |
| `supervoxel_id` | string | A supervoxel of the neuron, used to resolve to the current root. |
| `position` | string | Soma anchor position in BANC voxel space (`"x, y, z"`, 4 × 4 × 45 nm per voxel). |
| `cell_type` | string | Curated cell type, joined from SeaTable. Used to compute the per-`cell_type` consensus columns. |
| `cell_type_neurotransmitter_predicted` | string | Per-`cell_type` consensus NT (count-weighted argmax over members of the cell type). |
| `cell_type_neurotransmitter_score` | double | Per-`cell_type` consensus confidence in `[0, 1]`. |

## Usage

In R:

```r
library(readr); library(dplyr)
nt <- read_csv("banc_888_neurotransmitter_prediction_v2.csv",
               col_types = cols(root_id = "c", supervoxel_id = "c"))
nt %>% filter(neurotransmitter_score > 0.9, count >= 20) %>%
  count(neurotransmitter_predicted, sort = TRUE)
```

In Python:

```python
import pandas as pd
nt = pd.read_csv("banc_888_neurotransmitter_prediction_v2.csv",
                 dtype={"root_id": str, "supervoxel_id": str})
high_conf = nt[(nt["neurotransmitter_score"] > 0.9) & (nt["count"] >= 20)]
```

To use the cell-type consensus as a fallback when an individual neuron's
own prediction has low support:

```r
nt %>% mutate(nt_call = if_else(count < 5,
                                cell_type_neurotransmitter_predicted,
                                neurotransmitter_predicted))
```

## Related files

- `banc_888_meta.feather` — carries the same per-neuron
  `neurotransmitter_predicted` / `neurotransmitter_score` columns, plus
  the literature-verified `neurotransmitter_verified` and
  `neuropeptide_verified` curations.
- `banc_888_synapses_v2_enriched.parquet` — the per-synapse table this
  aggregation feeds from; carries the eight per-NT probabilities and
  `syn_top_nt` / `syn_top_p` at single-synapse resolution.
- `banc_nt_prediction_w_sizethresh_5_11102025.md` — the upstream
  classifier output (Krishna Dasari / Drugowitsch lab) read by
  `banc-calculate-ntpred.R`.

## Notes

- **v2 only.** The filename includes `_v2` to distinguish this from a
  prospective v3 per-neuron prediction CSV. The paper's NT analyses use
  the v2 numbers.
- The argmax NT is the per-neuron majority over per-synapse argmax
  decisions. A small number of neurons have low total `count` and
  correspondingly noisy `neurotransmitter_score`; filter on `count` if
  you need high confidence.
- The CSV is **not** sorted by `root_id`. Use the meta table's
  `neurotransmitter_predicted` column if you need a stably ordered
  per-neuron NT column for indexing.
- `root_id` and `supervoxel_id` are 18-19 digit integers — read them as
  strings to avoid silent precision loss in any reader that does
  64-bit-float promotion.
