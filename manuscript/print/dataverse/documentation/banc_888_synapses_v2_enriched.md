---
filename: banc_888_synapses_v2_enriched.parquet
gcs_path: gs://lee-lab_brain-and-nerve-cord-fly-connectome/compiled_data/banc_888/banc_888_synapses_v2_enriched.parquet
size_bytes: 10327245312
size_human: 9.62 GB
nrows: 168951110
ncols: 21
content_type: application/x-parquet

# --- Fields posted to Dataverse ---
description: >-
  Per-synapse table at synapse-prediction version v2, enriched with pre- and
  post-synaptic root IDs at materialization v888, neuropil and region labels,
  per-synapse coordinates in nanometers, the per-synapse
  neurotransmitter-classifier output (eight per-NT probabilities plus the
  argmax call and its confidence), and a compartment code derived from the
  flow-centrality axon/dendrite split. 168 951 110 rows × 21 columns; the
  v2 is the synapse set used by Bates, Phelps, Kim, Yang et al., 2026 in
  every figure and quantitative analysis of the paper, and is the basis
  for the Codex-style "connectivity at >= 5 synapses" thresholding. Autapses are
  excluded at write time, and synapses where neither end is a member of the
  v888 neuron set are dropped. Row layout is partitioned by parquet row
  group so that filters on neuropil, region, or pre/post identifier benefit
  from predicate pushdown — essential for working with a 9.6 GB parquet
  table on a workstation. A v3 counterpart at the stricter `size >= 10`
  threshold is also available (`banc_888_synapses_v3_enriched.parquet`).
categories:
  - Data
  - Synapses
directoryLabel: compiled_data
restrict: false
tabIngest: false
---

# banc_888_synapses_v2_enriched.parquet

## Purpose

`banc_888_synapses_v2_enriched.parquet` is the per-synapse master table for
the v2 synapse-prediction snapshot, with downstream enrichment applied.
Each row is a single predicted synaptic contact, identified by its `id`,
situated by `(X, Y, Z)` coordinates in BANC nanometer space, and joined to
a pre- and post-synaptic root identifier at the v888 materialization.
Three layers of enrichment have been added on top of the raw CAVE export:

- **Spatial context** — every synapse is tagged with the neuropil it falls
  in (or `outside` for fiber tracts and white matter), the coarser CNS
  region, and the left/right side of the CNS.
- **Neurochemistry** — the per-synapse output of the BANC neurotransmitter
  classifier (Drugowitsch lab, transferred from Eckstein et al. 2024).
  Each row carries one probability per candidate neurotransmitter, the
  top-prediction string, and the probability of that top prediction.
- **Compartment** — a postsynaptic compartment code from the
  flow-centrality axon/dendrite split (where available), populated by
  joining the per-neuron split CSVs onto synapse `connector_id`.

The same row set has a sibling at synapse-prediction version v3
(`banc_888_synapses_v3_enriched.parquet`), with a different size cutoff
and an updated detection model; the two are not row-aligned (see Notes).

## Provenance

Built by **bancpipeline** (`banc/meta/banc-data.R`, Section 3, v2 branch at
lines 471-499 + 503-520 + 522-591; synapse parquet written at line 597).
The script:

1. Reads the neuropil-labeled v2 synapse parquet
   (`banc_<version>_synapses_v2_neuropils.parquet`), produced upstream by
   `banc/metrics/banc-calculate-neuropil-inclusion.R`.
2. Filters to `size >= 2` voxels (`banc.size.threshold = 2` in
   `banc-data.R:53`), drops autapses, and drops rows where neither end is
   a member of the v888 neuron set.
3. Joins the per-synapse **NT-classifier output**
   (`synapses/v2.0/banc_nt_prediction_w_sizethresh_5_11102025.parquet`,
   Drugowitsch lab) on synapse `id`, renaming `predicted_nt` →
   `syn_top_nt` and `probability` → `syn_top_p`.
4. Joins per-neuron flow-centrality compartment labels (from
   detailed-split CSVs, falling back to L2-split CSVs) on synapse
   `connector_id`, mapping the integer codes through
   `hemibrainr:::standard_compartments`.

The upstream parquets (`synapse_neuropil_lookup_v2.parquet`,
`banc_nt_prediction_w_sizethresh_5_11102025.parquet`) are also deposited
separately for users who want to redo a different join.

## Schema

| column | dtype | description |
|---|---|---|
| `id` | string | Unique synapse identifier from the CAVE synapse table. |
| `size` | double | Synapse footprint in voxels (`>= 2`). |
| `pre_root_id` | large_string | Presynaptic root ID at v888. |
| `post_root_id` | large_string | Postsynaptic root ID at v888. |
| `X` | double | Synapse centroid x-coordinate in BANC nanometers. |
| `Y` | double | Synapse centroid y-coordinate, nanometers. |
| `Z` | double | Synapse centroid z-coordinate, nanometers. |
| `neuropil` | string | Neuropil short code from the BANC neuropil parcellation (e.g. `LO`, `ME`, `SMP`, `AVLP`, `GNG`, `MesoNM-T2`, `LTct`); no left/right suffix at v2. `outside` for synapses that fail the alpha-shape inclusion test. A synapse on a neuropil boundary may carry a comma-joined pair. |
| `region` | large_string | Coarse CNS region: `central_brain`, `optic_lobe`, `ventral_nerve_cord`, or `outside` (alpha-shape fall-through). A handful of rows carry legacy values `optic_lobes` / `vnc` / `outside_optic_lobes` from an older lookup generation; treat these as equivalent to their canonical counterparts. |
| `side` | string | Laterality computed from `bancr:::banc_lr_position(units = "nm")`: `left`, `right`. |
| `acetylcholine` | double | Classifier probability for acetylcholine, `[0, 1]`. |
| `dopamine` | double | Classifier probability for dopamine. |
| `gaba` | double | Classifier probability for GABA. |
| `glutamate` | double | Classifier probability for glutamate. |
| `histamine` | double | Classifier probability for histamine. |
| `octopamine` | double | Classifier probability for octopamine. |
| `serotonin` | double | Classifier probability for serotonin. |
| `tyramine` | double | Classifier probability for tyramine. |
| `syn_top_nt` | string | Argmax over the eight probabilities — the predicted neurotransmitter for the synapse. |
| `syn_top_p` | double | Value of the argmax probability, i.e. the confidence of `syn_top_nt`. |
| `label` | int32 | Compartment code from the flow-centrality split (per `hemibrainr:::standard_compartments`): `0` = unknown, `1` = soma, `2` = axon, `3` = dendrite, `4` = primary dendrite, `7` = primary neurite. `NA` where no split was available. |

## Usage

Predicate pushdown is essential — never read the whole file. From R via
arrow:

```r
library(arrow); library(dplyr)
syn <- open_dataset("banc_888_synapses_v2_enriched.parquet") %>%
  filter(neuropil == "MB_CA_R", syn_top_p > 0.7) %>%
  collect()
```

From Python:

```python
import pyarrow.dataset as ds
syn = ds.dataset("banc_888_synapses_v2_enriched.parquet").to_table(
    filter=(ds.field("pre_root_id") == "720575941521131930")
).to_pandas()
```

bancr exposes `banc_partners()` and `banc_partner_summary()` over this
table; users who only need a partner edgelist should use those rather
than touching the synapse table directly.

## Synapse-prediction version history

The deposit includes only the **v2 (2025-02-26)** and **v3** enriched
tables, the ones used in the paper. For full provenance, earlier
prediction runs are also on GCS but are **not deposited**:

| version | date | postsynaptic probabilities (precomputed) | postsynaptic segmentation (precomputed) | raw edgelist (CSV, `.df` extension) |
|---|---|---|---|---|
| v1.0 | 2024-05-29 → 2024-06-04 | `precomputed://gs://zetta_lee_fly_cns_001_synapse/240529_run/20240529084550` | `precomputed://gs://zetta_lee_fly_cns_001_synapse/240529_run/240604_seg` | `gs://zetta_lee_fly_cns_001_synapse/240529_run/240604_assignment/final_edgelist.df` |
| v1.1 | 2024-05-29 → 2024-06-23 | same as v1.0 | `gs://zetta_lee_fly_cns_001_synapse/240623_run/seg/` | `gs://zetta_lee_fly_cns_001_synapse/240623_run/assignment/final_edgelist.df` (mirror: `gs://lee-lab_brain-and-nerve-cord-fly-connectome/synapses/v1.1/final_edgelist.csv`) |
| **v2.0** | 2025-02-26 | (n/a — model output not posted separately) | `precomputed://gs://zetta_lee_fly_cns_001_synapse/250226_assignment/seg` | `gs://zetta_lee_fly_cns_001_synapse/250226_assignment/assignment/final_edgelist.df` (mirror: `gs://lee-lab_brain-and-nerve-cord-fly-connectome/synapses/v2.0/final_edgelist.csv`) |

v1.1 contained more synapses than v1.0 ("Alex found that lowering a size
threshold for the postsynaptic terminal increased recall without ruining
precision for FAFB"), and an improved presynaptic-assignment model tuned
on more FAFB-FlyWire data. **The paper figures use v2.0**; v1.x is
retained only for users reproducing preprint-era analyses.

## Related files

- `banc_888_synapses_v3_enriched.parquet` — the v3 synapse-prediction
  counterpart (stricter `size >= 10`, updated detection model; NT
  predictions are kept as a side file rather than inline).
- `banc_888_edgelist_simple_v2.feather` — neuron-to-neuron edgelist
  rolled up from this synapse table at `size >= 5`.
- `banc_888_edgelist_split_v2.feather` — compartment-resolved edgelist
  derived from the same source.
- `synapse_neuropil_lookup_v2.md` — upstream neuropil-per-synapse lookup
  that contributed the `neuropil`, `region`, and `side` columns.
- `banc_nt_prediction_w_sizethresh_5_11102025.md` — upstream NT
  classifier output that contributed the eight per-NT probability columns
  and `syn_top_nt` / `syn_top_p`.
- `2024-09-20_aelysia_synapse_sample_complete.csv` — manual-review
  sample of v2 synapses used to choose the `size >= 5` cutoff in paper
  Methods, "Synapse detection evaluation"; deposited as a standalone
  sample (not joined into this parquet).

## Notes

- **Autapses are excluded at write time** (`pre_root_id != post_root_id`).
- **v2 vs v3.** v3 uses an updated synapse-detection model and a stricter
  `size >= 10` cutoff. It carries about 18 % more synapses overall —
  detections the v2 model missed — but small-but-real synapses present in
  v2 are dropped in v3 by the higher size cutoff. The paper figures use
  v2. v3 is preferred for new work unless backward compatibility is
  needed.
- **Coordinates** are in BANC nanometers, not voxels. Divide by
  `(4, 4, 45)` to obtain voxel indices.
- **`id` is opaque** — it is the CAVE synapse identifier; stable across
  materializations but does not encode position.
- **NT probabilities sum to 1 per row** (within floating-point noise) by
  construction of the softmax classifier head.
- For the Codex-style "connectivity at >= 5 synapses" threshold, prefer
  `banc_888_edgelist_simple_v2.feather` over re-deriving from this
  table.
