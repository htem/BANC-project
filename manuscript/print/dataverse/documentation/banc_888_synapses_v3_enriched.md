---
filename: banc_888_synapses_v3_enriched.parquet
gcs_path: gs://lee-lab_brain-and-nerve-cord-fly-connectome/compiled_data/banc_888/banc_888_synapses_v3_enriched.parquet
size_bytes: 15487130177
size_human: 14.42 GiB
nrows: 198741886
ncols: 10
content_type: application/x-parquet

# --- Fields posted to Dataverse ---
description: >-
  Per-synapse table at synapse-prediction version v3, enriched with pre- and
  post-synaptic root IDs at materialization v888, a neuropil label, CNS
  region, and laterality. 198 741 886 rows × 10 columns. The v3 snapshot
  uses a stricter size threshold (`size >= 10` voxels) and an updated
  synapse-detection model relative to v2, and carries about 18 % more
  detections overall while dropping the small, marginal v2 calls.
  Coordinates are in BANC nanometers (the upstream v3 voxel resolution is
  16 × 16 × 45 nm; the script converts to nm before writing). The v3 file
  omits the per-synapse NT probabilities that v2 carries inline — those
  are deposited separately at
  `banc_nt_prediction_v3_w_sizethresh_10_05042026.parquet` and join on
  `id`. Row layout is partitioned by parquet row group, so filters on
  `neuropil`, `region`, or pre/post identifier benefit from predicate
  pushdown — essential for working with a 5.6 GB table on a workstation.
  Preferred over v2 for new work; Bates, Phelps, Kim, Yang et al., 2026
  use v2 throughout every figure and quantitative analysis.
categories:
  - Data
  - Synapses
directoryLabel: compiled_data
restrict: false
tabIngest: false
---

# banc_888_synapses_v3_enriched.parquet

## Purpose

`banc_888_synapses_v3_enriched.parquet` is the per-synapse master table for
the v3 synapse-prediction snapshot, with downstream enrichment applied.
Each row is one predicted synaptic contact, identified by its `id`,
situated by `(X, Y, Z)` in BANC nanometer space, and joined to a pre- and
post-synaptic root identifier at the v888 materialization. Two spatial
enrichments are applied on top of the raw v3 export:

- **Neuropil** — every synapse is tagged with the neuropil it falls in
  (or `outside` for synapses in fiber tracts and white matter), based on
  alpha-shape point-in-surface tests against the BANC brain and VNC
  neuropil meshes.
- **Region and side** — the coarser CNS region and the left/right side
  of the CNS. Bulk values are `central_brain`, `optic_lobes`, `vnc`;
  lower-count sub-partitions discovered by the synapse-location overlap
  (`brain`, `neck`, `sez`) appear at boundaries; `outside` marks
  synapses that fail the alpha-shape inclusion test. Note: `region` at
  the synapse level uses a finer vocabulary than `region` at the neuron
  level in `banc_888_meta.feather` (`central_brain`, `optic_lobe`,
  `ventral_nerve_cord`, `cervical_connective`). Normalise to the neuron-
  level partition on read if you need consistency.

Unlike the v2 enriched table, this file does **not** carry per-synapse
neurotransmitter probabilities inline. To keep the parquet small enough
for predicate pushdown on a workstation, the NT classifier output is kept
as a side file (`banc_nt_prediction_v3_w_sizethresh_10_05042026.parquet`)
that joins on synapse `id`.

## Provenance

Built by **bancpipeline** (`banc/meta/banc-data.R`, Section 3, v3 branch at
lines 410-469; synapse parquet written at line 597). The script:

1. Reads the v3 CAVE-ingested synapse export
   (`synapses_v3_human_readable_id_size_prerootid_postrootid_prex_prey_prez.parquet`)
   for synapse IDs, sizes, and pre- and post-synaptic v888 root IDs.
2. Inner-joins against the locally-produced v3 spatial parquet
   (`banc_888_synapses_v3.parquet`, built by
   `banc/metrics/banc-synapses-v3-optimised.R`) on `syn_id` → `id` for
   neuropil, region, side, and `(X, Y, Z)` centroids in BANC nanometer
   space.
3. Filters `size >= banc.size.threshold` (`= 2` in `banc-data.R:53`,
   though the upstream v3 export is already at `size >= 10`), drops
   autapses (`pre_root_id != post_root_id`), and drops rows where
   neither end is in the v888 neuron set.

The corresponding per-synapse NT classifier output
(`banc_nt_prediction_v3_w_sizethresh_10_05042026.parquet`, Drugowitsch
lab) is **not** joined in at this stage; it is kept as a side file and
joined on `id` only when needed.

## Schema

| column | dtype | description |
|---|---|---|
| `id` | large_string | Unique synapse identifier from the CAVE v3 synapse export. |
| `size` | int32 | Synapse footprint in voxels (`>= 10`). |
| `pre_root_id` | large_string | Presynaptic root ID at v888. |
| `post_root_id` | large_string | Postsynaptic root ID at v888. |
| `neuropil` | large_string | Neuropil code from the alpha-shape parcellation, namespaced by parcellation source (`ITO_optic_*`, `ITO_midbrain_*`, `MANC_vnc_*`, `COURT_vnc_*`). A synapse on a boundary may carry a comma-joined list (e.g. `ITO_optic_LO_R,ITO_optic_ME_R`). `outside` for synapses that fail the alpha-shape inclusion test. |
| `region` | string | Coarse CNS region. Bulk values are `central_brain`, `optic_lobes`, `vnc`; lower-count partitions include `brain` (the brain-without-optic-lobes catchall), `neck`, `sez`; `outside` for synapses that fail the alpha-shape inclusion test. |
| `side` | string | Laterality from `bancr:::banc_lr_position(units = "nm")`: `left`, `right` (positive x = right). |
| `X` | double | Synapse centroid x-coordinate in BANC nanometers (the upstream v3 voxel resolution is 16 × 16 × 45 nm; the script multiplies through to nm). |
| `Y` | double | Synapse centroid y-coordinate in nanometers. |
| `Z` | double | Synapse centroid z-coordinate in nanometers. |

## Usage

Predicate pushdown is essential — never read the whole file. In R via
arrow:

```r
library(arrow); library(dplyr)
syn <- open_dataset("banc_888_synapses_v3_enriched.parquet") %>%
  filter(neuropil == "MB_CA_R", region == "central_brain") %>%
  collect()
```

In Python:

```python
import pyarrow.dataset as ds
syn = ds.dataset("banc_888_synapses_v3_enriched.parquet").to_table(
    filter=(ds.field("pre_root_id") == "720575941521131930")
).to_pandas()
```

To attach per-synapse NT predictions from the side file:

```r
nt <- open_dataset("banc_nt_prediction_v3_w_sizethresh_10_05042026.parquet")
syn_with_nt <- syn %>% inner_join(collect(nt), by = "id")
```

## Related files

- `banc_888_synapses_v2_enriched.parquet` — the v2 counterpart at the
  earlier `size >= 2` filter (with `size >= 5` recommended downstream),
  with NT probabilities inline. Used by the BANC paper figures.
- `banc_nt_prediction_v3_w_sizethresh_10_05042026.md` — per-synapse NT
  classifier output for v3; joins on `id` to add the eight per-NT
  probabilities and `syn_top_nt` / `syn_top_p`.
- `synapse_neuropil_lookup_v3.md` — versioned cache of the
  neuropil-per-synapse lookup whose values appear in `neuropil` here.
- `banc_888_edgelist_simple_v3.feather` — the neuron-to-neuron edgelist
  rolled up from this synapse table.
- `banc_888_meta.feather` — per-neuron metadata; joins on `pre_root_id`
  or `post_root_id` ↔ `banc_888_id`.

## Notes

- **v2 vs v3.** v3 uses an updated synapse-detection model and a stricter
  `size >= 10` cutoff. v3 carries about 18 % more synapses overall —
  detections the v2 model missed — but small-but-real synapses present
  in v2 are dropped in v3 by the higher size cutoff. The two are not
  row-aligned: `id` is reassigned per detection run.
- **Coordinates are in BANC nanometers**, not voxels. Divide by
  `(4, 4, 45)` to obtain v888 segmentation-voxel indices, or by
  `(16, 16, 45)` for v3 detection-voxel indices.
- **NT predictions are deliberately kept out of this file.** The v3
  classifier output is large enough to dominate parquet read times for
  workflows that do not need it; users who do can `inner_join` the side
  file on `id`.
- **`region == "outside"`** flags synapses where the alpha-shape test
  fell into none of the volume hulls — typically peripheral-nerve
  segments and near-mesh corner cases.
- Autapses are excluded at write time.
