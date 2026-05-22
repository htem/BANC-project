---
filename: banc_fanc_1116_nblast.feather
gcs_path: gs://lee-lab_brain-and-nerve-cord-fly-connectome/nblast/banc_fanc_1116_nblast.feather
size_bytes: 93624386
size_human: 89.29 MB
nrows: 1658475
ncols: 11
content_type: application/x-arrow

# --- Fields posted to Dataverse ---
description: >-
  NBLAST morphological-similarity table between BANC VNC neurons and
  the Lee-lab FANC v1116 female adult nerve cord connectome, bridged
  into the JRC2018VNCF template via the BANC ↔ JRC2018VNCF elastix
  registration (`registrations/vnc_240721`). 1 658 475 rows × 11
  columns — one row per BANC VNC query and FANC candidate match,
  recording normalized score and the matched FANC cell-type label.
  FANC is the immediate predecessor of BANC from the same
  laboratory, segmented from a separate female fly using the same
  reconstruction pipeline, and is the closest cross-individual
  comparator available for the BANC VNC. Same-sex, same-pipeline,
  different individual — the cleanest test of how stable VNC
  morphology is across animals, isolating individual variation as
  the only source of divergence. Used both as a reference input to
  the iterative cell-type-matching algorithm described in the paper
  Methods, and as a standalone resource for cross-individual VNC
  morphology comparison and reproducibility analysis.
categories:
  - Data
  - NBLAST
directoryLabel: nblast
restrict: false
tabIngest: false
---

# banc_fanc_1116_nblast.feather

## Purpose

This file is the pairwise NBLAST morphological-similarity table
between BANC VNC neurons and the Lee-lab FANC v1116 connectome. Each
row records a candidate match: a BANC `query_root_id`, a FANC
`match_id`, the cell-type label of that FANC neuron, and the NBLAST
`score`.

FANC and BANC are both female adult fly VNCs reconstructed by the
same laboratory using the same pipeline, but from different
individuals. This NBLAST is therefore the canonical cross-individual
control for VNC morphology: an apples-to-apples comparison that
holds sex, segmentation pipeline, and reconstruction conventions
constant, isolating individual variation as the only source of
divergence. It is the principal reference for VNC reproducibility
claims in the paper.

## Provenance

Computed by **bancpipeline** (`banc/nblast/banc-nblast-compile.R`):

1. BANC neurons were skeletonised at L2 (chunked-graph) resolution.
2. Skeletons were registered into the **JRC2018VNCF** template via
   the BANC-to-JRC2018VNCF elastix registration deposited alongside
   this file (`registrations/vnc_240721`).
3. FANC v1116 skeletons (in their native space) were bridged into
   JRC2018VNCF using the FANC → JRC2018VNCF bridge.
4. NBLAST was run with the `natverse` toolchain and normalized
   against query self-scores.

## Schema

| column | dtype | description |
|---|---|---|
| `pt_root_id` | string | Root ID of the BANC query at the current materialisation; tracks segmentation edits via the supervoxel anchor. |
| `pt_supervoxel_id` | string | A supervoxel of the query neuron, used for chunked-graph re-resolution. Stable across root-ID changes. |
| `pt_position` | string | Anchor point on the query, BANC voxel space (`"x, y, z"`). |
| `query_root_id` | string | BANC root ID at the time the NBLAST was run; compare with `pt_root_id` to detect stale rows. |
| `match_id` | string | FANC v1116 `cell_id` of the candidate match. |
| `match_cell_type` | string | Cell-type label of the FANC match. |
| `score` | double | Normalized NBLAST score in `[-1, 1]` (1 = perfect; ≥ 0.3 is loose, ≥ 0.5 is solid, ≥ 0.7 is strong). |
| `root_626` | string | Query root ID at v626 materialisation. |
| `root_850` | string | Query root ID at v850. |
| `root_888` | string | Query root ID at v888. |
| `validation` | bool | `TRUE` for matches that survived expert review; `FALSE` (or null) otherwise. |

## Usage

In R via bancr:

```r
library(bancr); library(dplyr)
m <- banc_nblast_matches(dataset = "fanc")
m %>% group_by(query_root_id) %>% slice_max(score, n = 5)
```

The curated top-1 FANC cell-type call per BANC VNC neuron is also
exposed as the `fanc_cell_type` column of `banc_888_meta.feather`.

## Related files

- `banc_manc_v1.2.1_nblast.feather` — male VNC counterpart to this
  table (deeper cell-type catalog, but cross-sex).
- `banc_fafb_783_nblast.feather`,
  `banc_hemibrain_v1.2.1_nblast.feather` — brain-side NBLAST tables.
- `banc_malecns_v0.9_nblast.feather` — BANC ↔ Janelia maleCNS.
- `banc_native_nblast.feather`, `banc_mirror_nblast.feather` —
  within-BANC and BANC-vs-mirror NBLAST tables.
- `registrations/vnc_240721/` — BANC ↔ JRC2018VNCF elastix
  registration used to bridge into FANC space.
- `banc_888_meta.feather` — `fanc_cell_type` and `fanc_nblast_match`
  columns expose the curated top match.

## Notes

- FANC's cell-type catalog is smaller and shallower than MANC's;
  expect fewer non-null `match_cell_type` values per BANC query.
  For most VNC neurons, the practical workflow is to consult MANC
  first for type identity and FANC second for cross-individual
  shape consistency.
- A high NBLAST score is necessary but not sufficient for a correct
  cell-type match. Use the curated `cell_type` column in
  `banc_888_meta.feather` as the source of truth.
- Scores are normalized against query self-scores; asymmetry between
  paired BANC↔FANC queries is expected.
