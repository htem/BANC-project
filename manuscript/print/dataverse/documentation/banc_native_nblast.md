---
filename: banc_native_nblast.feather
gcs_path: gs://lee-lab_brain-and-nerve-cord-fly-connectome/nblast/banc_native_nblast.feather
size_bytes: 5946082
size_human: 5.67 MB
nrows: 49289
ncols: 16
content_type: application/x-arrow

# --- Fields posted to Dataverse ---
description: >-
  Within-BANC NBLAST morphological-similarity table — BANC neurons
  scored against other BANC neurons with no mirroring — restricted
  to the curated canary subset used to seed clustering and
  serial-homologue detection. 49 289 rows × 16 columns; one row per
  query × match pair, with both sides' metadata (root IDs at v626 /
  v850 / v888, supervoxel IDs, soma positions). Distinct from
  `banc_mirror_nblast.feather`, which scores each neuron against
  the mirror image of every other; `banc_native_nblast.feather` is
  unmirrored and therefore captures same-side morphological
  similarity. The two principal use cases are detection of serial
  homologues across VNC neuromeres (where leg motor neurons in T1,
  T2 and T3 differ only by a vertical shift) and seeding the AN/DN
  morphology + connectivity clustering described in the paper
  Methods. Scope is intentionally limited to the canary set rather
  than full all-versus-all on 188 k neurons, which would be > 35
  billion rows; for exhaustive cross-dataset search use the
  cross-dataset NBLAST tables instead.
categories:
  - Data
  - NBLAST
directoryLabel: nblast
restrict: false
tabIngest: false
---

# banc_native_nblast.feather

## Purpose

This file is the within-BANC NBLAST morphological-similarity table
**without** mirroring. Every row is a candidate similarity between
two BANC neurons on the same side of the body. It is restricted to a
curated subset of canary neurons rather than running all-versus-all
across the full 188 k-neuron set, which keeps the table small and
analysable.

The two principal use cases:

1. **Serial-homologue detection within the VNC.** Many VNC cell
   types are repeated across the prothoracic, mesothoracic and
   metathoracic neuromeres; the within-BANC NBLAST surfaces these
   serial sets because a leg motor neuron in T1 looks almost
   identical to its T2 and T3 counterparts, modulo a vertical
   shift.
2. **AN/DN morphology + connectivity clustering seed.** The paper's
   ascending- and descending-neuron clustering pipeline
   (`panels_an_dn_umap.R`) uses this NBLAST as the morphological
   side of the hybrid distance, paired with connectivity-derived
   features.

## Provenance

Computed by **bancpipeline** (`banc/nblast/banc-nblast-compile.R`):

1. BANC neurons in the canary set were skeletonised at L2
   (chunked-graph) resolution.
2. Skeletons were registered into the **JRC2018F** template (brain)
   and **JRC2018VNCF** template (VNC) via the elastix registrations
   deposited alongside this file (`registrations/brain_240721`,
   `registrations/vnc_240721`).
3. NBLAST was run between every neuron and every other neuron in
   the canary set, then normalized against query self-scores. No
   mirror reflection was applied.

The restricted scope (49 289 rows ≪ a full all-versus-all on 188 k
neurons, which would be > 35 billion rows) is intentional: this
file is designed to support targeted analyses on annotated subsets,
not exhaustive search. For exhaustive cross-dataset search use the
cross-dataset NBLAST tables; for bilateral-pair work use
`banc_mirror_nblast.feather`.

## Schema

| column | dtype | description |
|---|---|---|
| `pt_root_id` | string | Root ID of the BANC **query** at the current materialisation; tracks segmentation edits via the supervoxel anchor. |
| `pt_supervoxel_id` | string | A supervoxel of the query neuron. Stable across root-ID changes. |
| `pt_position` | string | Anchor point on the query, BANC voxel space (`"x, y, z"`). |
| `match_root_id` | string | Root ID of the BANC **match** neuron at the current materialisation. |
| `match_supervoxel_id` | string | A supervoxel of the match neuron. |
| `match_position` | string | Anchor point on the match neuron, BANC voxel space. |
| `query_id` | string | BANC root ID of the query at the materialisation the NBLAST was originally computed against; compare with `pt_root_id` to detect stale rows. |
| `match_id` | string | BANC root ID of the match at the same originating materialisation. |
| `score` | double | Normalized NBLAST score in `[-1, 1]` (1 = perfect; same-side homologues typically score ≥ 0.5, serial homologues ≥ 0.3). |
| `root_626` | string | Query root ID at v626 materialisation. |
| `match_root_626` | string | Match root ID at v626. |
| `root_850` | string | Query root ID at v850. |
| `match_root_850` | string | Match root ID at v850. |
| `root_888` | string | Query root ID at v888. |
| `match_root_888` | string | Match root ID at v888. |
| `valid` | string | Flag indicating whether the pair survived expert review (`'t'` / `'f'` / blank for un-reviewed). |

## Usage

In R:

```r
library(arrow); library(dplyr)
m <- read_feather("banc_native_nblast.feather")
# top-k same-side morphological neighbours per neuron
m %>% group_by(root_888) %>% slice_max(score, n = 10)
```

The AN/DN clustering pipeline in the paper repository
(`R/figures/panels_an_dn_umap.R`) consumes this file alongside the
connectivity-derived partner matrix; the canonical clustering used
in the paper is `celltype_partners + Marchenko-Pastur`.

## Related files

- `banc_mirror_nblast.feather` — BANC versus **mirrored** BANC,
  same 16-column schema; the complementary table used for
  bilateral-pair detection.
- `banc_fafb_783_nblast.feather`,
  `banc_hemibrain_v1.2.1_nblast.feather`,
  `banc_manc_v1.2.1_nblast.feather`,
  `banc_malecns_v0.9_nblast.feather`,
  `banc_fanc_1116_nblast.feather` — cross-dataset NBLAST tables
  (11-column schema; one-sided metadata, plus the match's
  cross-dataset cell-type label).
- `registrations/brain_240721/`, `registrations/vnc_240721/` —
  elastix registrations used to bring BANC into the common template
  spaces before scoring.
- `banc_888_meta.feather` — the `cluster`, `manual_cluster`,
  `super_cluster` and `cell_type` columns are downstream of this
  file for the canary AN/DN set.

## Notes

- The file is small in absolute size (~5.7 MB, 49 k rows) because
  it is restricted to the curated canary set, not the full neuron
  population. For an arbitrary BANC neuron outside that set, expect
  no rows in this table.
- Serial-homologue scores are systematically lower than
  bilateral-pair scores because the leg-neuromere templates differ
  slightly in length and the matching synapses are at different
  absolute coordinates. A `score ≥ 0.3` threshold is reasonable for
  serial calls; for same-segment same-side calls use `≥ 0.5`.
- A high NBLAST score is necessary but not sufficient. Confirm
  candidate groupings against connectivity and against the curated
  `cell_type` / `super_cluster` columns in `banc_888_meta.feather`.
