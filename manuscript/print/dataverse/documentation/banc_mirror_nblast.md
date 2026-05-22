---
filename: banc_mirror_nblast.feather
gcs_path: gs://lee-lab_brain-and-nerve-cord-fly-connectome/nblast/banc_mirror_nblast.feather
size_bytes: 338917546
size_human: 323.21 MB
nrows: 2188645
ncols: 16
content_type: application/x-arrow

# --- Fields posted to Dataverse ---
description: >-
  NBLAST morphological-similarity table between every BANC neuron and
  the mirror image of every other BANC neuron, with normalized
  scores. 2 188 645 rows × 16 columns — one row per query × mirrored
  candidate pair, with both query and match metadata (root IDs at
  v626 / v850 / v888, supervoxel IDs, soma positions). Mirroring is
  performed in the JRC2018F (brain) and JRC2018VNCF (VNC) template
  spaces — bridged via `registrations/brain_240721` and
  `registrations/vnc_240721` — because the BANC EM volume is tilted
  relative to the anatomical midline. This is the resource used to
  identify left ↔ right cell-type pairs within BANC: a BANC neuron
  and the mirror of its bilateral partner should return a
  near-perfect NBLAST score. Unlike the cross-dataset NBLAST tables,
  this file carries metadata for both sides of every pair because
  both members may need re-resolution at a downstream
  materialisation. Used as input to the side assignment,
  bilateral-pair detection and `cell_type`-symmetry checks described
  in the paper Methods, and as a standalone resource for users who
  need bilateral pairings.
categories:
  - Data
  - NBLAST
directoryLabel: nblast
restrict: false
tabIngest: false
---

# banc_mirror_nblast.feather

## Purpose

This file is the pairwise NBLAST morphological-similarity table
between BANC neurons and the **mirror image** of all other BANC
neurons. The mirror is taken in JRC2018F (brain) and JRC2018VNCF
(VNC) template space, where the left-right axis is well-defined.

The intended use case is bilateral-pair detection. A BANC neuron's
left-side counterpart, after mirroring, occupies the same template
coordinates as the original neuron. The two should therefore return
a very high NBLAST score against each other. This table is the input
to the side assignment, bilateral-pair detection and `cell_type`
symmetry checks for the paper, and it is the file to consult if you
need pair identities downstream of the published `side` and
`cell_type` columns.

Unlike the cross-dataset NBLAST tables, this file carries metadata
for **both** the query and the match — supervoxel IDs, soma
positions and root IDs at every materialisation — because both
sides of every pair are BANC neurons that callers may want to
re-resolve at their own materialisation.

## Provenance

Computed by **bancpipeline** (`banc/nblast/banc-nblast-compile.R`):

1. BANC neurons were skeletonised at L2 (chunked-graph) resolution.
2. Skeletons were registered into the **JRC2018F** template (brain)
   and **JRC2018VNCF** template (VNC) via the elastix registrations
   deposited alongside this file (`registrations/brain_240721`,
   `registrations/vnc_240721`).
3. Each neuron was reflected across the template midline.
4. NBLAST was run between every BANC neuron and the mirrored set,
   then normalized against query self-scores.

## Schema

| column | dtype | description |
|---|---|---|
| `pt_root_id` | string | Root ID of the BANC **query** at the current materialisation; tracks segmentation edits via the supervoxel anchor. |
| `pt_supervoxel_id` | string | A supervoxel of the query neuron. Stable across root-ID changes. |
| `pt_position` | string | Anchor point on the query, BANC voxel space (`"x, y, z"`). |
| `match_root_id` | string | Root ID of the **mirrored match** BANC neuron at the current materialisation. |
| `match_supervoxel_id` | string | A supervoxel of the match neuron. |
| `match_position` | string | Anchor point on the match neuron, BANC voxel space. |
| `query_id` | string | BANC root ID of the query at the materialisation the NBLAST was originally computed against; compare with `pt_root_id` to detect stale rows. |
| `match_id` | string | BANC root ID of the match at the same originating materialisation. |
| `score` | double | Normalized NBLAST score in `[-1, 1]` (1 = perfect; bilateral-pair candidates usually score ≥ 0.5). |
| `root_626` | string | Query root ID at v626 materialisation. |
| `match_root_626` | string | Match root ID at v626. |
| `valid` | string | Flag indicating whether the pair survived expert review (`'t'` / `'f'` / blank for un-reviewed). |
| `root_850` | string | Query root ID at v850. |
| `match_root_850` | string | Match root ID at v850. |
| `root_888` | string | Query root ID at v888. |
| `match_root_888` | string | Match root ID at v888. |

## Usage

In R, the top mirror-NBLAST match per neuron is the bilateral-pair
candidate:

```r
library(arrow); library(dplyr)
m <- read_feather("banc_mirror_nblast.feather")
pairs <- m %>% group_by(root_888) %>% slice_max(score, n = 1)
```

For most neurons, `match_root_888` is the bilateral partner; for
unpaired neurons (midline, asymmetric, sex-specific) the top match
is either the neuron itself or a low-score artefact and should be
filtered on `score`.

## Related files

- `banc_native_nblast.feather` — within-BANC NBLAST **without**
  mirroring; used for serial-homologue detection and cell-type
  clustering.
- `banc_fafb_783_nblast.feather`,
  `banc_hemibrain_v1.2.1_nblast.feather`,
  `banc_manc_v1.2.1_nblast.feather`,
  `banc_malecns_v0.9_nblast.feather`,
  `banc_fanc_1116_nblast.feather` — cross-dataset NBLAST tables
  (11-column schema, one-sided metadata).
- `registrations/brain_240721/`, `registrations/vnc_240721/` —
  elastix registrations used to bridge into JRC2018F / JRC2018VNCF
  before mirroring.
- `banc_888_meta.feather` — `side` column derives partly from this
  table; the curated `cell_type` should be symmetric across pairs
  identified here.

## Notes

- Mirroring is performed in template space, not in BANC native
  space, because the BANC EM volume is slightly tilted with respect
  to the anatomical midline. The elastix registrations are
  therefore load-bearing for symmetry analysis.
- Midline neurons (e.g. some MBONs, the giant fibre, descending
  neurons with bilateral arbors) self-match and have no genuine
  bilateral partner; their top mirror match is the neuron itself.
- Sexually-dimorphic populations may have a high-score mirror match
  that is **not** a homologue but a near-symmetric distractor. Use
  `cell_type` / `super_class` from `banc_888_meta.feather` to
  filter.
- A high NBLAST score is necessary but not sufficient: confirm
  candidate bilateral pairs against connectivity (shared partners
  in `banc_888_edgelist_simple_v3.feather`) before treating them
  as definitive.
