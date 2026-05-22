---
filename: banc_hemibrain_v1.2.1_nblast.feather
gcs_path: gs://lee-lab_brain-and-nerve-cord-fly-connectome/nblast/banc_hemibrain_v1.2.1_nblast.feather
size_bytes: 198943946
size_human: 189.73 MB
nrows: 2963651
ncols: 11
content_type: application/x-arrow

# --- Fields posted to Dataverse ---
description: >-
  NBLAST morphological-similarity table between BANC neurons and the
  Janelia Hemibrain v1.2.1 central-brain connectome, bridged into the
  JRC2018F template via the BANC ↔ JRC2018F elastix registration
  (`registrations/brain_240721`). 2 963 651 rows × 11 columns — one
  row per BANC query neuron and Hemibrain candidate match, recording
  normalized score and the matched Hemibrain cell-type label.
  Hemibrain is a partial volume covering most of the right central
  brain of a female fly, so matches are dense in the central brain
  and absent for the optic lobes, the contralateral side, and the
  entire ventral nerve cord. Used as one of several reference inputs
  to the iterative cell-type-matching algorithm described in the
  paper Methods ("Cell-type matching and annotation"); the Hemibrain
  match is the headline link for central-brain cell types that
  pre-date FAFB-FlyWire typing, and the only NBLAST link available
  for many classical Janelia central-brain types. Suitable as a
  standalone resource for morphology-based search against Hemibrain
  body IDs and as the upstream input for reproducing the BANC
  central-brain cell typing.
categories:
  - Data
  - NBLAST
directoryLabel: nblast
restrict: false
tabIngest: false
---

# banc_hemibrain_v1.2.1_nblast.feather

## Purpose

This file is the pairwise NBLAST morphological-similarity table
between BANC neurons and Janelia's Hemibrain v1.2.1 dataset. Each row
records a candidate match: a BANC `query_root_id`, a Hemibrain
`match_id` (Hemibrain body ID), the cell-type label of that Hemibrain
neuron, and the NBLAST `score`.

Hemibrain remains the most heavily-typed central-brain resource in
the field. For most classical Drosophila central-brain cell types,
the Hemibrain label is the canonical name. This NBLAST is therefore
the primary morphology bridge from BANC into the existing central-brain
type catalog, and it is the only one of the cross-dataset NBLAST
files that resolves to Hemibrain body IDs directly.

## Provenance

Computed by **bancpipeline** (`banc/nblast/banc-nblast-compile.R`):

1. BANC neurons were skeletonised at L2 (chunked-graph) resolution.
2. Skeletons were registered into the **JRC2018F** template via the
   BANC-to-JRC2018F elastix registration deposited alongside this
   file (`registrations/brain_240721`).
3. Hemibrain v1.2.1 skeletons (in their native space) were bridged
   into JRC2018F using the standard Janelia hemibrain → JRC2018F
   bridge.
4. NBLAST was run with the `natverse` toolchain and normalized
   against query self-scores.

Because Hemibrain covers only a partial brain volume, BANC neurons
whose arbors fall outside the Hemibrain bounding box have no
candidate matches in this table.

## Schema

| column | dtype | description |
|---|---|---|
| `pt_root_id` | string | Root ID of the BANC query at the current materialisation; tracks segmentation edits via the supervoxel anchor. |
| `pt_supervoxel_id` | string | A supervoxel of the query neuron, used for chunked-graph re-resolution. Stable across root-ID changes. |
| `pt_position` | string | Anchor point on the query, BANC voxel space (`"x, y, z"`). |
| `query_root_id` | string | BANC root ID at the time the NBLAST was run; compare with `pt_root_id` to detect stale rows. |
| `match_id` | string | Hemibrain v1.2.1 body ID of the candidate match. |
| `match_cell_type` | string | Cell-type label of the Hemibrain match (Janelia-curated `type`). |
| `score` | double | Normalized NBLAST score in `[-1, 1]` (1 = perfect; ≥ 0.3 is loose, ≥ 0.5 is solid, ≥ 0.7 is strong). |
| `root_626` | string | Query root ID at v626 materialisation. |
| `root_850` | string | Query root ID at v850. |
| `root_888` | string | Query root ID at v888. |
| `validation` | bool | `TRUE` for matches that survived expert review; `FALSE` (or null) otherwise. |

## Usage

In R via bancr:

```r
library(bancr); library(dplyr)
m <- banc_nblast_matches(dataset = "hemibrain")
m %>% group_by(query_root_id) %>% slice_max(score, n = 5)
```

The curated, top-1 Hemibrain cell-type call per BANC neuron is also
exposed as the `hemibrain_cell_type` column of `banc_888_meta.feather`.

## Related files

- `banc_fafb_783_nblast.feather` — analogous NBLAST against the
  full-female-brain FAFB-FlyWire v783 connectome.
- `banc_manc_v1.2.1_nblast.feather` — VNC counterpart to this table
  (BANC ↔ MANC v1.2.1).
- `banc_malecns_v0.9_nblast.feather` — BANC ↔ Janelia maleCNS
  (whole-CNS, male).
- `banc_fanc_1116_nblast.feather` — BANC VNC ↔ FANC v1116.
- `banc_native_nblast.feather`, `banc_mirror_nblast.feather` —
  within-BANC and BANC-vs-mirror NBLAST tables.
- `registrations/brain_240721/` — BANC ↔ JRC2018F elastix
  registration used to bridge into Hemibrain space.
- `banc_888_meta.feather` — `hemibrain_cell_type` and
  `hemibrain_nblast_match` columns expose the curated top match.

## Notes

- Hemibrain covers only the right hemisphere of the central brain.
  For a BANC neuron whose homologous Hemibrain neuron lives on the
  left side, the match is to the mirror partner; symmetry has to be
  resolved at the cell-type level rather than the body-ID level.
- A high NBLAST score is necessary but not sufficient for a correct
  cell-type match. Always treat `cell_type` in `banc_888_meta.feather`
  as the source of truth; the top-NBLAST row is the seed, not the
  verdict.
- Scores are normalized against query self-scores, not against the
  match's self-score; asymmetry between paired BANC↔Hemibrain queries
  is expected.
