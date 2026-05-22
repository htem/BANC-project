---
filename: banc_fafb_783_nblast.feather
gcs_path: gs://lee-lab_brain-and-nerve-cord-fly-connectome/nblast/banc_fafb_783_nblast.feather
size_bytes: 578407490
size_human: 551.62 MB
nrows: 8254752
ncols: 11
content_type: application/x-arrow

# --- Fields posted to Dataverse ---
description: >-
  NBLAST morphological-similarity table between BANC neurons and the
  full-female-brain FAFB-FlyWire v783 connectome, bridged into the
  JRC2018F template via the BANC ↔ JRC2018F elastix registration
  (`registrations/brain_240721`). 8 254 752 rows × 11 columns — one
  row per BANC query neuron and FAFB candidate match, recording the
  normalized score, the matched FAFB cell-type label, and the BANC
  query root ID at v626 / v850 / v888 for cross-materialisation
  joins. FAFB-FlyWire is the most thoroughly cell-typed brain
  connectome and the principal reference for BANC brain cell types:
  most central-brain and DN identities in BANC are seeded from this
  table before manual review. Same-sex (female) and whole-brain
  coverage make this the natural brain-side companion to MANC for
  the VNC. Used as the brain-side morphology input to the iterative
  cell-type-matching algorithm described in the paper Methods
  ("Cell-type matching and annotation"), and suitable as a
  standalone resource for morphology-based search against FAFB and
  as the upstream input for reproducing the BANC brain cell typing.
categories:
  - Data
  - NBLAST
directoryLabel: nblast
restrict: false
tabIngest: false
---

# banc_fafb_783_nblast.feather

## Purpose

This file is the pairwise NBLAST morphological-similarity table between
BANC neurons and FAFB-FlyWire v783 neurons, bridged into a common
template space. Each row records a candidate match: a BANC `query_root_id`,
a FAFB `match_id`, the cell-type label of that FAFB neuron, and the
NBLAST `score`. The table is intentionally **long** (one row per pair,
not a square matrix), so that a user can filter to the top-k matches for
each query without first inverting a sparse matrix.

It is the headline morphology-side resource the BANC paper used to
seed cell-type matching against the whole female brain. The same
similarity table appears as the right-hand side of every "did NBLAST
agree with the curated match?" analysis.

## Provenance

Computed by **bancpipeline** (`banc/nblast/banc-nblast-compile.R`):

1. BANC neurons were skeletonised at L2 (chunked-graph) resolution.
2. Skeletons were registered into the **JRC2018F** template via the
   BANC-to-JRC2018F elastix registration deposited alongside this file
   (`registrations/brain_240721`).
3. FAFB-FlyWire v783 skeletons (already in JRC2018F space) were used as
   targets.
4. NBLAST was run with the `natverse` toolchain, normalized against
   self-scores per neuron.

A separate file (`banc_native_nblast.feather`) does the analogous
all-versus-all NBLAST within BANC itself; the mirror complement
(`banc_mirror_nblast.feather`) compares each BANC neuron against the
mirror image of every other BANC neuron, the input for left/right pair
matching.

## Schema

| column | dtype | description |
|---|---|---|
| `pt_root_id` | string | Root ID of the **BANC query** neuron at the current materialisation. Tracks segmentation edits via the supervoxel anchor. |
| `pt_supervoxel_id` | string | A supervoxel of the query neuron, used for chunked-graph re-resolution. Stable across root-ID changes. |
| `pt_position` | string | Anchor point on the query neuron, BANC voxel space (`"x, y, z"`). |
| `query_root_id` | string | BANC root ID at the time the NBLAST was run; compare with `pt_root_id` to detect stale rows. |
| `match_id` | string | Root ID of the candidate FAFB-FlyWire v783 match (FlyWire `root_783`). |
| `match_cell_type` | string | Cell-type label of the FAFB match (FAFB-curated `type`). |
| `score` | double | Normalized NBLAST score in `[-1, 1]` (1 = perfect; ≥ 0.3 is loose, ≥ 0.5 is solid, ≥ 0.7 is strong). |
| `root_626` | string | Query root ID at v626 materialisation. |
| `root_850` | string | Query root ID at v850. |
| `root_888` | string | Query root ID at v888. |
| `validation` | bool | `TRUE` for matches that survived expert review; `FALSE` (or null) otherwise. |

## Usage

In R via bancr:

```r
library(bancr); library(dplyr)
m <- banc_nblast_matches(dataset = "fafb")
# top-k by score
m %>% group_by(query_root_id) %>% slice_max(score, n = 5)
```

The table is large in absolute rows (~8.3 M) but small per query — most
BANC neurons appear with their top ~25-50 FAFB matches.

## Related files

- `banc_native_nblast.feather` — within-BANC NBLAST (used for serial
  homologue detection within the VNC and for cell-type clustering).
- `banc_mirror_nblast.feather` — BANC versus mirrored BANC, used for
  left/right pair matching.
- `banc_manc_v1.2.1_nblast.feather`, `banc_malecns_v0.9_nblast.feather`,
  `banc_hemibrain_v1.2.1_nblast.feather`,
  `banc_fanc_1116_nblast.feather` — analogous NBLAST tables to other
  reference connectomes; same schema modulo the `match_cell_type` source.
- `registrations/brain_240721/` — the BANC ↔ JRC2018F elastix
  registration used to bridge into FAFB's template space.
- `banc_888_meta.feather` — `fafb_cell_type`, `fafb_match` and
  `fafb_nblast_match` columns expose the curated top match.

## Notes

- A high NBLAST score is necessary but not sufficient for a correct
  cell-type match. A substantial fraction of curator-accepted cell-type
  labels do not agree with the top-NBLAST match (`validation = FALSE`
  on the top-1 row); use the curated `cell_type` column in
  `banc_888_meta.feather` as the source of truth.
- Scores are normalized against query self-scores, not against the
  match's self-score. Asymmetry can therefore appear when the same pair
  is scored in both directions.
- The optic-lobe match coverage is intentionally limited — different
  matching machinery (connectivity-based, see the paper Methods,
  "Iterative cross-dataset alignment") is more useful there.
