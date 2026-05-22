---
filename: banc_manc_v1.2.1_nblast.feather
gcs_path: gs://lee-lab_brain-and-nerve-cord-fly-connectome/nblast/banc_manc_v1.2.1_nblast.feather
size_bytes: 121791594
size_human: 116.15 MB
nrows: 1906831
ncols: 11
content_type: application/x-arrow

# --- Fields posted to Dataverse ---
description: >-
  NBLAST morphological-similarity table between BANC VNC neurons and
  the Janelia MANC v1.2.1 male adult nerve cord connectome, bridged
  into the JRC2018VNCF template via the BANC ↔ JRC2018VNCF elastix
  registration (`registrations/vnc_240721`). 1 906 831 rows × 11
  columns — one row per BANC VNC query and MANC candidate match,
  recording normalized score and the matched MANC cell-type label.
  MANC is the deepest typed VNC resource available and the primary
  morphology bridge for BANC VNC cell types: the headline ascending-
  and descending-neuron catalogs, motor-neuron typing and intrinsic
  VNC cell types in the BANC paper are all seeded from this table
  before manual review. The cross-sex caveat is real — MANC is male,
  BANC is female — but most VNC neurons have a clear opposite-sex
  homologue; sexually-dimorphic populations are flagged in
  `banc_888_meta.feather`. Used as the VNC-side morphology input to
  the iterative cell-type-matching algorithm described in the paper
  Methods, and suitable as a standalone resource for VNC morphology
  search against MANC.
categories:
  - Data
  - NBLAST
directoryLabel: nblast
restrict: false
tabIngest: false
---

# banc_manc_v1.2.1_nblast.feather

## Purpose

This file is the pairwise NBLAST morphological-similarity table
between BANC VNC neurons and Janelia's MANC v1.2.1 male adult nerve
cord connectome. Each row records a candidate match: a BANC
`query_root_id`, a MANC `match_id` (MANC body ID), the cell-type
label of that MANC neuron, and the NBLAST `score`.

MANC is currently the most heavily typed VNC connectome and the
single most useful reference for BANC's VNC neurons. The headline
ascending-neuron and descending-neuron catalogs, motor-neuron
typing, and intrinsic VNC cell types in the BANC paper are all
seeded from this NBLAST table before manual review.

## Provenance

Computed by **bancpipeline** (`banc/nblast/banc-nblast-compile.R`):

1. BANC neurons were skeletonised at L2 (chunked-graph) resolution.
2. Skeletons were registered into the **JRC2018VNCF** template via
   the BANC-to-JRC2018VNCF elastix registration deposited alongside
   this file (`registrations/vnc_240721`).
3. MANC v1.2.1 skeletons (in their native space) were bridged into
   JRC2018VNCF using the standard Janelia MANC → JRC2018VNCF bridge.
4. NBLAST was run with the `natverse` toolchain and normalized
   against query self-scores.

## Schema

| column | dtype | description |
|---|---|---|
| `pt_root_id` | string | Root ID of the BANC query at the current materialisation; tracks segmentation edits via the supervoxel anchor. |
| `pt_supervoxel_id` | string | A supervoxel of the query neuron, used for chunked-graph re-resolution. Stable across root-ID changes. |
| `pt_position` | string | Anchor point on the query, BANC voxel space (`"x, y, z"`). |
| `query_root_id` | string | BANC root ID at the time the NBLAST was run; compare with `pt_root_id` to detect stale rows. |
| `match_id` | string | MANC v1.2.1 body ID of the candidate match. |
| `match_cell_type` | string | Cell-type label of the MANC match (Janelia-curated `type`). |
| `score` | double | Normalized NBLAST score in `[-1, 1]` (1 = perfect; ≥ 0.3 is loose, ≥ 0.5 is solid, ≥ 0.7 is strong). |
| `root_626` | string | Query root ID at v626 materialisation. |
| `root_850` | string | Query root ID at v850. |
| `root_888` | string | Query root ID at v888. |
| `validation` | bool | `TRUE` for matches that survived expert review; `FALSE` (or null) otherwise. |

## Usage

In R via bancr:

```r
library(bancr); library(dplyr)
m <- banc_nblast_matches(dataset = "manc")
m %>% group_by(query_root_id) %>% slice_max(score, n = 5)
```

The curated top-1 MANC cell-type call per BANC VNC neuron is also
exposed as the `manc_cell_type` column of `banc_888_meta.feather`.

## Related files

- `banc_fanc_1116_nblast.feather` — VNC NBLAST against the female
  FANC v1116 connectome (same sex as BANC; smaller cell-type
  catalog).
- `banc_fafb_783_nblast.feather`,
  `banc_hemibrain_v1.2.1_nblast.feather` — brain-side counterparts.
- `banc_malecns_v0.9_nblast.feather` — BANC ↔ Janelia maleCNS, also
  male and covering the whole CNS.
- `banc_native_nblast.feather`, `banc_mirror_nblast.feather` —
  within-BANC and BANC-vs-mirror NBLAST tables.
- `registrations/vnc_240721/` — BANC ↔ JRC2018VNCF elastix
  registration used to bridge into MANC space.
- `banc_888_meta.feather` — `manc_cell_type` and `manc_nblast_match`
  columns expose the curated top match.

## Notes

- **Cross-sex caveat.** MANC is from a male fly; BANC is from a
  female. Most VNC cell types have one-to-one female ↔ male
  homologues at the morphology level, and the NBLAST works well
  even across sex. But sexually-dimorphic cell types (notably the
  pIP10 / vPR9 / TN1 chains, female-specific descending neurons,
  and some abdominal-ganglion motor pools) will mis-match or
  return low scores. Consult the `sexually_dimorphic` column of
  `banc_888_meta.feather` before relying on a male-female NBLAST
  match for these populations.
- A high NBLAST score is necessary but not sufficient for a correct
  cell-type match. Always use the curated `cell_type` column in
  `banc_888_meta.feather` as the source of truth.
- Scores are normalized against query self-scores; asymmetry between
  paired BANC↔MANC queries is expected.
