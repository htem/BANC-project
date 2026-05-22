---
filename: banc_malecns_v0.9_nblast.feather
gcs_path: gs://lee-lab_brain-and-nerve-cord-fly-connectome/nblast/banc_malecns_v0.9_nblast.feather
size_bytes: 246058658
size_human: 234.66 MB
nrows: 3840033
ncols: 11
content_type: application/x-arrow

# --- Fields posted to Dataverse ---
description: >-
  NBLAST morphological-similarity table between BANC neurons and the
  Janelia maleCNS v0.9 whole-CNS connectome, bridged into JRC2018F
  (brain) and JRC2018VNCF (VNC) template spaces via the BANC ↔
  JRC2018 elastix registrations (`registrations/brain_240721`,
  `registrations/vnc_240721`). 3 840 033 rows × 11 columns — one
  row per BANC query and maleCNS candidate match, recording
  normalized score and the matched maleCNS cell-type label.
  maleCNS is the only published connectome other than BANC that
  reconstructs the entire central nervous system — brain and
  ventral nerve cord — in a single individual, and is therefore the
  natural cross-dataset comparator for circuits that span the neck
  connective. The cross-sex caveat applies (maleCNS is male, BANC
  is female), but most cell types have clear opposite-sex
  homologues; sexually-dimorphic populations are flagged in
  `banc_888_meta.feather`. Used as one of several reference inputs
  to the iterative cell-type-matching algorithm described in the
  paper Methods, and as the principal whole-CNS validation that
  BANC findings generalize beyond a single individual or sex.
categories:
  - Data
  - NBLAST
directoryLabel: nblast
restrict: false
tabIngest: false
---

# banc_malecns_v0.9_nblast.feather

## Purpose

This file is the pairwise NBLAST morphological-similarity table
between BANC neurons and the Janelia maleCNS v0.9 connectome. Each
row records a candidate match: a BANC `query_root_id`, a maleCNS
`match_id`, the cell-type label of that maleCNS neuron, and the
NBLAST `score`.

maleCNS is, alongside BANC, one of two whole-CNS connectomes
currently available, and the only other dataset in which brain and
VNC neurons are reconstructed in the same volume. It is therefore
indispensable for matching ascending and descending neurons whose
identity depends on having both the brain and the cord half of the
arbor visible in the same individual. The paper uses this NBLAST
together with connectivity-based matching to validate that
neck-spanning circuits identified in BANC are conserved across sex
and across reconstruction pipeline.

## Provenance

Computed by **bancpipeline** (`banc/nblast/banc-nblast-compile.R`):

1. BANC neurons were skeletonised at L2 (chunked-graph) resolution.
2. Brain skeletons were registered into the **JRC2018F** template
   via `registrations/brain_240721`; VNC skeletons into
   **JRC2018VNCF** via `registrations/vnc_240721`.
3. maleCNS v0.9 skeletons were bridged into the corresponding
   JRC2018F / JRC2018VNCF spaces using Janelia's CNS template
   bridges.
4. NBLAST was run with the `natverse` toolchain and normalized
   against query self-scores. Brain-region and VNC-region matches
   are concatenated in this single table.

## Schema

| column | dtype | description |
|---|---|---|
| `pt_root_id` | string | Root ID of the BANC query at the current materialisation; tracks segmentation edits via the supervoxel anchor. |
| `pt_supervoxel_id` | string | A supervoxel of the query neuron, used for chunked-graph re-resolution. Stable across root-ID changes. |
| `pt_position` | string | Anchor point on the query, BANC voxel space (`"x, y, z"`). |
| `query_root_id` | string | BANC root ID at the time the NBLAST was run; compare with `pt_root_id` to detect stale rows. |
| `match_id` | string | maleCNS v0.9 body ID of the candidate match. |
| `match_cell_type` | string | Cell-type label of the maleCNS match (Janelia-curated `type`). |
| `score` | double | Normalized NBLAST score in `[-1, 1]` (1 = perfect; ≥ 0.3 is loose, ≥ 0.5 is solid, ≥ 0.7 is strong). |
| `root_626` | string | Query root ID at v626 materialisation. |
| `root_850` | string | Query root ID at v850. |
| `root_888` | string | Query root ID at v888. |
| `validation` | bool | `TRUE` for matches that survived expert review; `FALSE` (or null) otherwise. |

## Usage

In R via bancr:

```r
library(bancr); library(dplyr)
m <- banc_nblast_matches(dataset = "malecns")
m %>% group_by(query_root_id) %>% slice_max(score, n = 5)
```

The curated top-1 maleCNS cell-type call per BANC neuron is also
exposed as the `malecns_cell_type` column of `banc_888_meta.feather`.

## Related files

- `banc_hemibrain_v1.2.1_nblast.feather`,
  `banc_fafb_783_nblast.feather` — brain-side NBLAST counterparts.
- `banc_manc_v1.2.1_nblast.feather` — VNC counterpart against MANC
  (same sex as maleCNS, deeper VNC type catalog).
- `banc_fanc_1116_nblast.feather` — BANC VNC ↔ female FANC v1116.
- `banc_native_nblast.feather`, `banc_mirror_nblast.feather` —
  within-BANC and BANC-vs-mirror NBLAST tables.
- `registrations/brain_240721/`, `registrations/vnc_240721/` —
  elastix registrations used to bridge into JRC2018F / JRC2018VNCF.
- `banc_888_meta.feather` — `malecns_cell_type` and
  `malecns_nblast_match` columns expose the curated top match.

## Notes

- **Cross-sex caveat.** maleCNS is male, BANC is female. Most cell
  types have clear opposite-sex homologues that NBLAST identifies
  correctly, but sexually-dimorphic populations (P1, pIP10, vPR9,
  TN1, female-specific descending neurons and a subset of
  abdominal-ganglion motor pools) will mis-match or return low
  scores. Consult the `sexually_dimorphic` column of
  `banc_888_meta.feather` before relying on a BANC ↔ maleCNS match
  for these populations.
- maleCNS v0.9 is a preliminary release; cell-type labels are still
  being refined upstream. Some `match_cell_type` values are placeholder
  identifiers rather than published types.
- A high NBLAST score is necessary but not sufficient for a correct
  cell-type match. Use the curated `cell_type` column in
  `banc_888_meta.feather` as the source of truth.
- Scores are normalized against query self-scores; asymmetry between
  paired BANC↔maleCNS queries is expected.
