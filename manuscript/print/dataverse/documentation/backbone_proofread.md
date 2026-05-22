---
filename: backbone_proofread.parquet
gcs_path: gs://lee-lab_brain-and-nerve-cord-fly-connectome/neuron_annotations/v888/backbone_proofread.parquet
size_bytes: 10316976
size_human: 9.84 MB
nrows: 211947
ncols: 10
content_type: application/x-parquet

# --- Fields posted to Dataverse ---
description: >-
  Upstream CAVE annotation table recording every neuron that has reached
  the "backbone proofread" bar at materialization v888. Backbone-proofread
  is a lower bar than full proofreading — primary neurites and major
  microtubule-rich processes have been thoroughly reviewed, so the overall
  morphology of the cell is expected to be correct, but minor branches
  and a small number of synapses may still require adjustment (paper
  Methods, "Proofreading"). 211 947 rows × 10 columns; each row is one
  marker point on a neuron with a boolean proofread flag, a creating-user
  identifier, and CAVE's standard point-annotation columns
  (`pt_supervoxel_id`, `pt_root_id`, `pt_position`). Flows into the
  `roughly_proofread` column of `banc_888_meta.feather`. Download this
  file directly if you want the native CAVE shape, the per-row provenance
  (`created`, `user_id`), historical rows superseded by later edits
  (`valid = FALSE`), or if you want to drive your own re-aggregation of
  proofreading status without depending on the consolidated rollup. Most
  users should instead consume `banc_888_meta.feather` and filter on
  `roughly_proofread`.
categories:
  - Annotations
  - Data
directoryLabel: annotations/v888
restrict: false
tabIngest: false
---

# backbone_proofread.parquet

## Purpose

`backbone_proofread.parquet` is the CAVE annotation table that records
which BANC neurons have been **backbone-proofread**. A neuron is marked
backbone-proofread when its primary neurites (if not sensory), or its
major microtubule-rich processes, have undergone a thorough review (paper
Methods, "Proofreading"). This is a lower bar than full proofreading — it
indicates that the overall morphology of the cell is expected to be
correct and that future proofreading is not anticipated to radically
alter the neuron's core shape or identity, even if minor branches or a
small number of synapses may still require adjustment.

Each row is a marker point placed on a neuron, carrying CAVE's standard
point-annotation columns plus a boolean `proofread` flag and the
identifier of the proofreader who created the entry. The same neuron may
have multiple historical entries; only rows with `valid = TRUE` are
active at v888.

## Provenance

Authored directly in CAVE by the BANC proofreading teams (Princeton,
SixEleven, Aelysia, individual labs and citizen scientists; 155
proofreaders in total — paper Methods, "Proofreading"). Pulled from CAVE
at the v888 materialization snapshot and deposited here in its native
shape, before any downstream aggregation.

**CAVE description (verbatim)** (created 2024-03-30, voxel resolution 4 × 4 × 45 nm):

> Cells that have had their backbone proofread, meaning there are no major false merge errors in the object and all the major branches of the backbone have been extended so no significant parts of the neuron are missing.
> "valid_id" contains the segment ID of the neuron at the time the annotation was made.
> "user_id" contains the CAVE user ID of the user who created the annotation.

The downstream consumer is `banc_888_meta.feather`, where this table
contributes the `roughly_proofread` boolean. (The fully-proofread
`proofread` column in the meta is derived from a separate CAVE flag plus
the proofreading review; see `proofreading_notes.parquet`.)

## Schema

| column | dtype | description |
|---|---|---|
| `id` | int64 | CAVE annotation identifier (per-row, unique within the table). |
| `created` | timestamp[us, UTC] | Wall-clock time the annotation was created. |
| `superceded_id` | int64 | If this row replaces an earlier annotation, the `id` of the row it supersedes; `0` otherwise. |
| `valid` | bool | `TRUE` if the row is currently active in CAVE; `FALSE` for rows superseded by a later entry. |
| `valid_id` | int64 | The currently-valid `id` for the same logical annotation lineage (self-reference when `valid = TRUE`). |
| `proofread` | bool | `TRUE` if the neuron at this marker point is backbone-proofread. |
| `user_id` | int32 | CAVE user identifier of the proofreader who created the row. |
| `pt_supervoxel_id` | int64 | Supervoxel containing the marker point; used to resolve to the current root via the chunked graph. |
| `pt_root_id` | int64 | Root identifier of the marked neuron at the v888 materialization. |
| `pt_position` | list<int64> | Marker point in BANC voxel space, as a 3-element list `[x, y, z]`. |

## Usage

In R via arrow:

```r
library(arrow); library(dplyr)
bp <- read_parquet("backbone_proofread.parquet") %>%
  filter(valid, proofread)
nrow(bp)                       # active backbone-proofread markers at v888
length(unique(bp$pt_root_id))  # distinct neurons
```

In Python via pyarrow:

```python
import pyarrow.dataset as ds
bp = ds.dataset("backbone_proofread.parquet").to_table(
    filter=(ds.field("valid") == True) & (ds.field("proofread") == True)
).to_pandas()
```

To filter against the curated metadata, join on `pt_root_id` (as
`banc_888_id` in `banc_888_meta.feather`).

## Related files

- `banc_888_meta.feather` — downstream consumer; this table contributes
  the `roughly_proofread` boolean.
- `proofreading_notes.parquet` — sibling annotation table carrying
  per-neuron proofreader notes and flagged issues.
- `codex_annotations.parquet` — master annotation table; join on
  `pt_root_id` to look up the cell type and other curations for any
  backbone-proofread neuron.
- `cell_representative_point.parquet` — stable per-neuron representative
  point, an alternative join key for cross-table work.

## Notes

- **Flat shape.** One row per marker point — unlike `codex_annotations.parquet`,
  which is long-form (one row per `(target_id, classification_system)`).
- `pt_position` is stored as a list of three int64 values in voxel space
  (4 × 4 × 45 nm). Multiply by the voxel size to obtain nanometers.
- Rows where `valid = FALSE` are kept for audit, not for active use.
  Filter to `valid = TRUE` for any analytical work.
- A given neuron may have several backbone-proofread marker points
  placed at different times; treat `pt_root_id` rather than `id` as the
  natural per-neuron key.
- The full-proofreading flag (`proofread == "TRUE"` in
  `banc_888_meta.feather`) is a stricter status than the boolean in this
  file. See paper Methods, "Proofreading" for the distinction.
