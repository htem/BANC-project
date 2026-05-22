---
filename: cell_representative_point.parquet
gcs_path: gs://lee-lab_brain-and-nerve-cord-fly-connectome/neuron_annotations/v888/cell_representative_point.parquet
size_bytes: 5503064
size_human: 5.25 MB
nrows: 158265
ncols: 7
content_type: application/x-parquet

# --- Fields posted to Dataverse ---
description: >-
  One stable representative point per BANC neuron at materialization v888.
  Each row is a single marker point — typically inside the cell body or
  near the primary neurite — chosen to give each neuron a durable join
  key that survives proofreading edits to the neuron's geometry. 158 265
  rows × 7 columns; CAVE's standard point-annotation columns plus the
  per-row provenance (`created`, `valid`, `superceded_id`). This is the
  table that `codex_annotations.parquet` joins against via `target_id`,
  so it is the natural pivot between the long-form annotation table and
  the per-neuron downstream tables such as `banc_888_meta.feather`. Use
  this file when you need a small, fast lookup from `id` (or
  `pt_root_id`) to a canonical 3D point for a neuron, when reconstructing
  the long-to-wide pivot that produces the flat cell-type table, or when
  you want a per-neuron point that is guaranteed to lie on the neuron
  itself rather than being inferred from a soma centroid or supervoxel
  midpoint.
categories:
  - Annotations
  - Data
directoryLabel: annotations/v888
restrict: false
tabIngest: false
---

# cell_representative_point.parquet

## Purpose

`cell_representative_point.parquet` assigns each BANC neuron a single,
**stable representative point** — a marker placed inside the cell body
or near the primary neurite. The point is intentionally durable: even
when the neuron's reconstructed geometry changes through proofreading,
this marker is updated to track the same biological cell, so it can be
used as a per-neuron join key across CAVE annotation tables.

The companion table `codex_annotations.parquet` is structured as one row
per (neuron × classification system); the link from that long-form table
back to a per-neuron entity is `target_id` → `id` on this file. Without
this table, the long-to-wide pivot that produces the flat cell-type table
cannot be reconstructed.

## Provenance

Authored in CAVE by the BANC annotation team. One row per neuron, placed
as part of the annotation workflow so that downstream tables
(`codex_annotations.parquet`, the various tag tables) have a stable
target to refer to. Pulled from CAVE at v888 and deposited here in its
native shape.

**CAVE description (verbatim)** (created 2025-07-10, voxel resolution 1 × 1 × 1 (placeholder) nm):

> Testing of new annotations.
> 
> (The CAVE table description is currently a placeholder; the table is in active rollout. Use the schema and notes in this file as the authoritative reference.)

## Schema

| column | dtype | description |
|---|---|---|
| `id` | int64 | Per-neuron CAVE annotation identifier. This is the value that `codex_annotations.target_id` refers to. |
| `created` | timestamp[us, UTC] | Wall-clock time the representative point was created. |
| `superceded_id` | int64 | If this row replaces an earlier representative point, the `id` of the row it supersedes; `0` otherwise. |
| `valid` | bool | `TRUE` if the row is currently active in CAVE; `FALSE` for superseded rows. |
| `pt_supervoxel_id` | int64 | Supervoxel containing the marker point. |
| `pt_root_id` | int64 | Root identifier of the neuron at the v888 materialization. |
| `pt_position` | list<int64> | Representative point in BANC voxel space, as a 3-element list `[x, y, z]`. |

## Usage

In R via arrow:

```r
library(arrow); library(dplyr)
rep <- read_parquet("cell_representative_point.parquet") %>%
  filter(valid)

# join codex_annotations -> rep -> meta
codex <- read_parquet("codex_annotations.parquet") %>% filter(valid_ref, valid)
flat  <- codex %>% inner_join(rep, by = c("target_id" = "id"))
```

In Python via pyarrow:

```python
import pyarrow.dataset as ds
rep = ds.dataset("cell_representative_point.parquet").to_table(
    filter=ds.field("valid") == True
).to_pandas()
```

## Related files

- `codex_annotations.parquet` — long-form curated annotation table;
  joins to this file on `target_id` → `id`.
- `banc_888_meta.feather` — downstream per-neuron consolidated metadata;
  join on `pt_root_id` (= `banc_888_id`).
- `somas_v1.parquet` — sibling per-neuron point table, but seeded from
  automated nucleus detection rather than from a curated representative
  point.

## Notes

- **Flat shape.** One row per marker point — unlike `codex_annotations.parquet`,
  which is long-form (one row per `(target_id, classification_system)`).
- `id` is unique per neuron within the v888 snapshot and is the canonical
  target for cross-table joins in CAVE's annotation schema.
- `pt_position` is stored as a 3-element list of int64 in voxel space
  (4 × 4 × 45 nm).
- The number of rows (158 265) is smaller than the number of root IDs in
  `banc_888_meta.feather` (188 162) because not every segment in the
  meta has been annotated with a representative point — small fragments
  and non-neuronal segments are excluded.
- Rows with `valid = FALSE` are historical and were superseded; filter
  to `valid = TRUE` for active work.
