---
filename: cell_info.parquet
gcs_path: gs://lee-lab_brain-and-nerve-cord-fly-connectome/neuron_annotations/v888/cell_info.parquet
size_bytes: 12315918
size_human: 11.75 MB
nrows: 376231
ncols: 10
content_type: application/x-parquet

# --- Fields posted to Dataverse ---
description: >-
  Community-contributed informal annotations on BANC neurons at
  materialization v888. Anyone with edit access in CAVE could add a `tag`
  to a neuron — labels range from cell-type guesses, hemilineage
  assertions and developmental notes to flagged segmentation issues — so
  this table is informal and not curator-vetted. 376 231 rows × 10
  columns; each row pairs a free-form `tag` (and optional `tag2`) with a
  marker point on a neuron, plus the creating-user identifier and CAVE's
  standard point-annotation columns. Useful as a discovery resource for
  surfacing candidate identities, as a source of candidate cell-type
  proposals, and as a record of which neurons have been thought about by
  multiple curators, but it is not the canonical annotation table. For
  the curated taxonomy, use `codex_annotations.parquet` or the
  consolidated `banc_888_meta.feather`. Tags from this file are NOT
  merged wholesale into the meta; only those that survived curator review
  made it into the canonical `cell_type` / `super_class` columns there.
categories:
  - Annotations
  - Data
directoryLabel: annotations/v888
restrict: false
tabIngest: false
---

# cell_info.parquet

## Purpose

`cell_info.parquet` is the CAVE annotation table for **community-contributed
informal labels** on BANC neurons. It is the catch-all bag of free-text
tags that any user with edit access in CAVE could attach to a neuron. The
content ranges across:

- Cell-type guesses ("looks like an OAN") and confirmations.
- Hemilineage assertions, neuromere notes, developmental observations.
- Segmentation flags ("possible merger", "missing primary neurite").
- Annotation campaigns and lab-internal codes.

Because contributions are not curator-vetted, the tags are best treated
as a discovery resource — useful for surfacing candidate identities or
for finding neurons that someone has previously thought about — but not
as a source of truth. The curated cell-type taxonomy lives in
`codex_annotations.parquet` and, ultimately, in `banc_888_meta.feather`.

## Provenance

Authored directly in CAVE by community members and lab annotators. Pulled
from CAVE at the v888 materialization snapshot and deposited here in its
native point-annotation shape. No downstream filtering, harmonization or
spelling correction has been applied; users who depend on cell-type
strings being controlled-vocabulary should not consume this table
directly.

**CAVE description (verbatim)** (created 2023-10-30, voxel resolution 4 × 4 × 45 nm):

> A general-purpose cell type / cell information table. Included are cell types (e.g. broad types like motor neuron, central neuron, sensory neuron, & glia, plus more specific subtypes of each of those), anatomical descriptions (e.g. ascending, descending, soma in brain, soma in VNC), and specific neuron identities (e.g. giant fiber, DNa01), and more — see https://banc.community/Annotations-(cell-types,-etc.) for a list of all annotations.
> 
> Each row of this table is a key-value pair, with the "tag2" column being the key (or parent node in the annotation tree) and the "tag" column being the actual annotation (or child node in the annotation tree). Example key-value pairs are "soma region"-"soma in VNC", "primary class"-"glia", "primary class"-"sensory neuron", "sensory neuron"-"chordotonal neuron", "chordotonal neuron"-"claw chordotonal neuron". A term can be a value in one row but then a key in another row to indicate a subclass.

## Schema

| column | dtype | description |
|---|---|---|
| `id` | int64 | CAVE annotation identifier. |
| `created` | timestamp[us, UTC] | Wall-clock time the annotation was created. |
| `superceded_id` | int64 | If this row replaces an earlier annotation, the `id` of the row it supersedes; `0` otherwise. |
| `valid` | bool | `TRUE` if the row is currently active in CAVE; `FALSE` for superseded rows. |
| `tag` | string | Primary free-form label; controlled vocabulary is NOT enforced. |
| `tag2` | string | Optional secondary label; same lack of vocabulary control. |
| `user_id` | int32 | CAVE user identifier of the contributor. |
| `pt_supervoxel_id` | int64 | Supervoxel containing the marker point. |
| `pt_root_id` | int64 | Root identifier of the marked neuron at v888. |
| `pt_position` | list<int64> | Marker point in BANC voxel space, as a 3-element list `[x, y, z]`. |

## Usage

In R via arrow:

```r
library(arrow); library(dplyr)
ci <- read_parquet("cell_info.parquet") %>% filter(valid)
# search for free-text tag matches
ci %>% filter(grepl("DNa02|dna02", tag, ignore.case = TRUE))
```

In Python via pyarrow:

```python
import pyarrow.dataset as ds
ci = ds.dataset("cell_info.parquet").to_table(
    filter=ds.field("valid") == True
).to_pandas()
ci[ci["tag"].str.contains("hemilineage", na=False, case=False)]
```

## Related files

- `codex_annotations.parquet` — the curated master annotation table;
  the source of truth for cell-type and taxonomy labels.
- `banc_888_meta.feather` — downstream consolidated table; only those
  community labels that survived curator review appear there.
- `cell_representative_point.parquet` — stable per-neuron representative
  point; an alternative join key when many rows touch the same neuron.

## Notes

- **Flat shape.** One row per marker point — unlike `codex_annotations.parquet`,
  which is long-form (one row per `(target_id, classification_system)`).
  A single neuron can still carry many `cell_info` rows from different
  contributors and different time points; group by `pt_root_id` rather
  than treating each row as canonical.
- The `tag` and `tag2` fields are free text; expect typos, abbreviations,
  alternative casing and lab-internal shorthand. Do not assume any string
  appearing here matches a cell type in `banc_888_meta.feather`.
- Rows with `valid = FALSE` are historical and were superseded by a
  later entry; filter to `valid = TRUE` for any active analysis.
- Volume is large (~376k rows) relative to the ~188k neurons in BANC,
  because most neurons accumulate several tags over their proofreading
  life.
