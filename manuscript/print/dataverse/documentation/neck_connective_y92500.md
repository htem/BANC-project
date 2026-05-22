---
filename: neck_connective_y92500.parquet
gcs_path: gs://lee-lab_brain-and-nerve-cord-fly-connectome/neuron_annotations/v888/neck_connective_y92500.parquet
size_bytes: 139052
size_human: 135.79 KB
nrows: 3652
ncols: 8
content_type: application/x-parquet

# --- Fields posted to Dataverse ---
description: >-
  CAVE annotation table marking every neuronal profile in the y = 92500
  cross-section of the neck connective at materialization v888, the
  anterior seed plane used to identify neurons that transit the neck
  connective between brain and VNC. 3 652 rows × 8 columns; each row is
  one marker point on a profile, with an optional `tag` capturing the
  curator's note for that profile and CAVE's standard point-annotation
  columns. Combined with the posterior plane at y = 121000, this is the
  basis for the AN/DN identification described in paper Methods,
  "Neurons of the neck connective". Use this file when you need to
  reconstruct which segments crossed the anterior neck plane in the v888
  snapshot, including profiles that turned out not to be true ANs or DNs
  (sensory or efferent ANs, intrinsic neurons of the neck region, false
  positives that did not survive review), when you want the
  curator-attached notes on individual profiles, or when you want raw
  marker points for visualization. For the consolidated per-neuron AN /
  DN classification, consume `banc_888_meta.feather` instead.
categories:
  - Annotations
  - Data
directoryLabel: annotations/v888
restrict: false
tabIngest: false
---

# neck_connective_y92500.parquet

## Purpose

`neck_connective_y92500.parquet` is the CAVE annotation table for the
**y = 92500 seed plane** through the neck connective. We seeded two
transverse planes through the neck connective — at y = 92500 (anterior)
and y = 121000 (posterior) — and reviewed every neuronal profile in
both planes, in order to capture every neuron that transits the neck
connective between brain and VNC (paper Methods, "Proofreading" and
"Neurons of the neck connective"). This file holds the per-profile
records for the y = 92500 plane.

Each row is one marker point placed on one profile, carrying CAVE's
standard point-annotation columns plus a free-text `tag` that captures
the curator's note for that profile (cell-type guess, proofreading
status, flag for review, etc.). The neurons that survive review and
match cross-dataset reference labels become the ascending and descending
populations analyzed throughout the paper.

## Provenance

CAVE annotation table `neck_connective_y92500` (created 2023-10-15,
voxel resolution `(4, 4, 45)` nm). Authored by the BANC neck-connective
annotation team; pulled from CAVE at the v888 materialization snapshot
and deposited here in native point-annotation shape.

**CAVE description (verbatim)**: "A collection of points placed in the
neck connective at the plane with y coordinate 92500. The tag
indicates which half of the neck connective (left or right) the point
is associated with. (Note that 'left' and 'right' are specified from
the fly's perspective — if you are using a neuroglancer state that
hasn't explicitly had the image data flipped to address this, the left
half of the image data will be the fly's right and the right half of
the image data will be the fly's left.)"

Pairs with `neck_connective_y121000.parquet` (the deeper sampling
plane). Together the two tables bracket the neck connective.

The corresponding analysis numbers in the paper are reported against the
**combined** anterior + posterior seed-plane review, not against this
file alone. For the consolidated per-neuron AN/DN identification, use
the `super_class` column of `banc_888_meta.feather`
(`super_class %in% c("ascending", "descending")`). The v888 `region`
column does **not** carry a `neck_connective` value — `region` partitions
neurons into `central_brain`, `optic_lobe`, `ventral_nerve_cord`, or
`cervical_connective`, and ascending/descending neurons span those
regions (the cell bodies are in brain or VNC; only their axons cross
the cervical connective).

## Schema

| column | dtype | description |
|---|---|---|
| `id` | int64 | CAVE annotation identifier. |
| `created` | timestamp[us, UTC] | Wall-clock time the annotation was created. |
| `superceded_id` | int64 | If this row replaces an earlier annotation, the `id` of the row it supersedes; `0` otherwise. |
| `valid` | bool | `TRUE` if the row is currently active in CAVE; `FALSE` for superseded rows. |
| `tag` | string | Curator's free-text note for the profile (cell-type guess, proofreading flag, review status). Controlled vocabulary is not enforced. |
| `pt_supervoxel_id` | int64 | Supervoxel containing the marker point. |
| `pt_root_id` | int64 | Root identifier of the marked neuron at the v888 materialization. |
| `pt_position` | list<int64> | Marker point in BANC voxel space, as a 3-element list `[x, y, z]`. y is fixed at 92500 for all rows. |

## Usage

In R via arrow:

```r
library(arrow); library(dplyr)
neck <- read_parquet("neck_connective_y92500.parquet") %>%
  filter(valid)
nrow(neck)                          # profiles in the anterior plane
length(unique(neck$pt_root_id))     # distinct root IDs reached
```

In Python via pyarrow:

```python
import pyarrow.dataset as ds
neck = ds.dataset("neck_connective_y92500.parquet").to_table(
    filter=ds.field("valid") == True
).to_pandas()
```

## Related files

- `banc_888_meta.feather` — downstream consolidated table; AN/DN
  identification lives in `super_class` and `region`.
- `peripheral_nerves.parquet` — sibling seed-plane table for axons
  entering or exiting peripheral nerves; the analogous tool for
  identifying sensory and motor neurons.
- `codex_annotations.parquet` — master annotation table; join on
  `pt_root_id` for the curated cell-type and taxonomy labels of any
  neck-transiting neuron.

## Notes

- **Flat shape.** One row per profile in the seed plane — unlike
  `codex_annotations.parquet`, which is long-form (one row per
  `(target_id, classification_system)`).
- **`y` is fixed at 92500** for every row in this file. The companion
  posterior plane (`y = 121000`) is held in a separate CAVE table that
  was not part of this Dataverse deposit; the consolidated AN/DN
  identification in `banc_888_meta.feather` reflects both planes.
- Not every row corresponds to a true AN or DN — some profiles turn out
  to be intrinsic neurons of the neck region, sensory or efferent ANs
  (which the paper excludes from the AN/DN analyses; see paper Methods,
  "Neurons of the neck connective"), or false positives that did not
  survive review.
- Rows with `valid = FALSE` are historical; filter on `valid = TRUE` for
  active work.
- For the high-level AN / DN counts cited in the paper (~1 849 ANs and
  ~1 316 DNs), consume `banc_888_meta.feather` rather than counting rows
  in this file.
