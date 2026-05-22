---
filename: neck_connective_y121000.parquet
gcs_path: gs://lee-lab_brain-and-nerve-cord-fly-connectome/neuron_annotations/v888/neck_connective_y121000.parquet
size_bytes: 85778
size_human: 83.8 KB
nrows: 3712
ncols: 8
content_type: application/x-parquet

# --- Fields posted to Dataverse ---
description: >-
  CAVE annotation table of seed points placed on the cross-section of
  the neck connective at the y = 121 000 plane (one of the two neck-
  connective sampling planes; the deeper one). Each row is one point,
  tagged with the half of the neck connective it sits on (`neck
  connective (left)` or `neck connective (right)`, viewed from the
  fly's perspective). 3 712 rows × 8 columns at v888. Used as the seed
  set for identifying neck-traversing neurons (AN / DN counts, side
  assignment, and the per-neuron coloured cross-section panels in
  Extended Data Fig. 2a). Pairs with the shallower `neck_connective_y92500`
  table — together they bracket the neck connective and let consumers
  decide which plane (or both) to use.
categories:
  - Annotations
  - Data
directoryLabel: annotations/v888
restrict: false
tabIngest: false
---

# neck_connective_y121000.parquet

## Purpose

`neck_connective_y121000.parquet` is one of the two **neck-connective
seed tables** in BANC. Each row is one CAVE point placed in the neck
connective at the y = 121 000 plane, the deeper of the two sampling
planes. The point is tagged with the side of the connective it sits on
(left or right, from the fly's perspective).

These seed points are how we enumerate "neurons that pass through the
neck connective" — ANs (ascending), DNs (descending), or visceral /
neurosecretory cells with axons that traverse the neck. Tag-side
counting yields the per-side AN / DN counts cited in Extended Data
Fig. 2a; intersecting the y = 92 500 and y = 121 000 sets cross-checks
that an axon truly traverses the connective rather than terminating
inside it.

## Provenance

CAVE annotation table `neck_connective_y121000` (created 2023-11-07,
last modified 2024-01-04; the table is `PUBLIC` read-write). Voxel
resolution `(4, 4, 45)` nm. Created and curated by the BANC team;
exported at the v888 materialization.

**CAVE description (verbatim)**: "A collection of points placed in the
neck connective at the plane with y coordinate 121000. The tag
indicates which half of the neck connective (left or right) the point
is associated with. (Note that 'left' and 'right' are specified from
the fly's perspective — if you are using a neuroglancer state that
hasn't explicitly had the image data flipped to address this, the left
half of the image data will be the fly's right and the right half of
the image data will be the fly's left.)"

## Schema

| column | dtype | description |
|---|---|---|
| `id` | string | CAVE annotation identifier of the seed point. |
| `created` | timestamp[us] | Wall-clock time the annotation was created. |
| `superceded_id` | string | If this row replaces an earlier annotation, the `id` of the row it supersedes; empty otherwise. |
| `valid` | bool | `TRUE` if the annotation is currently active in CAVE; `FALSE` for superseded rows. |
| `tag` | string | One of `"neck connective (left)"` or `"neck connective (right)"`. Left / right are **from the fly's perspective** — in unflipped Neuroglancer states, fly-left appears on the right side of the image and vice versa. |
| `pt_supervoxel_id` | string | Supervoxel containing the seed point. May be `NA` for points that have not been re-anchored after segmentation edits. |
| `pt_root_id` | string | Root identifier of the segment that owns the seed point at v888 (i.e. the neuron passing through the connective at this point). May be `NA` for unanchored points. |
| `pt_position` | list<int64> | Seed point in BANC voxel space, as a 3-element list `[x, 121000, z]` — the `y` coordinate is by definition `121000`. |

## Usage

In R via arrow:

```r
library(arrow); library(dplyr)
y121 <- read_parquet("neck_connective_y121000.parquet") %>% filter(valid)
y121 %>% count(tag)            # per-side seed counts
length(unique(y121$pt_root_id))   # distinct neck-traversing neurons at y=121000
```

In Python via pyarrow:

```python
import pyarrow.dataset as ds
y121 = ds.dataset("neck_connective_y121000.parquet").to_table(
    filter=ds.field("valid") == True
).to_pandas()
```

To compare against the y = 92 500 plane:

```r
y92  <- read_parquet("neck_connective_y92500.parquet")  %>% filter(valid)
both <- intersect(na.omit(y121$pt_root_id), na.omit(y92$pt_root_id))
length(both)   # neurons traversing both planes
```

## Related files

- `neck_connective_y92500.parquet` — the **other** neck-connective seed
  table (shallower plane). Use the two together to identify neurons
  that traverse the entire neck connective rather than just one plane.
- `banc_888_meta.feather` — per-neuron metadata. Join on `pt_root_id`
  to retrieve cell-type, super-class, region etc. for each neck-
  traversing neuron.
- `peripheral_nerves.parquet` — sibling CAVE seed table for points at
  the peripheral nerve entry / exit sites.

## Notes

- **Two planes, not one.** The neck connective is sampled at y = 92 500
  AND y = 121 000. Each table is a self-sufficient seed set; users
  often want the intersection (true through-passing axons) or the
  union (any neuron touching the connective).
- **Side is from the fly's perspective.** If you visualise the
  annotations alongside the EM image without the standard image flip,
  fly-left will appear on the right side of your screen. CAVE-served
  Neuroglancer states (e.g. `https://ng.banc.community/2026a/neck-connective`)
  apply the flip; bespoke states may not.
- **`pt_supervoxel_id` / `pt_root_id` can be NA.** These are populated
  by re-anchoring seed points after segmentation edits; some points
  are anchored only at the position level. Treat NA as "no
  re-anchoring at this materialization yet" rather than "no neuron at
  this point."
- **`pt_position` is in voxel space** (4 × 4 × 45 nm). Multiply by the
  voxel size to obtain nanometres.
- Rows with `valid = FALSE` are historical (superseded); filter to
  `valid = TRUE` for active analysis.
