---
filename: peripheral_nerves.parquet
gcs_path: gs://lee-lab_brain-and-nerve-cord-fly-connectome/neuron_annotations/v888/peripheral_nerves.parquet
size_bytes: 629173
size_human: 614.43 KB
nrows: 15615
ncols: 8
content_type: application/x-parquet

# --- Fields posted to Dataverse ---
description: >-
  CAVE annotation table marking every axon profile that enters or exits a
  peripheral nerve in BANC at materialization v888. 15 615 rows × 8
  columns; each row is one marker point on one axon profile at the nerve
  seed plane, with a `tag` recording the nerve identity (one of 48
  bilateral nerves, controlled vocabulary) and CAVE's standard
  point-annotation columns. We manually identified 48 nerves and
  verified that every axon profile in their seed cross-section
  corresponds to a segmented neuron (paper Methods, "Proofreading"; 47
  seed planes total, since two adjacent nerves enter as a single merged
  bundle in one location and share one seed plane there). This is the
  upstream source for the `nerve`
  column of `banc_888_meta.feather` and the basis for sensory and motor
  identification across BANC. Download this file directly when you need
  the per-profile granularity, the curator's tags, the per-row
  provenance (`created`, `valid`, supersession lineage), or rows in their
  native CAVE shape rather than the consolidated per-neuron rollup in
  the meta.
categories:
  - Annotations
  - Data
directoryLabel: annotations/v888
restrict: false
tabIngest: false
---

# peripheral_nerves.parquet

## Purpose

`peripheral_nerves.parquet` is the CAVE annotation table for the
**peripheral-nerve seed planes**. To capture all neurons with cell bodies
outside the CNS — primarily sensory neurons and the motor neurons that
project out to peripheral targets — we manually identified 48 nerves and
seeded every neuron profile in a transverse plane through each nerve
(paper Methods, "Proofreading"; 47 seed planes total, since two adjacent
nerves enter as a single merged bundle in one location and share one seed
plane there).

Each row in this file is one marker point placed on one axon profile at
one of those seed planes, with a `tag` that records which nerve the
profile belongs to (e.g. `left_antennal_nerve`,
`right_mesothoracic_leg_nerve`, `right_cervical_nerve`). This is the
upstream source for the `nerve` column of `banc_888_meta.feather`, and
forms the basis for identifying sensory and motor neurons via their
`super_class` and `flow` annotations.

## Provenance

Authored in CAVE by the BANC peripheral-nerve annotation team. Each
profile in each seed plane was inspected and labeled. Pulled from CAVE
at the v888 materialization snapshot and deposited here in native
point-annotation shape.

**CAVE description (verbatim)** (created 2023-10-30, voxel resolution 4 × 4 × 45 nm):

> A collection of points placed in the axons of neurons as they travel through peripheral nerves. The tag indicates the name of the peripheral nerve, including its side (left vs right). (Note that "left" and "right" are specified from the fly's perspective — if you are using a neuroglancer state that hasn't explicitly had the image data flipped to address this, the left half of the image data will be the fly's right and the right half of the image data will be the fly's left.)

## Schema

| column | dtype | description |
|---|---|---|
| `id` | int64 | CAVE annotation identifier. |
| `created` | timestamp[us, UTC] | Wall-clock time the annotation was created. |
| `superceded_id` | int64 | If this row replaces an earlier annotation, the `id` of the row it supersedes; `0` otherwise. |
| `valid` | bool | `TRUE` if the row is currently active in CAVE; `FALSE` for superseded rows. |
| `tag` | string | Nerve identity (e.g. `left_antennal_nerve`, `right_metathoracic_leg_nerve`, `right_cervical_nerve`). Uses the controlled vocabulary documented in the BANC annotation taxonomy. |
| `pt_supervoxel_id` | int64 | Supervoxel containing the marker point. |
| `pt_root_id` | int64 | Root identifier of the marked neuron at the v888 materialization. |
| `pt_position` | list<int64> | Marker point in BANC voxel space, as a 3-element list `[x, y, z]`. |

## Usage

In R via arrow:

```r
library(arrow); library(dplyr)
pn <- read_parquet("peripheral_nerves.parquet") %>% filter(valid)
pn %>% count(tag, sort = TRUE)              # rows per nerve
pn %>% distinct(pt_root_id, tag) %>% count(tag, sort = TRUE)  # neurons per nerve
```

In Python via pyarrow:

```python
import pyarrow.dataset as ds
pn = ds.dataset("peripheral_nerves.parquet").to_table(
    filter=ds.field("valid") == True
).to_pandas()
pn.groupby("tag")["pt_root_id"].nunique().sort_values(ascending=False)
```

## Related files

- `banc_888_meta.feather` — downstream consolidated table; this file is
  the source of its `nerve` column. Sensory / motor identification flows
  from here via `super_class` and `flow`.
- `neck_connective_y92500.parquet` — sibling seed-plane table for axons
  transiting the neck connective; the analogous tool for AN / DN
  identification.
- `codex_annotations.parquet` — master annotation table; join on
  `pt_root_id` to look up the curated cell-type and taxonomy labels for
  any peripheral-nerve neuron.
- BANC annotation taxonomy reference — controlled vocabulary for the
  `tag` values (the 48 nerve names).

## Notes

- **Flat shape.** One row per marker point — unlike `codex_annotations.parquet`,
  which is long-form (one row per `(target_id, classification_system)`).
- The 48 nerves correspond to the bilateral peripheral nerves that are
  separable when they enter the CNS. Two adjacent nerves enter as a
  single merged bundle at one location and share a single seed plane
  there, which is why the paper reports 47 seed planes for 48 nerves
  (paper Methods, "Proofreading").
- A neuron can appear in this table multiple times when its axon was
  seeded in more than one plane (e.g. branched peripheral processes).
  Use `(pt_root_id, tag)` rather than `id` as the per-neuron-per-nerve
  key.
- Damaged-nerve caveat: dissection damaged both the left and right
  antennal nerves, so sensory neurons from Johnston's organ and nearby
  central-brain intrinsic neurons that pass close to the entry point are
  less well represented (paper Methods, "Specimen and dataset
  description"). The annotations are still present, but the underlying
  neurons may be incompletely reconstructed.
- Rows with `valid = FALSE` are historical; filter to `valid = TRUE` for
  active work.
