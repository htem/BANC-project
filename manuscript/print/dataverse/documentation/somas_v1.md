---
filename: somas_v1.parquet
gcs_path: gs://lee-lab_brain-and-nerve-cord-fly-connectome/neuron_annotations/v888/somas_v1.parquet
size_bytes: 5350459
size_human: 5.10 MB
nrows: 153892
ncols: 7
content_type: application/x-parquet

# --- Fields posted to Dataverse ---
description: >-
  Automatic nucleus / soma detection table for BANC at materialization
  v888. 153 892 rows × 7 columns; each row is one detected nucleus, with
  a marker point at the nucleus centroid in BANC voxel space, the
  supervoxel that contains it, and the root identifier of the segment it
  currently belongs to. Produced by the BANC nucleus segmentation
  pipeline (CNN over the EM volume) and used to seed proofreading —
  automatically identified nuclei account for every neuron with a cell
  body inside the CNS, which we then proofread and extended to
  reconstruct the full morphology (paper Methods, "Proofreading").
  Contributes the `nucleus_id` and soma-position columns of
  `banc_888_meta.feather`. Download this file directly when you need the
  per-nucleus granularity (e.g. segments owning more than one nucleus due
  to a merger), the link between a nucleus and the root it currently
  belongs to, the soma positions in voxel space for all somatic neurons,
  or nuclei that have not yet been assigned to a proofread neuron.
  `v1` in the filename refers to the nucleus-segmentation version, not
  the BANC materialization.
categories:
  - Annotations
  - Data
directoryLabel: annotations/v888
restrict: false
tabIngest: false
---

# somas_v1.parquet

## Purpose

`somas_v1.parquet` is BANC's automatic **nucleus / soma detection
table**. Each row is one detected nucleus, with a marker point at the
nucleus centroid in BANC voxel space, the supervoxel that contains it,
and the root identifier of the segment the nucleus currently belongs to.
This is the table that allowed us to account for every neuron with a
cell body inside the CNS — we proofread segments associated with
automatically-detected nuclei and then extended them to reconstruct the
full morphology (paper Methods, "Proofreading").

Downstream, `banc_888_meta.feather` consumes this table for the
`nucleus_id` and soma-position columns of every CNS-resident neuron.
This file remains useful when you want per-nucleus granularity (for
example, neurons that own more than one nucleus due to a merger, or
nuclei that have not yet been assigned to a segmented neuron) rather
than the per-neuron rollup in the meta.

## Provenance

Produced by the BANC nucleus-detection segmentation pipeline, which runs
a CNN over the EM volume to identify nuclei (paper Methods, "Specimen
and dataset description" and "Proofreading"). The `v1` in the filename
refers to the version of the nucleus segmentation, not the BANC
materialization (which is v888 throughout the deposit).

**CAVE description (verbatim)** (created 2024-03-11, voxel resolution 4 × 4 × 45 nm):

> (This is the most up-to-date full soma table, but see also "somas_v1b" which contains corrected points for 651 nuclei.)
> 
> Points in the middle of nuclei that were automatically found by a convolutional neural network. Version 1 of the automated nucleus segmentation, created March 8th 2024 by Zetta AI. In total 180,130 nuclei were included in the initial list of nucleus predictions that passed a very low size threshold of 0.0368 cubic microns, but the majority of predictions with size smaller than 4 cubic microns appear to be false positives and so are not included in this table. Additionally, two enormous false positives with size greater than 250 cubic microns were excluded. After applying this filter (4 to 250 cubic microns), 154,533 nuclei remained, comprising the points in this table.
> 
> The points here are the centroid of the nucleus segmentation object, which is usually fine to use but sometimes that point is outside either the nucleus segmentation or the full cell segmentation. The table "somas_v1b" contains updated points for 651 nuclei where it was possible to "move" the centroid to a nearby location that is both in the nucleus segmentation and in the full cell segmentation.

`pt_root_id` is updated to the current v888 root for each nucleus via
CAVE chunked-graph queries.

## Schema

| column | dtype | description |
|---|---|---|
| `id` | int64 | Per-nucleus identifier (the value that appears as `nucleus_id` in `banc_888_meta.feather`). |
| `created` | timestamp[us] | Wall-clock time the nucleus annotation was created (no timezone tag — UTC by convention). |
| `superceded_id` | string | If this row replaces an earlier nucleus annotation, the `id` of the row it supersedes (typed as string in this file, unlike the int64 superceded_id columns in sibling tables); empty otherwise. |
| `valid` | bool | `TRUE` if the row is currently active in CAVE; `FALSE` for superseded rows. |
| `pt_supervoxel_id` | int64 | Supervoxel containing the nucleus centroid. |
| `pt_root_id` | int64 | Root identifier of the segment that currently owns the nucleus at the v888 materialization. May be `0` (or fall back to a non-neuronal segment) for nuclei not yet attached to a proofread neuron. |
| `pt_position` | list<int64> | Nucleus centroid in BANC voxel space, as a 3-element list `[x, y, z]`. |

## Usage

In R via arrow:

```r
library(arrow); library(dplyr)
sn <- read_parquet("somas_v1.parquet") %>% filter(valid)
length(unique(sn$pt_root_id))   # distinct segments owning a nucleus
sn %>% count(pt_root_id, sort = TRUE) %>% head()  # segments with >1 nucleus
```

In Python via pyarrow:

```python
import pyarrow.dataset as ds
sn = ds.dataset("somas_v1.parquet").to_table(
    filter=ds.field("valid") == True
).to_pandas()
```

## Companion precomputed volume

The annotation table here pairs with a precomputed nucleus-segmentation
volume — voxel-labelled mask of each detected nucleus — that is **not**
deposited on Dataverse (too large for mirroring). Two copies exist on
GCS; we recommend the Lee-lab mirror as it has no planned deletion:

- **Lee-lab mirror (preferred):**
  `precomputed://gs://lee-lab_brain-and-nerve-cord-fly-connectome/nuclei/seg_v1`
- **Zetta original:**
  `precomputed://gs://zetta_lee_fly_cns_001_kisuk/final/nucleus/v1/seg`
  (may be deleted)

To view in Spelunker / Neuroglancer add a segmentation layer with the
Lee-lab source on top of the BANC EM image
(`precomputed://gs://zetta_lee_fly_cns_001_alignment/v1_sharded`).

## Related files

- `banc_888_meta.feather` — downstream consolidated table; this file is
  the source of its `nucleus_id` and soma-position columns.
- `cell_representative_point.parquet` — sibling per-neuron point table,
  but curated rather than automated; in many cases the representative
  point sits inside the cell body whose nucleus is in this file.
- `codex_annotations.parquet` — master curated annotation table; join on
  `pt_root_id` to look up cell-type and taxonomy labels for any
  nucleated neuron.

## Notes

- **Flat shape.** One row per detected nucleus — unlike
  `codex_annotations.parquet`, which is long-form (one row per
  `(target_id, classification_system)`).
- **Schema asymmetry.** The `superceded_id` column is typed as `string`
  in this file (unlike the `int64` `superceded_id` columns in the
  sibling tables) and `created` is `timestamp[us]` without a UTC
  timezone tag. These are quirks of how the upstream nucleus detection
  pipeline wrote its annotations; consumers can cast as needed.
- **Multi-nucleus segments.** Where `pt_root_id` is repeated, the
  segment owns more than one nucleus — typically a merger that has not
  yet been split during proofreading. Filter against the
  `roughly_proofread` / `proofread` flags in `banc_888_meta.feather` to
  exclude these cases when counting cells.
- **Coverage.** Roughly 154k nuclei are detected — fewer than the ~188k
  rows in `banc_888_meta.feather`, because nucleated cell bodies are
  only one of the entry points; the meta also includes neurons whose
  cell bodies lie outside the CNS (sensory) and large fragments without
  a nucleus that are nevertheless cell-typed.
- `pt_position` is in voxel space (4 × 4 × 45 nm). Multiply by voxel
  size to obtain nanometers.
- Rows with `valid = FALSE` are historical; filter to `valid = TRUE` for
  active work.
