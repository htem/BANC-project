---
filename: banc_problem_regions.csv
local_path: /Users/papers/BANC-project/manuscript/print/supplemental_data/supplemental_data_10.txt
dataverse_filename: banc_problem_regions.csv
size_bytes: 1215
size_human: 1.2 KiB
nrows: 22
ncols: 7
content_type: text/csv

# --- Fields posted to Dataverse ---
description: >-
  Bounding boxes (in BANC voxel coordinates) of known dataset problem
  regions — alignment artifacts, tissue-folding artifacts, and regions
  of insufficient image quality that callers should be aware of when
  interpreting reconstructions or synapse predictions. 22 regions × 7
  columns: a free-text issue label and the six min/max voxel
  coordinates that define an axis-aligned bounding box. Distributed
  with the paper as `supplemental_data_10.csv`; renamed for the
  Dataverse so that downstream users can find it by topic rather
  than by supplement number.
categories:
  - Documentation
  - Data
directoryLabel: documentation
restrict: false
tabIngest: false
---

# banc_problem_regions.csv

## Purpose

A small reference table listing 22 regions of the BANC volume that have
known quality issues — tissue folds, alignment artifacts, regions with
section damage, regions where the segmentation pipeline produced known
artifacts — and the axis-aligned bounding box that contains each.

The intended use is defensive: callers reconstructing neurons that pass
through one of these boxes should expect missing or spurious segmentation
and synapse predictions, and should treat conclusions drawn from neurons
in these regions with appropriate caution.

## Provenance

Curated manually during proofreading; produced as
`supplemental_data_10.csv` by `R/text/supplemental_data.R` in the
BANC-project repository. Renamed `banc_problem_regions.csv` for the
Dataverse upload, but the contents are unchanged.

## Schema

| column | dtype | description |
|---|---|---|
| `issue` | string | Free-text label for the issue (e.g. `tunnel of death`, `T1 soup`, `dorsal esophageal crush`, `champagne patch`, `left VLP blowout`, `butt wiggle`). Values are not a controlled vocabulary; treat them as descriptive notes. |
| `min_x` | integer | Bounding-box minimum x, BANC voxel space (4 nm per voxel along x). |
| `min_y` | integer | Bounding-box minimum y, BANC voxel space (4 nm per voxel along y). |
| `min_z` | integer | Bounding-box minimum z, BANC voxel space (45 nm per voxel along z). |
| `max_x` | integer | Bounding-box maximum x. |
| `max_y` | integer | Bounding-box maximum y. |
| `max_z` | integer | Bounding-box maximum z. |

## Usage

In R:

```r
pr <- readr::read_csv("banc_problem_regions.csv")
# Is a point inside any flagged region?
inside <- function(x, y, z, pr) {
  with(pr, any(min_x <= x & x <= max_x &
               min_y <= y & y <= max_y &
               min_z <= z & z <= max_z))
}
```

In Python:

```python
import pandas as pd
pr = pd.read_csv("banc_problem_regions.csv")
hit = pr[
    (pr.min_x <= X) & (X <= pr.max_x) &
    (pr.min_y <= Y) & (Y <= pr.max_y) &
    (pr.min_z <= Z) & (Z <= pr.max_z)
]
```

## Related files

- `banc_888_meta.feather` — neurons whose reconstruction is affected
  by one of these regions can carry `status` flags
  (e.g. `TRACING_ISSUE_*`) tagging the affected neurons individually.
- The paper Methods section "Missing data" describes the procedure
  for handling damaged / missing volume regions.

## Notes

- Coordinates are in **BANC voxel space** (4 × 4 × 45 nm). Multiply by
  the voxel size to obtain nanometers.
- Bounding boxes can overlap. A point may be inside more than one
  flagged region.
- The list is curated, not exhaustive — small or subtle artifacts that
  affect single neurons but not larger regions are not represented
  here; rely on the `status` column in the per-neuron metadata for
  per-neuron flags.
