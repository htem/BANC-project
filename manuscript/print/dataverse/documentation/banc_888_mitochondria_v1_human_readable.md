---
filename: banc_888_mitochondria_v1_human_readable.csv.gz
gcs_path: gs://lee-lab_brain-and-nerve-cord-fly-connectome/neuron_annotations/v888/mitochondria_v1_human_readable.csv.gz
size_bytes: 1107598431
size_human: 1.0 GiB
content_type: application/gzip

# --- Fields posted to Dataverse ---
description: >-
  Raw, gzipped CSV of every BANC mitochondrion detection at the v888
  materialization — one row per mitochondrion (38 928 244 rows total),
  with the CAVE annotation id, centroid position in voxel space, voxel
  count (size), and the supervoxel id / root_id at v888 that contains
  the mitochondrion's centroid. Produced from the v1 mitochondria
  segmentation built by Zetta on 2025-04-23 over the BANC EM volume
  (see `banc_mitochondria_v1.md` for the upstream precomputed volume).
  This CSV is the per-row table you join against `banc_888_meta.feather`
  to count mitochondria per neuron, query mitochondria by spatial
  position, or relate mitochondrial content to per-synapse statistics
  in `banc_888_synapses_v2_enriched.parquet`.
categories:
  - Annotations
  - Data
directoryLabel: neuron_annotations
restrict: false
tabIngest: false
---

# banc_888_mitochondria_v1_human_readable.csv.gz

## Purpose

`banc_888_mitochondria_v1_human_readable.csv.gz` is the **per-mitochondrion
annotation table** at the v888 BANC materialization. Each row is one
mitochondrion detected in the v1 mitochondria segmentation volume; the
columns give the CAVE id, the centroid in voxel space, the voxel count,
and the supervoxel / root_id at v888 that contains the centroid.

Use this table to:

- count mitochondria per neuron (group-by `pt_root_id`);
- relate mitochondrial content to per-synapse statistics by joining
  against `banc_888_synapses_v2_enriched.parquet` on `pt_root_id`;
- spatially query mitochondria (filter on `pt_x`, `pt_y`, `pt_z` in
  voxel coordinates).

The CSV is the **human-readable** projection of the underlying CAVE
annotation table `mitochondria_v1` — list-typed position columns are
unpacked into `pt_x / pt_y / pt_z` scalars.

## Provenance

Generated from the v1 mitochondria segmentation built by Zetta on
2025-04-23 over the BANC EM volume; the upstream precomputed volume
is documented separately in [`banc_mitochondria_v1.md`](banc_mitochondria_v1.md)
(not deposited — too large to mirror). The CAVE annotation table was
created on 2025-05-15.

**CAVE `mitochondria_v1` description (verbatim)** (created 2025-05-15,
voxel resolution 16 × 16 × 45 nm):

> Created from gs://zetta_lee_fly_cns_001_mito/250423_mito/assignment/merged_cleft_info.df

## Schema

The CSV has **no header row**; column order is fixed and matches the
CAVE table. Confirm on your copy with:

```bash
gunzip -c banc_888_mitochondria_v1_human_readable.csv.gz | head -2
```

| col | name | dtype | description |
|-----|------|-------|-------------|
| 1 | `id` | int64 | CAVE mitochondrion identifier. |
| 2 | `pt_x` | int32 | Centroid x in voxel space (16 nm voxels). |
| 3 | `pt_y` | int32 | Centroid y in voxel space (16 nm voxels). |
| 4 | `pt_z` | int32 | Centroid z in voxel space (45 nm voxels). |
| 5 | `size` | int64 | Mitochondrion footprint in voxels. |
| 6 | `pt_supervoxel_id` | int64 | Supervoxel containing the centroid. |
| 7 | `pt_root_id` | int64 | v888 root_id of the neuron containing the centroid. |

## Usage

1.0 GiB gzipped, 38 928 244 rows — too large to load whole-file into
R on a laptop without paging. Stream the file instead:

R (data.table, streaming via `cmd`):

```r
library(data.table)
cn <- c("id", "pt_x", "pt_y", "pt_z", "size",
        "pt_supervoxel_id", "pt_root_id")
mito <- fread(cmd = paste(
  "gsutil cat",
  "gs://lee-lab_brain-and-nerve-cord-fly-connectome/neuron_annotations/",
  "v888/mitochondria_v1_human_readable.csv.gz | gunzip"),
  col.names = cn)
mito[pt_root_id == 720575941521131930, .N]
```

Python (pyarrow streaming):

```python
import pyarrow.csv as pcsv
opts = pcsv.ReadOptions(
    column_names=["id","pt_x","pt_y","pt_z","size",
                  "pt_supervoxel_id","pt_root_id"])
reader = pcsv.open_csv("banc_888_mitochondria_v1_human_readable.csv.gz",
                       read_options=opts)
```

## Related files

- `banc_mitochondria_v1.md` — upstream precomputed segmentation
  volume (not deposited; pointed at via the GCS URL).
- `banc_888_synapses_v2_enriched.parquet` — per-synapse table with
  matching `pt_root_id` convention; join to relate mitochondrial
  content to synaptic statistics per neuron.
- `banc_888_meta.feather` — per-neuron metadata at v888; join on
  `pt_root_id == root_id` for cell-type-grouped analyses.

## Notes

- **Mitochondria-segmentation `v1` is independent of BANC materialization
  `v888`.** The voxel labelling does not change with CAVE materialization
  versions; only `pt_root_id` updates as proofreaders merge / split
  neurons.
- **No header row.** Column names listed in the Schema table above are
  prescribed by CAVE convention but must be supplied at read time.
- **Coordinates are in voxel space at 16 × 16 × 45 nm** (the mitochondria
  segmentation voxel size), not the 4 × 4 × 45 nm BANC EM voxel size.
  Multiply by 16 / 16 / 45 to obtain nanometres.
- **38 928 244 rows** — confirmed 2026-05-22 by streaming
  `gsutil cat ... | gunzip | wc -l`.
