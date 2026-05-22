---
filename: (not deposited — precomputed volume external link only)
gcs_path: gs://zetta_lee_fly_cns_001_mito/250423_mito/seg
content_type: precomputed (Neuroglancer)

# --- Fields posted to Dataverse ---
description: >-
  BANC mitochondria segmentation, version v1 (built 2025.04.23). A
  precomputed multi-resolution segmentation volume aligned to the BANC
  EM data; each voxel labelled with an integer mitochondrion identifier.
  The volume is the upstream source for the per-mitochondrion annotation
  table (deposited separately) and is browseable in Neuroglancer / Spelunker
  by pointing at the GCS source URL below. We do not deposit the
  precomputed volume itself on Dataverse — at multi-terabyte volume scale
  it is impractical to mirror — but we include this stub doc so that users
  can locate the upstream copy and so that the volume's existence is
  citable alongside the annotation table.
categories:
  - Annotations
  - Data
directoryLabel: external_links
restrict: false
tabIngest: false
---

# banc_mitochondria_v1 (precomputed segmentation, external)

## Purpose

`banc_mitochondria_v1` is the **precomputed mitochondria-segmentation
volume** that underlies the BANC mitochondria annotation table. Each
voxel is labelled with an integer mitochondrion identifier; loading the
volume in Neuroglancer / Spelunker over the BANC EM image lets users
see individual mitochondria as 3D-segmented bodies and traverse the
per-mitochondrion CAVE annotations.

This volume is **not deposited in this Dataverse** — its multi-terabyte
size makes mirroring impractical. The deposit instead ships the
per-mitochondrion **annotation table** (see Related files); this stub
doc records the precomputed source so users can reproduce the volume
visualisation.

## Provenance

Produced by Zetta over the BANC EM volume on 2025-04-23 (v1; the only
released version). Served from the same GCS project as the BANC
segmentation. No Lee-lab mirror exists as of the deposit date — point
visualisers directly at the Zetta path.

**CAVE description (verbatim)** (created 2025-05-15, voxel resolution 16 × 16 × 45 nm):

> Created from gs://zetta_lee_fly_cns_001_mito/250423_mito/assignment/merged_cleft_info.df

## Source

```
precomputed://gs://zetta_lee_fly_cns_001_mito/250423_mito/seg
```

To view in Spelunker / Neuroglancer, add a segmentation layer with this
source on top of the BANC EM image layer
(`precomputed://gs://zetta_lee_fly_cns_001_alignment/v1_sharded`).

## Related files

- `banc_888_mitochondria.parquet` — per-mitochondrion CAVE annotation
  table at materialization v888 (one row per mitochondrion, with the
  parent root_id, centroid position, and a bounding-box pair). This is
  the per-row table you join against `banc_888_meta.feather` to count
  mitochondria per neuron or to query mitochondria by spatial position.
- `banc_888_synapses_v2_enriched.parquet` — per-synapse table with
  matching CAVE-id conventions. Combine the two tables to relate
  per-synapse statistics to mitochondrial content per neuron.

## Notes

- **Mitochondria-segmentation `v1` is independent of BANC materialization
  `v888`.** The voxel labelling does not change with CAVE materialization
  versions; only the `pt_root_id` field of the **annotation** table
  updates as proofreaders merge / split neurons.
- **No Lee-lab mirror** is planned at this time (as of 2025-06-05). If
  the Zetta path becomes unavailable, the annotation table is still
  usable — only the voxel-level visualisation depends on the precomputed
  volume.
