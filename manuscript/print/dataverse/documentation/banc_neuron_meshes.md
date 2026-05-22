---
filename: banc_neuron_meshes.zip
gcs_path: gs://lee-lab_brain-and-nerve-cord-fly-connectome/neuron_meshes/
unzipped_size_bytes: 167656267776
unzipped_size_human: 156.1 GiB
content_type: application/zip

# --- Fields posted to Dataverse ---
description: >-
  Full-resolution segmentation meshes for every BANC segment at v888,
  hosted as a Neuroglancer precomputed-mesh layer. Meshes are the
  triangle-surface output of the chunked-graph mesh pipeline applied
  to the BANC volume segmentation; they capture every protrusion and
  membrane contour the segmentation resolves. Most useful for 3D
  visualization in Neuroglancer, Codex, Blender, or any other
  mesh-aware tool, and for any downstream analysis that needs surface
  geometry rather than just a skeleton. The ZIP is a large packaging
  of the source GCS layer (multi-fragment, sharded files plus the
  `info` manifest). For per-neuron access without downloading the
  ZIP, use the live mesh source via CloudVolume, Neuroglancer at
  ng.banc.community, or the bancr R helper `banc_mesh()`.
categories:
  - Data
  - Meshes
directoryLabel: meshes
restrict: false
tabIngest: false
---

# banc_neuron_meshes.zip

## Purpose

3D meshes for the BANC segmentation at v888. Each mesh is a triangle
surface that traces one segment through the volume, preserving fine
morphology that the L2 skeletons lose. Meshes are the canonical 3D
representation Neuroglancer renders when you load a neuron from BANC.

The set is intended for users who need full geometric detail — for
example, anyone computing volume- or surface-based morphological
features, rendering publication-quality 3D figures, or running
mesh-aware matching algorithms.

## Provenance

Produced by the BANC chunked-graph mesh pipeline over the BANC
volumetric segmentation; hosted on GCS as a Neuroglancer
precomputed-mesh layer (`info` + `meshes/<root_id>:<lod>` fragments).
The ZIP is packaged from that GCS layer at upload time.

## Contents

Not tabular. The ZIP unpacks to a Neuroglancer precomputed-mesh
directory:

- `info` — JSON manifest describing the mesh layer (LOD scheme,
  vertex quantization, etc.).
- `meshes/<root_id>:<lod>` — per-segment mesh fragments at multiple
  levels of detail (binary Neuroglancer mesh shards).
- `segment_properties/` — companion segment properties for the layer.

Loaders that consume Neuroglancer precomputed mesh (CloudVolume,
navis, bancr) handle reassembly automatically.

## Usage

For batch access with `cloud-volume`:

```python
import cloudvolume
cv = cloudvolume.CloudVolume(
    "graphene://https://cave.fanc-fly.com/segmentation/table/banc/v888",
    parallel=True,
)
mesh = cv.mesh.get(720575941521131930)  # returns a trimesh-like object
```

In R via bancr:

```r
library(bancr)
n <- banc_mesh(id = "720575941521131930")
plot3d(n)
```

## Related files

- `banc_swc_skeletons.zip` — per-segment L2 skeletons for the same
  set, preferred when surface detail is not needed.
- `banc_888_meta.feather` — resolves each root ID to identity and
  annotations.
- `banc_neuropil_meshes.zip` — neuropil-region meshes used as
  anatomical context when plotting individual neurons.

## Notes

- Mesh detail varies across the volume — most neurons are fully
  meshed, but small fragments and glia are not always represented.
- The ZIP is very large. For single-neuron access, fetch from the
  live mesh source (CAVE or Neuroglancer) rather than unpacking the
  ZIP.
- Triangles are not de-duplicated across neurons; expect overlapping
  surfaces at contact points between neurons.
- Mesh coordinates are in BANC voxel space (4 × 4 × 45 nm). Multiply
  by the voxel size to obtain nanometers.
