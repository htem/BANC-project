---
filename: banc_microCT.zip
gcs_path: gs://lee-lab_brain-and-nerve-cord-fly-connectome/microCT/
unzipped_size_bytes: 1306644843
unzipped_size_human: 1.22 GiB
content_type: application/zip

# --- Fields posted to Dataverse ---
description: >-
  X-ray microCT volume of the BANC fly, acquired before
  electron-microscopy sectioning. Provides whole-body anatomical
  context — head, thorax, abdomen and appendages — that the EM volume
  itself does not cover (the EM imaged only the CNS). Useful for
  placing CNS structures in the body, validating peripheral-target
  assignments for sensory and motor neurons, and providing
  supplementary context for figures. Hosted on GCS as a Neuroglancer
  precomputed multi-resolution image volume; the ZIP distributed here
  packages the same volume plus auxiliary outline meshes and a
  provenance record.
categories:
  - Data
  - Images
directoryLabel: microCT
restrict: false
tabIngest: false
---

# banc_microCT.zip

## Purpose

A pre-EM X-ray microCT scan of the same fly used for BANC. The microCT
volume covers the whole insect — not just the CNS — so it can be used
to place neurons in the larger body plan, to inspect the route of
peripheral nerves through cuticle and muscle, and to give figures a
whole-body anatomical reference that the EM volume cannot provide.

## Provenance

Acquired at Harvard Medical School with assistance from Richard Schalek
prior to serial-section EM sample preparation. The volume is the source
referenced in the paper Methods section "Specimen" and Extended Data
Fig. 1.

## Contents

Not tabular. The ZIP unpacks to a Neuroglancer precomputed image
directory:

- `info` — JSON manifest (`uint16`, single channel, voxel sizes at
  multiple resolution levels with the finest at ~1.3 µm isotropic).
- `provenance` — JSON acquisition / processing record.
- `1304.7_1304.7_1304.7/`, `2608_2608_2608/`, `5216_5216_5216/`,
  `10432_10432_10432/` — chunked image data at successive
  downsampling levels.
- `meshes/` — auxiliary surface meshes (e.g. body outline).

The volume lives in its own coordinate space (micrometers per voxel
per the `info` resolutions), not in BANC EM voxel space.

## Usage

For viewing, point Neuroglancer (or any precomputed-aware viewer) at
the unpacked directory. For Fiji / ImageJ / napari workflows, convert
one level to a TIFF stack first — for example with `cloud-volume`:

```python
import cloudvolume, numpy as np, tifffile
cv = cloudvolume.CloudVolume(f"file://{path_to_unpacked_dir}",
                             mip=0, parallel=True)
vol = cv[:][..., 0]  # (X, Y, Z) uint16
tifffile.imwrite("banc_microCT.tif", np.transpose(vol, (2, 1, 0)))
```

## Companion precomputed sources

The deposit ships the image volume as a ZIP. Two sibling precomputed
resources on GCS are not deposited but worth knowing about:

- **Tissue mesh** — segid 1 in
  `precomputed://gs://lee-lab_brain-and-nerve-cord-fly-connectome/microCT/meshes`.
  A coarse mesh of the BANC fly's tissue boundary derived from the
  microCT volume; useful as a whole-body scaffold to drop alongside
  EM-space content in a viewer.
- **Pre-configured Neuroglancer state** that loads both the microCT
  image and the tissue mesh in a single click:
  https://spelunker.cave-explorer.org/#!middleauth+https://global.daf-apis.com/nglstate/api/v1/5950055028621312

## Related files

- `banc_neuron_meshes.zip` — CNS-side morphology that the microCT
  helps to place in the body.
- `banc_neuropil_meshes.zip` — neuropil region outlines.
- `behavior.zip` — pre-EM behavioral recordings of the same fly
  (microCT and behavior are companion characterizations of the BANC
  individual).
- The paper Methods section "Specimen" describes the acquisition.

## Notes

- The microCT volume is in **its own coordinate space**, not in BANC
  EM space. Registration between the two has not been published as
  part of this dataset and is not deposited here.
- The microCT scan is from the same individual fly as the EM volume,
  which is the relevant point of comparison; do not expect
  representative-fly anatomy.
- Resolution is on the order of ~1.3 µm per voxel at the finest level
  — far coarser than EM (4 × 4 × 45 nm), but appropriate for
  whole-body context.
