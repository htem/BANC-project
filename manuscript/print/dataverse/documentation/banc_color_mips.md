---
filename: neuron_colormips_v888.zip
gcs_path: gs://lee-lab_brain-and-nerve-cord-fly-connectome/neuron_colormips_v888.zip
unzipped_size_bytes: 654332805
unzipped_size_human: 624.0 MiB
content_type: application/zip

# --- Fields posted to Dataverse ---
description: >-
  Color-depth maximum-intensity projection (MIP) images of BANC
  proofread neurons at v888, rendered into the JRC2018 unisex brain
  template (`JRC2018_Unisex_20x_HR`) and the JRC2018 unisex VNC
  template (`JRC2018_VNC_Unisex_40x_DS`). Color encodes depth along
  the optical axis; brightness encodes the neuron's mask. The format
  follows the standard NeuronBridge color-MIP convention used to
  search for matching driver lines via light-microscopy databases.
  Distributed as a ZIP of PNG files named by v888 root ID. These are
  the images used to populate the BANC entries on the NeuronBridge
  service (neuronbridge.janelia.org), which lets users link a
  connectome neuron to candidate split-GAL4 driver lines for in-vivo
  experiments.
categories:
  - Data
  - Images
directoryLabel: color_mips
restrict: false
tabIngest: false
---

# banc_color_mips.zip

## Purpose

Color-depth MIPs (CDMs) provide a compact 2D representation of a
neuron's 3D morphology that can be searched against large
light-microscopy libraries to identify driver lines that label the same
neuron. The encoding gives each pixel a color that encodes the depth
at which the neuron's mask is brightest, so two neurons with similar
3D morphology produce similar CDM images.

The set bundled here is the BANC contribution to the NeuronBridge
service — proofread BANC neurons at v888 rendered into the JRC2018
unisex brain and VNC templates, ready for search against the
FlyLight, FlyEM-Hemibrain, MANC and FAFB CDM corpora hosted at
NeuronBridge.

## Provenance

Generated using the BANC python package
(https://pypi.org/project/banc/), with an R equivalent at
https://natverse.org/neuronbridger/. Reconstructions were transformed
into the JRC2018 unisex brain (`JRC2018_Unisex_20x_HR`, 1210 × 566 px)
and / or the JRC2018 unisex VNC (`JRC2018_VNC_Unisex_40x_DS`,
573 × 1119 px) using the brain and VNC registrations deposited here,
then rendered into CDM PNGs in the NeuronBridge convention. See the
paper Methods section "Color MIPs".

## Contents

Not tabular. The ZIP unpacks to two sibling directories, one per
template space:

- `JRC2018_UNISEX_20x_HR/<root_id>_in_JRC2018_UNISEX_20x_HR.png` —
  brain-side CDMs.
- `JRC2018_VNC_UNISEX_461/<root_id>_in_JRC2018_VNC_UNISEX_461.png` —
  VNC-side CDMs.

Each PNG is the standard NeuronBridge CDM encoding (RGBA, depth-coded
hue, mask-coded brightness). Brain-side coverage is broader than
VNC-side because more neurons project into the brain template.

## Usage

Most users will not handle the files directly. The recommended access
path is **NeuronBridge**:

```
https://neuronbridge.janelia.org/search?ds=banc
```

For programmatic search against the NeuronBridge corpus, see Janelia's
`neuronbridge-python` package.

For local processing:

```python
from PIL import Image
import numpy as np
cdm = np.asarray(Image.open(
    "JRC2018_UNISEX_20x_HR/"
    "720575941521131930_in_JRC2018_UNISEX_20x_HR.png"
))
# cdm is HxWx4 (RGBA); channels carry depth and mask
```

## Related files

- `banc_888_meta.feather` — look up the neuron's identity by root ID.
- `registration_brain_jrc2018f.zip`, `registration_vnc_jrc2018vncf.zip`
  — used in the bridging from BANC to JRC2018 space.
- `banc_neuron_meshes.zip` — source 3D morphology that the CDMs are
  rendered from.

## Notes

- CDMs are in the JRC2018 unisex brain and VNC template spaces
  (`JRC2018_Unisex_20x_HR` and `JRC2018_VNC_Unisex_40x_DS`), so they
  can be compared directly against FlyLight light-microscopy CDMs in
  NeuronBridge.
- The set covers proofread BANC neurons; expect CDMs of lower visual
  quality for partially-proofread or truncated neurons.
- New BANC materializations may produce updated CDMs; this archive is
  pinned to v888.
