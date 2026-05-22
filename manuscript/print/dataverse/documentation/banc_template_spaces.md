---
filename: banc_template_spaces.zip
gcs_path: gs://lee-lab_brain-and-nerve-cord-fly-connectome/templates/
unzipped_size_bytes: 848034232
unzipped_size_human: 808.7 MiB
content_type: application/zip

# --- Fields posted to Dataverse ---
description: >-
  Template-space image volumes for BANC: the BANC synapse-density
  "neuropil stain" for brain and VNC, the JRC2018 Female brain and
  JRC2018 Female VNC reference templates, and the BANC synapse-density
  volumes after alignment into JRC2018F space. These templates
  underpin every cross-dataset registration in the project — including
  FAFB to BANC (brain) and MANC to BANC (VNC) bridging used for NBLAST
  matching — and serve as the anatomical scaffold for the
  neuropil-mesh and color-MIP pipelines. Source GCS layers are
  Neuroglancer precomputed multi-resolution image volumes (`*.ng`
  directories); the ZIP packages the same set for offline use.
categories:
  - Data
  - Registration
directoryLabel: registrations
restrict: false
tabIngest: false
---

# banc_template_spaces.zip

## Purpose

Provides the template volumes that anchor every spatial transformation
in the BANC project. Three roles:

1. The **BANC synapse-density** brain and VNC volumes — the predicted
   synapse density blurred and downsampled to the resolution of the
   JRC2018 light templates, acting as a "neuropil stain" derived from
   EM. These are the moving images for BANC → JRC2018 alignment.
2. The **JRC2018F** and **JRC2018F VNC** standard templates — the
   light-microscopy reference volumes the BANC stain is registered
   against. Required to project FAFB-FlyWire and MANC neurons into
   BANC for cell-type matching, and to project BANC neurons out into
   the FlyLight light-microscopy ecosystem.
3. The **BANC synapse-density volumes warped into JRC2018F space** —
   the outputs of the registration, used downstream for the
   NeuronBridge color-MIP pipeline and for cross-checking alignment.

## Provenance

Built as part of the cross-dataset alignment pipeline described in the
paper Methods section "Neuropils and template alignment". BANC-side
volumes derive from a Gaussian-blurred synapse-density map computed
from the BANC v1.1 synapse predictions; JRC2018F / JRC2018F VNC
reference volumes are from Bogovic et al. (2020).

## Contents

Not tabular. The ZIP unpacks to one Neuroglancer precomputed image
directory per template (each `*.ng/` containing an `info` JSON
manifest plus chunked image data at multiple resolution levels). The
set as packaged from GCS includes:

- `JRC2018F_brain.ng/` — JRC2018F whole-female-brain reference.
- `JRC2018F_VNC.ng/` — JRC2018F whole-female-VNC reference.
- `JRC2018_FEMALE.ng/` — combined JRC2018F reference volume.
- `JRC2018F_aligned240721_to_BANC.ng/` — JRC2018F brain warped into
  BANC space.
- `banc-synapses-v1.1-brain_aligned240720_to_JRC2018F_brain.ng/` and
  `banc-synapses-v1.1-brain_aligned240721_to_JRC2018F_brain.ng/` —
  BANC brain synapse-density volumes aligned to JRC2018F (two
  registration runs).
- `banc-synapses-v1.1-VNC_aligned240721_to_JRC2018F_VNC.ng/` — BANC
  VNC synapse-density volume aligned to JRC2018F VNC.

## Usage

For viewing, load any `.ng/` directory into Neuroglancer directly.
For natverse-style bridging, prefer the elastix parameter chains in
`registration_brain_jrc2018f.zip` / `registration_vnc_jrc2018vncf.zip`
— these are the actual transforms; the templates here are the source
and target volumes those transforms operate on.

## Related files

- `registration_brain_jrc2018f.zip` — the elastix registration that
  warps between JRC2018F and BANC brain space.
- `registration_vnc_jrc2018vncf.zip` — the analogous registration for
  the VNC.
- `banc_neuropil_meshes.zip` — neuropil region surfaces, often
  visualized alongside these templates.
- `banc_color_mips.zip` — NeuronBridge MIPs derived using these
  templates.
- Paper Methods section "Neuropils and template alignment".

## Notes

- Voxel sizes and data types are documented per template in the
  `info` JSON inside each `*.ng/` directory.
- The two `240720` / `240721` BANC-brain warps are sibling runs of
  the same alignment with slightly different parameter selections;
  the `240721` chain is the canonical one used downstream.
- For high-precision registration work, prefer the elastix parameter
  files in the `registrations/` ZIPs — these templates are sources
  and targets for the registrations, not the registrations themselves.
