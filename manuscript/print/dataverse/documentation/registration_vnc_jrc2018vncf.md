---
filename: registration_vnc_jrc2018vncf.zip
gcs_path: gs://lee-lab_brain-and-nerve-cord-fly-connectome/registrations/vnc_240721/
unzipped_size_bytes: 71237061
unzipped_size_human: 67.9 MiB
content_type: application/zip

# --- Fields posted to Dataverse ---
description: >-
  The BANC ↔ JRC2018F-VNC registration, in elastix format. Maps the
  BANC EM VNC (via the synapse-density "neuropil stain" volume) to
  the JRC2018 Female VNC template and back. Every VNC-side
  cross-dataset operation in the project — bridging MANC and FANC
  neurons into BANC space, rendering BANC VNC neurons into JRC2018F
  VNC for the color-MIP / NeuronBridge pipeline, and aligning VNC
  neuropil parcellations onto BANC — uses this registration.
  Distributed as a ZIP of the elastix parameter chain (manual affine
  + automated affine + coarse and fine B-spline), forward and inverse
  transform files, manually annotated corresponding-point lists, and
  the shell script that drives the registration.
categories:
  - Data
  - Registration
directoryLabel: registrations
restrict: false
tabIngest: false
---

# registration_vnc_jrc2018vncf.zip

## Purpose

The forward (BANC → JRC2018VNCF) and reverse (JRC2018VNCF → BANC) VNC
registrations used throughout BANC for cross-dataset matching,
template-space rendering, and NBLAST. Each registration is an elastix
parameter chain plus the requisite deformation grids.

## Provenance

Computed in July 2024 (date tag `240721`) by the BANC alignment
pipeline (paper Methods, "Neuropils and template alignment"). The
BANC VNC synapse-density volume was registered to JRC2018F VNC using
a manual affine initialization, an elastix automated affine, and two
B-spline deformable stages (coarse then fine), driven by
`register_vnc.sh`. Manually identified corresponding-point lists
supervise the alignment.

## Contents

Not tabular. The ZIP unpacks to:

- `0_manual_affine.txt` — initial hand-tuned affine.
- `1_elastix_affine.txt` — elastix automated affine parameters.
- `2_elastix_Bspline_coarse.txt`,
  `3_elastix_Bspline_fine.txt` — two-stage B-spline deformable
  parameters.
- `BANC_to_template.txt`, `template_to_BANC.txt` — forward and inverse
  composed transforms.
- `corresponding_points_vnc_banc.txt`,
  `corresponding_points_vnc_JRC2018F.txt` — point correspondences
  used during alignment.
- `register_vnc.sh` — driver script wiring the stages together.

## Usage

In `elastix` / `transformix` directly:

```bash
transformix -in input.nrrd -tp template_to_BANC.txt -out output_dir
```

In R via natverse:

```r
library(nat.templatebrains)
# bridge VNC neurons from MANC (JRC2018F VNC) into BANC:
neurons_in_banc <- xform_brain(neurons_MANC,
                               sample = JRC2018F_VNC,
                               reference = BANCVNC)
```

## Related files

- `registration_brain_jrc2018f.zip` — the analogous registration for
  the brain.
- `banc_template_spaces.zip` — the source / target template volumes
  these registrations operate on.
- `banc_neuropil_meshes.zip` — VNC neuropil-region OBJs are registered
  into BANC space using this transform.
- VNC-side NBLAST tables that depend on this registration:
  `banc_manc_v1.2.1_nblast.feather`, `banc_fanc_1116_nblast.feather`,
  `banc_malecns_v0.9_nblast.feather` (VNC portion),
  `banc_mirror_nblast.feather` (VNC portion).
- `banc_color_mips.zip` — VNC neurons are rendered into the JRC2018
  unisex VNC template using this registration for NeuronBridge.

## Notes

- The registration is for the **VNC only**. For brain neurons, pair
  with the brain registration.
- The VNC registration is somewhat looser than the brain because the
  reference template's VNC has less anatomical contrast than the
  brain template; expect sub-neuromere accuracy rather than
  sub-neuropil accuracy.
- Voxel sizes and isotropy are recorded inside the elastix parameter
  files and the source / target volumes (see
  `banc_template_spaces.zip`).
