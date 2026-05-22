---
filename: registration_brain_jrc2018f.zip
gcs_path: gs://lee-lab_brain-and-nerve-cord-fly-connectome/registrations/brain_240721/
unzipped_size_bytes: 19656147
unzipped_size_human: 18.7 MiB
content_type: application/zip

# --- Fields posted to Dataverse ---
description: >-
  The BANC ↔ JRC2018F brain registration, in elastix format. Maps the
  BANC EM brain (via the synapse-density "neuropil stain" volume) to
  the JRC2018F standard female-brain template (Bogovic et al. 2020)
  and back. This is the registration that every brain-side
  cross-dataset operation in BANC uses — bridging FAFB-FlyWire and
  Hemibrain neurons into BANC space, rendering BANC neurons into
  JRC2018F for the color-MIP pipeline and NeuronBridge, and aligning
  template-space neuropil parcellations onto BANC. Distributed as a
  ZIP of the elastix parameter chain (manual affine + automated
  affine + coarse and fine B-spline), forward and inverse transform
  files, manually annotated corresponding-point lists, and the shell
  script that drives the registration.
categories:
  - Data
  - Registration
directoryLabel: registrations
restrict: false
tabIngest: false
---

# registration_brain_jrc2018f.zip

## Purpose

The forward (BANC → JRC2018F) and reverse (JRC2018F → BANC) brain
registrations used throughout BANC for cross-dataset matching,
template-space rendering, and NBLAST. Each registration is a stack of
elastix transformation parameter files plus the requisite affine and
b-spline deformation grids.

## Provenance

Computed in July 2024 (date tag `240721` in the directory name) by the
BANC alignment pipeline (paper Methods, "Neuropils and template
alignment"). The BANC synapse-density volume was registered to
JRC2018F using a manual affine initialization, an elastix automated
affine, and two B-spline deformable stages (coarse then fine), driven
by `register_brain.sh`. Manually identified corresponding-point lists
(BANC and JRC2018F coordinates) supervise the alignment.

## Contents

Not tabular. The ZIP unpacks to:

- `0_manual_affine.txt` — initial hand-tuned affine.
- `1_elastix_affine.txt` — elastix automated affine parameters.
- `2_elastix_Bspline_coarse.txt`,
  `3_elastix_Bspline_fine.txt` — two-stage B-spline deformable
  parameters.
- `BANC_to_template.txt`, `template_to_BANC.txt` — forward and inverse
  composed transforms.
- `corresponding_points_brain_banc.txt`,
  `corresponding_points_brain_JRC2018F.txt` — point correspondences
  used during alignment.
- `register_brain.sh` — driver script wiring the stages together.

## Usage

In `elastix` / `transformix` directly:

```bash
transformix -in input.nrrd -tp template_to_BANC.txt -out output_dir
```

In R via natverse:

```r
library(nat.templatebrains)
# bridge neurons from FAFB (JRC2018F) into BANC space
neurons_in_banc <- xform_brain(neurons_FAFB,
                               sample = JRC2018F,
                               reference = BANC)
```

## Related files

- `registration_vnc_jrc2018vncf.zip` — the analogous registration for
  the VNC.
- `banc_template_spaces.zip` — the source / target template volumes
  these registrations operate on.
- `banc_color_mips.zip` — uses this registration to produce JRC2018U
  CDMs.
- `banc_neuropil_meshes.zip` — neuropil-region OBJs are registered into
  BANC space using this transform.
- Brain-side NBLAST tables that depend on this registration:
  `banc_fafb_783_nblast.feather`, `banc_hemibrain_v1.2.1_nblast.feather`,
  `banc_malecns_v0.9_nblast.feather`, `banc_mirror_nblast.feather` (for
  the brain portion of the mirror NBLAST).

## Notes

- The registration is for the **brain only**. For VNC neurons (or
  brain + VNC bridging across the neck), pair with the VNC
  registration.
- The composed `template_to_BANC.txt` / `BANC_to_template.txt`
  transforms chain affine + two B-spline stages; pure-affine bridging
  is not adequate for sub-neuropil precision.
- The date tag `240721` is preserved for reproducibility — older
  registrations (e.g. for v626 work) used different parameter
  selections; this one is the canonical v888 brain registration.
- Voxel sizes and isotropy are recorded inside the elastix parameter
  files and the source / target volumes (see
  `banc_template_spaces.zip`).
