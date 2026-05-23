---
filename: the_banc_fly_connectome_archive.zip
upstream_url: https://github.com/jasper-tms/the-BANC-fly-connectome
default_branch: main
pinned_commit: 01d797a
pinned_date: '2026-05-22'
release_tag: v0.6.1
license: GPL-3.0
language: Python
content_type: application/zip
description: '**Note on citation:** this code repository does not yet have a minted Zenodo DOI. When one is minted (via a tagged GitHub Release with Zenodo integration enabled), it will become the canonical citable resource; this Dataverse copy is a byte-for-byte snapshot of the main-branch HEAD on the upload date. Snapshot of the-BANC-fly-connectome, Jasper Phelps''s Python toolkit for accessing and working with the BANC (Brain And Nerve Cord) connectome of an adult female Drosophila melanogaster. The repository''s `banc` package bundles client helpers for the BANC CAVE deployment (segmentation, annotation tables, chunked-graph proofreading), routines for mesh download and coordinate transformation, an Elastix-based interface for aligning neurons to JRC2018F / JRC2018VNCF template space, color-MIP utilities, and example notebooks demonstrating common BANC workflows. The repository also hosts the canonical Neuroglancer state files for the BANC paper at neuroglancer_states/ and the bundled volume meshes under data/volume_meshes/. The `banc/` package directory is a symlink that re-exposes the `fanc/` source tree (forked from FANC tooling) adapted for BANC. Developed by Jasper Phelps in Wei-Chung Allen Lee''s lab at Harvard Medical School and released under GPL-3.0; v0.6.1 is the latest tagged release at upload time. This is the Python counterpart to bancr; the two libraries are independent but complementary, and many users will install both. Distributed here as a ZIP pinned to v0.6.1; the live package continues to evolve at the GitHub URL above.'
categories:
- Code
directoryLabel: code
restrict: false
tabIngest: false
---
# the_banc_fly_connectome_archive.zip

## Purpose

`banc` (the Python module installed from this repository) is Jasper
Phelps' main BANC analysis toolkit. It provides the Python side of
the work that bancr handles in R: connecting to the BANC CAVE
deployment, pulling segmentation and annotation data, downloading
meshes, transforming between BANC voxel space and the shared
JRC2018F / JRC2018VNCF template spaces, and producing Neuroglancer
states. Users building Python pipelines on top of BANC — or anyone
who wants the canonical example notebooks — should install this
package.

## Provenance

Authored and maintained by Jasper Phelps (Lee Lab, Harvard Medical School). The repository forked from FANC tooling (`https://github.com/htem/FANC_auto_recon`) and was reshaped around BANC; the `banc` entry at the top of the tree is a symlink into the `fanc/` source directory whose files have been adapted for the BANC.

## Repository contents

- `banc` (symlink) → `fanc/` — core Python package source: CAVE clients, mesh utilities, Elastix transform interfaces, Neuroglancer-state helpers.
- `example_notebooks/` — Jupyter notebooks demonstrating common workflows.
- `neuroglancer_states/` — canonical Neuroglancer state files for the BANC paper; pinned to the v888 segmentation and also deposited separately in this Dataverse.
- `data/volume_meshes/` — bundled neuropil and region meshes (including the JRC2018_VNC_FEMALE volume set) used by the Python plotting paths.
- `colormips/` — color-MIP helper code.
- `slackbots/`, `tests/`, `.github/` — Slack integration, test suite and CI configuration.
- `pyproject.toml`, `README.md`, `LICENSE` — packaging and project metadata.

## Usage

```bash
pip install git+https://github.com/jasper-tms/the-BANC-fly-connectome.git
```

```python
import banc
client = banc.connect()                 # CAVE client
nrn = banc.skeletons.load(720575941521131930)
banc.coordinate_transforms.banc_to_template(...)
```

The example notebooks under `example_notebooks/` walk through the
canonical workflows; start there.

## Related files

- `bancr_archive.zip` — the R-side counterpart with overlapping functionality (metadata, partners, NBLAST, meshes).
- `neuroglancer_states_2026a.zip` — the same Neuroglancer states shipped under `neuroglancer_states/` in this repo, deposited separately for convenience.
- `banc_888_meta.feather`, `banc_neuron_meshes.zip` — the metadata and meshes the package operates on.

## Notes

- This Dataverse copy is a point-in-time snapshot pinned to commit `fa3104b` on the `main` branch, corresponding to release tag `v0.6.1` (8 May 2026). For citation, prefer the upstream GitHub URL plus a commit SHA over the Dataverse archive.
- Licensed under GPL-3.0; redistribution and derivative works are permitted under those terms.
- Live segmentation queries and proofreading operations require CAVE credentials and access to the BANC community (see the upstream README's "Provide credentials" section); the example notebooks are not exercised end-to-end without these.
- Optional Elastix integration depends on `pytransformix` and a working Elastix install; HDF5 is a system dependency for the bundled mesh / table I/O (Mac users typically install via Homebrew with `HDF5_DIR=/opt/homebrew/opt/hdf5`).
