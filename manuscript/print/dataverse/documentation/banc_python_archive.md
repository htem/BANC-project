---
filename: banc-0.6.1.tar.gz
upstream_url: https://pypi.org/project/banc/
source_repo: https://github.com/jasper-tms/the-BANC-fly-connectome
default_branch: main
pinned_commit: (set at deposit time — matches PyPI release sdist)
pinned_date: (set at deposit time)
release_tag: v0.6.1
license: GPL-3.0
language: Python
content_type: application/x-gzip

# --- Fields posted to Dataverse ---
description: >-
  Source distribution (sdist) of the BANC Python client package, as
  published to PyPI at https://pypi.org/project/banc/. Provides the
  pip-installable Python interface to the BANC connectome — CAVE client
  helpers, mesh download, coordinate transforms between BANC voxel space
  and the JRC2018F / JRC2018VNCF template spaces, color-MIP utilities,
  and Neuroglancer-state generation. Authored by Jasper Phelps (Lee Lab,
  Harvard Medical School). This is the lean PyPI sdist — installable
  with `pip install banc` — and is a strict subset of the full
  `the_banc_fly_connectome_archive.zip` (which additionally contains the
  Neuroglancer state files, bundled volume meshes, example notebooks,
  Slackbots and tests). Released under GPL-3.0. Distributed here as a
  point-in-time tarball pinned to the PyPI release tagged v0.6.1; the
  live package continues to evolve and new releases land at the PyPI
  URL above.
categories:
  - Code
directoryLabel: code
restrict: false
tabIngest: false
---

# banc-0.6.1.tar.gz

## Purpose

`banc` is the PyPI-published Python client for the BANC connectome,
authored by Jasper Phelps. It is the canonical lightweight install path
for users who want the BANC Python tooling but don't need the full
GitHub working tree (notebooks, neuroglancer states, bundled meshes,
slackbots, CI). The pip-installable distribution provides:

- CAVE client helpers for the BANC deployment (segmentation, annotation
  tables, chunked-graph proofreading queries).
- Mesh download + L2-skeleton routines.
- Coordinate-transform code for BANC voxel space ↔ JRC2018F /
  JRC2018VNCF template spaces (Elastix backend).
- Color-MIP utilities.
- Neuroglancer-state generation helpers.

This is the Python counterpart to bancr (R). The two libraries are
independent but complementary; many users will install both.

## Provenance

Maintained by Jasper Phelps in Wei-Chung Allen Lee's lab at Harvard
Medical School. Built from the source tree at
https://github.com/jasper-tms/the-BANC-fly-connectome, which itself
forked from FANC tooling (`https://github.com/htem/FANC_auto_recon`)
and was reshaped around BANC. The top-level `banc/` directory in that
repo is a symlink onto the `fanc/` source whose modules have been
adapted for BANC.

Released to PyPI as `banc` starting with v0.5.x; the deposit here
corresponds to v0.6.1 (the latest tagged release at upload time).
Subsequent releases will appear on PyPI under the same name.

## Distribution contents

This is a Python source distribution (sdist) — a single
`banc-0.6.1.tar.gz` containing the `banc/` package, `pyproject.toml`,
README and LICENSE. It is **not** the full GitHub repo; see
`the_banc_fly_connectome_archive.zip` for the complete working tree
(notebooks, neuroglancer states, bundled meshes, tests).

Typical install + run:

```bash
pip install banc                            # from PyPI (preferred for new work)
# or, from this Dataverse copy:
pip install banc-0.6.1.tar.gz
```

```python
import banc
client = banc.connect()                     # CAVE client
nrn = banc.skeletons.load(720575941521131930)
banc.coordinate_transforms.banc_to_template(...)
```

## Related files

- `the_banc_fly_connectome_archive.zip` — the **full** GitHub repo this
  package is built from. Includes Neuroglancer states, bundled meshes,
  example notebooks, Slackbots, tests — everything the lean PyPI sdist
  intentionally omits. Use this if you need the example notebooks or
  the paper's Neuroglancer states; use the PyPI tarball if you only
  want the installable Python library.
- `bancr_archive.zip` — the R-side counterpart with overlapping
  functionality (metadata, partners, NBLAST, meshes).
- `neuroglancer_states_2026a.zip` — the canonical Neuroglancer state
  files used in the BANC paper figures; consumed by the
  `banc.neuroglancer` helpers in this package.

## Notes

- **Citation**: prefer the PyPI URL + version tag
  (`pip install banc==0.6.1`) for new work. The Dataverse archive here
  is a stable static snapshot for the BANC paper deposit.
- Licensed under GPL-3.0; redistribution and derivative works are
  permitted under those terms.
- Live CAVE queries (segmentation lookups, proofreading operations)
  require BANC community credentials — see the upstream README's
  "Provide credentials" section.
- Elastix-based coordinate transforms depend on `pytransformix` and a
  working Elastix install; HDF5 is a system dependency for the bundled
  mesh / table I/O. macOS users typically install via Homebrew with
  `HDF5_DIR=/opt/homebrew/opt/hdf5`.
- **The PyPI sdist is intentionally a strict subset** of the full repo.
  Anything under `neuroglancer_states/`, `data/volume_meshes/`,
  `example_notebooks/`, `slackbots/`, `tests/` lives only in
  `the_banc_fly_connectome_archive.zip` (and on GitHub).
