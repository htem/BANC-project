---
filename: fly_connectome_data_tutorial_archive.zip
upstream_url: https://github.com/sjcabs/fly_connectome_data_tutorial
default_branch: main
pinned_commit: '5416119'
pinned_date: '2026-05-22'
release_tag: (none — pinned to main HEAD)
license: MIT
language: R + Python
content_type: application/zip
description: '**Note on citation:** this code repository does not yet have a minted Zenodo DOI. When one is minted (via a tagged GitHub Release with Zenodo integration enabled), it will become the canonical citable resource; this Dataverse copy is a byte-for-byte snapshot of the main-branch HEAD on the upload date. Snapshot of fly_connectome_data_tutorial, the R + Python tutorial materials prepared for the San Juan Winter School on Connectomics and Brain Simulation (SJCABS). The tutorial walks new users through every major Drosophila connectome dataset — BANC, FAFB-FlyWire, MANC, Hemibrain and maleCNS — at a uniform level, covering data access and metadata exploration, neuron morphology and NBLAST, connectivity and neurotransmitter prediction, and indirect connectivity via influence scores. All datasets in the tutorial are harmonised to the unified BANC metadata schema so cross-dataset comparisons work the same way in every lesson. Lessons are delivered as matched R notebook + Python notebook pairs that produce comparable outputs. Authored by Sven Dorkenwald and Alexander Bates with contributions from Philipp Schlegel and Greg Jefferis. For users in this Dataverse the most load-bearing artefact is data/dataset_documentation/banc_data.md — a third-party, user-facing column dictionary digest of the BANC tables. Distributed here as a ZIP pinned to the main-branch HEAD at upload date; the tutorial continues to evolve at the GitHub URL above.'
categories:
- Code
directoryLabel: code
restrict: false
tabIngest: false
---
# fly_connectome_data_tutorial_archive.zip

## Purpose

This tutorial is the recommended starting point for users who are new to the *Drosophila* connectome dataset family — including BANC. It teaches the same workflows in R and Python, with matched notebooks for each lesson, so groups with mixed-language analysis stacks can adopt it as a shared on-ramp. The companion column dictionary at `data/dataset_documentation/banc_data.md` is the most accessible third-party documentation of the BANC tables.

## Provenance

Instructors Sven Dorkenwald (Princeton, FAFB-FlyWire) and Alexander Bates (Harvard, BANC). Contributions from Philipp Schlegel (MRC LMB, navis / fafbseg-py) and Greg Jefferis (MRC LMB, natverse). BANC metadata in the tutorial is sourced from `bancr::banc_meta()` and is kept in sync with the v888 snapshot deposited in this Dataverse.

## Repository contents

- `R/` — R notebooks for each lesson, plus setup scripts that wrap installation of the natverse / bancr / fafbseg / neuprintr stacks.
- `python/` — Python (Jupyter) notebooks mirroring each lesson, built on navis, fafbseg-py, CAVEclient and neuprint-python.
- `data/` — small metadata files used in the lessons, plus the `data/dataset_documentation/` per-dataset Markdown column dictionaries. `banc_data.md` is the entry of interest for users of this Dataverse: a tabular, user-facing description of every BANC metadata, edgelist, synapse and influence column.
- `inst/` — images and supplementary materials (banner, per-dataset thumbnails) for the lessons.
- `LICENSE`, `README.md` — license and the SJCABS-facing landing page with dataset links and citation guidance.

## Usage

The repository ships installable R and Python environments. The
shortest path to a working setup is:

```r
source("R/00_setup.R")   # installs natverse + dataset packages
```

```bash
pip install -r python/requirements.txt
jupyter lab python/
```

Then work through the lessons in numerical order. Each lesson is
~30 minutes; the full series is ~2 hours.

## Related files

- `banc_888_meta.feather`, the `banc_888_edgelist_*` and synapse
  files in this Dataverse — the BANC data the tutorial reads.
- `bancr_archive.zip` — the R package the tutorial uses for BANC.
- `the_banc_fly_connectome_archive.zip` — the Python package the
  tutorial uses for BANC.

## Notes

- This Dataverse copy is a point-in-time snapshot pinned to commit `45c761f` on the `main` branch (29 April 2026). For citation, prefer the GitHub URL plus a commit SHA over the Dataverse archive.
- Licensed under MIT for the tutorial code in this repository. The third-party connectome datasets the notebooks reference (FAFB-FlyWire, Hemibrain, MANC, maleCNS, BANC) are licensed by their respective creators; consult each dataset's own deposit for terms.
- The tutorial pulls live data from CAVE and from this Dataverse; rerunning every cell requires network access. For an offline reproduction, point each loader at the local copies of the BANC files in this deposit.
