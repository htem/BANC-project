---
filename: connectome_influence_calculator_archive.zip
upstream_url: https://github.com/DrugowitschLab/ConnectomeInfluenceCalculator
default_branch: main
pinned_commit: 6b70294
pinned_date: 2026-05-13
release_tag: v0.4
license: BSD-3-Clause
language: Python
content_type: application/zip

# --- Fields posted to Dataverse ---
description: >-
  Snapshot of the Drugowitsch lab's ConnectomeInfluenceCalculator, the Python implementation of the linear-dynamical-systems influence model used to compute every adjusted-influence number in the BANC paper. Given a connectome stored as a SQLite file, the package builds a sparse rescaled connectivity matrix and solves the steady-state response equation r∞ = -(W~ - I)^-1 s for each seed pattern s. Implementation uses PETSc and SLEPc through the petsc4py / slepc4py wrappers, making it tractable on large connectomes (hundreds of thousands of neurons, millions of edges). It optionally supports signed connectivity (negative weights for inhibitory presynapses), per-call silencing of specified neurons, and an adjustable minimum post-synaptic count threshold (default 5). For BANC specifically, the package is the underlying engine called by the R-side influencer package (deposited separately) and by bancpipeline, which together produce the sharded all-to-all influence parquet at compiled_data/banc_888/influence/all_to_all/ as well as the sensory- and effector-subclass aggregations. Released under BSD-3-Clause; latest tagged release at upload date is v0.4 (24 November 2025). Distributed here as a ZIP pinned to v0.4; the live package continues to evolve at the GitHub URL above.
categories:
  - Code
directoryLabel: code
restrict: false
tabIngest: false
---

# connectome_influence_calculator_archive.zip

## Purpose

This is the Python implementation of the influence calculation that
underlies the BANC paper's adjusted-influence scores. "Influence"
here is the steady-state response of each target neuron to a sustained
signal injected at a seed, computed as the closed-form solution to a
linear dynamical system over a rescaled connectivity matrix. The
package is designed for the large-sparse-matrix regime — hundreds of
thousands of neurons, millions of edges — using PETSc and SLEPc as
its numerical backbone.

## Provenance

Authored by Zaki Ajabi and Jan Drugowitsch (Drugowitsch lab, Harvard
Medical School). The mathematical model is the steady-state solution
r∞ = -(W~ - I)^-1 s, where W~ is the rescaled synaptic weight matrix
and s is the seed input pattern. The package is the engine called by
both the R-side influencer package and bancpipeline's influence
sharding scripts.

## Repository contents

- `InfluenceCalculator/` — Python package source. The `InfluenceCalculator` class (in `InfluenceCalculator.py`) handles SQLite ingestion, matrix assembly, PETSc / SLEPc invocation, eigenvalue caching, and provides the per-seed `calculate_influence()` entry point.
- `tests/` — pytest coverage of the core solver, a Jupyter example notebook (`Influence_test_example.ipynb`) and a toy SQLite connectome (`toy_network_example.sqlite`).
- `pyproject.toml` — pip-installable build manifest; conda installation of PETSc / SLEPc and their wrappers is also documented in the README.
- `LICENSE.txt`, `README.md`, `CONTRIBUTING.md`, `.zenodo.json` — project metadata, Zenodo DOI configuration and contribution guidelines.

## Usage

```python
from InfluenceCalculator import InfluenceCalculator
ic = InfluenceCalculator('BANC_dataset.sqlite', signed=True, count_thresh=5)
seed_ids = ic.meta[ic.meta['seed_01'] == 'olfactory'].root_id
influence_df = ic.calculate_influence(seed_ids)
```

For the BANC paper the package is most often reached indirectly, through influencer's `influence_calculator_py()` (R) or through bancpipeline's `banc/banc-influence.R` SLURM job template.

## Related files

- `influencer_archive.zip` — R wrapper that calls this package via reticulate.
- `bancpipeline_archive.zip` — `banc/banc-influence.R` is the BANC-specific driver that produces every influence parquet deposited here.
- `influence_all_to_effector_subclass.parquet`, `influence_sensory_subclass_to_all.parquet`, `influence/all_to_all/chunk_*.parquet` — the influence outputs in this Dataverse.

## Notes

- This Dataverse copy is a point-in-time snapshot pinned to commit `6b70294` on the `main` branch, corresponding to release tag `v0.4` (24 November 2025). For citation, the upstream GitHub URL plus a commit SHA or release tag is preferred; the repository also carries a Zenodo DOI badge for academic citation.
- Licensed under BSD-3-Clause; permissive redistribution.
- A working PETSc + SLEPc installation is required; the README recommends Python 3.13.x and provides conda-forge installation recipes. Installation can be involved on macOS — most BANC users reach the calculator through influencer's pre-built conda environment instead.
