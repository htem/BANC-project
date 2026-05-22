---
filename: bancpipeline_archive.zip
upstream_url: https://github.com/htem/bancpipeline
default_branch: main
pinned_commit: 2da50aa
pinned_date: 2026-05-22
release_tag: (none — pinned to main HEAD)
license: GPL-3.0
language: R + shell (SLURM)
content_type: application/zip

# --- Fields posted to Dataverse ---
description: >-
  Snapshot of bancpipeline, the R + SLURM data-processing pipeline that produces every compiled-data product in this Dataverse. The pipeline runs on Harvard Medical School's O2 cluster and is the canonical reproducibility resource for the BANC paper: starting from the raw CAVE annotation tables, the BANC SeaTable curation store, the Zetta.ai synapse detections and the Drugowitsch lab's synapse-level neurotransmitter predictions, it assembles banc_888_meta.feather, the per-neuron metrics feather, the v2 / v3 enriched synapse parquets, the simple and split edgelists, the NBLAST cross-dataset match feathers (FAFB, MANC, Hemibrain, FANC, maleCNS, native, mirror), the per-neuron neurotransmitter prediction CSV, and the influence shards and aggregations. Top-level o2_banc_*.sh scripts orchestrate the SLURM jobs and the R sources live under banc/. The pipeline depends on bancr for data access, on influencer + the Drugowitsch lab's ConnectomeInfluenceCalculator for the influence step, on the natverse stack for cross-dataset registration and NBLAST, and on Andrew Champion's Elastix registrations for BANC ↔ JRC2018F / JRC2018VNCF transforms. Developed in Rachel Wilson's lab at Harvard Medical School. Distributed here as a ZIP pinned to the main-branch HEAD at upload date; the live repository continues to evolve at the GitHub URL above.
categories:
  - Code
directoryLabel: code
restrict: false
tabIngest: false
---

# bancpipeline_archive.zip

## Purpose

bancpipeline is the canonical reproducibility resource for the BANC
paper's compiled-data layer. Every `banc_888_*.feather`,
`banc_888_*.parquet`, NBLAST feather, neurotransmitter-prediction
CSV and influence shard deposited in this Dataverse is produced by
one of the R scripts in this repository, dispatched onto the HMS O2
SLURM cluster by the matching `o2_banc_*.sh` driver. Users who want
to inspect or rerun the exact derivation of any of those files —
including the per-step filters, joins and column-precedence rules —
should consult this archive.

## Provenance

Developed in Rachel Wilson's lab at Harvard Medical School by
Alexander Bates and collaborators. The pipeline depends on bancr
for CAVE / SeaTable / GCS access, on influencer (and
ConnectomeInfluenceCalculator) for the influence step, on the
natverse stack (`nat`, `nat.flybrains`, `nat.jrcbrains`,
`hemibrainr`) for cross-dataset registration and NBLAST, and on
Andrew Champion's Elastix registration outputs for transforms
between BANC voxel space and JRC2018F / JRC2018VNCF template
spaces.

## Repository contents

- `banc/` — the main R source as a flat directory of `banc-*.R` scripts. Headline files include `banc-data.R` (assembles the per-version metadata feather, metrics feather, synapse parquets and edgelists), the per-feature builders (`banc-l2.R`, `banc-regions.R`, `banc-roots.R`, `banc-ntpred.R`, etc.), the NBLAST scripts (`banc-{fafb,manc,malecns,hemibrain,fanc}-nblast.R` plus `banc-nblast.R` / `banc-nblast-lr.R`) and the influence driver (`banc-influence.R`). The companion `bancpipeline_schema.md` reference (in this Dataverse) documents which script writes which compiled-data file.
- `o2_banc*.sh` — SLURM driver scripts (one per major workload: meta build, FAFB / MANC / Hemibrain / FANC / maleCNS NBLAST, mesh / skeleton transforms, neuroglancer-state upload, synapse enrichment, ID updates, splitting).
- `analysis/`, `figures/`, `annotations/`, `inst/` — auxiliary scripts, curation surfaces and embedded reference images.
- `setup/` — environment bootstrap (R packages, conda environments) plus imported meshes.
- `data/`, `deform/`, `malecns/`, `manc/` — auxiliary inputs and per-dataset helper code.
- `LICENSE`, `README.md`, `bancpipeline.Rproj` — package metadata.

## Usage

```bash
# On HMS O2 (requires HMS affiliation and CAVE credentials):
sbatch o2_banc.sh                # full meta + metrics + edgelist build
sbatch o2_banc_nblast.sh         # NBLAST cross-dataset matching
sbatch o2_banc_synapses.sh       # synapse-level enrichment
```

Most users will not run bancpipeline directly — instead, install bancr (`remotes::install_github("natverse/bancr")`) and consume the pre-built outputs deposited here. The pipeline is most useful as documentation of provenance for those outputs, and as a starting point for users building extensions (e.g. a new cross-dataset match).

## Related files

- Every `banc_888_*` feather and parquet in this Dataverse is produced by this pipeline.
- `bancr_archive.zip` — the user-facing R client built on top of the pipeline's outputs.
- `influencer_archive.zip`, `connectome_influence_calculator_archive.zip` — the influence engine called by `banc/banc-influence.R`.
- `bancpipeline_schema.md` (alongside this file in the Dataverse documentation set) — full digest of which script produces which compiled-data file and with what column-level provenance.

## Notes

- This Dataverse copy is a point-in-time snapshot pinned to commit `56659bb` on the `main` branch (1 May 2026). For citation, prefer the upstream GitHub URL plus a commit SHA over the Dataverse archive.
- The upstream repository is **private** at the `wilson-lab` organisation, primarily because the pipeline embeds paths, credentials and SLURM configurations specific to the HMS O2 cluster. This Dataverse deposit makes the snapshot publicly available for reproducibility of the BANC paper.
- Licensed under GPL-3.0 (the badge in the upstream README that reads "MIT" is stale; the `LICENSE` file in the repository declares GPL-3.0). Redistribution and derivative works are permitted under those terms.
- In the pinned commit, the `banc/` source is a flat directory of `banc-*.R` scripts. The live repository (post-pin) reorganises these into thematic subdirectories (`banc/meta/`, `banc/metrics/`, `banc/nblast/`, `banc/influence/`, `banc/matching/`, `banc/update/`, `banc/clustering/`), which is the layout referenced in `bancpipeline_schema.md`. The script names and responsibilities are unchanged.
- Running the pipeline as-is requires HMS O2 access, CAVE credentials and write access to the BANC SeaTable; for users outside that environment the pipeline is best read as documentation rather than executed verbatim.
