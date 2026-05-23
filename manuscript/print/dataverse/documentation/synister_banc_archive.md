---
filename: synister_banc_archive.zip
upstream_url: https://github.com/htem/synister_banc
default_branch: main
pinned_commit: 760f207
pinned_date: '2026-05-22'
release_tag: (none — pinned to main HEAD)
license: (no LICENSE file in upstream as of 2025-07-29 — see Notes)
language: Python
content_type: application/zip
description: '**Canonical citation:** the authoritative archive for this software is on Zenodo (DOI https://doi.org/10.5281/zenodo.20350569). Cite the Zenodo DOI rather than this Dataverse copy — the Dataverse archive is a byte-for-byte snapshot pinned to the main-branch HEAD on the upload date, mirrored here for one-stop replication. Snapshot of synister_banc, the repository that holds the BANC neurotransmitter-prediction code, ground-truth labels, and pointers to the trained-model predictions. Based on the methodology of Eckstein et al. (2024) "Neurotransmitter classification from electron microscopy images at synaptic sites in Drosophila", adapted for the BANC v888 materialization. Predicts one of eight fast-acting neurotransmitters (acetylcholine, dopamine, GABA, glutamate, histamine, octopamine, serotonin, tyramine) at each predicted presynaptic site. The training / test ground-truth set and the per-synapse prediction parquet are stored on Google Cloud Storage and pointed to from this repository; the per-neuron rollups (banc_888_neurotransmitter_prediction_v2.csv) are what figure scripts and the published `banc_888_meta.feather` consume. Distributed as a ZIP pinned to the main-branch HEAD at upload date; the live code continues at the GitHub URL above.'
categories:
- Code
directoryLabel: code
restrict: false
tabIngest: false
---
# synister_banc_archive.zip

## Purpose

`synister_banc` is the small repository that holds the BANC
neurotransmitter-prediction model code, ground-truth tables, and pointers
to the deposited prediction files on GCS / CAVE. Predictions cover the
eight fast-acting transmitters used in the BANC paper (acetylcholine,
dopamine, GABA, glutamate, histamine, octopamine, serotonin, tyramine)
and are produced from cropped 3-D EM views centred on each predicted
presynaptic site.

This archive is what users want when they need to reproduce or extend the
prediction step itself, audit the ground-truth assignments, or retrain on
a new EM dataset. For consumers who only want the BANC predictions as a
table, the deposited per-neuron and per-synapse CSV / parquet files
(`banc_888_neurotransmitter_prediction_v2.csv`,
`banc_888_synapses_v2_human_readable.csv.gz`,
`banc_nt_prediction_*_w_sizethresh_*.parquet`) are the recommended entry
point.

## Provenance

Developed by Kevin M. Delgado, Diane-Yayra Adjavon, Alexander S. Bates and
Jingxuan Fan, building on the synapse-prediction CNN methodology of
Eckstein et al. 2024 (FAFB / Hemibrain / MANC neurotransmitter classifier;
preprint at https://www.biorxiv.org/content/10.1101/2020.06.12.148775v2).
Adapted for BANC's v2 synapse predictions, retrained on a BANC-specific
ground-truth subset (3,376 identified cell types from FAFB / MANC /
Hemibrain literature; 58,801 BANC neurons matched into these cell types;
motor neurons excluded because they have few CNS presynapses). See
paper Methods: "Neurotransmitter prediction" for the full training
protocol.

## Repository contents

- `README.md` — pointers to the deposited prediction parquet on GCS
  (`gs://leelab_fly_cns/files/banc_nt_prediction_w_sizethresh_5_09072025.parquet`),
  the CAVE-side simplified view
  (`synapses_250226_nt_prediction_5`), and the ground-truth bucket
  (`gs://leelab_fly_cns/files/banc_nt_ground_truth`).
- Training / inference Python source (3-D CNN ResNet-18 with focal-loss
  training, Adam optimiser, 1.06 M iterations at batch size 16). See
  paper Methods §"Neurotransmitter prediction" for hyperparameters.

## Predictions

Three deposited products downstream of this code:

- **Per-synapse predictions (v2)** — `gs://leelab_fly_cns/files/banc_nt_prediction_w_sizethresh_5_09072025.parquet`.
  Only synapses with detected size > 5 voxels have NT predictions. This
  is the size threshold used for the published paper figures.
- **CAVE simplified view** —
  `https://cave.fanc-fly.com/annotation/views/aligned_volume/brain_and_nerve_cord/table/synapses_250226_nt_prediction_5`.
- **v3 predictions (testing)** — same model architecture retrained on
  the v3 synapse predictions at size ≥ 10. See sibling docs
  `banc_nt_prediction_w_sizethresh_5_11102025.md` and
  `banc_nt_prediction_v3_w_sizethresh_10_05042026.md` for schema and
  caveats.

## Neuron-level rollup

A per-neuron most-likely-NT call is obtained by summing the eight class
probabilities across all of a neuron's predicted presynaptic sites and
selecting the argmax. This rollup is shipped as
`banc_888_neurotransmitter_prediction_v2.csv` in the BANC deposit and is
joined into `banc_888_meta.feather`. The rollup assumes Dale's law even
though some CNS neurons co-transmit; see paper Methods.

## Related files

- `drosophila_neurotransmitters_archive.zip` — fully-cited compilation of
  per-cell-type ground-truth labels (https://github.com/funkelab/drosophila_neurotransmitters)
  used as the BANC training set.
- `drosophila_neuropeptides_archive.zip` — companion repository for the
  neuropeptide-expression ground truth.
- `banc_888_neurotransmitter_prediction_v2.csv` — neuron-level rollup of
  this model's per-synapse predictions; the file the paper figures and
  the meta feather consume.
- `banc_888_synapses_v2_human_readable.csv.gz` — raw v2 synapse table; the
  per-synapse NT prediction parquet keys to its `id` column.
- `banc_nt_prediction_v3_w_sizethresh_10_05042026.md` /
  `banc_nt_prediction_w_sizethresh_5_11102025.md` — sibling Dataverse docs
  describing the deposited prediction parquets in more detail.

## Notes

- **No upstream LICENSE file** as of the snapshot date (2025-07-29). When
  the deposit is finalised, the upstream repo should add an OSI-approved
  open-source licence (GPL-3 to match the rest of the BANC code stack
  would be the natural choice). Until that lands, distribution is
  governed by the BANC paper's Harvard Dataverse terms.
- **Serotonin caveat**: the CNN classifies many peptidergic neurons as
  serotonergic, mirroring the original Eckstein 2024 model. Treat
  serotonin predictions with caution and cross-reference the verified
  neuropeptide annotations in the meta feather.
- **Size threshold ≥ 5 voxels** is what produced the figures in the
  paper. The v3 prediction set uses size ≥ 10, which drops some smaller
  but real v2 detections; see `banc_nt_prediction_v3_w_sizethresh_10_05042026.md`.
- This Dataverse copy is a point-in-time snapshot of the main branch.
  Cite the GitHub URL plus a commit SHA for new analyses.
