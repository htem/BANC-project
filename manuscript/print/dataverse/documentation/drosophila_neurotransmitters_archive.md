---
filename: drosophila_neurotransmitters_archive.zip
upstream_url: https://github.com/funkelab/drosophila_neurotransmitters
default_branch: main
pinned_commit: 513f7d9
pinned_date: 2026-05-14
release_tag: (none — pinned to main HEAD)
license: CC BY 4.0
language: CSV + R
content_type: application/zip

# --- Fields posted to Dataverse ---
description: >-
  Snapshot of funkelab/drosophila_neurotransmitters, a curated literature database of fast-acting small-molecule transmitter usage in Drosophila melanogaster organised by connectomic cell type. The repository ships a version-controlled gt_data.csv where each row pairs a cell type with a study reporting transmitter expression, an evidence flag (immunohistochemistry, RNAi, MCFO, scRNA-seq, etc.) and a confidence score, together with per-transmitter columns for acetylcholine, glutamate, GABA, glycine, dopamine, serotonin, octopamine, tyramine, histamine and nitric oxide (-1 / 0 / +1 for negative / no / positive evidence). A cross-dataset cell-type mapping at inst/extdata/cell_type_cross_matching.csv links the cell-type names back to FAFB-FlyWire, MANC, Hemibrain, FANC, optic-lobe, maleCNS, L1 and BANC identifiers. This is the source dataset behind the neurotransmitter_verified column in banc_888_meta.feather, and the literature-verified benchmark against which the Eckstein et al. 2024 CNN transmitter classifier is evaluated. Curated by Alexander Bates with Diane Adjavon and Jan Funke. Distributed as a ZIP pinned to the main-branch HEAD at upload date; the repository continues to evolve at the GitHub URL above.
categories:
  - Code
directoryLabel: code
restrict: false
tabIngest: false
---

# drosophila_neurotransmitters_archive.zip

## Purpose

This is the literature-derived ground-truth table from which the
BANC `neurotransmitter_verified` column is populated. For each
connectomic cell type the table records every study that has
identified the small-molecule transmitter it uses, the evidence
method, and a curator confidence score, so that downstream users
can filter on strength of evidence rather than taking a single
label at face value. The complementary
`drosophila_neuropeptides` repository handles slower peptide
signalling on the same schema.

## Provenance

Collated by Alexander Bates while in Rachel Wilson's lab at Harvard
Medical School, and curated together with Diane Adjavon in Jan
Funke's lab at Janelia Research Campus. The cell-type
cross-mapping is maintained in tandem with the equivalent file in
`drosophila_neuropeptides`. The Eckstein et al. 2024 CNN classifier
provides predicted transmitters; this repository captures the
literature-verified labels against which those predictions are
benchmarked.

## Repository contents

- `gt_data.csv` — primary data table: one row per cell type × study, with transmitter columns (-1 / 0 / 1) and confidence + evidence metadata.
- `gt_sources/` — source tables from individual studies that the curators ingested into `gt_data.csv`.
- `inst/extdata/cell_type_cross_matching.csv` — maps cell-type names across the connectome datasets (FAFB-FlyWire, MANC, Hemibrain, FANC, optic-lobe, maleCNS, L1 and BANC).
- `R/` — R scripts that normalise the CSV, validate the schema and push the result into `franken_meta` (the cross-dataset metadata used in BANC pipelines).
- `validate.py`, `requirements.txt` — Python validation entry point with its dependency manifest.
- `settings/`, `drosophila_transitters.Rproj` — project-local settings and R project file.
- `CITATIONS.md`, `CONTRIBUTING.md`, `README.md`, `LICENSE` — bibliography of contributing studies, contribution guidelines, README and license.

## Usage

```r
nt <- readr::read_csv("gt_data.csv")
ct_map <- readr::read_csv("inst/extdata/cell_type_cross_matching.csv")
nt %>% filter(cell_type == "DNa01") %>%
  select(cell_type, acetylcholine, gaba, glutamate, neurotransmitter_verified_source)
```

For BANC specifically, the resolved per-neuron labels are already
joined into `banc_888_meta.feather` under
`neurotransmitter_verified`. The raw CSV is most useful when you
want the evidence trail behind each label or want to filter on
confidence score.

## Related files

- `banc_888_meta.feather` — `neurotransmitter_verified` column is
  populated from this repository's CSV.
- `drosophila_neuropeptides_archive.zip` — companion repository
  documenting neuropeptides; same curator team, same schema.
- `banc_888_neurotransmitter_prediction_v2.csv` — Eckstein et al.
  2024 CNN-predicted transmitters; this repository's labels are the
  literature-verified benchmark for that classifier.
- `bancpipeline_archive.zip` — `banc/banc-ntpred.R` and `banc/franken-annotations-fix.R` read this repository's CSV.

## Notes

- This Dataverse copy is a point-in-time snapshot pinned to commit `513f7d9` on the `main` branch (14 May 2026), which adds the CC BY 4.0 LICENSE file. For citation, prefer the upstream GitHub URL plus a commit SHA over the Dataverse archive.
- The upstream repository is public at the `funkelab` organisation; the Dataverse deposit redistributes it under the CC BY 4.0 license. Curator team: Alexander Bates, Diane Adjavon, Jan Funke.
- The repository was launched in June 2024 with ground-truth entries spanning a broad survey of *D. melanogaster* central nervous system cell types from the published literature; coverage grows as new studies are added.
