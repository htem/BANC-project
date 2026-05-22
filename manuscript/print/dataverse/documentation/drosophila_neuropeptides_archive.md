---
filename: drosophila_neuropeptides_archive.zip
upstream_url: https://github.com/funkelab/drosophila_neuropeptides
default_branch: main
pinned_commit: 08d6b7f
pinned_date: 2026-05-14
release_tag: (none — pinned to main HEAD)
license: CC BY 4.0
language: CSV + R
content_type: application/zip

# --- Fields posted to Dataverse ---
description: >-
  Snapshot of funkelab/drosophila_neuropeptides, a curated literature database of neuropeptide usage in Drosophila melanogaster organised around connectomic cell types. The repository ships a version-controlled gt_np_data.csv where each row pairs a cell type with a study reporting peptide expression, an evidence flag (immunohistochemistry, EASI-FISH, RNAi, MCFO, scRNA-seq, etc.) and a confidence score, together with one column per Zandawala 2024 gene-symbol peptide (taking values -1 / 0 / +1 for negative / no / positive evidence). A cross-dataset cell-type mapping table at inst/extdata/cell_type_cross_matching.csv links the cell-type names back to FAFB-FlyWire, MANC, Hemibrain, FANC, optic-lobe, maleCNS, L1 and BANC identifiers. This is the source dataset behind the neuropeptide_verified column in banc_888_meta.feather. Curated by Alexander Bates with Meet Zandawala, Diane Adjavon and Jan Funke. Distributed as a ZIP pinned to the main-branch HEAD at upload date; the repository is private upstream and continues to evolve at the GitHub URL above.
categories:
  - Code
directoryLabel: code
restrict: false
tabIngest: false
---

# drosophila_neuropeptides_archive.zip

## Purpose

This is the literature-derived ground-truth table from which the
BANC `neuropeptide_verified` column is populated. For each
connectomic cell type the table records every study that has
identified the peptides it expresses, the evidence method
(immunohistochemistry, EASI-FISH, RNAi, MCFO, scRNA-seq, etc.) and
a curator confidence score, so that downstream users can filter on
strength of evidence rather than taking a single label at face
value.

## Provenance

Collated by Alexander Bates while in Rachel Wilson's lab at Harvard
Medical School, in collaboration with Meet Zandawala (University of
Nevada, Reno) and curated jointly with Diane Adjavon in Jan Funke's
lab at Janelia Research Campus. The peptide-name vocabulary follows
Zandawala 2024; the cell-type cross-mapping is maintained in
tandem with the equivalent file in `drosophila_neurotransmitters`.

## Repository contents

- `gt_np_data.csv` — primary data table: one row per cell type × study, with peptide columns (-1 / 0 / 1) and confidence + evidence metadata.
- `gt_sources/` — source tables from individual studies that the curators ingested into `gt_np_data.csv`, including the `zandawala_2024/neuropeptide_meta.csv` canonical peptide-name glossary.
- `inst/extdata/cell_type_cross_matching.csv` — maps cell-type names across the connectome datasets (FAFB-FlyWire, MANC, Hemibrain, FANC, optic-lobe, maleCNS, L1 and BANC).
- `R/` — R scripts that normalise the CSV, validate the schema and push the result into `franken_meta` (the cross-dataset metadata used in BANC pipelines).
- `CITATIONS.md`, `README.md`, `LICENSE`, `drosophila_neuropeptides.Rproj` — full bibliography of contributing studies, the curator-facing README, and project metadata.

## Usage

```r
np <- readr::read_csv("gt_np_data.csv")
ct_map <- readr::read_csv("inst/extdata/cell_type_cross_matching.csv")
np %>% filter(cell_type == "DNa01") %>% select(cell_type, asta, tk, npf, neuropeptide_verified_source)
```

For BANC specifically, the resolved per-neuron labels are already
joined into `banc_888_meta.feather` under `neuropeptide_verified`
(semicolon-separated where a neuron expresses more than one
peptide). The raw CSV is most useful when you want the evidence
trail behind each label.

## Related files

- `banc_888_meta.feather` — `neuropeptide_verified` column is
  populated from this repository's CSV.
- `drosophila_neurotransmitters_archive.zip` — companion repository
  documenting fast-acting small-molecule transmitters; same curator
  team, same schema.
- `bancpipeline_archive.zip` — `banc/franken-annotations-fix.R` and the meta-integration scripts read this repository's CSV to populate per-dataset metadata.

## Notes

- This Dataverse copy is a point-in-time snapshot pinned to commit `08d6b7f` on the `main` branch (14 May 2026), which adds the CC BY 4.0 LICENSE file. For citation, prefer the upstream GitHub URL plus a commit SHA over the Dataverse archive.
- The upstream repository is currently **private** at the `funkelab` organisation (a request to make it public is in flight). The CC BY 4.0 license file is visible in the snapshot even though the repository itself remains private; the Dataverse deposit redistributes the contents under the same license. Curator team: Alexander Bates, Meet Zandawala, Diane Adjavon, Jan Funke.
- The peptide-symbol vocabulary changed in May 2026 to follow Zandawala 2024 (e.g. `allatostatin-a` → `AstA`, `tachykinin` → `Tk`). Anyone with older snapshots should consult the schema note in the upstream README before merging.
