# R/text/ — non-figure text outputs

These scripts produce the numbers, supplementary tables, and machine-
readable indices that the manuscript prose cites. They follow the same
load convention as `R/figures/` scripts but write to
`manuscript/print/` (and Google Sheets where applicable) rather than to
`figures/figure_N/links/`.

## Scripts

- **`numbers.R`** — compiles every `var/<identity>` placeholder cited in
  the manuscript into a single `manuscript/print/numbers.csv`, then
  refreshes the bound Google Sheet so that the corresponding hyperlink
  text in the doc body updates. Writes ~250 rows (auto + manual). The
  Drive write requires `banc.keys$gsheet_banc_variables_id` to be set
  via `data/private/keys.csv` — absent that, the local CSV is still
  saved and the Drive step is skipped with a clear message.
- **`supplemental_data.R`** — writes the ten Supplementary Data CSVs
  under `manuscript/print/supplemental_data/supplemental_data_*.txt`.
  These match the Supps cited in the paper (annotation taxonomy,
  per-dataset metadata, UMAP coordinates, literature review, dataset
  issues).
- **`nblast_top_match_correct.R`** — recomputes the cross-dataset
  NBLAST top-match accuracy table (% of BANC neurons whose top NBLAST
  hit in FAFB/MANC has the matching cell type). Writes
  `data/nblast/banc_888_top_match_correct.csv` which `numbers.R` reads
  to populate the corresponding manuscript variables.
- **`ngl_links.R`** — regenerates the Neuroglancer state URLs cited in
  figure legends and vignettes. Reads from
  `figures/vignette_neuron_lists/network_<vig>_neurons.csv`.
- **`generate_author_list.R`** — utility that produces the author list +
  affiliations block from the structured author CSV. Manually run when
  authorship updates.

## Order of operations

If rerunning the whole pipeline:

1. Run the figure scripts first (they write the per-panel statistical
   sidecars that `numbers.R` and `supplemental_data.R` read).
2. `Rscript R/text/nblast_top_match_correct.R`
3. `Rscript R/text/supplemental_data.R`
4. `Rscript R/text/numbers.R`
