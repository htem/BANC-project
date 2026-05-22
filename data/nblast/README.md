# `data/nblast/` — top-NBLAST-match accuracy summary

One CSV: `banc_888_top_match_correct.csv`. Per-region NBLAST top-match
accuracy for BANC vs FAFB-FlyWire v783 and BANC vs MANC v1.2.1. For
every BANC neuron the top-scoring NBLAST match in the other dataset is
looked up and classified as "correct" if its cell type (via
`franken.meta`) equals the BANC neuron's cell type.

## Schema

Six columns (3 regions × 2 datasets) × 2 metrics each:

| Metric | Column suffix | What it is |
| --- | --- | --- |
| `pct_correct`        | per (region, dataset) | % of top matches whose cell type agrees |
| `mean_score_correct` | per (region, dataset) | Mean normalised NBLAST score among correct matches |

## Provenance

Written by `R/text/nblast_top_match_correct.R`, which reads the NBLAST
similarity feathers cached locally in `data/cache/` (pulled from GCS by
`bancr::banc_nblast_matches()`). Re-run when the NBLAST feathers refresh
or `banc.meta` cell-type assignments shift.

## Consumers

`R/text/numbers.R` (line ~342) reads this CSV and writes six `add_row()`
entries (3 regions × 2 datasets × {pct, mean_score}), which appear as
`var/nblast_*_pct_*` and `var/nblast_*_score_*` hyperlinks in the
manuscript prose.
