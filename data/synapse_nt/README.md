# `data/synapse_nt/` — neurotransmitter-prediction confusion matrices

Synapse-level neurotransmitter classifier confusion matrices, versioned
by classifier release.

## Layout

```
synapse_nt/
├── v1/   # BANC v1 NT classifier (preprint era)
└── v2/   # BANC v2 NT classifier (paper version, retrained Jul 2025)
```

Within each version directory, both raw-count and row-normalised forms
are provided. The v2 files include the test-set evaluation date in the
filename (`22072025`, i.e. 22 Jul 2025):

| File pattern | What it is |
| --- | --- |
| `nt_prediction_confusion_matrix_on_gt_*.csv`            | Raw counts: rows = ground-truth neurotransmitter, cols = predicted neurotransmitter. |
| `nt_prediction_*normalized*_confusion_matrix_*.csv`     | Row-normalised (each ground-truth row sums to 1) — used directly as the heatmap input. |

## Provenance

Trained and predicted by the BANC neurotransmitter classifier (Methods
§"Neurotransmitter prediction") — see
[`synister_banc`](https://github.com/htem/synister_banc) for the model
code. The "on_gt" file family is evaluated on a held-out hand-labelled
ground-truth set; the "test_set" variant is the canonical v2 evaluation.

## Consumers

`R/figures/panels_transmitter_predictions.R` reads the v2 row-normalised
test-set file (currently
`v2/nt_prediction_confusion_matrix_on_gt_normalized_22072025_test_set.csv`)
for **ED Fig. 3a–e** (neurotransmitter-prediction accuracy by class).

## Paper version

**v2** is the paper version. v1 is retained for reproducibility of
preprint-era panels.
