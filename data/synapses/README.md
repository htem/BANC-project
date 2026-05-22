# `data/synapses/` — per-synapse human-review samples

Human-reviewed synapse samples used to validate the synapse classifier
in Methods §"Synapse detection evaluation" and **ED Fig. 1b–d**.

## Layout

| File | What it is |
| --- | --- |
| `2024-09-20_aelysia_synapse_sample_complete.csv` | The first round of Aelysia-team manual synapse review (Sep 2024) — completed sample. |
| `251013_evaluation_links.csv` | The 13 Oct 2025 evaluation batch — one row per to-review synapse, with the Neuroglancer link and the reviewer's eventual call. |
| `251013_synapse_evaluation.csv` | The 13 Oct 2025 per-synapse outcomes (the evaluator's verdict on each candidate). |
| `banc_888_v2_synapse_sample_small_2026-04-21.csv` | The v888 / synapses-v2 small-sample evaluation used in the paper. |
| `banc_888_v3_synapse_sample_2026-05-14.csv` | The v888 / synapses-v3 sample — used to compare v2 and v3 predictions on the same neurons. |

## Provenance

Per-synapse rosters were exported from CAVE; the verdict columns were
filled by hand by reviewers (Aelysia team for the 2024 round; the BANC
authors for the 2025/2026 rounds). The exact link between a candidate
synapse and its Neuroglancer view is the `connector_id` / coordinate
columns.

## Consumers

`R/figures/panels_synapse_review.R` reads these files to compute the
per-synapse precision / recall / sample-size tallies that go into ED
Fig. 1b–d, and writes `data/synapse_capture/`-style sidecars consumed by
`R/text/numbers.R` (e.g. `synapse_v2_sample_fiveorless_*` variables).
