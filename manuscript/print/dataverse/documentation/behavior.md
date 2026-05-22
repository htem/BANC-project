---
filename: behavior.zip
gcs_path: gs://lee-lab_brain-and-nerve-cord-fly-connectome/behavior/
unzipped_size_bytes: 365283167
unzipped_size_human: 348.3 MiB
content_type: application/zip

# --- Fields posted to Dataverse ---
description: >-
  Behavioral characterization of the individual fly used for the BANC
  EM volume. Before sample preparation, the fly was scored in a Y-maze
  handedness assay alongside a population of control flies; the raw
  per-trial data, metadata, example analysis CSVs / code, and a
  population summary plot are included. Used in the paper to place
  the BANC fly within the natural range of fly behavior (in
  particular, to document its strongly right-handed locomotor bias).
  Organized as a folder tree (raw_data / meta_data / example_csv /
  example_code / README) with a top-level population-summary PNG.
categories:
  - Data
  - Behavior
directoryLabel: behavior
restrict: false
tabIngest: false
---

# behavior.zip

## Purpose

The BANC EM volume comes from a single adult female *Drosophila
melanogaster*. To anchor that individual in the natural range of fly
behavior, the fly was screened in an acrylic Y-maze handedness assay
before sample preparation; this archive contains the per-trial data
for the BANC fly and a control cohort.

Two complementary uses:

- **Individual characterization** — recording the BANC fly's
  locomotor handedness and total choice count (the paper reports
  70% right turns over 582 choices, placing it in the 96.75th
  percentile of the screening cohort).
- **Population context** — comparing the BANC fly against the n=1171
  control cohort; this is what the population-summary PNG
  illustrates.

## Provenance

Acquired and analyzed by the de Bivort lab (Ryan Maloney) following
the lab's high-throughput Y-maze handedness paradigm. After the
behavioral test, the fly was housed and processed for the EM
workflow. See paper Methods section "Specimen".

## Contents

Not tabular at the archive level. The ZIP unpacks to:

- `rBIas_all_flies.png` — population-summary plot.
- `BANC_fly_behavior/README.md.rtf` — top-level README.
- `BANC_fly_behavior/raw_data/` — per-trial raw recordings (the bulk
  of the ZIP).
- `BANC_fly_behavior/meta_data/` — trial-level metadata and
  condition labels.
- `BANC_fly_behavior/example_csv/` — pre-extracted trajectory CSVs.
- `BANC_fly_behavior/example_code/` — analysis scripts that read the
  CSVs and reproduce the summary metrics.

Per-trial CSV schemas are documented in the `example_code` scripts;
the README provides a top-level orientation.

## Usage

```bash
unzip behavior.zip
open BANC_fly_behavior/README.md.rtf   # macOS
cat BANC_fly_behavior/README.md.rtf    # everywhere else
```

The example_code scripts can be run on the example_csv files to verify
the analysis end-to-end before adapting to your own questions.

## Related files

- `banc_microCT.zip` — whole-body microCT scan of the same fly.
- The paper Methods section "Specimen" and Extended Data Fig. 1
  describe the assay.

## Notes

- BANC-fly data come from a **single fly**. Population statistics in
  the summary plot are from a separate control cohort (n=1171).
- Raw video is not included unless it is in `raw_data/` (check the
  README); the bulk of the archive is tracking output and derived
  metrics.
- The behavioral paradigm and scoring conventions follow the de
  Bivort lab's published handedness assays; for the BANC-specific
  setup the README inside the archive is the canonical reference.
