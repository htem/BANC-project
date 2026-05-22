---
filename: influence_sensory_subclass_to_all.parquet
gcs_path: gs://lee-lab_brain-and-nerve-cord-fly-connectome/compiled_data/banc_888/influence/influence_sensory_subclass_to_all.parquet
size_bytes: 284977015
size_human: 271.78 MB
nrows: 13943894
ncols: 3
content_type: application/x-parquet

# --- Fields posted to Dataverse ---
description: >-
  Pre-computed adjusted-influence scores from each sensory cell
  sub-class onto every proofread BANC neuron — the dual of
  influence_all_to_effector_subclass. 13,943,894 rows by 3 columns
  (`source` sub-class label, `target` root_id, `influence` scalar).
  Sensory neurons are grouped by their `cell_sub_class` label,
  contributions across the members of each sub-class are summed, and
  the resulting per-source-class signal is propagated through the
  BANC network to every proofread or roughly-proofread target. The
  underlying calculation uses the ConnectomeInfluenceCalculator on
  the input-normalised connectivity matrix at `lambda_max = 0.99`
  and `count_thresh = 5`; the reported scalar is
  `max(0, log(raw_influence) + 24)` (paper Methods, "Influence").
  Used for the BANC paper panels that show
  sensory-side influence onto AN/DN clusters.
categories:
  - Data
  - Influence
directoryLabel: influence
restrict: false
tabIngest: false
---

# influence_sensory_subclass_to_all.parquet

## Purpose

This is the **sensory-sub-class-to-all-neurons** influence table:
for each sensory sub-class, the adjusted influence it exerts on
every proofread or roughly-proofread BANC neuron. Sensory sub-classes
are the natural coarsening when looking at how a sensory modality
influences downstream targets, since the receptive fields of
individual sensors are too fine-grained to read on a heatmap. The
table has three columns: `source` (a sensory sub-class label drawn
from `cell_sub_class` in the meta feather), `target` (a target
neuron root ID at v888), and `influence` (the adjusted scalar).

## Provenance

The underlying neuron-to-neuron influence was computed with the
**ConnectomeInfluenceCalculator** Python package (Ajabi and
Drugowitsch; https://github.com/DrugowitschLab/ConnectomeInfluenceCalculator,
Zenodo DOI 10.5281/ZENODO.15999930), driven from **bancpipeline** at
`banc/influence/banc-build-influence.R` and aggregated at
`banc/influence/banc-aggregate-influence.R`.

The calculator solves a linear-dynamical-systems model of signal
propagation. The connectivity matrix `W` is built from the
neuron-to-neuron edgelist (`banc_888_edgelist_simple_v2.feather`, v2
synapses with `size >= 5`) with each entry the **input-normalised**
synapse count; edges with `count < 5` (`count_thresh = 5`) are
dropped inside the calculator. `W` is rescaled so its largest real
eigenvalue equals `lambda_max = 0.99`. A unit signal is held at the
sensory seeds and the network's steady-state response is solved
analytically as `r_inf = -(W_tilde - I)^-1 s` using sparse PETSc /
SLEPc solvers.

For this file:

1. Seeds are grouped by sensory sub-class (`cell_sub_class` for
   neurons whose `super_class` is `sensory`); each group is driven
   as a single seed.
2. The reported scalar is `max(0, log(raw_influence) + 24)`,
   matching the default `const = 24` in `bancr::banc_influence()`
   and in `influencer`'s adjusted-influence routine.

The pre-aggregation neuron-to-neuron influence is too large to
deposit as a single file and is instead sharded under
`influence/all_to_all/`. The sensory sub-class file is the most
commonly used cooked-down view on the sensory side.

## Schema

| column | dtype | description |
|---|---|---|
| `source` | string | Sensory sub-class label. Drawn from `cell_sub_class` in `banc_888_meta.feather` for neurons whose `super_class` is `sensory`. |
| `target` | string | Root ID of the downstream neuron at v888 materialisation. |
| `influence` | double | Adjusted influence score, in `[0, infinity)`. Zero means no detectable influence at the per-pipeline floor (raw influence below `exp(-24) ≈ 3.78e-11`). |

## Usage

In R via bancr:

```r
library(arrow); library(dplyr)
inf <- read_parquet("influence_sensory_subclass_to_all.parquet")
inf %>% filter(source == "wing_campaniform_sensillum_neuron") %>%
  arrange(desc(influence)) %>% head(20)
```

In Python via pyarrow:

```python
import pyarrow.dataset as ds
ds.dataset("influence_sensory_subclass_to_all.parquet").to_table(
    filter=(ds.field("source") == "wing_campaniform_sensillum_neuron"),
).to_pandas()
```

## Related files

- `influence_all_to_effector_subclass.parquet` — the dual aggregation,
  from every neuron to each effector sub-class.
- `influence/all_to_all/` (sharded parquet directory) — the full
  pre-aggregation neuron-to-neuron influence; same algorithm, same
  edgelist, no roll-up to sub-classes.
- `banc_888_edgelist_simple_v2.feather` — the upstream edgelist that
  defines the connectivity matrix `W`.
- `banc_888_meta.feather` — defines the `cell_sub_class` vocabulary
  used as `source` here; join on `source` (as `cell_sub_class`) to
  look up the member root IDs of each sensory sub-class.
- `code/connectome_influence_calculator_archive.zip` (and the live
  https://github.com/DrugowitschLab/ConnectomeInfluenceCalculator;
  Zenodo DOI 10.5281/ZENODO.15999930) — the Python package that
  implements the calculator and was used to produce this file.
- `code/influencer_archive.zip` (and the live
  https://github.com/natverse/influencer; Zenodo DOI
  10.5281/zenodo.15999929) — the R port used by `bancr::banc_influence()`.
- The paper Methods section "Influence" defines the
  adjusted-influence metric and its constants in full.

## Notes

- The adjusted-influence constant `24` is the same one used
  everywhere in the paper and in `bancr::banc_influence()` (default
  `const = 24`).
- The `target` column is restricted to the proofread +
  roughly-proofread neuron set used in the all-to-all build. Filter
  against `banc_888_meta.feather` (`proofread == "TRUE"`) if you only
  want fully-proofread targets.
- Some sensory sub-classes have very few member neurons (for example
  specific rare chemoreceptors); their influence values can be more
  variable than those for the larger sub-classes such as
  mechanosensory or visual projection. Treat low-membership rows
  with caution.
