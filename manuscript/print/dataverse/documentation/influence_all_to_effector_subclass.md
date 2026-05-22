---
filename: influence_all_to_effector_subclass.parquet
gcs_path: gs://lee-lab_brain-and-nerve-cord-fly-connectome/compiled_data/banc_888/influence/influence_all_to_effector_subclass.parquet
size_bytes: 35448133
size_human: 33.81 MB
nrows: 3334800
ncols: 3
content_type: application/x-parquet

# --- Fields posted to Dataverse ---
description: >-
  Pre-computed adjusted-influence scores from every BANC neuron onto
  each effector cell sub-class — motor, endocrine and efferent
  targets grouped by body part and effector type. 3,334,800 rows by
  3 columns (`source` root_id, `target` sub-class label, `influence`
  scalar). Adjusted influence is `max(0, log(raw_influence) + 24)`,
  where raw influence is the steady-state response of the target
  sub-class to a sustained unit signal at the source neuron, computed
  with the ConnectomeInfluenceCalculator on the input-normalised
  connectivity matrix at `lambda_max = 0.99` and `count_thresh = 5`
  (paper Methods, "Influence"). This is the table behind
  the BANC paper's AN/DN-to-effector influence panels.
categories:
  - Data
  - Influence
directoryLabel: influence
restrict: false
tabIngest: false
---

# influence_all_to_effector_subclass.parquet

## Purpose

This is the **all-neurons-to-effector-sub-class** influence table:
for every BANC neuron, the adjusted influence it exerts on each
effector sub-class (motor, endocrine and other efferent targets,
grouped by body part). Effector sub-classes are the natural
coarsening the paper uses for plotting AN/DN influence onto
effectors, since individual motor-neuron identities can be too
fine-grained to read on a heatmap. The table has three columns:
`source` (a neuron root ID), `target` (an effector sub-class label
such as `wing_steering_motor_neuron`, `front_leg_motor_neuron`,
`abdomen_neurosecretory_cell` or `digestive_tract_neurosecretory_cell`,
drawn from `cell_sub_class` in the meta feather), and `influence`
(the adjusted scalar).

## Provenance

The underlying neuron-to-neuron influence was computed with the
**ConnectomeInfluenceCalculator** Python package (Ajabi and
Drugowitsch; https://github.com/DrugowitschLab/ConnectomeInfluenceCalculator,
Zenodo DOI 10.5281/ZENODO.15999930), driven from **bancpipeline** at
`banc/influence/banc-build-influence.R` and aggregated at
`banc/influence/banc-aggregate-influence.R`.

The calculator implements the linear-dynamical-systems model
described in the paper Methods section "Influence".
Briefly: the connectivity matrix `W` is built from the
neuron-to-neuron edgelist (`banc_888_edgelist_simple_v2.feather`, v2
synapses with `size >= 5`) with each entry the **input-normalised**
synapse count — the fraction of a target's input that comes from a
given source. Edges with `count < count_thresh` (`count_thresh = 5`)
are dropped inside the calculator. `W` is rescaled so its largest
real eigenvalue equals `lambda_max = 0.99`, a unit signal is held at
the seed neurons, and the network's steady-state response is solved
analytically as `r_inf = -(W_tilde - I)^-1 s` using sparse PETSc /
SLEPc solvers.

For this file specifically:

1. Per-target steady-state response is summed across the member root
   IDs of each effector sub-class (drawn from `cell_sub_class` in
   `banc_888_meta.feather` for neurons whose `super_class` is
   `motor`, `endocrine` or `efferent`).
2. The adjusted-influence scalar reported is
   `max(0, log(raw_influence) + 24)`, matching the default
   `const = 24` in `bancr::banc_influence()` and in `influencer`'s
   adjusted-influence routine.

The pre-aggregation neuron-to-neuron influence is too large to
deposit as a single file and is instead sharded under
`influence/all_to_all/`. The effector sub-class file is the most
commonly used cooked-down view.

## Schema

| column | dtype | description |
|---|---|---|
| `source` | string | Root ID of the upstream (seed) neuron at v888 materialisation. |
| `target` | string | Effector sub-class label. Drawn from `cell_sub_class` in `banc_888_meta.feather` for neurons whose `super_class` is `motor`, `endocrine` or `efferent`. |
| `influence` | double | Adjusted influence score, in `[0, infinity)`. Zero means no detectable influence at the per-pipeline floor (raw influence below `exp(-24) ≈ 3.78e-11`). |

## Usage

In R via bancr:

```r
library(bancr); library(dplyr)
inf <- arrow::read_parquet("influence_all_to_effector_subclass.parquet")
inf %>% filter(source == "720575941521131930") %>%
  arrange(desc(influence)) %>% head(20)
```

For finer-grained neuron-to-neuron influence, use
`bancr::banc_influence()` against the `all_to_all` shards instead.

## Related files

- `influence_sensory_subclass_to_all.parquet` — the dual aggregation,
  from every sensory sub-class to every neuron.
- `influence/all_to_all/` (sharded parquet directory) — the full
  pre-aggregation neuron-to-neuron influence; same algorithm, same
  edgelist, no roll-up to sub-classes.
- `banc_888_edgelist_simple_v2.feather` — the upstream edgelist that
  defines the connectivity matrix `W`.
- `banc_888_meta.feather` — defines the `cell_sub_class` vocabulary
  used as `target` here; join on `target` (as `cell_sub_class`) to
  look up the member root IDs of each effector sub-class.
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
  `const = 24`). Do not confuse it with a per-target normalisation.
- Some effector sub-classes have very few member neurons (for
  example specific feeding-related motor groups); their influence
  values can be more variable than those for the larger sub-classes
  such as wing power or leg motor groups. Treat low-membership rows
  with caution.
- The `source` column is restricted to the proofread +
  roughly-proofread neuron set used in the all-to-all build. Filter
  against `banc_888_meta.feather` (`proofread == "TRUE"`) if you
  only want fully-proofread sources.
