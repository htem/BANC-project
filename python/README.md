# `python/` — Python-side analyses and pipeline helpers

The BANC paper's main analysis is in [`R/`](../R/), but the
compute-heavy backend (sparse-graph influence, cascade modelling,
spectral clustering, exploratory bottleneck analyses) is implemented in
Python. This directory collects per-component Python code from the
project's development history; the **productionised versions** live in
sister repositories (see [the parent README's "BANC universe"
section](../README.md#the-banc-universe)).

## Inventory by subdirectory

### [`analysis_is-the-neck-a-bottleneck/`](analysis_is-the-neck-a-bottleneck/)

Three numbered analysis scripts + Sankey assets exploring whether the
neck connective is a *signalling* bottleneck (not just an anatomical
one). Each script is currently a placeholder (`raise NotImplementedError`)
with a docstring documenting the planned approach; the directory's own
[README.md](analysis_is-the-neck-a-bottleneck/README.md) frames the
question and the three approaches.

| File | Status | Approach |
| --- | --- | --- |
| `1_compare_larva_and_adult.py`        | stub | Use larval CNS (no anatomical bottleneck) as a no-bottleneck baseline; compare fraction of neurons that bridge brain ↔ VNC. |
| `2_model_interregional_connectivity.py` | stub | Fit a model of within-brain + within-VNC connectivity, predict the no-neck case, compare to observed. |
| `3_graph_partitioning.py`              | stub | Cluster the BANC graph and check at what cluster count `n` brain + VNC get put into different clusters. |
| `Yijie_larva_type_meta.csv`             | data | Larval cell-type metadata supplied by Yijie Yin (for approach 1). |
| `*.png` / `*.ai` Sankey assets         | figs | Preliminary Sankey renders comparing larva vs adult brain ↔ VNC structure. |

### [`betweeness/`](betweeness/)

Early sensory→effector betweenness-centrality work that fed the paper's
Fig. 3a analysis. Two files:

| File | What it is |
| --- | --- |
| `Results_afferent_to_efferent.ipynb` | The original Jupyter notebook (2025-10-31) computing afferent → efferent betweenness for BANC v626 using the Brandes algorithm in python-igraph. |
| `betweenness_afferent_to_efferent.csv` | Per-neuron betweenness scores from the notebook run; superseded by the v888 versions in [`data/betweenness/888/`](../data/betweenness/888/). |

The current canonical pipeline lives at
[`bancpipeline/banc/betweenness/banc-betweenness.py`](https://github.com/htem/bancpipeline);
the version-naming in `data/betweenness/<NNN>/` matches its outputs.
The directory name here keeps the original typo (`betweeness` not
`betweenness`) so the notebook's hard-coded relative paths still resolve.

### [`cascade model/`](cascade%20model/)

The signal-cascade algorithm (Winding et al. 2023, *Science*) used to
cross-check the adjusted-influence metric (Fig. 2b, ED Fig. 4a). The
core class is `cascade_model.SignalCascade`:

| File | What it does |
| --- | --- |
| `cascade_model.py`               | The `SignalCascade` class: stochastic linear-threshold cascade over a sparse graph. Configurable `p_transmission`, `activation_threshold`, `n_iterations`, `max_timesteps`. |
| `batch_cascade.py`               | Batch driver — reads a JSON manifest of source neuron groups, runs `SignalCascade` per group, pickles activation distributions to disk. |
| `batch_cascade_franken.py`       | Same as `batch_cascade.py` but on the merged "frankenbrain v1.6" graph (BANC + FAFB + MANC). Produces the pickles in [`data/cascade/frankenbrain_v1.6/`](../data/cascade/frankenbrain_v1.6/). |
| `real_data_cascade_main.py`      | Single-source SLURM-worker entry point — used inside the array job that produced the per-modality cascade pickles on HMS O2. |
| `recovered_real_data_cascade_main.py` | Checkpoint-recovery variant of `real_data_cascade_main.py`; resumes from a partial pickle if the original SLURM job hit its wall-clock limit. |
| `banc.sh`                         | The SLURM submission wrapper used at HMS O2. |
| `*.ipynb`                         | Three exploratory notebooks (`mb_project_analysis.ipynb`, `modality_dis_to_DAN.ipynb`, `real_data_cascade.ipynb`) used to develop the cascade pipeline. |

Directory name contains a space; reference it as `python/cascade\ model/`
in shell. The productionised copy of `cascade_model.py` lives at
[`bancpipeline/analysis/python/cascade_model.py`](https://github.com/htem/bancpipeline);
the version here is kept as the original implementation.

### [`feedforward_layers/`](feedforward_layers/)

One CSV — `layers_banc_626.csv` — recording per-neuron forward / backward
graph-traversal layer counts for BANC v626 (Schlegel et al. 2024 method).
Original delivery of the layer table; downstream analysis reads the
copy at [`data/feedforward/layers_banc_626.csv`](../data/feedforward/).

### [`seatable/`](seatable/)

Two minimal Python-side examples for querying SeaTable directly via
the `seatable_api` package — useful templates if you need Python access
to the live curation tables, but most of the pipeline uses the R client
([`bancr::banctable_query()`](https://github.com/natverse/bancr)).

| File | What it queries |
| --- | --- |
| `read_from_banc_seatable.py`    | The `banc_meta` base — BANC manual curations. |
| `read_from_franken_seatable.py` | The `cns_meta` base — cross-dataset frankenbrain (BANC ↔ FAFB ↔ MANC) cell-type bridge. |

Both expect `BANCTABLE_TOKEN` in the environment; create a personal
token at the [SeaTable API portal](https://api.seatable.io/reference/getaccounttokenfromusername).

### [`spectral_clustering/`](spectral_clustering/)

The spectral-clustering development workspace — what eventually became
the paper's Fig. 6 / ED Fig. 10 CNS-network analysis.

| File | What it is |
| --- | --- |
| `banc_loading.py`                       | Helpers to load BANC v626 SQLite tables into `scipy.sparse.coo_matrix` form for spectral clustering. |
| `banc_spectral_clustering_final.ipynb`  | The notebook in which the spectral-clustering method was developed and the parameters (cluster count 13, embedding seed 3, cluster seed 10, min connection strength 1) were chosen. |
| `download-4.png`                         | Reference plot saved during method development. |

The productionised pipeline (which produced the canonical v888 outputs
in [`data/cns_network/`](../data/cns_network/)) lives at
[`bancpipeline/banc/clustering/banc-spectral-clustering.py`](https://github.com/htem/bancpipeline).

## Running locally

Python 3.10+; package dependencies are pinned in
[`../requirements.txt`](../requirements.txt). The compute-heavy paths
(spectral clustering on the full BANC graph, PETSc-backed influence
calculation, the cascade model with high `n_iterations`) are typically
run on a SLURM cluster — see
[`bancpipeline/o2/README.md`](https://github.com/htem/bancpipeline/blob/main/o2/README.md)
for the HMS O2 job-submission patterns we used.

## Code style

Every module here has a top-level docstring summarising its purpose,
inputs, outputs, and where the productionised version (if any) lives.
Function-level docstrings follow the NumPy convention. The
`raise NotImplementedError` stubs in
`analysis_is-the-neck-a-bottleneck/` keep their docstrings as a planning
record.

## Where the productionised code lives

| Concern | Repository |
| --- | --- |
| Influence calculation (PETSc / SLEPc backend) | [`ConnectomeInfluenceCalculator`](https://github.com/DrugowitschLab/ConnectomeInfluenceCalculator) |
| Influence calculation (R wrapper) | [`influencer`](https://github.com/natverse/influencer) |
| Neurotransmitter prediction model | [`synister_banc`](https://github.com/htem/synister_banc) |
| Full BANC data pipeline (skeletonisation, NBLAST, metric calculation, betweenness, spectral clustering, …) | [`bancpipeline`](https://github.com/htem/bancpipeline) |
| BANC Python client (PyPI) | [`banc`](https://pypi.org/project/banc/) |

See [the parent README's "BANC universe" section](../README.md#the-banc-universe)
for the full ecosystem map.
