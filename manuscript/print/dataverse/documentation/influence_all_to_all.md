---
filename: influence/all_to_all/
gcs_path: gs://lee-lab_brain-and-nerve-cord-fly-connectome/compiled_data/banc_888/influence/all_to_all/
size_bytes: 308262988315
size_human: 287.10 GB
nshards: 277
shard_format: parquet
shard_pattern: chunk_NNNN.parquet
shard_example_size_bytes: 1097030518
shard_example_size_human: 1.02 GB
shard_example_nrows: 69107500
content_type: directory (parquet partition)

# --- Fields posted to Dataverse ---
# This is a DIRECTORY upload — each shard chunk_NNNN.parquet is uploaded as
# its own Dataverse file under `directoryLabel: influence/all_to_all`, with
# the description below applied to each shard. A companion top-level README
# file (this body, written as influence_all_to_all_README.md) sits in the
# same directoryLabel as the index.
description: >-
  One of 277 parquet shards comprising the full BANC v888
  all-neurons-to-all-neurons raw-influence table. Schema is three
  columns — upstream_id, downstream_id, raw_influence — and each
  shard holds 500 source neurons by ~138,000 target neurons (~69
  million rows, ~1 GB). Across the full 277-shard set, every
  proofread or roughly-proofread BANC neuron appears exactly once as
  an upstream seed. Raw influence is the steady-state response of the
  target to a sustained unit signal at the source, computed with the
  ConnectomeInfluenceCalculator on the input-normalised connectivity
  matrix at lambda_max = 0.99 and count_thresh = 5 (paper Methods,
  "Influence"). Adjusted influence — the metric used throughout the
  paper — is max(0, log(raw_influence) + 24). To pull pairs
  efficiently, scan
  with pyarrow.dataset over the whole directory with predicate
  pushdown on upstream_id or downstream_id; do not download the full
  287 GB unless you have storage to spare.
categories:
  - Data
  - Influence
directoryLabel: influence/all_to_all
restrict: false
tabIngest: false
---

# influence/all_to_all/ — sharded BANC v888 all-pairs raw influence

## Purpose

The 277 parquet files at `influence/all_to_all/chunk_NNNN.parquet`
together form the **full BANC v888 all-neurons-to-all-neurons raw
influence table**. Every row records a directed pair: a source
neuron, a target neuron, and the raw influence score from source to
target. Adjusted influence — the metric used throughout the paper —
is `max(0, log(raw_influence) + 24)` (see paper Methods, "Influence";
computed on the fly by readers).

The table is too large to deposit as a single object (~287 GB), so it
is sharded by **source neuron**. Each shard holds 500 seed neurons
against ~138,000 proofread or roughly-proofread targets (~69 million
rows per shard). Across the 277 shards every proofread or
roughly-proofread BANC neuron appears exactly once as a source.

## Provenance

Built by **bancpipeline** at `banc/influence/banc-build-influence.R`,
driving the **ConnectomeInfluenceCalculator** Python package
(Ajabi and Drugowitsch; https://github.com/DrugowitschLab/ConnectomeInfluenceCalculator,
Zenodo DOI 10.5281/ZENODO.15999930). The build pipeline:

1. Loads the neuron-to-neuron edgelist
   (`banc_888_edgelist_simple_v2.feather`, v2 synapses with
   `size >= 5`) and filters to the proofread + roughly-proofread
   neuron set (the union of CAVE `backbone_proofread` and SeaTable
   `roughly_proofread`).
2. Initialises `influence_calculator_py(filename = sqlite_path,
   count_thresh = 5, signed = FALSE)`. The Python calculator drops
   edges with `count < count_thresh`, then builds the connectivity
   matrix `W` with each entry the **input-normalised** synapse count
   (the fraction of a target's input that comes from a given source),
   and rescales `W` so its largest real eigenvalue equals
   `lambda_max = 0.99`.
3. Iterates over batches of 500 seed neurons. For each batch, holds
   a unit signal at the seeds and solves analytically for the
   network's steady-state response,
   `r_inf = -(W_tilde - I)^-1 s`, against all proofread or
   roughly-proofread targets, using sparse PETSc / SLEPc solvers.
4. Writes each batch as one parquet shard (`chunk_NNNN.parquet`)
   with one row per (source, target) pair.

Adjusted influence is computed on the fly by readers as
`max(0, log(raw_influence) + 24)`; the constant `24` corresponds to a
floor at `exp(-24) ≈ 3.78e-11` raw influence. It matches the default
`const = 24` in `bancr::banc_influence()` and in `influencer`'s
adjusted-influence routine.

## Shard schema

Every shard has the same three columns:

| column | dtype | description |
|---|---|---|
| `upstream_id` | string | Root ID of the source neuron at v888 materialisation. Exactly 500 distinct values per shard, drawn from the proofread + roughly-proofread seed set. |
| `downstream_id` | string | Root ID of the target neuron at v888. ~138,000 distinct values per shard, covering all proofread or roughly-proofread targets reached by the calculator. |
| `raw_influence` | double | Steady-state response of the target to a sustained unit signal at the source. Always positive; very small values dominate the long tail. Take `max(0, log(raw_influence) + 24)` for the adjusted-influence metric used in the paper. |

## Usage

Treat the whole directory as a single pyarrow dataset — predicate pushdown
across all 277 shards is fast because each shard is partitioned into
~66 row groups internally. Do not download the full directory unless you
have ~300 GB free.

In Python via pyarrow:

```python
import numpy as np
import pyarrow as pa
import pyarrow.dataset as ds
fs = pa.fs.GcsFileSystem(anonymous=False)
inf = ds.dataset(
    "lee-lab_brain-and-nerve-cord-fly-connectome/compiled_data/banc_888/influence/all_to_all/",
    filesystem=fs, format="parquet",
)
pairs = inf.to_table(
    filter=(ds.field("upstream_id") == "720575941521131930"),
    columns=["upstream_id", "downstream_id", "raw_influence"],
).to_pandas()
pairs["adjusted_influence"] = (np.log(pairs["raw_influence"]) + 24).clip(lower=0)
```

In R via bancr, the high-level wrapper `banc_influence()` reads from
a local cache built from this directory:

```r
inf <- banc_influence(upstream_ids = c("720575941521131930"))
```

## Related files

- `influence_all_to_effector_subclass.parquet` — coarsened roll-up to
  effector sub-classes; ~30 MB, 3.3 M rows. Use this if you only need
  influence onto effector groups, not individual neurons.
- `influence_sensory_subclass_to_all.parquet` — the dual roll-up, from
  every sensory sub-class to every neuron.
- `banc_888_edgelist_simple_v2.feather` — the upstream edgelist from
  which the influence operator was built.
- `banc_888_meta.feather` — provides anatomical and functional labels
  for both source and target neurons.
- `code/connectome_influence_calculator_archive.zip` (and the live
  https://github.com/DrugowitschLab/ConnectomeInfluenceCalculator;
  Zenodo DOI 10.5281/ZENODO.15999930) — the Python package that
  implements the calculator and was used to produce these shards.
- `code/influencer_archive.zip` (and the live
  https://github.com/natverse/influencer; Zenodo DOI 10.5281/zenodo.15999929)
  — the R port used by bancr's `banc_influence()`.
- The paper Methods section "Influence" defines the metric and its
  scaling in full.

## Notes

- **Shard-to-seed mapping.** The shard a given source neuron lives
  in is not encoded in the filename. To locate a specific source,
  scan the whole dataset; pyarrow's predicate pushdown on
  `upstream_id` makes this fast (seconds per query, dominated by
  shard listing rather than data read).
- **Why not all-to-all over every BANC neuron.** Sources and targets
  are restricted to the proofread + roughly-proofread neuron set
  (~138,000 neurons; union of CAVE `backbone_proofread` and SeaTable
  `roughly_proofread`). Neurons without any proofreading flag carry
  too much segmentation noise for a reliable steady-state response.
- **Why 500 seeds per shard.** Empirical compromise: shards stay
  near 1 GB each (good for GCS streaming and for distributing across
  cluster workers), and 277 shards is small enough to enumerate
  without pagination.
- **Row count per shard.** The seed batch is always 500 sources, but
  some seeds drop out if they are missing from the proofread
  edgelist; the target set per shard is the global proofread +
  roughly-proofread pool reached by the calculator. Expect ~69M
  rows per shard, with slight variation.
- **Disk-space warning.** The full set is 287 GB. Avoid
  `gsutil rsync` unless you have explicit reason to copy everything;
  the predicate-pushdown read pattern over GCS is the recommended
  access path.
