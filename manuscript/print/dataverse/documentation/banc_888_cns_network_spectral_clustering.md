---
filename: banc_888_cns_network_spectral_clustering_v2.csv
gcs_path: gs://lee-lab_brain-and-nerve-cord-fly-connectome/compiled_data/banc_888/banc_888_cns_network_spectral_clustering_v2.csv
sibling_file: banc_888_cns_network_spectral_clustering_v3.csv
content_type: text/csv

# --- Fields posted to Dataverse ---
description: >-
  Per-neuron CNS-network assignment from the spectral-clustering
  analysis described in Fig. 6 and Extended Data Fig. 10. Each row is
  one intrinsic neuron that entered the clustering pool (central brain
  / VNC intrinsics + ANs + DNs + visual projection + visual centrifugal,
  iteratively pruned to retain neurons with at least one input and one
  output partner among the remaining set). Columns: root_id,
  supervoxel_id, position, spectral_cluster integer, UMAP coordinates
  on the connectivity Laplacian, and cns_network — the named network
  used in figures (e.g. central complex related, abdominal VNC, left
  olfactory). Distributed in two versions: v2 (rolled up from v2
  synapses, count >= 5) and v3 (rolled up from v3 synapses,
  count >= 10). The paper uses the v2 variant.
categories:
  - Data
  - Connectivity
directoryLabel: compiled_data
restrict: false
tabIngest: false
---

# banc_888_cns_network_spectral_clustering_v2.csv (and _v3.csv)

## Purpose

The CNS-wide spectral-clustering output behind Fig. 6 and Extended
Data Fig. 10. Each row is one intrinsic BANC neuron with its assigned
network label, integer cluster id, and 2D UMAP coordinates on the
graph Laplacian. The `cns_network` column is the user-facing label
(e.g. `central complex related`, `abdominal VNC`, `left olfactory`)
that flows into `banc_888_meta.feather`.

## Provenance

Built by the bancpipeline spectral-clustering pipeline
(`banc/clustering/banc-spectral-clustering.{py,R}`). Briefly: the
intrinsic-neuron edgelist (count >= 5 for v2, count >= 10 for v3) is
converted to a weighted undirected graph; the first k eigenvectors of
the graph Laplacian are computed; k-means assigns each neuron to one
of k clusters. Iterative pruning drops neurons that lack at least one
input and one output among the remaining set before the eigenvector
step. The CNS-network labels are post-hoc assignments to the integer
clusters, decided by the BANC team based on each cluster's anatomy
and dominant cell-type membership.

Parameters used for the paper version (`_v2`): min connection
strength = 1, cluster count = 14, cluster seed = 10, embedding
seed = 3.

The `_v3` variant is the same pipeline against the v3 synapse
edgelist; the cluster assignments are nearly the same but the integer
labels are not directly comparable across versions.

## Schema

| column | dtype | description |
|---|---|---|
| `root_id` | string | BANC v888 root ID of the intrinsic neuron. |
| `supervoxel_id` | string | A supervoxel of the neuron, for chunked-graph resolution. |
| `position` | string | Soma position in BANC raw voxel space, `"x, y, z"`. |
| `spectral_cluster` | int | 1-indexed cluster ID assigned by k-means on the Laplacian eigenvectors. |
| `umap_x` | double | UMAP coordinate 1 of the connectivity-Laplacian embedding. |
| `umap_y` | double | UMAP coordinate 2. |
| `cns_network` | string | User-facing network label (e.g. `central complex related`, `abdominal VNC`, `left olfactory`). Same vocabulary that appears in the `cns_network` column of `banc_888_meta.feather`. |

## Usage

```r
library(readr); library(dplyr)
sc <- read_csv("banc_888_cns_network_spectral_clustering_v2.csv")
sc %>% count(cns_network, sort = TRUE)
```

```python
import pandas as pd
sc = pd.read_csv("banc_888_cns_network_spectral_clustering_v2.csv")
sc.groupby("cns_network").size().sort_values(ascending=False)
```

## Related files

- `banc_888_meta.feather` — joins to this table on `root_id`; the
  `cns_network` column there is the value carried in this file.
- Supplementary Data 8 (in `banc_supplemental_data.zip`) is a richer
  per-neuron view that pairs the spectral-clustering output with the
  AN/DN / effector cluster labels and full annotation columns.
- `banc_888_edgelist_simple_v2.feather` / `_v3.feather` — the edgelist
  the spectral-clustering pipeline consumes.
- `bancpipeline_archive.zip` — `banc/clustering/banc-spectral-clustering.{py,R}`
  contains the implementation.

## Notes

- The version suffix follows the synapse-prediction snapshot used to
  build the edgelist, not the CAVE materialization (which is v888 for
  both files). Use `_v2` to reproduce the paper.
- Cluster integers are not stable across reruns — only the
  `cns_network` strings are part of the project's controlled
  vocabulary. Join on those for downstream code.
- The optic lobe is excluded by design (its ~93 k intrinsic neurons
  would dominate any unweighted clustering).
