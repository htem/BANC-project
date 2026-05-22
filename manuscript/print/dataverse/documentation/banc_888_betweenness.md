---
filename: banc_888_betweenness_all_to_all_v2.csv
gcs_path: gs://lee-lab_brain-and-nerve-cord-fly-connectome/compiled_data/banc_888/banc_888_betweenness_all_to_all_v2.csv
sibling_files:
  - banc_888_betweenness_all_to_all_v3.csv
  - banc_888_betweenness_afferent_to_efferent_v2.csv
  - banc_888_betweenness_afferent_to_efferent_v3.csv
content_type: text/csv

# --- Fields posted to Dataverse ---
description: >-
  Per-neuron betweenness centrality on the BANC v888 connectome.
  Distributed in four variants: all-to-all (sum over every directed
  source-target pair) and afferent-to-efferent (sum over sensory
  ascending + sensory descending + sensory sources to motor + visceral
  circulatory + ascending visceral circulatory targets), each at the v2
  synapse threshold (count >= 5) and the v3 synapse threshold
  (count >= 10). Computed with the Brandes algorithm (igraph Python
  implementation) on the unweighted directed BANC edgelist, restricted
  to intrinsic + AN + DN + visual projection / centrifugal neurons.
  Used to identify bottleneck neurons in Fig. 3a (afferent-to-efferent
  variant) and Extended Data Fig. 5a (all-to-all variant), and to rank
  candidate hubs in the AN/DN behavior-centric modules analysis.
  Producer: bancpipeline/banc/betweenness/banc-betweenness.py
  (Methods §"Betweenness centrality"; htem/bancpipeline).
categories:
  - Data
  - Connectivity
directoryLabel: compiled_data
restrict: false
tabIngest: false
---

# banc_888_betweenness_*.csv (4 variants)

## Purpose

Per-neuron betweenness centrality for BANC v888 — a standard network
metric that quantifies how often a given node lies on the shortest
paths between pairs of other nodes (excluding the endpoints). Neurons
with high betweenness centrality lie on many routes between other
neurons and are positioned at putative bottlenecks in the network.

Two flavors are deposited:

- **All-to-all** betweenness — sum over every directed
  (source, target) pair in the network. Extended Data Fig. 5a.
- **Afferent-to-efferent** betweenness — same Brandes algorithm but the
  source-target accumulation is restricted to sensory neurons (sources
  S = sensory ascending, sensory descending, sensory) and effector
  neurons (targets T = motor, visceral circulatory, ascending visceral
  circulatory). Used to rank neurons that act as bottlenecks for
  sensory→motor routing. Fig. 3a.

Each flavor is computed at two synapse-prediction snapshots:

- **`_v2`** — v2 synapse edgelist at count >= 5 (paper). Use this to
  reproduce the figures.
- **`_v3`** — v3 synapse edgelist at count >= 10. Provided for
  comparison; results are very close but not identical.

## Provenance

Computed by **bancpipeline** at `banc/betweenness/` from the
intrinsic-neuron edgelist:

1. Build a directed graph G = (V, E) over the same neuron pool used in
   the CNS-network spectral clustering (intrinsic central brain + VNC
   neurons + ANs + DNs + visual projection + visual centrifugal), with
   directed edges where the connection has count >= 5 (v2) or >= 10
   (v3).
2. Compute unnormalized, unweighted, directed betweenness centrality
   using the Brandes algorithm[Brandes 2001] as implemented in the
   Python interface of igraph.
3. For the afferent-to-efferent variant, restrict the
   source-target accumulation in the Brandes inner loop to (S, T)
   pairs.

Paper Methods section "Betweenness centrality" gives the formal
definition and the citations.

## Schema

| column | dtype | description |
|---|---|---|
| `vertex_id` | int | 0-indexed vertex ID within the igraph object used for the computation. Not stable across reruns. |
| `root_888` | string | BANC v888 root ID of the neuron. |
| `super_class` | string | super_class from `banc_888_meta.feather`. |
| `cell_type` | string | cell_type from `banc_888_meta.feather`. |
| `betweenness` | double | Brandes betweenness centrality. Sum over every shortest-path-pair fraction (or every (S, T) afferent-to-efferent pair for the afferent-to-efferent variant). |

## Usage

```r
library(readr); library(dplyr)
bw <- read_csv("banc_888_betweenness_all_to_all_v2.csv")
bw %>% arrange(desc(betweenness)) %>% head(50)
```

```python
import pandas as pd
bw = pd.read_csv("banc_888_betweenness_afferent_to_efferent_v2.csv")
bw.sort_values("betweenness", ascending=False).head(50)
```

## Related files

- `banc_888_meta.feather` — joins on `root_888` for richer per-neuron
  context.
- `banc_888_edgelist_simple_v2.feather` / `_v3.feather` — the edgelist
  the betweenness pipeline consumes.
- `banc_888_cns_network_spectral_clustering_v2.csv` (and `_v3`) — the
  same neuron pool is used for spectral clustering; betweenness ranks
  candidate hubs within those networks.
- `bancpipeline_archive.zip` — `banc/betweenness/` contains the
  implementation.

## Notes

- Betweenness values are **unnormalized**. Divide by `(N-1)(N-2)/2` or
  `|S| * |T|` for the unfiltered and afferent-to-efferent variants
  respectively to obtain normalized centralities; magnitudes are
  comparable across variants only after normalization.
- `vertex_id` is 0-indexed and **not stable across reruns** — only
  `root_888` is a stable join key.
- The all-to-all computation is expensive (the unfiltered BANC graph
  is ~60 k nodes); the deposited CSVs are the cached outputs from a
  single 8-hour run on the Wilson lab cluster.
