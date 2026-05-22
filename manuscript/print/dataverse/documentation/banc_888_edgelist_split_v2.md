---
filename: banc_888_edgelist_split_v2.feather
gcs_path: gs://lee-lab_brain-and-nerve-cord-fly-connectome/compiled_data/banc_888/banc_888_edgelist_split_v2.feather
size_bytes: 355937714
size_human: 339.4 MB
nrows: 6309984
ncols: 13
content_type: application/x-arrow

# --- Fields posted to Dataverse ---
description: >-
  Compartment-to-compartment edgelist for the BANC connectome at
  materialization v888, rolled up from the v2 synapse set with the
  flow-centrality axon/dendrite split applied to both ends of every
  synapse. One row per directed `(pre, post, pre_label, post_label)`
  combination; 6 309 984 rows × 13 columns. Compartment labels come from
  the per-neuron split CSVs and take values `axon`, `dendrite`,
  `primary.dendrite`, `primary.neurite`, or `unknown`. Where a neuron's
  reconstruction is truncated at the CNS boundary, the side facing the
  CNS is forced: afferent neurons are forced to `axon` on the pre side
  and `axon` on the post side; efferent neurons are forced to `dendrite`
  on the pre side and `dendrite` on the post side. Each row carries the
  per-neuron predicted neurotransmitter and confidence on both ends, so
  connections can be filtered or colored by NT without joining onto the
  per-neuron NT table. Compartment-aware companion to
  `banc_888_edgelist_simple_v2.feather`.
categories:
  - Data
  - Connectivity
directoryLabel: compiled_data
restrict: false
tabIngest: false
---

# banc_888_edgelist_split_v2.feather

## Purpose

`banc_888_edgelist_split_v2.feather` is the compartment-resolved
neuron-to-neuron edgelist for BANC at materialization v888. Where
`banc_888_edgelist_simple_v2.feather` gives one row per directed
neuron pair, this file splits each pair into the compartment-to-compartment
sub-edges that make it up — axon → dendrite, axon → primary_neurite,
dendrite → soma, and so on — using the flow-centrality
axon/dendrite split assigned per neuron by the BANC l2split pipeline.

The edgelist is the natural starting point for analyses that need to
distinguish axo-dendritic from axo-axonic contacts, identify primary-
neurite synapses for filtering, or trace input onto soma compartments.
Each row also carries the per-neuron predicted neurotransmitter and its
confidence on both ends, so NT-colored compartment plots can be made
without an extra join.

## Provenance

Built by **bancpipeline** (`banc/meta/banc-data.R`, Section 5 at lines
642-764). The script:

1. Reads per-neuron synapse CSVs from the L2 flow-centrality split outputs
   (`banc.l2split.save.path/synapses/`).
2. Filters `size > banc.size.threshold` (`= 2` in `banc-data.R:53`, so
   effectively `size >= 3`), drops autapses, and drops rows where neither
   end is in the v888 neuron set.
3. Maps integer compartment codes to strings via
   `hemibrainr:::standard_compartments` (`0 = unknown`, `1 = soma`,
   `2 = axon`, `3 = dendrite`, `4 = primary.dendrite`,
   `7 = primary.neurite`).
4. Joins presynaptic compartment labels via `connector_id` lookup
   (each `connector_id` carries a label from the synapse it sits on).
5. Applies dataset-boundary overrides on both endpoints: where the
   neuron is afferent the corresponding label is forced to `axon`, and
   where it is efferent the corresponding label is forced to `dendrite`,
   because the flow-centrality split is unreliable on morphology that
   stops at the CNS boundary.
6. Groups by `(pre, post, pre_label, post_label)` and counts.
7. Joins the per-neuron NT prediction (`banc_ntpred.feather`) onto both
   ends to populate `pre_conf_nt` / `pre_conf_nt_p` / `post_conf_nt` /
   `post_conf_nt_p` (set to `"unknown"` / `NA` where no prediction).

A v3 counterpart does not currently exist — the flow-centrality split was
computed against the v2 synapse set and has not yet been re-projected
onto v3.

## Schema

| column | dtype | description |
|---|---|---|
| `post` | string | Postsynaptic neuron root ID at v888. Joins to `banc_888_meta$banc_888_id`. |
| `pre` | string | Presynaptic neuron root ID at v888. |
| `post_label` | string | Postsynaptic compartment. One of `axon`, `dendrite`, `primary.dendrite`, `primary.neurite`, `unknown`. Forced to `axon` if the post neuron is afferent and `dendrite` if it is efferent. |
| `pre_label` | string | Presynaptic compartment; same vocabulary. Forced to `axon` if the pre neuron is afferent and `dendrite` if it is efferent. |
| `count` | int32 | Synapse count for this compartment-to-compartment edge. |
| `post_count` | int32 | Total inputs to `post` across all compartments and all sources. |
| `norm` | double | `count / post_count` — the fraction of `post`'s total input contributed by this compartment-to-compartment sub-edge. Rounded to 4 significant figures. |
| `pre_count` | int32 | Total outputs of `pre` across all compartments and all targets. |
| `connection` | string | Compartment-pair label, equal to `paste(pre_label, post_label, sep="-")` (e.g. `axon-dendrite`, `axon-axon`, `unknown-dendrite`). |
| `post_conf_nt` | string | Per-neuron predicted neurotransmitter of `post`; `"unknown"` where no prediction exists. |
| `post_conf_nt_p` | double | Confidence of the post-side per-neuron NT prediction, `[0, 1]`. |
| `pre_conf_nt` | string | Per-neuron predicted neurotransmitter of `pre`; `"unknown"` where no prediction exists. |
| `pre_conf_nt_p` | double | Confidence of the pre-side per-neuron NT prediction. |

## Usage

In R via arrow:

```r
library(arrow); library(dplyr)
sp <- read_feather("banc_888_edgelist_split_v2.feather")
# Axo-axonic input onto a given neuron
sp %>% filter(post == "720575941521131930",
              pre_label == "axon", post_label == "axon") %>%
  arrange(desc(count))
```

To compare axo-dendritic vs axo-axonic share by source:

```r
sp %>% filter(connection %in% c("axon-dendrite", "axon-axon")) %>%
  group_by(pre, connection) %>% summarise(syn = sum(count), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = connection, values_from = syn, values_fill = 0)
```

In Python:

```python
import pyarrow.feather as feather
sp = feather.read_table("banc_888_edgelist_split_v2.feather").to_pandas()
axo_axonic = sp[(sp["pre_label"] == "axon") & (sp["post_label"] == "axon")]
```

## Related files

- `banc_888_edgelist_simple_v2.feather` — the un-split version of this
  edgelist. `count` here, summed over compartment combinations for a
  given `(pre, post)`, agrees with `count` there up to the size-threshold
  difference (`size > 2` here, `size >= 5` in the simple edgelist).
- `banc_888_synapses_v2_enriched.parquet` — the per-synapse table this
  edgelist is rolled up from.
- `banc_888_neurotransmitter_prediction_v2.csv` — the per-neuron NT
  prediction that supplies the four `*_conf_nt` columns.
- `banc_888_meta.feather` — per-neuron metadata, used to identify
  afferent and efferent neurons whose compartment labels are forced
  (`flow == "afferent"` or `flow == "efferent"`).

## Notes

- **Size threshold differs from the simple edgelist.** This file uses
  `size > 2` (rolled up from the flow-centrality split CSVs);
  `banc_888_edgelist_simple_v2.feather` uses `size >= 5`. Sums of `count`
  across compartments for a given `(pre, post)` will exceed the
  corresponding `count` in the simple edgelist when small synapses are
  involved. To recover the simple-edgelist convention, filter on
  `count >= 5` per row before grouping (an approximation; the simple
  edgelist applies the threshold at the synapse level, not the edge
  level).
- **`pre_label` and `post_label` are constrained for afferent and
  efferent neurons.** Their flow-centrality splits are unreliable because
  their reconstructions stop at the CNS boundary; the script forces
  afferent endpoints to `axon` and efferent endpoints to `dendrite` by
  construction.
- The `*_conf_nt` columns are **per-neuron** predictions, not
  per-synapse. For per-synapse NT calls, use the `syn_top_nt` column in
  `banc_888_synapses_v2_enriched.parquet`.
- No `_v3` compartment edgelist is currently shipped — the
  flow-centrality split CSVs are v2-keyed.
