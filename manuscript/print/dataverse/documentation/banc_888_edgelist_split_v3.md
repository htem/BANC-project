---
filename: banc_888_edgelist_split_v3.feather
gcs_path: gs://lee-lab_brain-and-nerve-cord-fly-connectome/compiled_data/banc_888/banc_888_edgelist_split_v3.feather
size_bytes: 907872778
size_human: 866 MB
content_type: application/x-arrow

# --- Fields posted to Dataverse ---
description: >-
  Compartment-to-compartment edgelist for the BANC connectome at
  materialization v888, rolled up from the v3 synapse set with the
  flow-centrality axon/dendrite split applied to both ends of every
  synapse. One row per directed `(pre, post, pre_label, post_label)`
  combination. Compartment labels come from the per-neuron split CSVs
  and take values `axon`, `dendrite`, `primary.dendrite`,
  `primary.neurite`, or `unknown`. Where a neuron's reconstruction is
  truncated at the CNS boundary, the side facing the CNS is forced:
  afferent neurons are forced to `axon` on the pre side and `axon` on
  the post side; efferent neurons are forced to `dendrite` on the pre
  side and `dendrite` on the post side. Each row carries the per-neuron
  predicted neurotransmitter and confidence on both ends, so connections
  can be filtered or colored by NT without joining onto the per-neuron
  NT table. Compartment-aware companion to
  `banc_888_edgelist_simple_v3.feather`; the v3 counterpart of
  `banc_888_edgelist_split_v2.feather`, built against the v3 synapse
  detection model.
categories:
  - Data
  - Connectivity
directoryLabel: compiled_data
restrict: false
tabIngest: false
---

# banc_888_edgelist_split_v3.feather

## Purpose

`banc_888_edgelist_split_v3.feather` is the compartment-resolved
neuron-to-neuron edgelist for BANC at materialization v888, built
against the v3 synapse-prediction model. Where
`banc_888_edgelist_simple_v3.feather` gives one row per directed
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

Built by **bancpipeline** (`banc/meta/banc-data.R`), the v3 counterpart
of the v2 build documented in `banc_888_edgelist_split_v2.md`. The
script:

1. Reads per-neuron synapse CSVs from the L2 flow-centrality split outputs
   (`banc.l2split.save.path/synapses/`), now keyed against the v3
   detection set rather than v2.
2. Applies the v3 size threshold (`size >= 10` voxels; the simple v3
   edgelist uses the same threshold, see
   `banc_888_edgelist_simple_v3.md`), drops autapses, and drops rows
   where neither end is in the v888 neuron set.
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
7. Joins the v3 per-neuron NT prediction onto both ends to populate
   `pre_conf_nt` / `pre_conf_nt_p` / `post_conf_nt` / `post_conf_nt_p`
   (set to `"unknown"` / `NA` where no prediction).

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
sp <- read_feather("banc_888_edgelist_split_v3.feather")
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
sp = feather.read_table("banc_888_edgelist_split_v3.feather").to_pandas()
axo_axonic = sp[(sp["pre_label"] == "axon") & (sp["post_label"] == "axon")]
```

## Related files

- `banc_888_edgelist_simple_v3.feather` — the un-split version of this
  edgelist, same v3 synapse source, same `size >= 10` cutoff. `count`
  here, summed over compartment combinations for a given `(pre, post)`,
  agrees with `count` there.
- `banc_888_synapses_v3_enriched.parquet` — the per-synapse table this
  edgelist is rolled up from.
- `banc_888_edgelist_split_v2.feather` — the v2 counterpart of this
  file. The compartment split itself is the same flow-centrality
  pipeline applied to a different synapse detection set.
- `banc_888_meta.feather` — per-neuron metadata, used to identify
  afferent and efferent neurons whose compartment labels are forced
  (`flow == "afferent"` or `flow == "efferent"`).
