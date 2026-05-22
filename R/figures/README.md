# R/figures/ — figure-panel scripts

One `panels_*.R` script per figure or closely-related panel group. The
script name describes the analysis (e.g. `panels_betweenness_layers.R`),
not the figure number; the mapping is the table below.

Each script reads `banc.meta` (and where needed `banc.edgelist.simple`)
via `source("R/startup/banc-startup.R")` followed by
`source("R/startup/banc-meta.R")`, then writes its PDF and sidecar text
outputs to `figures/figure_N/links/` (main figures) or
`figures/figure_N/links/supplement/` (extended-data figures).

## Figure → script index

| Figure | Generator script(s) |
|---|---|
| 1  | `panels_inventory.R`, `panels_neuroanatomy.R`, `panels_proofread_matching.R`, `panels_synapse_review.R`, `panels_transmitter_predictions.R`, `panels_connectivity_comparison.R`, `panels_annotation_hierarchy.R` |
| 2  | `panels_sensory_motor.R`, `panels_influence_validation.R`, `panels_body_parts.R`, `panels_pre_effector_influence.R`, `panels_an_dn_polarity.R` |
| 3  | `panels_betweenness_layers.R`, `panels_an_dn_umap.R`, `panels_an_dn_connectivity.R`, `panels_an_dn_influence.R`, `panels_efferent_umap.R`, `panels_vignette_networks.R` |
| 4  | `panels_cluster_sensory_correlations.R`, `panels_cell_type_blowouts.R`, `panels_example_neuroanatomies.R`, `panels_vignette_networks.R`, `panels_bias.R` |
| 5  | `panels_an_dn_influence.R`, `panels_vignette_networks.R` |
| 6  | `panels_cns_networks.R`, `panels_cns_network_analyses.R`, `panels_cns_network_diagram.R`, `panels_mbx_cx_control.R`, `panels_vignette_networks.R` |
| ED | Each script writes its own `links/supplement/` panels in the same run; the `@section Paper:` block at the top of every `panels_*.R` lists the exact ED panels produced. |

## Conventions

- `panels_<plural>.R` for figure-panel scripts (single-file panels too —
  the script may produce several closely-related PDFs).
- `BANC_NCORES=1` for any script that hits the parallel influence
  calculator (memory predictability).
- One-off scripts whose output isn't a figure panel (e.g. quick stat
  re-runs) go in `R/text/` or `R/annotations/` rather than here.

## Companion archive

The two scripts that **write live** to SeaTable or rebuild the
interactive cluster tools used to live here. They have moved to
`R/annotations/`:
- `R/annotations/rebuild_interactive_tools.R`
- `R/annotations/make_annotation_terms_spreadsheet.R`
