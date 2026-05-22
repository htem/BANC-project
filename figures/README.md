# `figures/` — paper figure artefacts

Per-figure illustrator-ready PDFs, per-figure Neuroglancer-state PNGs,
statistical sidecar `.txt` files, and the `.ai` files that compose them
into the final paper figures. **Generated** by the panel scripts in
[`../R/figures/`](../R/figures/); **composed** in Adobe Illustrator from
the per-panel PDFs.

## Layout

Every main figure has its own subdirectory:

```
figure_N/
├── figure_N.ai                       # Illustrator master (links into links/)
├── figure_N.png                      # rasterised export of the master
├── extended_data_figure_*.ai         # ED figure masters (one per ED panel group)
├── extended_data_figure_*.png        # rasterised ED exports
└── links/                            # per-panel outputs
    ├── *.pdf                         # main-figure panels (vector)
    ├── *.txt                         # paired statistical sidecars
    ├── supplement/                   # ED-figure panels + their sidecars
    ├── extra/                        # exploratory / threshold-sweep outputs (not in paper)
    └── neuroanatomy/                 # 3D mesh renderings — ALWAYS exempt from move rules
```

The `links/neuroanatomy/` subtree is the canonical exception: both the
main and ED `.ai` files are allowed to reference assets in place there.

## Figure → script index

The script names describe the analysis, not the figure number.
For the authoritative table see
[`../R/figures/README.md`](../R/figures/README.md); summarised here:

| Figure | Title | Producing scripts (in `R/figures/`) |
| --- | --- | --- |
| **1** | A brain-and-nerve-cord connectome | `panels_inventory.R`, `panels_neuroanatomy.R`, `panels_proofread_matching.R`, `panels_synapse_review.R`, `panels_transmitter_predictions.R`, `panels_connectivity_comparison.R`, `panels_annotation_hierarchy.R` |
| **2** | Linking sensors and effectors through local and long-range circuits | `panels_sensory_motor.R`, `panels_influence_validation.R`, `panels_body_parts.R`, `panels_pre_effector_influence.R`, `panels_an_dn_polarity.R` |
| **3** | Clustering ANs and DNs into behaviour-centric modules | `panels_betweenness_layers.R`, `panels_an_dn_umap.R`, `panels_an_dn_connectivity.R`, `panels_an_dn_influence.R`, `panels_efferent_umap.R`, `panels_vignette_networks.R` |
| **4** | Specialisations and coordination within a functional cluster | `panels_cluster_sensory_correlations.R`, `panels_cell_type_blowouts.R`, `panels_example_neuroanatomies.R`, `panels_vignette_networks.R`, `panels_bias.R` |
| **5** | Interactions between behaviour-centric modules | `panels_an_dn_influence.R`, `panels_vignette_networks.R` |
| **6** | Linking CNS networks with AN and DN clusters | `panels_cns_networks.R`, `panels_cns_network_analyses.R`, `panels_cns_network_diagram.R`, `panels_mbx_cx_control.R`, `panels_vignette_networks.R` |
| **ED 1–10** | Extended Data figures | written by the same scripts above to `links/supplement/` (the `panels_*.R` header in each script names which ED panels it produces) |

## Reproducing a figure

```bash
# whole paper, top-down driver:
./manuscript/scripts/regenerate_paper.sh

# or one panel script at a time (the usual pattern when iterating):
BANC_NCORES=1 Rscript R/figures/panels_betweenness_layers.R
```

Every script begins with the same boilerplate (`source banc-startup`,
`source banc-meta`, `source banc-edgelist` if needed), then writes its
PDF + sidecar text outputs into `links/` (main panels) or
`links/supplement/` (ED panels). Set `BANC_LIVE=1` to force a fresh
SeaTable + GCS pull instead of the committed snapshot.

## File formats

| Extension | What it is |
| --- | --- |
| `.ai`       | Adobe Illustrator master files. References (`links/...`) the per-panel PDFs by **relative path** — moving panels breaks the master unless paths are rewritten. The `manuscript/scripts/fix_ai_paths.py` utility patches paths inside `.ai` files safely. |
| `.pdf`      | Per-panel vector outputs (the unit the `.ai` masters link to). |
| `.png`      | Rasterised exports of `.ai` masters (for quick previewing) and 3D mesh renderings (under `links/neuroanatomy/`). |
| `.txt`      | Per-panel statistical sidecars (Kruskal–Wallis + Dunn pairwise tables, regression summaries, paired Wilcoxon results). One sidecar per significance test in the panel. |
| `.csv`      | Per-panel data exports (rare — most data lives in [`../data/`](../data/) or upstream). |

## Companion documentation

- [`../R/figures/README.md`](../R/figures/README.md) — figure → script
  index, conventions for `panels_*.R`, the `BANC_NCORES` env-var
  override.
- [`../data/README.md`](../data/README.md) — where the underlying data
  comes from (`bancpipeline` outputs, snapshots, external references).
- Each `panels_*.R` script's top-of-file roxygen2 header lists the
  exact paper panels it produces (`@section Paper:` block).

## Colours

All figures use the canonical LaCroix-derived palette in
[`../settings/paper_colours_lacroix.csv`](../settings/paper_colours_lacroix.csv).
Edit there if you need to change a figure colour; every panel will
pick up the new colour on its next run.
