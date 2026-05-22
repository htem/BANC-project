# `settings/` — shared configuration and palettes

Small reference files loaded at session startup by
[`R/startup/banc-startup.R`](../R/startup/banc-startup.R). One row per
file:

| File | What it is |
| --- | --- |
| [`paper_colours_lacroix.csv`](paper_colours_lacroix.csv) | The canonical colour palette for every figure in the paper. One row per `label` (a body part, super_class, super_cluster, effector group, …); columns: `label`, `hex`, `type`, `colour` (the LaCroix-family colour name). Loaded as `paper.cols` (a named character vector mapping label → hex). Used by 80+ panel scripts; *the* single source of truth for figure colours. |
| [`meta_hierarchical_policy.csv`](meta_hierarchical_policy.csv) | The annotation-taxonomy hierarchy policy: one row per (`flow`, `super_class`, `cell_class`, `cell_sub_class`, `cell_function`, `cell_function_detailed`) tuple, with a `count` of how many neurons fall into that combination. Used as a quick reference for which combinations are populated vs empty, and as the policy table for the SeaTable hierarchy enforcement (`bancpipeline/banc/annotations/`). |
| [`LaCroix.png`](LaCroix.png) | The palette swatch image the `paper_colours_lacroix.csv` hex codes were sampled from. Reference only — not loaded by code. |
| [`README.md`](README.md) | This file. |

## Why a separate directory

`settings/` holds **reference data that defines the paper's house style**
(palette, taxonomy policy). It is conceptually distinct from `data/`,
which holds **analysis artefacts** (per-neuron metrics, cluster
assignments, edgelists). Both are loaded at startup, but only this
directory should change to alter how the figures look without changing
the underlying analysis.

## Editing the palette

If you change `paper_colours_lacroix.csv`, every figure that consumes
`paper.cols` will pick up the new colour on its next run. Add new labels
(super clusters, effector groups) as a new row; don't rename existing
labels in place — every figure script keys on the label string.
