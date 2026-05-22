---
filename: codex_annotations.parquet
gcs_path: gs://lee-lab_brain-and-nerve-cord-fly-connectome/neuron_annotations/v888/codex_annotations.parquet
size_bytes: 74350281
size_human: 70.91 MB
nrows: 1841078
ncols: 12
content_type: application/x-parquet

# --- Fields posted to Dataverse ---
description: >-
  Master CAVE annotation table at materialization v888 — the canonical
  curated source of BANC's cell-type taxonomy and the table that drives
  the FlyWire Codex interface. Each row is one curator-vetted label
  (`cell_type`) attached to one neuron (`target_id`) under one
  `classification_system` (e.g. `super_class`, `cell_class`,
  `cell_sub_class`, `cell_type`, `hemilineage`, `region`, `side`,
  `nerve`, `neuromere`, `flow`). 1 841 078 rows × 12 columns; one neuron
  typically carries several rows, one per taxonomy level. This is the
  upstream source for almost every identity column in
  `banc_888_meta.feather`, and the right file to consume when you want
  CAVE's native long-form structure with full per-row provenance
  (`created`, `valid`, supersession lineage), when you want to filter by
  `classification_system` before pivoting, or when you want access to
  historical labels that have since been replaced. Within the CAVE-table
  layer this file is analogous in importance to `banc_888_meta.feather`
  at the compiled layer — both define what a neuron is in BANC. Most
  downstream users should consume the meta and reach for this file only
  when the long-form view or per-row history is needed.
categories:
  - Annotations
  - Data
directoryLabel: annotations/v888
restrict: false
tabIngest: false
---

# codex_annotations.parquet

## Purpose

`codex_annotations.parquet` is the **master curated annotation table**
for BANC at v888. It is the table that the FlyWire Codex interface reads
from when it displays cell-type, super-class, hemilineage and related
fields for a BANC neuron, and it is the upstream source for almost every
identity column in the consolidated `banc_888_meta.feather`. Within the
CAVE-table layer it is analogous in importance to
`banc_888_meta.feather` at the compiled layer — both define what a
neuron *is* in BANC.

The table is **long-form**: each row asserts one label
(`cell_type`) on one neuron (`target_id`) under one
`classification_system`. The classification systems correspond to the
levels of the BANC annotation taxonomy (paper Methods, "Annotation
taxonomy"): `super_class`, `cell_class`, `cell_sub_class`, `cell_type`,
`hemilineage`, `region`, `side`, `nerve`, `neuromere`, `flow`, and the
verified neurochemistry fields. A typical neuron therefore carries
several rows in this file — one for each level at which it has been
classified.

To recover the flat per-neuron view, pivot wide on `classification_system`
with `cell_type` as the value column, joining `target_id` → `id` on
`cell_representative_point.parquet` to get a stable per-neuron join key.
The bancr helper `banc_codex_annotations()` does this pivot for you.

## Provenance

Curated by the core BANC team across the v626 → v888 materialization
sequence; written directly into CAVE annotation tables. Cell-type labels
are inherited from FAFB (for brain neurons and DNs) and MANC (for VNC
neurons and ANs), with a small number of exceptions where types were
further split to define single cell types — see paper Methods,
"Cell-type matching and annotation". The other classification systems
(`super_class`, `region`, `hemilineage`, …) follow the controlled
vocabulary documented in the BANC annotation taxonomy reference
(`supplemental_data_1.csv`).

**CAVE description (verbatim)** (created 2025-07-16, voxel resolution 1 × 1 × 1 (placeholder) nm):

> Testing of new annotations. This table will update the "target_id" foreign_key when updates are made to the "cell_representative_point" table.
> 
> (The CAVE table description is currently a placeholder; the table is in active rollout. Use the schema and notes in this file as the authoritative reference.)

This file is the snapshot of the CAVE table at the v888 materialization.
Rows with `valid_ref = FALSE` or `valid = FALSE` are historical and were
superseded by later edits.

## Schema

The two pairs of `*_ref` columns refer to the annotation **target** (the
representative-point row in `cell_representative_point.parquet`), while
the un-suffixed `id`, `created`, `valid` columns refer to **this**
annotation row.

| column | dtype | description |
|---|---|---|
| `id_ref` | int64 | `id` of the representative-point row in `cell_representative_point.parquet` that this annotation is attached to. |
| `created_ref` | timestamp[us, UTC] | Creation time of the referenced representative-point row. |
| `valid_ref` | bool | `TRUE` if the referenced representative-point row is currently active. |
| `pt_supervoxel_id` | int64 | Supervoxel containing the representative point. |
| `pt_root_id` | int64 | Root identifier of the annotated neuron at the v888 materialization. |
| `id` | int64 | CAVE annotation identifier of this annotation row. |
| `created` | timestamp[us, UTC] | Wall-clock time this annotation was created. |
| `valid` | bool | `TRUE` if this annotation row is currently active in CAVE. |
| `target_id` | int64 | `id` of the per-neuron representative point that this row classifies; the join key to `cell_representative_point.parquet`. |
| `classification_system` | string | Taxonomy level being asserted: `super_class`, `cell_class`, `cell_sub_class`, `cell_type`, `hemilineage`, `region`, `side`, `nerve`, `neuromere`, `flow`, `neurotransmitter_verified`, `neuropeptide_verified` (among others). |
| `cell_type` | string | The label value within `classification_system` — despite the column name, this carries whatever value applies to the level (e.g. for a row with `classification_system = "super_class"`, this column holds `"descending"`). |
| `pt_position` | list<int64> | Representative point in BANC voxel space, as a 3-element list `[x, y, z]`. |

## Usage

In R, the recommended path is `bancr::banc_codex_annotations()`, which
reads from a local cache mirroring this file and returns the wide
per-neuron form. Direct consumption:

```r
library(arrow); library(dplyr); library(tidyr)
ca <- read_parquet("codex_annotations.parquet") %>% filter(valid_ref, valid)

# top labels per classification_system
ca %>% count(classification_system, sort = TRUE)

# wide per-neuron table
ca_wide <- ca %>%
  group_by(target_id, pt_root_id, classification_system) %>%
  summarise(cell_type = paste(unique(cell_type), collapse = ", "),
            .groups = "drop") %>%
  pivot_wider(names_from = classification_system, values_from = cell_type)
```

In Python via pyarrow:

```python
import pyarrow.dataset as ds
ca = ds.dataset("codex_annotations.parquet").to_table(
    filter=(ds.field("valid_ref") == True) & (ds.field("valid") == True)
).to_pandas()
# wide per-neuron
flat = (ca.groupby(["pt_root_id", "classification_system"])["cell_type"]
          .agg(lambda s: ", ".join(sorted(set(s.dropna()))))
          .unstack("classification_system"))
```

## Related files

- `cell_representative_point.parquet` — the per-neuron representative
  point table; joins on `target_id` → `id` and is required to recover
  the per-neuron pivot.
- `banc_888_meta.feather` — downstream consolidated per-neuron table
  built on top of this file plus BANC SeaTable curations and the
  cross-dataset matching pipelines. Most users should consume the meta
  rather than this file directly.
- Sibling CAVE annotation tables in this deposit:
  `cell_info.parquet` (informal community labels, not curator-vetted),
  `backbone_proofread.parquet` (backbone-proofread flag),
  `proofreading_notes.parquet` (proofreader notes including the
  roughly-proofread tag), `somas_v1.parquet` (detected nuclei),
  `peripheral_nerves.parquet` (nerve-entry / -exit seeds),
  `neck_connective_y92500.parquet` (neck-plane seeds).
- BANC annotation taxonomy reference (`supplemental_data_1.csv`,
  bundled in `banc_supplemental_data.zip`) — controlled vocabulary for
  every `classification_system` value.

## Notes

- **Long-form, not flat.** One neuron generates several rows — one per
  taxonomy level. Always pivot before joining to a per-neuron table.
- **Two `valid` flags.** `valid_ref` refers to the per-neuron
  representative-point lineage; `valid` refers to this annotation row.
  For active analysis, filter on both.
- **`cell_type` column is overloaded.** Within CAVE this column carries
  the label for *every* classification system (it is the value column in
  a name/value pair). For a row with `classification_system =
  "hemilineage"`, the `cell_type` column will contain a hemilineage
  string like `"ALad1"`, not a cell-type string.
- The full set of `classification_system` values follows the BANC
  annotation taxonomy controlled vocabulary (paper Methods, "Annotation
  taxonomy"). Treat any value not in that vocabulary as a curator
  in-progress label.
- The optic-lobe coverage of `cell_type` is asymmetric between
  hemispheres — the right optic lobe is more thoroughly cell-typed than
  the left (see `banc_888_meta.feather` notes).
- Where a neuron has been re-classified between materializations, the
  earlier rows survive as `valid = FALSE` for audit. Treat them as
  history, not as alternative labels.
