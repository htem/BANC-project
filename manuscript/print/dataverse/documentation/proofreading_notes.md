---
filename: proofreading_notes.parquet
gcs_path: gs://lee-lab_brain-and-nerve-cord-fly-connectome/neuron_annotations/v888/proofreading_notes.parquet
size_bytes: 857667
size_human: 837.57 KB
nrows: 21614
ncols: 9
content_type: application/x-parquet

# --- Fields posted to Dataverse ---
description: >-
  Per-neuron proofreader notes attached in CAVE at materialization v888.
  21 614 rows × 9 columns; each row is one note from one proofreader,
  pinned to a marker point on a neuron, with a free-text `tag` carrying
  the note content and a `user_id` identifying the proofreader. Used to
  record flagged issues (segmentation problems, uncertain branches,
  regions of known dataset artefact such as misalignment or data loss)
  and the "roughly proofread" status — a category for neurons that are
  identifiable across a large region of arbor but may have some
  omission, often because they lie in regions of known artefact (paper
  Methods, "Proofreading"). 5 214 neurons carry the roughly proofread
  status in the v888 snapshot. Download this file when you need the raw
  per-note granularity rather than the rolled-up boolean in
  `banc_888_meta.feather`, when investigating proofreading provenance
  for a specific neuron, or when cross-referencing notes against the
  locations of known dataset artefacts in Supplementary Data 8.
categories:
  - Annotations
  - Data
directoryLabel: annotations/v888
restrict: false
tabIngest: false
---

# proofreading_notes.parquet

## Purpose

`proofreading_notes.parquet` is the CAVE annotation table for **free-form
proofreader notes** on BANC neurons. Each row is one note from one
proofreader, pinned to a marker point on a neuron, with a `tag` that
carries the note content. The table serves two roles:

- **Flagging issues** — segmentation problems, uncertain branches, regions
  of known dataset artefact (misalignment, data loss), branches that are
  candidates for further proofreading.
- **Recording the `roughly_proofread` status** — a category for neurons
  that are identifiable across a large region of arbor but may have some
  omission, typically because they pass through known-artefact regions
  (paper Methods, "Proofreading"). 5 214 neurons carry this status in
  the v888 snapshot.

Downstream, `banc_888_meta.feather` rolls these notes up into the
boolean `roughly_proofread` column and other proofreading flags; this
file is the source-of-truth for the per-note text and the proofreader
provenance.

## Provenance

Authored in CAVE by the BANC proofreading teams (Princeton, SixEleven,
Aelysia, individual labs and citizen scientists; 155 proofreaders in
total — paper Methods, "Proofreading"). Pulled from CAVE at the v888
materialization snapshot and deposited here in native point-annotation
shape.

**CAVE description (verbatim)** (created 2023-10-18, voxel resolution 4 × 4 × 45 nm):

> Notes about how well proofread a neuron is. The "user_id" column contains the CAVE user ID for whoever wrote the note.

The locations of known dataset artefacts that frequently appear in these
notes are also distributed as Supplementary Data 8 of the paper.

## Schema

| column | dtype | description |
|---|---|---|
| `id` | int64 | CAVE annotation identifier. |
| `created` | timestamp[us, UTC] | Wall-clock time the note was created. |
| `superceded_id` | int64 | If this row replaces an earlier note, the `id` of the row it supersedes; `0` otherwise. |
| `valid` | bool | `TRUE` if the row is currently active in CAVE; `FALSE` for superseded rows. |
| `tag` | string | Free-text note content (e.g. `roughly_proofread`, `artefact_region`, `merger_candidate`, plus prose descriptions). Controlled vocabulary is not enforced. |
| `user_id` | int32 | CAVE user identifier of the proofreader who wrote the note. |
| `pt_supervoxel_id` | int64 | Supervoxel containing the marker point. |
| `pt_root_id` | int64 | Root identifier of the marked neuron at the v888 materialization. |
| `pt_position` | list<int64> | Marker point in BANC voxel space, as a 3-element list `[x, y, z]`. |

## Usage

In R via arrow:

```r
library(arrow); library(dplyr)
pn <- read_parquet("proofreading_notes.parquet") %>% filter(valid)

# roughly_proofread neurons via the tag
rp <- pn %>% filter(grepl("roughly_proofread", tag, ignore.case = TRUE))
length(unique(rp$pt_root_id))
```

In Python via pyarrow:

```python
import pyarrow.dataset as ds
pn = ds.dataset("proofreading_notes.parquet").to_table(
    filter=ds.field("valid") == True
).to_pandas()
pn[pn["tag"].str.contains("roughly_proofread", case=False, na=False)]
```

## Related files

- `banc_888_meta.feather` — downstream consolidated table; this file is
  the source of its `roughly_proofread` boolean and contributes to
  `status` and other proofreading-provenance columns.
- `backbone_proofread.parquet` — sibling annotation table for the
  stricter `backbone_proofread` status (primary neurites and major
  microtubule-rich processes have been reviewed).
- `codex_annotations.parquet` — master annotation table; join on
  `pt_root_id` to look up the curated cell-type labels for neurons that
  carry proofreading notes.
- Paper Supplementary Data 8 — locations of known dataset artefacts
  frequently referenced in these notes.

## Notes

- **Flat shape.** One row per marker point — unlike `codex_annotations.parquet`,
  which is long-form (one row per `(target_id, classification_system)`).
- A given neuron can carry several notes from different proofreaders at
  different time points. Group by `pt_root_id` rather than treating each
  row as canonical.
- The `tag` field is free text; expect prose, abbreviations, lab-internal
  shorthand and a small number of controlled strings (notably
  `roughly_proofread`).
- The boolean `roughly_proofread` column in `banc_888_meta.feather`
  rolls this table up to per-neuron granularity; for a yes/no answer at
  the neuron level, consume the meta. For provenance and full note text,
  consume this file.
- Rows with `valid = FALSE` are historical and were superseded by later
  edits; filter to `valid = TRUE` for active work.
