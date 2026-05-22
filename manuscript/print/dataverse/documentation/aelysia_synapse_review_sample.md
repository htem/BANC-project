---
filename: 2024-09-20_aelysia_synapse_sample_complete.csv
local_path: /Users/papers/BANC-project/data/synapses/2024-09-20_aelysia_synapse_sample_complete.csv
size_bytes: 342927
size_human: 343 KB
nrows: 6029
ncols: 6
content_type: text/csv

# --- Fields posted to Dataverse ---
description: >-
  Manual review of a balanced sample of 6,029 v2 synapse detections by
  the Aelysia annotation team, used to evaluate the false-positive and
  false-negative behavior of the BANC synapse detector across the
  range of postsynapse sizes. Synapses were stratified by region and
  by postsynapse size, reviewed in Neuroglancer and labeled `True`,
  `False`, or `Ambiguous`. The review identified a sharp drop in
  detection quality below postsynapse size 5, which motivated the
  paper-wide `size >= 5` threshold for v2 (Methods, "Synapse
  detection evaluation"). This file is the source of the `label`
  column in banc_888_synapses_v2_enriched.parquet for the rows it
  covers.
categories:
  - Data
  - Synapses
  - Documentation
directoryLabel: synapses/manual_review
restrict: false
tabIngest: false
---

# 2024-09-20_aelysia_synapse_sample_complete.csv

## Purpose

This file is the **Aelysia manual-review synapse sample**: 6,029 v2
synapse detections, stratified by region (`midbrain`, `vnc`, `nerve`,
`optic`, `outside`) and by postsynapse size, each reviewed by human
annotators and labeled `True`, `False`, or `Ambiguous`. The sample is
the basis for the paper's sparse evaluation of the v2 synapse
detector — complementing the dense per-cutout ground-truth used to
compute precision and recall — and is the empirical evidence behind
the `size >= 5` threshold the paper adopts for all v2 connectivity
and neurochemistry analyses. Pooled across bins, the review found
that detections with size below 5 are predominantly false, whereas
detections at size 5 or above are predominantly correct — see the
paper Methods section "Synapse detection evaluation" for the
headline numbers.

## Provenance

Annotated by the **Aelysia LTD** (Bristol, UK) commercial annotation
team during 2024, with the export snapshotted on 2024-09-20. The
review workflow loaded each candidate synapse in Neuroglancer at its
detected coordinate and asked the annotator to mark it true, false or
ambiguous on inspection of the surrounding EM. The detection set is
the v2 CAVE synapse export at the time of review.

**This sample is NOT joined into the synapse parquet.** The `label`
column of `banc_888_synapses_v2_enriched.parquet` is the
**compartment code** from the flow-centrality split (0 = unknown,
1 = soma, 2 = axon, 3 = dendrite, 4 = primary.dendrite,
7 = primary.neurite), not a manual-review flag. Users who want
manual-review labels alongside the synapse table should join this
CSV by synapse `id` themselves.

The paper reports the headline true / false / ambiguous numbers from
this review in Methods, "Synapse detection evaluation"; the underlying
CSV is provided here for transparency and so that users can re-derive
the size-threshold decision under different assumptions.

## Schema

The file is a six-column CSV. Column dtypes given are the natural types on read; `id` is best read as a string to preserve precision (see Notes).

| column | dtype | description |
|---|---|---|
| `id` | string / int64 | Synapse identifier from the CAVE v2 synapse table. Joins to `id` in `banc_888_synapses_v2_enriched.parquet`. |
| `Coordinate 1` | string | Synapse coordinate in BANC voxel space, formatted as `"(x, y, z)"`. |
| `Tags` | string | Annotator label. Levels: `True` (correct detection), `False` (false positive), `Ambiguous`. Capitalisation follows the Neuroglancer review interface. |
| `neuropil` | string | Neuropil or nerve at the synapse location (for example `AMMC_L`, `IPS_R`, `LNp_T3_L`, `ADMN(L)`, `DProN(R)`). For sampled sites lying in peripheral nerves the nerve name appears here. Used as a stratification axis when drawing the sample. |
| `size` | int32 | Postsynapse size in voxels — the attribute the size threshold is set on. |
| `region` | string | Coarse region of the sampled site. Observed values: `midbrain`, `vnc`, `optic`, `nerve` (peripheral nerves), `outside`. Note this vocabulary differs from the `region` field in `synapse_neuropil_lookup_v2.parquet` — these review-time labels were chosen by the annotation team. |

## Usage

The file is small enough to read end-to-end. From R:

```r
library(readr)
rv <- read_csv("2024-09-20_aelysia_synapse_sample_complete.csv",
               col_types = cols(id = col_character()))
table(rv$Tags, rv$size < 5)
```

From Python:

```python
import pandas as pd
rv = pd.read_csv(
    "2024-09-20_aelysia_synapse_sample_complete.csv",
    dtype={"id": "string"},
)
rv.groupby(["Tags", rv["size"] < 5]).size()
```

To rebuild the `label` column of the v2 enriched table, map `Tags ==
"True"` to 1, `Tags == "False"` to 0, and `Tags == "Ambiguous"` to
`NA`, then left-join on `id`.

## Related files

- `banc_888_synapses_v2_enriched.parquet` — the master v2 synapse
  table, where the labels in this CSV are exposed as the `label`
  column.
- `banc_888_synapses_v3_enriched.parquet` — the v3 counterpart; the
  v3 detector and its higher `size >= 10` threshold were not
  validated against this same sample, so the v3 table does not carry
  a `label` column.
- `synapse_neuropil_lookup_v2.parquet` — the v2 spatial join that
  underlies the `neuropil` column in the enriched table; lookup
  `region` is a different vocabulary from the `region` column in this
  review CSV.
- The paper's Methods section "Synapse detection evaluation" gives
  the headline true / false / ambiguous proportions above and below
  size 5, and reports the dense per-cutout precision and recall used
  alongside this sparse sample.

## Notes

- **Read `id` as a string** to avoid `int64` precision loss in
  language runtimes that demote large integers to floats (notably
  pandas before nullable `Int64`).
- **`size < 5` is the actionable cut.** The paper-wide threshold is
  `size >= 5`; this sample is what justifies that cut. See the paper
  Methods section "Synapse detection evaluation" for the headline
  true / false / ambiguous proportions either side of the cut.
- **`Tags` capitalisation** (`True` / `False` / `Ambiguous`) is
  preserved as exported by the review tool; normalise with
  `tolower()` if you join against case-sensitive downstream labels.
- **Tag counts** in this snapshot: 4,251 `True`, 1,515 `False`, 263
  `Ambiguous` (6,029 total).
- **Sample stratification.** The sample was drawn to balance region
  and size, with smaller size bins (where false positives concentrate)
  intentionally over-represented. Per-bin confidence intervals are
  correspondingly wide; the headline numbers are pooled across bins.
- This is one of two synapse-review resources from the project; the
  other is the dense per-cutout ground-truth that produces the
  F-score, precision and recall reported in the paper. The dense set
  is not deposited as a separate file.
