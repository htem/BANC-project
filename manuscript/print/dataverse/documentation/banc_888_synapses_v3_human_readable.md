---
filename: banc_888_synapses_v3_human_readable.csv.gz
gcs_path: gs://lee-lab_brain-and-nerve-cord-fly-connectome/neuron_connectivity/v888/synapses_v3_human_readable.csv.gz
size_bytes: 15560927238
size_human: 14.5 GB
content_type: application/gzip

# --- Fields posted to Dataverse ---
description: >-
  Raw, gzipped CSV of every BANC v3 synapse — the pre-enrichment
  output of the upstream synapse-prediction pipeline, expanded into
  human-readable columns (synapse id, pre / post-synaptic root_id at
  v888, supervoxel ids, presynaptic and postsynaptic 3D positions in
  voxel space, synapse size in voxels). One row per predicted synaptic
  contact, autapses included. Use this file when you want the full
  raw table without the spatial / neuropil / NT enrichments applied
  for the published `banc_888_synapses_v3_enriched.parquet`. The two
  files share the same row set (modulo the autapse / null-root
  filtering applied at enrichment write time); the enriched parquet
  is the recommended consumer for new-work analyses; the v2 variants
  are what Bates, Phelps, Kim, Yang et al., 2026 use. This raw CSV is
  the recommended source if you want to redo the enrichment your own way.
categories:
  - Data
  - Synapses
directoryLabel: synapses/raw
restrict: false
tabIngest: false
---

# banc_888_synapses_v3_human_readable.csv.gz

## Purpose

`banc_888_synapses_v3_human_readable.csv.gz` is the **raw, gzipped CSV
of every BANC v3 synapse** at the v888 materialization — the
pre-enrichment output of the upstream synapse-prediction pipeline,
expanded into human-readable columns (synapse `id`, pre- /
post-synaptic `root_id` and `supervoxel_id`, presynaptic and
postsynaptic positions in voxel space, synapse `size` in voxels).
One row per predicted synaptic contact, autapses included.

Use this file when you want the full raw table without the spatial /
neuropil / neurotransmitter enrichments that the deposited
`banc_888_synapses_v3_enriched.parquet` ships with. The enriched
parquet drops autapses, filters to v888-member root_ids, joins the
NT-classifier output, and adds neuropil/region/side columns; if any
of those steps are not what you want, start from this raw file
instead.

## Provenance

Generated upstream by the BANC synapse-prediction pipeline, at the v3
prediction snapshot (updated detection model with a stricter
`size >= 10` cutoff). The CSV is the "human-readable" projection of
the CAVE annotation table `synapses_v3`. NT predictions for v3 live
in a separate parquet rather than being baked into this CSV (see
Related files); the row set here is the bare connectivity. The slim
parquet variant
`synapses_v3_human_readable_id_size_prerootid_postrootid_prex_prey_prez.parquet`
on GCS carries the same row set but only the join keys + pre-position;
bancpipeline pulls that variant when it only needs root-id pairs (see
`bancpipeline/tasks.md:255` and `update.md`).

**CAVE `synapses_v3` description (verbatim)** (created 2026-04-10, voxel
resolution `(16, 16, 45)` for the position list-columns):

> Version 3 of synapses. Still in testing.

(The v3 detection model picks up ~8 – 18 % more synapses overall than
v2 — but the higher `size ≥ 10` cutoff drops some small-but-real v2
detections. The paper figures use v2 throughout; v3 is the recommended
target for new work unless backward compatibility with the paper is
needed. The full v1.0 / v1.1 / v2.0 / v3.0 prediction-version history
is in `banc_888_synapses_v2_enriched.md` and `banc_888_synapses_v3_enriched.md`.)

## Schema

The underlying CAVE `synapses_v3` table has 12 columns, identical in
shape to `synapses_v2`. The human-readable CSV unpacks the three
position list-columns into x / y / z scalars. Confirm exact column
order on your copy with `gunzip -c synapses_v3_human_readable.csv.gz | head -2`.

| column | dtype | description |
|---|---|---|
| `id` | int64 | CAVE synapse identifier. |
| `created` | timestamp[us] | Annotation creation time. |
| `superceded_id` | int64 | Earlier-annotation `id` this row supersedes (empty otherwise). |
| `valid` | bool | `TRUE` if currently active in CAVE. |
| `size` | double | Synapse footprint in voxels (size ≥ 10 in the enriched parquet; the raw CSV may carry smaller-size rows from the upstream model). |
| `pre_pt_supervoxel_id` | int64 | Presynaptic supervoxel. |
| `pre_pt_root_id` | int64 | Presynaptic root_id at v888. |
| `post_pt_supervoxel_id` | int64 | Postsynaptic supervoxel. |
| `post_pt_root_id` | int64 | Postsynaptic root_id at v888. |
| `pre_pt_position` | list<int64> | Presynaptic centroid `[x, y, z]`; the human-readable CSV unpacks into `prex / prey / prez` scalars. |
| `post_pt_position` | list<int64> | Postsynaptic centroid. |
| `ctr_pt_position` | list<int64> | Midpoint between pre and post centroids. |

NT predictions for v3 are kept in a side parquet
(`banc_nt_prediction_v3_w_sizethresh_10_*.parquet` on GCS / Dataverse)
rather than baked into this CSV — join on synapse `id` if you want
them.

## Usage

11.4 GB gzipped — do not load whole-file into R memory. Either stream
with `arrow::open_csv_dataset` (with `lazy_eval`), or pre-filter via
`zcat` + `awk` / `head` before reading in R.

From Python via pyarrow (streaming filter):

```python
import pyarrow.csv as pcsv
import pyarrow.compute as pc
# Read in chunks and filter on pre_root_id without loading the full file
reader = pcsv.open_csv("synapses_v3_human_readable.csv.gz",
                       read_options=pcsv.ReadOptions(use_threads=True))
chunks = []
for batch in reader:
    mask = pc.equal(batch.column("pre_root_id"), "720575941521131930")
    chunks.append(batch.filter(mask))
```

## Related files

- `banc_888_synapses_v3_enriched.parquet` — the **derived** form (the
  one the paper analyses use): autapses dropped, NT classifier
  joined, neuropil / region / side added. Recommended starting point
  for paper-style analyses.
- `banc_888_synapses_v2_human_readable.csv.gz` — the v2 prediction
  counterpart of this file (size ≥ 5 cutoff).
- `banc_888_edgelist_simple_v2.feather` — neuron-to-neuron edgelist
  rolled up from the v3 synapse table at size ≥ 10.

## Notes

- **Autapses included.** Filter `pre_root_id != post_root_id` if you
  want to match the enriched parquet's row set.
- **Coordinates are in voxel space** (4 × 4 × 45 nm). Multiply by the
  voxel size to obtain nanometres.
- **Size 15.6 GB** — too large for `pandas.read_csv` on most laptops.
  Stream / chunk it.
- **`v2` is the synapse-prediction model version**, not the BANC
  materialization (which is v888 throughout the deposit).
