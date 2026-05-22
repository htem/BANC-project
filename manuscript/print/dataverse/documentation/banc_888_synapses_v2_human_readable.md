---
filename: banc_888_synapses_v2_human_readable.csv.gz
gcs_path: gs://lee-lab_brain-and-nerve-cord-fly-connectome/neuron_connectivity/v888/synapses_v2_human_readable.csv.gz
size_bytes: 12257440602
size_human: 11.4 GB
content_type: application/gzip

# --- Fields posted to Dataverse ---
description: >-
  Raw, gzipped CSV of every BANC v2 synapse — the pre-enrichment
  output of the upstream synapse-prediction pipeline, expanded into
  human-readable columns (synapse id, pre / post-synaptic root_id at
  v888, supervoxel ids, presynaptic and postsynaptic 3D positions in
  voxel space, synapse size in voxels). One row per predicted synaptic
  contact, autapses included. Use this file when you want the full
  raw table without the spatial / neuropil / NT enrichments applied
  for the published `banc_888_synapses_v2_enriched.parquet`. The two
  files share the same row set (modulo the autapse / null-root
  filtering applied at enrichment write time); the enriched parquet
  is the recommended consumer for analyses reproducing Bates, Phelps,
  Kim, Yang et al., 2026; this raw CSV is the recommended source if you
  want to redo the enrichment your own way.
categories:
  - Data
  - Synapses
directoryLabel: synapses/raw
restrict: false
tabIngest: false
---

# banc_888_synapses_v2_human_readable.csv.gz

## Purpose

`banc_888_synapses_v2_human_readable.csv.gz` is the **raw, gzipped CSV
of every BANC v2 synapse** at the v888 materialization — the
pre-enrichment output of the upstream synapse-prediction pipeline,
expanded into human-readable columns (synapse `id`, pre- /
post-synaptic `root_id` and `supervoxel_id`, presynaptic and
postsynaptic positions in voxel space, synapse `size` in voxels).
One row per predicted synaptic contact, autapses included.

Use this file when you want the full raw table without the spatial /
neuropil / neurotransmitter enrichments that the deposited
`banc_888_synapses_v2_enriched.parquet` ships with. The enriched
parquet drops autapses, filters to v888-member root_ids, joins the
NT-classifier output, and adds neuropil/region/side columns; if any
of those steps are not what you want, start from this raw file
instead.

## Provenance

Generated upstream by the BANC synapse-prediction pipeline, at the v2
prediction snapshot (2025-02-26 model). The CSV is the "human-readable"
projection of the underlying CAVE annotation table `synapses_v2`. The
gzipped CSV here also bakes in the v2 per-synapse neurotransmitter
predictions (see `banc-startup.R:236` in `bancpipeline`); the bare CAVE
table does not. `bancpipeline` uses this CSV as the canonical source of
v2 connectivity (see `update.md`: "`banc_888_edgelist_simple_v2.feather`
— built from `synapses_v2_human_readable.csv.gz`").

**CAVE `synapses_v2` description (verbatim)** (created 2025-08-14, voxel
resolution `(1, 1, 1)` nm — the synapse table is the only BANC CAVE
table that uses nanometre coordinates rather than voxels):

> Created from gs://zetta_lee_fly_cns_001_synapse/250226_assignment/assignment/final_edgelist.df.
>
> Information about synapse prediction, including descriptions of the columns in this table, can be found at https://banc.community/Automated-segmentation.
>
> Note that coordinates in this table are in nanometers (1, 1, 1 nm), unlike most other BANC CAVE tables which have coordinates in voxels (4, 4, 45 nm).

(The full v1.0 / v1.1 / v2.0 prediction-version history is in
`banc_888_synapses_v2_enriched.md`.)

## Schema

The underlying CAVE `synapses_v2` table has 12 columns; the
human-readable CSV unpacks the three position list-columns into x/y/z
scalars and adds the NT-classifier output. Confirm exact column order
on your copy with `gunzip -c synapses_v2_human_readable.csv.gz | head -2`.

Core columns (from CAVE):

| column | dtype | description |
|---|---|---|
| `id` | int64 | CAVE synapse identifier. |
| `created` | timestamp[us] | Wall-clock time the annotation was created. |
| `superceded_id` | int64 | If this row replaces an earlier annotation, the `id` of the row it supersedes; empty otherwise. |
| `valid` | bool | `TRUE` if currently active in CAVE; `FALSE` for superseded rows. |
| `size` | double | Synapse footprint in voxels (size ≥ 2 in the CAVE export; the published `banc_888_synapses_v2_enriched.parquet` applies a `size ≥ 5` threshold). |
| `pre_pt_supervoxel_id` | int64 | Supervoxel containing the presynaptic centroid. |
| `pre_pt_root_id` | int64 | Presynaptic root_id at v888. |
| `post_pt_supervoxel_id` | int64 | Supervoxel containing the postsynaptic centroid. |
| `post_pt_root_id` | int64 | Postsynaptic root_id at v888. |
| `pre_pt_position` | list<int64> | Presynaptic centroid `[x, y, z]` in **nanometres** in the CAVE table; the human-readable CSV unpacks it into `prex / prey / prez` columns. |
| `post_pt_position` | list<int64> | Postsynaptic centroid (same convention). |
| `ctr_pt_position` | list<int64> | Midpoint between pre- and post-synaptic centroids (same convention). |

Additional columns baked into this CSV (not in the bare CAVE table):
the eight per-NT probabilities (acetylcholine, dopamine, gaba,
glutamate, histamine, octopamine, serotonin, tyramine) and the argmax
call + confidence (`syn_top_nt` / `syn_top_p`).

## Usage

11.4 GB gzipped — do not load whole-file into R memory. Either stream
with `arrow::open_csv_dataset` (with `lazy_eval`), or pre-filter via
`zcat` + `awk` / `head` before reading in R.

From Python via pyarrow (streaming filter):

```python
import pyarrow.csv as pcsv
import pyarrow.compute as pc
# Read in chunks and filter on pre_root_id without loading the full file
reader = pcsv.open_csv("synapses_v2_human_readable.csv.gz",
                       read_options=pcsv.ReadOptions(use_threads=True))
chunks = []
for batch in reader:
    mask = pc.equal(batch.column("pre_root_id"), "720575941521131930")
    chunks.append(batch.filter(mask))
```

## Related files

- `banc_888_synapses_v2_enriched.parquet` — the **derived** form (the
  one the paper analyses use): autapses dropped, NT classifier
  joined, neuropil / region / side added. Recommended starting point
  for paper-style analyses.
- `banc_888_synapses_v3_human_readable.csv.gz` — the v3 prediction
  counterpart of this file (size ≥ 10 cutoff, updated detection model).
- `banc_888_edgelist_simple_v2.feather` — neuron-to-neuron edgelist
  rolled up from the v2 synapse table at size ≥ 5.

## Notes

- **Autapses included.** Filter `pre_root_id != post_root_id` if you
  want to match the enriched parquet's row set.
- **Coordinates are in voxel space** (4 × 4 × 45 nm). Multiply by the
  voxel size to obtain nanometres.
- **Size 12.3 GB** — too large for `pandas.read_csv` on most laptops.
  Stream / chunk it.
- **`v2` is the synapse-prediction model version**, not the BANC
  materialization (which is v888 throughout the deposit).
