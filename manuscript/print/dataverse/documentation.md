# BANC v888 dataset — Harvard Dataverse documentation

**Persistent identifier:** [doi:10.7910/DVN/7WTH1N](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/7WTH1N)
**Reference paper:** Bates, Phelps, Kim, Yang et al. (2026). *Distributed control circuits across a brain-and-cord connectome*.
**Materialization:** v888 (snapshot date 2026-04-16)
**Total deposit size:** ≈ 391 GB
**License:** CC BY 4.0 (deposit); see per-archive docs for internal code-repo licenses.

## Overview

This dataset contains the BANC connectome — the first densely
reconstructed electron-microscopy connectome that unites the adult
female *Drosophila melanogaster* brain and ventral nerve cord — together
with the analysis-ready tables, derived metrics, and code used in the
paper.

A live, browseable version of the connectome is hosted at
[Codex (codex.flywire.ai/banc)](https://codex.flywire.ai/banc) and at
[ng.banc.community](https://ng.banc.community/view). This Dataverse
deposit is the **DOI-citable static snapshot** of the same data, with
the per-file documentation and code provenance needed to reproduce the
paper's analyses.

For a guided tour, start with the headline metadata table
[`compiled_data/banc_888_meta.feather`](#compiled_data) and pair it
with the relevant edgelist and synapse table. **The paper this deposit
accompanies — Bates, Phelps, Kim, Yang et al., 2026 — uses synapses v2
(size ≥ 5) throughout every figure and quantitative analysis**, so to
reproduce paper results use `banc_888_edgelist_simple_v2.feather` and
`banc_888_synapses_v2_enriched.parquet`. For new analyses, the v3
variants (`banc_888_edgelist_simple_v3.feather`,
`banc_888_synapses_v3_enriched.parquet`, size ≥ 10) carry the updated
detector — ~18% more synapses overall — and are the recommended default
going forward. For 3D visualization, the Neuroglancer states
(`neuroglancer_states_2026a.zip`) combined with the neuron meshes
(`banc_neuron_meshes.zip`) reproduce every figure rendering in the paper.

## How to cite

Cite **both** the paper and this Dataverse deposit. A suggested citation
for the deposit:

> Bates, A.S., Phelps, J.S., Kim, M., Yang, H.H., et al. (2026).
> "BANC v888 data deposit". Harvard Dataverse, V1.
> https://doi.org/10.7910/DVN/7WTH1N

Several individual code archives in `code/` carry their own preferred
citations (Zenodo DOI for `ConnectomeInfluenceCalculator`, GitHub +
SHA for the others); see the per-file documentation under
`documentation/code/` for each.

## Where each file lives in the deposit

The deposit is organized into Dataverse "directories" (Dataverse
`directoryLabel` field, rendered as folder-style groupings in the UI).
The layout mirrors the bucket layout at
`gs://lee-lab_brain-and-nerve-cord-fly-connectome/` for ergonomic
parity between the cloud-storage view and the DOI deposit.

### `documentation/` — front matter and reference text

| file | size | description |
|---|---:|---|
| `acknowledgements.md` | 17 KB | Author list, affiliations, BANC-FlyWire Consortium, acknowledgements, funding, author contributions and competing interests. |
| `banc_problem_regions.csv` | 1.2 KB | Bounding boxes of 22 known dataset problem regions (alignment artifacts, tissue folds, section damage). |
| `bibliography.bib` | 631 KB | BibTeX bibliography for the BANC paper (~590 entries). |

### `compiled_data/` — analysis-ready BANC tables

These are the per-neuron and per-synapse tables the paper analyses use.
They are derived end-products of the bancpipeline build (see
`code/bancpipeline_archive.zip`) over the CAVE annotation tables and
synapse exports.

| file | size | description |
|---|---:|---|
| `banc_888_meta.feather` | 49 MB | **Start here.** Per-neuron metadata, 188 162 rows × 79 cols (identity, region, cell-type hierarchy, cluster, neurochemistry, morphology). |
| `banc_888_metrics.feather` | 7.5 MB | Per-neuron quantitative metrics (cable length, volume, synapse counts), 188 199 rows × 12 cols. |
| `banc_888_neurotransmitter_prediction_v2.csv` | 21 MB | Per-neuron neurotransmitter prediction summary, CSV form. |
| `banc_888_synapses_v2_enriched.parquet` | 9.6 GB | Per-synapse table, v2 snapshot (size ≥ 5), 169 M rows × 21 cols with neuropil + NT enrichment. |
| `banc_888_synapses_v3_enriched.parquet` | 5.6 GB | Same shape, v3 snapshot (size ≥ 10), updated NT classifier. |
| `banc_888_edgelist_simple_v2.feather` | 285 MB | Neuron-to-neuron edgelist rolled up from v2 synapses, 11.5 M rows × 6 cols. |
| `banc_888_edgelist_simple_v3.feather` | 336 MB | Same from v3 synapses, 13.5 M rows × 6 cols. |
| `banc_888_edgelist_split_v2.feather` | 339 MB | Compartment-to-compartment edgelist, 6.3 M rows × 13 cols. |
| `banc_888_cns_network_spectral_clustering_v2.csv` | <10 MB | Spectral-clustering output — per-neuron CNS-network assignment + Laplacian UMAP for the intrinsic-neuron pool (Fig. 6). v3 variant is also deposited. |
| `banc_888_betweenness_all_to_all_v2.csv` | <10 MB | All-to-all Brandes betweenness centrality per neuron. v3 variant + afferent-to-effector variants of both also deposited. |

### `supplemental_data/` — paper supplementary tables

The ten supplemental_data_*.csv tables cited by the paper, bundled as a
single ZIP. Covers the BANC annotation taxonomy (Supp 1), the BANC /
FAFB / MANC / maleCNS per-neuron metadata tables (Supps 2–5), the AN/DN
PCA-UMAP + cluster assignments (Supp 6), the effector UMAP + cluster
assignments (Supp 7), the CNS-network spectral-clustering output
(Supp 8), the cell-function literature review for ANs / DNs / visual
projection neurons (Supp 9), and the bounding-box list of dataset
artifacts (Supp 10).

| file | size | description |
|---|---:|---|
| `banc_supplemental_data.zip` | 87 MB | Bundle of supplemental_data_1.csv through supplemental_data_10.csv. |

### `synapses/` — synapse-side raw and intermediate tables

The upstream sources joined into `compiled_data/banc_888_synapses_v*_enriched.parquet`.
Useful if you want to re-derive the join differently, or work with one
side of the enrichment in isolation.

| file | size | description |
|---|---:|---|
| `synapses/v2.0/banc_nt_prediction_w_sizethresh_5_11102025.parquet` | 3.6 GB | Per-synapse v2 NT-classifier output (size ≥ 5). |
| `synapses/v2.0/synapse_neuropil_lookup_v2.parquet` | 1.95 GB | Per-synapse neuropil/region/side lookup, v2 synapses. |
| `synapses/v3.0/banc_nt_prediction_v3_w_sizethresh_10_05042026.parquet` | 5.4 GB | v3 NT-classifier output (size ≥ 10). |
| `synapses/v3.0/synapse_neuropil_lookup_v3.parquet` | 2.2 GB | v3 neuropil lookup. |
| `synapses/manual_review/2024-09-20_aelysia_synapse_sample_complete.csv` | 343 KB | Aelysia manual review of 6 029 v2 synapses (the source of synapse-quality validation in the paper). |

### `annotations/v888/` — CAVE annotation tables

The upstream raw annotation tables that flow into the meta feather.
Useful if you need the data in its native CAVE shape or want to track
how a specific annotation propagated.

| file | size | description |
|---|---:|---|
| `codex_annotations.parquet` | 71 MB | **The big one.** Master annotation table (curated by the core BANC team), drives Codex; canonical labels for cell_type, super_class, etc. |
| `cell_info.parquet` | 12 MB | Community-contributed annotations (not curator-vetted). |
| `backbone_proofread.parquet` | 9.8 MB | Flags for neurons with backbone-level proofreading. |
| `proofreading_notes.parquet` | 858 KB | Per-neuron proofreader notes (incl. roughly-proofread tags). |
| `somas_v1.parquet` | 5.1 MB | BANC's detected nuclei / somas. |
| `cell_representative_point.parquet` | 5.2 MB | Stable representative points per neuron (join key for codex_annotations). |
| `peripheral_nerves.parquet` | 629 KB | Per-axon nerve-entry/exit seed annotations. |
| `neck_connective_y92500.parquet` | 139 KB | Neck-plane (y=92500) seed annotations defining the AN/DN set. |

### `nblast/` — pairwise morphological similarity

NBLAST tables of BANC neurons against each external reference connectome,
all bridged through JRC2018 templates. Useful for cell-type matching
and for morphology-based search.

| file | size | description |
|---|---:|---|
| `banc_fafb_783_nblast.feather` | 552 MB | BANC ↔ FAFB-FlyWire v783 (full female brain). |
| `banc_manc_v1.2.1_nblast.feather` | 116 MB | BANC VNC ↔ MANC v1.2.1 (full male VNC). |
| `banc_fanc_1116_nblast.feather` | 89 MB | BANC VNC ↔ FANC v1116 (female VNC, Lee lab). |
| `banc_malecns_v0.9_nblast.feather` | 235 MB | BANC ↔ maleCNS v0.9 (full male CNS). |
| `banc_hemibrain_v1.2.1_nblast.feather` | 190 MB | BANC ↔ Hemibrain v1.2.1 (central brain). |
| `banc_mirror_nblast.feather` | 323 MB | BANC ↔ mirrored BANC (left/right pair detection). |
| `banc_native_nblast.feather` | 5.7 MB | BANC all-versus-all (canary subset; serial-homologue detection). |

### `influence/` — adjusted-influence scores

The headline functional metric from the paper (paper Methods, "Influence").
Two roll-ups plus the full all-pairs shard set.

| file | size | description |
|---|---:|---|
| `influence_all_to_effector_subclass.parquet` | 34 MB | All-neurons → effector sub-class adjusted influence (paper Fig. 2e, Fig. 5). |
| `influence_sensory_subclass_to_all.parquet` | 272 MB | Sensory sub-class → all-neurons adjusted influence (paper Fig. 2, Fig. 3). |
| `influence/all_to_all/chunk_NNNN.parquet` × 277 | 287 GB | Full BANC v888 all-to-all influence, sharded by source neuron. |

### `meshes/`, `skeletons/`, `color_mips/` — morphological data

Per-neuron 3D representations, distributed as ZIPs.

| file | size | description |
|---|---:|---|
| `banc_swc_skeletons.zip` | 24.5 GB | L2-resolution SWC skeletons, ~165 000 files. |
| `banc_neuron_meshes.zip` | 45 GB | Full-resolution segmentation meshes (Neuroglancer format). |
| `banc_neuropil_meshes.zip` | 42 MB | Watertight neuropil-region OBJ meshes. |
| `banc_color_mips.zip` | 701 MB | Color-depth MIP PNGs in JRC2018U space (for NeuronBridge search). |

### `microCT/`, `registrations/` — anatomical context

| file | size | description |
|---|---:|---|
| `banc_microCT.zip` | 1.22 GB | Pre-EM X-ray microCT of the BANC fly's whole body. |
| `banc_template_spaces.zip` | 808 MB | BANC + JRC2018 brain/VNC template NRRDs. |
| `registration_brain_jrc2018f.zip` | 19 MB | BANC ↔ JRC2018F brain elastix registration. |
| `registration_vnc_jrc2018vncf.zip` | 71 MB | BANC ↔ JRC2018VNCF VNC elastix registration. |

### `behavior/` — pre-EM behavioral characterization

| file | size | description |
|---|---:|---|
| `behavior.zip` | 348 MB | rBIas walking-arena recordings, tracking and summary scoring for the BANC fly. |

### `neuroglancer_states/` — paper-figure Neuroglancer states

| file | size | description |
|---|---:|---|
| `neuroglancer_states_2026a.zip` | 6.4 MB | 91 JSON state files behind every Neuroglancer link in the paper. |

### `code/` — code archives (snapshots; live copies on GitHub)

Each is a snapshot ZIP pinned at the upload date. Live copies continue
to evolve at the GitHub URL listed in each per-file documentation.

| file | source repository |
|---|---|
| `bancr_archive.zip` | natverse/bancr — R client for BANC data |
| `synister_banc_archive.zip` | htem/synister_banc — neurotransmitter-prediction model + ground truth |
| `bancpipeline_archive.zip` | wilson-lab/bancpipeline — the build pipeline that produced the compiled data above |
| `banc_project_archive.zip` | htem/BANC-project — paper analysis code (figures, supplemental data) |
| `influencer_archive.zip` | natverse/influencer — R influence calculator (data.table backend) |
| `connectome_influence_calculator_archive.zip` | DrugowitschLab/ConnectomeInfluenceCalculator — Python influence calculator (PETSc backend) |
| `nat_ggplot_archive.zip` | natverse/nat.ggplot — R ggplot2 neuron plotting |
| `the_banc_fly_connectome_archive.zip` | jasper-tms/the-BANC-fly-connectome — Python tools and Neuroglancer-state sources |
| `banc-0.6.1.tar.gz` | pypi.org/project/banc — lean pip-installable Python client (sdist of the same repo) |
| `fly_connectome_data_tutorial_archive.zip` | sjcabs/fly_connectome_data_tutorial — R + Python tutorial |
| `drosophila_neuropeptides_archive.zip` | funkelab/drosophila_neuropeptides — fast-acting neuropeptide curation |
| `drosophila_neurotransmitters_archive.zip` | funkelab/drosophila_neurotransmitters — fast-acting NT curation |

## Materialization version note

The dataset is pinned to **v888** (snapshot date 2026-04-16), the **version of record** for the published paper. Earlier materializations are retained as join keys so legacy resources can be matched to the current snapshot:

- **`root_888`** — published version (this deposit; same value as `banc_888_id`).
- **`root_850`** — interim analyses between preprint and publication.
- **`root_626`** — preprint version (BioRxiv).

Materialization-naming conventions:
- **`root_id`** in the deposit always refers to v888.
- `banc_888_id` and `root_888` are synonyms of `root_id`, present for symmetry.

## Per-file documentation

Every file in the deposit has a per-file description attached as its Dataverse `description` metadata — click the file in the Dataverse UI (or fetch `/api/files/{id}/metadata` via the Dataverse API) to see its full schema, provenance, usage examples, related files and notes. The same per-file documentation is also distributed as Markdown inside `code/banc_project_archive.zip`, under `manuscript/print/dataverse/documentation/`, for users who prefer to grep the whole set locally.

## Reading and writing the data

R users should start with the **bancr** package
(`code/bancr_archive.zip` or the live https://github.com/natverse/bancr).
Key entry points:

- `banc_meta()` — load `banc_888_meta.feather`
- `banc_edgelist()` — load `banc_888_edgelist_simple_v3.feather`
- `banc_partners(id)` — synapse-level partner table for one neuron
- `banc_influence(upstream_ids, downstream_ids)` — query the
  all-to-all influence shards
- `banc_skeleton(id)` — fetch a single SWC skeleton
- `banc_mesh(id)` — fetch a single mesh
- `banc_nblast_matches(dataset = "fafb_783")` — query the NBLAST tables

Python users should start with the **the-BANC-fly-connectome** package
(`code/the_banc_fly_connectome_archive.zip` or the live
https://github.com/jasper-tms/the-BANC-fly-connectome), in tandem with
**cloud-volume**, **pyarrow**, and **CAVEclient** for direct CAVE access.

To inspect the raw schemas without a full download, pyarrow works against the **Dataverse file download URL** (pull just the bytes you need; works for any user with the DOI):

```python
import pyarrow.feather as feather
import urllib.request
# File ID is visible in the Dataverse UI next to each file; example shown
url = "https://dataverse.harvard.edu/api/access/datafile/<FILE_ID>"
with urllib.request.urlopen(url) as r:
    table = feather.read_table(r.read())
```

Or, if you have access to the source GCS bucket (`gs://lee-lab_brain-and-nerve-cord-fly-connectome/`):

```python
import pyarrow.dataset as ds
import pyarrow as pa
fs = pa.fs.GcsFileSystem(anonymous=False)
table = ds.dataset(
    "lee-lab_brain-and-nerve-cord-fly-connectome/compiled_data/banc_888/banc_888_meta.feather",
    filesystem=fs, format="feather"
).to_table()
```

## Live versus archived

This Dataverse deposit is intentionally a **static snapshot**. The
canonical live resources continue to evolve:

- **Codex** at https://codex.flywire.ai/banc — browse and download
  per-neuron data with the latest materialization.
- **Neuroglancer** at https://ng.banc.community — 3D visualization.
- **CAVE** at https://global.daf-apis.com — programmatic access to
  segmentation and synapses at any materialization.
- **GCS bucket** at `gs://lee-lab_brain-and-nerve-cord-fly-connectome/`
  — the source bucket from which this Dataverse was assembled.

If the answer you need is "what does BANC look like right now?", prefer
the live resources. If the answer you need is "what data does Bates,
Phelps, Kim, Yang et al. (2026) cite?", this Dataverse is the
authoritative source of record.

## Issues, corrections and feedback

- Issues with the data: file an issue at
  https://github.com/htem/BANC-project/issues.
- Issues with the bancr API: https://github.com/natverse/bancr/issues.
- Corrections to this documentation file: PR against
  https://github.com/htem/BANC-project; the source lives at
  `manuscript/print/dataverse/documentation.md`.

## Acknowledgements

See `documentation/acknowledgements.md` for the full author list,
consortium membership, acknowledgements and funding statement.
