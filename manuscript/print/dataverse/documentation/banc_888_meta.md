---
filename: banc_888_meta.feather
gcs_path: gs://lee-lab_brain-and-nerve-cord-fly-connectome/compiled_data/banc_888/banc_888_meta.feather
size_bytes: 51450978
size_human: 49.07 MB
md5: 8c8babff28b21c57ecc999e664560ef5
nrows: 188162
ncols: 79
content_type: application/x-arrow

# --- Fields posted to Dataverse ---
description: >-
  Per-neuron metadata table for the BANC connectome at materialization v888,
  one row per segment (n = 188 162) and 79 columns. Carries the full BANC
  annotation taxonomy — soma anchor and root-side position, region,
  hemilineage, developmental neuromere, entry / exit nerve, the four-level
  cell-type hierarchy (super_class > cell_class > cell_sub_class >
  cell_type), functional and body-part labels, cross-dataset matches to
  FAFB-FlyWire v783, MANC v1.2.1, maleCNS v0.9, Hemibrain v1.2.1, and FANC
  v1.116, AN/DN behavior-centric cluster and super_cluster membership,
  CNS-network membership from spectral clustering, predicted and
  literature-verified neurotransmitter and neuropeptide identity, and
  per-neuron morphological metrics (cable length, volume, mitochondria,
  synapse counts, segregation index, primary-dendrite width). Compiled by
  bancpipeline from BANC SeaTable curation, CAVE annotation tables, the
  influence-derived clustering, and a synapse-level neurotransmitter
  classifier. The headline per-neuron table for the BANC paper and the
  natural starting point for any workflow built on bancr's banc_meta().
categories:
  - Data
  - Annotations
directoryLabel: compiled_data
restrict: false
tabIngest: false
---

# banc_888_meta.feather

## Purpose

`banc_888_meta.feather` is the one-row-per-neuron metadata table for the BANC
connectome at CAVE materialization v888 (April 16, 2026 snapshot). It
collects everything the project knows about each neuron — identity across
materializations, anatomical and developmental classification, membership in
higher-level functional groupings, inferred and verified neurochemistry, and
a small set of morphological metrics — in a single file that downstream
analyses join to connectivity, influence, or synapse tables on `root_id`
(= `banc_888_id`).

All 188 162 rows are exposed; non-neuronal segments and unproofread fragments
carry mostly-empty annotations and can be filtered out via the `proofread` /
`roughly_proofread` flags or by selecting on `super_class`. This is the table
the bancr R package returns from `banc_meta()`, and the table from which the
manuscript's figure scripts derive all per-neuron groupings.

## Provenance

Built by **bancpipeline** (`banc/meta/banc-data.R`, Section 1; column
selection at lines 340-362, `arrow::write_feather` at line 372). The build
pulls column-by-column from four sources, with a documented precedence that
resolves disagreements consistently:

- **CAVE tables** at v888 provide identifiers, soma position, proofreading
  status, root-side anatomy, and the per-seed-pool assignment columns
  (`seed_01`–`seed_14`).
- **BANC SeaTable** is the canonical store for manual curation — every
  member of the cell-type hierarchy, the function and body-part labels, the
  cluster / super_cluster assignments, and the verified neurotransmitter
  and neuropeptide identities.
- **Cross-dataset matching pipelines** (FAFB-FlyWire v783, MANC v1.2.1,
  maleCNS v0.9, Hemibrain v1.2.1, FANC v1.116) write the `*_cell_type`,
  `*_match`, and `*_nblast_match` columns; the cell-type matching algorithm
  is described in the paper Methods, "Cell-type matching and annotation".
- **Neurotransmitter classifier** (Eckstein et al. 2024, transferred to
  BANC) produces `neurotransmitter_predicted` and `neurotransmitter_score`;
  the ground-truth labels in `neurotransmitter_verified` and
  `neuropeptide_verified` come from manual curation against the published
  literature.

Where a field could be filled from multiple sources, the precedence is
**SeaTable > GCS-staged CAVE > franken-meta** (the historical metadata
cache), except that `proofread` itself is sourced **GCS > SeaTable** because
it is a segmentation property rather than a curation choice.

## Schema

Columns are grouped here by purpose; the order in the feather follows the
columns block below.

### Identity and provenance

| column | dtype | description |
|---|---|---|
| `banc_888_id` | string | Primary key — the neuron's root identifier at the v888 materialization. Synonymous with `root_id` in bancr. |
| `supervoxel_id` | string | One supervoxel that belongs to the neuron, used to resolve to the current root via CAVE chunked-graph queries. |
| `position` | string | Soma anchor coordinate in BANC voxel space (`"x, y, z"`, 4 × 4 × 45 nm per voxel); empty for segments without a curated soma anchor. |
| `root_626` | string | Root ID for the same neuron at v626 (the preprint / BioRxiv materialization), retained for join compatibility with preprint-era resources. |
| `root_850` | string | Root ID for the same neuron at v850 (the interim materialization used between preprint and publication). |
| `root_888` | string | Root ID for the same neuron at v888 (the published version of record). Same value as `banc_888_id` / `root_id`; present for symmetry. |
| `nucleus_id` | string | Nucleus identifier from the BANC nucleus segmentation; empty if no nucleus was assigned. |
| `proofread` | string | `"TRUE"` if the neuron has passed full proofreading, `"FALSE"` otherwise. |
| `roughly_proofread` | string | `"TRUE"` if the neuron has been proofread to a lower bar (e.g. backbone-only), `"FALSE"` otherwise. |
| `status` | string | Comma-separated curation flags (e.g. `TRACING_ISSUE_RESOLVED`, `REVIEW_MATCH_AN_DN`, `FAFB_MATCH_MANUALLY_CHECKED`, `MERGE_MONSTER`, `SENT_TO_AELYSIA_TRACING`); empty for unflagged neurons. Multiple flags accumulate as the neuron passes through different reviews. |
| `side` | string | Laterality of the neuron, computed from the soma anchor by `bancr:::banc_lr_position()`. Values: `left`, `right`. |
| `root_position` | string | Centroid of the root supervoxel in voxel space. Falls back to a position inside the root when no soma is curated. |
| `root_position_nm` | string | Same as `root_position` but in nanometers (= voxel × `(4, 4, 45)`). |
| `root_region` | string | Region of the root supervoxel, from the alpha-shape neuropil parcellation (e.g. `ITO_midbrain_AL_R`, `MANC_vnc_NTct_UTct_T1_L`, `COURT_vnc_ABDNM`). Empty for segments outside the parcellation. |

### Anatomical and developmental classification

| column | dtype | description |
|---|---|---|
| `region` | string | CNS region of the neuron's arbor: `central_brain`, `optic_lobe`, or `ventral_nerve_cord`. Ascending and descending neurons (those that fully transit the cervical connective) are identified by `grepl("ascending\|descending", super_class)` rather than a dedicated region value. |
| `hemilineage` | string | Developmental hemilineage identifier (e.g. `00A`, `ALad1`, `LB7`). |
| `nerve` | string | Entry or exit nerve, for neurons whose cell body lies outside the CNS (e.g. `left_antennal_nerve`, `right_mesothoracic_leg_nerve`). |
| `tract` | string | Major fiber tract carrying the neuron, where assignable. |
| `neuromere` | string | VNC neuromere of the soma or main arbor (`T1`–`T3`, `A1`–`A8`, `GNG`). |
| `flow` | string | Direction of information flow with respect to the CNS: `intrinsic`, `afferent` (sensory entering the CNS), or `efferent` (motor / endocrine / visceral exiting the CNS). |

### Cell-type hierarchy

| column | dtype | description |
|---|---|---|
| `super_class` | string | Coarsest division. Current values include `central_brain_intrinsic`, `optic_lobe_intrinsic`, `ventral_nerve_cord_intrinsic`, `ascending`, `descending`, `sensory`, `sensory_ascending`, `sensory_descending`, `motor`, `visual_projection`, `visual_centrifugal`, `visceral_circulatory`, `ascending_visceral_circulatory`, plus the non-neuronal `glia`, `trachea`, and `not_a_neuron`. The live vocabulary is the `super_class` column of `codex_annotations.parquet`. |
| `cell_class` | string | Hierarchical below `super_class` (e.g. `olfactory_receptor_neuron`, `antennal_lobe_projection_neuron`, `mushroom_body_output_neuron`). |
| `cell_sub_class` | string | Hierarchical below `cell_class`. |
| `cell_type` | string | Most specific level. Names are inherited from FAFB for brain neurons and DNs, and from MANC for VNC neurons and ANs, with a small number of exceptions where types were further split (e.g. `ORN_DM6`, `DNge110`). |

### Cross-dataset matches

For each external dataset, `*_cell_type` carries the name of the matched
neuron, `*_match` carries the proofread match confidence flag, and
`*_nblast_match` records the top NBLAST candidate when the cell-type match
is ambiguous or absent.

| column | dtype | description |
|---|---|---|
| `fafb_cell_type`, `fafb_match`, `fafb_nblast_match` | string | Match to FAFB-FlyWire v783. |
| `fafb_alignment_cell_type` | string | Cell type proposed by the connectivity-alignment algorithm (paper Methods, "Iterative cross-dataset alignment"). |
| `manc_cell_type`, `manc_match`, `manc_nblast_match` | string | Match to MANC v1.2.1. |
| `malecns_cell_type`, `malecns_match`, `malecns_nblast_match` | string | Match to maleCNS v0.9. |
| `hemibrain_cell_type`, `hemibrain_match`, `hemibrain_nblast_match` | string | Match to Hemibrain v1.2.1. |
| `fanc_cell_type`, `fanc_match`, `fanc_nblast_match` | string | Match to FANC v1.116. |

### Higher-level functional groupings

| column | dtype | description |
|---|---|---|
| `sexually_dimorphic` | string | Sexual-dimorphism call for the cell type. Values: `isomorphic`, `dimorphic`, `female-specific`, `male-specific`. |
| `cluster` | string | Fine AN/DN cluster label of the form `AN_NN` / `DN_NN`, derived from the celltype-partner PCA-UMAP + Marchenko-Pastur clustering pipeline (paper Methods, "AN/DN clusters"). Empty for non-AN/DN neurons. |
| `manual_cluster` | string | Hand-curated override for the small number of cluster boundaries adjusted after inspection. Same `AN_NN` / `DN_NN` vocabulary as `cluster`. |
| `super_cluster` | string | Behavior-centric super-cluster name (e.g. `head orienting`, `flight steering 1`, `flight steering 2`, `walking`, `walking steering`, `postural control`, `taste-touch`, `tactile`, `central complex related`, `flight power`, `grooming`, `feeding`). The live vocabulary is in `codex_annotations.parquet` and is the AN/DN-side mapping from cluster integers maintained in `R/startup/banc-cluster-update.R`. |
| `cns_network` | string | CNS-wide spectral-clustering network membership (paper Fig. 6 and Methods, "CNS networks"); current values include `superior brain`, `inferior brain`, `posterior brain`, `lateral brain`, `flange median bundle`, `central complex related`, `left olfactory`, `right olfactory`, `left visual`, `right visual`, `dorsal VNC`, `leg VNC`, `abdominal VNC`. |
| `body_part_sensory` | string | For afferent neurons, the body part innervated (e.g. `antenna`, `wing`, `front_leg`, `haltere`). |
| `body_part_effector` | string | For efferent neurons, the body part targeted, taken at the site of action when this differs from the innervation site (e.g. wing power motor neurons innervate thoracic muscle but move the wing). |
| `peripheral_target_type` | string | The sensor or effector structure or organ targeted by an afferent / efferent neuron (e.g. `chordotonal_organ`, `bristle`, `campaniform_sensillum`, `levator_muscle`). |
| `cell_function` | string | Brief functional descriptor, applied largely to afferent and efferent neurons (e.g. `neck_motor`, `wing_power`, `proprioception`, `gustatory`). Multi-function neuropeptidergic neurons carry comma-joined labels. |
| `cell_function_detailed` | string | Finer-grained variant of `cell_function` (e.g. `auditory_high_frequency`, `bitter`, `alcoholic_fermentation_volatile`). |

### Neurochemistry

| column | dtype | description |
|---|---|---|
| `neurotransmitter_predicted` | string | Neurotransmitter inferred by the BANC CNN classifier (`acetylcholine`, `gaba`, `glutamate`, `dopamine`, `serotonin`, `octopamine`, `tyramine`, `histamine`, `none`). |
| `neurotransmitter_score` | double | Confidence of the prediction in `[0, 1]`. |
| `neurotransmitter_verified` | string | Manually verified neurotransmitter from the literature, where available. |
| `neuropeptide_verified` | string | Manually verified neuropeptide(s) from the literature (semicolon-separated where a neuron expresses more than one). |

### Morphological metrics

| column | dtype | description |
|---|---|---|
| `l2_nodes` | double | Number of L2 chunked-graph nodes in the reconstruction. |
| `l2_cable_length_um` | double | Total skeletal cable length, in micrometers. |
| `volume_nm3` | double | Volume of the segmentation, in cubic nanometers. |
| `input_connections` | double | Number of postsynaptic sites that belong to this neuron (incoming synapses). |
| `output_connections` | double | Number of presynaptic sites (outgoing synapses). |
| `input_side_index` | double | Laterality index for incoming synapses in [-1, 1] (negative → predominantly left, positive → predominantly right). |
| `output_side_index` | double | Same for outgoing synapses. |
| `mitochondria` | double | Mitochondrion count inside the segmentation. |
| `mitochondria_volume` | double | Total mitochondrial volume (cubic nanometers). |
| `pd_width` | double | Primary-dendrite width, used in cell-typing of optic-lobe neurons. |
| `segregation_index` | double | Axon/dendrite segregation index in `[0, 1]` (1 = fully polarized). |

### Seed / proofreading provenance

| column | dtype | description |
|---|---|---|
| `seed_01`–`seed_14` | string | Per-seed-pool assignment recording which annotation / proofreading campaign labeled the neuron, with the seed-pool's own categorical value (e.g. `descending_walking` in `seed_01`, `antenna_campaniform_sensillum_neuron` in `seed_02`, `CNS_08` in `seed_14`). Empty when the campaign did not touch the neuron. Useful for tracing curation provenance. |

## Usage

In R via bancr:

```r
library(bancr); library(dplyr)
m <- banc_meta()          # reads from the local cache; mirrors this file
m %>% filter(as.logical(proofread), super_class == "descending") %>%
  count(super_cluster, sort = TRUE)
```

In Python via pyarrow:

```python
import pyarrow.dataset as ds
m = ds.dataset("banc_888_meta.feather", format="feather").to_table(
    columns=["banc_888_id","super_class","super_cluster","proofread"]
).to_pandas()
```

To stream from this Dataverse deposit without downloading the whole file,
pyarrow can open the IPC file in a single network read once you fetch
the bytes; for repeated use, cache locally.

## Related files

- `banc_888_metrics.feather` — same morphological metric columns, split
  out as a smaller standalone file with the per-metric column set.
- `banc_888_edgelist_simple_v2.feather`, `banc_888_edgelist_simple_v3.feather`
  — neuron-to-neuron edgelists that join to `banc_888_id` on both `pre`
  and `post`.
- `banc_888_edgelist_split_v2.feather` — compartment-to-compartment
  edgelist; same join keys.
- `banc_888_synapses_v2_enriched.parquet`,
  `banc_888_synapses_v3_enriched.parquet` — per-synapse tables that join
  on `pre_root_id` / `post_root_id`.
- `banc_888_neurotransmitter_prediction_v2.csv` — standalone copy of the
  predicted-NT columns, for users who only need the classifier output.
- CAVE annotation parquets under `annotations/v888/` — the upstream
  curated tables from which most identity and annotation columns derive
  (`codex_annotations.parquet` is the master; siblings include
  `cell_info.parquet`, `cell_representative_point.parquet`,
  `backbone_proofread.parquet`, `proofreading_notes.parquet`,
  `somas_v1.parquet`, `peripheral_nerves.parquet`,
  `neck_connective_y92500.parquet`).
- `banc_swc_skeletons.zip`, `banc_neuron_meshes.zip` — per-neuron
  morphological data keyed on `banc_888_id`.
- NBLAST feathers under `nblast/` — pairwise morphological similarity
  for every neuron in this table against the major reference connectomes
  (FAFB v783, MANC v1.2.1, FANC v1116, maleCNS v0.9, Hemibrain v1.2.1)
  and against itself / its mirror.
- `influence/all_to_all/` — per-pair influence between every neuron in
  this table.
- `banc_888_cns_network_spectral_clustering_v2.csv` (and `_v3`) —
  upstream spectral-clustering output. The `cns_network` column in
  this meta is sourced from there.
- `banc_888_betweenness_all_to_all_v2.csv` (and the v3 + sensory-to-
  effector variants) — per-neuron Brandes betweenness centrality on
  the same intrinsic-neuron pool used by the spectral clustering.
- `banc_problem_regions.csv` — bounding boxes of dataset problem
  regions; some `status` codes in this table flag membership.
- `banc_supplemental_data.zip` — the paper's 10 Supplementary Data
  CSVs, including Supp 2 which is a citable snapshot of this table.

## Notes

- The file is written as Arrow IPC (Feather V2). All identifier-like
  columns are typed as `string` rather than `int64`; cast on read if your
  downstream code needs integers.
- The `proofread` / `roughly_proofread` columns are the string
  `"TRUE"` / `"FALSE"`, not R logicals. bancr converts to logical
  on read; pyarrow consumers should compare against the strings.
- **Proofread tiers are disjoint, not nested.** `proofread == "TRUE"` is
  the strict tier (reviewed end-to-end, signed off as correctly
  segmented); use for analyses sensitive to false-positive merges or
  splits (synaptic-connectivity, motif counts, edge-weight analyses).
  `roughly_proofread == "TRUE"` is the backbone-only tier — fine branches
  may not have been checked; use for surveys of cell-type membership,
  gross morphology, or arbor extent. The two flags are mutually exclusive
  for a given row; the lenient inclusion criterion is
  `proofread == "TRUE" | roughly_proofread == "TRUE"`
  (see `filter_valid_neurons()` in `R/startup/banc-functions.R`).
  Neurons that fail both flags are unproofread segmentation output and
  should generally be excluded.
- **Antennal-nerve damage caveat.** Dissection damaged both the left and
  right antennal nerves; sensory neurons from Johnston's organ and
  central-brain intrinsic neurons that pass close to the entry point are
  consequently under-represented in BANC. Annotations are still present,
  but the underlying reconstructions may be incomplete (see
  `peripheral_nerves.parquet` Notes and paper Methods, "Specimen and
  dataset description").
- **Dataset problem regions.** `banc_problem_regions.csv` lists bounding
  boxes of known alignment artefacts, tissue folds, and section damage.
  Some `status` values in this table flag membership; cross-reference if a
  cell type's neurons cluster spatially in a problem region before drawing
  biological conclusions.
- A handful of super_cluster names from earlier materializations have been
  merged into the current vocabulary (`takeoff-landing` → `postural
  control`; the standalone `interoceptive` cluster → `taste-touch`;
  `wing-leg-tactile` / `head-leg-tactile` → `tactile`). A small number of
  legacy rows still carry retired names — treat the AN/DN-side mapping in
  `R/startup/banc-cluster-update.R` as canonical for the paper.
- For neurons in the optic lobe, classification by `super_class` /
  `cell_class` / `cell_sub_class` is rich but `cell_type` coverage is
  asymmetric between hemispheres — the right optic lobe is more thoroughly
  cell-typed than the left.
