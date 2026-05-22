---
filename: banc_supplemental_data.zip
local_path: /Users/papers/BANC-project/manuscript/print/supplemental_data
n_files: 10
unzipped_size_bytes: 91000000
unzipped_size_human: 87 MB
content_type: application/zip

# --- Fields posted to Dataverse ---
description: >-
  The ten supplemental_data_*.csv tables cited in the BANC paper, bundled
  as a single ZIP. Supplementary Data 1 is the annotation-taxonomy
  reference (every category, every term in each category, for every
  classification system used in BANC). Supplementary Data 2-5 are
  per-neuron metadata tables for BANC v888, FAFB-FlyWire v783, MANC
  v1.2.1 and maleCNS v0.9 respectively, with cross-dataset match
  identifiers and re-annotated to BANC conventions where the source
  project's annotation was thinner. Supplementary Data 6 gives the
  AN/DN PCA-UMAP coordinates and cluster / super-cluster assignments
  (Fig. 3 and Extended Data Fig. 6); Supplementary Data 7 the effector
  UMAP and cluster / super-cluster assignments (Extended Data Fig. 4f);
  Supplementary Data 8 the CNS-network spectral-clustering output
  (Fig. 6 and Extended Data Fig. 10). Supplementary Data 9 is a curated
  literature review of cell function for ANs, DNs and visual-projection
  neurons used to interpret the clusters. Supplementary Data 10 is the
  bounding-box list of 22 known data-quality issues in BANC v888 (raw
  voxel coordinates, BANC v888 segmentation).
categories:
  - Data
  - Documentation
directoryLabel: supplemental_data
restrict: false
tabIngest: false
---

# banc_supplemental_data.zip

## Purpose

Companion supplementary tables for the BANC paper. Each file is a
self-contained CSV referenced by figure legends and Methods entries.
The ZIP packages all ten together so users can pull the lot in one go;
each CSV is also valid stand-alone.

## Provenance

Built by `R/text/supplemental_data.R` in the BANC-project repository
from `banc_888_meta.feather`, the cross-dataset metadata exports
(franken_meta), the AN/DN clustering pipeline (`panels_an_dn_umap.R`),
the effector UMAP pipeline (`panels_efferent_umap.R`), the CNS-network
spectral clustering pipeline (bancpipeline), and the dataset-issue
bounding-box review.

## Contents

### Supplementary Data 1: Metadata categories and terms

Table of categories of annotations applied to BANC neurons and the
list of terms used in each category. For region, side, flow,
super_class, cell_class, cell_sub_class, cell_type and hemilineage,
only one term applies per neuron. For the other categories, neurons
can be labeled with more than one term. Not enumerated in this table
are cell_type, other_names, fafb_783_cell_type, manc_121_cell_type,
fanc_1116_cell_type, hemibrain_121_cell_type, fafb_783_match_id,
manc_121_match_id, fanc_1116_match_id, hemibrain_121_match_id — there
are too many possible options. Those categories are described in
prose.

Column meanings (every classification system carries its own column;
this is a long-form table):

- `flow` — from the perspective of the whole CNS, whether the neuron
  is afferent, efferent or intrinsic.
- `super_class` — coarse division, hierarchical below `flow`.
- `cell_class` — hierarchical below `super_class`.
- `cell_sub_class` — hierarchical below `cell_class`.
- `cell_type` — the name of the matched neuron from FAFB if it is a
  brain neuron or a DN, or from MANC if it is a VNC neuron or an AN.
  A few exceptions where those names did not define single cell types
  were further split. Hierarchical below `cell_sub_class`.
- `region` — region of the CNS; all neurons with arbors in the optic
  lobe are `optic_lobe`, all neurons that fully transit the neck
  connective between brain and VNC are `neck_connective`.
- `side` — from the fly's perspective, the side on which the cell body
  is located; for afferent neurons, the side of the entry nerve.
- `cell_function` — short functional descriptor, applied largely to
  afferent and efferent neurons.
- `cell_function_detailed` — more detailed functional descriptor than
  `cell_function`.
- `peripheral_target_type` — sensor or effector structure / organ
  targeted by an afferent or efferent neuron.
- `body_part_sensory` — part of the body innervated by an afferent
  neuron.
- `body_part_effector` — part of the body targeted by an efferent
  neuron. If known, this is the site of action when it differs from
  the body part innervated (e.g. wing power motor neurons innervate
  muscles in the thorax but move the wing).
- `nerve` — peripheral nerve (if applicable).
- `hemilineage` — developmental lineage (NA for many neurons).
- `sexually_dimorphic` — `isomorphic`, `dimorphic` or `female-specific`.
- `neurotransmitter_verified` / `neuropeptide_verified` —
  neurotransmitter / neuropeptide of the neuron, as reported in the
  literature.
- `neurotransmitter_predicted` — CNN-predicted primary
  neurotransmitter.
- `other_names` — names given to the neuron that are not the
  `cell_type` name.
- `fafb_783_cell_type`, `manc_121_cell_type`, `fanc_1116_cell_type`,
  `hemibrain_121_cell_type` — cell type of the matching neuron in
  FAFB v783 / MANC v1.2.1 / FANC v1116 / Hemibrain v1.2.1.
- `fafb_783_match_id`, `manc_121_match_id`, `fanc_1116_match_id`,
  `hemibrain_121_match_id` — segment ID of the matching neuron in
  each cross-dataset.

### Supplementary Data 2: BANC neuron metadata

Per-neuron metadata for the 187,590 proofread / roughly-proofread
neurons in the BANC v888 release, with cross-dataset match identifiers
to FAFB, MANC, Hemibrain, FANC and maleCNS where assigned. This is the
reference table for every BANC analysis in the paper.

Columns: `root_id` (BANC v888), `dataset` (constant `"BANC"`, included
so this table can be concatenated with Supps 3-5), `flow`,
`super_class`, `cell_class`, `cell_sub_class`, `cell_type`, `region`,
`side`, `cell_function`, `cell_function_detailed`,
`peripheral_target_type`, `body_part_sensory`, `body_part_effector`,
`nerve`, `hemilineage`, `sexually_dimorphic`,
`neurotransmitter_verified`, `neuropeptide_verified`,
`neurotransmitter_predicted`, `other_names`, plus per-dataset match IDs
`fafb_match`, `manc_match`, `hemibrain_match`, `fanc_match`,
`malecns_match` (NA where no match).

### Supplementary Data 3: Updated FAFB-FlyWire neuron metadata

Per-neuron metadata for the 138,924 FAFB-FlyWire v783 neurons used in
BANC's cross-dataset comparisons. Several columns (`super_class`,
`cell_class`, `cell_sub_class`, `body_part_*`, `cell_function`) were
re-annotated to follow BANC conventions; `cell_type` names are
unchanged from the FAFB project.

Columns: `root_783` (FlyWire neuron ID at FAFB v783), `dataset`
(constant `"FAFB"`), plus the same annotation columns as Supplementary
Data 2.

### Supplementary Data 4: Updated MANC neuron metadata

Per-neuron metadata for the 49,052 VNC neurons from MANC v1.2.1, with
`super_class` / `cell_class` / `cell_sub_class` / `body_part_*` /
`cell_function` aligned to BANC conventions. `cell_type` names are
unchanged from the MANC project.

Columns: `bodyid` (MANC neuron ID at v1.2.1), `dataset` (constant
`"MANC"`), plus the same annotation columns as Supplementary Data 2.

### Supplementary Data 5: maleCNS neuron metadata

Per-neuron metadata for the 165,114 neurons in the male CNS v0.9
release. Cell typing is all from the maleCNS project; annotations have
been aligned to BANC conventions to help users compare and co-analyze
these datasets.

Columns: `malecns_09_id` (maleCNS neuron ID at v0.9), `dataset`
(constant `"maleCNS"`), plus the same annotation columns as
Supplementary Data 2.

### Supplementary Data 6: ANs and DNs with PCA-UMAP coordinates and cluster assignments

Per-neuron metadata for the 3,161 ANs and DNs included in the
connectivity-PCA-UMAP analysis (Fig. 3d, Extended Data Fig. 6), with
cluster and super-cluster assignments.

Columns: `id` (BANC v888 root_id), `UMAP1`, `UMAP2` (2D embedding
from connectivity-PCA-UMAP), `supervoxel_id`, `position` (BANC raw
voxel space), `side`, `region` (primarily `neck_connective`),
`super_class` (ascending or descending for intrinsic neurons only),
`hemilineage`, `cell_function`, `nerve`, `cell_type`,
`fafb_cell_type`, `manc_cell_type`, `super_cluster` (named AN/DN
super-cluster used in figures — e.g. `head orienting`, `walking`,
`postural control`), `cns_network` (CNS-network assignment from the
spectral clustering, where applicable).

### Supplementary Data 7: Effector neurons with UMAP coordinates and functional cluster assignments

Per-neuron metadata for the 1,005 efferent neurons (motor +
visceral / circulatory) in the connectivity-UMAP analysis (Extended
Data Fig. 4f), with cluster and super-cluster assignments.

Columns: `id` (BANC v888 root_id), `UMAP1`, `UMAP2`,
`supervoxel_id`, `position`, `side`, `region`, `flow` (efferent),
`super_class` (motor / visceral_circulatory), `hemilineage`,
`cell_function` (e.g. `leg_motor`, `antenna_motor`, `neck_motor`),
`nerve`, `cell_type`, `fafb_cell_type`, `manc_cell_type`, `cluster`
(EFF_NN), `super_cluster` (named effector super-cluster — e.g.
`front leg`, `flight-steering`, `abdomen-ureter`).

### Supplementary Data 8: CNS network analysis with spectral clustering and UMAP embedding

Per-neuron metadata for the 54,691 intrinsic neurons that entered the
CNS-network spectral clustering analysis (Fig. 6a, Extended Data
Fig. 10). Spectral clustering parameters: min connection strength = 1,
cluster count = 13, cluster seed = 10, embedding seed = 3 (banc_888).

Columns: `root_id`, `UMAP1`, `UMAP2` (UMAP on the connectivity
Laplacian), `supervoxel_id`, `position`, `side`, `region`,
`super_class`, `hemilineage`, `cell_function`, `nerve`, `cell_type`,
`fafb_cell_type`, `manc_cell_type`, `cluster` (AN/DN
`AN_*` / `DN_*` or effector `EFF_*` where applicable),
`super_cluster`, `cns_network` (e.g. `central complex related`,
`abdominal VNC`, `left olfactory`).

### Supplementary Data 9: Literature review on cell function for ascending, descending and visual projection neurons

Curated list of literature-validated functional roles — the canaries
used to interpret the AN/DN and visual-projection clusters.

Columns: `cell_type`, `other_names`, `super_class` (ascending /
descending / visual_projection), `cell_function`, `citations` (short
citation key for the work that established the function), `doi`
(DOI where available).

### Supplementary Data 10: Bounding boxes for known dataset artifacts that negatively impact neuronal reconstruction

22 bounding boxes delineating regions with known data-quality issues
in the BANC dataset. Coordinates are in BANC raw voxel space
(1 voxel = 4 × 4 × 45 nm).

Columns: `issue` (short informal label — `tunnel of death`,
`T2 blowout`, `T1 soup`, `champagne patch`, `left VLP blowout`,
`dorsal CB wavy patch`, `dorsal esophageal crush`, `butt wiggle`),
`min_x`, `min_y`, `min_z` (lower corner of the bounding box),
`max_x`, `max_y`, `max_z` (upper corner).

## Usage

```r
library(readr); library(dplyr)
sd2 <- read_csv("supplemental_data_2.csv")          # BANC meta
sd3 <- read_csv("supplemental_data_3.csv")          # FAFB meta
sd6 <- read_csv("supplemental_data_6.csv")          # AN/DN UMAP
sd8 <- read_csv("supplemental_data_8.csv")          # CNS network UMAP
```

Supps 2-5 share a column convention so they can be concatenated:

```r
all_meta <- bind_rows(sd2, sd3, sd4, sd5)
```

## Related files

- `banc_888_meta.feather` — the live BANC metadata table that
  Supplementary Data 2 is derived from. Use that file in preference
  for analysis; use Supp 2 only for the citable snapshot.
- `banc_888_cns_network_spectral_clustering_v2.csv` (and `_v3`) —
  the upstream spectral-clustering output that flows into Supp 8.
- `codex_annotations.parquet` — the master CAVE annotation table
  that defines the `classification_system` enumeration in Supp 1.

## Notes

- All ten CSVs are UTF-8. Identifier columns (`root_id`, `root_783`,
  `bodyid`, `malecns_09_id`) are 64-bit integers; load as character
  to avoid silent precision loss.
- Supplementary Data 1 is long-form; pivot wide on
  `classification_system` if you need a single-row-per-neuron view of
  the annotations.
- Supp 2-5 are aligned to BANC conventions where the source project's
  annotation was thinner. Where it conflicts with the source project's
  own published metadata, the source project is the version of record
  for its own neurons.
