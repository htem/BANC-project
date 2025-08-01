# BANC Data Dictionary and Organization

This directory contains processed datasets, metadata, and analysis results used in the BANC paper. Most raw connectome data is accessed through SQLite databases (see main README for download locations).

💡 **For the most current connectome information, visit [FlyWire Codex](https://codex.flywire.ai/banc).**

## Directory Organisation

- `banc_annotations/` - Functional classification data
- `meta/` - Neuron metadata and inclusion criteria
- `schlegel_2024_fafb_neuron_ranks/` - Neuron layer rankings from Schlegel et al. 2024, graph traversal model for FAFB
- `synapse_capture/` - Synapse detection quality metrics
- `synapse_nt/` - Neurotransmitter prediction accuracy data
- `influence/` - Pre-computed influence metrics
- `cascade/` - Cascade modeling results
- `cns_network/` - Central nervous system network clustering
- Additional analysis-specific subdirectories

## BANC Annotation Hierarchy

The BANC dataset uses a hierarchical classification system for all neurons:

### Primary Classification Levels

**flow** - From the perspective of the whole CNS, whether the neuron is afferent, efferent, or intrinsic:
- `afferent` - Sensory input neurons
- `efferent` - Motor and effector output neurons  
- `intrinsic` - Interneurons (brain-brain, VNC-VNC connections)

**super_class** - Coarse division, hierarchical below flow:
- `sensory` - Sensory processing neurons
- `motor` - Motor control neurons
- `visceral/circulatory` - Autonomic system neurons
- `ascending` - Brain-projecting neurons from VNC
- `descending` - VNC-projecting neurons from brain
- Additional categories: `kenyon_cell`, `central_complex`, etc.

**cell_class** - Hierarchical below super_class:
- `chordotonal_organ_neuron` - Mechanosensory neurons
- `leg_motor_neuron` - Leg movement control
- `gustatory_neuron` - Taste processing
- `visual_projection_neuron` - Visual system output
- [And 100+ additional classes]

**cell_sub_class** - Hierarchical below cell_class:
- `wing_steering_motor_neuron` - Wing flight control
- `front_leg_hair_plate_neuron` - Leg proprioception
- `bitter_gustatory_neuron` - Bitter taste detection
- [And many specialised subtypes]

**cell_type** - The name of the matched neuron from FAFB (if brain neuron or DN) or MANC (if VNC neuron or AN), hierarchical below cell_sub_class:
- Examples: `ORN_DM6`, `DNge110`, `AN08B018`
- Exceptions exist where names were further split to define single cell types
- Unique identifiers for each distinct neuron type

---

## SQLite Database Column Reference

### Metadata Table (`meta`) - One row per neuron

#### Neuron Identifiers
- **root_id, root_626, root_783, bodyid, banc_id, cell_id, id** - Neuron unique identifiers across different datasets
- **root_626** - Root_id for release v626 of BANC (corresponds to preprint version)
- **root_783** - Root_id for FAFB dataset only
- **nucleus_id** - Associated nucleus identifier (FAFB and BANC)

#### Anatomical Properties  
- **side** - the side of the CNS; "L" = left, "R" = right, "S" = unsided/central, "?" = undefined
- **region** - Region of the CNS; all neurons with arbours in the optic lobe are `optic_lobe`, all neurons that fully transit the neck connective are `neck_connective` (`optic_lobe`, `midbrain`, `neck_connective`, `vnc`)
- **nerve** - Entry or exit nerve for afferent/efferent neurons
- **hemilineage** - Developmental lineage (ito_lee_hemilineage, hartenstein_hemilineage)

#### Functional Classification
- **flow** - From the perspective of the whole CNS, whether the neuron is afferent, efferent, or intrinsic
- **super_class** - Coarse division, hierarchical below flow
- **cell_class** - Hierarchical below super_class  
- **cell_sub_class** - Hierarchical below cell_class
- **cell_type** - The name of the matched neuron from FAFB (if brain neuron or DN) or MANC (if VNC neuron or AN), hierarchical below cell_sub_class. Exceptions exist where names were further split to define single cell types
- **modality** - Sensory modality or motor/behavioural association

#### Neurotransmitter Information
- **top_nt** - Most commonly predicted neurotransmitter
- **conf_nt** - Most commonly predicted when weighted by confidence
- **top_nt_p, conf_nt_p** - Average confidence scores for predictions
- **known_nt** - Neurotransmitter of neuron, as reported in the literature
- **known_nt_source** - Citations for literature information
- **cell_type_nt** - Cell type level neurotransmitter determination
- **cell_type_nt_conf** - Cell type level confidence score

#### Connectivity Metrics
- **input_connections** - Total number of input synapses
- **output_connections** - Total number of output synapses
- **total_outputs** - [UNDER CONSTRUCTION] Total output links
- **axon_outputs** - [UNDER CONSTRUCTION] Axonal output links
- **dend_outputs** - [UNDER CONSTRUCTION] Dendritic output links
- **total_outputs_density** - [UNDER CONSTRUCTION] Outputs per micron of cable
- **axon_outputs_density** - [UNDER CONSTRUCTION] Axonal outputs per micron
- **dend_outputs_density** - [UNDER CONSTRUCTION] Dendritic outputs per micron
- **total_inputs** - [UNDER CONSTRUCTION] Total input links
- **axon_inputs** - [UNDER CONSTRUCTION] Axonal input links  
- **dend_inputs** - [UNDER CONSTRUCTION] Dendritic input links
- **total_inputs_density** - [UNDER CONSTRUCTION] Inputs per micron of cable
- **axon_inputs_density** - [UNDER CONSTRUCTION] Axonal inputs per micron
- **dend_inputs_density** - [UNDER CONSTRUCTION] Dendritic inputs per micron

#### Morphological Properties
- **total_length** - Total cable length in microns
- **axon_length** - [UNDER CONSTRUCTION] Axonal cable length
- **dend_length** - [UNDER CONSTRUCTION] Dendritic cable length
- **pd_length** - [UNDER CONSTRUCTION] Primary dendrite cable length
- **pnt_length** - [UNDER CONSTRUCTION] Primary neurite cable length
- **segregation_index** - [UNDER CONSTRUCTION] Entropy score for axon-dendrite segregation

#### Morphological Statistics
- **root** - Root treenode ID in SWC file
- **nodes** - Number of nodes in neuron
- **segments** - Number of segments in neuron
- **branch_points** - Number of branch points
- **endpoints** - Number of endpoints
- **n_trees** - Number of trees (should be 1)
- **connectors** - Number of synapses

#### Laterality Indices
- **postsynapse_side_index** - [UNDER CONSTRUCTION] Side receiving most input
- **presynapse_side_index** - [UNDER CONSTRUCTION] Side producing most output
- **axon_postsynapse_side_index** - [UNDER CONSTRUCTION] Axonal input side bias
- **axon_presynapse_side_index** - [UNDER CONSTRUCTION] Axonal output side bias  
- **dendrite_postsynapse_side_index** - [UNDER CONSTRUCTION] Dendritic input side bias
- **dendrite_presynapse_side_index** - [UNDER CONSTRUCTION] Dendritic output side bias

#### Dense Core Vesicles (DCVs)
- **dcv_count** - Number of DCVs detected
- **dcv_density** - DCVs per unit cable length
- **dcv_soma_count** - Number of somatic DCVs
- **dcv_soma_density** - Somatic DCVs per soma volume

---

### Synapse Tables (`presynapses`, `postsynapses`) - One row per synaptic connection

#### Connection Identifiers
- **pre_id** - Presynaptic neuron ID
- **post_id** - Postsynaptic neuron ID
- **connector_id** - Unique synapse identifier
- **pre_svid, post_svid** - Supervoxel IDs for connection sides

#### Spatial Information
- **x, y, z** - Synapse coordinates in nanometer space
- **inside/neuropil** - Neuropil region(s) containing the synapse
- **region** - Gross anatomical region (optic, midbrain, neck, vnc)

#### Synapse Properties
- **prepost** - Connection direction (0=output, 1=input relative to post_id)
- **size** - Number of voxels in detected synapse
- **scores** - Buhmann prediction score
- **cleft_scores** - Synaptic cleft discriminability score (preferred over size/scores)
- **status** - Quality assessment (good vs suspicious locations)
- **offset** - Index in original SQL table

#### Neurotransmitter Predictions (FAFB only)
- **syn_top_nt** - Synapse-level neurotransmitter prediction
- **syn_top_nt_p** - Confidence score for prediction
- **gaba** - GABA prediction score
- **glutamate** - Glutamate prediction score
- **acetylcholine** - Acetylcholine prediction score
- **octopamine** - Octopamine prediction score
- **serotonin** - Serotonin prediction score
- **dopamine** - Dopamine prediction score

#### Morphological Context
- **label** - Compartment type (axon, dendrite, primary_dendrite, primary_neurite, unknown, soma)
- **treenode_id** - Associated treenode in SWC file
- **strahler_order** - Strahler order of branch
- **flow_cent** - Synaptic flow at node (Schneider-Mizell et al. 2016)

---

### Edge List Tables - Connection summaries

#### Simple Edge List (`edgelist_simple`) - One row per neuron pair
- **pre** - Presynaptic neuron ID
- **post** - Postsynaptic neuron ID  
- **count** - Number of synaptic connections
- **norm** - Normalized weight (count/post_count)
- **pre_count** - Number of presynaptic connections per neuron
- **post_count** - Total inputs to target neuron

#### Detailed Edge List (`edgelist`) - One row per compartment pair
- **pre, post** - Neuron IDs as above
- **pre_label, post_label** - Compartment types (axon, dendrite, etc.)
- **count** - Connections between specified compartments
- **norm** - Normalized by total neuron inputs
- **norm_label** - Normalized by compartment inputs
- **pre_count, post_count** - Connection totals as above
- **pre_label_count, post_label_count** - Compartment-specific connection totals

---

### Dense Core Vesicle Tables (FAFB)

#### Cell-wide DCVs (`dcv_cell`)
- **id** - Unique DCV identifier
- **sv_id** - Supervoxel ID for DCV centroid
- **root_784, segment_id** - Associated neuron identifiers
- **x, y, z** - DCV coordinates in nanometers
- **size** - Detection size in pixels (4x4x40nm voxels)
- **confidence** - Detection network confidence score

#### Somatic DCVs (`dcv_soma`)  
- **x, y, z** - Vesicle coordinates in FlyWire space
- **center_x, center_y, center_z** - Soma center coordinates
- **index** - Section index
- **area** - Number of pixels
- **eccentricity** - Shape circularity measure
- **luminance** - Mean pixel intensity
- **contrast** - Standard deviation of pixels
- **skew** - Pixel intensity skew
- **diameter** - Size measurement
- **orientation** - Angle of major axis
- **perimeter** - Approximate object perimeter
- **centroid** - Segmentation center
- **centroid_weighted** - Intensity-weighted center
- **flywire** - Coordinates in FlyWire space
- **flywire_bbox_start/end** - Bounding box in FlyWire space
- **fafb** - Coordinates in FAFB space  
- **fafb_bbox_start/end** - Bounding box in FAFB space

---

## Influence Metrics

Influence scores quantify indirect pathway effects using linear dynamical modeling (inspired by Pospisil et al., 2024).

### Influence Table Columns
- **id** - BANC neuron root_id
- **supervoxel_id** - [NOT YET PROVIDED] BANC supervoxel identifier
- **seed** - Seed group name (see notes/seeds.md)
- **level** - Seed group level specification
- **influence** - Raw influence score (no scaling)
- **influence_norm** - Influence normalized by number of seeds
- **influence_syn_norm** - Influence normalized by seed synapse count
- **influence_log** - Log-transformed influence (made positive)
- **influence_norm_log** - Log-transformed normalized influence
- **influence_syn_norm_log** - Log-transformed synapse-normalized influence
- **influence_zscore** - Z-score across CNS for same seed
- **influence_mad** - MAD score across CNS for same seed
- **influence_zscore2** - Z-score across seeds for each neuron
- **influence_mad2** - MAD score across seeds for each neuron
- **influence_zscore3** - Combined z-score (CNS and seed groups)
- **influence_mad3** - Combined MAD score (CNS and seed groups)

### Key Seed Groups
- **seed_02** - Sensory cell sub classes
- **seed_07** - Key cells types, including ANs, DNs and VPNs

## Data Processing Notes

- **Missing definitions** are marked with [DEFINITION HERE] and require clarification
- **Under construction** features are being actively developed
- **Cleft scores** are preferred over raw size/scores for synapse quality assessment
- **Influence calculations** use steady-state linear dynamical modeling with modal score ~8
- **Data integration** combines FAFB brain, MANC nerve cord, and BANC neck bridge regions

For specific dataset documentation, see subdirectory README files.
