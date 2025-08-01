<div align="center">

![BANC Banner](https://github.com/htem/BANC-project/blob/main/notes/banc-poster-new-neurons.png?raw=true)
Artwork by [Amy Sterling](https://www.amysterling.org/)

# The Brain-And-Nerve-Cord (BANC) Connectome

*Distributed control circuits across a brain-and-cord connectome*

**First Authors:** [Alexander S. Bates](https://scholar.google.com/citations?user=BOVTiXIAAAAJ&hl=en), [Jasper S. Phelps](https://scholar.google.com/citations?user=HT5N2EIAAAAJ&hl=en), [Minsu Kim](https://www.linkedin.com/in/mindy-minsu-kim), [Helen H. Yang](https://orcid.org/0000-0001-5140-9664)  
**Corresponding Authors:** Mala Murthy, Jan Drugowitsch, [Rachel I. Wilson](https://scholar.google.com/citations?user=T-RODd8AAAAJ&hl=en), [Wei-Chung Allen Lee](https://scholar.google.com/citations?user=y2s07ssAAAAJ&hl=en)

</div>

<div align="center">

![BANC Fly](https://github.com/htem/BANC-project/blob/main/figures/schematics/assets/fly_above.png?raw=true)

</div>

## Overview

This repository contains code and data for the first unified connectome of a limbed animal's brain and nerve cord. The BANC dataset includes ~160,000 neurons from an adult female *Drosophila melanogaster*, representing the complete central nervous system from brain to ventral nerve cord.

🏦 **Ready to take your research to the BANC?** This connectome offers unprecedented insights into distributed neural control...

🧠 **Don't just bank on assumptions about neural circuits** - explore the actual wiring with BANC's comprehensive connectivity data...

**Key Discoveries:**
- First unified brain-and-nerve-cord connectome revealing distributed control architecture
- Discovery of behaviour-centric neural modules organised by ascending/descending pathways  
- Local feedback loops that challenge traditional hierarchical models of motor control
- Novel influence metrics quantifying indirect neural pathway effects
- Integration with existing connectomes (FlyWire-FAFB brain, MANC nerve cord)

**Collaborative Effort:** This work represents a massive research community open-science effort with 155+ contributors from 35+ institutions worldwide, combining electron microscopy, AI-assisted segmentation, and community-based proofreading and annotation.

**Dataset Version:** This repository corresponds to BANC v626, the version used in the preprint. We plan to release an updated version in the coming months with improved reconstruction, enhanced annotations, and completed optic lobe coverage.

💡 **For the most up-to-date connectome information and latest annotations, visit [FlyWire Codex](https://codex.flywire.ai/banc) - your go-to source for current BANC data. FlyWire Codex was built and maintained by [Arie Marisliah](https://arie.matsliah.org/)**

## Repository Structure

This repository is organised for researchers and analysts to access BANC data and reproduce paper figures:

```
BANC-project/
├── figures/          # Publication figures with source panels
│   ├── figure_1/     # Figure files (.ai, .png) and linked panels in links/
│   ├── figure_2/     # Each figure has its own directory
│   └── ...
├── R/                # R scripts for data analysis and figure generation
│   ├── figures/      # Scripts that generate specific figure panels
│   ├── startup/      # Data loading and configuration scripts
│   └── exploration/  # Additional analysis scripts
├── data/             # Processed data and metadata files
├── settings/         # Color schemes and visualization standards
└── submission/       # Paper files and supplemental data
```

## Getting Started

### For Interactive Data Exploration
**No installation required** - Use these web interfaces:
- **[FlyWire Codex](https://codex.flywire.ai/banc)** - Interactive neuron browser
- **[Neuroglancer](https://ng.banc.community/view)** - 3D visualization and circuit exploration  
- **[CAVE Explorer](https://github.com/CAVEconnectome/CAVEclient)** - Advanced connectivity analysis

### For Data Analysis and Reproduction
```bash
# Clone the repository
git clone https://github.com/htem/BANC-project.git
cd BANC-project

# Install R dependencies (see R/startup/ for configuration)
# Download connectome data from Harvard Dataverse (see Data Access below)
```

### For Programmatic Access
- **R users:** Install `bancr` package: `devtools::install_github("flyconnectome/bancr")`
- **Python users:** See [community tools](https://banc.community) for Python clients

## Data Access

### Interactive Exploration (No Setup Required)
- **[FlyWire Codex](https://codex.flywire.ai/banc)** - Web-based neuron browser and circuit explorer with the most current data
- **[Neuroglancer](https://ng.banc.community/view)** - 3D visualisation of neurons and connectivity
- **[CAVE connectome annotation versioning engine]([https://spelunker.cave-explorer.org/](https://github.com/CAVEconnectome))** - Advanced connectivity analysis tools

### Direct Data Downloads
- **[Harvard Dataverse](https://doi.org/10.7910/DVN/8TFGGB)** - Complete connectome datasets, metadata, and processed data (SQLite files: 10-30GB each)
- **[Influence Scores](https://doi.org/10.5281/zenodo.15999930)** - Code for our indirect influence metric calculations

### Programmatic Access
- **[bancr R package](https://github.com/flyconnectome/bancr)** - R interface for BANC data analysis
- **[Community Tools](https://banc.community)** - Python clients and additional analysis tools
- **[CAVE API](https://global.daf-apis.com/info/)** - Programmatic access to annotations and connectivity

### Authenticated Data Access

**For users who want to edit the data and add their own annotations.**

To access BANC resources, you must have permissions to access the [BANC autosegmentation dataset](https://fanc-reconstruction.slack.com/archives/C01RZP5JH9C/p1616522511001900) and have [confirmed your acceptance](https://fanc-reconstruction.slack.com/archives/C01RZP5JH9C/p1617404290005300) of the BANC proofreading and data ownership guidelines. At this point you should have a linked Google account that will be authorised for access to BANC online resources.

If you have access, you can view *BANC* data in this helpful [neuroglancer scene](https://spelunker.cave-explorer.org/#!middleauth+https://global.daf-apis.com/nglstate/api/v1/4753860997414912).

The BANC project uses [CAVE tables](https://global.daf-apis.com/info/) to store many sorts of annotation information. You can see the available CAVE tables for BANC [here](https://cave.fanc-fly.com/annotation/views/aligned_volume/brain_and_nerve_cord). CAVE tables can be joined into useful [CAVE views](https://cave.fanc-fly.com/materialize/views/datastack/brain_and_nerve_cord) pinned to a materialisation, which can provide very useful objects such as the whole BANC edgelist. BANC view names and SQL formulas are given [here](https://github.com/jasper-tms/the-BANC-fly-connectome/wiki/CAVE-Views).

### Analysis Pipeline and Code
- **[Main Repository](https://github.com/htem/BANC-project/)** - This repository with figure reproduction code
- **[Processing Pipeline](https://github.com/wilson-lab/bancpipeline)** - Data processing and analysis scripts

## Paper Figures

All figures can be reproduced using the R scripts in this repository. Below are the main figures with links to high-resolution versions:

### Figure 1: An open-source brain-and-nerve-cord connectome
EM dataset overview, quality control metrics, data sharing platforms, and community annotation efforts.

![Figure 1](https://github.com/htem/BANC-project/blob/main/figures/figure_1/figure.png?raw=true)

**Supplement:** Quality control, imaging parameters, and alignment details
![Figure 1 Supplement](https://github.com/htem/BANC-project/blob/main/figures/figure_1/supplement-1.png?raw=true)

### Figure 2: Linking sensors and effectors through local and long-range circuits
Influence analysis revealing distributed control architecture and local feedback loops that challenge hierarchical models.

![Figure 2](https://github.com/htem/BANC-project/blob/main/figures/figure_2/figure.png?raw=true)

**Supplement:** Detailed influence validation and connectivity analysis
![Figure 2 Supplement](https://github.com/htem/BANC-project/blob/main/figures/figure_2/supplement-1.png?raw=true)

### Figure 3: Clustering ANs and DNs into behaviour-centric modules
Identification of ascending neuron (AN) and descending neuron (DN) clusters that form behaviour-specific functional modules.

![Figure 3](https://github.com/htem/BANC-project/blob/main/figures/figure_3/figure.png?raw=true)

**Supplement:** Detailed connectivity patterns within AN-DN modules
![Figure 3 Supplement](https://github.com/htem/BANC-project/blob/main/figures/figure_3/supplement-1.png?raw=true)

### Figure 4: Specializations and coordination within a functional supercluster
Detailed analysis of head and eye orienting circuits showing specialized and coordinated neural control.

![Figure 4](https://github.com/htem/BANC-project/blob/main/figures/figure_4/figure.png?raw=true)

**Supplement:** Additional circuit specializations and coordination examples
![Figure 4 Supplement](https://github.com/htem/BANC-project/blob/main/figures/figure_4/supplement-1.png?raw=true)

### Figure 5: Interactions between behaviour-centric modules
Network analysis of interactions between different behavioural modules and their coordination patterns.

![Figure 5](https://github.com/htem/BANC-project/blob/main/figures/figure_5/figure.png?raw=true)

### Figure 6: Linking CNS networks with superclusters of ANs and DNs
Integration of central brain networks (mushroom body, central complex) with behaviour-centric AN-DN modules.

![Figure 6](https://github.com/htem/BANC-project/blob/main/figures/figure_6/figure.png?raw=true)

**Supplement:** Detailed network connectivity patterns
![Figure 6 Supplement](https://github.com/htem/BANC-project/blob/main/figures/figure_6/supplement-1.png?raw=true)

---

<div align="center">

![Walking](https://github.com/htem/BANC-project/blob/main/figures/schematics/assets/walking.png?raw=true)

## Connectome Data Files

</div>

### Main Datasets

The BANC connectome data is provided as SQLite database files (10-30GB each) for efficient querying and analysis:

**Primary Dataset:**
- `banc_data.sqlite` - Complete BANC connectome with ~160,000 neurons (v626 release)
- `frankenbrain_v1.1_data.sqlite` - Integrated dataset combining FAFB brain + MANC nerve cord + BANC neck bridge
- `fafb_783_data.sqlite` - FlyWire FAFB brain connectome (reference)

### Data Access Locations

**For General Users:**
- **[Harvard Dataverse](https://doi.org/10.7910/DVN/8TFGGB)** - Public download of all datasets
- **Local Analysis:** Download datasets to local SSD (1-2TB recommended) for optimal performance

**For HMS Users:**
- **HMS O2 Cluster:** `/n/data1/hms/neurobio/wilson/banc/connectivity/`
- **Lab File Server:** `/Volumes/neurobio/wilsonlab/banc/connectivity/` (when mounted)

### Database Structure

Each SQLite file contains multiple tables optimized for different analyses:
- `meta` - Neuron metadata and annotations
- `edgelist_simple` - Neuron-to-neuron connections
- `edgelist` - Compartment-to-compartment connections  
- `presynapses/postsynapses` - Detailed synapse information
- Additional tables for influence metrics, DCVs, and neurotransmitter predictions

See `data/README.md` for detailed column descriptions and data dictionary.

---

<div align="center">

![Grooming](https://github.com/htem/BANC-project/blob/main/figures/schematics/assets/grooming.png?raw=true)

## Repository Guide

</div>

### Analysis Workflow

1. **Setup:** Configure data paths in `R/startup/banc-startup.R`
2. **Explore:** Use scripts in `R/exploration/` for custom analyses  
3. **Reproduce:** Run specific scripts in `R/figures/` to generate paper figures
4. **Visualize:** Access linked figure panels in `figures/*/links/` directories

### Key Directories

- **`figures/`** - Final publication figures (.ai, .png) with source panels in `links/` subdirectories
- **`R/figures/`** - Scripts that generate each figure panel (e.g., `panel_inventory.R` → Figure 1 panels)
- **`R/startup/`** - Configuration scripts for data loading and paths
- **`data/`** - Processed datasets, metadata, and analysis results  
- **`settings/`** - Standardized color schemes and visualization parameters
- **`submission/`** - Paper manuscript, supplemental data, and author information

### Color Schemes and Standards
Consistent visual styling is maintained through:
- `settings/paper_colours_lacroix.csv` - Standardized color palette
- `settings/meta_hierarchical_policy.csv` - Annotation display rules

---

<div align="center">

![Tactile Integration](https://github.com/htem/BANC-project/blob/main/figures/schematics/assets/tactile_integration.png?raw=true)

## Related Resources

</div>

### Connectome Context
- **[FlyWire-FAFB](https://flywire.ai)** - Adult brain connectome (~130,000 neurons)
- **[MANC](https://www.janelia.org/project-team/flyem/manc-connectome)** - Male adult nerve cord connectome
- **[HemiBrain](https://www.janelia.org/project-team/flyem/hemibrain)** - Hemibrain connectome (Janelia)
- **[Larval CNS](https://l1em.catmaid.virtualflybrain.org/)** - L1 larva complete nervous system

### Analysis Tools and Libraries
- **[natverse](https://natverse.org/)** - R packages for neuroanatomy analysis
- **[navis](https://navis.readthedocs.io/)** - Python library for neuron analysis
- **[CloudVolume](https://github.com/seung-lab/cloud-volume)** - Volumetric data access
- **[CAVE](https://caveclient.readthedocs.io/)** - Connectome annotation framework

### Community and Documentation
- **[FlyWire Academy](https://codex.flywire.ai/academy_home)** - Learning resources and community hub
- **[BANC Community](https://banc.community)** - Central hub for users and developers
- **[FlyWire Forum](https://flywire.ai/community)** - Community discussions and support
- **[Virtual Fly Brain](https://virtualflybrain.org/)** - Integrated *Drosophila* neuroscience data

**Support:** We are responsive to questions via email (see corresponding authors above) and through GitHub issues in this repository.

---

<div align="center">

![Landing](https://github.com/htem/BANC-project/blob/main/figures/schematics/assets/landing.png?raw=true)

## Database Schema Reference

</div>

An SQLite file is a lightweight, serverless, self-contained relational database engine that stores the entire database as a single file on disk. We are using them to compile connectome data for convenience. The major advantages are:

Serverless: Unlike MySQL or PostgreSQL, it doesn't require a separate server process
Portable: The entire database is contained in a single file that can be easily shared or moved
Lightweight: Low overhead, quick to set up, and requires minimal configuration
Zero-configuration: No complex setup needed
Cross-platform: Works identically across different operating systems

Here is what our connectome files contain:

**banc_data.sqlite**
```
banc_data.sqlite/
├──── meta/ * meta data annotation for each neuron in BANC, combining information from CAVE tables and our internal seatable
├──── edgelist_simple/ * all neuron-to-neuron connections so far in BANC
├──── edgelist/ * all compartment-to-comparmtent connections so far in BANC
├──── postsynapses/ * all input synapses attached to proofread BANC neurons
├──── presynapses/ * all output synapses attached to proofread BANC neurons
├──── an_dn_edgelist_simple/ * all neuron-to-neuron direct connections between neck connective neurons
├──── an_dn_edgelist/ * all compartment-to-compartment direct connections between neck connective neurons
├──── an_dn_synapses/  all output synapses attached to neck connective neurons
```

**frankenbrain_v1.1_data.sqlite**
```
frankenbrain_v1.1_data.sqlit/
├──── meta/ * meta data for each neuron in FAFB + MANC, BANC IDs used to collapse neurons cross-matched in 'neck_bridge', the 'id' field maps to entries in edgelist_simple.
├──── edgelist_simple/ * all neuron-to-neuron connections of the frankenbrain, concatenating FAFB and MANC using the 'bridge' below
├──── neck_bridge/ * our matching bridge to connect BANC neurons (pt_root_id) to their FAFB-FlyWire 783 (fafb_match) and MANC matches (manc_match)
```

**fafb_783_data.sqlite**
```
fafb_783_data.sqlite/
├──── meta/ * meta data annotation for each neuron in FAFB-Flywire 783
├──── edgelist_simple/ * all neuron-to-neuron connections so far in BANC
├──── edgelist/ * all compartment-to-comparmtent connections so far in BANC
├──── postsynapses/ * all input synapses attached to proofread BANC neurons
├──── presynapses/ * all output synapses attached to proofread BANC neurons
├──── dcv_soma/ * Yervand's somatic DCV detection and associated metrics
├──── dcv_cell/ * Stephan's cellular DCV detection
├──── neurotransmitter_predictions_v1/  all output synapses attached to neck connective neurons
```

## connectome .sqlite file data columns

meta - each row is a unique neuron
========================================================================================
***NOTE*** *entries differ between FAFB, FANC, HemiBrain, MANC and BANC neurons depending on what metadata annotators curated for those projects*

**root_id, root_783, bodyid, banc_id, cell_id, id**   :   the neuron ID for the source (i.e. upstream, presynaptic) neuron. For FAFB this is a root_id for BANC, root_783 for FAFB, cell_id for FANC and bodyid for MANC and Hemibrain.

**cell_type**     :   the name of the matched neuron from FAFB (if brain neuron or DN) or MANC (if VNC neuron or AN), hierarchical below cell_sub_class. Exceptions exist where names were further split to define single cell types

**side**    :   the side of the CNS; "L" = left, "R" = right

**hemilineage** :   the hemilineage to which the neuron is thought to belong (ito_lee_hemilineage, hartenstein_hemilineage are the same, but represent two different brain naming schemes)

**nerve**   :   entry or exit nerve

**nucleus_id**  :   the identifier of an asasociated nucleus in the given dataset, if relevant (FAFB and BANC).

**region**  :   region of the CNS; all neurons with arbours in the optic lobe are optic_lobe, all neurons that fully transit the neck connective between the brain and VNC are neck_connective

**flow** : from the perspective of the whole CNS, whether the neuron is afferent, efferent, or intrinsic

**super_class** : coarse division, hierarchical below flow

**cell_class** : hierarchical below super_class

**cell_sub_class** : hierarchical below cell_class

**modality**    :   sensory modality or motor/endocrine/behavioural association for the given neuron.

**top_nt, conf_nt**     :   the most commonly predicted (top_nt) transmitter, or the most commonly predicted when weighted by pre-synapse confidence score (conf_nt)

**top_nt_p, conf_nt_p** :   the avergae confience score at  the synapse-level, for top transmitter prediction for the neuron

**known_nt**     :   the transmitters reported to be used by the cell type in the literature

**known_nt_source**     :   the citations for that information 

**cell_type_nt**    :   the cell type level transmitter is determined by the highest {transmitter} cell type {dataset} confidence

**cell_type_nt_conf**     :   the cell type level transmitter confidence score (cell type nt) was calculated using our prediction confusion matrices.

**input_connections**     :   the total number of output links / postsynapses from the neuron

**output_connections**     :   the total number of output links / postsynapses from the neuron

**total_outputs**     :   the total number of output links / postsynapses from the neuron [UNDER CONSTRUCTION]

**axon_outputs**     :   the total number of axonal output links / postsynapses from the neuron [UNDER CONSTRUCTION]

**dend_outputs**      :   the total number of dendritic output links / postsynapses from the neuron [UNDER CONSTRUCTION]

**total_outputs_density** : the total number of output links / postsynapses from the neuron, per micron of cable [UNDER CONSTRUCTION]

**axon_outputs_density**  : the total number of axonal links / postsynapses from the neuron, per micron of cable [UNDER CONSTRUCTION]

**dend_outputs_density**  : the total number of dendritic links / postsynapses from the neuron, per micron of cable [UNDER CONSTRUCTION]

**total_inputs**      :   the total number of input links / postsynapses from the neuron [UNDER CONSTRUCTION]

**axon_inputs**       :   the total number of axonal input links / postsynapses from the neuron [UNDER CONSTRUCTION]

**dend_inputs**       :   the total number of dendritic input links / postsynapses from the neuron [UNDER CONSTRUCTION]

**total_inputs_density**  : the total number of input links / postsynapses from the neuron, per micron of cable [UNDER CONSTRUCTION]
 
**axon_inputs_density**   : the total number of axonal links / postsynapses from the neuron, per micron of cable [UNDER CONSTRUCTION]

**dend_inputs_density**   : the total number of dendritic links / postsynapses from the neuron, per micron of cable [UNDER CONSTRUCTION]

**total_length**      :   the total cable length, in microns, for the neuron

**axon_length**       :   the axonal cable length, in microns, for the neuron [UNDER CONSTRUCTION]

**dend_length**       :   the dendritic cable length, in microns, for the neuron [UNDER CONSTRUCTION]

**pd_length**         :   the primary dendrite (linker) cable length, in microns, for the neuron [UNDER CONSTRUCTION]

**pnt_length**        :   the primary neurite (cell body fibre) cable length, in microns, for the neuron [UNDER CONSTRUCTION]

**segregation_index** :   An entropy score for how segregated the neuron's synapses are into axon and dendrite [UNDER CONSTRUCTION]

**root**               :   the treenode ID (position in .swc file) of the neuron's root

**nodes**             :   the number of nodes in the neuron

**segments**          :   the number of segments in the neuron

**branch_points**     :   the number of branch points in the neuron

**endpoints**         :   the number of endpoints in the neuron

**n_trees**           :   the number of trees in the neuron, should be 1

**connectors**        :   the number of synapses in the neuron

**postsynapse_side_index** : side of the neuron that receives the most input [UNDER CONSTRUCTION]

**presynapse_side_index**  : side of the neuron that receives the most output [UNDER CONSTRUCTION]

**axon_postsynapse_side_index** : Same as postsynapse_side_index, but only for the axon [UNDER CONSTRUCTION]
 
**axon_presynapse_side_index**  : Same as presynapse_side_index, but only for the axon [UNDER CONSTRUCTION]

**dendrite_postsynapse_side_index** : Same as postsynapse_side_index, but only for the dendrite [UNDER CONSTRUCTION]

**dendrite_presynapse_side_index**  : Same as presynapse_side_index, but only for the dendrite [UNDER CONSTRUCTION]

**dcv_count** :   the number DCVs detected in this neuron

**dcv_density** :   the number DCVs detected in this neuron / `cable_length`

**dcv_soma_count** :   the number of somatic DCVs detected in this neuron

**dcv_soma_density** :   the number of somatic DCVs detected in this neuron / soma volume (?)


pre/post/synapses - each row is a unique synaptic connection
========================================================================================

**pre_id**  :   the neuron ID for the source (i.e. upstream, presynaptic) neuron. For FAFB this is a root_id for BANC, root_783 for FAFB, cell_id for FANC and bodyid for MANC and Hemibrain.

**post_id** :   the neuron ID for the target (i.e. downstream, pesynaptic) neuron. For FAFB this is a root_id for BANC, root_783 for FAFB, cell_id for FANC and bodyid for MANC and Hemibrain.

**x,y,z**   :   the  position of the connection in nanometer space for the given brain. For the franenbrain, with will be MANC or FAFB depending on the neuron.

**pre_svid**    :   the supervoxel ID for the presynaptic side of the link, not relevant for MANC or HemiBrain.

**post_svid** :   the supervoxel ID for the postsynaptic side of the link, not relevant for MANC or HemiBrain.

**prepost** :   whether the link is pre- (0, i.e. output synapse) or post (1, i.e. input) relative to post_id. In the presynapses table, all prepost==0, in the postsynaptic table, all prepost==1.

**syn_top_nt**  :   the Eckstein and Bates et al. 2023 synapse-level neurotransmitter prediction. Only valid for FAFB.

**syn_top_nt_p**    :   the confidence score assicated with syn_top_nt. Only valid for FAFB.

**gaba**    :   the Eckstein and Bates et al. 2023 synapse-level neurotransmitter prediction score for gaba. Only valid for FAFB.

**glutamate**    :   the Eckstein and Bates et al. 2023 synapse-level neurotransmitter prediction score for glutamate. Only valid for FAFB.

**acetylcholine**    :   the Eckstein and Bates et al. 2023 synapse-level neurotransmitter prediction score for acetylcholine. Only valid for FAFB.

**octopamine**    :   the Eckstein and Bates et al. 2023 synapse-level neurotransmitter prediction score for octopamine. Only valid for FAFB.

**serotonin**    :   the Eckstein and Bates et al. 2023 synapse-level neurotransmitter prediction score for serotonin. Only valid for FAFB.

**dopamine**    :   the Eckstein and Bates et al. 2023 synapse-level neurotransmitter prediction score for dopamine. Only valid for FAFB.

**scores**  :   the  Buhmamnn prediction score for the synapse, unsure of definition. Only valid for FAFB.

**cleft_scores**    :   a score that indicates how disrimiable the synaptic cleft is. More useful than `size` or `scores`.

**size**    :   the number of voxels (?) in the detected synapse.

**offset**  :   the index for the Buhmann synapse in the original .sql table.

**connector_id**  :   a unique identifer for the presynapse to which this link is associated.

**status**  :   whether the synaptic link seems good, or whether it is suspicious because it falls outside the neuropil, is on non-synaptic cable, etc.

**strahler_order**  :   the strahler order of the branch to which this synapse is attached

**flow_cent**   :   the synaptic flow at this node, see Schneider-Mizell et al. 2016

**label**   :   the compartment to which this synapse is attached, can be axon, dendrite, primary dendrite, primary neurite, unknown, soma.

**treenode_id** :   the treenode in the corresponding swc/d to which this synapse is best attached

**inside/neuropil** :   the neuropil volume inside of which this synaptic link can be found. If inside multiple volumes, they appear separated by a comma. 

**region** :   gross anatoical region in which this synapse lies, can be optic, midbrain, neck, vnc.


edgelist_simple - each row is a unique neuron-neuron connection
========================================================================================

**pre**     :   the neuron ID for the source (i.e. upstream, presynaptic) neuron. For FAFB this is a root_id for BANC, root_783 for FAFB, cell_id for FANC and bodyid for MANC and Hemibrain.

**post**    :   the neuron ID for the target (i.e. downstream, pesynaptic) neuron. For FAFB this is a root_id for BANC, root_783 for FAFB, cell_id for FANC and bodyid for MANC and Hemibrain.

**count**   :   the number of synaptic links that connect pre to post. For FAFB a cleft_score threshold of 50 has been applied.

**norm**    :   the normalised weight of a connection, this is count/post_count, where post_count are the total number of inputs to the target neuron (post).

**pre_count**  :   the total number of oututs from the target neuron (post) NOT the source neuron (pre). *I understand this is a little confusing, and will seek to change the column names to use pre/post for synapses and sourcd/target for neurons in the future.*

**post_count** :   the total number of inputs to the target neuron (post).


*other coumns with information from 'meta' may exist for convenience, with pre/post appended to the name to idicate labelled neuron*


edgelist - each row is a unique compartment-compartment connection
========================================================================================
***NOTE*** *each 'compartment' on each row is an axon/dendrite/primary neurite/primary dendrite/unknown cable for a neuron*

**pre**     :   the neuron ID for the source (i.e. upstream, presynaptic) neuron. For FAFB this is a root_id for BANC, root_783 for FAFB, cell_id for FANC and bodyid for MANC and Hemibrain.

**post**    :   the neuron ID for the target (i.e. downstrea, pesynaptic) neuron. For FAFB this is a root_id for BANC, root_783 for FAFB, cell_id for FANC and bodyid for MANC and Hemibrain.

**pre_count**  :   the total number of oututs from the target neuron (post) NOT the source neuron (pre). *I understand this is a little confusing, and will seek to change the column names to use pre/post for synapses and sourcd/target for neurons in the future.*

**post_count** :   the total number of inputs to the target neuron (post).

**pre_label**  :   the compartment of the presynaptic neuron (source), can be axon, dendrite, primary dendrite, primary neurite, unknown, soma.

**post_label**  :   the compartment of the postsynaptic neuron (target), can be axon, dendrite, primary dendrite, primary neurite, unknown, soma.

**pre_label_count**  :   the total number of oututs from the specified target neuron compartment (post+post_label) NOT the specified target neuron compartment (post+post_label). *I understand this is a little confusing, and will seek to change the column names to use pre/post for synapses and sourcd/target for neurons in the future.*

**post_label_count** :   the total number of inputs to the specified target neuron compartment (post+post_label).

**count**   :   the number of synaptic links that connect pre+pre_label to post+post_label. For FAFB a cleft_score threshold of 50 has been applied.

**norm**    :   the normalised weight of a connection, this is count/post_count NOT post_label_count, where post_count is the total number of inputs to the target neuron (post).

**norm_label**  :   the normalised weight of a connection, this is count/post_label_count NOT post_count, where post_label_count is the number of inputs to the target neuron compartment (post_post_label).

*other coumns with information from 'meta' may exist for convenience, with pre/post appended to the name to idicate labelled compartment*


dcv_cell - each row is a unique DCV detection from anywhere in a FAFB neuron
========================================================================================

**id**   :   a unique identifer for the DCV

**sv_id**   :  the supervoxel ID por this DCV's centroid 

**root_784, segment_id**   :   the unique identifier or the associated FAFB-FlyWire neuron

**x,y,z**  :   coordinates in nanomaters for the DCV's centroid

**size**    :   number of pixels in the detection, from in the 4x4x40nm v14 FAFB volume,not flywire's v14.1

**confidence**   :   a confidenced score from the detection network, for this DCV


dcv_soma - each row is a unique DCV detection from in a FAFB neuron soma
========================================================================================

**x,y,z**   : coordinates in FAFB v14.1, FlyWire space, nanometers

**center_x,center_y,center_z**   : centre of soma in FAFB v14.1, FlyWire space, raw

**index**   : section index vesicle is found in

**area**   : number of pixels

**eccentricity**   : a measure of how circular or not the segmentation is

**luminance**   : mean of pixels

**contrast**   : standard deviation of pixels

**skew**   : skew of pixels

**diameter**   : measure of size along a direction

**orientation**   : angle between x-axis and major axis of an estimate ellipse encompassing segmentation

**perimeter**   : approximate perimeter of object

**centroid**   : centre of segmentation

**centroid_weighted**   : centre of segmentation but using pixel intensities as a weight

**flywire**   : centre coordinate of vesicle in FlyWire space

**flywire_bbox_start**   : the start coordinate of the bounding box of vesicle in flywire space

**flywire_bbox_end**   : the end coordinate of the bounding box of vesicle in flywire space

**fafb**   : centre coordinate of vesicle in FAFB space

**fafb_bbox_start**   : the start coordinate of the bounding box of vesicle in FAFB space

**fafb_bbox_end**   : the end coordinate of the bounding box of vesicle in FAFB space   

**banc_data.sqlite**

## Influence

We have also calculated and compiled indirect 'influence' scores, using a linear dynamical model. See code and method [here](https://github.com/DrugowitschLab/ConnectomeInfluenceCalculator). These seed groups are explained [here](https://github.com/jasper-tms/BANC-project/blob/main/notes/seeds.md). Among the most useful are seed groups are seed_01 (run from afferents of different sensory modalities, at a coarse grain) and seed_03 (run from different sensory modalities, broken down by body part of origin as well as modality/sub-modality).

```
influence_banc_[version].sqlite/
├──── id/ * the BANC root_id for [version] for which influence scores were calculated
├──── supervoxel_id/ * the BANC supervoxel_id for id [not given yet, need to add]
├──── seed/ * name of seed group
├──── level/ *  seed group, as explained [here](https://github.com/jasper-tms/BANC-project/tree/main/python/Influence) 
├──── influence/ * influence score (no-scaling)
├──── influence_norm/ * influence / number of seeds used in run (i.e. at seed+level)
├──── influence_syn_norm/ * influence / number of synapses in seed used in run (i.e. at seed+level)
├──── influence_log/ * log(influence+very_small_constant) - min_across_seed(log(influence_norm+very_small_constant)) [i.e. the log is made to be positive]
├──── influence_norm_log/ * log(influence_norm+very_small_constant) - min_across_seed(log(influence_norm+very_small_constant))  [i.e. the log is made to be positive]
├──── influence_syn_norm/ * log(influence_syn_norm+very_small_constant) - min_across_seed(log(influence_syn_norm+very_small_constant))  [i.e. the log is made to be positive]
├──── influence_zscore z-sore/ * across all IDs in the CNS for the same seed
├──── influence_mad MAD score/ * across all IDs in the CNS for the same seed
├──── influence_zscore2 z-sore/ * across seeds in each mutually-exclusive seed group for each ID
├──── influence_mad2 MAD score/ * across seeds in each mutually-exclusive seed group for each ID
├──── influence_zscore3 z-sore/ * across all IDs in the CNS for the same seed, and across seeds in each mutually-exclusive seed group for each ID
├──── influence_mad3 MAD score/ * across all IDs in the CNS for the same seed, and across seeds in each mutually-exclusive seed group for each ID
```

## Acknowledgements

### Authors

Alexander S. Bates¹,²,*,✉, Jasper S. Phelps¹,³,*,✉, Minsu Kim¹,⁴,⁵,*, Helen H. Yang¹,*, Arie Matsliah⁶,†, Zaki Ajabi¹,†, Eric Perlman⁷,†, Kevin M. Delgado¹,†, Mohammed Abdal Monium Osman¹,†, Christopher K. Salmon⁶, Jay Gager⁶, Benjamin Silverman⁶, Sophia Renauld¹, Matthew F. Collie¹, Jingxuan Fan¹, Diego A. Pacheco¹, Yunzhi Zhao¹, Janki Patel¹, Wenyi Zhang¹, Laia Serratosa Capdevilla⁸, Ruairí J.V. Roberts⁸, Eva J. Munnelly⁸, Nina Griggs⁸, Helen Langley⁸, Borja Moya-Llamas⁸, Ryan T. Maloney⁹,¹⁰,¹¹, Szi-chieh Yu⁶, Amy R. Sterling⁶, Marissa Sorek⁶, Krzysztof Kruk¹², Nikitas Serafetinidis¹², Serene Dhawan⁶, Tomke Stürner¹³, Finja Klemm¹⁴, Paul Brooks¹⁵, Ellen Lesser¹⁶, Jessica M. Jones¹⁷, Sara E. Pierce-Lundgren¹⁷, Su-Yee Lee¹⁷, Yichen Luo¹⁷, Andrew P. Cook¹⁸, Theresa H. McKim¹⁹, Emily C. Kophs²⁰, Tjalda Falt²¹, Alexa M. Negrón Morales²², Austin Burke⁶, James Hebditch⁶, Kyle P. Willie⁶, Ryan Willie⁶, Sergiy Popovych²³, Nico Kemnitz²³, Dodam Ih²³, Kisuk Lee²³, Ran Lu²³, Akhilesh Halageri²³, J. Alexander Bae²³, Ben Jourdan²⁴, Gregory Schwartzman²⁵, Damian D. Demarest²⁶, Emily Behnke⁶, Doug Bland¹², Anne Kristiansen¹², Jaime Skelton¹², Tom Stocks¹², Dustin Garner¹², Farzaan Salman¹⁸,²⁷, Kevin C. Daly¹⁸,²⁸, Anthony Hernandez¹², Sandeep Kumar⁶, The BANC-FlyWire Consortium^, Sven Dorkenwald²⁹, Forrest Collman²⁹, Marie P. Suver²⁰, Lisa M. Fenk²¹, Michael J. Pankratz²⁶, Gregory S.X.E. Jefferis¹³, Katharina Eichler¹⁴, Andrew M. Seeds²², Stefanie Hampel²², Sweta Agrawal³⁰, Meet Zandawala³¹,³², Thomas Macrina²³, Diane-Yayra Adjavon³³, Jan Funke³³, John C. Tuthill¹⁷, Anthony Azevedo¹⁷, H. Sebastian Seung⁶,³⁴, Benjamin L. de Bivort⁹,¹⁰, Mala Murthy⁶,✉, Jan Drugowitsch¹,✉, Rachel I. Wilson¹,✉, Wei-Chung Allen Lee¹,³⁵,✉

### Affiliations

¹Department of Neurobiology, Harvard Medical School, Boston, MA, USA  
²Centre for Neural Circuit and Behaviour, University of Oxford, Oxford, UK  
³Present address: Neuroengineering Laboratory, Brain Mind Institute and Institute of Bioengineering, EPFL, Lausanne, Switzerland  
⁴Present address: Department of Molecular and Cellular Biology, Harvard University, Cambridge, MA, USA  
⁵Present address: Center for Brain Science, Harvard University, Cambridge, MA, USA  
⁶Princeton Neuroscience Institute, Princeton University, Princeton, NJ, USA  
⁷Yikes LLC, Baltimore, MD, USA  
⁸Aelysia LTD, Bristol, UK  
⁹Department of Organismic and Evolutionary Biology, Harvard University, Cambridge, MA, USA  
¹⁰Center for Brain Science, Harvard University, Cambridge, MA, USA  
¹¹Present address: Psychology Department, Colorado College, Colorado Springs, CO, USA  
¹²Eyewire, Boston, MA, USA  
¹³Neurobiology Division, MRC Laboratory of Molecular Biology, Cambridge, UK  
¹⁴Genetics Department, Leipzig University, Leipzig, Germany  
¹⁵Zoology Department, University of Cambridge, Cambridge, UK  
¹⁶Department of Molecular Genetics and Cell Biology, The University of Chicago, Chicago, IL, USA  
¹⁷Department of Neurobiology and Biophysics, University of Washington, Seattle, WA, USA  
¹⁸Department of Biology, West Virginia University, Morgantown, WV, USA  
¹⁹Department of Biology, University of Nevada Reno, Reno, NV, USA  
²⁰Department of Biological Sciences, Vanderbilt University, Nashville, TN, USA  
²¹Max Planck Institute for Biological Intelligence, Martinsried, Germany  
²²Institute of Neurobiology, University of Puerto Rico Medical Sciences Campus, San Juan, Puerto Rico  
²³Zetta AI LLC, Sherrill, NY, USA  
²⁴School of Informatics, University of Edinburgh, Edinburgh, UK  
²⁵Japan Advanced Institute of Science and Technology (JAIST), Nomi, Japan  
²⁶Molecular Brain Physiology and Behavior, LIMES Institute, University of Bonn, Bonn, Germany  
²⁷Present address: Department of Neurobiology, Harvard Medical School, Boston, MA, USA  
²⁸Department of Neuroscience, West Virginia University, Morgantown, WV, USA  
²⁹Allen Institute for Brain Science, Seattle, WA, USA  
³⁰School of Neuroscience, Virginia Tech, Blacksburg, VA, USA  
³¹Department of Biochemistry and Molecular Biology, University of Nevada Reno, Reno, NV, USA  
³²Neurobiology and Genetics, Theodor-Boveri-Institute, Biocenter, Julius-Maximilians-University of Würzburg, Am Hubland, Würzburg, Germany  
³³HHMI Janelia, Ashburn, VA, USA  
³⁴Computer Science Department, Princeton University, Princeton, NJ, USA  
³⁵F.M. Kirby Neurobiology Center, Boston Children's Hospital, Harvard Medical School, Boston, MA, USA

\* These authors contributed equally to this work  
† These authors contributed equally to this work  
^ A list of additional members and their affiliations appears in The BANC-FlyWire Consortium

✉ **Correspondence:** Alexander_Bates@hms.harvard.edu, jasper.s.phelps@gmail.com, mmurthy@princeton.edu, jan_drugowitsch@hms.harvard.edu, rachel_wilson@hms.harvard.edu, wei-chung_lee@hms.harvard.edu

### Data Collection and Processing
- **Electron Microscopy:** Minsu Kim and Jasper S. Phelps (Harvard Medical School, Lee Laboratory)
- **AI Segmentation and Technical Infrastructure:** [Zetta.ai](https://zetta.ai/) provided advanced AI segmentation and technical infrastructure
- **Bulk Proofreading:** [SixEleven](https://www.sixelevenbpo.com/) performed bulk proofreading of neurons to backbone completion, representing a majority of the proofreading effort
- **Fine Proofreading:** [Aelysia](https://aelysia.eu/) conducted fine proofreading for detailed reconstruction and connectivity
- **Community Collaboration:** [FlyWire](https://flywire.ai/) consortium enabled community-based annotation and collaboration
- **Connectome Integration:** BANC project team with FlyWire and MANC collaborations

### Funding and Support
This work was supported by grants from the NIH, NSF, and international funding agencies. Full funding details are provided in the paper acknowledgements.

### Citation
If you use BANC data or code, please cite:
*[Paper citation to be added upon publication]*

### License
Data and code are released under open-source licenses to facilitate scientific research and collaboration.

---

<div align="center">

![Takeoff](https://github.com/htem/BANC-project/blob/main/figures/schematics/assets/takeoff.png?raw=true)

*Thank you for exploring the BANC connectome!*

</div>


