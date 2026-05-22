# Data and resource locations
## These are locations with data we want to copy onto the Harvard Dataverse

## Documentation

### BANC column documentation
https://github.com/sjcabs/fly_connectome_data_tutorial/blob/main/data/dataset_documentation/banc_data.md

### Acknowledgements
/Users/papers/BANC-project/acknowledgements.md

### Bibliography
/Users/papers/BANC-project/manuscript/print/bibliography.bib

### Runtime requirements (R + Python versions, library/package versions, install recipe)
/Users/papers/BANC-project/requirements.txt

## Compiled data products for analysis

###  BANC meta data
lee-lab_brain-and-nerve-cord-fly-connectome/compiled_data/banc_888/banc_888_meta.feather

###  BANC metrics per neuron
lee-lab_brain-and-nerve-cord-fly-connectome/compiled_data/banc_888/banc_888_metrics.feather

###  BANC neurotransmitter predictions results per neuron
lee-lab_brain-and-nerve-cord-fly-connectome/compiled_data/banc_888/banc_888_neurotransmitter_prediction.csv

###  BANC SWC files (to ZIP)
lee-lab_brain-and-nerve-cord-fly-connectome/compiled_data/banc_888/banc_banc_space_swc

###  BANC v2 synapses raw
lee-lab_brain-and-nerve-cord-fly-connectome/neuron_connectivity/v888/synapses_v2_human_readable.csv.gz

###  BANC v3 synapses raw
lee-lab_brain-and-nerve-cord-fly-connectome/neuron_connectivity/v888/synapses_v3_human_readable.csv.gz

###  BANC v2 synapses enriched
lee-lab_brain-and-nerve-cord-fly-connectome/compiled_data/banc_888/banc_888_synapses_v2_enriched.parquet

###  BANC v3 synapses enriched
lee-lab_brain-and-nerve-cord-fly-connectome/compiled_data/banc_888/banc_888_synapses_v3_enriched.parquet

###  BANC v2 neuron-neuron edgelist (size threshold of >= 5 applied on BANC v2 synapses)
lee-lab_brain-and-nerve-cord-fly-connectome/compiled_data/banc_888/banc_888_edgelist_simple_v2.feather

###  BANC v3 neuron-neuron edgelist (size threshold of >= 10 applied on BANC v3 synapses)
lee-lab_brain-and-nerve-cord-fly-connectome/compiled_data/banc_888/banc_888_edgelist_simple_v3.feather

###  BANC v2 neuronal compartment-compartment edgelist
lee-lab_brain-and-nerve-cord-fly-connectome/compiled_data/banc_888/banc_888_edgelist_split_v2.feather

###  BANC v2 synapses' neurotransmitter classifications
gs://lee-lab_brain-and-nerve-cord-fly-connectome/synapses/v2.0/banc_nt_prediction_w_sizethresh_5_11102025.parquet

###  BANC v3 synapses' neurotransmitter classifications 
gs://lee-lab_brain-and-nerve-cord-fly-connectome/synapses/v3.0/banc_nt_prediction_v3_w_sizethresh_10_05042026.parquet

###  BANC v2 synapses' neuropil locations
gs://lee-lab_brain-and-nerve-cord-fly-connectome/synapses/v2.0/synapse_neuropil_lookup_v2.parquet

###  BANC v3 synapses' neuropil locations 
gs://lee-lab_brain-and-nerve-cord-fly-connectome/synapses/v3.0/synapse_neuropil_lookup_v3.parquet

###  BANC v2 manual review synapse sample
/Users/papers/BANC-project/data/synapses/2024-09-20_aelysia_synapse_sample_complete.csv

##   BANC dataset problem regions (rename banc_problem_regions.csv)
/Users/papers/BANC-project/manuscript/print/supplemental_data/supplemental_data_10.csv

###  BANC supplementary data (to ZIP — the 10 supplemental_data_*.csv files cited in the paper)
/Users/papers/BANC-project/manuscript/print/supplemental_data

###  BANC CNS network spectral clustering (v2 + v3)
gs://lee-lab_brain-and-nerve-cord-fly-connectome/compiled_data/banc_888/banc_888_cns_network_spectral_clustering_v2.csv
gs://lee-lab_brain-and-nerve-cord-fly-connectome/compiled_data/banc_888/banc_888_cns_network_spectral_clustering_v3.csv

###  BANC betweenness centrality (all-to-all + afferent-to-efferent, v2 + v3)
gs://lee-lab_brain-and-nerve-cord-fly-connectome/compiled_data/banc_888/banc_888_betweenness_all_to_all_v2.csv
gs://lee-lab_brain-and-nerve-cord-fly-connectome/compiled_data/banc_888/banc_888_betweenness_all_to_all_v3.csv
gs://lee-lab_brain-and-nerve-cord-fly-connectome/compiled_data/banc_888/banc_888_betweenness_afferent_to_efferent_v2.csv
gs://lee-lab_brain-and-nerve-cord-fly-connectome/compiled_data/banc_888/banc_888_betweenness_afferent_to_efferent_v3.csv

###  BANC neuron meshes (to ZIP)
lee-lab_brain-and-nerve-cord-fly-connectome/imported_meshes/banc_meshes

###  BANC color MIP files
lee-lab_brain-and-nerve-cord-fly-connectome/neuron_colormips

###  BANC neuropil meshes
lee-lab_brain-and-nerve-cord-fly-connectome/region_outlines

###  BANC all-to-all influence (sharded)
lee-lab_brain-and-nerve-cord-fly-connectome/compiled_data/banc_888/influence/all_to_all

###  BANC all to effector influence
lee-lab_brain-and-nerve-cord-fly-connectome/compiled_data/banc_888/influence/influence_all_to_effector_subclass.parquet

###  BANC sensory to all influence 
lee-lab_brain-and-nerve-cord-fly-connectome/compiled_data/banc_888/influence/influence_sensory_subclass_to_all.parquet

###  JRC2018F to BANC brain elastix registration (to ZIP)
lee-lab_brain-and-nerve-cord-fly-connectome/registrations/brain_240721

###  JRC2018VNCF to BANC elastix registration (to ZIP)
lee-lab_brain-and-nerve-cord-fly-connectome/registrations/vnc_240721

## NBLAST results

### BANC to FAFB v783
lee-lab_brain-and-nerve-cord-fly-connectome/nblast/banc_fafb_783_nblast.feather

###  BANC to HemiBrain v121
lee-lab_brain-and-nerve-cord-fly-connectome/nblast/banc_hemibrain_v1.2.1_nblast.feather

###  BANC to MANC v112
lee-lab_brain-and-nerve-cord-fly-connectome/nblast/banc_manc_v1.2.1_nblast.feather

###  BANC to FANC v116
lee-lab_brain-and-nerve-cord-fly-connectome/nblast/banc_fanc_1116_nblast.feather

###  BANC to maleCNS
lee-lab_brain-and-nerve-cord-fly-connectome/nblast/banc_malecns_v0.9_nblast.feather

###  BANC to BANC-mirrored
lee-lab_brain-and-nerve-cord-fly-connectome/nblast/banc_mirror_nblast.feather

###  BANC to BANC-native
lee-lab_brain-and-nerve-cord-fly-connectome/nblast/banc_native_nblast.feather

###  BANC microCT
lee-lab_brain-and-nerve-cord-fly-connectome/microCT

###  BANC-JRC2018 template spaces
lee-lab_brain-and-nerve-cord-fly-connectome/templates

## CAVE tables (project annotation records)

###  BANC backbone proofread neurons
gs://lee-lab_brain-and-nerve-cord-fly-connectome/neuron_annotations/v888/backbone_proofread.parquet

###  BANC cell info (community annotations)
gs://lee-lab_brain-and-nerve-cord-fly-connectome/neuron_annotations/v888/cell_info.parquet

###  BANC representative points table (keys to master annotation table, codex_annotations)
gs://lee-lab_brain-and-nerve-cord-fly-connectome/neuron_annotations/v888/cell_representative_point.parquet

###  BANC annotations for FlyWire Codex (curated by core BANC team)
gs://lee-lab_brain-and-nerve-cord-fly-connectome/neuron_annotations/v888/codex_annotations.parquet

###  Neck connective seed plane
gs://lee-lab_brain-and-nerve-cord-fly-connectome/neuron_annotations/v888/neck_connective_y92500.parquet

###  Nerve seed planes and names
gs://lee-lab_brain-and-nerve-cord-fly-connectome/neuron_annotations/v888/peripheral_nerves.parquet

###  Proofreading notes table (inc. roughly proofread)
gs://lee-lab_brain-and-nerve-cord-fly-connectome/neuron_annotations/v888/proofreading_notes.parquet

###  BANC's detected nuclei 
gs://lee-lab_brain-and-nerve-cord-fly-connectome/neuron_annotations/v888/somas_v1.parquet

###  BANC's detected mitochondria
MISSING

###  Neuroglancer states for the paper
https://github.com/jasper-tms/the-BANC-fly-connectome/tree/main/neuroglancer_states/2026a

### Behavioral characterisation of the BANC fly
lee-lab_brain-and-nerve-cord-fly-connectome/behavior

## Analysis and data-wrangling repositories (each to ZIP)

###  R client for BANC data
https://github.com/natverse/bancr/

###  R version of our influence code
https://github.com/natverse/influencer

###  Python version of our influence code
https://github.com/DrugowitschLab/ConnectomeInfluenceCalculator

###  Neurotransmitter-prediction model + ground truth for BANC
https://github.com/htem/synister_banc

###  R ggplot2 neuron plotting code
https://github.com/natverse/nat.ggplot/

###  BANC python tools and workflows
https://github.com/jasper-tms/the-BANC-fly-connectome

###  BANC python client (pip-installable distribution of the above)
https://pypi.org/project/banc/

###  R + Python connectome data access and analysis tutorial
https://github.com/sjcabs/fly_connectome_data_tutorial

###  Documentation for the finding of known fast-acting neuropeptide usage in Drosophila
https://github.com/funkelab/drosophila_neuropeptides

###  Documentation for finding neurons of known fast-acting neurotransmitter usage in Drosophila
https://github.com/funkelab/drosophila_neurotransmitters

###  BANC data processing pipeline (R + O2)
https://github.com/htem/bancpipeline

###  The BANC data analysis code and plot files supporting the paper
https://github.com/htem/BANC-project









