# Dataset Description for the BANC v888 Harvard Dataverse

This file holds the **dataset-level** description posted to the Dataverse
citation block (the `dsDescriptionValue` field via the Dataverse API).
Format matches our preprint Dataverse README: short bullets, HTML
`<br>` paragraph breaks, `<b>`/`<i>` allowed (Dataverse renders these
in the UI). The text deliberately avoids em dashes so it renders
consistently across the UI, DataCite XML, and downstream aggregators.

---

This repository contains the data associated with the <b>published version</b> of the manuscript "Distributed control circuits across a brain-and-cord connectome" (Bates, Phelps, Kim, Yang et al., <i>Nature</i>, 2026; open access; DOI: 10.1038/s41586-026-10735-w; https://www.nature.com/articles/s41586-026-10735-w; preprint v3: https://www.biorxiv.org/content/10.1101/2025.07.31.667571v3). The dataset represents a synapse-resolution reconstruction and annotation of the adult female <i>Drosophila melanogaster</i> central nervous system, spanning both the brain and ventral nerve cord (VNC).<br>
<br>
Included in this repository are:<br>
* Cell annotations: including soma locations, proofreading status, cell type assignments and information about cell functions<br>
* Neurotransmitter predictions<br>
* Connectivity matrices (v2 synapse-prediction snapshot, used in the paper; and the newer v3 snapshot for downstream work)<br>
* NBLAST similarity results between BANC neurons and neurons from existing connectomes including HemiBrain, FAFB (Full Adult Fly Brain), FANC (Female Adult Nerve Cord), MANC (Male Adult Nerve Cord) and maleCNS, as well as comparisons within BANC<br>
* L2 skeletal representations of neurons, generated using the pcg_skel tool<br>
* colorMIP images of all BANC neurons<br>
* Influence scores from defined source neuron groups (e.g., sensory), computed using the linear dynamical model described in the manuscript, plus the full all-to-all influence matrix<br>
* Aligned metadata linking data elements to the cell types and analyses presented in the BANC manuscript<br>
* Snapshot ZIPs of the BANC analysis code stack (bancr, bancpipeline, BANC-project, influencer, ConnectomeInfluenceCalculator, nat.ggplot, synister_banc, the-BANC-fly-connectome, fly_connectome_data_tutorial). Each code archive carries its own Zenodo DOI, listed in the per-archive documentation under `code/`; please cite the Zenodo DOI when referring to a specific software component.<br>
* Supplementary Information files from the paper<br>
<br>
The downloadable data is based on CAVE materialization <b>888</b>, snapshotted on April 17, 2026, and provides a stable reference for the analyses and figures in the published paper.<br>
<br>
The aligned EM image data, the flat v888 segmentation, and a long-term archive of the per-neuron mesh layer all live on BossDB (https://bossdb.org/project/bates_phelps_kim_yang2025, DOI: 10.60533/boss-2025-941r). The live, evolving reconstruction is browsable through FlyWire Codex (https://codex.flywire.ai/banc) and the BANC portal (https://banc.community), and is accessible programmatically through CAVE, the Connectome Annotation Versioning Engine (public datastack at https://global.daf-apis.com/info/datastack/brain_and_nerve_cord_public). The bulk-data source bucket is gs://lee-lab_brain-and-nerve-cord-fly-connectome/.<br>
<br>
For visual cross-dataset comparison, the BANC viewer at https://ng.banc.community/view shows the BANC EM image data and annotated meshes from v888 in a single Neuroglancer scene alongside registered neuron meshes from FAFB, FANC, HemiBrain, maleCNS and MANC. This makes it straightforward to compare a BANC neuron with its homologue or matched cell type in any of the other adult fly connectomes, without having to set up the cross-dataset registrations or load each dataset yourself.<br>
<br>
A brief guide on navigating our data: Neurons in our dataset were tracked using the CAVE (Connectome Annotation Versioning Engine) system; the public BANC datastack is at https://global.daf-apis.com/info/datastack/brain_and_nerve_cord_public. Neurons have a 'root_id', a 64-bit integer that identifies a unique neuron state, which changes each time a neuron is 'edited'. Each identified neuron in materialization 888 has a unique 'root_id'. To track 'neurons', we have tracked 'stable' points within neurons ('position') and the underlying fixed segmentation ID for an atomic set of segmented voxels associated with that position, i.e. a 'supervoxel_id'. A 'position' (a voxel) can give you a 'supervoxel_id' (a small, uneditable collection of voxels) and a 'supervoxel_id' can give you a 'root_id' (a large editable collection of supervoxels). Synaptic links also have their own 'id'. Columns appended with 'pt_' indicate that the given information was tracked from a point position that a user added to a CAVE table. A neuron is marked 'backbone_proofread' once its primary neurites or major microtubule-rich processes have been manually reviewed end to end; in 'backbone_proofread' neurons the overall morphology has been confirmed and is not expected to change radically with further work, although minor branches or a small number of synapses may still be refined. The lighter category 'roughly_proofread' identifies neurons that are recognisable but may still be missing larger branches (often due to local data artefacts); these are useful for cell type calls but not for fine connectivity. The v626 (preprint) and v850 (interim) root_ids are also retained as join keys, so users coming from preprint-era resources can cross-reference identities into v888.<br>
<br>
Researchers are encouraged to use these data in conjunction with the online resources on the BANC portal (https://banc.community) and FlyWire Codex (https://codex.flywire.ai/banc) for further annotation, exploration, and integration with community datasets. While this Dataverse is a stable snapshot of the data we used in the published paper, FlyWire Codex will serve more up-to-date and corrected data. The BANC is a live project.

(Updated 2026-06-04.)
