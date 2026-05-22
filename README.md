<div align="center">

<img src="https://github.com/htem/BANC-project/blob/main/images/banc-poster-new-neurons.png?raw=true"
     width="820" alt="BANC poster artwork by Amy Sterling">

<sub>Banner artwork by <a href="https://www.amysterling.org/">Amy Sterling</a></sub>

# The Brain-And-Nerve-Cord (BANC) connectome

### *Distributed control circuits across a brain-and-cord connectome*

**First authors:** [Alexander S. Bates](https://scholar.google.com/citations?user=BOVTiXIAAAAJ&hl=en), [Jasper S. Phelps](https://scholar.google.com/citations?user=HT5N2EIAAAAJ&hl=en), [Minsu Kim](https://www.linkedin.com/in/mindy-minsu-kim), [Helen H. Yang](https://orcid.org/0000-0001-5140-9664)
**Corresponding authors:** Mala Murthy, Jan Drugowitsch, [Rachel I. Wilson](https://scholar.google.com/citations?user=T-RODd8AAAAJ&hl=en), [Wei-Chung Allen Lee](https://scholar.google.com/citations?user=y2s07ssAAAAJ&hl=en)

[![bioRxiv](https://img.shields.io/badge/bioRxiv-2025.07.31.667571v3-bd2c00?style=flat)](https://www.biorxiv.org/content/10.1101/2025.07.31.667571v3)
[![PubMed](https://img.shields.io/badge/PubMed-40766407-1f6feb?style=flat)](https://pubmed.ncbi.nlm.nih.gov/40766407/)
[![Harvard Dataverse](https://img.shields.io/badge/Dataverse-10.7910%2FDVN%2F7WTH1N-orange?style=flat)](https://doi.org/10.7910/DVN/7WTH1N)
[![License: CC BY 4.0](https://img.shields.io/badge/License-CC%20BY%204.0-green?style=flat)](https://creativecommons.org/licenses/by/4.0/)

</div>

---

## About the BANC

The **Brain-And-Nerve-Cord (BANC)** is the first synapse-resolution connectome that unites the brain and ventral nerve cord of an animal. It is a reconstruction of the central nervous system of one adult female *Drosophila melanogaster* — approximately 188,000 neurons and 199 million predicted synapses, spanning the brain, suboesophageal zone, cervical connective, and the entire ventral nerve cord. Imaged at 4 nm in-plane resolution by serial-section electron microscopy, segmented and proofread by a community of researchers and citizen scientists, and annotated for cell type, neurotransmitter, hemilineage, behavioural function, and cross-dataset identity.

This repository accompanies the paper *Distributed control circuits across a brain-and-cord connectome* (Bates, Phelps, Kim, Yang et al., 2026). It holds the R analysis pipeline that produced every figure and statistical result, the per-figure illustrator-ready PDFs and tabular outputs, and a committed snapshot of the metadata needed to reproduce the analyses end-to-end. Larger data products (per-synapse tables, influence parquets, neuron meshes) are kept on the [Harvard Dataverse deposit](https://doi.org/10.7910/DVN/7WTH1N) and are pulled on demand by the analysis scripts.

<div align="center">

<img src="https://github.com/htem/BANC-project/blob/main/images/tyler-sloan/brain_vnc_neck_1000s_100p.png?raw=true" width="540"
     alt="Schematic overview of the BANC connectome by Tyler Sloan">

<sub>Schematic by <a href="https://www.thetransmitter.org/contributor/tyler-sloan/">Tyler Sloan</a></sub>

</div>

## What the paper reports

- A unified synapse-resolution connectome of the entire central nervous system of an adult fly — brain plus ventral nerve cord plus the cervical connective that bridges them.
- A new "influence" metric that quantifies indirect functional connections by propagating signals through the synaptic graph, calibrated against simpler graph-traversal baselines.
- Evidence that effector neurons are influenced most strongly by sensory neurons from the same body part, forming local sensori-motor feedback loops at the level of individual body parts.
- Long-range coordination of those local loops by ascending and descending neurons (ANs and DNs), organised into behaviour-centric clusters spanning flight, walking, head orienting, grooming, feeding, reproduction, postural control, and visceral state.
- Mushroom body and central-complex circuits acting as supervisors of these motor clusters rather than as a single central controller, supporting a distributed-control architecture for behaviour.

<div align="center">

<img src="https://github.com/htem/BANC-project/blob/main/images/tyler-sloan/modules__loops_50s_50p.png?raw=true" width="540"
     alt="Local sensorimotor feedback loops, by Tyler Sloan">

</div>

## What is in this repository

The repository is organised so that any figure or statistic in the paper can be traced back to the script that produced it. The R code reads from a committed metadata snapshot (`data/meta/banc_888_meta_<date>.parquet`) and pulls larger data products (edgelists, synapses, influence parquets) from the Dataverse / Google Cloud Storage deposit at runtime.

```
BANC-project/
├── R/
│   ├── startup/           # Configuration: paths, GCS helpers, metadata loader,
│   │                      # private-key loader. Sourced by every figure script.
│   ├── figures/           # One script per figure (or per closely-related group
│   │                      # of panels). The name maps to the figure number.
│   └── text/              # Scripts that produce non-figure outputs:
│                          # numbers.R compiles the in-text statistics into a
│                          # CSV and updates the manuscript variable sheet;
│                          # supplemental_data.R writes the ten supplementary
│                          # CSV tables; nblast_top_match_correct.R recomputes
│                          # the cross-dataset match accuracy table.
├── figures/               # Per-figure assets. Each figure_N/ contains the
│   │                      # Adobe Illustrator file plus a links/ tree with
│   │                      # the individual panels (vector PDFs, PNGs, .txt
│   │                      # statistics sidecars). The .ai files reference
│   │                      # the linked PDFs by relative path.
│   └── schematics/        # Author-original schematics referenced by figures.
├── data/
│   ├── meta/              # The bundled v888 metadata snapshot
│   │                      # (banc_888_meta_<YYYYMMDD>.parquet, 63 MB).
│   ├── private/           # Drive identifiers and other private keys
│   │                      # (gitignored; not present in the public clone).
│   └── synapses/          # Small manually-reviewed synapse samples used
│                          # for the synapse-prediction calibration figures.
├── settings/              # Colour schemes, paper-style ggplot themes.
├── manuscript/print/      # Paper-derived deposits — most importantly the
│                          # Dataverse upload workspace under
│                          # manuscript/print/dataverse/ (the manifest, the
│                          # per-file documentation, and the upload scripts).
├── images/                # Banner artwork, schematic illustrations (incl.
│   └── tyler-sloan/       # the five README schematics), and reference notes.
├── BANC-project.Rproj     # RStudio project file.
└── README.md
```

The companion development repo is [`jasper-tms/BANC-project`](https://github.com/jasper-tms/BANC-project); this `htem/BANC-project` is the version of record that ships with the paper.

## Quick start

```bash
git clone https://github.com/htem/BANC-project.git
cd BANC-project

# Install R dependencies (see DESCRIPTION-style notes in R/startup/banc-startup.R).
# The minimum is: arrow, readr, dplyr, ggplot2, igraph, reticulate, bancr.

# Reproduce, for example, Figure 3a (betweenness by super class):
BANC_NCORES=1 Rscript R/figures/panels_betweenness_layers.R
```

Most figure scripts assume the working directory is the repo root and read the metadata snapshot from `data/meta/`. Larger data products (edgelists, synapse parquets) are downloaded on first use from the project's GCS bucket; the download is cached locally under `data/cache/`. Long-running analyses respect `BANC_NCORES=1` to keep memory predictable.

<div align="center">

<img src="https://github.com/htem/BANC-project/blob/main/images/tyler-sloan/100_largest_100s_50p.png?raw=true" width="540"
     alt="Ascending and descending neuron clusters, by Tyler Sloan">

</div>

## Reproducing a figure

Each main-text figure has a generating script in `R/figures/`. The mapping is roughly:

| Figure | Generator script | Notes |
|---|---|---|
| 1 | `panels_inventory.R`, `panels_neuroanatomy.R`, `panels_proofread_matching.R` | Dataset inventory, completion stats, cross-dataset matching. |
| 2 | `panels_sensory_motor.R`, `panels_influence_validation.R`, `panels_body_parts.R` | Influence validation; local sensori-motor loops. |
| 3 | `panels_betweenness_layers.R`, `panels_an_dn_umap.R`, `panels_an_dn_connectivity.R` | AN/DN betweenness and PCA-UMAP clustering. |
| 4 | `panels_cluster_sensory_correlations.R`, `panels_cell_type_blowouts.R` | Within-cluster specialisation. |
| 5 | `panels_an_dn_influence.R` | Coordination between behaviour-centric clusters. |
| 6 | `panels_cns_networks.R`, `panels_cns_network_analyses.R`, `panels_mbx_cx_control.R` | CNS networks; supervisory inputs from MB and CX. |

Each script writes its outputs into `figures/figure_N/links/` (and `links/supplement/` for extended-data panels), where the corresponding `figure_N.ai` is set up to link them. Statistical sidecars are written next to each PDF as a matching `.txt` so the numbers in the paper are reproducible without rerunning the analysis.

<div align="center">

<img src="https://github.com/htem/BANC-project/blob/main/images/tyler-sloan/epg_kc_modules_360s_50p.png?raw=true" width="540"
     alt="CNS networks supervising behaviour clusters, by Tyler Sloan">

</div>

## Data resources

Browse and download:

- **[Harvard Dataverse — BANC v888 deposit](https://doi.org/10.7910/DVN/7WTH1N)** — the citable static snapshot of the connectome. Per-neuron metadata, neuron-to-neuron edgelists (v2 and v3), per-synapse tables, neurotransmitter predictions, betweenness and spectral-clustering tables, the all-to-all influence parquets, NBLAST cross-dataset similarity matrices, neuron meshes, SWC skeletons, registration template volumes, code archives, and the full Neuroglancer state JSONs.
- **[FlyWire Codex](https://codex.flywire.ai/banc)** — the most up-to-date version of the connectome (continues to evolve past the snapshot), with an interactive neuron browser and a clean export for programmatic use.
- **[Neuroglancer at ng.banc.community](https://ng.banc.community/view)** — 3D visualisation of every cell type, neuropil, microCT layer, and the Neuroglancer states behind each figure.
- **[manuscript/print/banc_data_locations.md](manuscript/print/banc_data_locations.md)** — a single index of every BANC data product and where it lives (Dataverse path, GCS bucket path, and companion-repo links), grouped by category (metadata, edgelists, synapses, influence, meshes, registrations, …). Start here if you're looking for a specific file.

## The BANC universe

The full BANC software stack is split across several repositories, each
with a specific scope. Static snapshots of every repo are archived on
the [Harvard Dataverse deposit](https://doi.org/10.7910/DVN/7WTH1N) as
companion `.zip` files; the canonical live URLs are below. A
comprehensive index of community tools is also at the project hub
[banc.community](https://banc.community) and the
[FlyWire Apps portal](https://flywire.ai/apps).

### Client libraries (use these to query BANC)

- **[bancr](https://github.com/natverse/bancr)** (R) — the natverse-compatible R client for CAVE, GCS, and synapse-query endpoints. Used by every analysis script in this repository. Headline calls: `banc_meta()`, `banc_partners()`, `banc_edgelist()`, `banc_influence()`, `banc_nblast_matches()`, `banc_read_neuron_meshes()`, `banc_view()`.
- **[banc](https://pypi.org/project/banc/)** (Python, PyPI) — Python client for querying / analysing BANC; companion to bancr.
- **[nat.ggplot](https://github.com/natverse/nat.ggplot)** (R) — natverse extension for neuroanatomical ggplot panels (used throughout `R/figures/panels_*.R` to render the side-by-side anatomy figures).
- **[CAVEclient / fafbseg-py](https://caveclient.readthedocs.io/)** (Python) — the underlying CAVE access layer; bancr uses these under the hood via reticulate.

### Pipelines and models (these built the data products in this repo)

- **[bancpipeline](https://github.com/htem/bancpipeline)** — the post-proofreading data pipeline. Produces `compiled_data/banc_888/*` on GCS (the cached connectivity / metadata / metric feathers everything else depends on). Covers skeletonisation, neuron matching, axon-dendrite splitting, neuropil assignment, synapse enrichment, betweenness, spectral clustering, completion metrics. Most data subdirectories in [`data/`](data/) cross-reference a script in here.
- **[ConnectomeInfluenceCalculator](https://github.com/DrugowitschLab/ConnectomeInfluenceCalculator)** (Python, PETSc-backed) — the influence metric of Methods §"Influence", Eqs. 1–10. Computes the sparse-graph steady-state activation that underlies Figure 2 onwards. Archived at Zenodo DOI [`10.5281/zenodo.17693838`](https://doi.org/10.5281/zenodo.17693838).
- **[influencer](https://github.com/natverse/influencer)** (R) — R wrapper around `ConnectomeInfluenceCalculator`; provides `influence_calculator_py()`, `query_influence()`, `count_thresh = 5` filtering, parallel PSOCK dispatch.
- **[synister_banc](https://github.com/htem/synister_banc)** — training and inference code for the BANC fast-acting neurotransmitter classifier. Outputs land in [`data/synapse_nt/`](data/synapse_nt/) (Methods §"Neurotransmitter prediction").
- **[drosophila_neurotransmitters](https://github.com/funkelab/drosophila_neurotransmitters)** (Funke Lab) — upstream NT-prediction infrastructure used by `synister_banc`.
- **[drosophila_neuropeptides](https://github.com/funkelab/drosophila_neuropeptides)** (Funke Lab) — companion neuropeptide-prediction model.

### Project hub + community resources

- **[the-BANC-fly-connectome](https://github.com/jasper-tms/the-BANC-fly-connectome)** — project home for proofreading guides, annotation taxonomy, the Neuroglancer state catalogue (canonical Neuroglancer scenes referenced from the paper figures), and the Slack annotation-aide bot.
- **[murthylab/codex](https://github.com/murthylab/codex)** — the FlyWire Codex web app that powers [codex.flywire.ai/banc](https://codex.flywire.ai/banc): interactive neuron browser, connectivity / annotation download endpoints, the colour-MIP search interface, and the per-cell-type tooltip + column dictionary referenced from the data subdir READMEs.
- **[fly_connectome_data_tutorial](https://github.com/sjcabs/fly_connectome_data_tutorial)** — Python + R course-format walkthrough of working with BANC, FAFB, MANC, hemibrain, and maleCNS together. Authored by **Alexander S. Bates** for **[SJCABS](https://sjcabs.com/)** (the *San Juan Winter School on Connectomics and Brain Simulation*). Designed so a graduate-level reader (or their LLM) can learn the data stack end-to-end. **Start here if you're new to the fly connectome data stack** — including for figuring out which BANC functions to call from this repo.

### This repository

- **[htem/BANC-project](https://github.com/htem/BANC-project)** — the public release of this analysis code. Contains exactly the scripts that produced every figure and statistical result in the paper, plus the bundled metadata snapshot needed to re-run them.

## Authenticated edit access

The above resources are read-only for general users. Editing the segmentation or contributing annotations requires authenticated CAVE access. To request access, contact the [BANC Slack workspace](https://fanc-reconstruction.slack.com/) and read the [BANC proofreading and data ownership guidelines](https://fanc-reconstruction.slack.com/archives/C01RZP5JH9C/p1617404290005300).

<div align="center">

<img src="https://github.com/htem/BANC-project/blob/main/images/tyler-sloan/epg_kc_modules_above-lit_360s_50p.png?raw=true" width="540"
     alt="Distributed control architecture, by Tyler Sloan">

</div>

## Citation

Please cite both the paper and the Dataverse deposit:

```bibtex
@article{bates2026banc,
  author  = {Bates, Alexander S. and Phelps, Jasper S. and Kim, Minsu and Yang, Helen H. and {others (BANC-FlyWire Consortium)}},
  title   = {Distributed control circuits across a brain-and-cord connectome},
  journal = {Nature},
  year    = {2026},
  note    = {bioRxiv preprint: https://doi.org/10.1101/2025.07.31.667571}
}

@misc{bates2026bancdata,
  author = {Bates, Alexander S. and {others}},
  title  = {BANC v888 — Brain-and-nerve-cord connectome (data and analysis code)},
  year   = {2026},
  doi    = {10.7910/DVN/7WTH1N},
  url    = {https://doi.org/10.7910/DVN/7WTH1N},
  note   = {Harvard Dataverse}
}
```

## Acknowledgements

This codebase was led by [Alexander Shakeel Bates](https://scholar.google.com/citations?user=BOVTiXIAAAAJ&hl=en) in the lab of [Rachel Wilson](https://neuro.hms.harvard.edu/faculty-staff/rachel-wilson) at Harvard Medical School, and working with [Wei-Chung Allen Lee](https://www.lee.hms.harvard.edu/), and with help from [Helen Yang](https://scholar.google.com/citations?user=T5rocQ0AAAAJ&hl=en), Jasper Phelps, Mo Osman and Tatsuo Okubo.

This work is the product of a large collaborative effort. The full author list, affiliations, and detailed acknowledgements are in the paper and in the [Dataverse front-matter](https://doi.org/10.7910/DVN/7WTH1N).

Original artwork in this README is by **Amy Sterling** (banner) and **Tyler Sloan** ([thetransmitter.org/contributor/tyler-sloan](https://www.thetransmitter.org/contributor/tyler-sloan/), the five schematics).

The BANC connectome would not exist without the BANC-FlyWire Consortium of proofreaders and annotators, the SixEleven proofreading team, the Lee and Wilson labs at Harvard Medical School, the Murthy lab at Princeton, the Drugowitsch lab at Harvard, and the broader fly-connectomics community that built the tools (CAVE, Neuroglancer, FlyWire, FAFB, MANC, hemibrain, maleCNS) on which this work rests.

### Funding

We gratefully acknowledge the funders who made this work possible. Major support came from the **NIH** (incl. R01NS121874, RF1MH117808, U19NS118246, U24NS126935, RF1MH117815, and a NINDS R35), the **Howard Hughes Medical Institute** (R.I.W. is an HHMI Investigator; additional support via HHMI Janelia and an HHMI Gilliam Fellowship), the **Wellcome Trust** (Sir Henry Wellcome Postdoctoral Fellowship to A.S.B., 222782/Z/21/Z), the **W. M. Keck Foundation**, the **NSF**, the **Max Planck Society**, the **Deutsche Forschungsgemeinschaft**, the **Medical Research Council** (UK), **JSPS** and **JST** (Japan), the **Beijing Natural Science Foundation**, and the **Smith Family Odyssey**, **Shanahan Family**, **Alice and Joseph Brooks**, and **Searle / McKnight Scholar** awards, along with several individual training and postdoctoral fellowships listed in full in [acknowledgements.md](acknowledgements.md).

## License

The code in this repository is released under [CC BY 4.0](LICENSE).
The data on the Dataverse deposit (DOI [10.7910/DVN/7WTH1N](https://doi.org/10.7910/DVN/7WTH1N)) is released under CC BY 4.0.
Internal code archives bundled with the deposit may carry their own licenses — see the per-archive documentation.
