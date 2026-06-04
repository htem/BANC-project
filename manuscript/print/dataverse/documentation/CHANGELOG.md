---
filename: CHANGELOG.md
content_type: text/markdown

# --- Fields posted to Dataverse ---
description: >-
  Per-version change log for the BANC Harvard Dataverse deposit at
  doi:10.7910/DVN/7WTH1N. One section per published Dataverse version,
  newest at top, with Added / Changed / Removed bullets describing
  file replacements, metadata edits, and policy changes since the
  previous version. The Dataverse per-version versionNote field is the
  one-line companion shown in the version-history UI; this file is the
  verbose record. Use this changelog plus the versionNumber returned
  by the Dataverse API to track exactly what is in any released
  version.
categories:
  - Documentation
directoryLabel: documentation
restrict: false
tabIngest: false
---

# Changelog

Dataset: **BANC v888 -- Distributed control circuits across a brain-and-cord connectome**
DOI: <https://doi.org/10.7910/DVN/7WTH1N>

Each section corresponds to one published Dataverse version. Versions
bump **major** (v1.0 -> v2.0) when file content changes; **minor**
(v1.0 -> v1.1) when only metadata changes (description, keywords,
authors, etc.). Newest at top.

---

## v1.1 (in development -- not yet released)

### Added

- Public BANC CAVE datastack URL
  (`https://global.daf-apis.com/info/datastack/brain_and_nerve_cord_public`)
  added to the dataset description (both the "CAVE programmatic access"
  paragraph and the data-navigation guide) and to the file descriptions of
  all ten CAVE annotation tables under `annotations/v888/`:
  `backbone_proofread.parquet`, `banc_888_mitochondria_v1_human_readable.csv.gz`,
  `cell_info.parquet`, `cell_representative_point.parquet`,
  `codex_annotations.parquet`, `neck_connective_y92500.parquet`,
  `neck_connective_y121000.parquet`, `peripheral_nerves.parquet`,
  `proofreading_notes.parquet`, `somas_v1.parquet`.
- Nature DOI for the paper (`10.1038/s41586-026-10735-w`,
  <https://www.nature.com/articles/s41586-026-10735-w>) added to the
  dataset description, alongside the bioRxiv v3 preprint URL. The
  Nature article is open access.

### Changed

- _(none yet)_

### Removed

- _(none yet)_

### Notes

- _(workspace for in-progress notes; promote to a dated v1.1 or v2.0
  section when published)_

---

## v1.0 -- 2026-05-27

Initial public release of the BANC v888 Harvard Dataverse deposit, the
data companion to *Distributed control circuits across a brain-and-cord
connectome* (Bates, Phelps, Kim, Yang et al., 2026). See the dataset
description for the full inventory; this section records that v1.0 is
the starting point, not an incremental update.

### Snapshot

- **Files:** 365
- **Total size:** 457.3 GiB
- **License:** CC BY 4.0
- **Snapshot date for the underlying data:** CAVE materialization v888
  on 2026-04-17 (paper Methods)
- **Companion artefacts:** EM image data + flat v888 segmentation +
  bulk mesh archive on BossDB (DOI:10.60533/boss-2025-941r); live
  reconstruction on FlyWire Codex (codex.flywire.ai/banc); cross-
  dataset viewer at ng.banc.community/view

### Contents summary (by directoryLabel)

| Path | Count | Headline |
|---|---|---|
| `annotations/v888/` | 10 | CAVE annotation tables incl. mitochondria |
| `behavior/` | 1 | Pre-EM rBIas tethered-walking ZIP |
| `code/` | 12 | Snapshot ZIPs of analysis stack with Zenodo DOIs |
| `color_mips/` | 1 | colorMIPs in JRC2018U space |
| `compiled_data/` | 11 | meta, metrics, edgelists, synapses, NT pred |
| `documentation/` | 4 | requirements.txt, bancpipeline_schema, etc. |
| `influence/` | 3 | Subclass aggregates + how-to README |
| `influence/all_to_all/` | 277 | Sharded all-to-all influence parquets (287 GiB) |
| `meshes/` | 1 | Neuropil-region meshes |
| `microCT/` | 1 | X-ray microCT of the BANC fly |
| `nblast/` | 18 | NBLAST feathers + reviewed_matches + PNG zips |
| `neuroglancer_states/` | 1 | 2026a state JSON archive |
| `registrations/` | 3 | BANC<->JRC2018F/VNCF Elastix + template volumes |
| `skeletons/` | 1 | Per-neuron SWC bundle (185 280 files, 15.4 GiB) |
| `supplemental_data/` | 12 | Paper SD tables + vignette rosters + numbers.csv |
| `synapses/manual_review/` | 1 | Aelysia synapse review sample |
| `synapses/nt_classifier/` | 1 | Train/test ground truth |
| `synapses/raw/` | 2 | v2/v3 human-readable raw CSVs |
| `synapses/v2.0/` | 2 | v2 per-synapse NT + neuropil lookup |
| `synapses/v3.0/` | 2 | v3 per-synapse NT + neuropil lookup |

### Metadata

- 100 dataset authors (87 with ORCIDs), in paper order
- 4 dataset contacts (Yang, Lee, Bates, Phelps)
- 4 producers (Wilson, Lee, Murthy, Seung labs)
- 39 grant attributions
- 19 keywords
- 14 software entries (with Zenodo concept DOIs where minted)
- 8 GitHub repos in relatedMaterial
- 5 related-dataset cross-references
- Biomedical metadata block populated (organism, technology, etc.)

### Notes on what is deliberately not in v1.0

- **Full-resolution neuron meshes** are not deposited -- multi-hundred-GiB
  precomputed Neuroglancer mesh layer; live on GCS and archived on
  BossDB. The pointer doc `banc_neuron_meshes.md` records both
  source URLs.
- **Precomputed mitochondria segmentation volume** -- analogous decision;
  see `banc_mitochondria_v1.md`. The per-mitochondrion CSV at
  `annotations/v888/banc_888_mitochondria_v1_human_readable.csv.gz`
  IS deposited.
- **NBLAST refresh expected later** -- at v1.0 the NBLAST feathers and
  reviewed_matches files reflect the 2026-05-26 GCS rebuild; further
  refinements will land in a later minor version.
