---
# API metadata sent with the file in a single POST to /api/datasets/:persistentId/add
# (description, categories, directoryLabel, restrict, tabIngest are the recognized
# per-file fields in jsonData; everything else here is local bookkeeping.)
filename: acknowledgements.md
local_path: /Users/papers/BANC-project/acknowledgements.md
size_bytes: 17495
md5: 562be3a40fdb4663f8eecb80bda98ac2
content_type: text/markdown

# --- Fields posted to Dataverse ---
description: >-
  Front matter for the BANC paper "Distributed control circuits across a
  brain-and-cord connectome" (Bates, Phelps, Kim, Yang et al., 2026).
  Markdown source with Unicode superscripts so the cross-references
  between authors and affiliations render in any reader.
  AUTHORS AND AFFILIATIONS: full author list with superscript-numbered
  affiliations, BANC-FlyWire Consortium membership, equal-contribution
  and corresponding-author footnotes, and contact addresses for the four
  corresponding authors.
  ACKNOWLEDGEMENTS: the SixEleven proofreading team named in full;
  project administration; software development; scientific discussions
  with named collaborators; X-ray microCT support; Janelia Scientific
  Computing Software (NeuronBridge ingestion); five anonymous reviewers;
  and feedback from the Lee and Wilson labs.
  FUNDING: NIH (R01NS121874, RF1MH117808, U19NS118246, U24NS126935,
  RF1MH117815, and many individual awards), Wellcome (Sir Henry Wellcome
  Postdoctoral Fellowship), Max Planck Society, Deutsche
  Forschungsgemeinschaft, NSF, JSPS and JST, MRC, HHMI, Kempner Graduate
  Fellowship, Smith Family Odyssey Award, NV INBRE, Alice and Joseph
  Brooks Fund, Beijing Natural Science Foundation, NINDS R35, W.M. Keck
  Foundation, Shanahan Family Foundation, Searle Scholar and McKnight
  Scholar Awards, and a Harvard/MIT Joint Research Grant.
  AUTHOR CONTRIBUTIONS AND COMPETING INTERESTS: per-author role
  attribution covering sample preparation, EM acquisition, segmentation,
  proofreading, cell typing, neurotransmitter prediction, influence-metric
  design, data analysis, vignette construction, and manuscript writing;
  declared financial interests in Zetta AI, Aelysia, and Yikes LLC; and
  Harvard's GridTape patent application.
  We thank the wider Drosophila neurobiology research community and its
  funders for their support.
categories:
  - Documentation
directoryLabel: documentation
restrict: false
tabIngest: false
---

# acknowledgements.md

## Purpose

Single-source-of-truth for the front-matter of the BANC paper that is not part
of the manuscript prose: author list + affiliations, consortium membership,
acknowledgements, funding, author contributions, competing interests.

## Provenance

- Generated from the corresponding-author-maintained Google Doc
  (`theBANC_main`; Drive link held privately) on 2026-05-14.
- Author list and affiliations updated to the v888 manuscript version
  (46 affiliations, 97 listed authors + Consortium).
- Acknowledgement prose expanded vs. v626 preprint to include additional
  collaborators (Valeria Silva, Barry Dickson, Lou Scheffer, Konrad Rokicki et
  al. at Janelia, Yijie Yin, Tyler Sloan, five anonymous reviewers); typo
  fix (Noah Petite → Noah Pettit); additional SixEleven member (Michelle
  Pantujan).
- Funding block expanded with new fellowship and grant attributions.

## Format

Plain markdown, UTF-8. Superscripts in author cross-references use the Unicode
superscript block (¹–⁹, ⁰, etc.) for direct readability without rendering.

## How to use

This file is intended as a stable reference for the canonical authorship and
acknowledgement information for the BANC paper. It is the same content rendered
in the published manuscript. Any conflict between this file and the published
manuscript should be resolved in favor of the published manuscript (this file
is informational, not authoritative).

## Columns / fields

Not tabular — narrative document. Sections:

- `# Acknowledgements` (title)
- `## Authors` — comma-separated list with Unicode-superscript affiliation
  refs and `*`, `†`, `^`, `✉` footnote markers
- `### Affiliations` — numbered list of institutional addresses
- Footnote keys (`*`, `†`, `^`, `✉`)
- `## The BANC-FlyWire Consortium` — additional consortium members and
  their affiliations
- `## Acknowledgements` — narrative prose
- `## Funding` — narrative prose listing grants and personal awards
- `## Author Contributions` — narrative prose
- `## Competing Interests` — narrative prose
- `## Additional Information` — corresponding-author contact assignments
  and a note on the accompanying Supplementary Notes + Supplementary
  Data files

## Related files in this dataset

- `bibliography.bib` — the BibTeX bibliography for every reference
  cited in the paper, the natural companion to this front-matter file.
- The paper text itself is *not* deposited as a separate file (per Nature
  policy the published version is the version of record).
- The dataset-level citation block on Dataverse mirrors the author list above
  for indexing; this file is the human-readable counterpart.
