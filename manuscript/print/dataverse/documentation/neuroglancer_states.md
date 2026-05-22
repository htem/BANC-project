---
filename: neuroglancer_states_2026a.zip
local_path: /Users/papers/BANC-project/manuscript/print/neuroglancer_states/2026a
n_state_files: 91
unzipped_size_bytes: 6710886
unzipped_size_human: 6.4 MiB
content_type: application/zip
upstream_source: https://github.com/jasper-tms/the-BANC-fly-connectome/tree/main/neuroglancer_states/2026a

# --- Fields posted to Dataverse ---
description: >-
  Neuroglancer state JSON files capturing the exact 3D scenes underlying
  Neuroglancer links in the BANC paper, supplements, vignettes and
  figure legends. Each `.json` is a self-contained scene description —
  segmentation layers, mesh layers, synapse layers, point annotations,
  camera pose — that can be loaded by ng.banc.community, Codex, or any
  other Neuroglancer-compatible viewer to reproduce the published view.
  The `2026a` tag pins these states to the v888 segmentation snapshot
  used for print. Distributed as a ZIP of the upstream directory at
  github.com/jasper-tms/the-BANC-fly-connectome; the authoritative live
  copy stays on GitHub.
categories:
  - Documentation
  - Data
directoryLabel: neuroglancer_states
restrict: false
tabIngest: false
---

# neuroglancer_states_2026a.zip

## Purpose

Neuroglancer links in the BANC paper, vignettes and figure legends
point to state files in this archive. Each `.json` describes the
exact contents of a 3D scene — which segments are loaded, what colors
they take, which synapses are shown, the camera angle, the
cross-section plane — in the schema that Neuroglancer and Codex
consume. Loading the state into `ng.banc.community/<state_name>`
reproduces the published view.

These states are pinned to the **v888** segmentation snapshot
(materialization version used in the printed paper). They are tagged
**2026a** in the repository to keep them separately versioned from
earlier (preprint, 2025a) states.

## Provenance

Authored manually during figure preparation and stored in version
control at
`https://github.com/jasper-tms/the-BANC-fly-connectome/tree/main/neuroglancer_states/2026a`.
The Dataverse copy is a ZIP of the directory at the time of upload; the
authoritative live copy remains on GitHub.

## Contents

Not tabular. Each `.json` file is a Neuroglancer state object —
typically containing top-level keys `layers`, `navigation`,
`crossSectionScale`, `projectionScale`, `selectedLayer`, `layout`,
`title`. The `layers` array lists segmentation, image and annotation
layers with their visibility, color, and (for segmentation layers) the
set of selected segment IDs.

The state files are human-readable and grep-able; file names
correspond to the trailing path segment of the Neuroglancer links
cited in the paper.

## Usage

To load a state in Neuroglancer:

```
https://ng.banc.community/state.json?<URL-encoded JSON>
```

Or, more practically, host the state file alongside any user-facing tool
and pass its URL via the Neuroglancer `?json_url=` parameter. The bancr
helpers `safe_ngl_encode_url()` and `banc_ngl_state()` wrap this for the
R side.

## Related files

- `banc_888_meta.feather` — segment IDs referenced in the state files
  are root IDs at the v888 materialization; this file resolves them to
  cell types and other annotations.
- The paper text — every Neuroglancer link in the manuscript and
  supplement points at a state in this archive; the file name is the
  last path segment of the link.

## Notes

- Some state files contain very long segment-ID lists; the archive
  ships the canonical `.json` form rather than the URL-encoded form
  to keep diffs human-readable.
- Loading these states **requires v888 segmentation to be live** —
  earlier materializations will fail to resolve the segment IDs in
  the state. The Neuroglancer URL
  `https://ng.banc.community/2026a/<name>` pins the segmentation to
  v888 automatically.
- Companion `codex.flywire.ai` links cited in the paper provide a
  parallel browseable view; those are constructed by URL parameters
  and do not need a state file.
