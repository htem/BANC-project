#!/usr/bin/env bash
# regenerate_paper.sh — Run all paper figure/text scripts in dependency order.
#
# Regenerates every figure and text asset for the BANC connectome paper.
# Designed so that changing the data version (e.g. v850 -> v900) requires
# only one argument; all R scripts pick up the version automatically via
# the BANC_VERSION env var set by this script.
#
# ─── USAGE ───────────────────────────────────────────────────────────────
#
#   ./manuscript/scripts/regenerate_paper.sh [VERSION] [OPTIONS]
#
#   VERSION   Data version to use. Format: banc_NNN (e.g. banc_888, banc_900).
#             Defaults to banc_888 if omitted.
#
# ─── OPTIONS ─────────────────────────────────────────────────────────────
#
#   --recalculate       Force recalculation of all cached results: UMAPs,
#                       influence PNGs, heatmaps, neuroanatomy meshes.
#                       This is the DEFAULT when running via this script.
#
#   --no-recalculate    Reuse cached results where they exist on disk.
#                       Useful when migrating to a new version but keeping
#                       existing UMAP coordinates, or when re-running after
#                       a partial failure without redoing completed work.
#                       Affected scripts: panel_an_dn_umap.R,
#                       panel_efferent_umap.R, panel_neuroanatomy.R,
#                       panels_cell_type_blowouts.R.
#
#   --ncores N          Number of parallel workers for influence computation.
#                       Default: 1 (sequential). Higher values use more RAM.
#                       Passed to R as BANC_NCORES env var.
#
# ─── ENVIRONMENT VARIABLES ──────────────────────────────────────────────
#
#   These are set automatically by this script but can be overridden:
#
#   BANC_VERSION        Data version string (set from $1 or default)
#   BANC_RECALCULATE    "TRUE" or "FALSE" — controls recalculate flags
#   BANC_NCORES         Parallel worker count for influence_calculator_py
#
# ─── EXAMPLES ────────────────────────────────────────────────────────────
#
#   # Full regeneration for a new data version:
#   ./manuscript/scripts/regenerate_paper.sh banc_900
#
#   # New version but keep existing UMAP coordinates and rendered PNGs:
#   ./manuscript/scripts/regenerate_paper.sh banc_900 --no-recalculate
#
#   # Re-run current default version (v888) without recalculating anything:
#   ./manuscript/scripts/regenerate_paper.sh --no-recalculate
#
#   # Full regen with 4 parallel workers:
#   ./manuscript/scripts/regenerate_paper.sh banc_900 --ncores 4
#
# ─── PREREQUISITES ──────────────────────────────────────────────────────
#
#   Before running for a NEW version, ensure these external data files
#   exist (they are produced by pipelines outside this repo):
#
#   1. GCS feather files at:
#      gs://brain-and-nerve-cord_exports/processed_data/banc/<version>/
#        - <version>_meta.feather
#        - <version>_edgelist_simple_v2.feather   (v888+: versioned suffix;
#          consumer default is _v2 as of 2026-04-21. _v3 also published.)
#
#   2. Betweenness centrality CSVs (v888+: _v2 suffix):
#      data/betweeness/betweenness_afferent_to_efferent_banc_<NNN>_v2.csv
#      data/betweeness/betweenness_all_to_all_banc_<NNN>_v2.csv
#
#   3. Feedforward layer assignments:
#      data/feedforward/layers_banc_<NNN>.csv
#
#   4. Spectral clustering (v888+: strength=2, count=14, _v2 suffix):
#      data/cns_network/spectral_clustering_min_connection_strength_2_banc_version_<NNN>_cluster_count_14_cluster_seed_10_embedding_seed_3_v2.csv
#
#   5. Synapse capture rates (v888+: _v2 flavour, size_thresh=5 default):
#      data/completion/banc_<NNN>_v2_{region,neuropil,gross,inout}_capture_rates_size_thresh_5.csv
#
#   6. Figure 6 neuroanatomy PDFs (from bancpipeline/banc/clustering):
#      figures/figure_6/links/neuroanatomy/*.pdf
#
#   Missing files cause individual scripts to warn/fallback, not crash the
#   whole pipeline. Check per-script logs in the output directory.
#
# ─── RUN ORDER & DEPENDENCIES ───────────────────────────────────────────
#
#   Phase 1 — Core data producers (MUST run first, strict serial):
#     panel_an_dn_umap.R       -> data/banc_neck_functional_classes.csv
#     panel_efferent_umap.R    -> data/banc_efferent_functional_classes.csv
#     panels_body_parts.R      -> data/determined_thresholds/influence_norm_log_elbow_threshold.csv
#                                 data/meta/banc_neck_inclusion.csv
#     These CSVs are read by banc-meta.R at startup of every subsequent script.
#
#   Phase 2 — Figures 3 + 4 + 5 (serial, memory-heavy):
#     panel_an_dn_polarity.R, panel_an_dn_connectivity.R,
#     panel_an_dn_influence.R (~90 min), panel_cluster_sensory_correlations.R,
#     panels_cell_type_blowouts.R (~2 hr),
#     panel_neuroanatomy.R (~12-20 hr, auto-restart up to 10 attempts)
#
#   Phase 3 — Figure 6:
#     panel_super_clusters.R, panel_mbx_cx_control.R
#
#   Phase 4 — Figure 2 (remaining panels):
#     panel_betweeness_layers.R, panel_efferent_umap.R,
#     panel_influence_validation.R, panel_sensory_motor.R,
#     panel_pre_effector_influence.R
#
#   Phase 5 — Figure 1:
#     panel_inventory.R, panel_proofread_matching.R
#
#   Phase 6 — Text & supplementary (depends on all figure outputs):
#     numbers.R, supplemental_data.R, ngl_links.R
#
#   Scripts run sequentially (NOT parallel) — they are memory-heavy and
#   several use all cores via PETSc. Concurrent runs thrash.
#
# ─── OUTPUT ──────────────────────────────────────────────────────────────
#
#   Per-script logs:  /tmp/banc_runs/<version>_<timestamp>/<script_name>.log
#   Figure assets:    figures/figure_N/links/ (PDFs, PNGs)
#   Data outputs:     data/ (CSVs, thresholds)
#   Text outputs:     manuscript/print/ (numbers, supplemental tables)
#
#   The pipeline halts on the first script failure. Fix the issue, then
#   re-run with --no-recalculate to skip already-completed work.
#
# ─────────────────────────────────────────────────────────────────────────

set -euo pipefail

# Parse arguments
RECALC="${BANC_RECALCULATE:-TRUE}"
NCORES="${BANC_NCORES:-1}"
POSITIONAL=()
while [[ $# -gt 0 ]]; do
  case "$1" in
    --recalculate)      RECALC="TRUE"; shift ;;
    --no-recalculate)   RECALC="FALSE"; shift ;;
    --ncores)           NCORES="$2"; shift 2 ;;
    --ncores=*)         NCORES="${1#*=}"; shift ;;
    -*)                 echo "Unknown option: $1" >&2; exit 1 ;;
    *)                  POSITIONAL+=("$1"); shift ;;
  esac
done

VERSION="${POSITIONAL[0]:-${BANC_VERSION:-banc_888}}"
export BANC_VERSION="$VERSION"
export BANC_RECALCULATE="$RECALC"
export BANC_NCORES="$NCORES"

LOGDIR="/tmp/banc_runs/${VERSION}_$(date +%Y%m%d_%H%M%S)"
mkdir -p "$LOGDIR"

echo "=== BANC paper regeneration ==="
echo "Version:     $VERSION"
echo "Recalculate: $BANC_RECALCULATE"
echo "NCORES:      $BANC_NCORES"
echo "Logs:        $LOGDIR"
echo "Started:     $(date)"
echo ""

# --- Phase 0: Pre-flight ---
echo "[pre-flight] Checking GCS data for $VERSION..."
if command -v gsutil &>/dev/null; then
  if ! gsutil ls "gs://brain-and-nerve-cord_exports/processed_data/banc/${VERSION}/" &>/dev/null; then
    echo "WARNING: GCS data not found at gs://brain-and-nerve-cord_exports/processed_data/banc/${VERSION}/"
    echo "Continuing — scripts will fall back to local cache if available."
  else
    echo "[pre-flight] GCS data found."
  fi
else
  echo "[pre-flight] gsutil not found — skipping GCS check."
fi
echo ""

run_script() {
  local script="$1"
  local name
  name=$(basename "$script" .R)
  local logfile="$LOGDIR/${name}.log"
  local start
  start=$(date +%s)
  echo -n "[$(date +%H:%M:%S)] $name ... "
  if Rscript "$script" > "$logfile" 2>&1; then
    local elapsed=$(( $(date +%s) - start ))
    echo "OK (${elapsed}s)"
  else
    local rc=$?
    local elapsed=$(( $(date +%s) - start ))
    echo "FAILED rc=$rc (${elapsed}s) — see $logfile"
    tail -20 "$logfile"
    exit $rc
  fi
}

run_resilient() {
  local script="$1"
  local name
  name=$(basename "$script" .R)
  local max_attempts="${2:-10}"
  local attempt=1
  while [ $attempt -le $max_attempts ]; do
    local logfile="$LOGDIR/${name}_attempt${attempt}.log"
    echo -n "[$(date +%H:%M:%S)] $name (attempt $attempt/$max_attempts) ... "
    local start
    start=$(date +%s)
    if Rscript "$script" > "$logfile" 2>&1; then
      local elapsed=$(( $(date +%s) - start ))
      echo "OK (${elapsed}s)"
      return 0
    else
      local elapsed=$(( $(date +%s) - start ))
      echo "FAILED (${elapsed}s)"
      attempt=$((attempt + 1))
      sleep 10
    fi
  done
  echo "ERROR: $name failed after $max_attempts attempts"
  exit 1
}

# --- Phase 1: Core data producers (strict serial — each writes CSVs consumed downstream) ---
echo "=== Phase 1: Core data producers ==="
run_script R/figures/panel_an_dn_umap.R
run_script R/figures/panel_efferent_umap.R
run_script R/figures/panels_body_parts.R

# --- Phase 2: Figures 3 + 4 + 5 ---
echo ""
echo "=== Phase 2: Figures 3 + 4 + 5 ==="
run_script R/figures/panel_an_dn_polarity.R
run_script R/figures/panel_an_dn_connectivity.R
run_script R/figures/panel_an_dn_influence.R
run_script R/figures/panel_cluster_sensory_correlations.R
run_script R/figures/panels_cell_type_blowouts.R
# panel_neuroanatomy.R moved to end (Phase 7) — needs recalculate=TRUE
# to overwrite stale outputs from renamed super_clusters.

# --- Phase 3: Figure 6 ---
echo ""
echo "=== Phase 3: Figure 6 ==="
run_script R/figures/panel_super_clusters.R
run_script R/figures/panel_mbx_cx_control.R

# --- Phase 4: Figure 2 leftovers ---
echo ""
echo "=== Phase 4: Figure 2 ==="
run_script R/figures/panel_betweeness_layers.R
run_script R/figures/panel_efferent_umap.R
run_script R/figures/panel_influence_validation.R
run_script R/figures/panel_sensory_motor.R
run_script R/figures/panel_pre_effector_influence.R

# --- Phase 5: Figure 1 (usually stable, run last among figures) ---
echo ""
echo "=== Phase 5: Figure 1 ==="
run_script R/figures/panel_inventory.R
run_script R/figures/panel_proofread_matching.R

# --- Phase 6: Text and supplementary data ---
echo ""
echo "=== Phase 6: Text ==="
run_script R/text/numbers.R
run_script R/text/supplemental_data.R
run_script R/text/ngl_links.R

# --- Phase 7: Neuroanatomy (slow, always recalculate to catch renamed super_clusters) ---
echo ""
echo "=== Phase 7: Neuroanatomy ==="
BANC_RECALCULATE=TRUE run_resilient R/figures/panel_neuroanatomy.R 10

echo ""
echo "=== All scripts completed successfully ==="
echo "Finished: $(date)"
echo "Logs:     $LOGDIR"
