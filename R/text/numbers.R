#' Paper variable computation (numbers.csv → Google Sheet → main doc)
#'
#' Aggregates every numerical value cited in the manuscript prose into a
#' single tidy table (`variable_name`, `value`, `type` ∈ {auto, hardcoded},
#' `description`) and pushes it to the paired Google Sheet
#' `theBANC_variables` so the bound Apps Script (manuscript/print/
#' updateHyperlinkVariables.gs) can refresh every `var/<identity>`
#' hyperlink in the main doc `theBANC_main`.
#'
#' Values come from three sources:
#'   (1) live computation on banc.meta / franken.meta / banc.edgelist.simple,
#'   (2) .txt sidecars written by panel scripts (e.g. influence_validation
#'       ge5 sidecars for sensory direct/indirect counts), and
#'   (3) hard-coded entries for manuscript-stable numbers.
#'
#' @section Reads:
#'   banc.meta, franken.meta, banc.edgelist.simple, paper.cols
#'   figures/figure_*/links/*.txt                                             (sidecars from panel scripts)
#'   data/nblast/banc_888_top_match_correct.csv                                (from nblast_top_match_correct.R)
#'   data/determined_thresholds/*.csv                                          (e.g. influence_norm_log_elbow)
#'
#' @section Writes:
#'   manuscript/print/numbers.csv                                              (one row per cited variable)
#'   Google Sheet `theBANC_variables` (tab `variables`) — write_sheet call
#'   uses gsheet_id stored in data/private/keys.csv (gitignored).
#'
#' @section Paper:
#'   Every parenthetical or inline number with a `var/<identity>` hyperlink
#'   in `theBANC_main` resolves through this table.
#'
#' @section Schema:
#'   `add_row(df, "<identity>", value, "auto", "<description>")` is the
#'   single-row helper; proportions < 1 are formatted as percentages by
#'   the post-processing block (search for `banc_neuropeptide_verified_pct_`);
#'   strings ending in `%` pass through unmodified.
#'
#' @section Used by:
#'   manuscript/print/updateHyperlinkVariables.gs                              (Apps Script bound to doc)
#'   manuscript/print/text/                                                     (cleaned doc download)
#'
#' @section Notes:
#'   Sheet writes here are pre-approved (the documented sync mechanism;
#'   see CLAUDE.md "Hard constraint: editing the doc / sheet"). Ad-hoc
#'   sheet writes initiated from a tool call still require explicit OK.
#'
#' @section Reproduce:
#'   Rscript R/text/numbers.R
source("R/startup/banc-startup.R")

# Get metadata
franken.meta <- tryCatch({
  fm <- franken_meta()
  if (is.null(fm) || nrow(fm) == 0) stop("empty result")
  # 2026-07-20: franken_meta() began returning a reduced 23-column frame with no
  # `dataset` column, which killed the "Missing types" block below
  # (dplyr::filter(..., dataset == "FAFB")) and aborted the whole script before
  # numbers.csv was written. A row-count check alone does not catch a schema
  # regression, so require the columns we actually consume; failing that, fall
  # through to franken-meta.R, which supplies the full frame.
  .fm_need <- c("dataset", "super_class", "flow", "cell_type")
  if (!all(.fm_need %in% colnames(fm))) {
    stop(sprintf("missing columns: %s",
                 paste(setdiff(.fm_need, colnames(fm)), collapse = ", ")))
  }
  fm
}, error = function(e) {
  message("franken_meta() failed: ", e$message, " — trying franken-meta.R")
  tryCatch({
    source("R/startup/franken-meta.R")
    franken.meta
  }, error = function(e2) {
    message("franken-meta.R also failed: ", e2$message, " — continuing without franken data")
    NULL
  })
})

banc.meta <- tryCatch({
  bm <- banctable_query()
  if (is.null(bm) || nrow(bm) == 0) stop("empty result")
  bm
}, error = function(e) {
  message("banctable_query() failed: ", e$message, " — using GCS + startup meta")
  source("R/startup/banc-meta.R")
  banc.meta
})

# Get neck_connective seed plane info (live=2 for latest materialisation)
neck_connective_y92500 <- banc_cave_query("neck_connective_y92500", live = 2)
if ("valid" %in% colnames(neck_connective_y92500)) {
  neck_connective_y92500 <- neck_connective_y92500 %>% dplyr::filter(valid == "t")
}
neck_connective_y92500 <- neck_connective_y92500 %>%
  dplyr::distinct(pt_root_id, .keep_all = TRUE) %>%
  dplyr::mutate(root_id = as.character(pt_root_id))

neck_connective_y121000 <- banc_cave_query("neck_connective_y121000", live = 2)
if ("valid" %in% colnames(neck_connective_y121000)) {
  neck_connective_y121000 <- neck_connective_y121000 %>% dplyr::filter(valid == "t")
}
neck_connective_y121000 <- neck_connective_y121000 %>%
  dplyr::distinct(pt_root_id, .keep_all = TRUE) %>%
  dplyr::mutate(root_id = as.character(pt_root_id))

neck_connective_all <- dplyr::bind_rows(neck_connective_y92500, neck_connective_y121000) %>%
  dplyr::distinct(pt_root_id, .keep_all = TRUE)

# Helper to add a row (value stored as numeric for computation, converted to string at end)
add_row <- function(df, name, val, type = "auto", desc = "") {
  dplyr::bind_rows(df, data.frame(
    variable_name = name, value = as.character(val), type = type, description = desc,
    stringsAsFactors = FALSE
  ))
}

# Initialise data frame
df <- data.frame(variable_name = character(), value = character(),
                 type = character(), description = character(),
                 stringsAsFactors = FALSE)

# Convenience: distinct neurons (used repeatedly)
bm <- banc.meta %>% dplyr::distinct(root_id, .keep_all = TRUE)
bm.neurons <- bm %>%
  dplyr::filter(!super_class %in% c("glia", "trachea", "not_a_neuron"),
                !grepl("DEBRIS|NOT_A_NEURON|TRACHEA", status))

###########################
### Influence threshold ###
###########################

df <- add_row(df, "influence_norm_thresh", threshold.inf.value, "auto",
              "Influence norm log elbow threshold (from panels_body_parts.R elbow analysis). Results: CNS networks.")

###################################################
### Modal pairwise adjusted influence (Fig. 2c) ###
###################################################
# Values are computed by panel_influence_validation.R on the same random
# BANC seed set it uses for the validation histograms (count_thresh = 0
# influence calculator), and written to
# data/determined_thresholds/pairwise_modal_influence.csv. Mirrors the
# pattern used for the influence-norm-log elbow threshold. If the CSV is
# missing, the rows are silently skipped — re-run panel_influence_validation.R
# to regenerate.
.modal_csv <- "data/determined_thresholds/pairwise_modal_influence.csv"
if (file.exists(.modal_csv)) {
  .modal_df <- readr::read_csv(.modal_csv, show_col_types = FALSE)
  for (.i in seq_len(nrow(.modal_df))) {
    .row <- .modal_df[.i, ]
    df <- add_row(df, .row$metric, .row$value, "auto",
                  sprintf("Mode of integer-rounded adjusted influence for %s (%s) pairs from %d random proofread BANC sources × proofread+RPR targets, IC count_thresh = %d (computed by panel_influence_validation.R, %d pairs). Fig. 2c.",
                          .row$pair_kind, .row$direct_thresh, .row$seed_n,
                          .row$ic_count_thresh, .row$n_pairs))
  }
  message(sprintf("Loaded modal pairwise CSV: %d rows from %s",
                  nrow(.modal_df), .modal_csv))
} else {
  message("Modal pairwise CSV not found (", .modal_csv,
          ") — re-run panel_influence_validation.R to regenerate.")
}

###############################################################
### Above-25th-pctl counts from the ge5 validation sidecar  ###
###############################################################
# panel_influence_validation.R writes per-facet quantile tables into the
# sidecar .txt next to each PDF. Pull out the 25th-percentile counts for
# the SENSORY facet of the Pass B (count >= 5) validation — used in the
# Methods sentence "X indirect connections and Y direct connections from
# sensory, are over the 25th percentile influence for direct-sensory".
.ge5_sidecar <- "figures/figure_2/links/influence_norm_log_vs_direct_connectivity_ge5.txt"
if (file.exists(.ge5_sidecar)) {
  .lines <- readLines(.ge5_sidecar, warn = FALSE)
  .parse_facet <- function(lines, facet_label, identity_stem) {
    # Find the per-facet block; bail silently if absent.
    start <- grep(sprintf("^== Facet: %s ==", facet_label), lines, fixed = FALSE)
    if (length(start) != 1L) return(invisible(NULL))
    block_end <- which(grepl("^== Facet: ", lines))
    end <- if (any(block_end > start)) min(block_end[block_end > start]) - 1L
           else length(lines)
    chunk <- lines[start:end]
    # Totals are tagged "Total direct interactions" / "Total indirect interactions".
    .tot_direct <- as.integer(sub(".*:\\s*(\\d+).*$", "\\1",
                                  grep("Total direct interactions",   chunk, value = TRUE)))
    .tot_indir  <- as.integer(sub(".*:\\s*(\\d+).*$", "\\1",
                                  grep("Total indirect interactions", chunk, value = TRUE)))
    # 25th-pctl threshold line + the two ">= 25th" count lines.
    .p25_thresh <- as.numeric(sub(".*25th pctl:\\s*([0-9.]+).*$", "\\1",
                                  grep("25th pctl:", chunk, value = TRUE)))
    .p25_thresh <- if (length(.p25_thresh) >= 1L) .p25_thresh[1] else NA_real_
    .direct_p25 <- as.integer(sub(".*:\\s*(\\d+)\\s*/.*$", "\\1",
                                  grep("Direct\\s+>= 25th",   chunk, value = TRUE)))
    .indir_p25  <- as.integer(sub(".*:\\s*(\\d+)\\s*/.*$", "\\1",
                                  grep("Indirect >= 25th", chunk, value = TRUE)))
    if (length(.direct_p25)) {
      df <<- add_row(df, sprintf("%s_ge5_p25_direct_count",   identity_stem),
                     .direct_p25, "auto",
                     sprintf("Direct (count >= 5) %s-seed → target pairs whose adjusted influence is at or above the 25th percentile of the direct distribution (%.4f); out of %d direct pairs total. Sidecar: %s.",
                             facet_label, .p25_thresh, .tot_direct, basename(.ge5_sidecar)))
    }
    if (length(.indir_p25)) {
      df <<- add_row(df, sprintf("%s_ge5_p25_indirect_count", identity_stem),
                     .indir_p25, "auto",
                     sprintf("Indirect %s-seed → target pairs whose adjusted influence is at or above the 25th percentile of the direct distribution (%.4f); out of %d indirect pairs total. Sidecar: %s.",
                             facet_label, .p25_thresh, .tot_indir, basename(.ge5_sidecar)))
    }
    if (length(.p25_thresh) && !is.na(.p25_thresh)) {
      df <<- add_row(df, sprintf("%s_ge5_p25_threshold", identity_stem),
                     signif(.p25_thresh, 4), "auto",
                     sprintf("25th-percentile of the direct-pair adjusted influence distribution for %s seeds (count >= 5 graph). Sidecar: %s.",
                             facet_label, basename(.ge5_sidecar)))
    }
    # Raw direct / indirect totals, formatted to 2 significant figures.
    # Same numbers the *_p25_*_count denominators are taken from, surfaced
    # here so prose can cite "≈X direct sensory→target pairs" etc.
    if (length(.tot_direct) && !is.na(.tot_direct)) {
      df <<- add_row(df, sprintf("%s_ge5_direct_count_raw", identity_stem),
                     formatC(signif(.tot_direct, 2), format = "d", big.mark = ","),
                     "auto",
                     sprintf("Raw count of direct (count >= 5) %s-seed → target pairs whose adjusted influence is above the floor; rounded to 2 sig figs (exact: %d). Sidecar: %s.",
                             facet_label, .tot_direct, basename(.ge5_sidecar)))
    }
    if (length(.tot_indir) && !is.na(.tot_indir)) {
      df <<- add_row(df, sprintf("%s_ge5_indirect_count_raw", identity_stem),
                     formatC(signif(.tot_indir, 2), format = "d", big.mark = ","),
                     "auto",
                     sprintf("Raw count of indirect %s-seed → target pairs whose adjusted influence is above the floor; rounded to 2 sig figs (exact: %d). Sidecar: %s.",
                             facet_label, .tot_indir, basename(.ge5_sidecar)))
    }
  }
  .parse_facet(.lines, "sensory", "sensory")
  .parse_facet(.lines, "AN/DN",   "an_dn")
  message("Loaded ge5 sidecar percentiles from ", .ge5_sidecar)
} else {
  message("ge5 validation sidecar not found (", .ge5_sidecar,
          ") — re-run panel_influence_validation.R to regenerate.")
}

###########################################
### Hop depth: sensory→AN/DN, AN/DN→eff  ###
###########################################
# Supports the methods statement that the multiplicative influence
# calculation traverses few hops in practice. We compute the directed
# shortest-path distance (BFS) on the same edgelist (proofread + count > 0)
# the influence calculator uses (see banc-functions.R:261), for two passes:
#   1. sensory neurons → AN/DN targets
#   2. AN/DN neurons   → effector targets (motor + efferent + endocrine + visceral)

message("Computing hop depth statistics...")
# Build the same edgelist the influence calculator traverses, without
# sourcing banc-edgelist.R (which assumes a banc.meta with `id` and the
# pre/post-keyed globals that numbers.R doesn't have in scope).
# Pattern mirrors banc-edgelist.R:22-26 (read raw cache) and lines 86-94
# (proofread filter), but uses root_id from the in-scope bm table.

.hop_edge_cache <- .banc_edgelist_cache
if (!file.exists(.hop_edge_cache)) {
  stop("Edgelist cache not found at ", .hop_edge_cache,
       " — run a script that loads banc-edgelist.R once to populate it.")
}
.hop_edgelist_raw <- arrow::read_feather(.hop_edge_cache) %>%
  dplyr::mutate(pre = as.character(pre),
                post = as.character(post))

.hop_proofread_ids <- bm %>%
  dplyr::filter(as.logical(proofread) %in% TRUE |
                  as.logical(roughly_proofread) %in% TRUE) %>%
  dplyr::pull(root_id) %>%
  as.character() %>%
  unique()

.hop_edgelist <- .hop_edgelist_raw %>%
  dplyr::filter(pre %in% .hop_proofread_ids,
                post %in% .hop_proofread_ids,
                count > 0) %>%
  dplyr::select(pre, post)
message(sprintf("Hop edgelist: %d edges after proofread + count>0 filter",
                nrow(.hop_edgelist)))
rm(.hop_edgelist_raw, .hop_edge_cache)

.hop_g <- igraph::graph_from_data_frame(d = .hop_edgelist, directed = TRUE)
rm(.hop_edgelist)
.hop_vnames <- igraph::V(.hop_g)$name

# Inline helper: BFS from each source to the target set, accumulating an
# integer histogram of finite hop counts. Returns list(median, p99, max,
# reach_pct, n_sources_used). Memory is O(1) per source.
.compute_hops <- function(source_ids, target_ids, g, vnames, n_sample, seed = 42) {
  .src_in_g <- intersect(source_ids, vnames)
  .tgt_in_g <- intersect(target_ids, vnames)

  set.seed(seed)
  .src_sample <- if (length(.src_in_g) > n_sample) {
    sample(.src_in_g, n_sample)
  } else {
    .src_in_g
  }

  .max_bin <- 50L
  .hist <- integer(.max_bin)
  .pairs_total     <- 0L
  .pairs_reachable <- 0L
  .tgt_vids <- igraph::V(g)[name %in% .tgt_in_g]
  .pb <- utils::txtProgressBar(min = 0, max = length(.src_sample), style = 3)
  for (.i in seq_along(.src_sample)) {
    .dvec <- as.numeric(igraph::distances(
      g,
      v    = igraph::V(g)[name == .src_sample[.i]],
      to   = .tgt_vids,
      mode = "out"
    ))
    .pairs_total <- .pairs_total + length(.dvec)
    .finite <- .dvec[is.finite(.dvec) & .dvec > 0]
    .pairs_reachable <- .pairs_reachable + length(.finite)
    if (length(.finite) > 0) {
      .ints <- pmin(as.integer(.finite), .max_bin)
      .hist <- .hist + tabulate(.ints, nbins = .max_bin)
    }
    utils::setTxtProgressBar(.pb, .i)
  }
  close(.pb)

  .total_finite <- sum(.hist)
  .cumsum <- cumsum(.hist)
  list(
    median        = which(.cumsum >= 0.50 * .total_finite)[1],
    p99           = which(.cumsum >= 0.99 * .total_finite)[1],
    max           = max(which(.hist > 0)),
    reach_pct     = round(100 * .pairs_reachable / .pairs_total, 1),
    n_sources_used = length(.src_sample)
  )
}

# --- Pass 1: sensory → AN/DN ---------------------------------------------
message("  Pass 1: sensory → AN/DN")
.hop_sensory_ids <- bm.neurons %>%
  dplyr::filter(grepl("sensory", super_class)) %>%
  dplyr::pull(root_id) %>% as.character() %>% unique()
.hop_andn_ids <- bm.neurons %>%
  dplyr::filter(super_class %in% c("ascending", "descending")) %>%
  dplyr::pull(root_id) %>% as.character() %>% unique()

.hop_sa <- .compute_hops(.hop_sensory_ids, .hop_andn_ids,
                          .hop_g, .hop_vnames, n_sample = 2000L)

df <- add_row(df, "sensory_to_andn_hops_median", .hop_sa$median, "auto",
              "Median directed shortest-path hops from sensory neurons to reachable AN/DN targets, on the proofread + count>0 edgelist used by the influence calculator. Methods: Influence.")
df <- add_row(df, "sensory_to_andn_hops_p99", .hop_sa$p99, "auto",
              "99th-percentile directed shortest-path hops from sensory neurons to reachable AN/DN targets — practical max. Methods: Influence.")
df <- add_row(df, "sensory_to_andn_hops_max", .hop_sa$max, "auto",
              "Absolute max directed shortest-path hops from sensory neurons to reachable AN/DN targets observed in sample. Methods: Influence.")
df <- add_row(df, "sensory_to_andn_pairs_reachable_pct", .hop_sa$reach_pct, "auto",
              sprintf("Percent of (sampled sensory, AN/DN) pairs connected by any directed path (sample n=%d sensory sources). Methods: Influence.", .hop_sa$n_sources_used))

# --- Pass 2: AN/DN → effector --------------------------------------------
# Effector definition matches banc-meta.R:468 (banc.eff.meta).
message("  Pass 2: AN/DN → effector")
.hop_eff_ids <- bm.neurons %>%
  dplyr::filter(grepl("efferent|motor|endocrine|visceral", super_class)) %>%
  dplyr::pull(root_id) %>% as.character() %>% unique()

# AN/DN is small enough (~3155) that we don't need to subsample sources.
.hop_ae <- .compute_hops(.hop_andn_ids, .hop_eff_ids,
                          .hop_g, .hop_vnames, n_sample = 5000L)

df <- add_row(df, "andn_to_effector_hops_median", .hop_ae$median, "auto",
              "Median directed shortest-path hops from AN/DN neurons to reachable effector targets (motor/efferent/endocrine/visceral), on the proofread + count>0 edgelist used by the influence calculator. Methods: Influence.")
df <- add_row(df, "andn_to_effector_hops_p99", .hop_ae$p99, "auto",
              "99th-percentile directed shortest-path hops from AN/DN neurons to reachable effector targets — practical max. Methods: Influence.")
df <- add_row(df, "andn_to_effector_hops_max", .hop_ae$max, "auto",
              "Absolute max directed shortest-path hops from AN/DN neurons to reachable effector targets observed in sample. Methods: Influence.")
df <- add_row(df, "andn_to_effector_pairs_reachable_pct", .hop_ae$reach_pct, "auto",
              sprintf("Percent of (AN/DN, effector) pairs connected by any directed path (sample n=%d AN/DN sources). Methods: Influence.", .hop_ae$n_sources_used))

rm(.hop_proofread_ids, .hop_g, .hop_vnames, .hop_sensory_ids, .hop_andn_ids,
   .hop_eff_ids, .hop_sa, .hop_ae, .compute_hops)
gc(verbose = FALSE)

###########################
### Synapse detection   ###
###########################

# Postsynaptic detections — check local cache, else hardcoded
# (synapse parquet is too large to download from GCS on local machines)
message("Counting postsynaptic detections...")
local_syn <- file.path("data", "cache", paste0(banc.gcs.dataset, "_synapses_v2_enriched.parquet"))
if (!file.exists(local_syn)) {
  # fallback to old naming convention
  local_syn <- file.path("data", "cache", paste0(banc.gcs.dataset, "_synapses_enriched.parquet"))
}
postsynaptic_count <- if (file.exists(local_syn)) {
  tryCatch({
    pf <- arrow::ParquetFileReader$create(local_syn)
    pf$GetMetaData()$num_rows
  }, error = function(e) NA_real_)
} else {
  NA_real_
}
if (is.na(postsynaptic_count)) {
  postsynaptic_count <- 218460852  # v626 value; update when v850 parquet is available locally
  message("  Using hardcoded synapse count (no local parquet cache)")
}
df <- add_row(df, "postsynaptic_detections", postsynaptic_count,
              ifelse(postsynaptic_count == 218460852, "hardcoded", "auto"),
              "Total postsynaptic detections in synapse table. Results: Synapses.")

# Aelysia v2 manual review tallies — counts of True/False/Ambiguous Tag
# values in the 4,648-row sample of size > 5 v2 synapses that Aelysia
# reviewed (2024-09-20). Drives var/synapse_v2_sample_fiveorless_{true,
# false,ambiguous} placeholders in the manuscript.
.aelysia_csv <- "data/synapses/2024-09-20_aelysia_synapse_sample_complete_v2.csv"
if (file.exists(.aelysia_csv)) {
  .ael <- readr::read_csv(.aelysia_csv, show_col_types = FALSE)
  .tag <- as.character(.ael$Tags)
  .n_true  <- sum(.tag == "True",      na.rm = TRUE)
  .n_false <- sum(.tag == "False",     na.rm = TRUE)
  .n_amb   <- sum(.tag == "Ambiguous", na.rm = TRUE)
  df <- add_row(df, "synapse_v2_sample_fiveorless_true", .n_true, "auto",
                sprintf("Manual review (Aelysia, 2024-09-20): synapses tagged True in the size > 5 v2 review sample (n = %d). Methods: Synapses.",
                        nrow(.ael)))
  df <- add_row(df, "synapse_v2_sample_fiveorless_false", .n_false, "auto",
                sprintf("Manual review (Aelysia, 2024-09-20): synapses tagged False in the size > 5 v2 review sample (n = %d). Methods: Synapses.",
                        nrow(.ael)))
  df <- add_row(df, "synapse_v2_sample_fiveorless_ambiguous", .n_amb, "auto",
                sprintf("Manual review (Aelysia, 2024-09-20): synapses tagged Ambiguous in the size > 5 v2 review sample (n = %d). Methods: Synapses.",
                        nrow(.ael)))
  message(sprintf("Aelysia synapse review tally: True=%d, False=%d, Ambiguous=%d (total %d)",
                  .n_true, .n_false, .n_amb, nrow(.ael)))
} else {
  message("Aelysia synapse review CSV not found: ", .aelysia_csv)
}

# Aelysia v3 manual review tallies — counterpart to the v2 block above.
# Source is the v3 review CSV (Aelysia; sample drawn 2026-05-14, full review
# completed 2026-06-04). Rows with no verdict Tag are dropped.
# Drives var/synapse_v3_sample_{true,false,ambiguous} placeholders.
.aelysia_v3_csv <- "data/synapses/2026-05-14_aelysia_synapse_sample_complete_v3.csv"
if (file.exists(.aelysia_v3_csv)) {
  .ael3 <- readr::read_csv(.aelysia_v3_csv, show_col_types = FALSE)
  .tag3 <- as.character(.ael3$Tags)
  .n_true3  <- sum(.tag3 %in% c("True","TRUE"),   na.rm = TRUE)
  .n_false3 <- sum(.tag3 %in% c("False","FALSE"), na.rm = TRUE)
  .n_amb3   <- sum(.tag3 == "Ambiguous",          na.rm = TRUE)
  .n_rev3   <- .n_true3 + .n_false3 + .n_amb3
  df <- add_row(df, "synapse_v3_sample_true", .n_true3, "auto",
                sprintf("Manual review (Aelysia, 2026-05-14): synapses tagged True in the v3 review sample (n_reviewed = %d of %d). Methods: Synapses.",
                        .n_rev3, nrow(.ael3)))
  df <- add_row(df, "synapse_v3_sample_false", .n_false3, "auto",
                sprintf("Manual review (Aelysia, 2026-05-14): synapses tagged False in the v3 review sample (n_reviewed = %d of %d). Methods: Synapses.",
                        .n_rev3, nrow(.ael3)))
  df <- add_row(df, "synapse_v3_sample_ambiguous", .n_amb3, "auto",
                sprintf("Manual review (Aelysia, 2026-05-14): synapses tagged Ambiguous in the v3 review sample (n_reviewed = %d of %d). Methods: Synapses.",
                        .n_rev3, nrow(.ael3)))
  df <- add_row(df, "synapse_v3_sample_n_reviewed", .n_rev3, "auto",
                sprintf("v3 review sample rows with a True/False/Ambiguous Tag set (of %d total sampled). Methods: Synapses.",
                        nrow(.ael3)))
  message(sprintf("Aelysia v3 synapse review tally: True=%d, False=%d, Ambiguous=%d (reviewed %d / %d)",
                  .n_true3, .n_false3, .n_amb3, .n_rev3, nrow(.ael3)))
} else {
  message("Aelysia v3 synapse review CSV not found: ", .aelysia_v3_csv)
}

# Autapses — check from edgelist (already loaded by startup)
# The simple edgelist has autapses pre-filtered, so use the raw edgelist before filtering
# Read unfiltered edgelist from cache/GCS for autapse computation
message("Computing autapse proportion...")
.raw_edgelist_cache <- .banc_edgelist_cache
if (file.exists(.raw_edgelist_cache)) {
  .raw_el <- arrow::read_feather(.raw_edgelist_cache)
} else {
  .raw_el <- banc.edgelist.simple  # fallback; autapses may already be removed
}
autapse_count <- sum(.raw_el$pre == .raw_el$post, na.rm = TRUE)
autapse_prop <- if (nrow(.raw_el) > 0) autapse_count / nrow(.raw_el) else NA_real_

if (autapse_prop == 0 || is.na(autapse_prop)) {
  # Edgelist has autapses removed — use known value from synapse review
  message("  Edgelist has autapses removed, using reviewed proportion.")
  autapse_prop <- 0.021
}
rm(.raw_el)

df <- add_row(df, "proportion_autapses", round(autapse_prop, 4), "auto",
              "Proportion of synaptic connections that are autapses (self-connections). Results: Synapses.")

##############################################
### Supplemental Data row counts (Supps 2-7) ##
##############################################
# Reads the CSVs supplemental_data.R writes so the doc's "Supplementary Data
# N" introductions can cite live row counts via var/<key> placeholders.
# Reads files (so numbers.R doesn't have to re-load every dataset); falls
# back to NA if the supp CSVs haven't been built yet.
.supp_count <- function(idx, key, desc) {
  path <- file.path("manuscript/print/supplemental_data",
                    sprintf("supplemental_data_%d.txt", idx))
  if (!file.exists(path)) {
    message(sprintf("  %s: %s missing — value set to NA", key, basename(path)))
    df <<- add_row(df, key, NA, "auto", desc)
    return(invisible(NULL))
  }
  # Count via wc to avoid loading the (huge) BANC / maleCNS metadata CSVs.
  n <- as.integer(sub(" .*", "",
                      system2("wc", c("-l", path), stdout = TRUE)))
  if (!is.na(n)) n <- max(0L, n - 1L)   # subtract header row
  df <<- add_row(df, key, n, "auto", desc)
}
.supp_count(2, "banc_meta_export_count",    "Rows in supplemental_data_2 (BANC metadata).")
.supp_count(3, "fafb_meta_export_count",    "Rows in supplemental_data_3 (FAFB metadata).")
.supp_count(4, "manc_meta_export_count",    "Rows in supplemental_data_4 (MANC metadata).")
.supp_count(5, "malecns_meta_export_count", "Rows in supplemental_data_5 (maleCNS metadata).")
.supp_count(6, "an_dn_umap_count",          "Rows in supplemental_data_6 (AN/DN UMAP).")
.supp_count(7, "eff_umap_count",            "Rows in supplemental_data_7 (effector UMAP).")

##############################################
### Fig 6g top AN/DN targets per seed_class ###
##############################################
# panel_mbx_cx_control.R writes a CSV at figures/figure_6/links/
# fig6g_top_targets_by_seed_class.csv listing the AN/DN cell types that
# pass the influence_norm_log threshold for each upstream class. Here we
# join those into one comma-separated string per seed_class so the doc
# can cite them via var/fig6g_<seed_class>_top_targets placeholders.
################################################
### NBLAST top-match correctness (per region) ###
################################################
# R/text/nblast_top_match_correct.R produces the CSV at
# data/nblast/banc_888_top_match_correct.csv. Re-run that script when the
# NBLAST feathers refresh; numbers.R just reads the cached CSV.
.nblast_csv <- "data/nblast/banc_888_top_match_correct.csv"
if (file.exists(.nblast_csv)) {
  .nblast <- readr::read_csv(.nblast_csv, show_col_types = FALSE)
  for (.i in seq_len(nrow(.nblast))) {
    .row <- .nblast[.i, ]
    .ds  <- sub("banc_(.*?)_nblast\\.feather", "\\1", .row$feather)
    .ds  <- sub("_v?[0-9].*$", "", .ds)         # fafb_783 -> fafb, manc_v1.2.1 -> manc
    .key_pct <- sprintf("banc_region_%s_%s_nblast_top_cell_type_correct_pct",
                         .row$region, .ds)
    .key_mn  <- sprintf("banc_region_%s_%s_nblast_top_cell_type_correct_mean_nblast",
                         .row$region, .ds)
    df <- add_row(df, .key_pct,
                  sprintf("%.1f%%", .row$pct_correct), "auto",
                  sprintf("%% of %s neurons whose top NBLAST match (vs %s) is correct (cell-type identity resolved via franken.meta). Methods: Cell type matching.",
                          gsub("_", " ", .row$region), toupper(.ds)))
    df <- add_row(df, .key_mn,
                  sprintf("%.2f", .row$mean_score_correct), "auto",
                  sprintf("Mean normalised NBLAST score for %s neurons whose top match (vs %s) is correct. Methods: Cell type matching.",
                          gsub("_", " ", .row$region), toupper(.ds)))
  }
  rm(.nblast)
} else {
  message("  data/nblast/banc_888_top_match_correct.csv missing — run R/text/nblast_top_match_correct.R first")
}
rm(.nblast_csv)

.fig6g_csv <- "figures/figure_6/links/fig6g_top_targets_by_seed_class.csv"
if (file.exists(.fig6g_csv)) {
  .fig6g <- readr::read_csv(.fig6g_csv, show_col_types = FALSE)
  for (.sc in c("central_complex_output", "mushroom_body_output",
                 "visual_projection")) {
    # The CSV has one row per (seed_class, super_cluster, target); the same
    # target can appear in multiple super_clusters, so dedup on target by
    # taking its max influence across super_clusters before listing.
    .targets <- .fig6g %>%
      dplyr::filter(seed_class == .sc) %>%
      dplyr::group_by(target) %>%
      dplyr::summarise(max_inf = max(influence_norm_log_max, na.rm = TRUE),
                       .groups = "drop") %>%
      dplyr::arrange(dplyr::desc(max_inf)) %>%
      dplyr::pull(target)
    .targets_str <- paste(.targets, collapse = ", ")
    df <- add_row(df,
                  paste0("fig6g_", .sc, "_top_targets"),
                  .targets_str,
                  "auto",
                  paste0("Unique AN/DN targets above influence threshold for ",
                         .sc, " (Fig 6g), ordered by max influence_norm_log_max ",
                         "across super_clusters. Methods: Influence."))
    df <- add_row(df,
                  paste0("fig6g_", .sc, "_top_targets_count"),
                  length(.targets),
                  "auto",
                  paste0("Unique AN/DN target count above influence threshold for ",
                         .sc, " (Fig 6g). Methods: Influence."))
  }
  rm(.fig6g)
} else {
  message("  fig6g_top_targets_by_seed_class.csv missing — run panel_mbx_cx_control.R first")
}
rm(.fig6g_csv)

##########################################
### v2 synapse review — by size cut-off ##
##########################################
# Counts of True / False / Ambiguous review tags split by synapse size bin
# (size <= 5 vs size > 5). Source: master review CSV with both the original
# size > 5 sample and the 2026-05-06 size <= 5 review merged in.
.syn_review <- tryCatch({
  readr::read_csv("data/synapses/2024-09-20_aelysia_synapse_sample_complete_v2.csv",
                  show_col_types = FALSE)
}, error = function(e) {
  message("  Could not load synapse review CSV: ", conditionMessage(e))
  NULL
})
if (!is.null(.syn_review)) {
  .syn_counts <- .syn_review %>%
    dplyr::mutate(
      size_bucket = ifelse(size <= 5, "fiveorless", "morethanfive"),
      tag_norm = dplyr::case_when(
        toupper(as.character(Tags)) == "TRUE"  ~ "true",
        toupper(as.character(Tags)) == "FALSE" ~ "false",
        toupper(as.character(Tags)) == "AMBIGUOUS" ~ "ambiguous",
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::filter(!is.na(tag_norm)) %>%
    dplyr::count(size_bucket, tag_norm)
  # Per-bucket totals so we can convert counts -> percentages within bucket.
  # Format as "<x>%" string up-front so the post-processor at numbers.R:1273
  # (proportion-to-percent multiplier) leaves them untouched.
  .bucket_totals <- .syn_counts %>%
    dplyr::group_by(size_bucket) %>%
    dplyr::summarise(total = sum(n), .groups = "drop")
  for (.b in c("fiveorless", "morethanfive")) {
    .total <- .bucket_totals$total[.bucket_totals$size_bucket == .b]
    if (length(.total) == 0 || .total == 0) next
    for (.t in c("true", "false", "ambiguous")) {
      .v <- .syn_counts$n[.syn_counts$size_bucket == .b & .syn_counts$tag_norm == .t]
      if (length(.v) == 0) .v <- 0L
      .pct_str <- paste0(signif(100 * .v / .total, 3), "%")
      df <- add_row(df,
                    paste0("synapse_v2_sample_", .b, "_", .t),
                    .pct_str, "auto",
                    sprintf("v2 synapse review: percentage of %s-tagged synapses with size %s (n=%d / %d). Results: Synapses.",
                            .t,
                            ifelse(.b == "fiveorless", "<= 5", "> 5"),
                            .v, .total))
    }
  }
  rm(.syn_counts, .bucket_totals)

  # Total sample size and mean review count per (neuropil × size_bin) cell.
  # Same size bins as the figure (panel_synapse_review.R: 1-2, 3, 4, 5, 6-10,
  # 11-20, 21-50, 51-100, 101+).
  .syn_total <- nrow(.syn_review)
  df <- add_row(df, "synapse_v2_sample_size", .syn_total, "auto",
                "Total reviewed synapses in the v2 sample (size 1-345). Results: Synapses.")

  .bin_breaks <- c(0, 2, 3, 4, 5, 10, 20, 50, 100, Inf)
  .bin_labels <- c("1-2","3","4","5","6-10","11-20","21-50","51-100","101+")
  .syn_per_bin <- .syn_review %>%
    dplyr::mutate(size_bin = cut(size, breaks = .bin_breaks, labels = .bin_labels,
                                 include.lowest = TRUE, right = TRUE)) %>%
    dplyr::filter(!is.na(neuropil), !is.na(size_bin)) %>%
    dplyr::count(neuropil, size_bin)
  .mean_per_bin <- if (nrow(.syn_per_bin) > 0) round(mean(.syn_per_bin$n), 1) else NA_real_
  df <- add_row(df, "synapse_v2_sample_n_per_bin", .mean_per_bin, "auto",
                "Mean reviewed-synapse count per (neuropil x size-bin) cell in the v2 sample. Results: Synapses.")
  rm(.syn_per_bin, .syn_total, .mean_per_bin, .bin_breaks, .bin_labels)
}
rm(.syn_review)

# Synaptic completion — from gross capture rates export
message("Computing synaptic completion...")
.gross_file <- .banc_completion_capture_csv("gross", size_thresh = .banc_capture_size_thresh)
if (file.exists(.gross_file)) {
  .gross <- readr::read_csv(.gross_file, show_col_types = FALSE)
  .total_synapses <- sum(.gross$n)
  # In the v888 gross capture file, pre/post_status takes values
  # {fragment, identified, proofread}. The union of the latter two replaces
  # the legacy single value "neuron". `panel_synapse_review.R` (lines 44-46,
  # 339-340) uses the same `c("neuron","identified","proofread")` union.
  .id_levels <- c("neuron", "identified", "proofread")
  .pre_neuron  <- sum(.gross$n[.gross$pre_status  %in% .id_levels])
  .post_neuron <- sum(.gross$n[.gross$post_status %in% .id_levels])
  .both_neuron <- sum(.gross$n[.gross$pre_status  %in% .id_levels &
                                 .gross$post_status %in% .id_levels])
  .pre_pct <- round(100 * .pre_neuron / .total_synapses, 0)
  .post_pct <- round(100 * .post_neuron / .total_synapses, 0)
  .both_pct <- round(100 * .both_neuron / .total_synapses, 0)

  df <- add_row(df, "synapse_total_links", .total_synapses, "auto",
                "Total synaptic links in detection. Methods: Synaptic completeness.")
  df <- add_row(df, "synapse_pre_completeness_pct", paste0(.pre_pct, "%"), "auto",
                "Pct presynaptic ends connected to proofread neuron. Methods: Synaptic completeness.")
  df <- add_row(df, "synapse_post_completeness_pct", paste0(.post_pct, "%"), "auto",
                "Pct postsynaptic ends connected to proofread neuron. Methods: Synaptic completeness.")
  df <- add_row(df, "synapse_both_completeness_pct", paste0(.both_pct, "%"), "auto",
                "Pct synaptic links with identified neuron on both sides. Methods: Synaptic completeness.")
  rm(.gross, .total_synapses, .pre_neuron, .post_neuron, .both_neuron, .pre_pct, .post_pct, .both_pct)
} else {
  message("  Gross capture rates file not found: ", .gross_file)
}
rm(.gross_file)

###########################
### Roughly proofread   ###
###########################

# Use bm.neurons (which already excludes glia / trachea / not_a_neuron and
# status-flagged DEBRIS/NOT_A_NEURON/TRACHEA) so PR, RPR, and PR+RPR are
# guaranteed consistent with the per-class / per-region totals.
roughly_proofread_count <- bm.neurons %>%
  dplyr::filter(as.logical(roughly_proofread) %in% TRUE) %>%
  nrow()
df <- add_row(df, "roughly_proofread_neuron_count", roughly_proofread_count, "auto",
              "Roughly proofread neurons, excluding glia/trachea/not_a_neuron. Methods: Proofreading.")

proofread_plus_roughly <- bm.neurons %>%
  dplyr::filter(as.logical(proofread) %in% TRUE | as.logical(roughly_proofread) %in% TRUE) %>%
  nrow()
df <- add_row(df, "proofread_plus_roughly_proofread_neuron_count", proofread_plus_roughly, "auto",
              "Proofread or roughly proofread neurons, excluding glia/trachea/not_a_neuron. Equals proofread_neuron_count + roughly_proofread_neuron_count. Methods: Proofreading.")
df <- add_row(df, "pairwise_influence_calculations",
              paste0(signif(proofread_plus_roughly^2 / 1e9, 2)), "auto",
              "Pairwise influence calculations in billions (proofread+roughly count squared / 1e9, 2 sig figs). Methods: Influence.")

# Per super_class neuron counts within the proofread + roughly_proofread set
.super_class_counts <- bm %>%
  dplyr::filter(as.logical(proofread) %in% TRUE |
                  as.logical(roughly_proofread) %in% TRUE,
                !super_class %in% c("glia", "trachea", "not_a_neuron"),
                !is.na(super_class), super_class != "") %>%
  dplyr::count(super_class, name = "n")
for (.i in seq_len(nrow(.super_class_counts))) {
  .sc <- .super_class_counts$super_class[.i]
  df <- add_row(df,
                sprintf("banc_super_class_%s_count", .sc),
                .super_class_counts$n[.i],
                "auto",
                sprintf("Proofread + roughly_proofread neurons with super_class = %s. Methods: Proofreading.", .sc))
}
rm(.super_class_counts)

# Per super_class counts including glia/trachea/not_a_neuron, with the blank
# super_class filled from fafb_alignment_super_class (SeaTable column; if the
# GCS cache is in use and that column is absent, fall back to mapping
# fafb_cell_type → super_class via franken.meta).
.fasc <- if ("fafb_alignment_super_class" %in% colnames(bm)) {
  bm$fafb_alignment_super_class
} else if ("fafb_cell_type" %in% colnames(bm) &&
             exists("franken.meta") &&
             all(c("fafb_cell_type", "super_class") %in% colnames(franken.meta))) {
  .fa_lookup <- franken.meta %>%
    dplyr::filter(!is.na(fafb_cell_type), fafb_cell_type != "",
                  !is.na(super_class), super_class != "") %>%
    dplyr::distinct(fafb_cell_type, .keep_all = TRUE) %>%
    dplyr::select(fafb_cell_type, super_class) %>%
    tibble::deframe()
  unname(.fa_lookup[bm$fafb_cell_type])
} else {
  rep(NA_character_, nrow(bm))
}

bm_with_fasc <- bm %>%
  dplyr::mutate(super_class_with_fafb = dplyr::coalesce(
    dplyr::na_if(super_class, ""),
    dplyr::na_if(.fasc, "")
  ))

.combined_sc_counts <- bm_with_fasc %>%
  dplyr::filter(as.logical(proofread) %in% TRUE |
                  as.logical(roughly_proofread) %in% TRUE,
                !is.na(super_class_with_fafb), super_class_with_fafb != "") %>%
  dplyr::count(super_class_with_fafb, name = "n")
for (.i in seq_len(nrow(.combined_sc_counts))) {
  .sc <- .combined_sc_counts$super_class_with_fafb[.i]
  df <- add_row(df,
                sprintf("banc_super_class_with_fafb_%s_count", .sc),
                .combined_sc_counts$n[.i],
                "auto",
                sprintf("Proofread + roughly_proofread cells with super_class = %s (super_class filled from fafb_alignment_super_class when blank). Includes glia/trachea/not_a_neuron categories. Methods: Proofreading.", .sc))
}
rm(.combined_sc_counts)

# Proofread / roughly-proofread cells with no super_class (after the
# fafb_alignment_super_class fallback) AND whose status does not flag them as
# non-neurons. These are cells we have proofread but cannot place into the
# super_class taxonomy.
.no_sc_count <- bm_with_fasc %>%
  dplyr::filter(as.logical(proofread) %in% TRUE |
                  as.logical(roughly_proofread) %in% TRUE,
                is.na(super_class_with_fafb) | super_class_with_fafb == "",
                !grepl("NOT_A_NEURON|GLIA|TRACHEA", status)) %>%
  nrow()
df <- add_row(df, "neuron_no_super_class", .no_sc_count, "auto",
              "Proofread + roughly_proofread cells with no super_class assignment (after fafb_alignment_super_class fallback), excluding cells flagged in status as NOT_A_NEURON/GLIA/TRACHEA. Methods: Proofreading.")
rm(.no_sc_count, .fasc, bm_with_fasc)

# Per region neuron counts within the proofread + roughly_proofread set
.region_counts <- bm %>%
  dplyr::filter(as.logical(proofread) %in% TRUE |
                  as.logical(roughly_proofread) %in% TRUE,
                !super_class %in% c("glia", "trachea", "not_a_neuron"),
                !is.na(region), region != "") %>%
  dplyr::count(region, name = "n")
for (.i in seq_len(nrow(.region_counts))) {
  .rg <- .region_counts$region[.i]
  df <- add_row(df,
                sprintf("banc_region_%s_count", .rg),
                .region_counts$n[.i],
                "auto",
                sprintf("Proofread + roughly_proofread neurons in region = %s. Methods: Proofreading.", .rg))
}
rm(.region_counts)

# Per-region sensory and effector fractions used in Fig 3a legend
# (L208: "35% of VNC neurons are sensory and 4% are effectors, versus 16%
# and 0.5% in the central brain"). Denominator = PR + RPR within the region
# (matches the banc_region_*_count totals above). Numerator = neurons in
# the region whose super_class is sensory or effector.
.pr_rpr_for_region <- bm.neurons %>%
  dplyr::filter(as.logical(proofread) %in% TRUE |
                  as.logical(roughly_proofread) %in% TRUE,
                !is.na(region), region != "")
for (.rg_key in c("central_brain", "ventral_nerve_cord")) {
  .rg_short <- if (.rg_key == "central_brain") "cb" else "vnc"
  .rg_sub <- .pr_rpr_for_region %>% dplyr::filter(region == .rg_key)
  .rg_n   <- nrow(.rg_sub)
  if (.rg_n > 0) {
    .sens_n <- .rg_sub %>% dplyr::filter(grepl("sensory", super_class)) %>% nrow()
    .eff_n  <- .rg_sub %>% dplyr::filter(super_class %in% c("motor", "visceral_circulatory",
                                                              "ascending_visceral_circulatory")) %>% nrow()
    df <- add_row(df,
                  sprintf("%s_sensory_pct_within_region", .rg_short),
                  paste0(signif(100 * .sens_n / .rg_n, 2), "%"),
                  "auto",
                  sprintf("Percent of PR+RPR %s neurons whose super_class contains 'sensory'. Fig 3a legend.",
                          gsub("_", " ", .rg_key)))
    df <- add_row(df,
                  sprintf("%s_effector_pct_within_region", .rg_short),
                  paste0(signif(100 * .eff_n / .rg_n, 2), "%"),
                  "auto",
                  sprintf("Percent of PR+RPR %s neurons in motor / visceral_circulatory / ascending_visceral_circulatory. Fig 3a legend.",
                          gsub("_", " ", .rg_key)))
  }
}
rm(.pr_rpr_for_region)

# v626 → v888 ID drift among proofread/roughly_proofread neurons. Tracks how
# many cited cell IDs in the preprint will resolve differently in the print
# version, and how many are new since the preprint snapshot.
#
#   changed = root_626 ≠ root_888 (same neuron, ID renumbered by merge/split)
#   added   = first marked proofread on or after the v626 release date
#             (2025-08-01); resolved via CAVE backbone_proofread joined on
#             pt_supervoxel_id (stable across re-segmentations). RPR-only
#             neurons (no CAVE entry) are excluded from "added" by
#             construction — there is no analogous timestamp source for
#             roughly_proofread, so we under-count them rather than guess.
.v626_cutoff <- as.POSIXct("2025-08-01", tz = "UTC")
.changed <- NA_integer_
.added   <- NA_integer_
if (all(c("root_626", "root_888", "supervoxel_id") %in% colnames(bm))) {
  .pr_v626 <- bm %>%
    dplyr::filter(as.logical(proofread) %in% TRUE |
                    as.logical(roughly_proofread) %in% TRUE) %>%
    dplyr::mutate(supervoxel_id = as.character(supervoxel_id),
                  root_888 = as.character(root_888),
                  root_626 = as.character(root_626))
  .pr_total <- nrow(.pr_v626)
  .changed <- sum(!is.na(.pr_v626$root_626) &
                    .pr_v626$root_626 != .pr_v626$root_888,
                  na.rm = TRUE)
  .first_pf <- tryCatch({
    bp <- bancr::banc_backbone_proofread()
    bp %>%
      dplyr::filter(valid == TRUE, proofread == TRUE) %>%
      dplyr::mutate(pt_supervoxel_id = as.character(pt_supervoxel_id)) %>%
      dplyr::group_by(pt_supervoxel_id) %>%
      dplyr::summarise(first_created = min(created), .groups = "drop")
  }, error = function(e) {
    message("  banc_backbone_proofread failed (", conditionMessage(e),
            ") — 'added' will be NA")
    NULL
  })
  if (!is.null(.first_pf)) {
    .pr_joined <- .pr_v626 %>%
      dplyr::left_join(.first_pf,
                        by = c("supervoxel_id" = "pt_supervoxel_id"))
    .added <- sum(!is.na(.pr_joined$first_created) &
                    .pr_joined$first_created >= .v626_cutoff)
    rm(.pr_joined, .first_pf)
  }
  .changed_pct <- if (.pr_total > 0) {
    sprintf("%.1f%%", 100 * .changed / .pr_total)
  } else NA_character_
  df <- add_row(df, "proofread_roughly_proofread_root_ids_changed_v626_to_v888",
                .changed, "auto",
                "PR/RPR neurons whose root_id changed v626 → v888 (root_626 ≠ root_888). Methods: Proofreading.")
  df <- add_row(df, "proofread_roughly_proofread_root_ids_changed_v626_to_v888_pct",
                .changed_pct, "auto",
                "Pct of PR/RPR neurons whose root_id changed v626 → v888. Methods: Proofreading.")
  df <- add_row(df, "proofread_roughly_proofread_root_ids_added_v626_to_v888",
                .added, "auto",
                "PR/RPR neurons first proofread on/after v626 release (2025-08-01). Methods: Proofreading.")
  rm(.pr_v626, .pr_total, .changed_pct)
}
rm(.v626_cutoff, .changed, .added)

###########################
### NT ground truth     ###
###########################

# Total cell types in the GT dataset (across FAFB/MANC/Hemibrain, before BANC matching)
message("Computing NT ground truth total...")
.gt_file <- "/Users/GD/LMBD/Papers/synister/drosophila_neurotransmitters/gt_data.csv"
if (file.exists(.gt_file)) {
  .gt_data <- readr::read_csv(.gt_file, col_types = readr::cols(.default = "c"))
  df <- add_row(df, "nt_gt_total_cell_types", length(unique(.gt_data$cell_type)), "auto",
                "Total cell types in NT ground truth dataset (all datasets). Methods: NT prediction.")
  rm(.gt_data)
} else {
  message("  NT ground truth file not found: ", .gt_file)
}

###########################
### Proofreader count   ###
###########################

message("Computing proofreader count from CAVE logs...")
tryCatch({
  .edits <- bancr::banc_cave_query("proofreading_edits", live = 2)
  .edit_counts <- .edits %>%
    dplyr::group_by(user_id) %>%
    dplyr::summarise(n_edits = dplyr::n(), .groups = "drop")
  .proofreader_n <- sum(.edit_counts$n_edits >= 100)
  df <- add_row(df, "proofreader_number", .proofreader_n, "auto",
                "Individuals with >=100 proofreading edits. Fig 1 legend.")
  rm(.edits, .edit_counts, .proofreader_n)
}, error = function(e) {
  message("  Could not query proofreading_edits from CAVE: ", e$message)
  df <<- add_row(df, "proofreader_number", 155, "hardcoded",
                 "Individuals with >=100 edits (hardcoded fallback). Fig 1 legend.")
})

###########################
### Neuron counts       ###
###########################

# Proofread neurons
proofread_count <- bm.neurons %>%
  dplyr::filter(proofread == TRUE) %>% nrow()
df <- add_row(df, "proofread_neuron_count", proofread_count, "auto",
              "Proofread neurons (strict proofread only). Fig 1 legend.")

# Identified entities — the broader pool used for synapse-completion
# accounting in the paper (proofread + roughly_proofread + anything with
# a cell_type assigned + any glia/trachea entity). Must be >= PR+RPR by
# construction. Same definition is intended to be used by bancpipeline for
# its synaptic-completion calculations.
identified_count <- bm %>%
  dplyr::filter(
    as.logical(proofread)         %in% TRUE |
    as.logical(roughly_proofread) %in% TRUE |
    (!is.na(cell_type) & cell_type != "") |
    grepl("glia|trachea", super_class, ignore.case = TRUE)
  ) %>% nrow()
df <- add_row(df, "identified_neuron_count", identified_count, "auto",
              "Identified entities: PR + RPR + any with cell_type set + glia/trachea. Fig 1 legend.")

# Total neurons (all non-glia/trachea/not_a_neuron)
neuron_count <- nrow(bm.neurons)
df <- add_row(df, "total_neuron_count", neuron_count, "auto",
              "Total neuron segments in BANC. Introduction.")

# Nuclei count
nuclei_count <- length(unique(na.omit(bm$nucleus_id[bm$nucleus_id != ""])))
df <- add_row(df, "banc_nuclei_count", nuclei_count, "auto",
              "Unique nuclei detected in BANC. Methods: Segmentation.")

# Mitochondria count
# Hard-coded from the row count of the v888 human-readable mitochondria
# CSV at:
#   gs://lee-lab_brain-and-nerve-cord-fly-connectome/neuron_annotations/
#     v888/mitochondria_v1_human_readable.csv.gz
# The CSV has no header; row count == mitochondria count.
# Computed 2026-05-22 via:
#   gsutil cat <gcs_path> | gunzip | wc -l   # ≈ 80 s on a 1.0 GB .csv.gz
# Re-running on every numbers.R refresh is unnecessary; uncomment the
# live block below if the CSV is ever rebuilt.
.mito_count <- 38928244L
# .mito_count <- as.integer(system2(
#   "bash", c("-c", paste(
#     "gsutil cat",
#     "gs://lee-lab_brain-and-nerve-cord-fly-connectome/neuron_annotations/",
#     "v888/mitochondria_v1_human_readable.csv.gz",
#     "| gunzip | wc -l")), stdout = TRUE))
df <- add_row(df, "banc_mitochondria_count", .mito_count, "auto",
              "Mitochondria detections in BANC v888 (CAVE mitochondria_v1 segmentation; row count of mitochondria_v1_human_readable.csv.gz). Methods: Segmentation.")
rm(.mito_count)

###########################
### Cell type counts    ###
###########################

# Ensure alignment columns exist
for (.col in c("fafb_alignment_cell_type", "fafb_alignment_decision",
               "manc_alignment_type", "malecns_alignment_type")) {
  if (!.col %in% colnames(bm.neurons)) bm.neurons[[.col]] <- NA_character_
}

# Cell typed neurons (total) — includes alignment matches
.has_ct <- !is.na(bm.neurons$cell_type) & bm.neurons$cell_type != ""
.has_alignment <- (!is.na(bm.neurons$fafb_alignment_cell_type) &
                     bm.neurons$fafb_alignment_cell_type != "" &
                     (is.na(bm.neurons$fafb_alignment_decision) |
                        bm.neurons$fafb_alignment_decision != "F")) |
                  (!is.na(bm.neurons$manc_alignment_type) & bm.neurons$manc_alignment_type != "") |
                  (!is.na(bm.neurons$malecns_alignment_type) & bm.neurons$malecns_alignment_type != "")
.typed_or_aligned <- .has_ct | .has_alignment

ct_count <- sum(.has_ct)
ct_aligned_count <- sum(.typed_or_aligned)
df <- add_row(df, "cell_typed_neuron_count", ct_aligned_count, "auto",
              "Neurons with cell type or alignment type annotation. Results: Cell types.")
df <- add_row(df, "cell_typed_neuron_no_aligned_types_count", ct_count, "auto",
              "Neurons with cell type annotation only (excluding alignment matches). Results: Cell types.")
df <- add_row(df, "aligned_types_count", ct_aligned_count - ct_count, "auto",
              "Neurons with alignment type but no cell type (fafb/manc/malecns alignment only). Results: Cell types.")

# Cell typed in CB + VNC
.is_cb_vnc <- bm.neurons$region %in% c("central_brain", "ventral_nerve_cord")
ct_cb_vnc_denom <- sum(.is_cb_vnc)
df <- add_row(df, "cell_typed_neuron_proportion_cb_vnc",
              sum(.typed_or_aligned & .is_cb_vnc) / ct_cb_vnc_denom, "auto",
              "Proportion of CB+VNC neurons with cell type or alignment type. Results: Cell types.")
df <- add_row(df, "cell_typed_neuron_no_aligned_types_proportion_cb_vnc",
              sum(.has_ct & .is_cb_vnc) / ct_cb_vnc_denom, "auto",
              "Proportion of CB+VNC neurons with cell type only (no alignment). Results: Cell types.")

# Overall proportion (hardcoded denominator: ~166k total CNS minus ~9390 lamina)
.cns_denom <- 166000 - 9390
df <- add_row(df, "expected_cns_neuron_count", 166000, "hardcoded",
              "Expected total CNS neuron count from franken meta. Results: Cell types.")
df <- add_row(df, "missing_lamina_count", 9390, "hardcoded",
              "Lamina neuron count missing from BANC. Results: Cell types.")
df <- add_row(df, "cell_typed_neuron_proportion", ct_aligned_count / .cns_denom, "auto",
              "Proportion of expected CNS neurons typed (incl alignment). Results: Cell types.")
df <- add_row(df, "cell_typed_neuron_no_aligned_types_proportion", ct_count / .cns_denom, "auto",
              "Proportion of expected CNS neurons typed (excl alignment). Results: Cell types.")
rm(.has_ct, .has_alignment, .typed_or_aligned, .is_cb_vnc, .cns_denom)

# ---- Optic-lobe cell-type coverage (FAFB alignment Methods paragraph) ------
#
# Feeds the Methods sentence "In the optic lobes specifically, N of M proofread
# neurons (P%) lacked a human-verified cell type ...". Previously these six
# numbers lived only as prose in bancpipeline at alignment/method.txt, with no
# var/ identity and no script emitting them; they drifted until the sentence was
# arithmetically impossible (the stated left-hand untyped count exceeded the
# stated total). Tracking them here keeps the doc in sync via the sheet.
#
# "Human-verified" mirrors the ground-truth rule in
# bancpipeline/alignment/presets/optic-lobe/prep.R:152-160 — a type counts only
# if it is non-blank, not `auto:`-prefixed (those are pipeline output, 56k of
# them), and not an unknown/fragment/glia placeholder. fafb_alignment_cell_type
# is deliberately NOT counted as verified: it is a pipeline suggestion awaiting
# human review, and conflating the two is what produced the original error.
.human_typed <- function(x) {
  !is.na(x) & trimws(x) != "" &
    !grepl("^auto:", x) &
    !grepl("unknown|fragment|glia", tolower(x))
}

.optic <- bm %>%
  dplyr::filter(as.logical(proofread) %in% TRUE |
                  as.logical(roughly_proofread) %in% TRUE,
                !super_class %in% c("glia", "trachea", "not_a_neuron"),
                region == "optic_lobe")

.optic_verified <- .human_typed(.optic$cell_type) |
                     .human_typed(.optic$fafb_cell_type)
.optic_suggested <- !is.na(.optic$fafb_alignment_cell_type) &
                      trimws(.optic$fafb_alignment_cell_type) != ""

df <- add_row(df, "optic_lobe_neuron_count", nrow(.optic), "auto",
              "Proofread + roughly_proofread optic lobe neurons, excluding glia/trachea/not_a_neuron. Methods: Cross-dataset cell type assignment.")
df <- add_row(df, "optic_lobe_untyped_count", sum(!.optic_verified), "auto",
              "Optic lobe neurons with no human-verified cell type (cell_type / fafb_cell_type, excluding auto: and placeholder terms). Methods: Cross-dataset cell type assignment.")
df <- add_row(df, "optic_lobe_untyped_proportion",
              sum(!.optic_verified) / nrow(.optic), "auto",
              "Proportion of optic lobe neurons lacking a human-verified cell type. Methods: Cross-dataset cell type assignment.")

# Guard the side comparison against NA. banc.meta as enriched here carries NA
# sides for a minority of optic-lobe rows (the GCS cache does not), and a bare
# `side == "right"` propagates those NAs straight through sum(), turning every
# per-side figure into NA. Tracked below so that right + left failing to sum to
# the total is visible rather than silent — that is precisely the flaw that made
# the original Methods sentence impossible to reconcile.
for (.sd in c("right", "left")) {
  .m <- !is.na(.optic$side) & .optic$side == .sd
  df <- add_row(df, sprintf("optic_lobe_%s_neuron_count", .sd), sum(.m), "auto",
                sprintf("Proofread + roughly_proofread optic lobe neurons on the %s. Methods: Cross-dataset cell type assignment.", .sd))
  df <- add_row(df, sprintf("optic_lobe_%s_untyped_count", .sd),
                sum(!.optic_verified & .m), "auto",
                sprintf("Optic lobe neurons on the %s with no human-verified cell type. Methods: Cross-dataset cell type assignment.", .sd))
  df <- add_row(df, sprintf("optic_lobe_%s_untyped_proportion", .sd),
                sum(!.optic_verified & .m) / sum(.m), "auto",
                sprintf("Proportion of %s optic lobe neurons lacking a human-verified cell type. Methods: Cross-dataset cell type assignment.", .sd))
}

df <- add_row(df, "optic_lobe_side_unassigned_count",
              sum(is.na(.optic$side) | trimws(.optic$side) == ""), "auto",
              "Optic lobe neurons with no side assignment; the amount by which right + left falls short of the total. Methods: Cross-dataset cell type assignment.")

# Split of the untyped pool into "pipeline has proposed something, awaiting
# review" versus "nothing at all" — the algorithm's outcome, quoted as a
# separate sentence so it is never mistaken for the pre-alignment input state.
df <- add_row(df, "optic_lobe_alignment_suggested_count",
              sum(!.optic_verified & .optic_suggested), "auto",
              "Optic lobe neurons with no human-verified type but carrying an unreviewed fafb_alignment_cell_type suggestion. Methods: Cross-dataset cell type assignment.")
df <- add_row(df, "optic_lobe_no_type_at_all_count",
              sum(!.optic_verified & !.optic_suggested), "auto",
              "Optic lobe neurons with neither a human-verified type nor an alignment suggestion. Methods: Cross-dataset cell type assignment.")
df <- add_row(df, "optic_lobe_no_type_at_all_proportion",
              sum(!.optic_verified & !.optic_suggested) / nrow(.optic), "auto",
              "Proportion of optic lobe neurons with no cell type of any kind after alignment. Methods: Cross-dataset cell type assignment.")

# Counted from the FAFB reference set the optic-lobe alignment runs against:
# distinct non-blank cell_type in
# bancpipeline/data/optic_lobe/fafb_optic_both_meta.csv. Hardcoded because that
# file is a bancpipeline artefact, not a BANC-project input.
df <- add_row(df, "fafb_optic_lobe_type_count", 685, "hardcoded",
              "Distinct FAFB optic lobe cell types used as alignment targets (bancpipeline/data/optic_lobe/fafb_optic_both_meta.csv). Methods: Cross-dataset cell type assignment.")

rm(.human_typed, .optic, .optic_verified, .optic_suggested)

# Unique cell type count
total_ct_unique <- bm.neurons %>%
  dplyr::filter(!is.na(cell_type), cell_type != "") %>%
  dplyr::distinct(cell_type) %>% nrow()
df <- add_row(df, "unique_cell_type_count", total_ct_unique, "auto",
              "Number of unique cell types annotated. Results: Cell types.")

###########################
### Neurotransmitter    ###
###########################

nt.removed.cts <- c('Tm9', 'Tm4', 'Tm2', 'Tm1', 'TmY20', 'Mi1', 'DVMn 1a-c',
                    'TTMn', 'Fe reductor MN', 'b1 MN', 'MNwm36', 'Acc. ti flexor MN',
                    'MNhl68','MNhm42', 'MNhm03', 'Tr flexor MN', 'Tr extensor MN', 'b3 MN',
                    'hg1 MN', 'hg3 MN', 'i1 MN', 'i2 MN', 'tp1 MN', 'tp2 MN', 'tpn MN',
                    'b2 MN', 'ltm2-femur MN', 'ltm1-tibia MN', 'ps2 MN',
                    'Ti flexor MN', 'Tergopleural/Pleural promotor MN', 'MNnm03',
                    'ps1 MN', 'MNhl65', 'MNhl73', 'MNml79')

# NT ground truth cell types: distinct cell_types among BANC neurons
# with verified neurotransmitter (the cell_type count of the same
# subset that produces banc_nt_neuron_count below), minus excluded
# motor types.
nt_gt_ct <- bm %>%
  dplyr::filter(!is.na(neurotransmitter_verified), neurotransmitter_verified != "",
                !cell_type %in% nt.removed.cts) %>%
  dplyr::distinct(cell_type) %>% nrow()
df <- add_row(df, "nt_gt_cell_type_count", nt_gt_ct, "auto",
              "NT ground truth cell types: distinct cell_types among BANC neurons with verified neurotransmitter (the cell_type count of banc_nt_neuron_count, motor types excluded). Methods: NT prediction.")

# NT verified neurons in BANC
nt_n_gt <- bm %>%
  dplyr::filter(!is.na(neurotransmitter_verified), neurotransmitter_verified != "",
                !cell_type %in% nt.removed.cts) %>% nrow()
df <- add_row(df, "banc_nt_neuron_count", nt_n_gt, "auto",
              "BANC neurons with verified NT (minus excluded types). Methods: NT prediction.")

# NT verified cell types in BANC
nt_ct_gt <- bm %>%
  dplyr::filter(!is.na(neurotransmitter_verified), neurotransmitter_verified != "",
                !cell_type %in% nt.removed.cts) %>%
  dplyr::distinct(cell_type) %>% nrow()
df <- add_row(df, "banc_nt_cell_type_count", nt_ct_gt, "auto",
              "BANC cell types with verified NT. Methods: NT prediction.")

#########################################
### Neuropeptide × predicted NT split ###
#########################################
# Among neurons with a verified neuropeptide (`neuropeptide_verified`
# non-empty in BANC meta), the % falling into each `neurotransmitter_predicted`
# category. NA / empty NT predictions are reported under `none`.
np_verified <- bm %>%
  dplyr::filter(!is.na(neuropeptide_verified), neuropeptide_verified != "")
np_total <- nrow(np_verified)
df <- add_row(df, "banc_neuropeptide_verified_count", np_total, "auto",
              "BANC neurons with a verified neuropeptide. Methods: NT prediction.")

np_by_nt <- np_verified %>%
  dplyr::mutate(nt_pred = dplyr::if_else(
    is.na(neurotransmitter_predicted) | neurotransmitter_predicted == "",
    "none", neurotransmitter_predicted
  )) %>%
  dplyr::count(nt_pred, sort = TRUE) %>%
  dplyr::mutate(pct = 100 * n / np_total)

for (i in seq_len(nrow(np_by_nt))) {
  nt_pred <- np_by_nt$nt_pred[i]
  n_i     <- np_by_nt$n[i]
  pct_i   <- np_by_nt$pct[i]
  df <- add_row(df, paste0("banc_neuropeptide_verified_pct_", nt_pred),
                sprintf("%.1f%%", pct_i), "auto",
                sprintf("%% of neurons with verified neuropeptide whose neurotransmitter_predicted is %s (n=%d / %d). Methods: NT prediction.",
                        nt_pred, n_i, np_total))
  df <- add_row(df, paste0("banc_neuropeptide_verified_n_", nt_pred),
                n_i, "auto",
                sprintf("Count of neurons with verified neuropeptide whose neurotransmitter_predicted is %s. Methods: NT prediction.",
                        nt_pred))
}

###########################
### AN/DN counts        ###
###########################

# AN neurons and cell types
an_count <- bm %>% dplyr::filter(super_class == "ascending") %>% nrow()
df <- add_row(df, "an_neuron_count", an_count, "auto",
              "Ascending neuron count. Results: Neck connective.")

an_matched <- bm %>% dplyr::filter(super_class == "ascending", !is.na(manc_match), manc_match != "") %>% nrow()
an_not_matched <- an_count - an_matched
df <- add_row(df, "an_neuron_matched_count", an_matched, "auto",
              "ANs with MANC match. Results: Neck connective.")
df <- add_row(df, "an_neuron_not_matched_count", an_not_matched, "auto",
              "ANs without MANC match. Results: Neck connective.")

an_ct <- bm %>% dplyr::filter(super_class == "ascending") %>% dplyr::distinct(cell_type) %>% nrow()
df <- add_row(df, "an_cell_type_count", an_ct, "auto",
              "AN cell type count. Results: Neck connective.")

# DN neurons and cell types
dn_count <- bm %>% dplyr::filter(super_class == "descending") %>% nrow()
df <- add_row(df, "dn_neuron_count", dn_count, "auto",
              "Descending neuron count. Results: Neck connective.")

df <- add_row(df, "an_dn_count", an_count + dn_count, "auto",
              "Total AN + DN neurons. Results: Neck connective.")

# Number of distinct AN/DN named super_clusters (functional groupings used
# in Fig 3 and the AN/DN naming Methods). Counts unique non-empty
# super_cluster values among neurons whose super_class is ascending or
# descending.
an_dn_cluster_count <- if ("super_cluster" %in% colnames(bm)) {
  bm %>%
    dplyr::filter(super_class %in% c("ascending", "descending"),
                  !is.na(super_cluster), super_cluster != "") %>%
    dplyr::distinct(super_cluster) %>% nrow()
} else { NA_integer_ }
df <- add_row(df, "an_dn_cluster_count", an_dn_cluster_count, "auto",
              "Distinct named AN/DN super_clusters in banc.meta (super_class in {ascending, descending}). Methods: Naming AN/DN clusters.")

dn_matched <- bm %>% dplyr::filter(super_class == "descending", !is.na(fafb_match), fafb_match != "") %>% nrow()
dn_not_matched <- dn_count - dn_matched
df <- add_row(df, "dn_neuron_matched_count", dn_matched, "auto",
              "DNs with FAFB match. Results: Neck connective.")
df <- add_row(df, "dn_neuron_not_matched_count", dn_not_matched, "auto",
              "DNs without FAFB match. Results: Neck connective.")

dn_ct <- bm %>% dplyr::filter(super_class == "descending") %>% dplyr::distinct(cell_type) %>% nrow()
df <- add_row(df, "dn_cell_type_count", dn_ct, "auto",
              "DN cell type count. Results: Neck connective.")

# Female-specific AN/DN counts (from sexually_dimorphic column)
if ("sexually_dimorphic" %in% colnames(bm)) {
  .dn_fem_specific <- bm %>%
    dplyr::filter(super_class == "descending",
                  grepl("female.specific", sexually_dimorphic, ignore.case = TRUE))
  df <- add_row(df, "dn_female_specific_neuron_count", nrow(.dn_fem_specific), "auto",
                "Female-specific DN neurons identified. Methods: Sexual dimorphism.")
  df <- add_row(df, "dn_female_specific_cell_type_count",
                dplyr::n_distinct(.dn_fem_specific$cell_type), "auto",
                "Female-specific DN cell types. Methods: Sexual dimorphism.")

  .an_fem_specific <- bm %>%
    dplyr::filter(super_class == "ascending",
                  grepl("female.specific", sexually_dimorphic, ignore.case = TRUE))
  df <- add_row(df, "an_female_specific_neuron_count", nrow(.an_fem_specific), "auto",
                "Female-specific AN neurons identified. Methods: Sexual dimorphism.")
  df <- add_row(df, "an_female_specific_cell_type_count",
                dplyr::n_distinct(.an_fem_specific$cell_type), "auto",
                "Female-specific AN cell types. Methods: Sexual dimorphism.")
  rm(.dn_fem_specific, .an_fem_specific)
}

# Developmental issue AN/DN count
.dev_issue <- bm %>%
  dplyr::filter(super_class %in% c("ascending", "descending"),
                grepl("DEVELOPMENTAL_ERROR", status, ignore.case = TRUE))
df <- add_row(df, "an_dn_developmental_issue_count", nrow(.dev_issue), "auto",
              "AN/DNs with developmental abnormality or stochastic arbor. Results: Neck connective.")
rm(.dev_issue)

# Approximate counts (2 significant figures)
df <- add_row(df, "ascending_neurons_approx", signif(an_count, 2), "auto",
              "Ascending neuron count rounded to 2 sig figs. Introduction.")
df <- add_row(df, "descending_neurons_approx", signif(dn_count, 2), "auto",
              "Descending neuron count rounded to 2 sig figs. Introduction.")

# Sensory descending
dn_sensory <- bm %>% dplyr::filter(super_class == "sensory_descending") %>% nrow()
dn_sensory_ct <- bm %>% dplyr::filter(super_class == "sensory_descending") %>% dplyr::distinct(cell_type) %>% nrow()
df <- add_row(df, "dn_sensory_count", dn_sensory, "auto", "Sensory descending neuron count. Results: Neck connective.")
df <- add_row(df, "dn_sensory_cell_type_count", dn_sensory_ct, "auto", "Sensory descending cell type count. Results: Neck connective.")

# Sensory ascending
an_sensory <- bm %>% dplyr::filter(super_class == "sensory_ascending") %>% nrow()
an_sensory_ct <- bm %>% dplyr::filter(super_class == "sensory_ascending") %>% dplyr::distinct(cell_type) %>% nrow()
df <- add_row(df, "an_sensory_count", an_sensory, "auto", "Sensory ascending neuron count. Results: Neck connective.")
df <- add_row(df, "an_sensory_cell_type_count", an_sensory_ct, "auto", "Sensory ascending cell type count. Results: Neck connective.")

# Ascending visceral circulatory
an_eff <- bm %>% dplyr::filter(super_class == "ascending_visceral_circulatory") %>% nrow()
an_eff_ct <- bm %>% dplyr::filter(super_class == "ascending_visceral_circulatory") %>% dplyr::distinct(cell_type) %>% nrow()
df <- add_row(df, "an_efferent_count", an_eff, "auto", "Ascending visceral circulatory neuron count. Results: Neck connective.")
df <- add_row(df, "an_efferent_cell_type_count", an_eff_ct, "auto", "Ascending visceral circulatory cell type count. Results: Neck connective.")

###########################
### Neck connective     ###
###########################

neck_efferent <- bm %>%
  dplyr::filter(super_class == "motor",
                grepl("neck", cell_class) | grepl("neck", cell_sub_class)) %>% nrow()
df <- add_row(df, "neck_efferent_count", neck_efferent, "auto",
              "Neck motor neuron count. Results: Neck connective.")

# Neck neuron segments (region=neck + AN + DN)
neck_all <- bm %>%
  dplyr::filter(grepl("ascending|descending", super_class)) %>%
  dplyr::filter(!super_class %in% c("glia", "trachea", "not_a_neuron")) %>%
  nrow()
cave_missing <- neck_connective_all %>%
  dplyr::filter(!(root_id %in% bm$root_id)) %>% nrow()
df <- add_row(df, "neck_count_all", neck_all + cave_missing, "auto",
              "All neck connective neuron segments (meta + CAVE seed plane). Results: Neck connective.")

neck_proofread <- bm %>%
  dplyr::filter(grepl("ascending|descending", super_class)) %>%
  dplyr::filter(!super_class %in% c("glia", "trachea", "not_a_neuron")) %>%
  dplyr::filter(proofread == TRUE) %>% nrow()
df <- add_row(df, "neck_proofread", neck_proofread, "auto",
              "Proofread neck connective neurons. Results: Neck connective.")

# CAVE seed plane total
df <- add_row(df, "neck_cave_total", nrow(neck_connective_all), "auto",
              "Neurons crossing neck seed planes (CAVE). Results: Neck connective.")

###########################
### Sensory / efferent  ###
###########################

# Gated on PR+RPR + non-neuron exclusion so totals reconcile across categories.
sensory_count <- bm.neurons %>%
  dplyr::filter(grepl("sensory", super_class),
                as.logical(proofread) %in% TRUE |
                  as.logical(roughly_proofread) %in% TRUE) %>%
  nrow()
df <- add_row(df, "total_sensory_count", sensory_count, "auto",
              "Sensory neurons (super_class contains 'sensory') among proofread + roughly_proofread, excluding non-neurons. Results: Sensory.")

sensory_orphan <- if ("cell_sub_class" %in% colnames(bm)) {
  bm %>%
    dplyr::filter(grepl("sensory", super_class),
                  grepl("orphan", cell_sub_class) | is.na(cell_sub_class)) %>% nrow()
} else {
  bm %>% dplyr::filter(grepl("sensory", super_class)) %>% nrow()  # assume all orphan if no subclass
}
df <- add_row(df, "orphan_sensory_count", sensory_orphan, "auto",
              "Sensory neurons without identified nerve (orphan). Results: Sensory.")
df <- add_row(df, "sensory_used_count", sensory_count - sensory_orphan, "auto",
              "Sensory neurons with identified nerve. Results: Sensory.")

# total_efferent_count uses flow == "efferent" (i.e. anything that exits the
# CNS) — this includes the strand-neuron SNpp54 cells which are super_class
# "sensory" but flow "efferent". effector_neuron_count below uses super_class
# strict, so the delta between the two is exactly those sensory-flagged
# efferents.
efferent_count <- bm.neurons %>%
  dplyr::filter(flow == "efferent",
                as.logical(proofread) %in% TRUE |
                  as.logical(roughly_proofread) %in% TRUE) %>%
  nrow()
df <- add_row(df, "total_efferent_count", efferent_count, "auto",
              "Efferent neurons (flow == 'efferent') among PR + RPR; includes sensory-flagged strand neurons (e.g. SNpp54). Results: Effectors.")

# Effector category coverage
.eff_with_bp <- bm %>%
  dplyr::filter(grepl("motor|visceral", super_class),
                !is.na(body_part_effector), body_part_effector != "",
                tolower(body_part_effector) != "unknown") %>% nrow()
df <- add_row(df, "effector_category_pct",
              paste0(round(100 * .eff_with_bp / efferent_count, 1), "%"), "auto",
              "Pct effectors with known body_part_effector. Results: Effectors.")

# Orphan effectors (no body_part_effector or unknown)
df <- add_row(df, "orphan_effector_count", efferent_count - .eff_with_bp, "auto",
              "Effectors without body_part_effector assignment. Results: Effectors.")
rm(.eff_with_bp)

# Sensory category coverage.
#
# BUGFIX 2026-07-20: this reported 103.2%, which is impossible for a coverage
# figure. The numerator was drawn from `bm` (every row in the meta) while the
# denominator `sensory_count` is restricted to bm.neurons AND to proofread +
# roughly_proofread. Sensory neurons that are unproofread, or flagged as
# non-neurons, counted towards the numerator but not the denominator. Both
# sides are now computed over one set, which also makes the count of
# still-unassigned sensory neurons meaningful.
.sens_set <- bm.neurons %>%
  dplyr::filter(grepl("sensory", super_class),
                as.logical(proofread) %in% TRUE |
                  as.logical(roughly_proofread) %in% TRUE)
.sens_known <- !is.na(.sens_set$body_part_sensory) &
                 .sens_set$body_part_sensory != "" &
                 tolower(.sens_set$body_part_sensory) != "unknown"
df <- add_row(df, "sensory_category_pct",
              paste0(round(100 * sum(.sens_known) / nrow(.sens_set), 1), "%"), "auto",
              "Pct sensory neurons (PR + RPR) with a known body_part_sensory. Results: Sensory.")
df <- add_row(df, "sensory_uncategorised_count", sum(!.sens_known), "auto",
              "Sensory neurons (PR + RPR) with no body_part_sensory assignment. Results: Sensory.")
# NB: sensory_category_count / effector_category_count are added further down
# under "Body part categories" — do not duplicate them here.
rm(.sens_set, .sens_known)

.pr_rpr <- bm.neurons %>%
  dplyr::filter(as.logical(proofread) %in% TRUE |
                  as.logical(roughly_proofread) %in% TRUE)

motor_count <- .pr_rpr %>% dplyr::filter(super_class == "motor") %>% nrow()
df <- add_row(df, "motor_neuron_count", motor_count, "auto",
              "Motor neurons (super_class == 'motor') among PR + RPR. Results: Effectors.")

motor_ct <- .pr_rpr %>% dplyr::filter(super_class == "motor", !is.na(cell_type), cell_type != "") %>%
  dplyr::distinct(cell_type) %>% nrow()
df <- add_row(df, "motor_cell_type_count", motor_ct, "auto",
              "Motor neuron cell types among PR + RPR. Results: Effectors.")

visc_count <- .pr_rpr %>% dplyr::filter(super_class == "visceral_circulatory") %>% nrow()
df <- add_row(df, "visceral_circulatory_count", visc_count, "auto",
              "Visceral/circulatory neurons (super_class == 'visceral_circulatory') among PR + RPR. Results: Effectors.")

# effector_neuron_count uses strict super_class membership (motor +
# visceral_circulatory + ascending_visceral_circulatory) so that all three
# 'true' effector super_classes are counted. Differs from total_efferent_count
# (above) only by the sensory-flagged strand neurons that have flow ==
# 'efferent'.
an_eff_strict <- .pr_rpr %>% dplyr::filter(super_class == "ascending_visceral_circulatory") %>% nrow()
df <- add_row(df, "effector_neuron_count", motor_count + visc_count + an_eff_strict, "auto",
              "Effector neurons (super_class in {motor, visceral_circulatory, ascending_visceral_circulatory}) among PR + RPR. Excludes sensory-flagged strand neurons. Results: Effectors.")
rm(.pr_rpr, an_eff_strict)

# Effector cell types with CNS output: (a) at least one neuron of that
# cell_type has a unitary connection (count >= 10) to a downstream cell type,
# AND (b) the cell type has >= 100 total CNS output synapses.
.eff_ids <- bm %>%
  dplyr::filter(grepl("motor|visceral_circulatory", super_class)) %>%
  dplyr::pull(root_id)
.eff_ct_lookup <- bm %>%
  dplyr::filter(root_id %in% .eff_ids) %>%
  dplyr::distinct(root_id, cell_type)
.eff_el <- arrow::read_feather(.raw_edgelist_cache) %>%
  dplyr::mutate(pre = as.character(pre), post = as.character(post)) %>%
  dplyr::filter(pre %in% .eff_ids) %>%
  dplyr::left_join(.eff_ct_lookup, by = c("pre" = "root_id"))
# (a) unitary connection >= 10
.eff_unitary <- .eff_el %>%
  dplyr::left_join(bm %>% dplyr::distinct(root_id, post_ct = cell_type),
                   by = c("post" = "root_id")) %>%
  dplyr::filter(!is.na(post_ct), post_ct != "") %>%
  dplyr::group_by(pre, cell_type, post_ct) %>%
  dplyr::summarise(total_count = sum(count, na.rm = TRUE), .groups = "drop") %>%
  dplyr::filter(total_count >= 10)
.eff_unitary_ct <- unique(.eff_unitary$cell_type)
# (b) at least one member with >= 100 total CNS output synapses
.eff_neuron_output <- .eff_el %>%
  dplyr::group_by(pre, cell_type) %>%
  dplyr::summarise(neuron_cns_output = sum(count, na.rm = TRUE), .groups = "drop") %>%
  dplyr::filter(neuron_cns_output >= 100) %>%
  dplyr::distinct(cell_type) %>%
  dplyr::pull(cell_type)
.eff_cns_ct <- intersect(.eff_unitary_ct, .eff_neuron_output)
.eff_cns_ids <- bm %>%
  dplyr::filter(cell_type %in% .eff_cns_ct,
                grepl("motor|visceral_circulatory", super_class)) %>%
  dplyr::pull(root_id) %>% unique()
df <- add_row(df, "effector_outputs_within_cns",
              length(.eff_cns_ids), "auto",
              sprintf("Effector neurons whose cell type has a member with unitary CNS output >= 10 AND a member with total output >= 100 (%d cell types). Results: Effectors.",
                      length(.eff_cns_ct)))

# Pre-effector hop counts (Fig 2). Re-creates the same hop trace as
# panel_pre_effector_influence.R at the count >= 10 threshold so the
# numbers cited in the prose track the values plotted in the pyramid.
.pre_eff_target_ids <- bm %>%
  dplyr::filter(grepl("motor|visceral_circulatory", super_class)) %>%
  dplyr::pull(root_id) %>% na.omit() %>% unique()
.pre_eff_el <- arrow::read_feather(.raw_edgelist_cache) %>%
  dplyr::mutate(pre = as.character(pre), post = as.character(post)) %>%
  dplyr::filter(count >= 10)
.pre_eff_ids <- .pre_eff_el %>%
  dplyr::filter(post %in% .pre_eff_target_ids,
                !pre %in% .pre_eff_target_ids) %>%
  dplyr::pull(pre) %>% unique()
.pre_pre_eff_ids <- .pre_eff_el %>%
  dplyr::filter(post %in% .pre_eff_ids,
                !pre %in% c(.pre_eff_ids, .pre_eff_target_ids)) %>%
  dplyr::pull(pre) %>% unique()
df <- add_row(df, "pre_effector_count", length(.pre_eff_ids), "auto",
              "Neurons presynaptic to any effector with a unitary connection of count >= 10 synapses (excluding effectors themselves). Fig 2 pre-effector pyramid.")
df <- add_row(df, "pre_pre_effector_count", length(.pre_pre_eff_ids), "auto",
              "Neurons presynaptic to any pre-effector with a unitary connection of count >= 10 synapses, excluding both effectors and pre-effectors. Fig 2 pre-effector pyramid.")
rm(.pre_eff_el, .pre_eff_ids, .pre_pre_eff_ids, .pre_eff_target_ids); gc()

###########################
### KC, glia, VPN       ###
###########################

# Use unfiltered edgelist for KC output connections (count > 0, not count >= 3)
kc_ids <- bm %>%
  dplyr::filter(grepl("kenyon_cell", cell_class)) %>%
  dplyr::pull(root_id)
kc_unfiltered <- arrow::read_feather(.banc_edgelist_cache) %>%
  dplyr::mutate(pre = as.character(pre)) %>%
  dplyr::filter(pre %in% kc_ids, count > 0) %>%
  dplyr::group_by(pre) %>%
  dplyr::summarise(output_connections = sum(count, na.rm = TRUE), .groups = "drop")
kc_mean <- mean(kc_unfiltered$output_connections)
df <- add_row(df, "banc_mean_kc_output_connections", round(kc_mean, 1), "auto",
              "Mean KC output connections (unfiltered edgelist). Results: Connectivity.")

glia_count <- bm %>% dplyr::filter(super_class == "glia") %>% nrow()
df <- add_row(df, "glia_count", signif(glia_count, 2), "auto", "Glia count (2 sig figs). Fig 1 legend.")

non_neuron_count <- bm %>%
  dplyr::filter(super_class %in% c("glia", "trachea", "not_a_neuron")) %>% nrow()
df <- add_row(df, "glia_trachea_not_a_neuron_count", non_neuron_count, "auto",
              "Non-neuronal objects (glia + trachea + not_a_neuron). Fig 1 legend.")

astrocyte_count <- bm %>% dplyr::filter(super_class == "glia", cell_class == "astrocyte") %>% nrow()
df <- add_row(df, "astrocyte_count", astrocyte_count, "auto", "Astrocyte count (cell_class). Fig 1 legend.")

vpn_count <- bm %>% dplyr::filter(super_class == "visual_projection") %>% nrow()
df <- add_row(df, "visual_projection_count", vpn_count, "auto",
              "Visual projection neuron count. Results: Visual.")

###########################
### Synapse review      ###
###########################

synapse_review_file <- file.path(banc.path, "data", "synapses",
                                  "2024-09-20_aelysia_synapse_sample_complete_v2.csv")
if (file.exists(synapse_review_file)) {
  syn_review <- readr::read_csv(synapse_review_file, show_col_types = FALSE)
  df <- add_row(df, "banc_reviewed_postsynaptic_connections", nrow(syn_review), "auto",
                "Reviewed postsynaptic connections in the sparse Aelysia sample (manual validation). Methods: Synapses.")
  df <- add_row(df, "banc_reviewed_postsynaptic_neuropils",
                length(unique(syn_review$neuropil)) / 2, "auto",
                "Neuropils covered by synapse review. Methods: Synapses.")
} else {
  message("  Synapse review file not found: ", synapse_review_file)
}

# Dense-cutout synapse evaluation (16 cutouts of 2 x 2 x 0.7 um^3,
# every synapse in each cube was reviewed). Source of the per-neuropil
# precision / recall / F1 bars in panel_synapse_review.R ->
# banc_synapse_region_sample_by_neuropil.pdf.
synapse_dense_file <- file.path(banc.path, "data", "synapses",
                                "251013_synapse_evaluation.csv")
if (file.exists(synapse_dense_file)) {
  syn_dense <- readr::read_csv(synapse_dense_file, show_col_types = FALSE)
  .dense_reviewed <- sum(syn_dense$true_positives, na.rm = TRUE) +
                     sum(syn_dense$false_negatives, na.rm = TRUE)
  df <- add_row(df, "banc_reviewed_postsynaptic_connections_dense",
                .dense_reviewed, "auto",
                "Real postsynaptic connections in the dense 16-cutout evaluation (true_positives + false_negatives across all cubes; excludes detector false positives, which were rejected on review). Source of banc_synapse_region_sample_by_neuropil.pdf. Methods: Synapse detection evaluation.")
} else {
  message("  Dense synapse evaluation file not found: ", synapse_dense_file)
}

###########################
### Nerves              ###
###########################

nerve_count <- bm %>%
  dplyr::filter(!is.na(nerve), nerve != "",
                grepl("nerve", nerve), grepl("_", nerve)) %>%
  dplyr::distinct(nerve) %>% nrow()
df <- add_row(df, "nerve_count", nerve_count, "auto",
              "Number of annotated peripheral nerves. Results: Sensory.")

###########################
### Missing photoreceptors
###########################

photo_count <- franken.meta %>%
  dplyr::distinct(neuron_id, .keep_all = TRUE) %>%
  dplyr::filter(grepl("photo|retin", cell_sub_class)) %>%
  dplyr::filter(!grepl("R7|R8", cell_type)) %>% nrow()
df <- add_row(df, "fafb_photo_count", photo_count, "auto",
              "FAFB R1-6 and ocellar photoreceptors (missing from BANC). Discussion.")

###########################
### CNS network analysis
###########################

# v3 spectral clustering: min_connection_strength=2, k=14. Resolved by the
# startup helper .banc_spectral_csv for the active banc.version.
cns_csv <- .banc_spectral_csv
if (file.exists(cns_csv)) {
  cns.umap <- readr::read_csv(cns_csv, col_types = readr::cols(.default = "c")) %>%
    dplyr::select(root_id, spectral_cluster, UMAP1 = umap_x, UMAP2 = umap_y) %>%
    dplyr::left_join(bm, by = "root_id") %>%
    dplyr::mutate(cns_network = paste0("CNS_", stringr::str_pad(
      as.integer(spectral_cluster) + 1, width = 2, pad = "0"))) %>%
    # Filter matches panel_super_clusters.R cns.umap construction so that
    # cns_network_neuron_count == sum of per-network counts in fig 6.
    dplyr::filter(!is.na(cns_network),
                  !super_class %in% c("glia", "sensory", "trachea",
                                      "sensory_ascending", "motor",
                                      "visceral_circulatory", "not_a_neuron"),
                  !is.na(super_class))

  df <- add_row(df, "cns_network_neuron_count", nrow(cns.umap), "auto",
                "Neurons in CNS spectral clustering (k=14, v3). Results: CNS networks.")
  df <- add_row(df, "cns_network_cluster_count", length(unique(na.omit(cns.umap$cns_network))), "auto",
                "Number of CNS network clusters. Results: CNS networks.")

  cns_intrinsic_denom <- bm.neurons %>%
    dplyr::filter(region %in% c("central_brain", "ventral_nerve_cord"), flow == "intrinsic") %>% nrow()
  df <- add_row(df, "cns_network_neuron_pct",
                paste0(signif(100 * nrow(cns.umap) / cns_intrinsic_denom, 2), "%"),
                "auto",
                "Percent of intrinsic CB+VNC neurons in clustering, 2 sig figs. Results: CNS networks.")

  # AN/DN/other composition of the KS-test sample in Fig 6e (out-of-network
  # connectivity). Filter matches panel_an_dn_influence.R's threshold of ≥5
  # synapses; for the audit count we include every PR+RPR neuron with a
  # cns_network assignment.
  .cns_pr_rpr <- bm.neurons %>%
    dplyr::filter(root_id %in% cns.umap$root_id,
                  as.logical(proofread) %in% TRUE |
                    as.logical(roughly_proofread) %in% TRUE)
  .cns_an  <- .cns_pr_rpr %>% dplyr::filter(super_class == "ascending")  %>% nrow()
  .cns_dn  <- .cns_pr_rpr %>% dplyr::filter(super_class == "descending") %>% nrow()
  .cns_oth <- nrow(.cns_pr_rpr) - .cns_an - .cns_dn
  df <- add_row(df, "cns_network_an_count",  .cns_an,  "auto",
                "ANs in the CNS-network sample. Fig 6e.")
  df <- add_row(df, "cns_network_dn_count",  .cns_dn,  "auto",
                "DNs in the CNS-network sample. Fig 6e.")
  df <- add_row(df, "cns_network_other_count", .cns_oth, "auto",
                "Non-AN/DN neurons in the CNS-network sample. Fig 6e.")
  df <- add_row(df, "cns_network_andn_other_total", nrow(.cns_pr_rpr), "auto",
                "Total of cns_network_an_count + dn + other (Fig 6e KS-test n).")
  rm(.cns_pr_rpr, .cns_an, .cns_dn, .cns_oth)
} else {
  message("  CNS clustering CSV not found: ", cns_csv)
}

# Spectral clustering input: neurons before pruning.
# Filter MUST match the canonical pipeline at
# bancpipeline/banc/clustering/banc-spectral-clustering.R:57-96 so that the
# number reproduces the value cited in the methods text. Two stages:
#
#   (1) Quality filter: drop glia/trachea/not_a_neuron/merge/debris by
#       super_class and status, require proofread or roughly_proofread.
#   (2) clustering_set assignment:
#         - super_class %in% {visual_projection, visual_centrifugal} → "visual"
#         - region %in% {central_brain, neck_connective, ventral_nerve_cord}
#           AND super_class does NOT match sensory|motor|efferent|afferent|visceral
#       Optic-lobe intrinsic neurons are excluded by being absent from the
#       clustering_regions list (they live in `region == "optic_lobe"`).
.spectral_input <- bm %>%
  dplyr::filter(
    !grepl("glia|trachea|not_a_neuron|merge|debris", super_class, ignore.case = TRUE),
    !grepl("GLIA|TRACHEA|NOT_A_NEURON|DEBRIS|MERGE|DELETE", status),
    as.logical(proofread) %in% TRUE | as.logical(roughly_proofread) %in% TRUE
  ) %>%
  dplyr::filter(
    super_class %in% c("visual_centrifugal", "visual_projection") |
      (region %in% c("central_brain", "neck_connective", "cervical_connective", "ventral_nerve_cord") |
         grepl("ascending|descending", super_class)) &
         !grepl("sensory|motor|efferent|afferent|visceral", super_class)
  )
df <- add_row(df, "spectral_clustering_input_count", nrow(.spectral_input), "auto",
              "Neurons input to spectral clustering before iterative pruning. Filter mirrors bancpipeline/banc/clustering/banc-spectral-clustering.R:57-96 (proofread or roughly_proofread; visual_projection + visual_centrifugal plus intrinsic neurons of central_brain/neck_connective/ventral_nerve_cord; optic-lobe intrinsics excluded by region). Methods: CNS networks.")
rm(.spectral_input)

# After pruning: neurons with a cns_network assignment
.spectral_pruned <- bm %>%
  dplyr::filter(!is.na(cns_network), cns_network != "")
df <- add_row(df, "spectral_clustering_pruned_count", nrow(.spectral_pruned), "auto",
              "Neurons after iterative pruning (with cns_network). Methods: CNS networks.")
rm(.spectral_pruned)

###########################
### AN/DN function      ###
###########################

banc.an.dn.meta <- bm %>%
  dplyr::filter(super_class %in% c("ascending", "descending")) %>%
  dplyr::filter(!grepl("^SA|^SN|^AN_4|AN_5|^IN", cell_type))
neck.ct <- banc.an.dn.meta %>% dplyr::distinct(cell_type) %>% dplyr::pull(cell_type)
# Use modality from cns.functions (not the full cell_function cascade)
modality_types <- if (exists("cns.functions") && nrow(cns.functions) > 0) {
  cns.functions %>%
    dplyr::filter(!is.na(modality), modality != "") %>%
    dplyr::distinct(cell_type) %>%
    dplyr::pull(cell_type)
} else {
  character(0)
}
neck.ct.function <- intersect(neck.ct, modality_types)

df <- add_row(df, "an_dn_cell_type_count", length(neck.ct), "auto",
              "AN/DN cell types (excl SA/SN/AN_4/AN_5/IN). Results: AN/DN function.")
df <- add_row(df, "an_dn_function_count", length(neck.ct.function), "auto",
              "AN/DN cell types with known modality. Results: AN/DN function.")
df <- add_row(df, "an_dn_function_pct",
              paste0(round(100 * length(neck.ct.function) / length(neck.ct), 1), "%"), "auto",
              "Percentage of AN/DN types with known modality. Results: AN/DN function.")

###########################
### Missing types       ###
###########################

fafb.cts <- franken.meta %>%
  dplyr::filter(!grepl("ascending|descending", super_class), flow != "efferent", dataset == "FAFB") %>%
  dplyr::distinct(cell_type) %>% dplyr::pull(cell_type)
manc.cts <- franken.meta %>%
  dplyr::filter(!grepl("ascending|descending", super_class), flow != "efferent", dataset == "MANC") %>%
  dplyr::distinct(cell_type) %>% dplyr::pull(cell_type)
banc.cts <- bm %>% dplyr::filter(!grepl("ascending|descending", super_class))
banc.cts <- unique(c(banc.cts$cell_type, banc.cts$fafb_cell_type, banc.cts$manc_cell_type))

fafb_missing_n <- sum(!fafb.cts %in% banc.cts)
manc_missing_n <- sum(!manc.cts %in% banc.cts)
df <- add_row(df, "fafb_ct_count_missing", fafb_missing_n, "auto",
              "FAFB cell types not found in BANC. Discussion.")
df <- add_row(df, "fafb_ct_prop_missing", fafb_missing_n / length(fafb.cts), "auto",
              "Proportion of FAFB types missing from BANC. Discussion.")
df <- add_row(df, "manc_ct_count_missing", manc_missing_n, "auto",
              "MANC cell types not found in BANC. Discussion.")
df <- add_row(df, "manc_ct_prop_missing", manc_missing_n / length(manc.cts), "auto",
              "Proportion of MANC types missing from BANC. Discussion.")

###########################
### Body part categories
###########################

# BUGFIX 2026-07-20: both counts previously included the literal placeholder
# "unknown" as if it were a body part, inflating each by exactly one — the
# Methods therefore claimed 24 effector and 47 sensory categories where the real
# taxonomies hold 23 and 46. "unknown" is the absence of an assignment, and the
# accompanying coverage percentages already exclude it, so counting it here
# contradicted them.
if ("body_part_effector" %in% colnames(bm)) {
  eff_cats <- bm %>% dplyr::filter(!is.na(body_part_effector), body_part_effector != "",
                                   tolower(body_part_effector) != "unknown") %>%
    dplyr::distinct(body_part_effector) %>% nrow()
  df <- add_row(df, "effector_category_count", eff_cats, "auto",
                "Distinct body part effector categories, excluding the 'unknown' placeholder. Results: Effectors.")
}

if ("body_part_sensory" %in% colnames(bm)) {
  sens_cats <- bm %>% dplyr::filter(!is.na(body_part_sensory), body_part_sensory != "",
                                    tolower(body_part_sensory) != "unknown") %>%
    dplyr::distinct(body_part_sensory) %>% nrow()
  df <- add_row(df, "sensory_category_count", sens_cats, "auto",
                "Distinct body part sensory categories, excluding the 'unknown' placeholder. Results: Sensory.")
}

if ("neuropil" %in% colnames(bm)) {
  neuropil_count <- bm %>% dplyr::filter(!is.na(neuropil), neuropil != "") %>%
    dplyr::distinct(neuropil) %>% nrow()
  df <- add_row(df, "neuropil_count", neuropil_count, "auto",
                "Distinct annotated neuropils. Methods.")
}

###########################
### Flow % by region    ###
###########################

flow_by_region <- bm.neurons %>%
  dplyr::filter(!is.na(region), region != "", !is.na(flow), region != "brain") %>%
  dplyr::group_by(region) %>%
  dplyr::summarise(
    n_total = dplyr::n(),
    n_afferent = sum(grepl("afferent", flow), na.rm = TRUE),
    n_efferent = sum(grepl("efferent", flow), na.rm = TRUE),
    n_intrinsic = sum(grepl("intrinsic", flow), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(pct_afferent = n_afferent / n_total,
                pct_efferent = n_efferent / n_total,
                pct_intrinsic = n_intrinsic / n_total)

for (i in seq_len(nrow(flow_by_region))) {
  r <- flow_by_region$region[i]
  df <- add_row(df, paste0("flow_afferent_pct_", r), flow_by_region$pct_afferent[i], "auto",
                paste0("Afferent proportion in ", r, ". Supplement."))
  df <- add_row(df, paste0("flow_efferent_pct_", r), flow_by_region$pct_efferent[i], "auto",
                paste0("Efferent proportion in ", r, ". Supplement."))
  df <- add_row(df, paste0("flow_intrinsic_pct_", r), flow_by_region$pct_intrinsic[i], "auto",
                paste0("Intrinsic proportion in ", r, ". Supplement."))
}

############################################
### NEW: Connection type proportions     ###
### (from split edgelist with labels)    ###
############################################

message("Loading split edgelist for connection-type analysis...")
.split_el_cache <- file.path("data", "cache",
                              paste0(banc.gcs.dataset, "_edgelist_split.feather"))
split_el <- NULL
if (file.exists(.split_el_cache)) {
  message("  Reading from local cache: ", .split_el_cache)
  split_el <- tryCatch(arrow::read_feather(.split_el_cache), error = function(e) NULL)
}
if (is.null(split_el)) {
  message("  Downloading from GCS via gsutil...")
  gcs_split_path <- file.path(banc.gcs.bucket, banc.gcs.dataset,
                                paste0(banc.gcs.dataset, "_edgelist_split.feather"))
  dir.create("data/cache", showWarnings = FALSE, recursive = TRUE)
  dl_status <- system2("gsutil", c("cp", gcs_split_path, .split_el_cache),
                         stdout = FALSE, stderr = FALSE)
  if (dl_status == 0 && file.exists(.split_el_cache)) {
    split_el <- tryCatch(arrow::read_feather(.split_el_cache), error = function(e) NULL)
    message("  Cached to: ", .split_el_cache)
  } else {
    message("  Could not download split edgelist from GCS.")
  }
}

if (is.null(split_el)) {
  message("  Skipping connection-type analysis (no split edgelist available).")
} else {

# Filter to: axon/dendrite labels on both sides AND both neurons proofread (not roughly)
valid_labels <- c("axon", "dendrite")
proofread_ids <- bm %>% dplyr::filter(proofread == TRUE) %>% dplyr::pull(root_id)
split_el_valid <- split_el %>%
  dplyr::filter(pre_label %in% valid_labels, post_label %in% valid_labels,
                pre %in% proofread_ids, post %in% proofread_ids)

total_valid <- nrow(split_el_valid)
df <- add_row(df, "connection_type_valid_total", total_valid, "auto",
              "Total connections with valid compartment labels on both sides. Results: Connectivity.")

# Overall proportions by connection type
if (total_valid > 0) {
  conn_props <- split_el_valid %>%
    dplyr::count(connection) %>%
    dplyr::mutate(pct = n / total_valid)
  for (i in seq_len(nrow(conn_props))) {
    ct <- gsub("[.-]", "_", conn_props$connection[i])
    df <- add_row(df, paste0("connection_type_count_", ct), conn_props$n[i], "auto",
                  paste0("Connections of type ", conn_props$connection[i], " (overall). Results: Connectivity."))
    df <- add_row(df, paste0("connection_type_pct_", ct), round(conn_props$pct[i], 4), "auto",
                  paste0("Proportion of type ", conn_props$connection[i], " (overall). Results: Connectivity."))
  }
}

# By region (using post neuron's region)
split_el_region <- split_el_valid %>%
  dplyr::left_join(bm %>% dplyr::select(root_id, region) %>%
                     dplyr::distinct(root_id, .keep_all = TRUE),
                   by = c("post" = "root_id"))

region_conn <- split_el_region %>%
  dplyr::filter(!is.na(region), region != "") %>%
  dplyr::group_by(region) %>%
  dplyr::mutate(region_total = dplyr::n()) %>%
  dplyr::group_by(region, connection) %>%
  dplyr::summarise(n = dplyr::n(), pct = dplyr::n() / dplyr::first(region_total),
                   .groups = "drop")

for (i in seq_len(nrow(region_conn))) {
  r <- region_conn$region[i]
  ct <- gsub("[.-]", "_", region_conn$connection[i])
  df <- add_row(df, paste0("connection_type_pct_", ct, "_", r),
                round(region_conn$pct[i], 4), "auto",
                paste0("Proportion of ", region_conn$connection[i], " in ", r, ". Results: Connectivity."))
}
} # end if split_el not null

###########################
### Cross-dataset edges ###
###########################

# Count matched cell-type-to-cell-type connections between BANC and FAFB/MANC.
# Loads FAFB and MANC data from GCS (not SQLite).
# Mirrors panel_connectivity_comparison.R: bilateral common cell types,
# side suffixes stripped.
message("Computing cross-dataset matched edges...")
tryCatch({
  .gcs_base <- "gs://lee-lab_brain-and-nerve-cord-fly-connectome/compiled_data"

  .count_matched_gcs <- function(other_meta, other_el, label) {
    .id_candidates <- c("root_id", "id", "pt_root_id",
                        grep("^(fafb|manc|malecns).*_id$", colnames(other_meta), value = TRUE))
    .id_col <- intersect(.id_candidates, colnames(other_meta))[1]
    if (is.na(.id_col)) stop(sprintf("Cannot find ID column in %s meta", label))
    .ct_map <- other_meta %>%
      dplyr::select(id = dplyr::all_of(.id_col), cell_type) %>%
      dplyr::filter(!is.na(cell_type), cell_type != "") %>%
      dplyr::mutate(id = as.character(id)) %>%
      dplyr::distinct(id, .keep_all = TRUE)
    .side_df <- other_meta[, c(.id_col, "side")]
    colnames(.side_df)[1] <- "id"
    .side_df$id <- as.character(.side_df$id)
    .other_filtered <- .ct_map %>%
      dplyr::left_join(.side_df, by = "id") %>%
      dplyr::group_by(cell_type) %>%
      dplyr::filter(all(c("left","right") %in% side)) %>%
      dplyr::ungroup()
    .banc_filtered <- bm %>%
      dplyr::filter(!is.na(cell_type), cell_type != "") %>%
      dplyr::group_by(cell_type) %>%
      dplyr::filter(all(c("left","right") %in% side)) %>%
      dplyr::ungroup()
    .common_cts <- setdiff(na.omit(intersect(.other_filtered$cell_type,
                                              .banc_filtered$cell_type)), c("NA",""))
    .other_ct_el <- other_el %>%
      dplyr::mutate(pre = as.character(pre), post = as.character(post)) %>%
      dplyr::left_join(.ct_map %>% dplyr::rename(pre_ct = cell_type), by = c("pre" = "id")) %>%
      dplyr::left_join(.ct_map %>% dplyr::rename(post_ct = cell_type), by = c("post" = "id")) %>%
      dplyr::filter(!is.na(pre_ct), !is.na(post_ct)) %>%
      dplyr::mutate(pre_ct = gsub("_.*","",pre_ct), post_ct = gsub("_.*","",post_ct)) %>%
      dplyr::filter(pre_ct %in% .common_cts, post_ct %in% .common_cts) %>%
      dplyr::group_by(pre_ct, post_ct) %>%
      dplyr::summarise(n = sum(count, na.rm=TRUE), .groups="drop") %>%
      dplyr::mutate(connection = paste0(pre_ct,"_",post_ct))
    .banc_ct_el <- .banc_el_raw %>%
      dplyr::left_join(.bm_ct %>% dplyr::rename(pre_ct = cell_type), by = c("pre" = "root_id")) %>%
      dplyr::left_join(.bm_ct %>% dplyr::rename(post_ct = cell_type), by = c("post" = "root_id")) %>%
      dplyr::filter(!is.na(pre_ct), !is.na(post_ct)) %>%
      dplyr::mutate(pre_ct = gsub("_.*","",pre_ct), post_ct = gsub("_.*","",post_ct)) %>%
      dplyr::filter(pre_ct %in% .common_cts, post_ct %in% .common_cts) %>%
      dplyr::group_by(pre_ct, post_ct) %>%
      dplyr::summarise(n = sum(count, na.rm=TRUE), .groups="drop") %>%
      dplyr::mutate(connection = paste0(pre_ct,"_",post_ct))
    n_matched <- sum(.other_ct_el$connection %in% .banc_ct_el$connection)
    message(sprintf("  %s: %d common cell types, %d other edges, %d matched in BANC",
                    label, length(.common_cts), nrow(.other_ct_el), n_matched))
    n_matched
  }

  .cache_dir <- "data/cache"
  .banc_el_raw <- arrow::read_feather(.banc_edgelist_cache) %>%
    dplyr::mutate(pre = as.character(pre), post = as.character(post))
  .bm_ct <- bm %>% dplyr::select(root_id, cell_type) %>%
    dplyr::filter(!is.na(cell_type), cell_type != "") %>%
    dplyr::mutate(root_id = as.character(root_id)) %>%
    dplyr::distinct(root_id, .keep_all = TRUE)
  .read_cached_gcs <- function(gcs_path) {
    local_path <- file.path(.cache_dir, basename(gcs_path))
    if (!file.exists(local_path)) {
      message(sprintf("    Downloading %s...", basename(gcs_path)))
      system2("gsutil", c("cp", gcs_path, local_path), stdout = FALSE, stderr = FALSE)
    }
    arrow::read_feather(local_path)
  }

  message("  Loading FAFB data...")
  .fafb_meta <- .read_cached_gcs(file.path(.gcs_base, "fafb", "fafb_783_meta.feather"))
  .fafb_el <- .read_cached_gcs(file.path(.gcs_base, "fafb", "fafb_783_simple_edgelist.feather"))
  .fafb_matched <- .count_matched_gcs(.fafb_meta, .fafb_el, "FAFB")
  rm(.fafb_meta, .fafb_el)

  message("  Loading MANC data...")
  .manc_meta <- .read_cached_gcs(file.path(.gcs_base, "manc", "manc_121_meta.feather"))
  .manc_el <- .read_cached_gcs(file.path(.gcs_base, "manc", "manc_121_simple_edgelist.feather"))
  .manc_matched <- .count_matched_gcs(.manc_meta, .manc_el, "MANC")
  rm(.manc_meta, .manc_el)

  df <- add_row(df, "fafb_banc_matched_ct_connections", .fafb_matched, "auto",
                "Cell-type-to-cell-type connections matched between FAFB and BANC. ED Fig 1.")
  df <- add_row(df, "manc_banc_matched_ct_connections", .manc_matched, "auto",
                "Cell-type-to-cell-type connections matched between MANC and BANC. ED Fig 1.")
  rm(.fafb_matched, .manc_matched, .count_matched_gcs, .gcs_base,
     .banc_el_raw, .bm_ct)
}, error = function(e) {
  message("  Could not compute cross-dataset edges: ", e$message)
})

###########################
### NBLAST vs manual    ###
###########################

message("Computing NBLAST vs manual match disagreement...")
# For each dataset (FAFB, MANC): where both nblast_match and manual match are filled,
# count the proportion of neurons where they differ
.both_fafb <- bm %>%
  dplyr::filter(!is.na(fafb_nblast_match), fafb_nblast_match != "",
                !is.na(fafb_match), fafb_match != "")
.both_manc <- bm %>%
  dplyr::filter(!is.na(manc_nblast_match), manc_nblast_match != "",
                !is.na(manc_match), manc_match != "")
.total_pairs <- nrow(.both_fafb) + nrow(.both_manc)
.agree <- sum(.both_fafb$fafb_nblast_match == .both_fafb$fafb_match, na.rm = TRUE) +
          sum(.both_manc$manc_nblast_match == .both_manc$manc_match, na.rm = TRUE)
.differ_pct <- round(100 * (1 - .agree / .total_pairs), 0)
df <- add_row(df, "morphology_review_versus_nblast_pct",
              paste0(.differ_pct, "%"), "auto",
              "Pct neurons where manual match differs from max-NBLAST match. Methods: Cell type matching.")
rm(.both_fafb, .both_manc, .total_pairs, .agree, .differ_pct)

###########################
### Sexual dimorphism   ###
###########################

message("Computing sexual dimorphism numbers...")
# Denominator = proofread + roughly_proofread VNC neurons, matching
# banc_region_ventral_nerve_cord_count (~24,596). The sexually_dimorphic
# annotation only exists on (roughly-)proofread neurons, so numerator and
# denominator must share this population; dividing by the unfiltered VNC would
# understate the proportion.
.vnc <- bm %>% dplyr::filter(
  region == "ventral_nerve_cord",
  as.logical(proofread) %in% TRUE | as.logical(roughly_proofread) %in% TRUE)
.vnc_total <- nrow(.vnc)
if ("sexually_dimorphic" %in% colnames(bm)) {
  # Values are: "dimorphic" (present in both sexes but morphologically
  # different), "female-specific" (present only in the female), "isomorphic".
  # Report the two SEPARATELY (do not fold female-specific into dimorphic).
  .dimorphic_count    <- sum(.vnc$sexually_dimorphic == "dimorphic",       na.rm = TRUE)
  .sex_specific_count <- sum(.vnc$sexually_dimorphic == "female-specific", na.rm = TRUE)
  .dimorphic_pct      <- round(100 * .dimorphic_count / .vnc_total, 0)
  .sex_specific_pct   <- round(100 * .sex_specific_count / .vnc_total, 0)

  df <- add_row(df, "vnc_sexually_dimorphic_count", .dimorphic_count, "auto",
                "Proofread female VNC neurons flagged dimorphic (present in both sexes but different; excludes female-specific). Results: Sexual dimorphism.")
  df <- add_row(df, "vnc_sexually_dimorphic_pct", paste0(.dimorphic_pct, "%"), "auto",
                "Pct of proofread female VNC that is sexually dimorphic. Results: Sexual dimorphism.")
  df <- add_row(df, "vnc_sex_specific_count", .sex_specific_count, "auto",
                "Proofread female VNC neurons flagged female-specific (present only in the female). Results: Sexual dimorphism.")
  df <- add_row(df, "vnc_sex_specific_pct", paste0(.sex_specific_pct, "%"), "auto",
                "Pct of proofread female VNC that is sex-specific (female-specific). Results: Sexual dimorphism.")

  # Class breakdown: these counts span all VNC classes, not only intrinsic
  # interneurons, so tally each flag by super_class category (intrinsic /
  # ascending / sensory / effector / unclassified). Order matters:
  # sensory_ascending falls under 'ascending' (checked before 'sensory').
  .vnc <- .vnc %>% dplyr::mutate(dclass = dplyr::case_when(
    super_class == "ventral_nerve_cord_intrinsic"           ~ "intrinsic",
    grepl("ascending", super_class)                         ~ "ascending",
    grepl("sensory", super_class)                           ~ "sensory",
    grepl("motor|effector|endocrine|visceral", super_class) ~ "effector",
    TRUE                                                    ~ "unclassified"))
  for (.flag in c("dimorphic", "female-specific")) {
    .tag <- if (.flag == "dimorphic") "sexually_dimorphic" else "sex_specific"
    .by  <- .vnc %>% dplyr::filter(sexually_dimorphic == .flag) %>% dplyr::count(dclass)
    for (.cl in c("intrinsic", "sensory", "ascending", "effector", "unclassified")) {
      .n <- .by$n[.by$dclass == .cl]; .n <- if (length(.n)) .n else 0L
      df <- add_row(df, sprintf("vnc_%s_%s_count", .tag, .cl), .n, "auto",
                    sprintf("Proofread female VNC %s neurons of class %s (super_class grouping). Results: Sexual dimorphism.",
                            .flag, .cl))
    }
  }
  rm(.dimorphic_count, .sex_specific_count, .dimorphic_pct, .sex_specific_pct)
} else {
  message("  sexually_dimorphic column not found in banc.meta")
}
rm(.vnc, .vnc_total)

# MANC sexual dimorphism counts (from franken.meta — MANC subset is the
# male VNC reference, so dimorphic-flagged MANC neurons are the male side
# of the dimorphism; male-specific are sex-restricted).
if (exists("franken.meta") &&
    "sexually_dimorphic" %in% colnames(franken.meta) &&
    "manc_id" %in% colnames(franken.meta)) {
  # Distinct MANC neurons: franken.meta can hold >1 row per manc_id from the
  # matching, so distinct() stops duplicate rows inflating the counts.
  .manc <- franken.meta %>% dplyr::filter(!is.na(manc_id)) %>%
    dplyr::distinct(manc_id, .keep_all = TRUE)
  .manc_total <- nrow(.manc)
  .manc_male_specific <- sum(.manc$sexually_dimorphic == "male-specific", na.rm = TRUE)
  .manc_dimorphic     <- sum(.manc$sexually_dimorphic == "dimorphic",     na.rm = TRUE)
  df <- add_row(df, "manc_sex_specific_male_count", .manc_male_specific, "auto",
                "Male-specific MANC neurons (distinct manc_id, franken.meta). Results: Sexual dimorphism.")
  df <- add_row(df, "manc_sex_specific_male_pct",
                paste0(round(100 * .manc_male_specific / .manc_total, 0), "%"), "auto",
                "Pct of the male (MANC) VNC that is male-specific. Results: Sexual dimorphism.")
  df <- add_row(df, "manc_dimorphic_male_count", .manc_dimorphic, "auto",
                "Sexually dimorphic MANC (male) neurons (distinct manc_id, franken.meta). Results: Sexual dimorphism.")
  df <- add_row(df, "manc_dimorphic_male_pct",
                paste0(round(100 * .manc_dimorphic / .manc_total, 0), "%"), "auto",
                "Pct of the male (MANC) VNC that is sexually dimorphic. Results: Sexual dimorphism.")

  # Class breakdown, parallel to the female VNC block above. Note the male set
  # additionally includes descending neurons (DN axons in the male VNC), which
  # the female region==ventral_nerve_cord set excludes.
  .manc <- .manc %>% dplyr::mutate(dclass = dplyr::case_when(
    grepl("intrinsic", super_class)                         ~ "intrinsic",
    grepl("ascending", super_class)                         ~ "ascending",
    grepl("descending", super_class)                        ~ "descending",
    grepl("sensory", super_class)                           ~ "sensory",
    grepl("motor|effector|endocrine|visceral", super_class) ~ "effector",
    TRUE                                                    ~ "unclassified"))
  for (.flag in c("dimorphic", "male-specific")) {
    .tag <- if (.flag == "dimorphic") "dimorphic" else "sex_specific"
    .by  <- .manc %>% dplyr::filter(sexually_dimorphic == .flag) %>% dplyr::count(dclass)
    for (.cl in c("intrinsic", "sensory", "ascending", "descending", "effector", "unclassified")) {
      .n <- .by$n[.by$dclass == .cl]; .n <- if (length(.n)) .n else 0L
      df <- add_row(df, sprintf("manc_%s_male_%s_count", .tag, .cl), .n, "auto",
                    sprintf("Male (MANC) %s neurons of class %s (super_class grouping). Results: Sexual dimorphism.",
                            .flag, .cl))
    }
  }
  rm(.manc, .manc_total, .manc_male_specific, .manc_dimorphic)
}

###########################
### Deduplicate         ###
###########################

df <- df %>% dplyr::distinct(variable_name, .keep_all = TRUE)

###########################
### Compare with old    ###
###########################

old_csv <- "manuscript/resubmission_2/numbers.csv"
if (file.exists(old_csv)) {
  old <- readr::read_csv(old_csv, show_col_types = FALSE) %>%
    dplyr::rename(variable_name = identity, old_value = number)
  comparison <- df %>%
    dplyr::inner_join(old, by = "variable_name") %>%
    dplyr::mutate(
      value_num = suppressWarnings(as.numeric(gsub("%", "", value))),
      old_num = suppressWarnings(as.numeric(gsub("%", "", old_value)))
    ) %>%
    dplyr::filter(!is.na(value_num), !is.na(old_num)) %>%
    dplyr::mutate(
      abs_change = value_num - old_num,
      pct_change = ifelse(old_num != 0, (value_num - old_num) / old_num, NA_real_)
    ) %>%
    dplyr::filter(abs(abs_change) > 100 | abs(pct_change) > 0.05)

  if (nrow(comparison) > 0) {
    message("\n=== NUMBERS WITH LARGE CHANGES (>5% or >100 absolute) ===")
    for (i in seq_len(nrow(comparison))) {
      message(sprintf("  %s: %s -> %s (%.1f%%)",
                      comparison$variable_name[i],
                      format(comparison$old_value[i], big.mark = ","),
                      format(comparison$value[i], big.mark = ","),
                      comparison$pct_change[i] * 100))
    }
  } else {
    message("\nAll numbers within 5% / 100 of previous values.")
  }
}

###########################
### Save CSV            ###
###########################

# Convert proportions to percentages with % sign (3 sig figs); leave integers
# and existing % as-is. Variables whose name ends in `_mean_nblast` are raw
# NBLAST scores (0–1, NOT proportions of anything) and must keep their
# decimal form.
.skip_pct_convert <- grepl("_mean_nblast$", df$variable_name)
df <- df %>%
  dplyr::mutate(value = {
    v_num <- suppressWarnings(as.numeric(value))
    dplyr::case_when(
      .skip_pct_convert ~ value,
      grepl("%", value) ~ value,  # already a percentage string
      !is.na(v_num) & abs(v_num) < 1 & v_num != 0 ~ paste0(signif(v_num * 100, 3), "%"),
      TRUE ~ value
    )
  })
rm(.skip_pct_convert)

# Nature thousands-separator convention: values >= 1,000 (or <= -1,000) get
# comma-separated thousands (e.g. 7010 -> "7,010"). Skip strings that are
# already formatted as percentages, that look like scientific notation, or
# that include any non-numeric character — those are passed through as-is.
df <- df %>%
  dplyr::mutate(value = {
    v_num <- suppressWarnings(as.numeric(value))
    is_pct <- grepl("%", value)
    is_sci <- grepl("[eE][-+]?[0-9]", value)
    has_dec <- grepl("\\.", value)
    can_format <- !is.na(v_num) & !is_pct & !is_sci & abs(v_num) >= 1000
    out <- value
    if (any(can_format & !has_dec)) {
      idx <- which(can_format & !has_dec)
      out[idx] <- formatC(v_num[idx], format = "d", big.mark = ",")
    }
    if (any(can_format & has_dec)) {
      idx <- which(can_format & has_dec)
      # preserve original significant figures by stripping trailing zeros
      out[idx] <- formatC(v_num[idx], format = "f", big.mark = ",",
                          digits = 6, drop0trailing = TRUE)
    }
    out
  })

readr::write_csv(df, "manuscript/print/numbers.csv")
message(sprintf("Saved %d numbers to manuscript/print/numbers.csv", nrow(df)))

###########################
### Largest neurons CSV ###
###########################

message("Generating banc_largest_neurons.csv...")
# Find the 100 largest cell types by max volume_nm3, then export all neurons of those types
.max_vol_by_ct <- bm.neurons %>%
  dplyr::filter(!is.na(cell_type), cell_type != "", !is.na(volume_nm3)) %>%
  dplyr::group_by(cell_type) %>%
  dplyr::summarise(max_vol = max(volume_nm3, na.rm = TRUE), .groups = "drop") %>%
  dplyr::arrange(dplyr::desc(max_vol)) %>%
  dplyr::slice_head(n = 100)

.largest <- bm.neurons %>%
  dplyr::filter(cell_type %in% .max_vol_by_ct$cell_type) %>%
  dplyr::left_join(.max_vol_by_ct, by = "cell_type") %>%
  dplyr::arrange(dplyr::desc(max_vol), cell_type, dplyr::desc(volume_nm3)) %>%
  dplyr::select(root_id, dplyr::any_of(sprintf("root_%d", .version_num)), supervoxel_id, position,
                neurotransmitter_predicted, cell_type, volume_nm3) %>%
  dplyr::select(-dplyr::any_of("max_vol"))

readr::write_csv(.largest, "manuscript/print/banc_largest_neurons.csv")
message(sprintf("Saved %d neurons (%d cell types) to manuscript/print/banc_largest_neurons.csv",
                nrow(.largest), length(unique(.largest$cell_type))))
rm(.max_vol_by_ct, .largest)

###########################
### Update Google Sheet ###
###########################

message("\nUpdating Google Sheet...")
# Sheet ID is loaded from data/private/keys.csv (gitignored). If the key
# is absent we skip the Drive write — the local CSV at
# manuscript/print/numbers.csv is still saved above.
gsheet_id  <- banc.keys$gsheet_banc_variables_id
sheet_name <- "variables"

if (is.null(gsheet_id) || !nzchar(gsheet_id)) {
  message("  banc.keys$gsheet_banc_variables_id not set — skipping Google Sheet update. ",
          "(To enable, add the row to data/private/keys.csv.)")
} else tryCatch({
  googlesheets4::gs4_auth(email = "alexander.shakeel.bates@gmail.com")

  # Read existing sheet, normalise column names
  existing <- googlesheets4::read_sheet(gsheet_id, sheet = sheet_name)

  # Normalise column names to match our schema
  col_map <- c(identity = "variable_name", name = "variable_name",
               number = "value", definition = "description")
  for (old_name in names(col_map)) {
    new_name <- col_map[old_name]
    if (old_name %in% colnames(existing) && !new_name %in% colnames(existing))
      existing <- dplyr::rename(existing, !!new_name := !!old_name)
  }
  if (!"type" %in% colnames(existing)) existing$type <- "manual"
  if (!"description" %in% colnames(existing)) existing$description <- ""
  if (!"variable_name" %in% colnames(existing)) existing$variable_name <- existing[[1]]
  if (!"value" %in% colnames(existing)) existing$value <- existing[[2]]

  # Coerce both to character for safe binding
  existing <- existing %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
  df_char <- df %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

  # Keep manual rows from existing, replace auto rows with new data
  manual_rows <- existing %>%
    dplyr::filter(type == "manual") %>%
    dplyr::filter(!variable_name %in% df_char$variable_name)  # don't duplicate if we computed it

  new_sheet <- dplyr::bind_rows(df_char, manual_rows) %>%
    dplyr::distinct(variable_name, .keep_all = TRUE) %>%
    dplyr::mutate(.sort_order = dplyr::case_when(
      type == "manual" ~ 1L, type == "hardcoded" ~ 2L, TRUE ~ 3L
    )) %>%
    dplyr::arrange(.sort_order, variable_name) %>%
    dplyr::select(-.sort_order)

  googlesheets4::write_sheet(new_sheet, ss = gsheet_id, sheet = sheet_name)
  message(sprintf("Google Sheet updated: %d rows (%d auto, %d manual)",
                  nrow(new_sheet),
                  sum(new_sheet$type == "auto", na.rm = TRUE),
                  sum(new_sheet$type == "manual", na.rm = TRUE)))
}, error = function(e) {
  message("Google Sheet update failed: ", e$message)
  message("CSV saved successfully — update sheet manually if needed.")
})

message("\n### numbers.R complete ###")
