## R/text/nblast_top_match_correct.R
##
## Per-region NBLAST top-match accuracy for BANC vs FAFB-FlyWire (v783) and
## BANC vs MANC (v1.2.1). For each BANC neuron, find the top-scoring NBLAST
## match in the other dataset and classify it as "correct" if the matched
## neuron's cell_type (looked up via franken.meta) equals the BANC neuron's
## cell_type. Per (region x dataset) compute:
##   pct_correct        — % of top matches whose cell_type agrees
##   mean_score_correct — mean normalised NBLAST score among correct matches
##
## Output:
##   data/nblast/banc_888_top_match_correct.csv
##
## Consumer:
##   R/text/numbers.R at the block around L342 reads this CSV and writes
##   six add_row() entries (3 regions x 2 datasets x {pct, mean_score}).
##
## Usage:
##   Rscript R/text/nblast_top_match_correct.R
## Re-run whenever the NBLAST feathers refresh or banc.meta cell_type
## assignments shift.

setwd_here <- function() {
  # Make sure we can be invoked from anywhere — anchor to the repo root.
  if (basename(getwd()) != "BANC-project") {
    repo <- "/Users/papers/BANC-project"
    if (dir.exists(repo)) setwd(repo)
  }
}
setwd_here()

source("R/startup/banc-startup.R")
# banc.meta and franken.meta are normally loaded by numbers.R upstream of
# this script; for a standalone invocation we ensure they're available here.
if (!exists("banc.meta") || is.null(banc.meta)) {
  source("R/startup/banc-meta.R")
}
if (!exists("franken.meta") || is.null(franken.meta)) {
  franken.meta <- tryCatch({ bancr::franken_meta() }, error = function(e) {
    message("franken_meta() failed (", conditionMessage(e),
            ") — trying R/startup/franken-meta.R")
    tryCatch({
      source("R/startup/franken-meta.R")
      get("franken.meta", envir = .GlobalEnv)
    }, error = function(e2) {
      stop("Cannot load franken.meta: ", conditionMessage(e2))
    })
  })
}

library(dplyr)
library(arrow)
library(readr)

.outdir <- "data/nblast"
dir.create(.outdir, recursive = TRUE, showWarnings = FALSE)
.outfile <- file.path(.outdir, "banc_888_top_match_correct.csv")

# Helper: top NBLAST match per BANC neuron, joined with the matched
# neuron's cell_type via franken.meta and the BANC neuron's
# cell_type / region via banc.meta. Returns per-region rollup.
#
# Arguments:
#   feather_path      — local arrow feather (data/cache/...)
#   franken_id_col    — franken.meta column that holds the matched
#                       dataset's neuron id (e.g. "fafb_id", "manc_id")
#   canonical_feather — the basename numbers.R expects in the CSV
#                       (no "nblast_" prefix; e.g. "banc_fafb_783_nblast.feather")
#   ds_label          — short label for log lines
.nblast_pct_per_region <- function(feather_path, franken_id_col,
                                   canonical_feather, ds_label) {
  if (!file.exists(feather_path)) {
    warning("NBLAST feather missing: ", feather_path,
            " — skipping ", ds_label)
    return(NULL)
  }
  message("Reading NBLAST feather: ", basename(feather_path))
  nb <- arrow::read_feather(feather_path) %>%
    dplyr::filter(score > 0,
                  !is.na(match_id),
                  # match_ids with underscores are cell-type-name fallbacks;
                  # we only want the clean neuron-id matches.
                  !grepl("_", as.character(match_id)))

  # Top match per BANC neuron (highest score wins; ties broken arbitrarily).
  nb_top <- nb %>%
    dplyr::mutate(pt_root_id = as.character(pt_root_id),
                  match_id   = as.character(match_id)) %>%
    dplyr::arrange(dplyr::desc(score)) %>%
    dplyr::distinct(pt_root_id, .keep_all = TRUE)

  # Join BANC cell_type + region.
  banc_lookup <- banc.meta %>%
    dplyr::distinct(root_id, cell_type, region) %>%
    dplyr::mutate(root_id = as.character(root_id)) %>%
    dplyr::rename(banc_cell_type = cell_type)
  nb_top <- nb_top %>%
    dplyr::left_join(banc_lookup, by = c("pt_root_id" = "root_id"))

  # Join matched dataset's cell_type via franken.meta.
  if (!franken_id_col %in% colnames(franken.meta)) {
    stop("franken.meta missing column ", franken_id_col)
  }
  # 2026-05-21: The NBLAST feathers already carry the matched dataset's
  # cell type as a `match_cell_type` column — there's no need to look it
  # up from franken.meta. Earlier code built an `fm_lookup` from
  # franken.meta and joined it in, but that conflicted with the existing
  # column (silently dropped or duplicated under suffix rules depending
  # on the join). The `franken_id_col` argument is retained for
  # backwards-compatible call sites but is no longer used here.
  if (!"match_cell_type" %in% colnames(nb_top)) {
    stop(sprintf("[%s] NBLAST feather missing match_cell_type column; rebuild required",
                 ds_label))
  }
  nb_top$match_cell_type <- as.character(nb_top$match_cell_type)
  message(sprintf("[%s] feather-supplied match_cell_type: %d / %d rows non-NA",
                  ds_label,
                  sum(!is.na(nb_top$match_cell_type) & nb_top$match_cell_type != ""),
                  nrow(nb_top)))

  # Correctness: both cell types known, and they agree.
  nb_top <- nb_top %>%
    dplyr::mutate(correct =
      !is.na(banc_cell_type) & banc_cell_type != "" &
      !is.na(match_cell_type) & match_cell_type != "" &
      banc_cell_type == match_cell_type
    )

  # Per-region aggregates. Restrict to the three canonical regions that
  # numbers.R cites.
  per_region <- nb_top %>%
    dplyr::filter(!is.na(region),
                  region %in% c("central_brain", "ventral_nerve_cord", "optic_lobe")) %>%
    dplyr::group_by(region) %>%
    dplyr::summarise(
      n_top_matches      = dplyr::n(),
      n_correct          = sum(correct, na.rm = TRUE),
      pct_correct        = 100 * mean(correct, na.rm = TRUE),
      mean_score_correct = if (any(correct, na.rm = TRUE))
        mean(score[correct], na.rm = TRUE) else NA_real_,
      .groups = "drop"
    ) %>%
    dplyr::mutate(feather = canonical_feather)

  message(sprintf("  %s: %d top matches, %d regions, %d correct (%.1f%%)",
                  ds_label, sum(per_region$n_top_matches),
                  nrow(per_region), sum(per_region$n_correct),
                  100 * sum(per_region$n_correct) / sum(per_region$n_top_matches)))
  per_region
}

fafb_stats <- .nblast_pct_per_region(
  feather_path      = "data/cache/nblast_banc_fafb_783_nblast.feather",
  franken_id_col    = "fafb_id",
  canonical_feather = "banc_fafb_783_nblast.feather",
  ds_label          = "FAFB"
)

manc_stats <- .nblast_pct_per_region(
  feather_path      = "data/cache/nblast_banc_manc_v1.2.1_nblast.feather",
  franken_id_col    = "manc_id",
  canonical_feather = "banc_manc_v1.2.1_nblast.feather",
  ds_label          = "MANC"
)

out <- dplyr::bind_rows(fafb_stats, manc_stats) %>%
  dplyr::select(region, feather, n_top_matches, n_correct,
                pct_correct, mean_score_correct)
readr::write_csv(out, .outfile)
message("Wrote: ", .outfile, " (", nrow(out), " rows)")
print(out)
