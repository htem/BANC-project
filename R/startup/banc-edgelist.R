#' Load BANC synaptic connectivity edgelist
#'
#' Builds `banc.edgelist.simple` (the proofread synaptic edge list used
#' by every connectivity / influence analysis in the paper). Resolution
#' is per (pre_id, post_id) at the cell-type level; `count` is the raw
#' synapse count.
#'
#' Source priority:
#'   1. Local cache feather at `data/cache/banc_NNN_edgelist_simple_v2.feather`
#'      (fastest; refreshed when GCS source changes).
#'   2. GCS feather at `gs://lee-lab.../compiled_data/banc_NNN/` (v850+).
#'   3. Legacy SQLite or live CAVE pull (v626 only).
#'
#' Global count threshold: NONE on the canonical edgelist (the v850-era
#' `count >= 3` filter was reverted on 2026-04-09). Per-call thresholds
#' (`count_thresh = 5` in influencer, `count >= 10` for hop-tracing) are
#' set explicitly at each call site.
#'
#' @section Reads:
#'   data/cache/banc_NNN_edgelist_simple_v2.feather                            (local cache, primary)
#'   gs://lee-lab.../compiled_data/banc_NNN/edgelist_simple_v2.feather         (fallback 1)
#'   data/banc-v626.sqlite                                                      (fallback 2, legacy)
#'
#' @section Writes:
#'   data/cache/banc_NNN_edgelist_simple_v2.feather                            (cache on first GCS fetch)
#'
#' @section Used by:
#'   Every figure/text script that touches connectivity (~30 R files).

# Local cache path for the raw edgelist (before filtering)
.edgelist_cache_dir <- file.path("data", "cache")
# v850+ uses different naming convention (edgelist_simple vs simple_edgelist)
# and the source flavour appends _v2 or _v3 to disambiguate from older runs.
# 2026-04-21: flipped consumer-side default from v3 to v2.
.version_num <- as.integer(strsplit(banc.gcs.dataset, "_")[[1]][2])
if (!is.na(.version_num) && .version_num >= 850) {
  .edgelist_cache_file <- file.path(.edgelist_cache_dir,
                                     paste0(banc.gcs.dataset, "_edgelist_simple_v2.feather"))
} else {
  .edgelist_cache_file <- file.path(.edgelist_cache_dir,
                                     paste0(banc.gcs.dataset, "_simple_edgelist.feather"))
}

# Load edgelist: prefer local cache, then GCS, then SQLite, then CAVE
if (file.exists(.edgelist_cache_file)) {
  # Fast path: read from local cache
  message("Loading edgelist from local cache: ", .edgelist_cache_file)
  banc.edgelist.simple <- arrow::read_feather(.edgelist_cache_file)
  message(sprintf("Loaded %d rows from cache", nrow(banc.edgelist.simple)))

} else if (exists("banc.gcs.bucket") && !is.null(banc.gcs.bucket)) {
  # GCS feather (primary remote path) — download and cache locally
  message("Loading edgelist from GCS (first time — will cache locally)...")
  gcs <- setup_gcs_filesystem()
  edgelist_path <- construct_path(banc.gcs.bucket, banc.gcs.dataset, "edgelist_simple")
  banc.edgelist.simple <- read_feather_smart(edgelist_path, gcs_filesystem = gcs)

  # Cache locally for next time
  dir.create(.edgelist_cache_dir, showWarnings = FALSE, recursive = TRUE)
  arrow::write_feather(banc.edgelist.simple, .edgelist_cache_file)
  message("Cached edgelist to: ", .edgelist_cache_file)

} else if (!is.null(banc.version)) {
  # Legacy SQLite fallback
  message("Loading edgelist from SQLite...")
  con <- DBI::dbConnect(RSQLite::SQLite(),
                        file.path(banc.dropbox.connectivity.save.path, banc.connectivity.version))
  banc.edgelist.simple <- dplyr::tbl(con, "edgelist_simple") %>%
    dplyr::collect()
  DBI::dbDisconnect(con)
} else {
  # Live CAVE
  message("Loading edgelist from CAVE...")
  banc.el <- banc_edgelist()
  banc.edgelist.simple <- banc.el %>%
    dplyr::mutate(pre = as.character(pre_pt_root_id),
                  post = as.character(post_pt_root_id)) %>%
    dplyr::rename(count = n) %>%
    dplyr::group_by(pre_pt_root_id) %>%
    dplyr::mutate(pre_count = sum(count, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(post_pt_root_id) %>%
    dplyr::mutate(post_count = sum(count, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(norm = round(count / post_count, 6)) %>%
    dplyr::filter(pre_pt_root_id != post_pt_root_id) %>%
    dplyr::arrange(dplyr::desc(count)) %>%
    dplyr::distinct()
}
rm(.edgelist_cache_dir, .edgelist_cache_file)

# Ensure pre/post are character
banc.edgelist.simple <- banc.edgelist.simple %>%
  dplyr::mutate(pre = as.character(pre),
                post = as.character(post))

# Compatibility: v850 uses post_count (was total_input in earlier versions)
if ("total_input" %in% colnames(banc.edgelist.simple) && !"post_count" %in% colnames(banc.edgelist.simple)) {
  banc.edgelist.simple$post_count <- banc.edgelist.simple$total_input
}

# NOTE (2026-04-07): the global count >= 3 filter has been removed.
# It was suppressing influence propagation in figure 3 sensor heatmaps
# (drops sensor columns with no above-floor influence anywhere). Scripts
# that need a stricter filter for connectivity plots must apply it locally.
# See CLAUDE.md "panel_an_dn_influence.R diagnosis" for context.

# Only keep connections involving proofread or roughly_proofread neurons
.proofread_ids <- banc.meta %>%
  dplyr::filter(as.logical(proofread) %in% TRUE | as.logical(roughly_proofread) %in% TRUE) %>%
  dplyr::pull(.data$id)
.n_before2 <- nrow(banc.edgelist.simple)
banc.edgelist.simple <- banc.edgelist.simple %>%
  dplyr::filter(pre %in% .proofread_ids, post %in% .proofread_ids)
message(sprintf("Edgelist after proofread filter: %d connections (dropped %d)",
                nrow(banc.edgelist.simple), .n_before2 - nrow(banc.edgelist.simple)))
rm(.proofread_ids, .n_before2)

# Add metadata to edgelist
banc.edgelist.simple <- banc.edgelist.simple %>%
  dplyr::left_join(banc.meta.post %>%
                     dplyr::select(post_id, post_neurotransmitter, post_cluster,
                                   post_side, post_region, post_super_class, post_super_cluster,
                                   post_hemilineage, post_cell_function, post_nerve,
                                   post_cell_class, post_cell_sub_class, post_cell_type, post_cell_sub_type) %>%
                     dplyr::distinct(post_id, .keep_all = TRUE),
                   by = c("post" = "post_id")) %>%
  dplyr::left_join(banc.meta.pre %>%
                     dplyr::select(pre_id, pre_neurotransmitter, pre_cluster,
                                   pre_side, pre_region, pre_super_class, pre_super_cluster,
                                   pre_hemilineage, pre_cell_function, pre_nerve,
                                   pre_cell_class, pre_cell_sub_class, pre_cell_type, pre_cell_sub_type) %>%
                     dplyr::distinct(pre_id, .keep_all = TRUE),
                   by = c("pre" = "pre_id"))

message(sprintf("Edgelist loaded: %d connections", nrow(banc.edgelist.simple)))
