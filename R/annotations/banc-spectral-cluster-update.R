#' Push spectral CNS-network labels to SeaTable (live write)
#'
#' Repopulates the `cns_network` column in SeaTable from the latest
#' spectral-clustering CSV produced by
#' `bancpipeline/banc/clustering/banc-spectral-clustering.R`.
#'
#' Logic:
#'   1. The spectral CSV already renames each new spectral_cluster to its
#'      most-common OLD `cns_network` label (majority vote performed in
#'      the upstream spectral script), so the CSV's `cns_network` column
#'      is the per-neuron new label expressed in the existing vocabulary.
#'   2. For every (cell_type, side, neuromere), derive ONE new
#'      `cns_network` = modal new label across that group's neurons.
#'      Bilateral cell_types are NOT collapsed across sides, VNC types
#'      are NOT collapsed across neuromeres. Ties broken by largest
#'      `l2_nodes` within the tied set.
#'   3. For neurons that lack a cell_type, fall back to their own
#'      per-neuron new label.
#'   4. Wipe + repopulate the SeaTable `cns_network` column accordingly.
#'
#' Hard constraint: LIVE SeaTable write. Run by hand; NOT sourced by
#' banc-startup.R and NOT called by any figure script.
#'
#' @section Reads:
#'   spectral CSV from bancpipeline/banc/clustering/                           (upstream artefact)
#'   SeaTable (current cns_network column)
#'
#' Writes (LIVE):
#'   SeaTable: cns_network column of the BANC table.
#'
#' @section Used by:
#'   Downstream `banc-meta-live.R`, panels_cns_networks.R,
#'   panels_cns_network_analyses.R, panels_cns_network_diagram.R.
# banc-cluster-update.R.

suppressMessages({
  library(bancr); library(dplyr); library(readr)
})

# ---------------------------------------------------------------------------
# Inputs (set min_strength / cluster_count / source to match the run you want)
# ---------------------------------------------------------------------------
SPECTRAL_MIN_STRENGTH  <- 1L
SPECTRAL_CLUSTER_COUNT <- 13L
SPECTRAL_SOURCE        <- "v2"        # "v2" or "v3"
BANC_VERSION_NUM       <- 888L        # data version (banc_NNN)

spectral_csv <- file.path(
  "data/cns_network",
  sprintf(
    "spectral_clustering_min_connection_strength_%d_banc_version_%d_cluster_count_%d_cluster_seed_10_embedding_seed_3_%s.csv",
    SPECTRAL_MIN_STRENGTH, BANC_VERSION_NUM,
    SPECTRAL_CLUSTER_COUNT, SPECTRAL_SOURCE)
)
stopifnot(file.exists(spectral_csv))

spectral <- readr::read_csv(spectral_csv,
                            col_types = readr::cols(.default = "c")) %>%
  dplyr::mutate(spectral_cluster = as.integer(spectral_cluster)) %>%
  dplyr::select(root_id, new_cns_network = cns_network, spectral_cluster)

# ---------------------------------------------------------------------------
# Pull SeaTable rows + join spectral CSV
# ---------------------------------------------------------------------------
cns.meta <- bancr::banctable_query(
  "SELECT _id, root_id, cell_type, side, neuromere, l2_nodes, cns_network from banc_meta"
) %>%
  dplyr::mutate(
    root_id  = as.character(root_id),
    l2_nodes = suppressWarnings(as.numeric(l2_nodes))
  )

cns.joined <- cns.meta %>%
  dplyr::left_join(spectral, by = "root_id")

# ---------------------------------------------------------------------------
# Modal new label per (cell_type, side, neuromere) — ties broken by max l2_nodes
# ---------------------------------------------------------------------------
celltype_assign <- cns.joined %>%
  dplyr::mutate(cell_type = ifelse(is.na(cell_type) | cell_type != "", root_id, cell_type)) %>%
  dplyr::filter(!is.na(new_cns_network), new_cns_network != "") %>%
  dplyr::group_by(cell_type, side, neuromere, new_cns_network) %>%
  dplyr::summarise(
    n      = dplyr::n(),
    max_l2 = suppressWarnings(max(l2_nodes, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  dplyr::mutate(max_l2 = ifelse(is.finite(max_l2), max_l2, -Inf)) %>%
  dplyr::group_by(cell_type, side, neuromere) %>%
  dplyr::arrange(dplyr::desc(n), dplyr::desc(max_l2), .by_group = TRUE) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::select(cell_type, side, neuromere, ct_new_cns_network = new_cns_network)

cns.update <- cns.joined %>%
  dplyr::left_join(celltype_assign, by = c("cell_type", "side", "neuromere")) %>%
  dplyr::mutate(cns_network = dplyr::case_when(
    !is.na(ct_new_cns_network) ~ ct_new_cns_network,    # cell_type rule
    !is.na(new_cns_network)    ~ new_cns_network,       # fallback: per-neuron
    TRUE                       ~ NA_character_          # wipe everything else
  )) %>%
  dplyr::select(`_id`, root_id, cell_type, cns_network) %>%
  base::as.data.frame()

message(sprintf(
  "cns_network update: %d rows, %d non-NA, %d distinct labels",
  nrow(cns.update),
  sum(!is.na(cns.update$cns_network)),
  dplyr::n_distinct(cns.update$cns_network[!is.na(cns.update$cns_network)])
))

# ---------------------------------------------------------------------------
# Wipe + repopulate. append_allowed = FALSE forces overwrite of every row's
# cns_network column (NAs in cns.update blank the seatable cell).
# ---------------------------------------------------------------------------
bancr::banctable_update_rows(
  base           = 'banc_meta',
  table          = 'banc_meta',
  df             = cns.update,
  append_allowed = FALSE,
  chunksize      = 1000
)
