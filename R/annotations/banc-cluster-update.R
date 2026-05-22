#' Push AN/DN ct_MP cluster + super_cluster to SeaTable (live write)
#'
#' Repopulates the `cluster` and `super_cluster` columns in SeaTable for
#' ANs and DNs from the latest ct_MP clustering output. The mapping from
#' ct_MP integer → super_cluster name is the canonical v888 paper table;
#' edit that mapping with care — every panel script keys off the names
#' produced here.
#'
#' Source of truth (2026-05-01):
#'   - cluster_assignments_all_methods.csv produced by
#'     `R/figures/panels_an_dn_umap.R` (recalculate=TRUE) using the
#'     celltype_partners + Marchenko-Pastur method. Partner cell_type is
#'     keyed as cell_type + hemilineage + nerve + neuromere; partners
#'     without cell_type are dropped from the input matrix.
#'   - ct_MP integer → super_cluster mapping (`ct_mp_to_super`), hand-
#'     curated against functional canaries (cns.functions) + sensory /
#'     effector influence signatures.
#'
#' What this script does:
#'   1. Pulls current SeaTable rows.
#'   2. For ANs / DNs, replaces `cluster` with `<AN|DN>_<NN>` from the
#'      ct_MP integer, and `super_cluster` from the mapping table.
#'   3. Modal-fills NA `cluster` / `super_cluster` for cells that share a
#'      cell_type with assigned ones.
#'   4. Pushes to SeaTable via `banctable_update_rows` — overwrites those
#'      columns.
#'
#' Hard constraint: this is a LIVE SeaTable write. Run by hand only; NOT
#' sourced by `banc-startup.R` and NOT called from any figure script.
#' SeaTable mutations are user-controlled.
#'
#' @section Reads:
#'   data/cluster_assignments_all_methods.csv                                  (from panels_an_dn_umap.R)
#'   SeaTable (current state of cluster / super_cluster columns)
#'
#' Writes (LIVE):
#'   SeaTable: cluster, super_cluster columns of the BANC table.
#'
#' @section Used by:
#'   Downstream `banc-meta-live.R` and every figure script that reads
#'   `super_cluster` (Fig. 3d/e/f, Fig. 4, Fig. 6, ED Figs. 6–10).
#'
#' Reproduce: run interactively after a fresh cluster_assignments_all_methods.csv.
#   5. Repopulates `cns_network` from the spectral clustering CSV (unchanged
#      from previous version, kept at the bottom).

banc.version <- NULL
source("R/startup/banc-startup.R")
banc.chosen.meta <- banctable_query("SELECT _id, root_id, super_class, cluster, super_cluster, cell_type from banc_meta")

# Helper: mode for character vectors (returns one value on ties)
mode_chr <- function(x) {
  x <- as.character(stats::na.omit(x))
  if (length(x) == 0L) return(NA_character_)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# ---------------------------------------------------------------------------
# 1. ct_MP cluster → super_cluster mapping (Option i, 2026-05-01)
# ---------------------------------------------------------------------------
# 19 ct_MP clusters → 17 super_clusters (visceral control merges 15+19).
# Two new super_clusters introduced: "walking steering" + "interoceptive".
# `takeoff-landing` retained (anchored on DNp10 in ct_MP 1).
ct_mp_to_super <- c(
  "1"  = "postural control",   # was "takeoff-landing" — renamed 2026-05-02
  "2"  = "walking",
  "3"  = "reproduction",       # under review (visceral/endocrine breadth)
  "4"  = "head orienting",
  "5"  = "taste-touch",        # contains DNp01/02/04 (Giant Fiber) as a minority
  "6"  = "flight power",
  "7"  = "feeding",
  "8"  = "threat response",
  "9"  = "probing",
  "10" = "vibratory",
  "11" = "flight steering 2",
  "12" = "flight steering 1",
  "13" = "tactile",
  "14" = "walking steering",
  "15" = "visceral control",
  "16" = "proprioceptive",
  "17" = "taste-touch",        # was "interoceptive" — merged into taste-touch 2026-05-02
  "18" = "grooming",
  "19" = "visceral control"
)

# ---------------------------------------------------------------------------
# 2. Read new cluster assignments and map to AN_/DN_ format
# ---------------------------------------------------------------------------
ct_mp <- readr::read_csv(
  "figures/figure_3/links/extra/cluster_options/cluster_assignments.csv",
  col_types = readr::cols(.default = readr::col_character())
) %>%
  dplyr::rename(root_id = id, ct_mp = cluster) %>%
  dplyr::mutate(root_id = as.character(root_id))

# Update root IDs to current materialization (handles cross-version drift)
ct_mp$root_id <- banc_updateids(ct_mp$root_id)
ct_mp <- ct_mp %>% dplyr::distinct(root_id, .keep_all = TRUE)

# ---------------------------------------------------------------------------
# 3. Build update df: AN/DN cluster + super_cluster from ct_MP
# ---------------------------------------------------------------------------
banc.cluster.update <- banc.chosen.meta %>%
  dplyr::left_join(ct_mp, by = "root_id") %>%
  dplyr::mutate(
    .new_super = ct_mp_to_super[ct_mp],
    .pad       = ifelse(!is.na(ct_mp),
                        formatC(as.integer(ct_mp), width = 2, flag = "0"),
                        NA_character_),
    .prefix    = dplyr::case_when(
      super_class == "ascending"  ~ "AN",
      super_class == "descending" ~ "DN",
      TRUE                        ~ NA_character_
    ),
    .new_cluster = ifelse(!is.na(.prefix) & !is.na(.pad),
                          paste0(.prefix, "_", .pad),
                          NA_character_)
  ) %>%
  dplyr::mutate(
    cluster = dplyr::if_else(
      grepl("ascending|descending", super_class) & !is.na(.new_cluster),
      .new_cluster, NA_character_
    ),
    super_cluster = dplyr::if_else(
      grepl("ascending|descending", super_class) & !is.na(.new_super),
      .new_super, super_cluster
    )
  ) %>%
  dplyr::select(`_id`, cell_type, super_class, cluster, super_cluster) %>%
  base::as.data.frame()

# Diagnostic: AN/DN cluster_num → super_cluster (hand-curated mapping)
message("ct_MP cluster → super_cluster mapping:")
print(tibble::enframe(ct_mp_to_super, name = "ct_mp", value = "super_cluster"))

# Heatmap: new ct_MP cluster vs old super_cluster (sanity check)
.hm_data <- banc.chosen.meta %>%
  dplyr::left_join(ct_mp, by = "root_id") %>%
  dplyr::filter(grepl("descending|ascending", super_class),
                !is.na(ct_mp), ct_mp != "",
                !is.na(super_cluster), super_cluster != "") %>%
  dplyr::count(ct_mp, super_cluster) %>%
  tidyr::pivot_wider(names_from = super_cluster, values_from = n, values_fill = 0)
.hm_mat <- as.matrix(.hm_data[, -1])
rownames(.hm_mat) <- .hm_data$ct_mp
.hm_mat_norm <- sweep(.hm_mat, 2, pmax(colSums(.hm_mat), 1), "/")
pheatmap::pheatmap(
  .hm_mat_norm,
  cluster_rows = TRUE, cluster_cols = TRUE,
  clustering_method = "ward.D2",
  color = colorRampPalette(c("white", "#4a90a4", "#b22222"))(100),
  main = "ct_MP cluster → old super_cluster (column-normalized)",
  fontsize_row = 10, fontsize_col = 9,
  cellwidth = 14, cellheight = 14,
  display_numbers = .hm_mat,
  number_format = "%d",
  fontsize_number = 7
)

# ---------------------------------------------------------------------------
# 4. Modal fill from cell_type — neurons sharing a cell_type with assigned
#    cells inherit the modal cluster + super_cluster. This propagates the
#    AN/DN assignments to siblings whose root_id wasn't in cluster_assignments.csv
#    (e.g. AN/DN cells filtered out of the UMAP for QC reasons).
# ---------------------------------------------------------------------------
celltype_modes <- banc.cluster.update %>%
  dplyr::filter(!is.na(cell_type)) %>%
  dplyr::group_by(cell_type) %>%
  dplyr::summarise(
    cluster_mode = mode_chr(cluster),
    super_cluster_mode = mode_chr(super_cluster),
    .groups = "drop"
  )

banc.cluster.update <- banc.cluster.update %>%
  dplyr::left_join(celltype_modes, by = "cell_type") %>%
  dplyr::mutate(
    cluster = dplyr::if_else(
      is.na(cluster) & !is.na(cluster_mode),
      cluster_mode, cluster
    ),
    super_cluster = dplyr::if_else(
      is.na(super_cluster) & !is.na(super_cluster_mode),
      super_cluster_mode, super_cluster
    )
  ) %>%
  dplyr::select(`_id`, super_cluster, cluster) %>%
  base::as.data.frame()

message(sprintf(
  "Update: %d rows total, %d with cluster, %d with super_cluster",
  nrow(banc.cluster.update),
  sum(!is.na(banc.cluster.update$cluster)),
  sum(!is.na(banc.cluster.update$super_cluster))
))

# ---------------------------------------------------------------------------
# 5. Push to SeaTable. append_allowed = FALSE means columns are overwritten
#    row-by-row; NAs blank the cell.
# ---------------------------------------------------------------------------
banctable_update_rows(base='banc_meta',
                      table = 'banc_meta',
                      df = banc.cluster.update,
                      append_allowed = FALSE,
                      chunksize = 1000)

####################
### CNS_NETWORK  ###
####################
# Repopulate the seatable cns_network column from the latest spectral
# clustering CSV (banc/clustering/banc-spectral-clustering.R output).
#
# Logic:
#   1. The spectral CSV already renames each new spectral_cluster to its most
#      common OLD cns_network label (majority vote done inside the script),
#      so the CSV's cns_network column is the per-neuron "new" label expressed
#      in the existing label vocabulary.
#   2. For every cell_type, derive ONE new cns_network = the modal new label
#      across that cell_type's neurons. Ties are broken by taking the value
#      of the largest neuron (max l2_nodes) within the tied set.
#   3. For neurons that lack a cell_type, fall back to their own per-neuron
#      new label.
#   4. Wipe + repopulate the seatable cns_network column accordingly.
#
# Inputs (set min_strength / cluster_count / source to match the run you want):
SPECTRAL_MIN_STRENGTH <- 1L
SPECTRAL_CLUSTER_COUNT <- 13L
SPECTRAL_SOURCE <- "v2"   # "v2" or "v3"

.version_num <- as.integer(sub("^banc_", "", banc.version))
spectral_csv <- file.path(
  "data/cns_network",
  sprintf(
    "spectral_clustering_min_connection_strength_%d_banc_version_%d_cluster_count_%d_cluster_seed_10_embedding_seed_3_%s.csv",
    SPECTRAL_MIN_STRENGTH, .version_num,
    SPECTRAL_CLUSTER_COUNT, SPECTRAL_SOURCE)
)
stopifnot(file.exists(spectral_csv))
spectral <- readr::read_csv(spectral_csv,
                            col_types = readr::cols(.default = "c")) %>%
  dplyr::mutate(spectral_cluster = as.integer(spectral_cluster)) %>%
  dplyr::select(root_id, new_cns_network = cns_network, spectral_cluster)

cns.meta <- banctable_query(
  "SELECT _id, root_id, cell_type, side, neuromere, l2_nodes, cns_network from banc_meta"
) %>%
  dplyr::mutate(
    root_id = as.character(root_id),
    l2_nodes = suppressWarnings(as.numeric(l2_nodes))
  )

cns.joined <- cns.meta %>%
  dplyr::left_join(spectral, by = "root_id")

# Modal new label per (cell_type, side, neuromere) — bilateral cell_types must
# not be collapsed onto one side, and VNC types must not collapse across
# neuromeres. Ties broken by largest l2_nodes within the tie.
celltype_assign <- cns.joined %>%
  dplyr::mutate(cell_type = ifelse(is.na(cell_type)| cell_type != "",root_id,cell_type)) %>%
  dplyr::filter(!is.na(new_cns_network), new_cns_network != "") %>%
  dplyr::group_by(cell_type, side, neuromere, new_cns_network) %>%
  dplyr::summarise(
    n = dplyr::n(),
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
    !is.na(ct_new_cns_network) ~ ct_new_cns_network,        # cell_type rule
    !is.na(new_cns_network)    ~ new_cns_network,           # fallback: per-neuron
    TRUE                       ~ NA_character_              # wipe everything else
  )) %>%
  dplyr::select(`_id`, root_id, cell_type, cns_network) %>%
  base::as.data.frame()

message(sprintf(
  "cns_network update: %d rows, %d non-NA, %d distinct labels",
  nrow(cns.update),
  sum(!is.na(cns.update$cns_network)),
  dplyr::n_distinct(cns.update$cns_network[!is.na(cns.update$cns_network)])
))

# Wipe + repopulate. append_allowed = FALSE forces overwrite of every row's
# cns_network column (NAs in cns.update become blanks in the seatable, which
# is what wipes the old value where the new logic doesn't assign one).
banctable_update_rows(base = 'banc_meta',
                      table = 'banc_meta',
                      df = cns.update,
                      append_allowed = FALSE,
                      chunksize = 1000)
