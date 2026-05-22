# banc-andn-cluster-update.R
#
# Push new ct_MP cluster + super_cluster assignments for ASCENDING and
# DESCENDING neurons to SeaTable.
#
# Source of truth:
#   - Per-neuron ct_MP clustering:
#       figures/figure_3/links/extra/cluster_options/cluster_assignments.csv
#     produced by panel_an_dn_umap.R (recalculate=TRUE) using the
#     celltype_partners + Marchenko-Pastur method (ct_MP). Partner cell_type
#     is keyed as cell_type+hemilineage+nerve+neuromere; partners without
#     cell_type are dropped from the matrix.
#   - ct_MP cluster -> super_cluster mapping (below) — hand-curated against
#     functional canaries + sensory/effector influence signatures.
#
# What this script does:
#   1. Pulls current SeaTable rows.
#   2. For ascending/descending neurons, replaces `cluster` with
#      <AN|DN>_<NN> from the ct_MP integer and `super_cluster` from the
#      mapping table below.
#   3. Modal-fills NA cluster/super_cluster for cells that share a cell_type
#      with assigned ones.
#   4. Pushes to SeaTable (`banctable_update_rows`).
#
# Run by hand. NOT sourced by banc-startup.R and NOT called by any figure
# script. Replaces the AN/DN portion of the deprecated banc-cluster-update.R.

suppressMessages({
  library(bancr); library(dplyr); library(tibble); library(tidyr)
  library(pheatmap)
})

mode_chr <- function(x) {
  x <- as.character(stats::na.omit(x))
  if (length(x) == 0L) return(NA_character_)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# ---------------------------------------------------------------------------
# 1. ct_MP cluster -> super_cluster mapping (Option i, 2026-05-01)
# ---------------------------------------------------------------------------
# 19 ct_MP clusters -> 17 super_clusters (visceral control merges 15+19).
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
banc.chosen.meta <- bancr::banctable_query(
  "SELECT _id, root_id, super_class, cluster, super_cluster, cell_type from banc_meta"
) %>%
  dplyr::mutate(root_id = as.character(root_id))

ct_mp <- readr::read_csv(
  "figures/figure_3/links/extra/cluster_options/cluster_assignments.csv",
  col_types = readr::cols(.default = readr::col_character())
) %>%
  dplyr::rename(root_id = id, ct_mp = cluster) %>%
  dplyr::mutate(root_id = as.character(root_id))

# Update root IDs to current materialization (handles cross-version drift)
ct_mp$root_id <- bancr::banc_updateids(ct_mp$root_id)
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

# Diagnostic: AN/DN cluster_num -> super_cluster (hand-curated mapping)
message("ct_MP cluster -> super_cluster mapping:")
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
  main = "ct_MP cluster -> old super_cluster (column-normalized)",
  fontsize_row = 10, fontsize_col = 9,
  cellwidth = 14, cellheight = 14,
  display_numbers = .hm_mat,
  number_format = "%d",
  fontsize_number = 7
)

# ---------------------------------------------------------------------------
# 4. Modal fill from cell_type — neurons sharing a cell_type with assigned
#    cells inherit the modal cluster + super_cluster. This propagates the
#    AN/DN assignments to siblings whose root_id wasn't in
#    cluster_assignments.csv (e.g. cells filtered out of the UMAP for QC).
# ---------------------------------------------------------------------------
celltype_modes <- banc.cluster.update %>%
  dplyr::filter(!is.na(cell_type)) %>%
  dplyr::group_by(cell_type) %>%
  dplyr::summarise(
    cluster_mode       = mode_chr(cluster),
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
# 5. Push to SeaTable. append_allowed = FALSE overwrites the column row-by-row.
# ---------------------------------------------------------------------------
bancr::banctable_update_rows(
  base           = 'banc_meta',
  table          = 'banc_meta',
  df             = banc.cluster.update,
  append_allowed = FALSE,
  chunksize      = 1000
)
