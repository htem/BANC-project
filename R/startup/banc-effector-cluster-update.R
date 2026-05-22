# banc-effector-cluster-update.R
#
# Enforce "same cell_type means same super_cluster" for EFFECTOR neurons by
# broadcasting the modal `super_cluster` (sourced from the older effector
# UMAP cache) to every effector row sharing that cell_type in SeaTable.
#
# Source of truth for the modal:
#   data/banc_annotations/v888/banc_efferent_functional_classes.csv
# (per-neuron super_cluster assignments from the older effector UMAP, keyed
# on root_888.)
#
# Run by hand. NOT sourced by banc-startup.R and NOT called by any figure
# script. Replaces the effector-relevant portion of the deprecated
# banc-cluster-update.R together with banc-andn-cluster-update.R and
# banc-spectral-cluster-update.R.

suppressMessages({
  library(bancr); library(dplyr); library(readr)
})

mode_chr <- function(x) {
  x <- as.character(stats::na.omit(x))
  if (length(x) == 0L) return(NA_character_)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

identical_with_na <- function(a, b) {
  (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b)
}

# ---------------------------------------------------------------------------
# 1. Pull current SeaTable rows we need to update
# ---------------------------------------------------------------------------
banc.chosen.meta <- bancr::banctable_query(
  "SELECT _id, root_id, super_class, cell_type, super_cluster from banc_meta"
) %>%
  dplyr::mutate(root_id = as.character(root_id))

# ---------------------------------------------------------------------------
# 2. Older effector UMAP per-neuron super_cluster (root_888 keyed)
# ---------------------------------------------------------------------------
eff_umap <- readr::read_csv(
  "data/banc_annotations/v888/banc_efferent_functional_classes.csv",
  show_col_types = FALSE,
  col_types = readr::cols(.default = readr::col_character())
) %>%
  dplyr::transmute(
    root_id           = as.character(root_888),
    eff_super_cluster = super_cluster
  ) %>%
  dplyr::filter(!is.na(eff_super_cluster), eff_super_cluster != "",
                !is.na(root_id), root_id != "")

message(sprintf("Old effector UMAP CSV: %d neurons with super_cluster",
                nrow(eff_umap)))

# ---------------------------------------------------------------------------
# 3. Modal old super_cluster per current cell_type
#    Only cell_types whose membership intersects the old UMAP get a modal.
# ---------------------------------------------------------------------------
celltype_modal <- banc.chosen.meta %>%
  dplyr::inner_join(eff_umap, by = "root_id") %>%
  dplyr::filter(!is.na(cell_type), cell_type != "") %>%
  dplyr::group_by(cell_type) %>%
  dplyr::summarise(
    eff_super_cluster_mode = mode_chr(eff_super_cluster),
    n_in_old_umap          = dplyr::n(),
    n_old_sc_distinct      = dplyr::n_distinct(eff_super_cluster),
    .groups = "drop"
  )

message(sprintf("Modal map: %d effector cell_types covered", nrow(celltype_modal)))
message(sprintf("  of which %d had multiple old super_clusters (modal pick)",
                sum(celltype_modal$n_old_sc_distinct > 1)))

# ---------------------------------------------------------------------------
# 4. Build update df: only effector rows whose cell_type has a modal
# ---------------------------------------------------------------------------
banc.effector.update <- banc.chosen.meta %>%
  dplyr::inner_join(celltype_modal, by = "cell_type") %>%
  dplyr::mutate(super_cluster = eff_super_cluster_mode) %>%
  dplyr::select(`_id`, super_cluster) %>%
  base::as.data.frame()

# Diagnostic: how many rows actually change vs already match modal
.diag <- banc.chosen.meta %>%
  dplyr::inner_join(celltype_modal, by = "cell_type") %>%
  dplyr::mutate(
    will_change = !identical_with_na(super_cluster, eff_super_cluster_mode)
  ) %>%
  dplyr::summarise(
    total_rows      = dplyr::n(),
    will_change     = sum(will_change, na.rm = TRUE),
    already_correct = sum(!will_change, na.rm = TRUE),
    blank_to_filled = sum(is.na(super_cluster) | super_cluster == "", na.rm = TRUE)
  )
message("Effector super_cluster update summary:")
print(.diag)

# ---------------------------------------------------------------------------
# 5. Push to SeaTable. append_allowed = FALSE overwrites the column row-by-row.
# ---------------------------------------------------------------------------
bancr::banctable_update_rows(
  base           = 'banc_meta',
  table          = 'banc_meta',
  df             = banc.effector.update,
  append_allowed = FALSE,
  chunksize      = 1000
)

message(sprintf("Pushed %d rows to SeaTable.", nrow(banc.effector.update)))
