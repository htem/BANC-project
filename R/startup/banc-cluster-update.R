# Update BANC table!
banc.version <- NULL
source("R/startup/banc-startup.R")

# Helper: mode for character vectors (returns one value on ties)
mode_chr <- function(x) {
  x <- as.character(stats::na.omit(x))
  if (length(x) == 0L) return(NA_character_)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# UMAP clusters by neuron
umap.sez.df.n <- NULL # placeholder
umap.an.df.n <- NULL # placeholder
umap.eff.df.n  <- read_csv(file = "data/banc_efferent_functional_classes.csv", col_types = banc.col.types) %>%
  dplyr::mutate(clusterno = gsub(".*_","",cluster))
umap.dn.df.n <- read_csv(file = "data/banc_neck_functional_classes.csv", col_types = banc.col.types)

# Update root IDs
umap.dn.df.n$root_id <- banc_updateids(umap.dn.df.n$id)
umap.eff.df.n$root_id <- banc_updateids(umap.eff.df.n$id)

# Update cluster numbers
banc.chosen.meta <- banctable_query("SELECT _id, root_id, supervoxel_id, position, super_class, cluster, super_cluster, cell_type from banc_meta")
banc.cluster.update <- banc.chosen.meta %>%
  dplyr::filter(!super_class %in% c("not_a_neuron","glia","debris")) %>%
  left_join(umap.dn.df.n %>%
              dplyr::select(root_id, dn_cluster = cluster) %>%
              dplyr::distinct(root_id, .keep_all = TRUE),
            by = c('root_id'))  %>%
  # left_join(umap.an.df.n %>%
  #             dplyr::select(root_id = id, an_cluster = cluster) %>%
  #             dplyr::distinct(root_id, .keep_all = TRUE),
  #           by = c('root_id'))  %>%
  left_join(umap.eff.df.n %>%
              dplyr::select(root_id, eff_cluster = cluster) %>%
              dplyr::distinct(root_id, .keep_all = TRUE),
            by = c('root_id'))  %>%
  # left_join(umap.sez.df %>%
  #             dplyr::select(root_id, sez_cluster = cluster) %>%
  #             dplyr::distinct(root_id, .keep_all = TRUE),
  #           by = c('root_id'))  %>%
  dplyr::mutate(cluster = dplyr::case_when(
    !is.na(dn_cluster)&grepl("descending|ascending",super_class) ~ dn_cluster,
    !is.na(eff_cluster)&grepl("efferent|motor|endocrine|visceral",super_class) ~ eff_cluster,
    #!is.na(sez_cluster) ~ sez_cluster,
    TRUE ~ NA
  )) %>%
  dplyr::select(`_id`, cell_type, super_cluster, cluster) %>%
  as.data.frame()
# If a row lacks a cluster or super_cluster, but has a cell_type:
# based on other rows with the same cell_type.
# If multiple possibilities exist, choose the modal answer.
# If the mode is split, choose one of them.

# Precompute per-cell_type modes from existing (non-NA) labels
celltype_modes <- banc.cluster.update %>%
  dplyr::filter(!is.na(cell_type)) %>%
  dplyr::group_by(cell_type) %>%
  dplyr::summarise(
    cluster_mode = mode_chr(cluster),
    super_cluster_mode = mode_chr(super_cluster),
    .groups = "drop"
  )

# Fill NAs where cell_type is known, using the modes
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

# Run update
banctable_update_rows(base='banc_meta', 
                      table = 'banc_meta', 
                      df = banc.cluster.update, 
                      append_allowed = FALSE, 
                      chunksize = 1000)

###############
### FRANKEN ###
###############
franken.cluster.meta <- franken_meta("SELECT _id, cell_type, super_class from franken_meta")
franken.cluster.update <- left_join(franken.cluster.meta %>%
                              dplyr::select(`_id`,cell_type, super_class),
                            umap.dn.df.n %>%
                              dplyr::select(cell_type, dn_cluster = cluster) %>%
                              dplyr::distinct(cell_type, .keep_all = TRUE),
                            by = c('cell_type')) %>%
  # left_join(umap.an.df %>%
  #             dplyr::select(cell_type, an_cluster = cluster) %>%
  #             dplyr::distinct(cell_type, an_cluster),
  #           by = c('cell_type'))  %>%
  left_join(umap.eff.df %>%
              dplyr::select(cell_type, eff_cluster = cluster) %>%
              dplyr::distinct(cell_type, .keep_all = TRUE),
            by = c('cell_type'))  %>%
  # left_join(umap.sez.df %>%
  #             dplyr::select(cell_type, sez_cluster = cluster) %>%
  #             dplyr::distinct(cell_type, .keep_all = TRUE),
  #           by = c('cell_type'))  %>%
  dplyr::mutate(cluster = dplyr::case_when(
    !is.na(dn_cluster)&grepl("descending|ascending",super_class) ~ dn_cluster,
    !is.na(eff_cluster)&grepl("efferent|motor|endocrine|visceral",super_class) ~ eff_cluster,
    # !is.na(sez_cluster) ~ sez_cluster,
    TRUE ~ NA
  )) %>%
  #dplyr::select(`_id`, cluster) %>%
  dplyr::distinct(`_id`, .keep_all = TRUE)
  # dplyr::select(-an_cluster,-dn_cluster, -eff_cluster, -sez_cluster)

# Update!
banctable_update_rows(base='cns_meta', 
                      table = 'franken_meta', 
                      df = franken.cluster.update, 
                      append_allowed = FALSE, 
                      chunksize = 1000)


