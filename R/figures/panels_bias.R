######################
######################
### EFFECTORS UMAP ###
######################
######################

###############
### STARTUP ###
###############

# load
source("R/startup/banc-startup.R")
source("R/startup/franken-meta.R")
source("R/startup/banc-meta.R")

# new meta
banc.eff2.meta <- banc.eff.meta %>%
  dplyr::mutate(body_part_effector = dplyr::case_when(
    grepl("power|steering|tension",cell_function) ~ cell_function,
    grepl("pitch|yaw|roll",cell_function_detailed) ~ cell_function_detailed,
    TRUE ~ body_part_effector
  ))

# Weird ones:
weird <- c("DNxl080", "DNge079")

# Get alternative dataset for validation (seed_02)
banc.an.dn.meta <- banc.neck.meta %>%
  dplyr::filter(super_class %in% c("ascending","descending")) %>%
  dplyr::filter(!grepl("^SA|^SN|^AN_4|^AN_5",cell_type))
chosen.cts <- unique(banc.an.dn.meta$cell_type, banc.an.dn.meta$composite_cell_type)
con <- DBI::dbConnect(RSQLite::SQLite(),
                      file.path(banc.dropbox.influence.save.path,influence.sqlite))
influence.neck.eff.db <- dplyr::tbl(con, influence.table) %>%
  dplyr::filter(!is_seed,
                level %in% c("seed_07")) %>%
  dplyr::filter(
    seed %in% !!chosen.cts,
    id %in% !!banc.eff2.meta$id
  ) %>%
  dplyr::collect() %>%
  calculate_influence_norms() %>%
  dplyr::left_join(banc.meta.post %>%
                     dplyr::distinct(post_root_id, .keep_all = TRUE),
                   by = c("id"="post_root_id")) %>%
  dplyr::left_join(banc.meta.pre%>%
                     dplyr::distinct(pre_id, .keep_all = TRUE),
                   by = c("seed"="pre_cell_type"))
dbDisconnect(con)

# Cast
influence.for.m <- reshape2::acast(data = influence.neck.eff.db, 
                                   formula = id ~ seed, 
                                   value.var = "influence_norm_log",
                                   fun.aggregate = mean,
                                   fill = 0)
not_all_na1 <- rowSums(!is.na(influence.for.m)) > 0
not_all_na2 <- colSums(!is.na(influence.for.m)) > 0
influence.for.m <- influence.for.m[not_all_na1, not_all_na2]
influence.m <- influence.for.m
influence.m[is.na(influence.m)] <- 0

umap_result <- uwot::umap(
  influence.m,
  metric = "cosine",
  n_epochs = 500,
  n_neighbors = 100, 
  min_dist = 0.1,
  n_trees = 100,
  n_components = 2
)

umap_eff_df <- base::data.frame(
  UMAP1 = umap_result[,1],
  UMAP2 = umap_result[,2],
  id = base::as.character(base::rownames(influence.m))
) %>%
  dplyr::left_join(
    banc.meta,
    by = "id"
  )

dist_matrix_umap <- stats::dist(umap_eff_df[,c("UMAP1","UMAP2")], method = "euclidean")
hc_umap <- stats::hclust(dist_matrix_umap, method = "ward.D2")
dynamic_clusters_umap <- dynamicTreeCut::cutreeDynamic(
  hc_umap,
  distM = base::as.matrix(dist_matrix_umap),
  deepSplit = 4,
  minClusterSize = 2
)
umap_eff_df$unordered_cluster <- dynamic_clusters_umap

summary_df_umap <- umap_eff_df %>%
  dplyr::group_by(unordered_cluster) %>%
  dplyr::summarise(
    n = dplyr::n(),
    max_side_prop = base::max(base::prop.table(base::table(side))),
    most_common_body_part = base::names(base::sort(base::table(body_part_effector), decreasing=TRUE))[1]
  ) %>%
  dplyr::ungroup()

biased_clusters_umap <- summary_df_umap %>%
  dplyr::filter(max_side_prop > 0.8 & most_common_body_part == "front_leg") %>%
  dplyr::pull(unordered_cluster)

biased_df_umap <- umap_eff_df %>%
  dplyr::filter(unordered_cluster %in% biased_clusters_umap) %>%
  dplyr::select(id, unordered_cluster)

biased_matrix_umap <- influence.m[biased_df_umap$id, , drop = FALSE]
base::rownames(biased_matrix_umap) <- biased_df_umap$id
biased_clusters_vector_umap <- biased_df_umap$unordered_cluster

means_per_cluster_umap <- base::apply(
  biased_matrix_umap, 2,
  function(seed_col) { base::tapply(seed_col, biased_clusters_vector_umap, base::mean, na.rm = TRUE) }
)
means_per_cluster_umap <- base::t(means_per_cluster_umap)

split_range_umap <- base::apply(means_per_cluster_umap, 1, function(x) base::diff(base::range(x, na.rm = TRUE)))
split_sd_umap <- base::apply(means_per_cluster_umap, 1, stats::sd, na.rm = TRUE)

driving_seeds_umap <- base::names(base::sort(split_range_umap, decreasing = TRUE)[1:10])

seeds_table_umap <- base::data.frame(
  seed = base::colnames(means_per_cluster_umap),
  split_range = split_range_umap,
  split_sd = split_sd_umap
)
seeds_table_sorted_umap <- seeds_table_umap[base::order(-seeds_table_umap$split_range), ]

# Print move prominent
base::cat("\nTop driving seeds (UMAP clusters):\n")
base::print(driving_seeds_umap)
base::print(seeds_table_sorted_umap[1:6, ])

# ----- 1. Remove 6 most problematic seeds (already available from your previous steps) -----
problem_seeds <- rownames(seeds_table_sorted_umap[1:200,])
influence.m.red <- influence.m[, !base::colnames(influence.m) %in% problem_seeds, drop=FALSE]

# ----- 2. Run UMAP on the full and reduced matrices -----
# Original
umap_result <- uwot::umap(
  influence.m,
  metric = "euclidean",
  n_epochs = 500,
  n_neighbors = 100, 
  min_dist = 0.1,
  n_trees = 100,
  n_components = 2
)

umap_eff_df <- base::data.frame(
  UMAP1 = umap_result[,1],
  UMAP2 = umap_result[,2],
  id = base::as.character(base::rownames(influence.m))
) %>%
  dplyr::left_join(
    banc.meta,
    by = "id"
  )

# Reduced
umap_result_red <- uwot::umap(
  influence.m.red,
  metric = "euclidean",
  n_epochs = 500,
  n_neighbors = 100, 
  min_dist = 0.1,
  n_trees = 100,
  n_components = 2
)

umap_eff_df_red <- base::data.frame(
  UMAP1 = umap_result_red[,1],
  UMAP2 = umap_result_red[,2],
  id = base::as.character(base::rownames(influence.m.red))
) %>%
  dplyr::left_join(
    banc.meta,
    by = "id"
  )

# ----- 3. Utility: Filter & compute centroids -----
filter_umap_df <- function(df) {
  df %>%
    dplyr::filter(side %in% c("right", "left")) %>%
    dplyr::group_by(cell_type) %>%
    dplyr::filter(dplyr::n_distinct(side) == 2) %>% # Requires both left & right
    dplyr::ungroup()
}

by_celltype_side_centroid_dist <- function(df) {
  df %>%
    dplyr::group_by(cell_type, side) %>%
    dplyr::summarise(
      mean_UMAP1 = base::mean(UMAP1, na.rm = TRUE),
      mean_UMAP2 = base::mean(UMAP2, na.rm = TRUE),
      n = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::group_by(cell_type) %>%
    dplyr::filter(dplyr::n() == 2) %>%
    tidyr::pivot_wider(
      id_cols = cell_type,
      names_from = side,
      values_from = c(mean_UMAP1, mean_UMAP2, n)
    ) %>%
    dplyr::mutate(
      centroid_dist = base::sqrt(
        (mean_UMAP1_right - mean_UMAP1_left)^2 +
          (mean_UMAP2_right - mean_UMAP2_left)^2
      )
    )
}

# Filter and compute
umap_eff_df_use    <- filter_umap_df(umap_eff_df)
umap_eff_df_red_use <- filter_umap_df(umap_eff_df_red)

centroids_before <- by_celltype_side_centroid_dist(umap_eff_df_use)
centroids_after  <- by_celltype_side_centroid_dist(umap_eff_df_red_use)

# ----- 4. Merge for "long" format for plotting -----
centroid_diff_plot <- dplyr::inner_join(
  centroids_before %>% dplyr::select(cell_type, dist_before = centroid_dist),
  centroids_after  %>% dplyr::select(cell_type, dist_after = centroid_dist),
  by = "cell_type"
)

centroid_diff_long <- centroid_diff_plot %>%
  tidyr::pivot_longer(
    cols = c(dist_before, dist_after),
    names_to = "when",
    values_to = "centroid_dist"
  ) %>%
  dplyr::mutate(when = dplyr::recode(when, dist_before = "before", dist_after = "after"))

# ----- 5. Paired log-spaghetti plot! -----
means_sd <- centroid_diff_long %>%
  dplyr::group_by(when) %>%
  dplyr::summarise(
    mean = base::mean(centroid_dist, na.rm = TRUE),
    sd = stats::sd(centroid_dist, na.rm = TRUE)
  )

ggplot2::ggplot(centroid_diff_long, ggplot2::aes(x = when, y = centroid_dist, group = cell_type)) +
  ggplot2::geom_line(color = "grey60", alpha = 0.7) +
  ggplot2::geom_point(
    data = dplyr::filter(centroid_diff_long, when == "before"),
    color = "blue", size = 2, alpha = 0.8
  ) +
  ggplot2::geom_point(
    data = dplyr::filter(centroid_diff_long, when == "after"),
    color = "red", size = 2, alpha = 0.8
  ) +
  # Mean (black dot) and error bar for each group
  ggplot2::geom_point(
    data = means_sd, 
    ggplot2::aes(x = when, y = mean), 
    inherit.aes = FALSE,
    shape = 21, fill = "black", color = "black", size = 3
  ) +
  ggplot2::geom_errorbar(
    data = means_sd, 
    ggplot2::aes(x = when, ymin = mean - sd, ymax = mean + sd),
    inherit.aes = FALSE,
    width = 0.15, color = "black"
  ) +
  ggplot2::scale_y_log10(
    name = "Euclidean distance (right vs left centroid) [log10 scale]"
  ) +
  ggplot2::labs(
    x = "",
    title = "Separation of right and left cell types in UMAP space\n(before: blue, after: red)\nBlack=mean±SD"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(size = 12)
  )



