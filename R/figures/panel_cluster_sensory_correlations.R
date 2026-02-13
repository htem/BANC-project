######################################################
### PLOT UMAPS BASED ON COSINE DIRECT CONNECTIVITY ###
######################################################

###############
### STARTUP ###
###############

# load
source("R/startup/banc-startup.R")
source("R/startup/franken-meta.R")
source("R/startup/banc-meta.R")
source("R/startup/banc-functions.R")
source("R/startup/banc-edgelist.R")

# new meta
banc.an.dn.meta <- banc.meta %>%
  dplyr::filter(super_class %in% c("ascending","descending")) %>%
  dplyr::filter(!grepl("^SA|^SN|^AN_4|AN_5|^IN",cell_type))
banc.targets <- banc.meta %>%
  dplyr::filter(grepl("mushroom_body_output|mushroom_body_dopaminergic_neuron|central_complex_input",cell_class)|
                  grepl("visual_centrifugal",super_class)) %>%
  rbind(banc.eff.meta)
banc.sources <- banc.meta %>%
  dplyr::filter(grepl("mushroom_body_output|central_complex_output",cell_class)|
                  grepl("visual_projection",super_class))

#################
### DIVERSITY ###
#################
chosen.seeds   <- stats::na.omit(unique(banc.sens.meta$seed_02))
chosen.targets <- stats::na.omit(unique(banc.an.dn.meta$id))

# Set up for influence calculation
banc.meta$root_id <- banc.meta$root_626
ic_banc <- influence_calculator_py(edgelist_simple = banc.edgelist.simple %>%
                                     dplyr::filter(count > 0),
                                   meta = banc.meta)

# Get sensory cell types (seeds) and AN/DN neurons (targets)
banc.sens.csc <- banc.sens.meta %>%
  dplyr::distinct(seed_02) %>%
  dplyr::pull(seed_02)
cts <- na.omit(banc.sens.csc)

# Initialize result dataframe
influence.sensors.db <- data.frame()

# Progress tracking
n_cts <- length(cts)
cat(sprintf("Processing %d sensory cell types...\n", n_cts))

# Calculate influence for each sensory cell type
for(i in seq_along(cts)){
  ct <- cts[i]

  if (i %% 10 == 0 || i == n_cts) {
    cat(sprintf("  Progress: %d / %d (%.1f%%)\n", i, n_cts, 100 * i / n_cts))
  }

  # Get neurons of this cell type
  banc.ct.meta <- subset(banc.meta, seed_02 == ct)
  banc.ct.ids <- unique(na.omit(banc.ct.meta$root_id))

  if (length(banc.ct.ids) == 0) next

  # Calculate influence to all neurons, then filter to AN/DN
  try({
    control_influence.id <- calculate_influence_py(ic_banc, banc.ct.ids) %>%
      dplyr::filter(id %in% banc.an.dn.meta$id) %>%  # Filter to AN/DN targets
      dplyr::mutate(
        seed = ct,
        level = "seed_02",
        influence_original = `Influence_score_(unsigned)`
      ) %>%
      dplyr::select(seed, level, id, influence_original)

    influence.sensors.db <- rbind(influence.sensors.db, control_influence.id)
  })
}

# Use the calculated influence
influence.sens.to.dn.df <- influence.sensors.db

# Make AN/DN the 'seed' for this flow (rows = AN/DN; columns = sensory sub-classes)
influence.sens.df <- influence.sens.to.dn.df %>%
  dplyr::rename(sens_id = seed, andn_id = id) %>%
  # attach AN/DN metadata (these will be our seed_* fields)
  dplyr::left_join(
    banc.meta %>%
      dplyr::distinct(root_id,
                      seed_cell_type     = cell_type,
                      seed_cluster       = cluster,
                      seed_super_cluster = super_cluster),
    by = c("andn_id" = "root_id")
  ) %>%
  # attach sensory metadata for feature labels (target_cell_type)
  dplyr::left_join(
    banc.sens.meta %>%
      dplyr::distinct(seed_02,
                      target_cell_type = cell_sub_class),
    by = c("sens_id" = "seed_02")
  ) %>%
  # harmonize with the AN/DN→effector pipeline: expose 'seed' = AN/DN id
  dplyr::transmute(
    seed = andn_id,
    seed_cell_type,
    seed_super_cluster,
    target_cell_type,
    influence_original
  ) %>%
  dplyr::filter(!is.na(target_cell_type),
                !is.na(seed_super_cluster),
                !is.na(seed_cell_type))

# Go wide: rows = AN/DN (seed), cols = sensory sub-classes (target_cell_type)
infl_wide_sens <- influence.sens.df %>%
  dplyr::group_by(seed, seed_cell_type, seed_super_cluster, target_cell_type) %>%
  dplyr::summarise(val = sum(influence_original, na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(
    names_from  = target_cell_type,
    values_from = val,
    values_fill = 0
  )

seed_meta_sens <- infl_wide_sens %>%
  dplyr::select(seed, seed_cell_type, seed_super_cluster)
X_sens <- infl_wide_sens %>%
  dplyr::select(-seed, -seed_cell_type, -seed_super_cluster) %>%
  as.matrix()
stopifnot(nrow(X_sens) == nrow(seed_meta_sens))
rownames(X_sens) <- seed_meta_sens$seed

# cosine
row_norm_s <- sqrt(rowSums(X_sens * X_sens))
row_norm_s[row_norm_s == 0] <- .Machine$double.eps
U_sens <- X_sens / row_norm_s
S_sens <- U_sens %*% t(U_sens)
rownames(S_sens) <- seed_meta_sens$seed
colnames(S_sens) <- seed_meta_sens$seed

# unique pairs
idx_s <- which(upper.tri(S_sens), arr.ind = TRUE)
pairs_df_sens <- tibble::tibble(
  seed_i  = rownames(S_sens)[idx_s[,1]],
  seed_j  = colnames(S_sens)[idx_s[,2]],
  cos_sim = S_sens[idx_s]
)
seed_meta_i_s <- seed_meta_sens %>% dplyr::rename(cell_i = seed_cell_type, sc_i = seed_super_cluster)
seed_meta_j_s <- seed_meta_sens %>% dplyr::rename(cell_j = seed_cell_type, sc_j = seed_super_cluster)
pairs_df_sens <- pairs_df_sens %>%
  dplyr::left_join(seed_meta_i_s, by = c("seed_i" = "seed")) %>%
  dplyr::left_join(seed_meta_j_s, by = c("seed_j" = "seed")) %>%
  dplyr::mutate(
    category = dplyr::case_when(
      cell_i == cell_j ~ "same cell type",
      sc_i   == sc_j   ~ "same supercluster",
      TRUE             ~ "different superclusters"
    ),
    category = factor(category,
                      levels = c("same cell type","same supercluster","different superclusters")),
    group = "sensory → AN/DN"
  )

# Direct influence calculation: AN/DN → effectors
cat("\nCalculating influence from AN/DN neurons to effector neurons...\n")

# Get AN/DN neurons (seeds) and effector neurons (targets)
chosen.seeds <- na.omit(unique(banc.an.dn.meta$seed_12))
chosen.targets <- na.omit(unique(c(banc.eff.meta$id)))

# Get unique seed_12 values
seed_12_values <- unique(banc.meta$seed_12)
seed_12_values <- seed_12_values[seed_12_values %in% chosen.seeds]
seed_12_values <- na.omit(seed_12_values)

# Initialize result dataframe
influence.dn.to.eff.db <- data.frame()

# Progress tracking
n_seeds <- length(seed_12_values)
cat(sprintf("Processing %d AN/DN seed classes...\n", n_seeds))

# Calculate influence for each AN/DN seed class
for(i in seq_along(seed_12_values)){
  seed_class <- seed_12_values[i]

  if (i %% 5 == 0 || i == n_seeds) {
    cat(sprintf("  Progress: %d / %d (%.1f%%)\n", i, n_seeds, 100 * i / n_seeds))
  }

  # Get neurons of this seed class
  seed_neurons <- banc.meta %>%
    dplyr::filter(seed_12 == seed_class) %>%
    dplyr::pull(root_id) %>%
    unique() %>%
    na.omit()

  if (length(seed_neurons) == 0) next

  # Calculate influence to effector neurons
  try({
    seed_influence <- calculate_influence_py(ic_banc, seed_neurons) %>%
      dplyr::filter(id %in% chosen.targets) %>%
      dplyr::mutate(
        seed = seed_class,
        level = "seed_12",
        influence_original = `Influence_score_(unsigned)`
      ) %>%
      dplyr::select(seed, level, id, influence_original)

    influence.dn.to.eff.db <- rbind(influence.dn.to.eff.db, seed_influence)
  })
}

# Use the calculated influence
influence.dn.to.eff.df <- influence.dn.to.eff.db

# Format
influence.dn.df <- influence.dn.to.eff.df %>%
  dplyr::mutate(seed = gsub(".*_","",seed)) %>%
  dplyr::left_join(banc.meta %>%
                     dplyr::distinct(root_id, target_cell_type = cell_type, target_cluster = cluster, target_super_cluster = super_cluster),
                   by = c("id"="root_id")) %>%
  dplyr::left_join(banc.meta %>%
                     dplyr::distinct(root_id, seed_cell_type = cell_type, seed_cluster = cluster, seed_super_cluster = super_cluster),
                   by = c("seed"="root_id")) %>%
  dplyr::filter(!is.na(target_cell_type), !is.na(seed_super_cluster), !is.na(seed_cell_type))

# Go wide
infl_wide <- influence.dn.df |>
  dplyr::transmute(
    seed,
    seed_cell_type,
    seed_super_cluster,
    target_cell_type,
    val = influence_original
  ) |>
  dplyr::group_by(seed, seed_cell_type, seed_super_cluster, target_cell_type) |>
  dplyr::summarise(val = sum(val, na.rm = TRUE), .groups = "drop") |>
  tidyr::pivot_wider(
    names_from  = target_cell_type,
    values_from = val,
    values_fill = 0
  )

## seeds + matrix (ensure names line up)
seed_meta <- infl_wide |>
  dplyr::select(seed, seed_cell_type, seed_super_cluster)
X <- infl_wide |>
  dplyr::select(-seed, -seed_cell_type, -seed_super_cluster) |>
  as.matrix()
stopifnot(nrow(X) == nrow(seed_meta))
rownames(X) <- seed_meta$seed

## cosine
row_norm <- sqrt(rowSums(X * X))
row_norm[row_norm == 0] <- .Machine$double.eps
U <- X / row_norm
S <- U %*% t(U)
rownames(S) <- seed_meta$seed
colnames(S) <- seed_meta$seed

## unique upper-tri pairs
idx <- which(upper.tri(S), arr.ind = TRUE)
pairs_df <- tibble::tibble(
  seed_i  = rownames(S)[idx[,1]],
  seed_j  = colnames(S)[idx[,2]],
  cos_sim = S[idx]
)

## explicit joins (no renaming inside by=)
seed_meta_i <- seed_meta |>
  dplyr::rename(cell_i = seed_cell_type, sc_i = seed_super_cluster)
seed_meta_j <- seed_meta |>
  dplyr::rename(cell_j = seed_cell_type, sc_j = seed_super_cluster)
pairs_df_eff <- pairs_df |>
  dplyr::left_join(seed_meta_i, by = c("seed_i" = "seed")) |>
  dplyr::left_join(seed_meta_j, by = c("seed_j" = "seed")) |>
  dplyr::mutate(
    category = dplyr::case_when(
      cell_i == cell_j ~ "same cell type",
      sc_i   == sc_j   ~ "same supercluster",
      TRUE             ~ "different superclusters"
    ),
    category = factor(category,
                      levels = c("same cell type","same supercluster","different superclusters"))
  ) %>%
  dplyr::mutate(group = "AN/DN → effector")
pairs_both <- dplyr::bind_rows(pairs_df_eff, pairs_df_sens)
p_violin_both <- ggplot2::ggplot(
  pairs_both,
  ggplot2::aes(x = category, y = cos_sim, fill = group)) +
  ggplot2::geom_violin(trim = TRUE, alpha = 0.9, colour = NA) +
  ggplot2::geom_boxplot(width = 0.12, outlier.shape = NA, fill = "white", colour = "grey10") +
  ggplot2::labs(x = NULL, y = "cosine similarity\n(seed influence over target cell types)") +
  ggplot2::coord_cartesian(ylim = c(NA, 1.05)) +
  ggplot2::facet_grid(~ group) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1, vjust = 1)) +
  ggplot2::scale_fill_manual(values = c(
    `AN/DN → effector` = paper.cols[["motor"]],
    `sensory → AN/DN` = paper.cols[["sensory"]]
  ), guide = "none")

print(p_violin_both)
ggplot2::ggsave(file.path(banc.fig4.path, "seed_influence_cosine_violin_sens_vs_eff.pdf"),
                p_violin_both, width = 9, height = 3, dpi = 300)

# write stats
write_diversity_nonparam_summary(
  df           = pairs_both,
  group_col    = "group",
  category_col = "category",
  value_col    = "cos_sim",
  plot_path    = file.path(banc.fig4.path, "seed_influence_cosine_violin_sens_vs_eff.pdf"),
  adjust_method = "holm",
  alpha         = 0.05
)

#######################################
### FACETED BOXPLOT BY SUPERCLUSTER ###
#######################################

# -----------------------------
# Prepare data with supercluster and group
# -----------------------------
pairs_both_sc <- pairs_both %>%
  dplyr::mutate(super_cluster = sc_i)

# Order superclusters by median similarity for "same supercluster"
sc_order_by_median <- pairs_both_sc %>%
  dplyr::filter(category == "same supercluster") %>%
  dplyr::group_by(super_cluster) %>%
  dplyr::summarise(median_same = median(cos_sim, na.rm = TRUE), .groups = "drop") %>%
  dplyr::arrange(desc(median_same)) %>%
  dplyr::pull(super_cluster)

pairs_both_sc <- pairs_both_sc %>%
  dplyr::mutate(super_cluster = factor(super_cluster, levels = sc_order_by_median))

# Color mapping per super_cluster
super_cluster_colors <- setNames(paper.cols[sc_order_by_median], sc_order_by_median)

# Just to be explicit about x order
pairs_both_sc <- pairs_both_sc %>%
  dplyr::mutate(category = factor(
    category,
    levels = c("same cell type", "same supercluster", "different superclusters")
  ))

# ----------------------------------------
# Helper: significance label for p-values
# ----------------------------------------
sig_label_func <- function(p) {
  if (p < 0.001) "***"
  else if (p < 0.01) "**"
  else if (p < 0.05) "*"
  else "ns"
}

# ---------------------------------------------------
# Build per-(group, supercluster) statistics + bracket geometry
# ---------------------------------------------------
build_stat_rows <- function(sc, grp, sc_data) {
  same_sc <- sc_data %>%
    dplyr::filter(category == "same supercluster") %>%
    dplyr::pull(cos_sim)

  same_ct <- sc_data %>%
    dplyr::filter(category == "same cell type") %>%
    dplyr::pull(cos_sim)

  diff_sc <- sc_data %>%
    dplyr::filter(category == "different superclusters") %>%
    dplyr::pull(cos_sim)

  test1 <- tryCatch(wilcox.test(same_sc, same_ct), error = function(e) list(p.value = NA))
  test2 <- tryCatch(wilcox.test(same_sc, diff_sc), error = function(e) list(p.value = NA))

  y_max   <- max(sc_data$cos_sim, na.rm = TRUE)
  y_min   <- min(sc_data$cos_sim, na.rm = TRUE)
  y_range <- y_max - y_min

  data.frame(
    super_cluster = sc,
    group         = grp,
    comparison    = c("same_sc_vs_same_ct", "same_sc_vs_diff_sc"),
    x_start       = c(1, 2),
    x_end         = c(2, 3),
    y             = c(y_max + 0.08 * y_range,
                      y_max + 0.15 * y_range),
    label         = c(sig_label_func(test1$p.value),
                      sig_label_func(test2$p.value)),
    stringsAsFactors = FALSE
  )
}

# Build stats for every (group, supercluster) combination
group_levels <- unique(pairs_both_sc$group)
stat_results <- do.call(
  rbind,
  lapply(group_levels, function(grp) {
    do.call(rbind, lapply(sc_order_by_median, function(sc) {
      sc_data <- dplyr::filter(pairs_both_sc, super_cluster == sc, group == grp)
      if (nrow(sc_data) == 0) return(NULL)
      build_stat_rows(sc, grp, sc_data)
    }))
  })
)

stat_results$super_cluster <- factor(stat_results$super_cluster,
                                     levels = sc_order_by_median)

# Fix long name
pairs_both_sc <- pairs_both_sc %>%
  dplyr::mutate(super_cluster = dplyr::case_when(
    super_cluster=="head and eye orienting" ~ "head and eye",
    TRUE ~ as.character(super_cluster)
  ))
stat_results <- stat_results %>%
  dplyr::mutate(super_cluster = dplyr::case_when(
    super_cluster=="head and eye orienting" ~ "head and eye",
    TRUE ~ as.character(super_cluster)
  ))

# Update factor levels after name fix
sc_levels_fixed <- gsub("head and eye orienting", "head and eye", sc_order_by_median)
pairs_both_sc$super_cluster <- factor(pairs_both_sc$super_cluster, levels = sc_levels_fixed)
stat_results$super_cluster <- factor(stat_results$super_cluster, levels = sc_levels_fixed)

# ------------------------
# Plot with facet_grid(group ~ super_cluster)
# ------------------------
p_violin_faceted <- ggplot(
  pairs_both_sc,
  aes(x = category, y = cos_sim, fill = group)) +
  geom_boxplot(outlier.size = 0, alpha = 0.7, width = 0.2) +
  scale_fill_manual(values = c(
    `AN/DN → effector` = paper.cols[["motor"]],
    `sensory → AN/DN` = paper.cols[["sensory"]]
  ), guide = "none") +

  # Horizontal bracket lines
  geom_segment(
    data = stat_results,
    aes(x = x_start, xend = x_end, y = y, yend = y, group = comparison),
    inherit.aes = FALSE
  ) +
  # Small vertical ticks at each end of the bracket
  geom_segment(
    data = stat_results,
    aes(x = x_start, xend = x_start,
        y = y, yend = y - 0.01 * (max(pairs_both_sc$cos_sim, na.rm = TRUE))),
    inherit.aes = FALSE
  ) +
  geom_segment(
    data = stat_results,
    aes(x = x_end, xend = x_end,
        y = y, yend = y - 0.01 * (max(pairs_both_sc$cos_sim, na.rm = TRUE))),
    inherit.aes = FALSE
  ) +
  # Significance labels centered above brackets
  geom_text(
    data = stat_results,
    aes(x = (x_start + x_end) / 2,
        y = y + 0.02 * (max(pairs_both_sc$cos_sim, na.rm = TRUE)),
        label = label),
    size = 2.5,
    inherit.aes = FALSE
  ) +

  facet_grid(group ~ super_cluster) +
  labs(
    x = NULL,
    y = "cosine similarity\n(seed influence over target cell types)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x   = element_text(angle = 90, hjust = 1, vjust = 1, size = 7),
    strip.text    = element_text(size = 8),
    strip.text.y  = element_blank(),
    legend.position = "none"
  )

# Show
print(p_violin_faceted)

# Save (doubled width)
ggplot2::ggsave(
  file.path(banc.fig4.supp.path, "seed_influence_cosine_violin_by_supercluster.pdf"),
  p_violin_faceted,
  width = 14,
  height = 4,
  dpi = 300
)

# Write stats per facet cell
write_diversity_nonparam_summary(
  df           = pairs_both_sc,
  group_col    = "group",
  category_col = "category",
  value_col    = "cos_sim",
  plot_path    = file.path(banc.fig4.supp.path, "seed_influence_cosine_violin_by_supercluster.pdf"),
  adjust_method = "holm",
  alpha         = 0.05
)

#############################################################
### EXTENDED mk_sc_div TO INCLUDE BETWEEN-SC COMPARISONS ###
#############################################################

mk_sc_div <- function(pairs_df_one) {
    pairs_df_one %>%
      dplyr::group_by(sc_ref = sc_i) %>%
      dplyr::summarise(
        med_within_type       = stats::median(cos_sim[(sc_i == sc_j) & (cell_i == cell_j)], na.rm = TRUE),
        n_within_type         = sum((sc_i == sc_j) & (cell_i == cell_j), na.rm = TRUE),
        med_cross_type_within = stats::median(cos_sim[(sc_i == sc_j) & (cell_i != cell_j)], na.rm = TRUE),
        n_cross_type_within   = sum((sc_i == sc_j) & (cell_i != cell_j), na.rm = TRUE),
        med_cross_type_between = stats::median(cos_sim[sc_i != sc_j], na.rm = TRUE),
        n_cross_type_between  = sum(sc_i != sc_j, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        delta = med_within_type - med_cross_type_within,
        delta_between = med_cross_type_within - med_cross_type_between
      )
  }

sc_div_eff  <- mk_sc_div(pairs_df_eff)  %>% dplyr::mutate(group = "AN/DN → effector")
sc_div_sens <- mk_sc_div(pairs_df_sens) %>% dplyr::mutate(group = "sensory → AN/DN")

sc_delta_both <- dplyr::bind_rows(sc_div_eff, sc_div_sens) %>%
  dplyr::filter(is.finite(delta)) %>%
  dplyr::group_by(group) %>%
  dplyr::arrange(dplyr::desc(delta), .by_group = TRUE) %>%
  dplyr::ungroup()

# Order super_clusters by max delta across groups (largest first)
sc_order <- sc_delta_both %>%
  dplyr::group_by(sc_ref) %>%
  dplyr::summarise(max_delta = max(delta, na.rm = TRUE), .groups = "drop") %>%
  dplyr::arrange(dplyr::desc(max_delta)) %>%
  dplyr::pull(sc_ref)

sc_delta_both_plot <- sc_delta_both %>%
  dplyr::mutate(sc_ref = factor(sc_ref, levels = sc_order))

pd <- ggplot2::position_dodge(width = 0.6)

p_lollipop_both <- ggplot2::ggplot(
  sc_delta_both_plot,
  ggplot2::aes(x = sc_ref, y = delta, fill = group, color = group, group = group)
) +
  # lollipop stems (0 -> delta), dodged by group
  ggplot2::geom_linerange(
    ggplot2::aes(ymin = 0, ymax = delta),
    position = pd, colour = "grey70", linewidth = 1
  ) +
  # lollipop heads, dodged by group
  ggplot2::geom_point(
    position = pd, shape = 21, size = 2.8, stroke = 0.6, colour = "black"
  ) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
  ggplot2::scale_fill_manual(values = c(
    `AN/DN → effector` = paper.cols[["efferent"]],
    `sensory → AN/DN`  = paper.cols[["afferent"]]
  ), guide = "none") +
  ggplot2::scale_color_manual(values = c(
    `AN/DN → effector` = paper.cols[["efferent"]],
    `sensory → AN/DN`  = paper.cols[["afferent"]]
  ), guide = "none") +
  ggplot2::scale_x_discrete(labels = function(x) gsub("_", " ", x)) +
  ggplot2::labs(
    x = "seed super_cluster",
    y = expression(Delta~"similarity  (within-cell type  −  cross-cell type within super-cluster)")
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.y = ggplot2::element_text(size = 10),
                 axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) 

# Save
print(p_lollipop_both)
ggplot2::ggsave(file.path(banc.fig4.extra.path, "supercluster_delta_lollipop_sens_vs_eff.pdf"),
                p_lollipop_both, width = 9, height = 3, dpi = 300)

########################
### INFLUENCE SCORES ###
########################

# Connect to .sql file
chosen.seeds <- na.omit(unique(banc.an.dn.meta$seed_12))
chosen.seeds <- na.omit(c(chosen.seeds,na.omit(unique(banc.sources$seed_07))))
chosen.targets <- na.omit(unique(c(banc.targets$id,banc.an.dn.meta$id)))
con <- DBI::dbConnect(RSQLite::SQLite(),
                      file.path(banc.dropbox.influence.save.path,influence.sqlite))
influence.dn.df <- dplyr::tbl(con, influence.table) %>%
  dplyr::filter(!is_seed,
                level %in% c("seed_12","seed_07"),
                seed %in% !!chosen.seeds,
                id %in% !!chosen.targets) %>%
  dplyr::select(seed, level, id, influence_original, influence_norm_original, influence_syn_norm) %>%
  dplyr::collect()
dbDisconnect(con)

# Format
influence.dn.df <- influence.dn.df %>%
  dplyr::left_join(banc.meta %>%
                     dplyr::distinct(root_id, super_class, cell_class, cluster, super_cluster),
                   by = c("id"="root_id"))

################
### HEATMAPS ###
################

# Calculate
inf.metrics <- c(
  "influence_log",
  "influence_norm_log",
  "influence_norm_log_minmax",
  "influence_log_minmax",
  "influence_log_minmax_seed",
  "influence_syn_norm_log"
)
inf.metrics <- "influence_norm_log"
for(inf.metric in inf.metrics){
  
  # Plot
  nn.cluster.out.nn.cluster.key.plot <- banc_plot_key_features(
    influence.meta = influence.dn.df %>%
      dplyr::mutate(seed_original = seed) %>%
      dplyr::mutate(super_cluster = dplyr::case_when(
        !is.na(super_cluster) ~ super_cluster,
        TRUE ~ NA
      )) %>%
      dplyr::left_join(banc.meta %>%
                         dplyr::mutate(super_cluster = dplyr::case_when(
                           !is.na(super_cluster) ~ super_cluster,
                           TRUE ~ NA
                         )) %>%
                         dplyr::mutate(seed = dplyr::case_when(
                           super_class %in% c("ascending","descending") ~ seed_12,
                           TRUE ~ seed_07
                         )) %>%
                         dplyr::filter(!is.na(seed)) %>%
                         dplyr::distinct(seed, .keep_all = TRUE) %>%
                         dplyr::distinct(seed,  seed_super_cluster = super_cluster),
                       by=c("seed")) %>%
      dplyr::mutate(seed = seed_super_cluster,
                    target = super_cluster) %>%
      dplyr::filter(!is.na(seed), 
                    !is.na(target)),
    ###
    inf.metric = inf.metric,
    target.map = NULL,
    width = 6,
    height = 6,
    recalculate = TRUE,
    row.annotation = NULL,
    show.annotation = FALSE,
    influence.level = NULL,
    save.path = banc.fig5.path,
    seed.map  = FALSE,
    chosen.seeds = NULL,
    chosen.targets = NULL, 
    row.cols = NULL,
    super.class = NULL,
    plot.name = sprintf("managed_neck_super_clusters_to_neck_super_clusters_%s.pdf",inf.metric),
    rev = FALSE,
    row.dend = NULL,
    col.dend = NULL,
    method = "euclidean",
    symmetric = TRUE,
    diagonal = FALSE
  )
  
  #  by cluster
  nn.cluster.out.nn.cluster.key.plot <- banc_plot_key_features(
    influence.meta = influence.dn.df %>%
      dplyr::filter(id %in% banc.an.dn.meta$id) %>%
      dplyr::mutate(target = cluster) %>%
      dplyr::left_join(banc.an.dn.meta %>%
                         dplyr::distinct(seed_12, .keep_all = TRUE) %>%
                         dplyr::distinct(seed_12, umap_cluster = cluster),
                       by=c("seed"="seed_12")) %>%
      dplyr::mutate(seed = umap_cluster) %>%
      dplyr::filter(!is.na(seed), 
                    !is.na(target)),
    ###
    inf.metric = inf.metric,
    target.map = NULL,
    width = 14,
    height = 14,
    recalculate = TRUE,
    row.annotation = NULL,
    show.annotation = FALSE,
    influence.level = "seed_12",
    save.path = banc.fig5.supp.path,
    seed.map  = FALSE,
    chosen.seeds = NULL,
    chosen.targets = NULL, 
    row.cols = NULL,
    super.class = NULL,
    plot.name = sprintf("managed_neck_clusters_to_neck_clusters_%s.pdf",inf.metric),
    rev = FALSE,
    row.dend = NULL,
    col.dend = NULL,
    method = "euclidean",
    symmetric = TRUE,
    diagonal = FALSE
  )
  
  # Plot
  nn.cluster.in.mb.cx.key.plot <- banc_plot_key_features(
    influence.meta = influence.dn.df %>%
      dplyr::filter(id %in% banc.an.dn.meta$id) %>%
      dplyr::mutate(target = dplyr::case_when(
        !is.na(super_cluster) ~ super_cluster,
        TRUE ~ NA
      )) %>%
      dplyr::left_join(banc.meta %>%
                         dplyr::left_join(cns.functions %>%
                                            dplyr::select(cell_type, response) %>%
                                            dplyr::distinct(cell_type, .keep_all = TRUE), 
                                          by = "cell_type") %>%
                         dplyr::mutate(response = dplyr::case_when(
                           grepl("central_complex|mushroom_body",cell_class) ~ seed_07,
                           grepl("visual",super_class) ~ response,
                           TRUE ~ NA
                         )) %>%
                         dplyr::mutate(seed = dplyr::case_when(
                           grepl("central_complex|mushroom_body",cell_class) ~ seed_07,
                           grepl("visual",super_class)&!is.na(response) ~ seed_07,
                           TRUE ~ NA
                         )) %>%
                         dplyr::mutate(seed_cell_class = dplyr::case_when(
                           grepl("central_complex",cell_class) ~ "central_complex",
                           grepl("mushroom",cell_class) ~ "mushroom_body",
                           grepl("visual",super_class) ~ "visual_projection",
                           TRUE ~ NA
                         )) %>%
                         dplyr::filter(!is.na(seed),
                                       !is.na(response),
                                       response!="") %>%
                         dplyr::distinct(seed, 
                                         .keep_all = TRUE) %>%
                         dplyr::distinct(seed, 
                                         response,
                                         seed_cell_class),
                       by=c("seed")) %>%
      dplyr::mutate(seed = gsub("_|,.*"," ",response),
                    seed = gsub(" $","",seed)) %>%
      dplyr::filter(!is.na(seed), 
                    !is.na(seed_cell_class),
                    !is.na(target)),
    ###
    inf.metric = inf.metric,
    target.map = NULL,
    width = 14,
    height = 6,
    recalculate = TRUE,
    col.annotation = "seed_cell_class",
    col.order = TRUE,
    row.annotation = NULL,
    show.annotation = FALSE,
    influence.level = NULL,
    save.path = banc.fig6.path,
    seed.map  = FALSE,
    chosen.seeds = NULL,
    chosen.targets = NULL, 
    row.cols = NULL,
    super.class = NULL,
    plot.name = sprintf("managed_neck_super_clusters_and_mb_cx_%s.pdf",inf.metric),
    rev = FALSE,
    row.dend = NULL,
    col.dend = NULL,
    method = "euclidean",
    symmetric = FALSE,
    diagonal = TRUE
  )
  
  # All by cluster
  nn.cluster.out.nn.cluster.key.plot <- banc_plot_key_features(
    influence.meta = influence.dn.df %>%
      dplyr::filter(id %in% banc.eff.meta$id) %>%
      dplyr::mutate(target = cluster) %>%
      dplyr::left_join(banc.an.dn.meta %>%
                         dplyr::distinct(seed_12, .keep_all = TRUE) %>%
                         dplyr::distinct(seed_12, umap_cluster = cluster),
                       by=c("seed"="seed_12")) %>%
      dplyr::mutate(seed = umap_cluster) %>%
      dplyr::filter(!is.na(seed), 
                    !is.na(target)),
    ###
    inf.metric = inf.metric,
    target.map = NULL,
    width = 12,
    height = 8,
    recalculate = TRUE,
    row.annotation = NULL,
    show.annotation = FALSE,
    influence.level = "seed_12",
    save.path = banc.fig5.supp.path,
    seed.map  = FALSE,
    chosen.seeds = NULL,
    chosen.targets = NULL, 
    row.cols = NULL,
    super.class = NULL,
    plot.name = sprintf("managed_neck_clusters_to_efferent_clusters_%s.pdf",inf.metric),
    rev = FALSE,
    row.dend = NULL,
    col.dend = NULL,
    method = "euclidean",
    symmetric = FALSE,
    diagonal = TRUE
  )
  
  # All by cluster
  nn.cluster.out.nn.cluster.key.plot <- banc_plot_key_features(
    influence.meta = influence.dn.df %>%
      dplyr::filter(id %in% banc.eff.meta$id) %>%
      dplyr::mutate(target = super_cluster) %>%
      dplyr::left_join(banc.an.dn.meta %>%
                         dplyr::distinct(seed_12, .keep_all = TRUE) %>%
                         dplyr::distinct(seed_12, seed_super_cluster = super_cluster),
                       by=c("seed"="seed_12")) %>%
      dplyr::mutate(seed = seed_super_cluster) %>%
      dplyr::filter(!is.na(seed), 
                    !is.na(target)),
    ###
    inf.metric = inf.metric,
    target.map = NULL,
    width = 12,
    height = 8,
    recalculate = TRUE,
    row.annotation = NULL,
    show.annotation = FALSE,
    influence.level = "seed_12",
    save.path = banc.fig5.path,
    seed.map  = FALSE,
    chosen.seeds = NULL,
    chosen.targets = NULL, 
    row.cols = NULL,
    super.class = NULL,
    plot.name = sprintf("managed_neck_super_clusters_to_efferent_super_clusters_%s.pdf",inf.metric),
    rev = TRUE,
    row.dend = NULL,
    col.dend = NULL,
    method = "euclidean",
    symmetric = FALSE,
    diagonal = TRUE
  )
}

####################
### Correlations ###
####################
# 
# # Get sensory seed map
# sensory.seed.map <- c(#abdomen_endocrine_left = "abdomen_endocrine", 
#   #abdomen_endocrine_right = "abdomen_endocrine", 
#   abdomen_multidendritic_neuron = "abdomen multidendritic", 
#   abdomen_orphan_neuron = "abdomen orphan", 
#   #abdomen_strand_neuron, 
#   abdominal_wall_multidendritic_neuron = "abdominal wall multidendritic", 
#   antenna_bristle_neuron = "antenna bristle",
#   antenna_campaniform_sensillum_neuron = "antenna campaniform", 
#   antenna_hygrosensory_receptor_neuron = "antenna hygrosensory receptor", 
#   antenna_olfactory_receptor_neuron = "antenna olfactory receptor", 
#   #antenna_orphan_neuron = "antenna orphan", 
#   antenna_thermosensory_receptor_neuron = "antenna thermosensory receptor", 
#   #aorta_orphan_neuron = "aorta",
#   #APDN3, 
#   cibarium_multidendritic_neuron = "cibarium multidendritic", 
#   crop_internal_taste_sensillum_neuron = "crop internal taste", 
#   #endocrine_left = "vnc endocrine", 
#   #endocrine_right = "vnc endocrine", 
#   eye_bristle_neuron = "eye bristle", 
#   front_leg_bristle_neuron = "leg bristle", 
#   front_leg_chordotonal_organ_neuron = "leg chordotonal", 
#   front_leg_claw_chordotonal_organ_neuron = "leg chordotonal", 
#   front_leg_club_chordotonal_organ_neuron = "leg chordotonal", 
#   front_leg_hair_plate_neuron = "leg hair plate", 
#   front_leg_hook_chordotonal = "leg chordotonal", 
#   front_leg_multidendritic_neuron = "leg multidendritic", 
#   front_leg_campaniform_sensillum_neuron = "leg campaniform", 
#   #front_leg_orphan_neuron = "leg orphan",  
#   front_leg_taste_peg_neuron = "leg taste peg", 
#   frontal_bristle_neuron = "head bristle",   
#   haustellum_bristle_neuron  = "head bristle",  
#   interocellar_bristle_neuron  = "head bristle",   
#   interommatidial_bristle_neuron  = "head bristle",   
#   occipital_bristle_neuron  = "head bristle",   
#   occipital_dorsal_bristle_neuron  = "head bristle",  
#   postocellar_bristle_neuron  = "head bristle",   
#   postorbital_dorsal_bristle_neuron  = "head bristle",   
#   postorbital_ventral_bristle_neuron  = "head bristle",   
#   vibrissa_bristle_neuron  = "head bristle",  
#   maxillary_palp_bristle_neuron  = "head bristle",  
#   haltere_bristle_neuron = "haltere bristle", 
#   haltere_campaniform_sensillum_neuron = "haltere campaniform", 
#   #haltere_orphan_neuron = "haltere orphan", 
#   hemolymph_sensory_neuron = "hemolymph", 
#   hind_leg_bristle_neuron = "leg bristle", 
#   hind_leg_campaniform_sensillum_neuron = "leg campaniform", 
#   hind_leg_chordotonal_organ_neuron = "leg chordotonal", 
#   hind_leg_claw_chordotonal_organ_neuron = "leg chordotonal", 
#   hind_leg_club_chordotonal_organ_neuron  = "leg chordotonal",  
#   hind_leg_hair_plate_neuron  = "leg hair plate",  
#   hind_leg_hook_chordotonal  = "leg chordotonal",  
#   hind_leg_multidendritic_neuron = "leg multidendritic",  
#   #hind_leg_orphan_neuron = "leg orphan",  
#   hind_leg_taste_peg_neuron = "leg taste peg",   
#   internal_thermosensory_receptor_neuron = "internal thermosensory receptor", 
#   johnstons_organ_A_neuron = "johnstons organ A", 
#   johnstons_organ_B_neuron = "johnstons organ B", 
#   johnstons_organ_C_neuron = "johnstons organ C", 
#   johnstons_organ_D_neuron = "johnstons organ D", 
#   johnstons_organ_E_neuron = "johnstons organ E", 
#   johnstons_organ_F_neuron = "johnstons organ F", 
#   johnstons_organ_other_neuron = "johnstons organ other", 
#   labellum_bristle_neuron = "labellum bristle", 
#   labellum_external_taste_sensillum_neuron = "labellum external taste", 
#   #labellum_orphan_neuron = "labellum orphan", 
#   labellum_taste_peg_neuron = "labellum taste peg", 
#   #leg_taste_peg_neuron = "leg_taste_peg", 
#   maxillary_palp_olfactory_receptor_neuron = "maxillary palp olfactory receptor", 
#   metathoracic_chordotonal_organ_neuron = "metathoracic chordotonal",
#   middle_leg_bristle_neuron = "leg bristle", 
#   middle_leg_campaniform_sensillum_neuron = "leg campaniform", 
#   middle_leg_chordotonal_organ_neuron = "leg chordotonal", 
#   middle_leg_claw_chordotonal_organ_neuron = "leg chordotonal", 
#   middle_leg_club_chordotonal_organ_neuron = "leg chordotonal", 
#   middle_leg_hair_plate_neuron  = "leg hair plate",  
#   middle_leg_hook_chordotonal = "leg chordotonal", 
#   middle_leg_multidendritic_neuron = "leg multidendritic", 
#   #middle_leg_orphan_neuron = "leg orphan", 
#   middle_leg_taste_peg_neuron = "leg taste peg",
#   #pars_intercerebralis_endocrine_enteric_left = "pars_intercerebralis_enteric", 
#   #pars_intercerebralis_endocrine_enteric_right = "pars_intercerebralis_enteric", 
#   #pars_lateralis_endocrine_corpus_allatum_left = "pars_lateralis_endocrine_retrocerebral_complex", 
#   #pars_lateralis_endocrine_corpus_allatum_right = "pars_lateralis_endocrine_retrocerebral_complex", 
#   #pars_lateralis_endocrine_retrocerebral_complex_left = "pars_lateralis_endocrine_retrocerebral_complex", 
#   #pars_lateralis_endocrine_retrocerebral_complex_right = "pars_lateralis_endocrine_retrocerebral_complex", 
#   pharynx_internal_taste_sensillum_neuron = "pharynx internal", 
#   pharynx_orphan_neuron = "pharynx internal", 
#   prosternal_hair_plate_neuron = "prosternal hair plate", 
#   prothoracic_chordotonal_organ_neuron = "prothoracic chordotonal", 
#   #retina_photoreceptor_neuron = "retina_photoreceptor", 
#   #subesophageal_zone_endocrine_left = "subesophageal zone endocrine", 
#   #subesophageal_zone_endocrine_right = "subesophageal zone endocrine", 
#   thorax_bristle_neuron = "thorax bristle", 
#   thorax_campaniform_sensillum_neuron = "thorax campaniform", 
#   thorax_multidendritic_neuron = "thorax multidendritic",
#   thorax_orphan_neuron = "thorax orphan", 
#   wheelers_chordotonal_organ_neuron = "wheelers organ chordotonal", 
#   wing_base_campaniform_sensillum_neuron = "wing campaniform", 
#   wing_base_chordotonal_organ_neuron = "wing chordotonal",
#   #wing_base_orphan_neuron = "wing base orphan", 
#   wing_campaniform_sensillum_neuron = "wing campaniform", 
#   #wing_endocrine_left = "wing_non_motor", 
#   #wing_endocrine_right = "wing_non_motor", 
#   wing_margin_bristle_neuron = "wing bristle",
#   wing_margin_taste_peg_neuron = "wing taste", 
#   wing_multidendritic_neuron = "wing multidendritic", 
#   wing_tegula_campaniform_sensillum_neuron = "wing campaniform", 
#   wing_tegula_chordotonal_organ_neuron = "wing chordotonal", 
#   wing_tegula_hair_plate_neuron = "wing hair plate", 
#   wing_tegula_orphan_neuron = "wing orphan",
#   visual_front_leg_feedback = "visual leg feedback", 
#   visual_horizontal_widefieldmotion = "visual horizontal widefield motion", 
#   `visual_large_objects,visual_thin_vertical_bar` = "visual thin vertical bar", 
#   visual_loom = "visual loom", 
#   `visual_object,visual_loom` = "visual loom",  
#   visual_polarized_light = "polarized light", 
#   visual_small_object = "visual small object", 
#   `visual_small_object,visual_loom` = "visual loom",  
#   visual_thin_vertical_bar = "visual thin vertical bar", 
#   visual_vertical_widefieldmotion = "visual vertical widefield motion",
#   visual_ocellar = "visual ocellar"
# )
neck.seeds <- na.omit(unique(banc.an.dn.meta$cluster))
neck.super.seeds <- na.omit(unique(banc.an.dn.meta$super_cluster))
names(neck.seeds) <- neck.seeds
sensor.seed.map <- c(sensory.seed.map,neck.seeds,neck.super.seeds)

# Get alternative dataset for validation (seed_02)
chosen.seeds <- unique(c(na.omit(unique(banc.an.dn.meta$seed_07)),
                  na.omit(banc.vpn.meta %>%
                            dplyr::left_join(cns.functions %>%
                                               dplyr::select(cell_type, response) %>%
                                               dplyr::distinct(cell_type, .keep_all = TRUE), 
                                             by = "cell_type") %>%
                            dplyr::filter(!is.na(response), 
                                          response!="") %>%
                            dplyr::pull(seed_07)),
                  na.omit(unique(banc.sens.meta$seed_02))))
con <- DBI::dbConnect(RSQLite::SQLite(),
                      file.path(banc.dropbox.influence.save.path,influence.sqlite))
chosen.ids <- banc.meta %>%
  dplyr::filter(!is.na(super_cluster)) %>%
  dplyr::pull(id)
influence.neck.and.sens.df <- dplyr::tbl(con, influence.table) %>%
  dplyr::filter(!is_seed,
                level %in% c("seed_02","seed_07"),
                seed %in% chosen.seeds,
                id %in% chosen.ids) %>%
  dplyr::collect() %>%
  dplyr::select(seed, level, id, influence_original, influence_norm_original, influence_syn_norm) %>%
  dplyr::filter(!grepl("unknown",seed))
dbDisconnect(con)

# Correlation, all by cluster
corr.nn.sens.cluster.sim.key.plot <- banc_plot_key_features(
  influence.meta = influence.neck.and.sens.df %>%
    dplyr::left_join(banc.meta %>%
                       dplyr::select(id, cell_type),
                     by = "id") %>%
    dplyr::mutate(target = cell_type) %>%
    dplyr::left_join(banc.an.dn.meta %>%
                       dplyr::distinct(seed_07, .keep_all = TRUE) %>%
                       dplyr::distinct(seed_07, umap_cluster = cluster),
                     by=c("seed"="seed_07")) %>%
    dplyr::left_join(cns.functions %>%
                       dplyr::select(cell_type, response) %>%
                       dplyr::distinct(cell_type, .keep_all = TRUE), 
                     by = c("seed"="cell_type")) %>%
    dplyr::mutate(
      seed = dplyr::case_when(
      !is.na(response) ~ response,
      !is.na(umap_cluster) ~ umap_cluster,
      grepl("^AN|^DN|7",seed) ~ NA,
      TRUE ~ seed
    )) %>%
    dplyr::filter(!is.na(target),
                  !is.na(seed),
                  target!="0",
                  target!="",
                  seed!="0",
                  seed!=""),
  ###
  color.min = 0.00,
  color.max = 0.5,
  row.select = unname(sensory.seed.map),
  col.select = na.omit(unique(banc.an.dn.meta$cluster)),
  inf.metric = "influence",
  target.map = NULL,
  width = 12,
  height = 12,
  recalculate = TRUE,
  row.annotation = NULL,
  show.annotation = FALSE,
  influence.level = NULL,
  save.path = banc.fig3.path,
  seed.map  = sensor.seed.map,
  chosen.seeds = NULL,
  chosen.targets = NULL, 
  row.cols = NULL,
  super.class = NULL,
  plot.name = sprintf("correlation_neck_clusters_and_sensors_%s.pdf","influence"),
  rev = TRUE,
  row.thresh = 0.95,
  autocorrelation = TRUE,
  symmetric = FALSE,
  diagonal = TRUE
)

# Correlation, all by super cluster
corr.nn.sens.super.cluster.sim.key.plot <- banc_plot_key_features(
  influence.meta = influence.neck.and.sens.df %>%
    dplyr::left_join(banc.meta %>%
                       dplyr::select(id, cell_type),
                     by = "id") %>%
    dplyr::mutate(target = cell_type) %>%
    dplyr::left_join(banc.an.dn.meta %>%
                       dplyr::distinct(seed_07, .keep_all = TRUE) %>%
                       dplyr::distinct(seed_07, seed_super_cluster = super_cluster),
                     by=c("seed"="seed_07")) %>%
    dplyr::left_join(cns.functions %>%
                       dplyr::select(cell_type, response) %>%
                       dplyr::distinct(cell_type, .keep_all = TRUE), 
                     by = c("seed"="cell_type")) %>%
    dplyr::mutate(
      seed = dplyr::case_when(
        !is.na(response) ~ response,
        !is.na(seed_super_cluster) ~ seed_super_cluster,
        grepl("^AN|^DN|7",seed) ~ NA,
        TRUE ~ seed
      )) %>%
    dplyr::filter(!is.na(target),
                  !is.na(seed),
                  target!="0",
                  target!="",
                  seed!="0",
                  seed!=""),
  ###
  color.min = 0.00,
  color.max = 0.5,
  row.select = unname(sensory.seed.map),
  col.select = na.omit(unique(banc.an.dn.meta$super_cluster)),
  inf.metric = "influence",
  target.map = NULL,
  cellheight = 8,
  cellwidth = 8,
  width = 8,
  height = 8,
  recalculate = TRUE,
  row.annotation = NULL,
  show.annotation = FALSE,
  col.order = super.clust.order,
  influence.level = NULL,
  save.path = banc.fig3.path,
  seed.map  = sensor.seed.map,
  chosen.seeds = NULL,
  chosen.targets = NULL, 
  row.cols = NULL,
  super.class = NULL,
  plot.name = sprintf("correlation_neck_super_clusters_and_sensors_%s.pdf","influence"),
  rev = TRUE,
  row.thresh = 0.95,
  autocorrelation = TRUE,
  symmetric = FALSE,
  diagonal = TRUE
)


