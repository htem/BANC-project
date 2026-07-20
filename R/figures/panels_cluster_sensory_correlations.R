#' AN/DN cluster ↔ sensory neuron influence diversity (ED Fig. 7a)
#'
#' For every sensory sub class, computes the adjusted-influence vector
#' (Eq. 10, source-size corrected) onto each AN and DN, then quantifies
#' the cosine similarity between sensory sub classes and AN/DN clusters
#' (ED Fig. 7a). Also bundles in mushroom-body output and central-complex
#' output cells as honorary "sources" for the equivalent comparison used
#' in ED Fig. 10f.
#'
#' Visual centrifugal neurons + visual projection neurons are treated as
#' an additional sensory category here, reflecting the convention in the
#' paper that processed visual-stream cell types serve as proxies for the
#' optic-lobe inputs to ANs/DNs.
#'
#' @section Reads:
#'   banc.meta, banc.edgelist.simple, banc.eff.meta, banc.sens.meta,
#'   banc.vpn.meta, paper.cols
#'   data/banc_annotations/v888/banc_neck_functional_classes.csv                                  (cluster labels)
#'
#' @section Writes:
#'   figures/figure_4/links/supplement/extended_data_fig_7a_*.pdf            (ED Fig. 7a)
#'   figures/figure_4/links/extra/cluster_sensor_diversity_*.pdf             (exploratory)
#'   figures/figure_4/links/*.txt                                             (cosine similarity tables)
#'
#' @section Paper:
#'   ED Fig. 7a — cosine similarity between sensory sub classes and AN/DN clusters.
#'   Methods §"Influence" Eqs. 9–10.
#'
#' @section Schema:
#'   The global synapse-count filter on `banc.edgelist.simple` was
#'   intentionally removed (2026-04-09); `count_thresh = 5` is the
#'   per-call influencer setting and matches the paper threshold.
#'
#' @section Reproduce:
#'   BANC_NCORES=1 Rscript R/figures/panels_cluster_sensory_correlations.R

###############
### STARTUP ###
###############

# load
source("R/startup/banc-startup.R")
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
# Global `count >= 3` filter intentionally removed (2026-04-09) to match
# the earlier global-filter removal in banc-edgelist.R. Keeping all edges
# restores sensor coverage in downstream heatmaps (same reason as the
# panel_an_dn_influence.R fix on 2026-04-07).
ic_banc <- influence_calculator_py(edgelist_simple = banc.edgelist.simple,
                                   meta = banc.meta,
                                   count_thresh = 5)

# Get sensory cell types (seeds) and AN/DN neurons (targets)
banc.sens.csc <- banc.sens.meta %>%
  dplyr::distinct(seed_02) %>%
  dplyr::pull(seed_02)
cts <- na.omit(banc.sens.csc)

# Pre-index banc.meta by seed_02 ONCE (avoids subset() on full meta per iteration).
# Accumulate into a pre-allocated list and rbindlist at the end — O(n) instead of
# rbind-in-loop O(n^2) growth (fixed 2026-04-09).
.seed02_ids <- split(banc.meta$root_id, banc.meta$seed_02)
.andn_ids_set <- unique(banc.an.dn.meta$id)

# Uses banc_influence_loop() from banc-functions.R (PSOCK-parallel).
influence.sens.to.dn.df <- banc_influence_loop(
  cts, "seed_02", "seed_02", .andn_ids_set, ic = ic_banc
)

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
      sc_i   == sc_j   ~ "same cluster",
      TRUE             ~ "different clusters"
    ),
    category = factor(category,
                      levels = c("same cell type","same cluster","different clusters")),
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

# Pre-index banc.meta by seed_12 (avoid filter-full-meta per iteration).
.seed12_ids <- split(banc.meta$root_id, banc.meta$seed_12)
.targets_set <- chosen.targets

# Uses banc_influence_loop() from banc-functions.R (PSOCK-parallel).
influence.dn.to.eff.df <- banc_influence_loop(
  seed_12_values, "seed_12", "seed_12", .targets_set, ic = ic_banc
)

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
      sc_i   == sc_j   ~ "same cluster",
      TRUE             ~ "different clusters"
    ),
    category = factor(category,
                      levels = c("same cell type","same cluster","different clusters"))
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

# write stats — KW dropped from output per fig 4a legend; pairwise Wilcoxon only.
write_diversity_nonparam_summary(
  df           = pairs_both,
  group_col    = "group",
  category_col = "category",
  value_col    = "cos_sim",
  plot_path    = file.path(banc.fig4.path, "seed_influence_cosine_violin_sens_vs_eff.pdf"),
  adjust_method = "holm",
  alpha         = 0.05,
  include_kw   = FALSE
)

#######################################
### FACETED BOXPLOT BY SUPERCLUSTER ###
#######################################

# -----------------------------
# Prepare data with supercluster and group
# -----------------------------
pairs_both_sc <- pairs_both %>%
  dplyr::mutate(super_cluster = sc_i)

# Fixed super_cluster order (user-supplied, 2026-05-20). Any super_cluster
# present in the data but absent from this vector is silently dropped from the
# facets, so it must list every super_cluster we expect to plot.
#
# "vibratory" was excluded on 2026-05-20 on the grounds that it had no AN/DN
# seeds in that cut. That is no longer true: it now carries 141 proofread AN/DN
# neurons (139 ascending, 2 descending) spanning 25 cell types, so it is
# restored here. Grouped with the other mechanosensory clusters.
sc_order_fixed <- c(
  "flight power", "flight steering 1", "flight steering 2", "head orienting",
  "postural control", "threat response", "proprioceptive", "tactile",
  "vibratory", "reproduction", "feeding", "taste-touch", "visceral control",
  "probing", "grooming", "walking", "walking steering"
)
sc_order_by_median <- intersect(sc_order_fixed, unique(pairs_both_sc$super_cluster))

pairs_both_sc <- pairs_both_sc %>%
  dplyr::filter(super_cluster %in% sc_order_by_median) %>%
  dplyr::mutate(super_cluster = factor(super_cluster, levels = sc_order_by_median))

# Color mapping per super_cluster
super_cluster_colors <- setNames(paper.cols[sc_order_by_median], sc_order_by_median)

# Just to be explicit about x order
pairs_both_sc <- pairs_both_sc %>%
  dplyr::mutate(category = factor(
    category,
    levels = c("same cell type", "same cluster", "different clusters")
  ))

# ----------------------------------------
# Helper: significance label for p-values
# ----------------------------------------
sig_label_func <- function(p) {
  if (is.na(p)) "ns"
  else if (p < 0.001) "***"
  else if (p < 0.01) "**"
  else if (p < 0.05) "*"
  else "ns"
}

# ---------------------------------------------------
# Build per-(group, supercluster) statistics + bracket geometry
# ---------------------------------------------------
build_stat_rows <- function(sc, grp, sc_data) {
  same_sc <- sc_data %>%
    dplyr::filter(category == "same cluster") %>%
    dplyr::pull(cos_sim)

  same_ct <- sc_data %>%
    dplyr::filter(category == "same cell type") %>%
    dplyr::pull(cos_sim)

  diff_sc <- sc_data %>%
    dplyr::filter(category == "different clusters") %>%
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

# Re-apply fixed factor order (after stat_results gets built with the same order).
pairs_both_sc$super_cluster <- factor(pairs_both_sc$super_cluster, levels = sc_order_by_median)
stat_results$super_cluster  <- factor(stat_results$super_cluster,  levels = sc_order_by_median)

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
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(
    axis.text.x   = element_text(angle = 90, hjust = 1, vjust = 1, size = 7),
    # Rotate the column-facet labels (super_cluster names — long, e.g.
    # "walking steering", "postural control") to 90° so they don't get
    # clipped between narrow facets (issue feedback 2026-05-03).
    strip.text.x  = element_text(size = 8, angle = 90, hjust = 0, vjust = 0.5),
    strip.text.y  = element_blank(),
    plot.margin   = margin(t = 40, r = 8, b = 8, l = 8, unit = "pt"),
    legend.position = "none"
  )

# Show
print(p_violin_faceted)

# Save — taller (was h=4 → h=5.5) to give the rotated super_cluster strip
# labels room above the panels without clipping (issue feedback 2026-05-03).
ggplot2::ggsave(
  file.path(banc.fig4.supp.path, "seed_influence_cosine_violin_by_supercluster.pdf"),
  p_violin_faceted,
  width = 14,
  height = 5.5,
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

# Pull cached parquet influence data (cheaper than re-computing on the fly).
# Pre-filter to the target id set BEFORE downstream per-plot filtering so we're
# not passing a 20M-row df to each of the 4+ banc_plot_key_features calls below
# (added 2026-04-09).
chosen.seeds <- na.omit(unique(banc.an.dn.meta$seed_12))
chosen.seeds <- na.omit(c(chosen.seeds,na.omit(unique(banc.sources$seed_07))))
chosen.targets <- na.omit(unique(c(banc.targets$id,banc.an.dn.meta$id)))
influence.dn.df <- query_influence(
    levels = c("seed_12", "seed_07"), seeds = chosen.seeds,
    ids = chosen.targets, normalize = FALSE
  ) %>%
  dplyr::select(seed, level, id, influence_original, influence_norm_original)

# Format + pre-filter to ids we actually plot (AN/DN ∪ effectors ∪ targets).
# Drops the df from ~20M rows to roughly the set we're plotting, so downstream
# per-plot filter+dcast work is much cheaper (added 2026-04-09). Each plot call
# still passes recalculate=TRUE so the per-plot subset is re-normalised inside
# banc_plot_key_features — preserves original semantics.
.plot_ids <- unique(c(banc.an.dn.meta$id, banc.eff.meta$id, chosen.targets))
influence.dn.df <- influence.dn.df %>%
  dplyr::filter(id %in% .plot_ids) %>%
  dplyr::left_join(banc.meta %>%
                     dplyr::distinct(root_id, super_class, cell_class, cluster, super_cluster),
                   by = c("id"="root_id"))

message(sprintf("[cluster_sensory] influence.dn.df after pre-filter: %d rows, %d unique seeds, %d unique ids",
                nrow(influence.dn.df),
                dplyr::n_distinct(influence.dn.df$seed),
                dplyr::n_distinct(influence.dn.df$id)))

################
### HEATMAPS ###
################

inf.metrics <- "influence_norm_log"
for(inf.metric in inf.metrics){
  message(sprintf("[cluster_sensory] metric loop: inf.metric = %s", inf.metric))

  # Plot 1: neck super_clusters -> neck super_clusters
  message("[cluster_sensory] PLOT 1/5: neck super_clusters to neck super_clusters")
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
    save.path = banc.fig5.extra.path,
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
  
  # Plot 2: neck clusters -> neck clusters
  message("[cluster_sensory] PLOT 2/5: neck clusters to neck clusters")
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
    save.path = banc.fig5.extra.path,
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
  
  # Plot 3: neck super_clusters + mushroom body / central complex
  message("[cluster_sensory] PLOT 3/5: neck super_clusters + MB/CX")
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
    save.path = banc.fig6.extra.path,
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
  
  # Plot 4: neck clusters -> efferent clusters
  message("[cluster_sensory] PLOT 4/5: neck clusters to efferent clusters")
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
    save.path = banc.fig5.extra.path,
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
  
  # Plot 5: neck super_clusters -> efferent super_clusters
  message("[cluster_sensory] PLOT 5/5: neck super_clusters to efferent super_clusters")
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
    save.path = banc.fig5.extra.path,
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
chosen.ids <- banc.meta %>%
  dplyr::filter(!is.na(super_cluster)) %>%
  dplyr::pull(id)
influence.neck.and.sens.df <- query_influence(
    levels = c("seed_02", "seed_07"), seeds = chosen.seeds,
    ids = chosen.ids, normalize = FALSE
  ) %>%
  dplyr::select(seed, level, id, influence_original, influence_norm_original) %>%
  dplyr::filter(!grepl("unknown",seed))

message(sprintf("[cluster_sensory] influence.neck.and.sens.df: %d rows, %d unique seeds, %d unique ids",
                nrow(influence.neck.and.sens.df),
                dplyr::n_distinct(influence.neck.and.sens.df$seed),
                dplyr::n_distinct(influence.neck.and.sens.df$id)))

# Correlation, all by cluster
message("[cluster_sensory] PLOT 6/7: correlation_neck_clusters_and_sensors")
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
  save.path = banc.fig3.extra.path,
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
message("[cluster_sensory] PLOT 7/7: correlation_neck_super_clusters_and_sensors")
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
  save.path = banc.fig3.supp.path,
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


