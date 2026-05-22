#' Mushroom-body and central-complex influence on ANs/DNs (Fig. 6f–g/h, ED Fig. 10f–g)
#'
#' Quantifies how mushroom-body output neurons (MBONs) and central-
#' complex output neurons influence AN/DN cell types, and the reciprocal
#' AN/DN → MB-input / CX-input influence. Three downstream panels:
#'
#'   - Fig. 6f — distribution of adjusted influence from MBONs, CX outputs,
#'               visual projection, and "sensory" onto the 1,000 AN/DN
#'               cell types; KS tests vs. sensory baseline.
#'   - Fig. 6g — per-cluster AN/DN cell-type counts above the 17.28
#'               influence threshold (the elbow from panels_body_parts.R).
#'   - Fig. 6h — example MB → AN/DN and CX → AN/DN circuit (delegated to
#'               panels_vignette_networks.R; this script produces the
#'               supporting heatmaps).
#'
#' Uses an unfiltered edgelist (count > 0) so that weak MB/CX → AN/DN
#' chains aren't pruned away at the graph level; downstream filtering is
#' applied at the adjusted-influence level via the 17.28 threshold.
#'
#' @section Reads:
#'   banc.meta, banc.edgelist.simple, paper.cols
#'   data/banc_annotations/v888/banc_neck_functional_classes.csv                                    (cluster labels)
#'   data/determined_thresholds/influence_norm_log_elbow_threshold.csv        (17.28 cutoff)
#'
#' @section Writes:
#'   figures/figure_6/links/mb_cx_influence_ks_*.pdf                          (Fig. 6f)
#'   figures/figure_6/links/mb_cx_cluster_threshold_*.pdf                     (Fig. 6g)
#'   figures/figure_6/links/supplement/extended_data_fig_10f_*.pdf            (ED Fig. 10f)
#'   figures/figure_6/links/supplement/extended_data_fig_10g_*.pdf            (ED Fig. 10g)
#'   figures/figure_6/links/*.txt                                              (KS test summaries)
#'
#' @section Paper:
#'   Fig. 6f — KS tests, visual projection / MBON / CX-output / sensory
#'             influence onto 1,000 AN/DN cell types; dashed line = 17.28.
#'   Fig. 6g — AN/DN cell types per cluster above the 17.28 cutoff per upstream source.
#'   ED Fig. 10f — MBON / CX-output → AN/DN cluster influence (Eq. 10).
#'   ED Fig. 10g — AN/DN cluster → MB-input / CX-input influence (Eq. 10).
#'   Methods §"Influence" (Eqs. 9–10).
#'
#' @section Schema:
#'   `bc.meta <- banc.meta` alias retained for legacy join paths.
#'   `count > 0` (no count threshold at graph level) is intentional here;
#'   the 17.28 elbow handles noise downstream.
#'
#' @section Reproduce:
#'   BANC_NCORES=1 Rscript R/figures/panels_mbx_cx_control.R

## LOAD PACKAGES ##
###################

# Load required packages and data for influence validation
source("R/startup/banc-startup.R")
source("R/startup/banc-meta.R")
source("R/startup/banc-edgelist.R")
library(influencer)
bc.meta <- banc.meta

# Define output paths for different figure types
banc.fig6.path <- "figures/figure_6/links/"
banc.fig6.supp.path <- "figures/figure_6/links/supplement"
banc.fig6.extra.path <- "figures/figure_6/links/extra"

##########################
## INFLUENCE CALCULATOR ##
##########################

# Define AN/DN metadata
banc.an.dn.meta <- banc.meta %>%
  dplyr::filter(super_class %in% c("ascending","descending")) %>%
  dplyr::filter(!grepl("^SA|^SN|^AN_4|AN_5|^IN",cell_type))

# Set up for influence calculation. We propagate ALL edges (no count filter)
# here so the central_complex_output / mushroom_body_output influence chains
# aren't pruned at low strength — those pathways often involve weak hops and
# get heavily attenuated by `count >= 3`. The downstream threshold
# (`threshold.inf.value`, loaded from data/determined_thresholds/) handles
# noise filtering at the influence level instead.
ic_banc <- influence_calculator_py(edgelist_simple = banc.edgelist.simple %>%
                                     dplyr::filter(count > 0),
                                   meta = bc.meta,
                                   count_thresh = 5)

# Calculcate influence of MBONs onto DNs/AN
banc.mbon <- banc.meta %>%
  dplyr::filter(grepl("MBON",cell_type))
banc.cxout <- banc.meta %>%
  dplyr::filter(grepl("central_complex_output",cell_class))
banc.vpn <- banc.meta %>%
  dplyr::filter(grepl("visual_projection",super_class))
banc.eff.meta <- banc.meta %>%
  dplyr::filter(grepl("motor",super_class)|grepl("visceral_circulatory",super_class))
banc.targets.meta <- banc.an.dn.meta %>%
  # plyr::rbind.fill(banc.an.dn.meta,
  #                          banc.eff.meta) %>%
  dplyr::mutate(target = dplyr::case_when(
    !is.na(body_part_effector) ~ body_part_effector,
    TRUE ~ cell_type
  ),
  target_super_class = super_class)
banc.targets.ids <- unique(c(banc.an.dn.meta$root_id, banc.eff.meta$root_id))

# Get influence results for BANC
banc.mbon.cts <- banc.mbon %>%
  dplyr::distinct(cell_type) %>%
  dplyr::pull(cell_type)
banc.vpn.cts <- banc.vpn %>%
  dplyr::distinct(cell_type) %>%
  dplyr::pull(cell_type)
banc.cxout.cts <- banc.cxout %>%
  dplyr::distinct(cell_type) %>%
  dplyr::pull(cell_type)
banc.sens.csc  <- banc.sens.meta %>%
  dplyr::distinct(cell_sub_class) %>%
  dplyr::pull(cell_sub_class)
cts <- unique(c(banc.mbon.cts,banc.cxout.cts,banc.sens.csc,banc.vpn.cts))
cts <- na.omit(cts)

# Rbindlist refactor (2026-04-09): pre-allocate + rbindlist instead of
# rbind-in-loop. Also pre-index banc.meta by cell_type / cell_sub_class and
# pre-compute the seed_class lookup so each iteration is cheap.
.ct_to_ids_by_sub <- split(banc.meta$root_id, banc.meta$cell_sub_class)
.ct_to_ids_by_ct  <- split(banc.meta$root_id, banc.meta$cell_type)
.targets_lookup <- banc.targets.meta %>%
  dplyr::distinct(id = root_id, target, target_super_class)
.seed_class_lookup <- function(ct) {
  if (ct %in% banc.mbon.cts) "mushroom_body_output"
  else if (ct %in% banc.cxout.cts) "central_complex_output"
  else if (ct %in% banc.vpn.cts) "visual_projection"
  else "sensory"
}
control_influence_list <- vector("list", length(cts))
for(i in seq_along(cts)){
  ct <- cts[[i]]
  banc.ct.ids <- if (ct %in% banc.sens.csc) {
    unique(.ct_to_ids_by_sub[[ct]])
  } else {
    unique(.ct_to_ids_by_ct[[ct]])
  }
  if (length(banc.ct.ids) == 0) next
  try({
    control_influence.id <- calculate_influence_py(ic_banc, banc.ct.ids) %>%
      dplyr::filter(id %in% banc.targets.ids) %>%
      dplyr::left_join(.targets_lookup, by = "id")
    control_influence.id$seed <- ct
    control_influence.id$seed_class <- .seed_class_lookup(ct)
    control_influence.id$influence_norm_original <- control_influence.id$`Influence_score_(unsigned)`/length(banc.ct.ids)
    control_influence_list[[i]] <- control_influence.id
  })
}
banc.control_influence <- as.data.frame(
  data.table::rbindlist(control_influence_list, fill = TRUE)
) %>%
  dplyr::mutate(influence_original = `Influence_score_(unsigned)`)

###############################################
## AN/DN: RANK vs RELATIVE CUMULATIVE INFLUENCE
## (metric = influence_original, SUM across seeds)
###############################################

# Parameters
top_prop <- 0.25  # 25% cumulative influence cutoff (Y-axis)

# Bring in super_cluster labels for targets (AN/DN)
target_clusters <- banc.meta %>%
  dplyr::filter(root_id %in% banc.targets.ids) %>%
  dplyr::distinct(id = root_id, super_cluster)

# Keep only AN/DN targets; add cluster labels and clean seed_class levels
ci_an_dn_all <- banc.control_influence %>%
  dplyr::filter(target_super_class %in% c("ascending","descending")) %>%
  dplyr::left_join(target_clusters, by = "id") %>%
  dplyr::mutate(
    seed_class    = base::factor(seed_class, levels = c("central_complex_output","mushroom_body_output","visual_projection","sensory")),
    super_cluster = dplyr::coalesce(super_cluster, "unlabeled")
  ) %>%
  dplyr::filter(!base::is.na(influence_original))

# First collapse to (seed_class, seed, target) to guard against duplicates per seed–target
by_seed_target <- ci_an_dn_all %>%
  dplyr::group_by(seed_class, seed, target, super_cluster) %>%
  dplyr::summarise(metric_seed = base::sum(influence_original, na.rm = TRUE), .groups = "drop")

# Then SUM across seeds within each class → one score per (seed_class, target)
by_target_sum <- by_seed_target %>%
  dplyr::group_by(seed_class, target, super_cluster) %>%
  dplyr::summarise(infl_target = base::sum(metric_seed, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(infl_target = dplyr::if_else(base::is.finite(infl_target), infl_target, 0))

# Rank targets (descending) within each class and compute cumulative proportion
curves_rel <- by_target_sum %>%
  dplyr::group_by(seed_class) %>%
  dplyr::arrange(dplyr::desc(infl_target), .by_group = TRUE) %>%
  dplyr::mutate(
    rank       = dplyr::row_number(),
    n_targets  = dplyr::n(),
    cum_infl   = base::cumsum(base::pmax(infl_target, 0)),
    total_infl = base::sum(base::pmax(infl_target, 0)),
    cum_prop   = dplyr::if_else(total_infl > 0, cum_infl / total_infl, NA_real_)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(total_infl > 0)  # drop any class with zero mass

# Plot curves with a horizontal 25% cutoff
p_ecdf_rel_sum_raw <- ggplot2::ggplot(
  curves_rel,
  ggplot2::aes(x = rank, y = cum_prop, color = seed_class, group = seed_class)) +
  ggplot2::geom_step(linewidth = 1.05) +
  ggplot2::geom_hline(yintercept = top_prop, linetype = "dashed", color = "black") +
  ggplot2::labs(
    title = "AN/DN: concentration of influence (relative)",
    subtitle = "metric = influence_original; SUM across seeds",
    x = "ranked AN/DN target cell types (within class)",
    y = "cumulative proportion of class influence",
    color = "seed class"
  ) +
  ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,1)) +
  ggplot2::scale_color_manual(values = paper.cols) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    legend.position = "none",
    panel.grid.minor = ggplot2::element_blank()
  )

# Save
print(p_ecdf_rel_sum_raw)
ggsave(
  filename = file.path(banc.fig6.extra.path, "ecdf_raw_influence_by_control_class_to_ans_dns.pdf"),
  plot = p_ecdf_rel_sum_raw,
  height = 4, width = 4, dpi = 300
)

##############################################################
## VIOLIN: per-target MAX of influence_norm_log by seed_class
## AN/DN targets only; one violin per seed class
## + KS tests vs 'sensory' with Holm-adjusted p-values
##############################################################

# --- Base table: AN/DN targets, precomputed logs, updated class labels ---------
banc.control.df <- banc.control_influence %>%
  calculate_influence_norms()
dens_base <- banc.control.df %>%
  dplyr::filter(target_super_class %in% c("ascending","descending")) %>%
  dplyr::filter(!is.na(influence_norm_log),
                !is.na(seed_class),
                !is.na(target)) %>%
  dplyr::mutate(
    seed_class = base::factor(
      seed_class,
      levels = c("central_complex_output",
                 "mushroom_body_output",
                 "visual_projection",
                 "sensory")
    )
  )

# --- Collapse to per-target MAX (within each seed_class) -----------------------
dens_max <- dens_base %>%
  dplyr::group_by(seed_class, target) %>%
  dplyr::summarise(
    influence_norm_log_max = base::max(influence_norm_log, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::filter(base::is.finite(influence_norm_log_max))

# --- Compute threshold using elbow analysis -----------------------
# Analysis based on panels_body_parts.R
# Calculate threshold from sensory sources to AN/DN targets using elbow method

cat("Elbow analysis: Sensory→AN/DN influence threshold\n")

# Function to find the most significant angle change in a specific range
# `find_angle_change_in_range()` lives in R/startup/banc-functions.R
# (hoisted 2026-05-21; shared with panels_body_parts.R).

# Prepare data for elbow analysis: use MAX influence per target (from dens_max)
# This matches the data shown in max_influence_norm_log_by_control_class_to_ans_dns
elbow_df <- dens_max %>%
  dplyr::filter(seed_class == "sensory") %>%
  dplyr::arrange(desc(influence_norm_log_max)) %>%
  dplyr::mutate(
    rank = row_number(),
    influence_norm_log = influence_norm_log_max  # Alias for consistency with elbow code
  )

cat(sprintf("Targets: n=%d, range=[%.2f, %.2f]\n", nrow(elbow_df),
            min(elbow_df$influence_norm_log, na.rm = TRUE),
            max(elbow_df$influence_norm_log, na.rm = TRUE)))

# Use the elbow threshold loaded by banc-startup.R from
# data/determined_thresholds/influence_norm_log_elbow_threshold.csv (computed
# by panels_body_parts.R). Do NOT hardcode here — that froze the value at the
# resubmission_2-era 17.15 even though the v850 elbow has shifted.
cat(sprintf("Using influence threshold from startup: %.4f\n", threshold.inf.value))

# Find the rank corresponding to this threshold value
elbow_point <- list(
  value = threshold.inf.value,
  rank = which.min(abs(elbow_df$influence_norm_log - threshold.inf.value))[1]
)

cat(sprintf("Threshold: %.2f (rank %d)\n", threshold.inf.value, elbow_point$rank))

# Create elbow plot
elbow_plot <- ggplot2::ggplot(elbow_df, ggplot2::aes(x = rank, y = influence_norm_log)) +
  geom_line(color = "gray70", alpha = 0.6) +
  geom_point(color = paper.cols["sensory"], size = 0.5, alpha = 0.3) +
  geom_vline(xintercept = elbow_point$rank, color = "black", linetype = "dashed", linewidth = 0.8) +
  geom_hline(yintercept = threshold.inf.value, color = "black", linetype = "dashed", linewidth = 0.8) +
  annotate("text",
           x = elbow_point$rank * 1.2,
           y = max(elbow_df$influence_norm_log, na.rm = TRUE) * 0.95,
           label = sprintf("Threshold = %.2f", threshold.inf.value),
           color = "black",
           fontface = "bold",
           hjust = 0) +
  labs(
    title = "Elbow Analysis: Sensory → AN/DN Influence",
    x = "Rank (sorted by descending influence)",
    y = "Influence (norm, log)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "lightgrey"),
    plot.title = element_text(face = "bold", size = 14)
  )

print(elbow_plot)
ggsave(
  filename = file.path(banc.fig6.extra.path, "sensory_to_an_dn_influence_elbow_threshold.pdf"),
  plot = elbow_plot,
  width = 8, height = 6, dpi = 300
)

cat("✓ Saved: sensory_to_an_dn_influence_elbow_threshold.pdf\n")


# --- Non-parametric tests: each seed_class vs "sensory" -------------------------
# Write summary with both KS and Wilcoxon tests comparing each seed_class to 'sensory'
nonparam_out <- write_nonparam_summary(
  df         = dens_max,                                   # your data frame
  out_path   = file.path(banc.fig6.path, "sens_influence_norm_log_compared_to_ans_dns_nonparam_summary.txt"),
  type_col   = "seed_class",                                # grouping column
  value_col  = "influence_norm_log_max",                    # numeric column
  ref_type   = "sensory",                                   # reference group
  adjust_method = "holm",
  alpha      = 0.05,
  calculate_effect_size = TRUE
)

# Skip write_crossed_interaction_summary if function not available
if (exists("write_crossed_interaction_summary")) {
  write_crossed_interaction_summary(
    df       = dens_max,
    source_col = "seed_class",
    target_col = "target",
    value_col = "influence_norm_log_max",
    out_path = file.path(banc.fig6.path, "sens_influence_norm_log_compared_to_ans_dns_interaction_summary.txt"),
    nsim_pb  = 1000,   # increase for final runs
    alpha    = 0.05
  )
} else {
  cat("Note: write_crossed_interaction_summary function not available, skipping interaction analysis.\n")
}

# --- Build violin plot with compact boxplot + threshold line -------------------
p_violin_inflog_max <- ggplot2::ggplot(
  dens_max,
  ggplot2::aes(x = seed_class, y = influence_norm_log_max, fill = seed_class)) +
  ggplot2::geom_violin(trim = FALSE, width = 0.9, alpha = 0.5, color = NA) +   # default width — wider values caused adjacent-class overlap
  ggplot2::geom_boxplot(width = 0.12, outlier.shape = NA, alpha = 0.35, color = "black") +
  ggplot2::geom_hline(yintercept = threshold.inf.value, linetype = "dashed", color = "black") +
  ggplot2::scale_fill_manual(values = paper.cols, guide = "none") +
  ggplot2::scale_x_discrete(expand = ggplot2::expansion(mult = c(0, 0.01))) +   # less gap
  ggplot2::labs(x = "seed class", y = "max influence_norm_log (per target within class)") +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(panel.grid.minor = ggplot2::element_blank()) +
  ggplot2::coord_flip() +
  ggplot2::ylim(c(8,24)) 

# Save
print(p_violin_inflog_max)
ggplot2::ggsave(
  filename = base::file.path(banc.fig6.path, "max_influence_norm_log_by_control_class_to_ans_dns.pdf"),
  plot = p_violin_inflog_max,
  height = 4, width = 4, dpi = 300
)

#########################################################
## SELECT TARGETS ABOVE THRESHOLD & COUNT SUPER_CLUSTERS
#########################################################
top_n_labels <- 10  # how many target names to show per seed_class

# Map targets -> super_cluster
target_sc_map <- banc.meta %>%
  dplyr::distinct(target = cell_type, super_cluster)

# Select targets above the sensory-median threshold and attach super_cluster
selected_targets <- dens_max %>%
  dplyr::filter(influence_norm_log_max >= threshold.inf.value) %>%
  dplyr::left_join(target_sc_map, by = "target") %>%
  dplyr::mutate(super_cluster = dplyr::coalesce(super_cluster, "unlabeled")) %>%
  dplyr::filter(super_cluster != "unlabeled")

# Count super_clusters within each seed_class
mix_counts_sel <- selected_targets %>%
  dplyr::distinct(seed_class, target, super_cluster) %>%
  dplyr::count(seed_class, super_cluster, name = "n_cell_types")

# Totals per class (used for label placement)
mix_totals_sel <- mix_counts_sel %>%
  dplyr::group_by(seed_class) %>%
  dplyr::summarise(total = base::sum(n_cell_types), .groups = "drop")

# Order stacks *within each seed_class* from largest to smallest
mix_counts_ord <- mix_counts_sel %>%
  dplyr::group_by(seed_class) %>%
  dplyr::arrange(dplyr::desc(n_cell_types), .by_group = TRUE) %>%
  dplyr::mutate(
    order_rank = dplyr::row_number(),
    sc_key     = base::paste(seed_class, sprintf("%03d", order_rank), super_cluster, sep = "|")
  ) %>%
  dplyr::ungroup()

# Colors for the stacked bars: one color per super_cluster, mapped onto sc_key
fill_colors <- mix_counts_ord %>%
  dplyr::distinct(sc_key, super_cluster) %>%
  dplyr::mutate(col = paper.cols[super_cluster]) %>%
  { stats::setNames(.$col, .$sc_key) }

# Build per-target labels (one row per target), joined with super_cluster
top_labels_long <- dens_max %>%
  dplyr::filter(influence_norm_log_max >= threshold.inf.value,
                seed_class != "sensory") %>%
  dplyr::group_by(seed_class) %>%
  dplyr::arrange(dplyr::desc(influence_norm_log_max), .by_group = TRUE) %>%
  dplyr::slice_head(n = top_n_labels) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(target_sc_map, by = "target") %>%
  dplyr::mutate(super_cluster = dplyr::coalesce(super_cluster, "unlabeled")) %>%
  dplyr::group_by(seed_class) %>%
  dplyr::mutate(rank = dplyr::row_number()) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(mix_totals_sel, by = "seed_class")

# Compute per-row y-positions so names stack above each bar without overlap
headroom  <- 0.05 * base::max(mix_totals_sel$total, na.rm = TRUE)   # gap above bar
line_step <- 0.035 * base::max(mix_totals_sel$total, na.rm = TRUE)  # vertical spacing
top_labels_long <- top_labels_long %>%
  dplyr::mutate(y_pos = total + headroom + line_step * (rank - 1))

# Text color palette for label names (fallback for "unlabeled")
label_sc_levels <- unique(top_labels_long$super_cluster)
text_cols <- paper.cols
if (!"unlabeled" %in% names(text_cols)) text_cols <- c(text_cols, unlabeled = "grey50")
text_cols <- text_cols[label_sc_levels]

# Plot: stacked bar + one text row per target, colored by super_cluster
p_mix_bar_ord <- ggplot2::ggplot(
  mix_counts_ord,
  ggplot2::aes(x = seed_class, y = n_cell_types, fill = sc_key)
) +
  ggplot2::geom_col() +
  ggplot2::geom_hline(yintercept = 0, linewidth = 0.3, color = "grey40") +
  ggplot2::geom_text(
    data = top_labels_long,
    ggplot2::aes(x = seed_class, y = y_pos, label = target, color = super_cluster),
    inherit.aes = FALSE,
    size = 3.0, vjust = 0, lineheight = 0.95
  ) +
  ggplot2::scale_fill_manual(values = fill_colors, guide = "none") +
  ggplot2::scale_color_manual(values = text_cols, guide = "none") +
  ggplot2::labs(
    title = "",
    subtitle = paste0("stacks ordered by size within each seed class; labels show top-",
                      top_n_labels, " targets by max(log-normalized influence), colored by super_cluster"),
    x = NULL, y = "cell type count"
  ) +
  ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.02, 0.20))) +
  ggplot2::coord_cartesian(clip = "off") +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(panel.grid.minor = ggplot2::element_blank())

# Save
base::print(p_mix_bar_ord)
ggplot2::ggsave(
  filename = base::file.path(banc.fig6.extra.path, "max_influence_norm_log_by_control_class_to_ans_dns_above_sensory_median_bar_plot.pdf"),
  plot = p_mix_bar_ord,
  width = 6, height = 8, dpi = 300
)

# Plot as heatmap
# Wide matrix for pheatmap: rows = seed_class, cols = super_cluster
df_heat <- mix_counts_sel %>%
  tidyr::pivot_wider(
    names_from  = super_cluster,
    values_from = n_cell_types,
    values_fill = 0
  )

# Build numeric matrix with rownames
heat_mat <- df_heat
rowlabs <- heat_mat$seed_class
heat_mat$seed_class <- NULL
heat_mat <- as.matrix(heat_mat)
rownames(heat_mat) <- rowlabs

# Optional: order columns by total count (descending)
if (ncol(heat_mat) > 1) {
  sc_order <- names(sort(colSums(heat_mat, na.rm = TRUE), decreasing = TRUE))
  heat_mat <- heat_mat[, sc_order, drop = FALSE]
}

# ----- Color scale: 0 -> white, 1 -> grey, >=2 -> gradient grey→black -----
max_count <- max(heat_mat, narm = TRUE)
if (max_count <= 0) {
  cols   <- c("white")
  breaks <- c(-0.5, 0.5)
} else if (max_count == 1) {
  cols   <- c("white", "grey")
  breaks <- c(-0.5, 0.5, 1.5)
} else {
  breaks <- c(-0.5, 0.5, 1.5, seq(2.5, max_count + 0.5, by = 1))
  cols <- c(
    "grey",
    "white",
    grDevices::colorRampPalette(c("white", paper.cols[["highlight"]]))(length(breaks) - 3)
  )
}

# Ensure row names are shown and not clipped
row_labels <- rownames(heat_mat)
row_labels <- gsub("_"," ",row_labels)
max_lab    <- max(nchar(row_labels), na.rm = TRUE)
plot_w     <- max(6, 4 + 0.12 * max_lab)  # widen figure if labels are long

# Draw heatmap (absolute counts) with numbers and 45° column labels
pheatmap::pheatmap(
  heat_mat,
  scale            = "none",
  cluster_rows     = TRUE,
  cluster_cols     = FALSE,
  treeheight_row   = 0,
  treeheight_col   = 0,
  main             = "",
  na_col           = "white",
  color            = cols,
  breaks           = breaks,
  show_rownames    = TRUE,
  labels_row       = row_labels,
  cellheight       = 15,
  cellwidth        = 15,
  fontsize_col     = 8,
  fontsize_row     = 9,
  angle_col        = 45,                
  display_numbers  = heat_mat,         
  number_format    = "%.0f",             
  number_color     = "black",          
  fontsize_number  = 8,
  filename         = file.path(banc.fig6.path,
                               "selected_targets_super_cluster_by_seed_class_heatmap.pdf"),
  width            = plot_w,
  height           = 6
)

# Sidecar: list the AN/DN target cell types in each super_cluster that are
# above threshold for the "central_complex_output" seed_class (i.e. the cells
# of the central_complex_output row of the heatmap above).
cx_targets_by_sc <- selected_targets %>%
  dplyr::filter(seed_class == "central_complex_output") %>%
  dplyr::distinct(super_cluster, target, influence_norm_log_max) %>%
  dplyr::group_by(super_cluster, target) %>%
  dplyr::summarise(
    influence_norm_log_max = base::max(influence_norm_log_max, na.rm = TRUE),
    .groups = "drop"
  )

# Order super_clusters to match the heatmap column order (descending total),
# falling back to per-class count for any super_clusters not in the heatmap.
sc_order_cx <- if (exists("sc_order")) sc_order else {
  cx_targets_by_sc %>%
    dplyr::count(super_cluster, sort = TRUE) %>%
    dplyr::pull(super_cluster)
}
sc_order_cx <- base::intersect(sc_order_cx, base::unique(cx_targets_by_sc$super_cluster))

cx_txt_path <- file.path(banc.fig6.path,
                         "selected_targets_super_cluster_by_seed_class_heatmap.txt")
cx_txt_lines <- c(
  "Cell types above threshold for seed_class = central_complex_output",
  sprintf("Threshold (influence_norm_log): %.4f", threshold.inf.value),
  sprintf("Total cell types: %d across %d super_clusters",
          dplyr::n_distinct(cx_targets_by_sc$target),
          length(sc_order_cx)),
  ""
)
for (sc in sc_order_cx) {
  sc_rows <- cx_targets_by_sc %>%
    dplyr::filter(super_cluster == sc) %>%
    dplyr::arrange(dplyr::desc(influence_norm_log_max))
  cx_txt_lines <- c(
    cx_txt_lines,
    sprintf("## %s (n = %d)", sc, nrow(sc_rows)),
    sprintf("  %s  [%.2f]", sc_rows$target, sc_rows$influence_norm_log_max),
    ""
  )
}
base::writeLines(cx_txt_lines, cx_txt_path)
cat(sprintf("Wrote %s\n", cx_txt_path))

# Sidecar CSV: above-threshold AN/DN targets per seed_class, for the three
# upstream classes summarised in Fig 6 panel g. numbers.R reads this to
# emit `fig6g_<seed_class>_top_targets` strings (comma-joined cell types,
# ordered by influence_norm_log_max within each seed_class).
fig6g_csv_path <- file.path(banc.fig6.path,
                              "fig6g_top_targets_by_seed_class.csv")
fig6g_targets <- selected_targets %>%
  dplyr::filter(seed_class %in% c("central_complex_output",
                                    "mushroom_body_output",
                                    "visual_projection")) %>%
  dplyr::distinct(seed_class, super_cluster, target,
                   influence_norm_log_max) %>%
  dplyr::group_by(seed_class, super_cluster, target) %>%
  dplyr::summarise(
    influence_norm_log_max = base::max(influence_norm_log_max, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::arrange(seed_class, dplyr::desc(influence_norm_log_max))
readr::write_csv(fig6g_targets, fig6g_csv_path)
cat(sprintf("Wrote %s (%d rows across %d seed_classes)\n",
            fig6g_csv_path, nrow(fig6g_targets),
            dplyr::n_distinct(fig6g_targets$seed_class)))
