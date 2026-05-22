#' AN/DN presynapse polarity by region and super class (ED Fig. 5b–c)
#'
#' Quantifies how AN and DN populations distribute their presynapses
#' between the brain and the VNC, and how effector neurons receive
#' inputs by presynaptic super class. The neck-passing AN/DN polarity
#' analysis underpins ED Fig. 5b–c (input-synapse proportions onto
#' brain and VNC effectors broken down by source super class, and the
#' brain-vs-VNC presynapse distribution for the n = 1,316 DNs and
#' n = 1,849 ANs in BANC v888).
#'
#' Significance tests use a Wilcoxon rank-sum first-significant-pair
#' helper (`first_sig_pairs()`); on-plot brackets are drawn with
#' `ggpubr::stat_compare_means`.
#'
#' @section Reads:
#'   banc.meta, banc.edgelist.simple, paper.cols                          (snapshots)
#'   banc.synapses (pre/post synapse coordinates from banc-edgelist.R)
#'
#' @section Writes:
#'   figures/figure_2/links/supplement/extended_data_fig_5b_*.pdf
#'   figures/figure_2/links/supplement/extended_data_fig_5c_*.pdf
#'   figures/figure_2/links/extra/...                                      (exploratory polarity plots)
#'   figures/figure_2/links/*.txt                                          (Wilcoxon summaries)
#'
#' @section Paper:
#'   ED Fig. 5b — proportion of input synapses onto brain vs VNC effectors
#'                broken down by presynaptic super class (visceral top,
#'                motor bottom).
#'   ED Fig. 5c — brain-vs-VNC presynapse distribution for AN/DN populations
#'                (n_DN = 1,316, n_AN = 1,849).
#'   Methods §"Annotation taxonomy", §"Synapse detection" (v2).
#'
#' @section Used by:
#'   R/text/numbers.R (dn_neuron_count, an_neuron_count).
#'
#' @section Reproduce:
#'   BANC_NCORES=1 Rscript R/figures/panels_an_dn_polarity.R

###############
### STARTUP ###
###############

# load
source("R/startup/banc-startup.R")
source("R/startup/banc-meta.R")
source("R/startup/banc-edgelist.R")

######################
### Neuron calibre ###
######################

# Helper: for each level of an ordered factor, find the FIRST significantly
# different level to its right and to its left (Wilcoxon test, p < alpha).
# Returns a deduplicated list of pairs suitable for the `comparisons` arg of
# ggpubr::stat_compare_means.
first_sig_pairs <- function(df, x_col, y_col, levels, alpha = 0.05) {
  n <- length(levels)
  pairs <- list()
  pair_keys <- character(0)
  add_pair <- function(a, b) {
    key <- paste(sort(c(a, b)), collapse = "||")
    if (!(key %in% pair_keys)) {
      pairs[[length(pairs) + 1L]] <<- c(a, b)
      pair_keys <<- c(pair_keys, key)
    }
  }
  vals_for <- function(lvl) {
    v <- df[df[[x_col]] == lvl, ][[y_col]]
    v[!is.na(v)]
  }
  test_p <- function(a, b) {
    if (length(a) < 2 || length(b) < 2) return(NA_real_)
    tryCatch(
      suppressWarnings(stats::wilcox.test(a, b)$p.value),
      error = function(e) NA_real_
    )
  }
  for (i in seq_len(n)) {
    yi <- vals_for(levels[i])
    if (length(yi) < 2) next
    # Scan right
    for (j in seq_len(n)) {
      if (j <= i) next
      p <- test_p(yi, vals_for(levels[j]))
      if (!is.na(p) && p < alpha) {
        add_pair(levels[i], levels[j])
        break
      }
    }
    # Scan left
    for (j in rev(seq_len(n))) {
      if (j >= i) next
      p <- test_p(yi, vals_for(levels[j]))
      if (!is.na(p) && p < alpha) {
        add_pair(levels[j], levels[i])
        break
      }
    }
  }
  pairs
}

### overview
pd.width.plot <- banc.meta %>%
  dplyr::filter(super_class != "sensory", 
                !is.na(pd_width)) %>%
  dplyr::mutate(group = dplyr::case_when(
    grepl("descending",super_class) ~ "descending",
    grepl("ascending",super_class) ~ "ascending",
    grepl("brain",region) ~ "brain",
    grepl("ventral_nerve_cord|vnc",region) ~ "vnc",
    grepl("optic",region) ~ "optic"
  )) %>%
  dplyr::distinct(root_id, group, pd_width)

### DNs — violin by super_cluster

# Wrangle
dn.pd.width.plot <- banc.meta %>%
  dplyr::filter(grepl("descending", super_class),
                !is.na(pd_width),
                !is.na(super_cluster)) %>%
  dplyr::distinct(root_id, super_cluster, pd_width)

# Order super_clusters by median pd_width (ascending)
sc_order <- dn.pd.width.plot %>%
  dplyr::group_by(super_cluster) %>%
  dplyr::summarise(median_pd_width = median(pd_width, na.rm = TRUE)) %>%
  dplyr::arrange(median_pd_width) %>%
  dplyr::pull(super_cluster)
dn.pd.width.plot <- dn.pd.width.plot %>%
  dplyr::mutate(super_cluster = factor(super_cluster, levels = sc_order))

# For each violin, find the first significantly different neighbour to the
# right and to the left (Wilcoxon, p < 0.05). Skip the layer if none.
dn_sig_pairs <- first_sig_pairs(dn.pd.width.plot, "super_cluster", "pd_width", sc_order)
dn_sig_layer <- if (length(dn_sig_pairs) > 0) {
  ggpubr::stat_compare_means(comparisons = dn_sig_pairs,
                             method = "wilcox.test",
                             hide.ns = TRUE,
                             size = 2.5,
                             label = "p.signif",
                             step.increase = 0.03)
} else NULL

# Violin plot with first-significant-neighbour Wilcoxon comparisons
g.dn.pd.width <- ggplot(dn.pd.width.plot, aes(x = super_cluster, y = pd_width, fill = super_cluster)) +
  geom_violin(color = NA, alpha = 0.75, scale = "width") +
  geom_boxplot(width = 0.15, outlier.size = 0.5, fill = NA) +
  scale_fill_manual(values = paper.cols) +
  dn_sig_layer +
  labs(x = "", y = "primary dendrite width") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 14),
    panel.grid.major.x = element_blank()
  )

# Print & save
print(g.dn.pd.width)
ggsave(plot = g.dn.pd.width,
       filename = file.path(banc.fig3.extra.path, "descending_primary_dendrite_width_by_cluster.pdf"),
       width = 12, height = 4.5, dpi = 300)

### ANs — violin by super_cluster

# Wrangle
an.pd.width.plot <- banc.meta %>%
  dplyr::filter(grepl("ascending", super_class),
                !is.na(pd_width),
                !is.na(super_cluster)) %>%
  dplyr::distinct(root_id, super_cluster, pd_width)

# Order super_clusters by median pd_width (ascending)
sc_order <- an.pd.width.plot %>%
  dplyr::group_by(super_cluster) %>%
  dplyr::summarise(median_pd_width = median(pd_width, na.rm = TRUE)) %>%
  dplyr::arrange(median_pd_width) %>%
  dplyr::pull(super_cluster)
an.pd.width.plot <- an.pd.width.plot %>%
  dplyr::mutate(super_cluster = factor(super_cluster, levels = sc_order))

# First significantly different neighbour right + left per violin (Wilcoxon).
an_sig_pairs <- first_sig_pairs(an.pd.width.plot, "super_cluster", "pd_width", sc_order)
an_sig_layer <- if (length(an_sig_pairs) > 0) {
  ggpubr::stat_compare_means(comparisons = an_sig_pairs,
                             method = "wilcox.test",
                             hide.ns = TRUE,
                             size = 2.5,
                             label = "p.signif",
                             step.increase = 0.03)
} else NULL

# Violin plot with first-significant-neighbour Wilcoxon comparisons
g.an.pd.width <- ggplot(an.pd.width.plot, aes(x = super_cluster, y = pd_width, fill = super_cluster)) +
  geom_violin(color = NA, alpha = 0.75, scale = "width") +
  geom_boxplot(width = 0.15, outlier.size = 0.5, fill = NA) +
  scale_fill_manual(values = paper.cols) +
  an_sig_layer +
  labs(x = "", y = "primary dendrite width") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 14),
    panel.grid.major.x = element_blank()
  )

# Print & save
print(g.an.pd.width)
ggsave(plot = g.an.pd.width,
       filename = file.path(banc.fig3.extra.path, "ascending_primary_dendrite_width_by_cluster.pdf"),
       width = 12, height = 4.5, dpi = 300)

### pd_width density: ascending vs descending
an.dn.pd <- banc.meta %>%
  dplyr::filter(super_class %in% c("ascending", "descending"),
                !is.na(pd_width)) %>%
  dplyr::distinct(root_id, super_class, pd_width)

g.pd.density <- ggplot(an.dn.pd, aes(x = pd_width, fill = super_class, color = super_class)) +
  geom_density(alpha = 0.4) +
  scale_fill_manual(values = paper.cols) +
  scale_color_manual(values = paper.cols) +
  labs(x = "primary dendrite width", y = "density") +
  theme_minimal() +
  theme(legend.position = "top")

ggsave(g.pd.density,
       filename = file.path(banc.fig2.extra.path, "neck_neuron_pd_width_density_ascending_vs_descending.pdf"),
       width = 6, height = 4, dpi = 300)

### pd_width vs cell type size (number of neurons per cell_type)
for (sc in c("ascending", "descending")) {
  ct.sizes <- banc.meta %>%
    dplyr::filter(super_class == sc, !is.na(pd_width), !is.na(cell_type)) %>%
    dplyr::group_by(cell_type) %>%
    dplyr::mutate(n_neurons = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(root_id, .keep_all = TRUE)

  g.ct.pd <- ggplot(ct.sizes, aes(x = factor(n_neurons), y = pd_width)) +
    geom_violin(fill = "grey80", color = NA, alpha = 0.75, scale = "width") +
    geom_boxplot(width = 0.15, outlier.size = 0.5) +
    labs(x = "neurons per cell type", y = "primary dendrite width",
         title = gsub("^(\\w)", "\\U\\1", sc, perl = TRUE)) +
    theme_minimal() +
    theme(panel.grid.major.x = element_blank())

  ggsave(g.ct.pd,
         filename = file.path(banc.fig2.extra.path,
                              paste0("neck_neuron_pd_width_vs_celltype_size_", sc, ".pdf")),
         width = 8, height = 5, dpi = 300)
}

### Neurons per cell type, per super_cluster (ascending and descending separately)
for (sc in c("ascending", "descending")) {
  ct.per.sc <- banc.meta %>%
    dplyr::filter(super_class == sc, !is.na(cell_type), !is.na(super_cluster)) %>%
    dplyr::group_by(super_cluster, cell_type) %>%
    dplyr::summarise(n_neurons = dplyr::n(), .groups = "drop")

  # Order super_clusters by median cell type size
  sc_order <- ct.per.sc %>%
    dplyr::group_by(super_cluster) %>%
    dplyr::summarise(med = median(n_neurons)) %>%
    dplyr::arrange(med) %>%
    dplyr::pull(super_cluster)
  ct.per.sc <- ct.per.sc %>%
    dplyr::mutate(super_cluster = factor(super_cluster, levels = sc_order))

  g.ct.count <- ggplot(ct.per.sc, aes(x = super_cluster, y = n_neurons, fill = super_cluster)) +
    geom_violin(color = NA, alpha = 0.75, scale = "width") +
    geom_boxplot(width = 0.15, outlier.size = 0.5, fill = NA) +
    scale_fill_manual(values = paper.cols) +
    labs(x = "", y = "neurons per cell type",
         title = gsub("^(\\w)", "\\U\\1", sc, perl = TRUE)) +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
      panel.grid.major.x = element_blank()
    )

  ggsave(g.ct.count,
         filename = file.path(banc.fig2.extra.path,
                              paste0("neck_neuron_celltype_count_by_super_cluster_", sc, ".pdf")),
         width = 12, height = 5, dpi = 300)
}

##################
### SIDE INDEX ###
##################

# Show histogram of fow centrality
banc.plot.meta <- banc.meta %>%
  dplyr::filter(super_class %in% c("ascending", "descending"),
                !is.na(super_cluster),
                !is.na(output_side_index),
                output_side_index!=0&input_side_index!=0) %>%
  dplyr::mutate(
    contralaterality = 1-abs(input_side_index+output_side_index)
  )

# Plot contralaterality index
g.bilat <- ggplot(banc.plot.meta, aes(x = contralaterality, color = super_class)) +
  geom_density(size = 1.2, position = "identity", na.rm = TRUE) +
  facet_wrap(~super_cluster, scales = "free_y") +
  scale_color_manual(values = paper.cols)  +
  labs(
    x = "(-1) fully ipsilateral to fully contralateral (1)",
    y = "density",
    color = "super class"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    plot.title = element_text(hjust = 0.5, size = 14),
    strip.text = element_text(size = 18)
  ) +
  theme(legend.position = "none",
        legend.title = element_blank())

# Show
print(g.bilat)

# Save
ggsave(g.bilat, 
       filename = file.path(banc.fig3.supp.path, "neck_neuron_contralaterality_index_super_cluster.pdf"), 
       width = 14, height = 7, dpi = 300)

#########################
### SEGREGATION INDEX ###
#########################

# Show histogram of flow centrality
banc.plot.meta <- banc.meta %>%
  dplyr::filter(super_class %in% c("ascending", "descending"),
                !is.na(segregation_index),
                segregation_index !=0) %>%
  dplyr::group_by(cell_type, super_class) %>%
  dplyr::summarise(segregation_index = mean(segregation_index, na.rm = TRUE))

# Plot segregation index
g.si <- ggplot(banc.plot.meta, aes(x = segregation_index, 
                           color = super_class)) +
  geom_density(size = 1.2, na.rm = TRUE) +
  scale_color_manual(values = paper.cols) +
  labs(
    x = "segregation index",
    y = "density",
    color = "super class"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    plot.title = element_text(hjust = 0.5, size = 14)
  ) +
  theme(legend.position = "none")

# Show
print(g.si)

# Save
ggsave(g.si,
       filename = file.path(banc.fig2.supp.path, "neck_neuron_segregation_index_super_class.pdf"),
       width = 6, height = 3, dpi = 300)

### Segregation index violin by super_cluster
si.sc.plot <- banc.meta %>%
  dplyr::filter(super_class %in% c("ascending", "descending"),
                !is.na(segregation_index),
                segregation_index != 0,
                !is.na(super_cluster))

# Order super_clusters by median segregation_index (ascending)
sc_order_si <- si.sc.plot %>%
  dplyr::group_by(super_cluster) %>%
  dplyr::summarise(median_si = median(segregation_index, na.rm = TRUE)) %>%
  dplyr::arrange(median_si) %>%
  dplyr::pull(super_cluster)
si.sc.plot <- si.sc.plot %>%
  dplyr::mutate(super_cluster = factor(super_cluster, levels = sc_order_si))

# Adjacent pairwise comparisons
si_adjacent_pairs <- lapply(seq_len(length(sc_order_si) - 1), function(i) sc_order_si[c(i, i + 1)])

g.si.sc <- ggplot(si.sc.plot, aes(x = super_cluster, y = segregation_index, fill = super_cluster)) +
  geom_violin(color = NA, alpha = 0.75, scale = "width") +
  geom_boxplot(width = 0.15, outlier.size = 0.5, fill = NA) +
  scale_fill_manual(values = paper.cols) +
  ggpubr::stat_compare_means(comparisons = si_adjacent_pairs,
                              method = "wilcox.test",
                              hide.ns = TRUE,
                              size = 2.5,
                              label = "p.signif",
                              step.increase = 0.03) +
  labs(x = "", y = "segregation index") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 14),
    panel.grid.major.x = element_blank()
  )

# Print & save
print(g.si.sc)
ggsave(g.si.sc,
       filename = file.path(banc.fig3.supp.path, "neck_neuron_segregation_index_super_cluster.pdf"),
       width = 12, height = 6, dpi = 300)

#####################
### SYNAPSE COUNT ###
#####################

banc.an.dn <- banc.neck.meta %>%
  dplyr::filter(super_class %in% c("ascending",
                                   "descending"))

# Use edgelist to get output synapse counts by region (brain vs VNC)
# instead of slow per-neuron CAVE banc_partners() calls
neck.syn.by.region <- banc.edgelist.simple %>%
  dplyr::filter(pre %in% banc.an.dn$root_id) %>%
  dplyr::mutate(region = dplyr::case_when(
    post_region %in% c("central_brain", "optic_lobe") ~ "brain",
    post_region == "ventral_nerve_cord" ~ "ventral_nerve_cord",
    TRUE ~ NA_character_
  )) %>%
  dplyr::filter(!is.na(region))

# Get per cell_type/count totals
ct_totals <- neck.syn.by.region %>%
  dplyr::left_join(
    banc.an.dn %>%
      dplyr::select(root_id, super_class, cell_type, neurotransmitter),
    by = c("pre" = "root_id")
  ) %>%
  dplyr::group_by(super_class, cell_type) %>%
  dplyr::summarise(total_count = sum(count, na.rm = TRUE), .groups = "drop")

# Get per cell_type, region counts
region_counts <- neck.syn.by.region %>%
  dplyr::left_join(
    banc.an.dn %>%
      dplyr::select(root_id, super_class, cell_type, neurotransmitter),
    by = c("pre" = "root_id")
  ) %>%
  dplyr::group_by(super_class, cell_type, region) %>%
  dplyr::summarise(region_count = sum(count, na.rm = TRUE), .groups = "drop")

# Join and compute normalized column
neck.synapses.plot <- region_counts %>%
  dplyr::left_join(
    ct_totals,
    by = c("super_class", "cell_type")
  ) %>%
  dplyr::filter(!is.na(super_class)) %>%
  dplyr::mutate(norm = region_count / total_count) %>%
  dplyr::arrange(cell_type, region)
  
# Calculate median and IQR per region and super_class
summary_df <- neck.synapses.plot %>%
  dplyr::group_by(super_class, region) %>%
  dplyr::summarise(
    med = median(region_count, na.rm = TRUE),
    q1 = quantile(region_count, 0.25, na.rm = TRUE),
    q3 = quantile(region_count, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

g.syn.regions <- ggplot(neck.synapses.plot, 
                        aes(x = region, y = region_count)) +
  # Paired lines
  geom_line(aes(group = cell_type), color = "darkgrey", size = 0.7) +
  # Violin
  geom_violin(aes(fill = region), color = NA, alpha = 0.75, width = 0.8, trim = FALSE) +
  # Jittered points
  #geom_jitter(aes(color = region), width = 0.1, size = 1) +
  # IQR bars
  geom_errorbar(data = summary_df,
                aes(x = region, ymin = q1, ymax = q3),
                width = 0.3, color = "black", inherit.aes = FALSE) +
  # Median points
  geom_point(data = summary_df,
             aes(x = region, y = med),
             color = "black", size = 3, shape = 95, inherit.aes = FALSE) +
  facet_wrap(~super_class) +
  labs(
    x = "",
    y = "presynaptic count",
    color = "region",
    fill = "region"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 8),
    plot.title = element_text(hjust = 0.5, size = 14)
  ) +
  scale_fill_manual(values = paper.cols) +
  scale_color_manual(values = paper.cols) +
  scale_y_continuous(trans = "log10") +
  theme(legend.position = "none")

# Show
print(g.syn.regions)

# Save
ggsave(g.syn.regions, 
       filename = file.path(banc.fig2.extra.path, "neck_neuron_presynapes_by_super_class.pdf"), 
       width = 8, height = 4, dpi = 300)


# Plot segregation index
g.syn.dens <- ggplot(neck.synapses.plot %>% 
                 dplyr::filter(region=="ventral_nerve_cord"), 
               aes(x = norm, 
                   color = super_class)) +
  geom_density(size = 1.2, na.rm = TRUE) +
  scale_color_manual(values = paper.cols) +
  labs(
    x = "proportion of presynapses in the ventral nerve cord",
    y = "density",
    color = "super_class"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    plot.title = element_text(hjust = 0.5, size = 14)
  ) +
  theme(legend.position = "none")

# Show
print(g.syn.dens)

# Save
ggsave(g.syn.dens, 
       filename = file.path(banc.fig2.supp.path, "neck_neuron_presynapse_proportion_in_vnc.pdf"), 
       width = 6, height = 3, dpi = 300, bg = "transparent")

#####################################
### INFLUENCE TO EFFECTOR SCATTER ###
#####################################
# new meta
banc.eff2.meta <- banc.eff.meta %>%
  dplyr::mutate(body_part_effector = dplyr::case_when(
    grepl("power|steering|tension",cell_function) ~ cell_function,
    grepl("pitch|yaw|roll",cell_function_detailed) ~ cell_function_detailed,
    TRUE ~ body_part_effector
  ))

# Get alternative dataset for validation (seed_02)
banc.an.dn.meta <- banc.meta %>%
  dplyr::filter(super_class %in% c("ascending","descending")) %>%
  dplyr::filter(!grepl("^SA|^SN|^AN_4|^AN_5",cell_type))
chosen.seeds <- unique(banc.an.dn.meta$seed_07)
influence.nn.eff.db <- query_influence(
    levels = "seed_07", seeds = chosen.seeds,
    ids = banc.eff.meta$id, normalize = FALSE
  )

# Organise
influence.plot.df <- influence.nn.eff.db %>%
  dplyr::left_join(banc.an.dn.meta %>%
                     dplyr::select(seed_07, seed_super_class = super_class) %>%
                     dplyr::distinct(seed_07, .keep_all = TRUE),
                   by = c("seed"="seed_07")) %>%
  dplyr::left_join(banc.eff2.meta %>%
                     dplyr::select(id, body_part_effector) %>%
                     dplyr::distinct(id, .keep_all = TRUE),
                   by = c("id")) %>%
  dplyr::mutate(target = id,
                seed = seed_super_class) %>%
  calculate_influence_norms()

# Pivot
influence.plot.wide <- influence.plot.df %>% 
  dplyr::filter(seed %in% c("ascending", "descending")) %>%
  dplyr::select(id, 
                seed, 
                influence_log, 
                body_part_effector) %>%
  tidyr::pivot_wider(names_from = seed, values_from = influence_log,
              names_prefix = "influence_log_") %>%
  dplyr::filter(influence_log_ascending>threshold.inf.value,
                influence_log_descending>threshold.inf.value) %>%
  dplyr::arrange(influence_log_ascending)

# Coordinate coloursand shapes
body.part.shapes <- c("retrocerebral complex" = 21, 
                      "corpus allatum" = 24,
                      "enteric complex" = 23, 
                      "digestive tract" = 21, 
                      "crop" = 21, 
                      "salivary gland" = 24, 
                      "pharynx" = 23, 
                      "proboscis" = 21, 
                      "antenna" = 21, 
                      "eye" = 24, 
                      "neck" = 23, 
                      "haltere" = 21, 
                      "wing" = 21, 
                      "front leg" = 24,
                      "middle leg" = 23, 
                      "hind leg" = 21,
                      "ureter" = 21, 
                      "abdomen" = 24, 
                      "reproductive tract" = 21, 
                      "uterus" = 21, 
                      "neurohemal complex" = 21,
                      "haltere power" = 3,
                      "haltere steering" = 4,
                      "wing power" = 3,
                      "wing steering"= 4,
                      "wing tension" = 8,
                      "neck yaw" = 3,
                      "neck pitch" = 4,
                      "neck roll" = 8,
                      "thoracic abdominal segmental" = 21
)
body.parts <- names(body.part.shapes)
paper.cols <- c(paper.cols,
                `haltere power` = paper.cols[["haltere"]],
                `haltere steering` = paper.cols[["haltere"]],
                `wing power` = paper.cols[["wing"]],
                `wing steering`= paper.cols[["wing"]],
                `wing tension` = paper.cols[["wing"]],
                `neck yaw` = paper.cols[["neck"]],
                `neck pitch` = paper.cols[["neck"]],
                `neck roll` = paper.cols[["neck"]])
paper.cols <- paper.cols[!duplicated(names(paper.cols))]
influence.plot.wide$body_part_effector <- gsub("_"," ",influence.plot.wide$body_part_effector)
influence.plot.wide$body_part_effector <- factor(influence.plot.wide$body_part_effector, levels = body.parts)

# Plot
g.inf.an.dn.corr <- ggplot(influence.plot.wide, 
       aes(x = influence_log_ascending, 
           y = influence_log_descending,
           #color = body_part_effector,
           #shape = body_part_effector,
           group = 1)) +
  geom_point(color = "lightgrey") +
  # geom_smooth(aes(color = NULL, group = 1),
  #             formula = 'y ~ x', 
  #             method = "lm", 
  #             se = FALSE, 
  #             color = "black", 
  #             linetype = "dashed") +
  # stat_poly_eq(aes(color = NULL, 
  #                  label = paste(after_stat(eq.label), 
  #                                after_stat(rr.label), sep = "~~~")),
  #              formula = y ~ x, 
  #              parse = TRUE, 
  #              label.x = 0.2, 
  #              label.y = 0.1, 
  #              fontface = "bold",
  #              size =5) +
  geom_abline(slope = 1, 
              intercept = 0, 
              color = "black", 
              linetype = "solid",
              linewidth = 1) +
  theme_minimal() +
  labs(
    x = "influence_log_norm (ascending)",
    y = "influence_log_norm (descending)",
    title = ""
  ) +
  #scale_color_manual(values = paper.cols) +
  #scale_shape_manual(values = body.part.shapes) +
  theme_minimal() + 
  #coord_equal() +
  theme(legend.position = "none")  +
  #ggplot2::coord_fixed() + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  ylim(c(17.5,25)) +
  xlim(c(17.5,25))

# Show
plot(g.inf.an.dn.corr)

# Save
ggsave(g.inf.an.dn.corr, 
       filename = file.path(banc.fig3.path, "influence_log_ascending_descending_to_efferent_scatter.pdf"),
       width = 5, height = 3, dpi = 300, bg = "transparent")


