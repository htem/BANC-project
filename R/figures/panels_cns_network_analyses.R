#' CNS-network adjacency, transit, and influence analyses (Fig. 6b–e, ED Fig. 10d–g)
#'
#' Owns the heavy downstream analyses on top of the spectral-clustering
#' partitioning (input from `panels_cns_networks.R`):
#'
#'   - network → network direct adjacency matrix (Fig. 6d, ED Fig. 10e).
#'   - network → effector-group influence (Fig. 6b).
#'   - sensory → network influence (ED Fig. 10d).
#'   - MB-output / CX-output → cluster influence (ED Fig. 10f);
#'     cluster → MB-input / CX-input influence (ED Fig. 10g).
#'   - neck-connective transits (which sequences of clusters does an
#'     AN/DN path through the AN-DN-only subgraph traverse?).
#'   - out-of-network partner proportions (Fig. 6e KS tests).
#'
#' Renamed from `panel_super_clusters.R` on 2026-05-18 when the
#' lightweight CNS-network plots were split into `panels_cns_networks.R`.
#'
#' @section Reads:
#'   banc.meta, banc.edgelist.simple, banc.eff.meta, paper.cols
#'   .banc_spectral_csv  (cns_network labels)
#'   data/banc_annotations/v888/banc_neck_functional_classes.csv                                    (cluster labels)
#'   data/influence/.../<network_or_cluster>_influence.csv                    (cached per-seed)
#'
#' @section Writes:
#'   figures/figure_6/links/cns_network_to_cns_network.pdf                    (Fig. 6d)
#'   figures/figure_6/links/major_cns_network_output_to_efferent_*.pdf        (Fig. 6b)
#'   figures/figure_6/links/cns_network_out_of_network_*.pdf                  (Fig. 6e)
#'   figures/figure_6/links/supplement/extended_data_fig_10d_*.pdf            (ED Fig. 10d)
#'   figures/figure_6/links/supplement/extended_data_fig_10e_*.pdf            (ED Fig. 10e)
#'   figures/figure_6/links/supplement/mb_cx_to_neck_cluster_*.pdf            (ED Fig. 10f)
#'   figures/figure_6/links/supplement/neck_cluster_to_mb_cx_*.pdf            (ED Fig. 10g)
#'   figures/figure_6/links/*.txt                                              (two-way ANOVA, KS, binomial)
#'
#' @section Paper:
#'   Fig. 6b — adjusted influence of each network on each effector group;
#'             source×target ANOVA Type-III, n = 522,148 obs.
#'   Fig. 6d — CNS-network × CNS-network adjacency matrix (color cap 2,000).
#'   Fig. 6e — KS tests on out-of-network partner proportions for ANs/DNs vs. other.
#'   ED Fig. 10d–g — influence sweeps to / from CNS networks.
#'   Methods §"Spectral clustering" + §"Naming CNS networks".
#'
#' @section Schema:
#'   The 13-network labels in `banc.meta$cns_network` are the canonical
#'   v888 partitioning; do not regenerate locally — re-read from
#'   `.banc_spectral_csv` so all figures stay in sync.
#'
#' @section Reproduce:
#'   BANC_NCORES=1 Rscript R/figures/panels_cns_network_analyses.R

####################
## STARTUP        ##
####################

source("R/startup/banc-startup.R")
source("R/startup/banc-meta.R")
source("R/startup/banc-functions.R")
source("R/startup/banc-edgelist.R")


# NOTE: The light CNS-network analyses (UMAP, super_class bars, neck-
# super_cluster heatmaps) used to live here in lines 18-368. They moved to
# panel_cns_networks.R on 2026-05-17 so a fast supplement-figure refresh
# doesn't have to re-run the multi-hour transit + influence analysis below.


######################
## TRANSIT ANALYSIS ##
######################

# Define AN/DN meta if not already present
if (!exists("banc.an.dn.meta")) {
  banc.an.dn.meta <- banc.meta %>%
    dplyr::filter(super_class %in% c("ascending", "descending")) %>%
    dplyr::filter(!grepl("^SA|^SN|^AN_4|AN_5|^IN", cell_type))
}

# Analyse how neurons connect within vs between CNS networks
cluster.elist <- banc.edgelist.simple %>%
  dplyr::filter(pre %in% banc.an.dn.meta$id | post %in% banc.an.dn.meta$id) %>%
  dplyr::select(pre, post, count, norm) %>%
  dplyr::left_join(banc.meta.post %>%
                     dplyr::select(post = post_id,
                                   post_cns_network,
                                   post_cluster,
                                   post_super_cluster),
                   by ="post") %>%
  dplyr::left_join(banc.meta.pre %>%
                     dplyr::select(pre = pre_id,
                                   pre_cns_network,
                                   pre_cluster,
                                   pre_super_cluster),
                   by ="pre") %>%
  dplyr::filter(!is.na(pre_cluster)|!is.na(post_cluster)|!is.na(pre_cns_network)|!is.na(post_cns_network)) %>%
  dplyr::distinct(post, pre, 
                  pre_cns_network, pre_cluster, pre_super_cluster,
                  post_cns_network, post_cluster, post_super_cluster,
                  count)

# Calculate output connectivity patterns for super clusters
cluster.elist.out <- cluster.elist %>%
  dplyr::filter(!is.na(pre_super_cluster),
                count >= 3) %>%
  dplyr::group_by(pre_super_cluster) %>%
  dplyr::filter(!post %in% banc.an.dn.meta$id, 
                !is.na(post_cns_network)) %>%
  dplyr::group_by(pre, pre_super_cluster, post_cns_network) %>%
  dplyr::summarise(partners = n(), .groups = "drop") %>%
  dplyr::ungroup() %>%
  dplyr::group_by(pre_super_cluster, post_cns_network) %>%
  dplyr::summarise(total_count = sum(partners, na.rm = TRUE), .groups = "drop") %>%
  dplyr::ungroup()
  
# Calculate input connectivity patterns for super clusters
cluster.elist.in <- cluster.elist %>%
  dplyr::filter(!is.na(post_super_cluster),
                count >= 3) %>%
  dplyr::group_by(post_super_cluster) %>%
  dplyr::filter(!pre %in% banc.an.dn.meta$id, 
                !is.na(pre_cns_network)) %>%
  dplyr::group_by(post, post_super_cluster, pre_cns_network) %>%
  dplyr::summarise(partners = n(), .groups = "drop") %>%
  dplyr::ungroup() %>%
  dplyr::group_by(post_super_cluster, pre_cns_network) %>%
  dplyr::summarise(total_count = sum(partners, na.rm = TRUE), .groups = "drop")

# OUTPUT: super_cluster -> cns_network
out_prop <- cluster.elist.out %>%
  group_by(pre_super_cluster) %>%
  mutate(prop_output = total_count / sum(total_count)) %>%
  select(super_cluster = pre_super_cluster, cns_network = post_cns_network, prop = prop_output) %>%
  mutate(direction = "output") %>%
  ungroup()

# INPUT: cns_network -> super_cluster
in_prop <- cluster.elist.in %>%
  group_by(post_super_cluster) %>%
  mutate(prop_input = total_count / sum(total_count)) %>%
  select(super_cluster = post_super_cluster, cns_network = pre_cns_network, prop = prop_input) %>%
  mutate(direction = "input") %>%
  ungroup()

# Count neurons in each super cluster by CNS network
super.cluster.totals <- banc.meta %>%
  dplyr::filter(super_class %in% c("ascending","descending")) %>%
  dplyr::distinct(id,.keep_all = TRUE) %>%
  dplyr::group_by(super_cluster, cns_network) %>%
  dplyr::summarise(total = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(!is.na(super_cluster)) %>%
  dplyr::group_by(super_cluster) %>%
  dplyr::mutate(total_pct = total / sum(total, na.rm = TRUE)) %>%
  dplyr::ungroup()
  
# Combine input and output connectivity data
stackdf <- bind_rows(out_prop, in_prop) %>%
  dplyr::left_join(super.cluster.totals,
                   by = c("super_cluster","cns_network")) %>%
  dplyr::mutate(total = ifelse(is.na(total),0,total))

# 1. Signed prop
stackdf <- stackdf %>%
  dplyr::mutate(prop_signed = ifelse(direction == "output", -prop, prop))

# (Optional) Make super_cluster an ordered factor
stackdf$super_cluster <- factor(stackdf$super_cluster, levels = super.clust.order)
stackdf$total_pct[is.na(stackdf$total_pct)] <- 0

# Make sure these are factors (to cover all possible levels)
stackdf$super_cluster <- as.factor(stackdf$super_cluster)
stackdf$cns_network <- as.factor(stackdf$cns_network)
stackdf$direction <- as.factor(stackdf$direction)

# Complete the grid, filling missing combos with 0 for 'prop' and 'total'
stackdf <- stackdf %>%
  tidyr::complete(
    super_cluster,
    cns_network,
    direction,
    fill = list(prop = 0, total = 0, prop_signed = 0, total_pct = 0)
  )

# Plot on the same y axis (no scaling needed)
g.stack <- ggplot(stackdf, aes(x = cns_network)) +
  geom_bar(
    aes(y = prop_signed, fill = direction), 
    stat = "identity", 
    position = "identity"
  ) +
  geom_line(
    aes(y = total_pct, group = 1), 
    color = "darkgrey", 
    size = 1
  ) +
  geom_point(
    aes(y = total_pct), 
    color = "darkgrey", 
    size = 2
  ) +
  facet_wrap(~super_cluster) +
  scale_y_continuous(
    labels = function(x) scales::percent(abs(x), accuracy = 1),
    breaks = scales::pretty_breaks(n = 8),
    limits = c(-1, 1)
  ) +
  labs(
    x = "CNS cluster",
    y = "%",
    fill = NULL,
    title = ""
  ) +
  scale_fill_manual(values = paper.cols) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 10, angle = 90, hjust = 1)
  )

# Export transit connectivity visualisation
print(g.stack)
ggsave(plot = g.stack,
       filename = file.path(banc.fig6.extra.path, "direct_connections_super_cluster_connections_with_cns_networks.pdf"),
       width = 24, 
       height = 8, 
       dpi = 300, 
       bg = "transparent")

# home analysis
home.cluster.elist.pre <- banc.edgelist.simple %>%
  dplyr::select(pre, post, count, norm) %>%
  dplyr::left_join(banc.meta.pre %>%
                     dplyr::select(pre = pre_id,
                                   pre_cell_type,
                                   pre_cell_class,
                                   pre_super_class,
                                   pre_cns_network),
                   by ="pre") %>%
  dplyr::left_join(banc.meta.post %>%
                     dplyr::select(post = post_id,
                                   post_cell_type,
                                   post_cell_class,
                                   post_super_class,
                                   post_cns_network),
                   by ="post") %>%
  dplyr::filter(!is.na(pre_cns_network)|!is.na(post_cns_network)) %>%
  dplyr::mutate(pre_type = dplyr::case_when(
    pre_super_class %in% c("ascending","descending") ~ pre_super_class,
    # grepl("central_complex_output",pre_cell_class) ~ "central_complex_output",
    # grepl("MBON",pre_cell_type) ~ "mushroom_body_output",
    # grepl("visual_projection",pre_super_class) ~ "visual_projection",
    TRUE ~ "other"
  )) %>%
  dplyr::group_by(pre) %>%
  dplyr::filter(count >= 3) %>%
  dplyr::mutate(partners = length(unique(post))) %>%
  dplyr::mutate(home = sum(pre_cns_network==post_cns_network,na.rm = TRUE),
                away = sum(pre_cns_network!=post_cns_network,na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(prop = away/(home+away)) %>%
  dplyr::distinct(type = pre_type, id = pre, home, away, prop)

# home analysis
home.cluster.elist.post <- banc.edgelist.simple %>%
  dplyr::select(pre, post, count, norm) %>%
  dplyr::left_join(banc.meta.pre %>%
                     dplyr::select(pre = pre_id,
                                   pre_cell_type,
                                   pre_cell_class,
                                   pre_super_class,
                                   pre_cns_network),
                   by ="pre") %>%
  dplyr::left_join(banc.meta.post %>%
                     dplyr::select(post = post_id,
                                   post_cell_type,
                                   post_cell_class,
                                   post_super_class,
                                   post_cns_network),
                   by ="post") %>%
  dplyr::filter(!is.na(post_cns_network)|!is.na(post_cns_network)) %>%
  dplyr::mutate(post_type = dplyr::case_when(
    post_super_class %in% c("ascending","descending") ~ post_super_class,
    # grepl("central_complex_output",post_cell_class) ~ "central_complex_output",
    # grepl("MBON",post_cell_type) ~ "mushroom_body_output",
    # grepl("visual_projection",post_super_class) ~ "visual_projection",
    TRUE ~ "other"
  )) %>%
  dplyr::group_by(post) %>%
  dplyr::filter(count >= 3) %>%
  dplyr::mutate(partners = length(unique(pre))) %>%
  dplyr::mutate(home = sum(pre_cns_network==post_cns_network,na.rm = TRUE),
                away = sum(pre_cns_network!=post_cns_network,na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(prop = away/(home+away)) %>%
  dplyr::distinct(type = post_type, id = post, home, away, prop)

# Unify
home.cluster.elist.unified <- home.cluster.elist.pre %>%
  rbind(home.cluster.elist.post) %>%
  dplyr::group_by(id) %>%
  dplyr::mutate(home = sum(home),
                away = sum(away),
                total = home+away) %>%
  dplyr::ungroup() %>%
  dplyr::filter(home!=0&away!=0) %>%
  dplyr::distinct(id, .keep_all = TRUE) %>%
  dplyr::mutate(prop = away / (home + away)) %>%
  dplyr::arrange(id)

# Plot
home.cluster.elist.unified$type <- factor(home.cluster.elist.unified$type,
                                          levels = c("other",
                                                     "ascending",
                                                     "descending",
                                                     "visual_projection",
                                                     "visual_centrifugal",
                                                     "mushroom_body_output",
                                                     "central_complex_output"))
g.transfer <- ggplot(home.cluster.elist.unified, aes(x = prop, color = type)) +
  geom_density(alpha = 0.5, adjust = 1, linewidth = 2) +
  labs(
    x = "",
    y = "density",
    color = "super class",
    title = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 1),
    axis.ticks = element_line(color = "black"),
    axis.title = element_text(color = "black"),
    axis.text = element_text(color = "black", size = 24)
  ) +
  scale_color_manual(values = paper.cols)

# Save
print(g.transfer)
ggsave(plot = g.transfer,
       filename = file.path(banc.fig6.extra.path, "neck_transits_cns_networks.pdf"),
       width = 12, 
       height = 5, 
       dpi = 300, 
       bg = "transparent")

# 1) Reorder 'type' by its median prop (descending)
type_order <- home.cluster.elist.unified %>%
  dplyr::filter(type!="other") %>%
  dplyr::group_by(type) %>%
  dplyr::summarise(median_prop = stats::median(prop, na.rm = TRUE), .groups = "drop") %>%
  dplyr::arrange(dplyr::desc(median_prop)) %>%
  dplyr::pull(type)
type_order <- c(as.character(type_order),"other")
home.cluster.elist.unified <- home.cluster.elist.unified %>%
  dplyr::mutate(type = forcats::fct_relevel(type, rev(as.character(type_order))))

# 2) KS tests: each type vs "other"
ref_vec <- home.cluster.elist.unified %>%
  dplyr::filter(type == "other") %>%
  dplyr::pull(prop)

ks_results <- home.cluster.elist.unified %>%
  dplyr::filter(type != "other") %>%
  dplyr::group_by(type) %>%
  dplyr::summarise(
    n           = dplyr::n(),
    median_prop = stats::median(prop, na.rm = TRUE),
    ks_D        = suppressWarnings(stats::ks.test(prop, ref_vec)$statistic[[1]]),
    p_value     = suppressWarnings(stats::ks.test(prop, ref_vec)$p.value),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    p_adj  = stats::p.adjust(p_value, method = "holm"),
    signif = dplyr::case_when(
      p_adj <= 1e-4 ~ "****",
      p_adj <= 1e-3 ~ "***",
      p_adj <= 1e-2 ~ "**",
      p_adj <= 5e-2 ~ "*",
      TRUE          ~ "ns"
    )
  ) %>%
  dplyr::arrange(p_adj)

# Inspect results in the console
print(ks_results)

# 3) Build violin plot (ordered) with optional significance labels.
# Colour is intentionally NOT mapped: every geom overrides it (geom_violin
# with color=NA, geom_boxplot with color="black"), so a `color = type`
# aesthetic + scale_color_manual just produces a "No shared levels" warning
# without affecting the rendered plot. Fill alone carries the group identity.
g.transfer <- ggplot2::ggplot(
  home.cluster.elist.unified,
  ggplot2::aes(x = type, y = prop, fill = type)) +
  ggplot2::geom_violin(trim = FALSE, alpha = 0.5, color = NA) +
  ggplot2::geom_boxplot(width = 0.12, outlier.shape = NA, alpha = 0.35, color = "black") +
  ggplot2::labs(
    x = "super class",
    y = "prop",
    title = ""
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    legend.position = "none",
    panel.grid.minor = ggplot2::element_blank()
  ) +
  ggplot2::scale_fill_manual(values = paper.cols) +
  ggplot2::coord_flip()

# Stats - use both KS and Wilcoxon tests
nonparam_out <- write_nonparam_summary(
  df         = home.cluster.elist.unified,
  out_path   = file.path(banc.fig6.path, "neck_transits_cns_networks_violin_nonparam_summary.txt"),
  type_col   = "type",         # grouping column
  value_col  = "prop",         # numeric column
  ref_type   = "other",        # reference group
  adjust_method = "holm",
  alpha      = 0.05,
  calculate_effect_size = TRUE
)

# Save
print(g.transfer)
ggplot2::ggsave(
  plot = g.transfer,
  filename = file.path(banc.fig6.path, "neck_transits_cns_networks_violin.pdf"),
  width = 5, height = 4, dpi = 300, bg = "transparent"
)

###############################
### HOME-AWAY CONNECTIVITY  ###
###############################

# home analysis
home.cluster.home.away.pre <- banc.edgelist.simple %>%
  dplyr::select(pre, post, count, norm) %>%
  dplyr::left_join(banc.meta.pre %>%
                     dplyr::select(pre = pre_id,
                                   pre_super_class,
                                   pre_cns_network),
                   by ="pre") %>%
  dplyr::left_join(banc.meta.post %>%
                     dplyr::select(post = post_id,
                                   post_super_class,
                                   post_cns_network),
                   by ="post") %>%
  dplyr::filter(!is.na(pre_cns_network)&!is.na(post_cns_network)) %>%
  dplyr::group_by(pre, post_cns_network) %>%
  dplyr::filter(count >= 5) %>%
  dplyr::mutate(count = sum(count, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(id = pre, count, home_cluster = pre_cns_network, away_cluster = post_cns_network) %>%
  dplyr::mutate(transit=dplyr::case_when(
    home_cluster==away_cluster ~ "home",
    home_cluster!=away_cluster ~ "away",
  ))

# And just for neck neurons
home.cluster.nn.home.away.pre <- banc.edgelist.simple %>%
  dplyr::select(pre, post, count, norm) %>%
  dplyr::left_join(banc.meta.pre %>%
                     dplyr::select(pre = pre_id,
                                   pre_super_class,
                                   pre_cns_network),
                   by ="pre") %>%
  dplyr::left_join(banc.meta.post %>%
                     dplyr::select(post = post_id,
                                   post_super_class,
                                   post_cns_network),
                   by ="post") %>%
  dplyr::filter(pre_super_class %in% c("ascending","descending")) %>%
  dplyr::filter(!is.na(pre_cns_network)&!is.na(post_cns_network)) %>%
  dplyr::group_by(pre, post_cns_network) %>%
  dplyr::filter(count >= 5) %>%
  dplyr::mutate(count = sum(count, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(id = pre, count, home_cluster = pre_cns_network, away_cluster = post_cns_network) %>%
  dplyr::mutate(transit=dplyr::case_when(
    home_cluster==away_cluster ~ "home",
    home_cluster!=away_cluster ~ "away",
  ))

# Define function to calculate connectivity matrices
cns_network_links <- function(df, 
                              partners = TRUE,
                              diagonal = FALSE, 
                              swing = FALSE){

  # Signed?
  if("pre_neurotransmitter"%in%colnames(df)){
    if(swing) {
      # Classify neurotransmitter
      df_swing <- df %>%
        dplyr::mutate(
          nt_group = dplyr::case_when(
            pre_neurotransmitter == "acetylcholine" ~ "positive",
            pre_neurotransmitter %in% c("gaba", "glutamate") ~ "negative",
            TRUE ~ "other"
          )
        ) %>%
        dplyr::filter(nt_group %in% c("positive", "negative"))
      
      # --- SWITCH: if 'count' column exists, sum counts per nt_group ---
      if("count" %in% colnames(df_swing)  & !partners) {
        df_swing_sum <- df_swing %>%
          dplyr::group_by(away = away_cluster, home = home_cluster, nt_group) %>%
          dplyr::summarise(n = sum(count, na.rm = TRUE), .groups = "drop")
        df_swing <- df_swing_sum %>%
          tidyr::pivot_wider(
            names_from = nt_group, values_from = n, values_fill = 0
          ) %>%
          dplyr::mutate(
            n_positive = positive,
            n_negative = negative,
            total = n_positive + n_negative
          )
      } else {
        # Default: count rows per group
        df_swing <- df_swing %>%
          dplyr::group_by(away = away_cluster, home = home_cluster) %>%
          dplyr::summarise(
            n_positive = sum(nt_group == "positive"),
            n_negative = sum(nt_group == "negative"),
            total = n_positive + n_negative,
            .groups = "drop"
          )
      }
      df_swing <- df_swing %>%
        dplyr::group_by(home) %>%
        dplyr::mutate(
          total_home = sum(total, na.rm = TRUE),
          percent_swing = (n_positive - n_negative) / total_home * 100
        ) %>%
        dplyr::ungroup()
      df_swing_agg <- df_swing %>%
        dplyr::group_by(away, home) %>%
        dplyr::summarise(percent_swing = mean(percent_swing, na.rm = TRUE), .groups = "drop")
      mat_swing <- tidyr::pivot_wider(
        df_swing_agg,
        names_from = home,
        values_from = percent_swing,
        values_fill = 0
      ) %>% as.data.frame()
      mat_p <- mat_swing 
    } else {
      # --- SWITCH: if 'count' column exists, sum counts, else count rows ---
      if("count" %in% colnames(df)  & !partners) {
        mat_p <- df %>%
          dplyr::mutate(nt_score = dplyr::case_when(
            pre_neurotransmitter == "acetylcholine" ~ 1 * count,
            pre_neurotransmitter %in% c("gaba", "glutamate") ~ -1 * count,
            TRUE ~ 0
          )) %>%
          dplyr::group_by(away = away_cluster, home = home_cluster) %>%
          dplyr::summarise(score = sum(nt_score, na.rm = TRUE), .groups = "drop") %>%
          tidyr::pivot_wider(names_from = home, values_from = score, values_fill = 0) %>%
          as.data.frame()
      } else {
        mat_p <- df %>%
          dplyr::mutate(nt_score = dplyr::case_when(
            pre_neurotransmitter == "acetylcholine" ~ 1,
            pre_neurotransmitter %in% c("gaba", "glutamate") ~ -1,
            TRUE ~ 0
          )) %>%
          dplyr::group_by(away = away_cluster, home = home_cluster) %>%
          dplyr::summarise(score = sum(nt_score, na.rm = TRUE), .groups = "drop") %>%
          tidyr::pivot_wider(names_from = home, values_from = score, values_fill = 0) %>%
          as.data.frame()
      }
    }
  }else{
    if ("count" %in% names(df) & !partners) {
      mat_p <- df %>%
        dplyr::group_by(away = away_cluster, home = home_cluster) %>%
        dplyr::summarise(count = sum(count, na.rm = TRUE), .groups = "drop") %>%
        tidyr::pivot_wider(names_from = home, values_from = count, values_fill = 0) %>%
        as.data.frame()
    } else {
      mat_p <- df %>%
        dplyr::count(away = away_cluster, home = home_cluster) %>%
        tidyr::pivot_wider(names_from = home, values_from = n, values_fill = 0) %>%
        as.data.frame() 
    }
  }
  
  # Clean up
  rownames(mat_p) <- mat_p$away
  mat_p <- mat_p[ , -1] 
  
  # Ensure mat_p is a data.frame matrix with correct row and column names
  all_names <- union(rownames(mat_p), colnames(mat_p))
  
  # Add missing rows with all zero (if any)
  missing_rows <- setdiff(all_names, rownames(mat_p))
  if(length(missing_rows) > 0){
    addmat <- matrix(0, nrow = length(missing_rows), ncol = ncol(mat_p),
                     dimnames = list(missing_rows, colnames(mat_p)))
    mat_p <- rbind(mat_p, addmat)
  }
  
  # Add missing columns with all zero (if any)
  missing_cols <- setdiff(all_names, colnames(mat_p))
  if(length(missing_cols) > 0){
    addmat <- matrix(0, nrow = nrow(mat_p), ncol = length(missing_cols),
                     dimnames = list(rownames(mat_p), missing_cols))
    mat_p <- cbind(mat_p, addmat)
  }
  
  # Now re-order rows and columns to match the union, for clustering
  mat_p <- mat_p[all_names, all_names]
  
  # Set diagonal (where home == away) to NA
  if(!diagonal){
    for(n in all_names) if(n %in% rownames(mat_p) && n %in% colnames(mat_p)) mat_p[n, n] <- NA
  }
  return(mat_p)
}

# DISABLED 2026-04-11: per-column range-normalisation + NA-on-diagonal.
# Replaced by the row-normalisation variant (each row sums to 1)
# applied inside banc_plot_key_features() — gives clearer "per source
# network, where does its output go" semantics for Fig. 6d. Kept for
# the column-normalised variant if needed.
# # Norm
# dimnams <- dimnames(mat_p)
# mat_p_norm <- apply(mat_p, 2, function(x) {
#   rng <- range(x, na.rm = TRUE)
#   if(diff(rng) == 0) return(rep(0, length(x)))
#   (x - rng[1]) / diff(rng)
# })
# dimnames(mat_p_norm) <- dimnams
# shared_names <- intersect(rownames(mat_p_norm), colnames(mat_p_norm))
# for(n in shared_names) mat_p_norm[n, n] <- NA

# Cluster on (say) rows, and use that order for both axes
mat_p <- cns_network_links(home.cluster.home.away.pre)
mat_n <- cns_network_links(home.cluster.home.away.pre, diagonal = TRUE)
d <- stats::dist(mat_p, method = "euclidean")
hclust_rows <- stats::hclust(d, method = "ward.D2")
reord <- hclust_rows$order
ordered_names <- rownames(mat_p)[reord]
ordered_names <- cns.network.order[cns.network.order %in% rownames(mat_p)]
mat_p <- mat_p[ordered_names, ordered_names]
mat_n <- mat_n[ordered_names, ordered_names]
mat_n[mat_n==0] <- ""

# Plot with no further clustering
pheatmap::pheatmap(
  mat_p,
  display_numbers = mat_n,
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  treeheight_row = 0, 
  treeheight_col = 0,
  main = "home (columns) to away (rows) \ncluster output partners",
  na_col = "grey",
  color = grDevices::colorRampPalette(c("white",paper.cols[["highlight"]]))(100),
  cellheight = 24,
  cellwidth = 24,
  fontsize_col = 12,
  fontsize_row = 12,
  filename = file.path(banc.fig6.extra.path,"partners_cns_network_outputs_to_cns_network.pdf")
)

# Plot for main
mat_p <- mat_p[ordered_names, ordered_names]
diag(mat_p) <- NA
pheatmap::pheatmap(
  mat_p[ordered_names, ordered_names],
  display_numbers = FALSE,
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  treeheight_row = 0, 
  treeheight_col = 0,
  main = "home (columns) to away (rows) \ncluster output partners",
  na_col = "grey",
  color = grDevices::colorRampPalette(c("white",paper.cols[["highlight"]]))(100),
  cellheight = 10,
  cellwidth = 10,
  fontsize_col = 8,
  fontsize_row = 8,
  filename = file.path(banc.fig6.path,"partners_cns_network_outputs_to_cns_network.pdf")
)

# Plot with no further clustering
mat_n <- cns_network_links(home.cluster.home.away.pre,partners=FALSE,diagonal=TRUE)[ordered_names, ordered_names]
mat_n[mat_n==0] <- ''
pheatmap::pheatmap(
  cns_network_links(home.cluster.home.away.pre,partners=FALSE)[ordered_names, ordered_names],
  display_numbers = mat_n,
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  treeheight_row = 0, 
  treeheight_col = 0,
  main = "home (columns) to away (rows) \ncluster output connections",
  na_col = "lightgrey",
  color = grDevices::colorRampPalette(c("white", highlight.col))(100),
  cellheight = 36,
  cellwidth = 36,
  fontsize_col = 12,
  fontsize_row = 12,
  filename = file.path(banc.fig6.extra.path,"connections_cns_network_outputs_to_cns_network.pdf")
)

# Plot with no further clustering
.nn_mat_full <- cns_network_links(home.cluster.nn.home.away.pre,partners=TRUE,diagonal=TRUE)
.nn_names <- ordered_names[ordered_names %in% rownames(.nn_mat_full) & ordered_names %in% colnames(.nn_mat_full)]
mat_n <- .nn_mat_full[.nn_names, .nn_names]
mat_n[mat_n==0] <- ''
pheatmap::pheatmap(
  cns_network_links(home.cluster.nn.home.away.pre)[.nn_names, .nn_names],
  display_numbers = mat_n,
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  treeheight_row = 0,
  treeheight_col = 0,
  main = "home (columns) to away (rows) \ncluster's CvC output partners",
  na_col = "lightgrey",
  color = grDevices::colorRampPalette(c("white", highlight.col))(100),
  cellheight = 20,
  cellwidth = 20,
  fontsize_col = 12,
  fontsize_row = 12,
  filename = file.path(banc.fig6.extra.path,"partners_cns_network_neck_outputs_to_cns_network.pdf")
)

# Plot with no further clustering
.nn_mat_full2 <- cns_network_links(home.cluster.nn.home.away.pre,partners=FALSE,diagonal=TRUE)
.nn_names2 <- ordered_names[ordered_names %in% rownames(.nn_mat_full2) & ordered_names %in% colnames(.nn_mat_full2)]
mat_n <- .nn_mat_full2[.nn_names2, .nn_names2]
mat_n[mat_n==0] <- ''
pheatmap::pheatmap(
  cns_network_links(home.cluster.nn.home.away.pre,partners=FALSE)[.nn_names2, .nn_names2],
  display_numbers = mat_n,
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  treeheight_row = 0,
  treeheight_col = 0,
  main = "home (columns) to away (rows) \ncluster's CvC output connections",
  na_col = "lightgrey",
  color = grDevices::colorRampPalette(c("white", highlight.col))(100),
  cellheight = 36,
  cellwidth = 36,
  fontsize_col = 12,
  fontsize_row = 12,
  filename = file.path(banc.fig6.extra.path,"connections_cns_network_neck_outputs_to_cns_network.pdf")
)

#########################
## SIGNED CONNECTIVITY ##
#########################

# Analyse excitatory vs inhibitory connectivity patterns
home.cluster.home.away.pre.signed <- banc.edgelist.simple %>%
  dplyr::select(pre, post, count, norm) %>%
  dplyr::left_join(banc.meta.pre %>%
                     dplyr::select(pre = pre_id,
                                   pre_super_class,
                                   pre_cns_network,
                                   pre_neurotransmitter),
                   by ="pre") %>%
  dplyr::left_join(banc.meta.post %>%
                     dplyr::select(post = post_id,
                                   post_super_class,
                                   post_cns_network),
                   by ="post") %>%
  dplyr::filter(!is.na(pre_cns_network)&!is.na(post_cns_network)) %>%
  dplyr::group_by(pre, pre_neurotransmitter, post_cns_network) %>%
  dplyr::filter(count >= 3) %>%
  dplyr::mutate(count = sum(count, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(id = pre, count, pre_neurotransmitter, home_cluster = pre_cns_network, away_cluster = post_cns_network) %>%
  dplyr::mutate(transit=dplyr::case_when(
    home_cluster==away_cluster ~ "home",
    home_cluster!=away_cluster ~ "away",
  ))

# home analysis for super_cluster
home.super.cluster.nn.home.away.pre.signed <- banc.edgelist.simple %>%
  dplyr::select(pre, post, count, norm) %>%
  dplyr::left_join(banc.meta.pre %>%
                     dplyr::select(pre = pre_id,
                                   pre_super_class,
                                   pre_super_cluster,
                                   pre_neurotransmitter),
                   by ="pre") %>%
  dplyr::left_join(banc.meta.post %>%
                     dplyr::select(post = post_id,
                                   post_super_class,
                                   post_super_cluster),
                   by ="post") %>%
  dplyr::filter(!is.na(pre_super_cluster)&!is.na(post_super_cluster)) %>%
  dplyr::group_by(pre, pre_neurotransmitter, post_super_cluster) %>%
  dplyr::filter(count >= 3) %>%
  dplyr::filter(post_super_class %in% c("ascending","descending"),
                pre_super_class %in% c("ascending","descending")) %>%
  dplyr::mutate(count = sum(count, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(id = pre, count, pre_neurotransmitter, home_cluster = pre_super_cluster, away_cluster = post_super_cluster) %>%
  dplyr::mutate(transit=dplyr::case_when(
    home_cluster==away_cluster ~ "home",
    home_cluster!=away_cluster ~ "away",
  ))

# Cluster on rows, and use that order for both axes
mat_p <- cns_network_links(home.cluster.home.away.pre.signed,diagonal=TRUE,partners=FALSE,swing=TRUE)
d <- stats::dist(mat_p, method = "euclidean")
hclust_rows <- stats::hclust(d, method = "ward.D2")
reord <- hclust_rows$order
ordered_names <- rownames(mat_p)[reord]
mat_p <- mat_p[ordered_names, ordered_names]

# Plot with no further clustering
pheatmap::pheatmap(
  mat_p,
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  treeheight_row = 0, 
  treeheight_col = 0,
  main = "home (columns) to away (rows)\n signed connection swing",
  na_col = "lightgrey",
  breaks = seq(-max(abs(mat_p), na.rm = TRUE), max(abs(mat_p), na.rm = TRUE), length.out = 101),
  color = grDevices::colorRampPalette(c(paper.cols[["post"]], "white",paper.cols[["pre"]]))(100),
  cellheight = 20,
  cellwidth = 20,
  fontsize_col = 12,
  fontsize_row = 12,
  filename = file.path(banc.fig6.extra.path,"connections_signed_cns_network_outputs_to_cns_network.pdf")
)

# Cluster on rows, and use that order for both axes
mat_p <- cns_network_links(home.super.cluster.nn.home.away.pre.signed,diagonal=TRUE,partners=FALSE,swing=TRUE)
d <- stats::dist(mat_p, method = "euclidean")
hclust_rows <- stats::hclust(d, method = "ward.D2")
reord <- hclust_rows$order
ordered_names <- rownames(mat_p)[reord]
mat_p <- mat_p[ordered_names, ordered_names]

# Plot with no further clustering
pheatmap::pheatmap(
  mat_p,
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  treeheight_row = 0, 
  treeheight_col = 0,
  main = "home (columns) to away (rows)\n CvC signed connection swing",
  na_col = "lightgrey",
  breaks = seq(-max(abs(mat_p), na.rm = TRUE), max(abs(mat_p), na.rm = TRUE), length.out = 101),
  color = grDevices::colorRampPalette(c(paper.cols[["post"]], "white",paper.cols[["pre"]]))(100),
  cellheight = 20,
  cellwidth = 20,
  fontsize_col = 12,
  fontsize_row = 12,
  filename = file.path(banc.fig6.extra.path,"connections_signed_swing_neck_super_cluster_outputs_to_neck_super_cluster.pdf")
)

########################
## INFLUENCE ANALYSIS ##
########################

# Analyse influence patterns between CNS networks and clusters
inf.metric <- "influence_log"

# Set up for influence calculation.
# Global `count >= 3` filter removed 2026-04-09 — see banc-edgelist.R for
# rationale. Matches the removal applied to panel_cluster_sensory_correlations.R
# and panels_cell_type_blowouts.R.
bc.meta <- banc.meta
ic_banc <- influence_calculator_py(edgelist_simple = banc.edgelist.simple,
                                   meta = bc.meta,
                                   count_thresh = 5)

# targets
banc.target.ids <- bc.meta$root_id
vpn.seeds <- bc.meta %>%
  dplyr::filter(super_class=="visual_projection") %>%
  dplyr::left_join(cns.functions %>%
                     dplyr::select(cell_type, vpn_function = response) %>%
                     dplyr::distinct(cell_type, .keep_all = TRUE),
                   by = "cell_type") %>%
  dplyr::pull(cell_type)
vpn.seeds <- unique(vpn.seeds)
control.seeds <- bc.meta %>%
  dplyr::filter(cell_class %in% c("mushroom_body_output_neuron", "central_complex_output_neuron")|
                cell_type %in% c("EPG","EL")) %>%
  dplyr::pull(cell_type)
control.seeds <- unique(control.seeds)
seeds07 <- unique(c(vpn.seeds,control.seeds))

# Get influence results for BANC
seeds <- c("seed_02","seed_07","super_cluster","cns_network")
# Uses banc_influence_loop() from banc-functions.R (PSOCK-parallel).
# One call per seed level; the helper auto-selects parallel/sequential.
.inf_results <- list()
for (seed in seeds) {
  cts <- na.omit(unique(bc.meta[[seed]]))
  if (seed == "seed_07") cts <- intersect(cts, seeds07)
  .inf_results[[seed]] <- banc_influence_loop(
    cts, seed, seed, banc.target.ids, ic = ic_banc,
    meta_df = as.data.frame(bc.meta)
  )
}
influence.cluster.orig.df <- as.data.frame(
  data.table::rbindlist(.inf_results, fill = TRUE)
)
rm(.inf_results)

# Get influence results for BANC (no-neck variant — drops AN/DN + neck neurons
# from the graph to measure what happens without the neck bottleneck).
# `count >= 3` filter removed 2026-04-09.
ic_no_neck_banc <- influence_calculator_py(edgelist_simple = banc.edgelist.simple %>%
                                     dplyr::filter(!grepl("ascending|descending",post_super_class),
                                                   !grepl("ascending|descending",pre_super_class),
                                     !post %in% banc.neck.meta$root_id,
                                     !pre %in% banc.neck.meta$root_id),
                                   meta = bc.meta,
                                   count_thresh = 5)
seeds <- c("seed_02","cns_network")
# Uses banc_influence_loop() — no-neck variant (each call uses ic_no_neck_banc).
.no_neck_results <- list()
for (seed in seeds) {
  cts <- setdiff(na.omit(unique(bc.meta[[seed]])), banc.neck.meta$cell_type)
  .no_neck_results[[seed]] <- banc_influence_loop(
    cts, seed, seed, banc.target.ids, ic = ic_no_neck_banc,
    meta_df = as.data.frame(bc.meta)
  )
}
influence.no.neck.df <- as.data.frame(
  data.table::rbindlist(.no_neck_results, fill = TRUE)
)
rm(.no_neck_results)
influence.no.neck.df <- influence.no.neck.df %>%
  dplyr::left_join(bc.meta %>%
                     dplyr::distinct(id, .keep_all = TRUE) %>%
                     dplyr::select(id, cell_type, super_class, cell_class, cell_sub_class, cluster, super_cluster, cns_network),
                   by = "id") %>%
  dplyr::mutate(
    seed = dplyr::case_when(
      is.na(seed) ~ NA,
      seed %in% names(cns.cluster.names) ~ cns.cluster.names[seed],
      TRUE ~ as.character(seed)
    )
  )

# DISABLED 2026-04-09 (v850 migration): direct SQLite query against
# influence_banc_626.sqlite. Superseded by query_influence() in
# R/startup/banc-functions.R (GCS-backed parquet/feather path).
# # Get influence meta
# con <- DBI::dbConnect(RSQLite::SQLite(),
#                       file.path(banc.dropbox.influence.save.path,"influence_banc_626.sqlite"))
# inf.banc.meta <- dplyr::tbl(con, "meta") %>%
#   dplyr::collect()
# inf.banc.meta$id <- inf.banc.meta$root_id
# dbDisconnect(con)
# 
# # Use updated banc.meta
# inf.banc.meta <- inf.banc.meta %>%
#   dplyr::distinct(id, supervoxel_id) %>%
#   dplyr::left_join(banc.meta %>%
#                      dplyr::select(-id) %>%
#                      dplyr::distinct(supervoxel_id, 
#                                      .keep_all = TRUE),
#                    by ="supervoxel_id")
# 
# # Get IDs
# banc.target.ids <- inf.banc.meta %>%
#   dplyr::filter(!is.na(cluster)|!is.na(cns_network)) %>%
#   dplyr::pull(id)
# 
# # Get chosen seeds
# vpn.seeds <- inf.banc.meta %>%
#   dplyr::filter(super_class=="visual_projection") %>%
#   dplyr::left_join(cns.functions %>%
#                      dplyr::select(cell_type, vpn_function = response) %>%
#                      dplyr::distinct(cell_type, .keep_all = TRUE),
#                    by = "cell_type") %>%
#   dplyr::pull(cell_type)
# vpn.seeds <- unique(vpn.seeds)
# control.seeds <- inf.banc.meta %>%
#   dplyr::filter(cell_class %in% c("mushroom_body_output_neuron", "central_complex_output_neuron")|
#                   cell_type %in% c("EPG","EL")) %>%
#   dplyr::pull(cell_type)
# control.seeds <- unique(control.seeds)
# seeds07 <- unique(c(vpn.seeds,control.seeds))
# 
# # Connect to .sql file
# con <- DBI::dbConnect(RSQLite::SQLite(),
#                       file.path(banc.dropbox.influence.save.path,"influence_banc_626.sqlite"))
# influence.cluster.df <- dplyr::tbl(con, "influence") %>%
#   dplyr::filter(!is_seed,
#                 level %in% c("seed_02","seed_11","seed_14")|(seed%in%!!seeds07 & level=="seed_07"),
#                 id %in% !!banc.target.ids) %>%
#   dplyr::select(seed, level, id, influence_original, influence_norm_original, influence_syn_norm) %>%
#   dplyr::collect()
# dbDisconnect(con)
# 
# Join data
influence.cluster.df <- influence.cluster.orig.df %>%
  dplyr::left_join(bc.meta %>%
                     dplyr::distinct(id, .keep_all = TRUE) %>%
                     dplyr::select(id, cell_type, super_class, cell_class, cell_sub_class, cluster, super_cluster, cns_network),
                   by = "id") %>%
  dplyr::mutate(
    seed = dplyr::case_when(
      is.na(seed) ~ NA,
      seed %in% names(cns.cluster.names) ~ cns.cluster.names[seed],
      TRUE ~ as.character(seed)
    )
  )

# Drop data frame
influence.drop.diff <- influence.cluster.df %>%
  dplyr::mutate(target = cns_network,
                seed = seed) %>%
  dplyr::filter(!is.na(seed), 
                !is.na(target),
                level %in% c("cns_network","seed_02")) %>%
  calculate_influence_norms() %>%
  dplyr::right_join(influence.no.neck.df %>%
                      dplyr::mutate(target = cns_network,
                                    seed = seed,
                                    level = "cns_network") %>%
                      dplyr::filter(!is.na(seed), 
                                    !is.na(target),
                                    level %in% c("cns_network","seed_02")) %>%
                      calculate_influence_norms() %>%
                      dplyr::distinct(target, 
                                      seed, 
                                      level, 
                                      dropped_influence_log = influence_log, 
                                      dropped_influence_norm_log = influence_norm_log),
                    by=c("target","seed","level")) %>%
  dplyr::filter(!is.na(dropped_influence_log),!is.na(dropped_influence_norm_log),
                !is.na(influence_log),!is.na(influence_norm_log)) %>%
  dplyr::mutate(drop = influence_log-dropped_influence_log,
                drop_norm = influence_norm_log-dropped_influence_norm_log)

######################
### Plot Influence ###
######################

# Analyse sensory influence on CNS networks
inf.metric <- "influence_log"
sensors.to.cns.network.key.plot <- banc_plot_key_features(
  influence.meta = influence.cluster.df %>%
    dplyr::mutate(target = cns_network) %>%
    dplyr::mutate(seed = dplyr::case_when(
      seed %in% vpn.seeds ~ cns.functions[match(seed,cns.functions$cell_type),"response"],
      TRUE ~ seed
    )) %>%
    dplyr::filter(level %in% c("seed_02","seed_07"),
                  !is.na(seed), 
                  !is.na(target),
                  seed!="0",
                  target!="0"),
  ###
  inf.metric = inf.metric,
  target.map = NULL,
  cellwidth = 12,
  cellheight = 12,
  width = 16, 
  height = 6,
  recalculate = TRUE,
  row.order = cns.network.order,
  show.annotation = FALSE,
  save.path = banc.fig6.supp.path,
  seed.map  = sensory.seed.map.detailed,
  chosen.seeds = unname(sensory.seed.map.detailed),
  chosen.targets = NULL, 
  row.cols = NULL,
  super.class = NULL,
  plot.name = sprintf("sensors_to_cns_network_%s.pdf",inf.metric),
  rev = FALSE,
  row.dend = NULL,
  col.dend = NULL,
  method = "euclidean"
)

# Primary CNS cluster outputs
cns.cluster.main.out.to.super.eff.key.plot <- banc_plot_key_features(
  influence.meta = influence.cluster.df %>%
    dplyr::filter(grepl("EFF", cluster)) %>%
    dplyr::mutate(target = super_cluster) %>%
    dplyr::filter(!is.na(target),
                  !is.na(seed),
                  target!="0",
                  target!="",
                  seed!="0",
                  seed!=""),
  ###
  inf.metric = inf.metric,
  influence.level = "cns_network",
  target.map = NULL,
  cellheight = 12,
  cellwidth = 12,
  width = 5,
  height = 5,
  col.order = cns.network.order,
  row.order = eff.super.order,
  recalculate = TRUE,
  show.annotation = FALSE,
  save.path = banc.fig6.path,
  seed.map  = NULL,
  chosen.seeds = NULL,
  chosen.targets = NULL, 
  row.cols = NULL,
  super.class = NULL,
  plot.name = sprintf("major_cns_network_output_to_efferent_super_clusters_%s.pdf",inf.metric),
  rev = FALSE,
  symmetric = FALSE,
  diagonal = TRUE
)
# Guard: the upstream banc_plot_key_features may have returned NULL (empty
# data for EFF clusters — see defensive skip message above). Only run the
# stats block when the data has ≥ 2 distinct source×target groups.
.anova_eff_df <- influence.cluster.df %>%
  dplyr::filter(grepl("EFF", cluster)) %>%
  dplyr::mutate(target = super_cluster) %>%
  dplyr::filter(!is.na(target),
                !is.na(seed),
                seed!="0",
                seed!="",
                target!="0",
                !is.infinite(adjusted_influence)) %>%
  dplyr::distinct(source=seed, target, value = adjusted_influence) %>%
  dplyr::ungroup()
if (nrow(.anova_eff_df) > 0 &&
    dplyr::n_distinct(.anova_eff_df$source) >= 2 &&
    dplyr::n_distinct(.anova_eff_df$target) >= 2) {
  write_anova_summary(.anova_eff_df,
                      file.path(banc.fig6.path,sprintf("major_cns_network_output_to_efferent_super_clusters_%s.txt",inf.metric)))
} else {
  message(sprintf("skipping write_anova_summary for EFF super_clusters: %d rows, %d sources, %d targets — need ≥2 of each",
                  nrow(.anova_eff_df),
                  dplyr::n_distinct(.anova_eff_df$source),
                  dplyr::n_distinct(.anova_eff_df$target)))
}

# Additional statistical tests
cns_to_eff_data <- influence.cluster.df %>%
  dplyr::filter(grepl("EFF", cluster)) %>%
  dplyr::mutate(target = super_cluster) %>%
  dplyr::filter(!is.na(target), !is.na(seed), seed!="0", seed!="",
                target!="0", !is.infinite(adjusted_influence))

# Guard: if the upstream plot returned NULL, the $influence.matrix won't
# exist. Build a fallback from the filtered data, or skip the test block.
influence_matrix <- if (!is.null(cns.cluster.main.out.to.super.eff.key.plot) &&
                        !is.null(cns.cluster.main.out.to.super.eff.key.plot$influence.matrix)) {
  cns.cluster.main.out.to.super.eff.key.plot$influence.matrix
} else {
  NULL
}
test_groups <- c("central complex related", "right olfactory", "left olfactory")

if (is.null(influence_matrix) || nrow(cns_to_eff_data) == 0) {
  message("skipping additional EFF super_cluster tests — influence_matrix is NULL or no data")
} else {

output_file <- file.path(banc.fig6.path,
                         sprintf("major_cns_network_output_to_efferent_super_clusters_%s_additional_tests.txt", inf.metric))
sink(output_file)

cat("CNS Network → Effector Super Clusters: Additional Tests\n")
cat("Metric:", inf.metric, "| Date:", format(Sys.time(), "%Y-%m-%d"), "\n\n")

cat("TEST 1: Specific networks have NO values above median (Exact probability)\n")
cat("------------------------------------------------------------------------\n")

# Calculate 50th percentile threshold for the entire matrix
percentile_threshold <- quantile(as.vector(influence_matrix), 0.50, na.rm = TRUE)
cat(sprintf("Matrix 50th percentile threshold: %.3f\n\n", percentile_threshold))

# Test each network and calculate probability
test_results <- data.frame()
for (group in test_groups) {
  # Check if this network exists as a column in the matrix
  if (!group %in% colnames(influence_matrix)) {
    cat(sprintf("%s: not found in matrix\n", group))
    next
  }

  # Get all influence values for this CNS network → effector connections
  network_values <- influence_matrix[, group]
  network_values <- network_values[!is.na(network_values)]

  if (length(network_values) == 0) {
    cat(sprintf("%s: no data\n", group))
    next
  }

  # Count how many effector groups have influence > 50th percentile
  n_total <- length(network_values)
  n_above <- sum(network_values > percentile_threshold)

  # Exact probability: P(all N values below median) = 0.5^N
  # This is the probability under null that each value independently has 50% chance of being above median
  p_exact <- 0.5^n_total

  test_results <- rbind(test_results, data.frame(
    network = group,
    n_total = n_total,
    n_above = n_above,
    p_exact = p_exact,
    stringsAsFactors = FALSE
  ))

  cat(sprintf("%s: %d/%d above median (expect 50%%) | P(0 above by chance)=%.3e\n",
              group, n_above, n_total, p_exact))
}

# Combined probability: all three networks showing this pattern
if (nrow(test_results) > 0) {
  # Check if all tested networks have n_above == 0
  all_zero <- all(test_results$n_above == 0)

  if (all_zero) {
    # Product of individual probabilities (assuming independence)
    p_combined <- prod(test_results$p_exact)
    sig <- ifelse(p_combined < 0.001, "***",
                  ifelse(p_combined < 0.01, "**",
                         ifelse(p_combined < 0.05, "*", "ns")))

    cat(sprintf("\nCombined: P(all %d networks have 0 above median)=%.3e %s\n",
                nrow(test_results), p_combined, sig))
    cat("Interpretation: All three tested networks lack high-influence connections\n")
  } else {
    cat("\nNote: Not all tested networks have zero values above median\n")
    cat("Networks with values above median:\n")
    for (i in 1:nrow(test_results)) {
      if (test_results$n_above[i] > 0) {
        cat(sprintf("  %s: %d/%d above median\n",
                    test_results$network[i],
                    test_results$n_above[i],
                    test_results$n_total[i]))
      }
    }
  }
}

cat("\nTEST 2: Most CNS networks have high influence on ≥1 effector group\n")
cat("-------------------------------------------------------------------\n")

# Count how many CNS networks have at least one effector connection > 50th percentile
networks_above_threshold <- apply(influence_matrix, 2, function(col) any(col > percentile_threshold, na.rm = TRUE))
n_networks_total <- length(networks_above_threshold)
n_networks_above <- sum(networks_above_threshold)
pct_networks_above <- 100 * n_networks_above / n_networks_total

# Binomial test: under null, each network has 50% chance of having ≥1 connection above median
# Test if observed proportion is significantly GREATER than 50%
binom_test <- binom.test(n_networks_above, n_networks_total, p = 0.5, alternative = "greater")
sig <- ifelse(binom_test$p.value < 0.001, "***",
              ifelse(binom_test$p.value < 0.01, "**",
                     ifelse(binom_test$p.value < 0.05, "*", "ns")))

cat(sprintf("Networks with ≥1 connection above median: %d/%d (%.1f%%)\n",
            n_networks_above, n_networks_total, pct_networks_above))
cat(sprintf("Binomial test (vs 50%% expected): p=%.3e %s | 95%% CI [%.2f, %.2f]\n",
            binom_test$p.value, sig, binom_test$conf.int[1], binom_test$conf.int[2]))

# List networks WITHOUT high influence
networks_below <- names(networks_above_threshold)[!networks_above_threshold]
if (length(networks_below) > 0) {
  cat(sprintf("\nNetworks WITHOUT high influence (n=%d):\n", length(networks_below)))
  for (network in networks_below) {
    max_inf <- max(influence_matrix[, network], na.rm = TRUE)
    cat(sprintf("  %s (max=%.3f)\n", network, max_inf))
  }
}

sink()

}  # end if (is.null(influence_matrix) ...) guard for additional EFF tests

# Analyse super cluster to super cluster influence patterns
nn.super.cluster.to.cns.cluster.key.plot <- banc_plot_key_features(
  influence.meta = influence.cluster.df %>%
    dplyr::mutate(target = cns_network,
                  seed = seed) %>%
    dplyr::filter(!is.na(seed), 
                  !is.na(target)),
  ###
  influence.level = "cns_network",
  inf.metric = inf.metric,
  target.map = NULL,
  cellwidth = 12,
  cellheight = 12,
  width = 12, 
  height = 6,
  row.order = cns.network.order,
  col.order = cns.network.order,
  recalculate = TRUE,
  row.annotation = NULL,
  show.annotation = FALSE,
  save.path = banc.fig6.supp.path,
  seed.map  = FALSE,
  chosen.seeds = NULL,
  chosen.targets = NULL, 
  row.cols = NULL,
  super.class = NULL,
  plot.name = sprintf("cns_network_to_cns_network_%s.pdf",inf.metric),
  rev = FALSE,
  row.dend = NULL,
  col.dend = NULL,
  method = "euclidean",
  symmetric = FALSE,
  diagonal = FALSE
)

# Analyse super cluster to super cluster influence patterns
nn.dropped.cns.network.to.cns.network.key.plot <- banc_plot_key_features(
  influence.meta = influence.drop.diff,
  ###
  influence.level = "cns_network",
  inf.metric = "drop",
  target.map = NULL,
  cellwidth = 12,
  cellheight = 12,
  width = 12, 
  height = 6,
  row.order = cns.network.order,
  col.order = cns.network.order,
  recalculate = FALSE,
  row.annotation = NULL,
  show.annotation = FALSE,
  save.path = banc.fig6.extra.path,
  seed.map  = FALSE,
  chosen.seeds = NULL,
  chosen.targets = NULL,
  row.cols = NULL,
  super.class = NULL,
  plot.name = sprintf("cns_network_to_cns_network_drop_after_an_dn_removal_%s.pdf","drop"),
  rev = FALSE,
  row.dend = NULL,
  col.dend = NULL,
  method = "euclidean",
  symmetric = FALSE,
  diagonal = TRUE
)

# Analyse mushroom body and central complex influence on CNS networks
inf.metric <- "influence_norm_log"
mb.cx.to.cns.cluster.key.plot <- banc_plot_key_features(
  influence.meta = influence.cluster.df %>%
    dplyr::mutate(target = cns_network) %>%
    dplyr::filter(seed %in% control.seeds,
                  !is.na(seed), 
                  !is.na(target),
                  seed!="0",
                  target!="0"),
  ###
  inf.metric = inf.metric,
  target.map = NULL,
  cellwidth = 12,
  cellheight = 12,
  width = 14, 
  height = 6,
  row.order = cns.network.order,
  recalculate = TRUE,
  show.annotation = FALSE,
  save.path = banc.fig6.extra.path,
  seed.map  = NULL,
  chosen.seeds = NULL,
  chosen.targets = NULL, 
  row.cols = NULL,
  super.class = NULL,
  plot.name = sprintf("mb_cx_to_cns_network_%s.pdf",inf.metric),
  rev = FALSE,
  row.dend = NULL,
  col.dend = NULL,
  method = "euclidean"
)

# Analyse CNS network influence on neck super clusters
inf.metric <- "influence_log"
nn.super.cluster.to.cns.cluster.key.plot <- banc_plot_key_features(
  influence.meta = influence.cluster.df %>%
    dplyr::filter(!super_class %in% c("ascending", "descending", 
                                      "motor", "visceral_circulatory"),
                  seed %in% super.clust.order,
                  !is.na(cns_network)) %>%
    dplyr::mutate(target = cns_network) %>%
    dplyr::filter(!is.na(seed), 
                  !is.na(target)),
  ###
  influence.level = "super_cluster",
  inf.metric = inf.metric,
  target.map = NULL,
  cellwidth = 12,
  cellheight = 12,
  width = 12, 
  height = 6,
  col.order = super.clust.order,
  row.order = cns.network.order,
  recalculate = TRUE,
  row.annotation = NULL,
  show.annotation = FALSE,
  save.path = banc.fig6.extra.path,
  seed.map  = FALSE,
  chosen.seeds = NULL,
  chosen.targets = NULL,
  row.cols = NULL,
  super.class = NULL,
  plot.name = sprintf("super_cluster_to_cns_network_%s.pdf",inf.metric),
  rev = FALSE,
  row.dend = NULL,
  col.dend = NULL,
  method = "euclidean",
  symmetric = FALSE,
  diagonal = TRUE
)

# Analyse mushroom body and central complex influence on neck super clusters
nn.super.cluster.mb.cx.in.key.plot <- banc_plot_key_features(
  influence.meta = influence.cluster.df %>%
    dplyr::filter(!is.na(seed),
                  id %in% banc.an.dn.meta$id) %>%
    dplyr::left_join(banc.meta %>%
                       dplyr::distinct(seed_07, .keep_all = TRUE) %>%
                       dplyr::distinct(seed_07, seed_class = cell_class),
                     by=c("seed"="seed_07")) %>%
    dplyr::filter(grepl("central_complex|mushroom",seed_class)) %>%
    dplyr::mutate(target = super_cluster,
                  seed = gsub("_neuron|_"," ",seed)) %>%
    dplyr::filter(!is.na(target),
                  !is.na(seed),
                  seed!="0",
                  seed!="",
                  target!="0",
                  !grepl("^GFC", seed)),
  influence.level = c("seed_07"),
  seed.map = NULL,
  inf.metric = "influence_log",
  save.path = banc.fig6.supp.path,
  target.map = NULL,
  recalculate = TRUE,
  row.annotation = NULL,
  col.annotation = "seed_class",
  show.annotation = FALSE,
  col.thresh = 0.5,
  super.class = NULL,
  cellwidth = 12,
  cellheight = 12,
  width = 12, 
  height = 6,
  row.order = super.clust.order,
  plot.name = sprintf("neck_super_clusters_from_mb_and_cx_%s.pdf","influence_log"),
  rev = FALSE, 
  method = "euclidean"
)

# # Analyse neck super cluster influence on mushroom body and central complex
chosen.cts <- na.omit(unique(banc.meta$cell_type[grepl("central_complex_input|mushroom_body|mushroom_body_dopamin",banc.meta$cell_class)]))
nn.super.cluster.out.mb.cx.key.plot <- banc_plot_key_features(
  influence.meta = influence.cluster.df %>%
    dplyr::filter(cell_type %in% chosen.cts) %>%
    dplyr::mutate(target = dplyr::case_when(
      grepl("lateral_horn|mushroom_body_dopaminergic|mushroom_body_extrinsic",cell_sub_class) ~ NA,
      !is.na(cell_sub_class) ~ cell_sub_class,
      TRUE ~ NA
    )) %>%
    dplyr::filter(!is.na(target)) %>%
    dplyr::mutate(target = gsub("_neuron|_"," ",target)) %>%
    dplyr::filter(!is.na(target),
                  !is.na(seed),
                  seed!="0",
                  seed!="",
                  target!="0"),
  ###
  inf.metric = "influence_log",
  target.map = NULL,
  cellwidth = 12,
  cellheight = 12,
  width = 12, 
  height = 6,
  recalculate = TRUE,
  row.annotation = NULL,
  show.annotation = FALSE,
  influence.level = "super_cluster",
  save.path = banc.fig6.supp.path,
  seed.map  = FALSE,
  chosen.seeds = NULL,
  chosen.targets = NULL,
  row.cols = NULL,
  super.class = NULL,
  col.order = super.clust.order,
  plot.name = sprintf("neck_super_clusters_to_mb_cx_%s.pdf","influence_log"),
  rev = TRUE,
  method = "euclidean"
)

# DISABLED 2026-05-18: ggraph-based super_cluster ↔ super_cluster +
# CNS ↔ CNS network diagrams. Superseded by panels_cns_network_diagram.R
# (which is the supported producer of the Fig. 6c diagram going forward,
# with L/R visual + L/R olfactory network merging baked in). Kept as a
# reference for the original 85th-percentile edge threshold + Fraser-
# Reingold layout if anyone needs to reproduce the pre-merge diagram.
# #######################################
# ### super cluster <-> super cluster ###
# #######################################
#
# # Get our matrices
# m4 <- nn.super.cluster.to.nn.super.cluster.key.plot$influence.matrix
# m4[is.na(m4)] <- 0
# 
# # Edge list: super cluster -> CNS cluster (from columns in m1)
# edges <- as.data.frame(t(as.table(m4)))
# colnames(edges) <- c("to", "from", "weight")
# edges <- edges[edges$weight > 0, ]
# 
# # 1. Compute 50th percentile threshold of edge weights
# thresh <- quantile(edges$weight, 0.85, na.rm = TRUE)
# 
# # 2. Filter edges above threshold
# edges <- edges[edges$weight > thresh, ]
# 
# # 3. Calculate log-weight for plotting (add a small constant if needed to avoid log(0))
# edges$logweight <- log(edges$weight)
# 
# # Node dataframe & type/col
# nodes <- data.frame(
#   name = unique(c(edges$from, edges$to))
# )
# 
# # Create the directed graph
# g <- igraph::graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)
# 
# # Now you can plot with ggraph as well (ggraph supports igraph objects)
# set.seed(42)
# g.sp.sp <- ggraph(g, layout = "fr") +
#   ggraph::geom_edge_bend(
#     aes(width = logweight),
#     alpha = 1,
#     color = "grey40",
#     show.legend = FALSE,
#     arrow = grid::arrow(type = "closed", length = unit(3, "mm")),
#     end_cap = ggraph::circle(7, "mm")
#   ) +
#   ggraph::geom_node_point(size = 7, color = "grey30") +
#   ggraph::geom_node_text(aes(label = name), repel = TRUE, size = 3.5) +
#   theme_void() +
#   labs(title = "super cluster ↔ super cluster (edges > 85th percentile)") +
#   scale_edge_width(range = c(0.05, 2))
# 
# # Save
# plot(g.sp.sp)
# ggsave(plot = g.sp.sp,
#        filename = file.path(banc.fig6.path, sprintf("%s_neck_super_cluster_network_plot.pdf",inf.metric)),
#        width = 8, 
#        height = 8, 
#        dpi = 300, 
#        bg = "transparent")
# 
# ###################
# ### CNS <-> CNS ###
# ###################
# 
# # Get our matrices
# m3 <- cns.cluster.cns.cluster.key.plot$influence.matrix
# m3[is.na(m3)] <- 0
# cns_networks <- rownames(m3)
# 
# # Edge list: super cluster -> CNS cluster (from columns in m1)
# edges <- as.data.frame(t(as.table(m3)))
# colnames(edges) <- c("to", "from", "weight")
# edges <- edges[edges$weight > 0, ]
# 
# # 1. Compute 50th percentile threshold of edge weights
# thresh <- quantile(edges$weight, 0.7, na.rm = TRUE)
# 
# # 2. Filter edges above threshold
# edges <- edges[edges$weight > thresh, ]
# 
# # 3. Calculate log-weight for plotting (add a small constant if needed to avoid log(0))
# edges$logweight <- log(edges$weight)
# 
# # Node dataframe & type/col
# nodes <- data.frame(
#   name = unique(c(edges$from, edges$to))
# )
# 
# # Create the directed graph
# g <- igraph::graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)
# 
# # Now you can plot with ggraph as well (ggraph supports igraph objects)
# set.seed(42)
# g.cns.cns <- ggraph(g, layout = "fr") +
#   ggraph::geom_edge_bend(
#     aes(width = logweight),
#     alpha = 1,
#     color = "grey40",
#     show.legend = FALSE,
#     arrow = grid::arrow(type = "closed", length = unit(3, "mm")),
#     end_cap = ggraph::circle(7, "mm")
#   ) +
#   ggraph::geom_node_point(size = 7, color = "grey30") +
#   ggraph::geom_node_text(aes(label = name), repel = TRUE, size = 3.5) +
#   theme_void() +
#   labs(title = "CNS ↔ CNS cluster (edges > 75th percentile)") +
#   scale_edge_width(range = c(0.05, 2))
# 
# # Save
# ggsave(plot = g.cns.cns,
#        filename = file.path(banc.fig6.path, sprintf("%s_cns_cns_network_plot.pdf",inf.metric)),
#        width = 8, 
#        height = 8, 
#        dpi = 300, 
#        bg = "transparent")
# 
# ###########################
# ### CNS -> super -> CNS ###
# ###########################
# 
# # Get our matrices
# m1 <- nn.super.cluster.to.cns.cluster.key.plot$influence.matrix
# m2 <- cns.cluster.to.nn.super.cluster.key.plot$influence.matrix
# m1[is.na(m1)] <- 0
# m2[is.na(m2)] <- 0
# 
# # Edge list: super cluster -> CNS cluster (from columns in m1)
# edges1 <- as.data.frame(t(as.table(m1)))
# colnames(edges1) <- c("from", "to", "weight")
# edges1 <- edges1[edges1$weight > 0, ]
# thresh <- quantile(edges1$weight, 0.85, na.rm = TRUE)
# edges1 <- edges1[edges1$weight > thresh, ]
# 
# # Edge list: CNS cluster -> super cluster (from columns in m2)
# edges2 <- as.data.frame(t(as.table(m2)))
# colnames(edges2) <- c("from", "to", "weight")
# edges2 <- edges2[edges2$weight > 0, ]
# edges2 <- edges2[edges2$weight > 0, ]
# thresh <- quantile(edges2$weight, 0.85, na.rm = TRUE)
# edges2 <- edges2[edges2$weight > thresh, ]
# 
# # Combine edges
# edges <- rbind(
#   edges1[, c("from", "to", "weight")],
#   edges2[, c("from", "to", "weight")]
# )
# 
# # 3. Calculate log-weight for plotting (add a small constant if needed to avoid log(0))
# edges$logweight <- log(edges$weight)
# 
# # Node dataframe & type/col
# nodes <- data.frame(
#   name = unique(c(edges$from, edges$to))
# )
# nodes$type <- ifelse(nodes$name %in% cns_networks, "Super cluster", "CNS cluster")
# type_colors <- c("CNS cluster" = "tomato", "Super cluster" = "skyblue")
# 
# # Create the directed graph
# g <- igraph::graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)
# 
# # Plot
# set.seed(42)
# g.nn.cns <- ggraph(g, layout = "fr") +
#   ggraph::geom_edge_bend(
#     aes(width = logweight),
#     alpha = 1,
#     color = "grey40",
#     show.legend = FALSE,
#     arrow = grid::arrow(type = "closed", length = unit(3, "mm")),
#     end_cap = ggraph::circle(7, "mm")  
#   ) +
#   ggraph::geom_node_point(aes(color = type), size = 7) +
#   ggraph::geom_node_text(aes(label = name), repel = TRUE, size = 3.5) +
#   scale_color_manual(values = type_colors) +
#   theme_void() +
#   labs(title = "Super cluster ↔ CNS cluster (edges > 85th percentile, with arrows)") +
#   scale_edge_width(range = c(0.05, 2))
# 
# # Save
# plot(g.nn.cns)
# ggsave(plot = g.nn.cns,
#        filename = file.path(banc.fig6.path, sprintf("%s_nn_cns_network_plot.pdf",inf.metric)),
#        width = 8, 
#        height = 8, 
#        dpi = 300, 
#        bg = "transparent")
# 
# ###########################
# ### CNS <-> CNS -> EFF  ###
# ###########################
# 
# # Get our matrices
# m3 <- cns.cluster.cns.cluster.key.plot$influence.matrix
# m5 <- cns.cluster.to.eff.super.cluster.key.plot$influence.matrix
# m3[is.na(m3)] <- 0
# m5[is.na(m5)] <- 0
# 
# # Edge list: super cluster -> CNS cluster (from columns in m1)
# edges1 <- as.data.frame(t(as.table(m3)))
# colnames(edges1) <- c("from", "to", "weight")
# edges1 <- edges1[edges1$weight > 0, ]
# 
# # Edge list: CNS cluster -> super cluster (from columns in m2)
# edges2 <- as.data.frame(t(as.table(m5)))
# colnames(edges2) <- c("from", "to", "weight")
# edges2 <- edges2[edges2$weight > 0, ]
# 
# # Combine edges
# edges <- rbind(
#   edges1[, c("from", "to", "weight")],
#   edges2[, c("from", "to", "weight")]
# )
# 
# # 1. Compute 50th percentile threshold of edge weights
# thresh <- quantile(edges$weight, 0.85, na.rm = TRUE)
# 
# # 2. Filter edges above threshold
# edges <- edges[edges$weight > thresh, ]
# 
# # 3. Calculate log-weight for plotting (add a small constant if needed to avoid log(0))
# edges$logweight <- log(edges$weight)
# 
# # Node dataframe & type/col
# nodes <- data.frame(
#   name = unique(c(edges$from, edges$to))
# )
# nodes$type <- ifelse(nodes$name %in% cns_networks, "CNS cluster", "super cluster")
# type_colors <- c("CNS cluster" = "tomato", "super cluster" = "chartreuse")
# 
# # Create the directed graph
# g <- igraph::graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)
# 
# # Plot
# set.seed(42)
# g.cns.eff<- ggraph(g, layout = "fr") +
#   ggraph::geom_edge_bend(
#     aes(width = logweight),
#     alpha = 1,
#     color = "grey40",
#     show.legend = FALSE,
#     arrow = grid::arrow(type = "closed", length = unit(3, "mm")),
#     end_cap = ggraph::circle(7, "mm")  
#   ) +
#   ggraph::geom_node_point(aes(color = type), size = 7) +
#   ggraph::geom_node_text(aes(label = name), repel = TRUE, size = 3.5) +
#   scale_color_manual(values = type_colors) +
#   theme_void() +
#   labs(title = "efferent super cluster <- CNS ↔ CNS modules (edges > 85th percentile, with arrows)") +
#   scale_edge_width(range = c(0.05, 2))
# 
# # Save
# print(g.cns.eff)
# ggsave(plot = g.cns.eff,
#        filename = file.path(banc.fig6.path, sprintf("%s_cns_eff_network_plot.pdf",inf.metric)),
#        width = 8, 
#        height = 8, 
#        dpi = 300, 
#        bg = "transparent")






