#' CNS-network spectral clustering: light visualisations (Fig. 6a, ED Fig. 10a–c)
#'
#' Loads the precomputed spectral-clustering output (k = 13 partitions of
#' 50,568 central-brain + VNC intrinsic + AN/DN + visual-projection +
#' visual-centrifugal neurons; Methods §"Spectral clustering", Eqs. 13–14)
#' and renders the lightweight network-inventory figures that do not
#' require fresh `query_influence()` runs:
#'
#'   - Fig. 6a — per-network 2D synapse-density KDE + super-class inventory.
#'   - ED Fig. 10a — PCA-UMAP of CNS networks coloured by network.
#'   - ED Fig. 10b — super-class proportions per CNS network (stacked bars).
#'   - ED Fig. 10c — AN/DN counts per CNS network, grouped by AN/DN cluster.
#'
#' Split out from the heavier `panels_cns_network_analyses.R` (originally
#' one combined script, `panel_super_clusters.R`) on 2026-05-17 so a
#' supplement-figure refresh runs in ~10–20 s instead of multi-hour.
#'
#' @section Reads:
#'   banc.meta, banc.edgelist.simple, paper.cols
#'   .banc_spectral_csv  (per banc-startup.R; v850 spectral output, canonical)
#'
#' @section Writes:
#'   figures/figure_6/links/cns_umap.{pdf,png}                                (Fig. 6a backdrop)
#'   figures/figure_6/links/cns_network_by_super_class.pdf                    (ED Fig. 10b)
#'   figures/figure_6/links/supplement/<cns_network>_bar_super_class.pdf      (per-network bars)
#'   figures/figure_6/links/supplement/cns_network_by_neck_*.pdf              (network ↔ cluster heatmaps)
#'   figures/figure_6/links/extra/darkmode/*.pdf                              (dark-mode variants)
#'
#' @section Paper:
#'   Fig. 6a — CNS networks: 2D KDE of synapses + super-class inventory.
#'   ED Fig. 10a–c — PCA-UMAP, super-class proportions, AN/DN counts per network.
#'   Methods §"Spectral clustering" (Eqs. 13–14) + §"Naming CNS networks".
#'
#' @section Schema:
#'   Optic-lobe intrinsic neurons are deliberately excluded from the
#'   spectral input — they dominate by count and obscure CNS-wide modules.
#'
#' @section Used by:
#'   panels_cns_network_analyses.R (reads the same cns_network labels);
#'   R/text/numbers.R (network sizes, AN/DN counts per network).
#'
#' @section Reproduce:
#'   BANC_NCORES=1 Rscript R/figures/panels_cns_networks.R

####################
## STARTUP        ##
####################

source("R/startup/banc-startup.R")
source("R/startup/banc-meta.R")
source("R/startup/banc-functions.R")
source("R/startup/banc-edgelist.R")

####################
## DATA LOADING   ##
####################

# Load CNS network UMAP coordinates and filter for analysis.
# v850 spectral clustering — v746/v626 fallbacks removed 2026-04-09 since
# the v850 file is now the canonical source.
cns_network_file <- .banc_spectral_csv
cns.umap <- readr::read_csv(cns_network_file,
                            col_types = banc.col.types) %>%
  dplyr::select(root_id,
                UMAP1 = umap_x,
                UMAP2 = umap_y) %>%
  dplyr::left_join(banc.meta, by = "root_id") %>%
  dplyr::filter(!is.na(cns_network)) %>%
  dplyr::filter(!super_class%in%c("glia","sensory", "trachea","sensory_ascending","motor","visceral_circulatory","not_a_neuron"),
                !is.na(super_class))

####################
## CNS NETWORK VIZ ##
####################

# Generate UMAP visualisation of CNS network organisation
cns_network_centroids <- cns.umap %>%
  dplyr::group_by(cns_network) %>%
  dplyr::summarise(UMAP1 = mean(UMAP1),
            UMAP2 = mean(UMAP2))

# Create CNS network UMAP with colour-coded clusters
p_cns_networks <- ggplot(data = cns.umap,
                  aes(x = UMAP1, y = UMAP2)) +
  geom_point(data = subset(cns.umap,
                           !is.na(cns_network)),
             aes(color = cns_network),
             alpha = 0.95,
             size = 1.5,
             stroke = 1) +
  # geom_text(data = cns_network_centroids,
  #           aes(label = cns_network),
  #           colour = "black",
  #           size = 4,
  #           hjust = -1,
  #           fontface = "bold") +
  #scale_color_manual(values = paper.cols) +
  scale_color_cerise_limon() +
  theme_void() +
  labs(title = "",
       x = "UMAP1",
       y = "UMAP2") +
  guides(
    color = guide_legend(ncol = 4, byrow = TRUE, override.aes = list(size=4))
  ) +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title=element_blank(),
    legend.text = element_text(size = 9, color = "black"),
    plot.margin = margin(t = 0, r = 50, b = 0, l = 50, unit = "pt")
  )

# Display and export CNS network UMAP
plot(p_cns_networks)
ggsave(plot = p_cns_networks,
       filename = file.path(banc.fig6.extra.path, "cns_umap.png"),
       width = 10, height = 10, dpi = 300)
ggsave(plot = p_cns_networks,
       filename = file.path(banc.fig6.supp.path, "cns_umap.pdf"),
       width = 10, height = 10, dpi = 300)
ggsave(plot = convert_to_dark_mode(p_cns_networks),
       filename = file.path(banc.fig6.darkmode.path, "dark_mode_cns_umap.png"),
       width = 10, height = 10, dpi = 300)

####################
## BAR CHART ANALYSIS ##
####################

# Analyse super class composition within each CNS network
df_bar <- cns.umap %>%
  dplyr::mutate(
    super_class = dplyr::case_when(
      super_class %in% c("ascending","descending", "visual_projection", "visual_centrifgual") ~ super_class,
      grepl("mushroom_body|central_complex|kenyon_cell|lateral_horn|antennal_lobe|suboesophageal_zone", cell_class) ~ cell_class,
      #!is.na(sez_class)|grepl("wedge|AMMC", cell_class)|grepl("wedge|AMMC", cell_sub_class) ~ "suboesophageal_zone",
      TRUE ~ super_class,
    )
  ) %>%
  dplyr::mutate(
    super_class = gsub("_intrinsic_neuron|_output_neuron|_input_neuron|_centrifugal_neuron|_centrifugal_neuron|_projection_neuron|_extrinsic_neuron|_local_neuron","",super_class),
  ) %>%
  dplyr::count(cns_network, super_class) %>%
  dplyr::group_by(cns_network) %>%
  dplyr::mutate(prop = n / sum(n)) %>%
  dplyr::ungroup()

# To order by mean proportion of each super_class:
super_order <- df_bar %>%
  dplyr::group_by(super_class) %>%
  dplyr::summarise(mean_prop = mean(prop)) %>%
  dplyr::arrange(mean_prop) %>%
  dplyr::pull(super_class)
df_bar <- dplyr::mutate(df_bar, super_class = factor(super_class, levels = super_order))

# Use the canonical cns.network.order (banc-meta.R) so this plot's column
# order matches the other heatmaps in extended_data_figure_10.
cluster_order <- cns.network.order[cns.network.order %in% unique(df_bar$cns_network)]
cluster_order <- c(cluster_order,
                   setdiff(unique(df_bar$cns_network), cluster_order))
df_bar <- dplyr::mutate(df_bar, cns_network = factor(cns_network, levels = cluster_order))

# Colours — use paper.cols directly, no overrides (2026-04-13).
paper.cols2 <- paper.cols

# Data frame with total count per cns_network
df_totals <- df_bar %>%
  dplyr::group_by(cns_network) %>%
  dplyr::summarise(total_n = sum(n), .groups = "drop")

# For a stacked bar, y = 1 is a safe position (100%)
df_totals$label_y <- 1.02 # Slightly above the top for readability

# Plot
g.bar <- ggplot2::ggplot(df_bar, ggplot2::aes(x = cns_network, y = prop, fill = super_class)) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::geom_text(
    data = df_totals,
    ggplot2::aes(x = cns_network, y = label_y, label = total_n),
    inherit.aes = FALSE,
    size = 3,
    vjust = 0
  ) +
  ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = ggplot2::expansion(mult = c(0, 0.08))) +
  ggplot2::labs(x = "CNS cluster", y = "proportion", fill = "super class") +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
  ggplot2::scale_fill_manual(values = paper.cols2) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal"
  ) + guides(fill = guide_legend(nrow = 3, byrow = TRUE))

# Show
print(g.bar)

# Save. width 8 -> 10 and height 8 -> 5 (2026-05-20): the prior 8x8 was too
# tall (and the legend was still clipping). 3-row legend fits the full list
# of super_classes without truncation at width=10.
ggsave(plot = g.bar,
       filename = file.path(banc.fig6.supp.path,
                            "cns_network_by_super_class.pdf"),
       width = 10,
       height = 5,
       dpi = 300)

# Generate individual bar charts for each CNS network
df_bar2 <- cns.umap %>%
  dplyr::count(cns_network, super_class) %>%
  dplyr::group_by(cns_network) %>%
  dplyr::mutate(prop = n / sum(n)) %>%
  dplyr::ungroup()
df_bar2$super_class <- factor(df_bar2$super_class, levels = c("central_brain_intrinsic",
                                                             "descending",
                                                             "ascending",
                                                             "ventral_nerve_cord_intrinsic"))

# One plot per bar
clusters <- unique(df_bar2$cns_network)
# paper.cols2[["central_brain_intrinsic"]] = paper.cols[["sez"]]  # removed 2026-04-13, use paper.cols directly
for (cluster in clusters) {
  df_sub <- df_bar2[df_bar2$cns_network == cluster, ]
  cluster.nam <- cluster #names(cns.cluster.names)[which(cns.cluster.names==cluster)]
  cluster.nam <- gsub(" |\\/","_",cluster.nam)

  # Calculate total "n" for current cluster
  total_n <- sum(df_sub$n)

  # Label y-position, adjust if necessary
  label_y <- 0.05

  # Thin bar: width=.2, place number at x=1.2 just beside bar
  g <- ggplot2::ggplot(df_sub, ggplot2::aes(x = factor(1), y = prop, fill = super_class, group = super_class)) +
    ggplot2::geom_bar(stat = "identity", width = 0.2) +
    ggplot2::geom_text(
      data = data.frame(x = 1.25, y = 0, label = total_n), # y=0 for bottom, adjust as needed
      mapping = ggplot2::aes(x = x, y = y, label = label),
      inherit.aes = FALSE,
      angle = 90,
      size = 8,
      hjust = 0,          # left align
      vjust = 0.5,        # center on y
      fontface = "bold"
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::percent_format(accuracy = 1),
      expand = ggplot2::expansion(mult = c(0, 0.08))
    ) +
    ggplot2::labs(
      title = "",
      x = "",
      y = "",
      fill = ""
    ) +
    ggplot2::scale_fill_manual(values = paper.cols2) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "none",
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )

  # Save — these are metadata-derived super_class composition bars, NOT 3D
  # neuroanatomy renderings, so they live in extra/ rather than neuroanatomy/.
  ggsave(
    plot = g,
    filename = file.path(banc.fig6.extra.path, paste0(cluster.nam, "_bar_super_class", ".pdf")),
    width = 1.5,
    height = 4,
    dpi = 300
  )
}

############################
## HEATMAP COUNT ANALYSIS ##
############################

# Analyse neck cluster membership across CNS networks
df_heat <- cns.umap %>%
  dplyr::filter(!is.na(cluster),
                super_class %in% c("ascending",
                                   "descending")) %>%
  dplyr::count(cluster, cns_network) %>%
  tidyr::pivot_wider(names_from = cns_network,
                     values_from = n,
                     values_fill = 0) %>%
  as.data.frame()

# Make the row names = cluster and remove the cluster column for pheatmap
heat_mat <- df_heat
rownames(heat_mat) <- heat_mat$cluster
heat_mat$cluster <- NULL
heat_prop <- sweep(heat_mat, 1, rowSums(heat_mat), FUN = "/") * 100
pheatmap::pheatmap(
  heat_prop[,cns.network.order[cns.network.order%in%colnames(heat_prop)]],
  scale = "none",
  cluster_rows = TRUE,
  cluster_cols = FALSE,
  treeheight_row = 0,
  treeheight_col = 0,
  main = "",
  na_col = "white",
  color = colorRampPalette(c("white",highlight.col))(100),
  cellheight = 10,
  cellwidth = 10,
  fontsize_col = 8,
  fontsize_row = 8,
  filename = file.path(banc.fig6.extra.path,"cns_network_by_neck_cluster.pdf"),
)

# Generate heatmaps for super cluster vs CNS network relationships
df_heat <- cns.umap %>%
  dplyr::filter(!is.na(super_cluster),
                super_class %in% c("ascending",
                                   "descending")) %>%
  dplyr::count(super_cluster, cns_network) %>%
  tidyr::pivot_wider(names_from = cns_network,
                     values_from = n,
                     values_fill = 0) %>%
  as.data.frame()
heat_mat <- df_heat
rownames(heat_mat) <- heat_mat$super_cluster
heat_mat$super_cluster <- NULL
heat_prop <- sweep(heat_mat, 2, colSums(heat_mat), FUN = "/") * 100
pheatmap::pheatmap(
  heat_prop[,cns.network.order[cns.network.order%in%colnames(heat_prop)]],
  scale = "none",
  cluster_rows = TRUE,
  cluster_cols = TRUE,
  treeheight_row = 0,
  treeheight_col = 0,
  main = "",
  color = colorRampPalette(c("white",highlight.col))(100),
  cellheight = 10,
  cellwidth = 10,
  fontsize_col = 8,
  fontsize_row = 8,
  na_col = "lightgrey",
  filename = file.path(banc.fig6.extra.path,"cns_network_by_neck_super_cluster_row_normalised.pdf"),
)
heat_prop <- sweep(heat_mat, 1, rowSums(heat_mat), FUN = "/") * 100
pheatmap::pheatmap(
  heat_prop[,cns.network.order[cns.network.order%in%colnames(heat_prop)]],
  scale = "none",
  cluster_rows = TRUE,
  cluster_cols = TRUE,
  treeheight_row = 0,
  treeheight_col = 0,
  main = "",
  color = colorRampPalette(c("white",highlight.col))(100),
  cellheight = 10,
  cellwidth = 10,
  fontsize_col = 8,
  fontsize_row = 8,
  na_col = "lightgrey",
  filename = file.path(banc.fig6.extra.path,"cns_network_by_neck_super_cluster_column_normalised.pdf"),
)
cns.counts <-t(as.matrix(table(cns.umap$cns_network)))[,colnames(heat_mat)]
heat_prop <- sweep(heat_mat, 2, cns.counts[colnames(heat_mat)], "/")
pheatmap::pheatmap(
  heat_prop[,cns.network.order[cns.network.order%in%colnames(heat_prop)]],
  scale = "none",
  cluster_rows = TRUE,
  cluster_cols = TRUE,
  treeheight_row = 0,
  treeheight_col = 0,
  main = "",
  color = colorRampPalette(c("white",highlight.col))(100),
  cellheight = 10,
  cellwidth = 10,
  fontsize_col = 8,
  fontsize_row = 8,
  na_col = "lightgrey",
  filename = file.path(banc.fig6.extra.path,"cns_network_by_neck_super_cluster_cns_network_normalised.pdf"),
)
heat_mat_capped <- heat_mat
heat_mat_capped[heat_mat_capped>100] <- 100
missing_cols <- setdiff(unname(cns.cluster.names), colnames(heat_mat_capped))
if (length(missing_cols) > 0) {
  for (col in missing_cols) {
    heat_mat_capped[[col]] <- 0
  }
}
pheatmap::pheatmap(
  heat_mat_capped[super.clust.order[super.clust.order %in% rownames(heat_mat_capped)],
                  cns.network.order[cns.network.order %in% colnames(heat_mat_capped)], drop = FALSE],
  scale = "none",
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  treeheight_row = 0,
  treeheight_col = 0,
  main = "",
  na_col = "white",
  color = grDevices::colorRampPalette(c("white", "#a7adb2", "#8b929a", "#767b8d", "#494d5e", "grey10"))(100),
  cellheight = 10,
  cellwidth = 10,
  fontsize_col = 8,
  fontsize_row = 8,
  filename = file.path(banc.fig6.supp.path,"cns_network_by_neck_super_cluster_count.pdf"),
)
