###################################
## FIGURE 6: SUPER CLUSTERS     ##
###################################
# Analyses CNS network organisation and super cluster relationships.
# Shows how major functional neuron groupings are distributed across
# brain regions and their connectivity patterns.
# Output: figures/figure6/*_cns_network_*.pdf

####################
## STARTUP        ##
####################

source("R/startup/banc-startup.R")
source("R/startup/franken-meta.R")
source("R/startup/banc-meta.R")
source("R/startup/banc-functions.R")
source("R/startup/banc-edgelist.R")
banc.version <- NULL
source("R/startup/banc-meta.R")
banc.meta$root_id <- banc.meta$root_626

####################
## DATA LOADING   ##
####################

# Load CNS network UMAP coordinates and filter for analysis
cns.umap <- readr::read_csv("data/cns_network/spectral_clustering_min_connection_strength_1_banc_version_626_cluster_count_13_cluster_seed_10_embedding_seed_3.csv", 
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
       filename = file.path(banc.fig6.supp.path, "cns_umap.png"),
       width = 10, height = 10, dpi = 300)
ggsave(plot = p_cns_networks,
       filename = file.path(banc.fig6.supp.path, "cns_umap.pdf"),
       width = 10, height = 10, dpi = 300)
ggsave(plot = convert_to_dark_mode(p_cns_networks),
       filename = file.path(banc.fig6.extra.path, "dark_mode_cns_umap.png"),
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
      !is.na(sez_class)|grepl("wedge|AMMC", cell_class)|grepl("wedge|AMMC", cell_sub_class) ~ "suboesophageal_zone",
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

# Order clusters by neck neuron inventory
cluster_order <- df_bar %>%
  dplyr::mutate(prop = dplyr::case_when(
    super_class %in% c("ascending","descending") ~ NA,
    TRUE ~ prop)) %>%
  dplyr::group_by(cns_network) %>%
  dplyr::summarise(sum_prop = sum(prop,na.rm=TRUE)) %>%
  dplyr::arrange(sum_prop) %>%
  dplyr::pull(cns_network)
cluster_order <- na.omit(unique(c(cluster_order,unique(df_bar$cns_network))))
df_bar <- dplyr::mutate(df_bar, cns_network = factor(cns_network, levels = cluster_order))

# Colours
paper.cols2 <- paper.cols
paper.cols2[["central_brain_intrinsic"]] = "lightgrey"
paper.cols2[["ventral_nerve_cord_intrinsic"]] = "grey10"
paper.cols2[["visual_projection"]] = "grey90"
paper.cols2[["visual_centrifugal"]] = "grey70"

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
  ) + guides(fill = guide_legend(nrow = 2, byrow = TRUE))

# Show
print(g.bar)

# Save
ggsave(plot = g.bar,
       filename = file.path(banc.fig6.supp.path, 
                            "cns_network_by_super_class.pdf"),
       width = 8, 
       height = 6, 
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
paper.cols2[["central_brain_intrinsic"]] = paper.cols[["sez"]]
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
  
  # Save
  ggsave(
    plot = g,
    filename = file.path(banc.fig6.anat.path, paste0(cluster.nam, "_bar_super_class", ".pdf")),
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
  filename = file.path(banc.fig6.path,"cns_network_by_neck_cluster.pdf"),
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
  heat_mat_capped[super.clust.order,cns.network.order, drop = FALSE],
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
  filename = file.path(banc.fig6.path,"cns_network_by_neck_super_cluster_count.pdf"),
)

######################
## TRANSIT ANALYSIS ##
######################

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
                count > 5) %>%
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
                count > 5) %>%
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
       filename = file.path(banc.fig6.supp.path, "direct_connections_super_cluster_connections_with_cns_networks.pdf"),
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
  dplyr::filter(count > 5) %>%
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
  dplyr::filter(count > 5) %>%
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
       filename = file.path(banc.fig6.path, "neck_transits_cns_networks.pdf"),
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

# 3) Build violin plot (ordered) with optional significance labels
g.transfer <- ggplot2::ggplot(
  home.cluster.elist.unified,
  ggplot2::aes(x = type, y = prop, fill = type, color = type)) +
  ggplot2::geom_violin(trim = FALSE, alpha = 0.5, color = NA) +
  ggplot2::geom_boxplot(width = 0.12, outlier.shape = NA, alpha = 0.35, color = "black") +
  ggplot2::labs(
    x = "super class",
    y = "prop",
    color = "super class",
    title = ""
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    legend.position = "none",
    panel.grid.minor = ggplot2::element_blank()
  ) +
  ggplot2::scale_fill_manual(values = paper.cols) +
  ggplot2::scale_color_manual(values = paper.cols) +
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
  dplyr::filter(count > 5) %>%
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
  dplyr::filter(count > 5) %>%
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
  if("pre_top_nt"%in%colnames(df)){
    if(swing) {
      # Classify neurotransmitter
      df_swing <- df %>%
        dplyr::mutate(
          nt_group = dplyr::case_when(
            pre_top_nt == "acetylcholine" ~ "positive",
            pre_top_nt %in% c("gaba", "glutamate") ~ "negative",
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
            pre_top_nt == "acetylcholine" ~ 1 * count,
            pre_top_nt %in% c("gaba", "glutamate") ~ -1 * count,
            TRUE ~ 0
          )) %>%
          dplyr::group_by(away = away_cluster, home = home_cluster) %>%
          dplyr::summarise(score = sum(nt_score, na.rm = TRUE), .groups = "drop") %>%
          tidyr::pivot_wider(names_from = home, values_from = score, values_fill = 0) %>%
          as.data.frame()
      } else {
        mat_p <- df %>%
          dplyr::mutate(nt_score = dplyr::case_when(
            pre_top_nt == "acetylcholine" ~ 1,
            pre_top_nt %in% c("gaba", "glutamate") ~ -1,
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
ordered_names <- cns.network.order
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
  filename = file.path(banc.fig6.supp.path,"partners_cns_network_outputs_to_cns_network.pdf")
)

# Plot for main
mat_p <- mat_p[cns.network.order,cns.network.order]
diag(mat_p) <- NA
pheatmap::pheatmap(
  mat_p[cns.network.order,cns.network.order],
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
mat_n <- cns_network_links(home.cluster.nn.home.away.pre,partners=TRUE,diagonal=TRUE)[ordered_names, ordered_names]
mat_n[mat_n==0] <- ''
pheatmap::pheatmap(
  cns_network_links(home.cluster.nn.home.away.pre)[ordered_names, ordered_names],
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
mat_n <- cns_network_links(home.cluster.nn.home.away.pre,partners=FALSE,diagonal=TRUE)[ordered_names, ordered_names]
mat_n[mat_n==0] <- ''
pheatmap::pheatmap(
  cns_network_links(home.cluster.nn.home.away.pre,partners=FALSE)[ordered_names, ordered_names],
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
                                   pre_top_nt),
                   by ="pre") %>%
  dplyr::left_join(banc.meta.post %>%
                     dplyr::select(post = post_id,
                                   post_super_class,
                                   post_cns_network),
                   by ="post") %>%
  dplyr::filter(!is.na(pre_cns_network)&!is.na(post_cns_network)) %>%
  dplyr::group_by(pre, pre_top_nt, post_cns_network) %>%
  dplyr::filter(count > 5) %>%
  dplyr::mutate(count = sum(count, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(id = pre, count, pre_top_nt, home_cluster = pre_cns_network, away_cluster = post_cns_network) %>%
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
                                   pre_top_nt),
                   by ="pre") %>%
  dplyr::left_join(banc.meta.post %>%
                     dplyr::select(post = post_id,
                                   post_super_class,
                                   post_super_cluster),
                   by ="post") %>%
  dplyr::filter(!is.na(pre_super_cluster)&!is.na(post_super_cluster)) %>%
  dplyr::group_by(pre, pre_top_nt, post_super_cluster) %>%
  dplyr::filter(count > 5) %>%
  dplyr::filter(post_super_class %in% c("ascending","descending"),
                pre_super_class %in% c("ascending","descending")) %>%
  dplyr::mutate(count = sum(count, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(id = pre, count, pre_top_nt, home_cluster = pre_super_cluster, away_cluster = post_super_cluster) %>%
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
  filename = file.path(banc.fig6.supp.path,"connections_signed_cns_network_outputs_to_cns_network.pdf")
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
  filename = file.path(banc.fig6.supp.path,"connections_signed_swing_neck_super_cluster_outputs_to_neck_super_cluster.pdf")
)

########################
## INFLUENCE ANALYSIS ##
########################

# Analyse influence patterns between CNS networks and clusters
inf.metric <- "influence_log"

# Set up for influence calculation
bc.meta <- banc.meta
bc.meta$root_id <- bc.meta$root_626
ic_banc <- influence_calculator_py(edgelist_simple = banc.edgelist.simple %>%
                                     dplyr::filter(count > 0), 
                                   meta = bc.meta)

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
total_tasks <- sum(sapply(seeds, function(seed) {
  cts <- na.omit(unique(bc.meta[[seed]]))
  if (seed == "seed_07") cts <- intersect(cts, seeds07)
  length(cts)
}))
pb <- progress::progress_bar$new(
  format = "Influence [:bar] :current/:total (:percent) eta: :eta",
  total  = total_tasks, clear = FALSE, width = 70
)
influence.cluster.orig <- list()
for (seed in seeds) {
  cts <- na.omit(unique(bc.meta[[seed]]))
  if (seed == "seed_07") {
    cts <- intersect(cts, seeds07)
  }
  for (ct in cts) {
    try({
      banc.ct.ids <- unique(bc.meta$root_id[bc.meta[[seed]] == ct])
      control_influence.id <- calculate_influence_py(ic_banc, banc.ct.ids) %>%
        dplyr::filter(id %in% banc.target.ids)
      control_influence.id$seed <- ct
      control_influence.id$level <- seed
      control_influence.id$influence_norm_original <-
        control_influence.id$`Influence_score_(unsigned)` / length(banc.ct.ids)
      influence.cluster.orig <- append(influence.cluster.orig, list(control_influence.id))
    }, silent = TRUE)
    pb$tick()
  }
}
influence.cluster.orig.df <- do.call(rbind, influence.cluster.orig) %>%
  dplyr::mutate(influence_original = `Influence_score_(unsigned)`)

# Get influence results for BANC
ic_no_neck_banc <- influence_calculator_py(edgelist_simple = banc.edgelist.simple %>%
                                     dplyr::filter(count > 0,
                                                   !grepl("ascending|descending",post_super_class),
                                                   !grepl("ascending|descending",pre_super_class),
                                     !post %in% banc.neck.meta$root_id,
                                     !pre %in% banc.neck.meta$root_id,
                                     !post %in% banc.neck.meta$root_626,
                                     !pre %in% banc.neck.meta$root_626),
                                   meta = bc.meta)
seeds <- c("seed_02","cns_network")
influence.no.neck.orig <- list()
for(seed in seeds){
  cts <- na.omit(unique(bc.meta[[seed]]))
  cts <- setdiff(cts,banc.neck.meta$cell_type)
  for(ct in cts){
    try({
      banc.ct.ids <- unique(bc.meta$root_id[bc.meta[[seed]]==ct])
      control_influence.id <- calculate_influence_py(ic_no_neck_banc, banc.ct.ids) %>%
        dplyr::filter(id %in% banc.target.ids)
      control_influence.id$seed <- ct
      control_influence.id$level <- seed
      control_influence.id$influence_norm_original <- control_influence.id$`Influence_score_(unsigned)`/length(banc.ct.ids)
      influence.no.neck.orig <- append(influence.no.neck.orig, list(control_influence.id))
    }) 
  }
}
influence.no.neck.df <- do.call(rbind, influence.no.neck.orig)%>%
  dplyr::rename(influence_original = `Influence_score_(unsigned)`) %>%
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
# write_anova_summary(influence.cluster.df %>%
#                       dplyr::filter(grepl("EFF", cluster)) %>%
#                       dplyr::mutate(target = super_cluster) %>%
#                       dplyr::filter(!is.na(target),
#                                     !is.na(seed),
#                                     seed!="0",
#                                     seed!="",
#                                     target!="0",
#                                     !is.infinite(adjusted_influence)) %>%
#                       dplyr::distinct(source=seed, target, value = adjusted_influence) %>%
#                       dplyr::ungroup(),
#                     file.path(banc.fig6.path,sprintf("major_cns_network_output_to_efferent_super_clusters_%s.txt",inf.metric)))

# Additional statistical tests
cns_to_eff_data <- influence.cluster.df %>%
  dplyr::filter(grepl("EFF", cluster)) %>%
  dplyr::mutate(target = super_cluster) %>%
  dplyr::filter(!is.na(target), !is.na(seed), seed!="0", seed!="",
                target!="0", !is.infinite(adjusted_influence))

influence_matrix <- cns.cluster.main.out.to.super.eff.key.plot$influence.matrix
test_groups <- c("central complex", "right olfactory", "left olfactory")

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
  save.path = banc.fig6.supp.path,
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
  save.path = banc.fig6.supp.path,
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
                  target!="0"),
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
      grepl("lateral_horn|mushroom_body_dopaminergic",cell_sub_class) ~ NA,
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






