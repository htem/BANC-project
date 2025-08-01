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
source("R/startup/banc_an_dn_data.R")

# new meta
banc.eff2.meta <- banc.eff.meta %>%
  dplyr::mutate(body_part_effector = dplyr::case_when(
    grepl("power|steering|tension",cell_function) ~ cell_function,
    grepl("pitch|yaw|roll",cell_function_detailed) ~ cell_function_detailed,
    TRUE ~ body_part_effector
  ))
banc.an.dn.meta <- banc.meta %>%
  dplyr::filter(super_class %in% c("ascending","descending")) %>%
  dplyr::filter(!grepl("^SA|^SN|^AN_4|AN_5|^IN",cell_type))
banc.targets <- banc.meta %>%
  dplyr::filter(super_class %in% c("descending","asecending","visual_centrifugal")|
                  grepl("mushroom_body_input|central_complex_input",cell_class)|
                  root_id%in%!!banc.eff.meta$root_id)

# Triaged neck neurons
neck.inclusion <- readr::read_csv(file="data/meta/banc_neck_inclusion.csv", 
                                  col_types = banc.col.types)
banc.in <- subset(neck.inclusion,in_group)$root_id
banc.out <- subset(neck.inclusion,!in_group)$root_id

# Recalculate?
recalculate <- FALSE

#####################################################
### INPUT+OUTPUT CONNECTIVITY UMAP DATA BY NEURON ###
#####################################################

if(recalculate){
  source("R/startup/banc-edgelist.R")
  
  # Make matrix
  an.dn.ids <- unique(banc.an.dn.meta$root_id)
  
  # Get edgelist
  neck.elist.pre <- banc.edgelist.simple %>%
    dplyr::filter(pre %in% an.dn.ids)
  neck.elist.post <- banc.edgelist.simple %>%
    dplyr::filter(post %in% an.dn.ids)
  neck.elist.cat <- rbind(neck.elist.pre %>%
                            dplyr::mutate(pre_cell_type = dplyr::case_when(
                              !is.na(pre_cell_type) ~ pre_cell_type,
                              TRUE ~ pre,
                            )) %>%
                            dplyr::group_by(id = pre, post) %>%
                            dplyr::mutate(count = sum(count, na.rm = TRUE),
                                          norm = mean(norm, na.rm = TRUE)) %>%
                            dplyr::ungroup() %>%
                            dplyr::mutate(partner_id = paste0("post_",post)) %>%
                            dplyr::distinct(id, partner_id, count, norm),
                          neck.elist.post %>%
                            dplyr::mutate(post_cell_type = dplyr::case_when(
                              !is.na(post_cell_type) ~ post_cell_type,
                              TRUE ~ post,
                            )) %>%
                            dplyr::group_by(id = post, pre) %>%
                            dplyr::mutate(count = sum(count, na.rm = TRUE),
                                          norm = mean(norm, na.rm = TRUE)) %>%
                            dplyr::ungroup() %>%
                            dplyr::mutate(partner_id = paste0("pre_",pre)) %>%
                            dplyr::distinct(id, partner_id, count, norm)) %>%
    dplyr::distinct(id, partner_id, count, norm)
  inout_connection_matrix <- neck.elist.cat %>%
    dplyr::filter(id %in% an.dn.ids) %>%
    reshape2::dcast(partner_id ~ id,
                    fun.aggregate = mean,
                    value.var = "norm",
                    fill = 0)
  rownames(inout_connection_matrix) <- inout_connection_matrix$partner_id
  inout_connection_matrix$partner_id <- NULL
  
  # Remove all-zero rows from the original matrix
  non_zero_rows <- which(rowSums(abs(inout_connection_matrix)) > 0.0001)
  inout_connection_matrix <- inout_connection_matrix[non_zero_rows, ]
  non_zero_cols <- which(colSums(abs(inout_connection_matrix)) > 0.0001)
  inout_connection_matrix <- inout_connection_matrix[,non_zero_cols]
  
  # Calculate cosine similarity
  sparsity <- sum(inout_connection_matrix == 0) / prod(dim(inout_connection_matrix))
  print(paste("Sparsity:", sparsity))
  sparse_matrix <- as(as.matrix(t(inout_connection_matrix)), "dgCMatrix")
  
  # Calculate cosine similarity
  undirected_cosine_sim_matrix <- cosine_similarity_sparse(t(sparse_matrix))
  undirected_cosine_sim_matrix[is.infinite(undirected_cosine_sim_matrix)] <- 0
  dimnames(undirected_cosine_sim_matrix) <- list(colnames(inout_connection_matrix),colnames(inout_connection_matrix))
  
  # Perform PCA
  pca_result <- prcomp(undirected_cosine_sim_matrix, center = TRUE, scale. = TRUE)
  
  # Calculate cumulative explained variance ratio
  var_explained <- pca_result$sdev^2 / sum(pca_result$sdev^2)
  cumulative_var_explained <- cumsum(var_explained)
  
  # Find number of components that explain 50% of variance
  n_components <- which(cumulative_var_explained >= 0.50)[1]
  
  # Plot cumulative explained variance
  plot(cumulative_var_explained, 
       xlab = "Number of Components", 
       ylab = "Cumulative Proportion of Variance Explained",
       type = "b")
  abline(h = 0.50, col = "red", lty = 2)
  abline(v = n_components, col = "blue", lty = 2)
  
  # Print the number of components
  print(paste("Number of components explaining 50% of variance:", n_components))
  
  # Example index selection
  # ref_matrix <- inout_connection_matrix[,intersect(colnames(inout_connection_matrix),banc.in)]
  # proj_matrix <- inout_connection_matrix[,intersect(colnames(inout_connection_matrix),banc.out)]
  # set.seed(42)
  # umap_fit <- uwot::umap(t(ref_matrix),
  #                        metric = "cosine",
  #                        n_epochs = 500,
  #                        n_neighbors = 100,
  #                        min_dist = 0,
  #                        n_trees = 100,
  #                        spread = 10,
  #                        n_components = 2,
  #                        ret_model = TRUE)
  # ref_coords <- umap_fit$embedding
  # proj_coords <- uwot::umap_transform(t(proj_matrix), umap_fit)
  # umap_result <- rbind(
  #   data.frame(UMAP1 = ref_coords[,1],
  #              UMAP2 = ref_coords[,2],
  #              node = colnames(ref_matrix),
  #              set = "reference"),
  #   data.frame(UMAP1 = proj_coords[,1],
  #              UMAP2 = proj_coords[,2],
  #              node = colnames(proj_matrix),
  #              set = "projection")
  # )
  # rownames(umap_result) <- umap_result$node
  
  # Represent as UMAP
  set.seed(42)  
  umap_result <- uwot::umap(t(inout_connection_matrix),
                            metric = "cosine",
                            n_epochs = 500,
                            n_neighbors = 100, 
                            min_dist = 0,
                            n_trees = 100,
                            spread = 10,
                            n_components = 2)
  rownames(umap_result) <- colnames(inout_connection_matrix)
  # umap_result_n <- uwot::umap(t(inout_connection_matrix),
  #                           metric = "cosine",
  #                           n_epochs = 500,
  #                           n_neighbors = 100, 
  #                           min_dist = 0,
  #                           n_trees = 100,
  #                           spread = 10,
  #                           n_components = n_components)
  # rownames(umap_result_n) <- colnames(inout_connection_matrix)
  
  # Create a data frame with UMAP coordinates
  umap.dn.df <- data.frame(
    UMAP1 = umap_result[,1],
    UMAP2 = umap_result[,2],
    id = rownames(umap_result)) %>% 
    dplyr::left_join(banc.meta %>%
                       dplyr::select(id, top_nt,
                                     side, region, super_class, 
                                     hemilineage, cell_function, nerve, 
                                     composite_cell_type, cell_class, cell_sub_class, 
                                     cell_type, fafb_cell_type, manc_cell_type) %>%
                       dplyr::mutate(cell_type = dplyr::case_when(
                         !is.na(cell_type) ~ cell_type,
                         TRUE ~ id, 
                       )) %>%
                       dplyr::distinct(id, .keep_all = TRUE),
                     by = "id") %>%
    dplyr::mutate(
      neck_group = dplyr::case_when(
        id %in% banc.in ~ "IN",
        TRUE ~ "OUT"
      ),
      group = paste0(super_class,"_", neck_group),
      label = ifelse(!is.na(cell_function),cell_type,NA)) %>%
    dplyr::ungroup()
  
  # Perform  clustering
  dist_matrix <- dist(umap_result[,1:2], method = "euclidean")
  hc <- hclust(dist_matrix, method = "ward.D2")
  dynamic_clusters <- cutreeDynamic(hc, 
                                    distM = as.matrix(dist_matrix),
                                    deepSplit = 4, 
                                    minClusterSize = 2)  
  umap.dn.df$clusterno <- factor(dynamic_clusters)
  
  # Calculate centroids of clusters
  centroids <- umap.dn.df %>%
    dplyr::group_by(clusterno) %>%
    dplyr::summarize(UMAP1_centroid = mean(UMAP1),
                     UMAP2_centroid = mean(UMAP2))
  
  # Calculate pairwise distances between centroids
  dist_matrix <- dist(centroids[, c("UMAP1_centroid", "UMAP2_centroid")], method = "euclidean")
  
  # Order clusters based on hierarchical clustering
  hc <- hclust(dist_matrix, method = "ward.D2")
  dd <- as.dendrogram(hc)
  ordered_cluster <- 1:length(order.dendrogram(dd))
  names(ordered_cluster) <- order.dendrogram(dd)
  
  # Map original cluster numbers to new ordered cluster numbers
  umap.dn.df$cluster <- ordered_cluster[as.character(umap.dn.df$cluster)]
  umap.dn.df$cluster <- factor(umap.dn.df$cluster, levels = unique(umap.dn.df$cluster))
  
  # Ensure we have enough colors for all clusters
  n_clusters <- length(unique(umap.dn.df$cluster))
  cluster_colors <- cerise_limon_palette(n_clusters)
  names(cluster_colors) <- sort(unique(umap.dn.df$cluster))
  umap.dn.df$colours <- cluster_colors[umap.dn.df$cluster]
  
  # # # Perform spectral clustering on tjhe UMAP points themselves
  # sc <- kernlab::specc(umap_result_n,26)
  # umap.dn.df$clusterno <- factor(sc)
  
}else{
  
  # preloaded
  umap.dn.df <- umap.dn.df %>%
    dplyr::filter(cluster!="0")
  
}

# Calculate cluster centroids
cluster_centroids <- umap.dn.df %>%
  dplyr::filter(cluster!="0",
                !is.na(UMAP1)) %>%
  mutate(cluster = gsub("AN_|DN_|EFF_","",cluster)) %>%
  group_by(cluster) %>%
  summarise(UMAP1 = mean(UMAP1),
            UMAP2 = mean(UMAP2))

# Calculate concave hulls for each cluster
hulls <- umap.dn.df %>%
  dplyr::filter(cluster!="0",
                !is.na(UMAP1),
                !is.na(UMAP2)) %>%
  mutate(cluster = gsub("AN_|DN_|EFF_","",cluster)) %>%
  dplyr::group_by(cluster)   %>%
  do({
    cluster_id <- unique(.$cluster)
    hull_data <- concaveman::concaveman(as.matrix(.[, c("UMAP1", "UMAP2")]),
                                        concavity = 2, length_threshold = 0.5)
    as.data.frame(hull_data) %>%
      dplyr::mutate(cluster = cluster_id)
  }) %>%
  dplyr::ungroup()

#################
### PLOT UMAP ###
#################

# Plot UMAP with convex hulls
g.dn.clusters <- ggplot() +
  geom_polygon(data = hulls, 
               aes(x = V1, y = V2, group = factor(cluster)), 
               alpha = 0.2, fill = "grey90", color = "black", linetype = "dotted") +
  geom_point(data = umap.dn.df, 
             aes(x = UMAP1, y = UMAP2, color = super_class),
             fill = "white",
             size = 2,
             shape = 19,
             alpha = 0.75) +
  scale_color_manual(values = paper.cols) +
  scale_fill_manual(values = paper.cols) +
  geom_text(data = cluster_centroids, 
            aes(x = UMAP1, y = UMAP2, label = cluster),
            colour = "black",
            size = 8, 
            fontface = "bold") +
  theme_void() +
  theme(legend.position = "bottom",
        legend.key.size = unit(0.75, "lines"),
        legend.text = element_text(size = rel(0.75)),
        legend.title = element_text(size = rel(0.75)),
        legend.spacing.x = unit(0.75, "lines"),
        legend.spacing.y = unit(0.75, "lines"),
        legend.margin = margin(t = 3, r = 3, b = 3, l = 3, unit = "pt")
  ) +
  ggplot2::coord_fixed()

# Display plot
print(g.dn.clusters)
ggsave(plot = g.dn.clusters,
       filename = file.path(banc.fig3.path,"neck_inout_connectivity_cosine_umap_hull.pdf"),
       width = 8, height = 8, dpi = 300)
ggsave(plot = convert_to_dark_mode(g.dn.clusters),
       filename = file.path(banc.fig3.extra.path,"dark_mode_neck_inout_connectivity_cosine_umap_hull.pdf"),
       width = 8, height = 8, dpi = 300)

################################
### SUPER CLUSTER MEMBERSHIP ###
################################

# Plot UMAP with convex hulls
g.super.clusters <- ggplot() +
  geom_polygon(data = hulls, 
               aes(x = V1, y = V2, group = factor(cluster)), 
               alpha = 0.2, fill = "grey90", color = "black", linetype = "dotted") +
  geom_point(data = umap.dn.df %>%
               dplyr::filter(!is.na(super_cluster)), 
             aes(x = UMAP1, y = UMAP2, color = super_cluster),
             fill = "white",
             size = 2,
             shape = 19,
             alpha = 0.9) +
  scale_color_manual(values = paper.cols) +
  geom_text(data = cluster_centroids, 
            aes(x = UMAP1, y = UMAP2, label = cluster),
            colour = "black",
            size = 8, 
            fontface = "bold") +
  theme_void() +
  theme(legend.position = "bottom",
        legend.key.size = unit(0.75, "lines"),
        legend.text = element_text(size = rel(0.75)),
        legend.title = element_text(size = rel(0.75)),
        legend.spacing.x = unit(0.75, "lines"),
        legend.spacing.y = unit(0.75, "lines"),
        legend.margin = margin(t = 3, r = 3, b = 3, l = 3, unit = "pt")
  ) +
  ggplot2::coord_fixed()

# Display plot
print(g.super.clusters)
ggsave(plot = g.super.clusters,
       filename = file.path(banc.fig3.path,"neck_inout_connectivity_cosine_umap_super_clusters.pdf"),
       width = 8, height = 8, dpi = 300)
ggsave(plot = convert_to_dark_mode(g.dn.clusters),
       filename = file.path(banc.fig3.extra.path,"dark_mode_neck_inout_connectivity_cosine_umap_super_clusters.pdf"),
       width = 8, height = 8, dpi = 300)

#####################
### CELL FUNCTION ###
#####################

# Update the plot
cfs <- na.omit(unique(umap.dn.df$cell_function)) 
if(length(cfs)>25){
  shapes <- sample(1:25,length(cfs),replace = FALSE)
}else{
  shapes <- 25:(25-length(cfs)+1)
}
names(shapes) <- cfs
g.dn.clusters.hulls <- ggplot(umap.dn.df, 
                              aes(x = UMAP1, 
                                  y = UMAP2)) +
  geom_polygon(data = hulls, 
               aes(x = V1, y = V2, group = factor(cluster)), 
               alpha = 0.2, fill = "grey90", color = "black", linetype = "dotted") +
  geom_point(data = subset(umap.dn.df, is.na(cell_function)), 
             color = 'darkgrey',
             alpha = 0.5, 
             size = 0.5) +
  geom_point(data = subset(umap.dn.df, !is.na(cell_function)), 
             aes(color = cell_function, fill = cell_function, shape = cell_function),
             stroke = 1,
             alpha = 0.99, 
             size = 3) +
  scale_color_cerise_limon(guide = guide_legend(title = "function:")) +
  scale_fill_cerise_limon(guide = guide_legend(title = "function:")) +
  scale_shape_manual(values = shapes) +
  theme_void() +
  labs(title = "",
       x = "UMAP1",
       y = "UMAP2") +
  guides(
    color = guide_legend(nrow = 4, byrow = TRUE, override.aes = list(size=4)),
    fill = guide_legend(nrow = 4, byrow = TRUE),
    shape = guide_legend(nrow = 4, byrow = TRUE, override.aes = list(size=4))
  ) +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal", 
    legend.title=element_blank(),
    legend.text = element_text(size = 9, color = "black"),
    plot.margin = margin(t = 0, r = 50, b = 0, l = 50, unit = "pt")
  )  +
  ggplot2::coord_fixed()

# Show
print(g.dn.clusters.hulls)

# Save
ggsave(plot = g.dn.clusters.hulls,
       filename = file.path(banc.fig3.supp.path,"neck_inout_connectivity_umap_hulls_shapes.pdf"),
       width = 9, height = 8, dpi = 300)
ggsave(plot = convert_to_dark_mode(g.dn.clusters.hulls),
       filename = file.path(banc.fig3.extra.path,"dark_mode_neck_inout_connectivity_umap_hulls_shapes.pdf"),
       width = 8, height = 8, dpi = 300)

# Now with labels
g.dn.clusters.hulls <- ggplot(umap.dn.df, 
                              aes(x = UMAP1, 
                                  y = UMAP2)) +
  geom_polygon(data = hulls, 
               aes(x = V1, y = V2, group = factor(cluster)), 
               alpha = 0.2, fill = "grey90", color = "black", linetype = "dotted") +
  geom_point(data = subset(umap.dn.df, is.na(cell_function)), 
             color = 'darkgrey',
             alpha = 0.5, 
             size = 0.5) +
  geom_point(data = subset(umap.dn.df, !is.na(cell_function)), 
             aes(color = cell_function, fill = cell_function),
             stroke = 1,
             alpha = 0.99, 
             size = 1.5) +
  ggrepel::geom_label_repel(
    data = umap.dn.df %>%
      dplyr::filter(!is.na(label)) %>%
      dplyr::distinct(cell_type, label, .keep_all = TRUE),
    aes(label=label, color = cell_function, fill = cell_function),
    color = "white",
    box.padding = 0.75,  
    point.padding = 0.5,   
    label.padding = 0.25, 
    min.segment.length = 0, 
    max.overlaps = Inf,
    segment.color = "black",
    show.legend = FALSE,
    size = 1.5
  ) + 
  scale_color_cerise_limon(guide = guide_legend(title = "function:")) +
  scale_fill_cerise_limon(guide = guide_legend(title = "function:")) +
  scale_shape_manual(values = shapes) +
  theme_void() +
  labs(title = "",
       x = "UMAP1",
       y = "UMAP2") +
  guides(
    color = guide_legend(nrow = 4, byrow = TRUE, override.aes = list(size=4)),
    fill = guide_legend(nrow = 4, byrow = TRUE),
    shape = guide_legend(nrow = 4, byrow = TRUE, override.aes = list(size=4))
  ) +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal", 
    legend.title=element_blank(),
    legend.text = element_text(size = 9, color = "black"),
    plot.margin = margin(t = 0, r = 50, b = 0, l = 50, unit = "pt")
  )  +
  ggplot2::coord_fixed()

# Save
print(g.dn.clusters.hulls)
ggsave(plot = g.dn.clusters.hulls,
       filename = file.path(banc.fig3.supp.path,"neck_inout_connectivity_umap_hulls_labels.pdf"),
       width = 8, height = 8, dpi = 300)
ggsave(plot = convert_to_dark_mode(g.dn.clusters.hulls),
       filename = file.path(banc.fig3.extra.path,"dark_mode_neck_inout_connectivity_umap_hulls_labels.pdf"),
       width = 8, height = 8, dpi = 300)

#############
### ICONS ###
#############

# Add icons path
icon_folder <- "figures/schematics/umap_icons"
umap.dn.df$image <- ifelse(
  !is.na(umap.dn.df$cell_function),
  file.path(icon_folder, paste0(umap.dn.df$cell_function, ".svg")),
  NA_character_
)
umap.dn.df$image[!file.exists(umap.dn.df$image) & !is.na(umap.dn.df$image)] <- NA

# Plot
g.dn.clusters.icons <- ggplot(umap.dn.df, 
                              aes(x = UMAP1, y = UMAP2)) +
  geom_polygon(
    data = hulls, 
    aes(x = V1, y = V2, group = factor(cluster)), 
    alpha = 0.2, fill = "grey90", color = "black", linetype = "dotted", inherit.aes = FALSE
  ) +
  # Plot gray points for NAs
  geom_point(
    data = subset(umap.dn.df, is.na(cell_function)), 
    color = 'darkgrey',
    alpha = 0.5, 
    size = 0.5
  ) +
  # Plot PNG icon for each point (except NAs)
  ggimage::geom_image(
    data = subset(umap.dn.df, !is.na(cell_function)), 
    aes(image = image), 
    size = 0.03,
    asp = 1
  ) +
  theme_void() +
  labs(title = "", x = "UMAP1", y = "UMAP2") +
  theme(
    plot.margin = margin(t = 0, r = 50, b = 0, l = 50, unit = "pt")
  )  +
  ggplot2::coord_fixed()

# Show
print(g.dn.clusters.icons)

# Save
ggsave(plot = g.dn.clusters.icons,
       filename = file.path(banc.fig3.path,"neck_inout_connectivity_umap_hulls_icons.pdf"),
       width = 9, height = 8, dpi = 300)
ggsave(plot = convert_to_dark_mode(g.dn.clusters.icons),
       filename = file.path(banc.fig3.extra.path,"dark_mode_neck_inout_connectivity_umap_hulls_icons.pdf"),
       width = 8, height = 8, dpi = 300)

####################
### BRAUN GROUPS ###
####################

# Get groups
braun.df <- readxl::read_excel("data/braun_et_al/41586_2024_7523_MOESM4_ESM.xlsx", 
                               sheet = "DN cluster behaviors") %>%
  dplyr::mutate(behavior = dplyr::case_when(
    `Cluster number in figure`==1 ~ "grooming",
    `Cluster number in figure`==2 ~ "escape_takeoff",
    `Cluster number in figure`==3 ~ "walking",
    `Cluster number in figure`==4 ~ "flight 4",
    `Cluster number in figure`==9 ~ "steering",
    `Cluster number in figure`==10 ~ "flight 10",
    `Cluster number in figure` > 12 ~ NA,
    is.na(`Cluster number in figure`) ~ NA,
    TRUE ~ paste0("unknown ",`Cluster number in figure`)
  )) %>%
  dplyr::distinct(cell_type=`DN name`,
                  behavior) %>%
  dplyr::distinct(cell_type,.keep_all = TRUE)

#  Plotting df
umap.braun.dn.df <- umap.dn.df %>%
  dplyr::mutate(cell_type = gsub("_.*","",cell_type)) %>%
  dplyr::left_join(braun.df, 
                   by = "cell_type") %>%
  dplyr::filter(behavior%in%names(paper.cols))

# Plot
g.dn.clusters.braun <- ggplot(umap.braun.dn.df, 
                              aes(x = UMAP1, y = UMAP2)) +
  geom_polygon(
    data = hulls, 
    aes(x = V1, y = V2, group = factor(cluster)), 
    alpha = 0.2, fill = "grey90", color = "black", linetype = "dotted", inherit.aes = FALSE
  ) +
  geom_point(
    data = subset(umap.braun.dn.df, is.na(behavior)), 
    color = 'darkgrey',
    alpha = 0.5, 
    size = 0.5
  ) +
  geom_point(
    data = subset(umap.braun.dn.df, !is.na(behavior)),
    mapping = aes(fill = behavior,color = behavior),
    alpha = 1, 
    size = 2.5
  ) +
  geom_text(data = cluster_centroids, 
            aes(x = UMAP1, y = UMAP2, label = cluster),
            colour = "black",
            size = 8, 
            fontface = "bold") +
  theme_void() +
  labs(title = "", x = "UMAP1", y = "UMAP2") +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal", 
    legend.title=element_blank(),
    legend.text = element_text(size = 9, color = "black"),
    plot.margin = margin(t = 0, r = 50, b = 0, l = 50, unit = "pt")
  )  +
  ggplot2::coord_fixed() +
  scale_color_manual(values = paper.cols)

# Show
print(g.dn.clusters.braun)

# Save
ggsave(plot = g.dn.clusters.braun,
       filename = file.path(banc.fig3.supp.path,"neck_inout_connectivity_umap_hulls_braun.pdf"),
       width = 9, height = 8, dpi = 300)

############################
### HIGH LEVEL INFLUENCE ###
############################

# Connect to .sql file
con <- DBI::dbConnect(RSQLite::SQLite(),
                      file.path(banc.dropbox.influence.save.path,influence.sqlite))
chosen.seeds <- na.omit(unique(banc.an.dn.meta$seed_12))
influence.db.dn <- dplyr::tbl(con, influence.table) %>%
  dplyr::filter(!is_seed,
                level %in% c("seed_12"),
                seed %in% !!chosen.seeds,
                id %in% !!banc.targets$root_id) %>%
  dplyr::collect() %>%
  dplyr::left_join(banc.targets %>%
                 dplyr::select(root_id,
                               target = super_class),
               by = c("id"="root_id"))
dbDisconnect(con)

# Wrangle for
inf.metric <- "influence_log_minmax"
super.classes <-c("motor","visceral_circulatory","visual_centrifugal")
for(super.class in super.classes){
  
  influence.df <- influence.db.dn %>%
    dplyr::ungroup() %>%
    calculate_influence_norms() %>%
    dplyr::select(-id)
  
  # Append to UMAP
  umap_dn_df.bp <- umap.dn.df %>%
    # dplyr::left_join(banc.an.dn.meta %>%
    #                    dplyr::select(root_id, seed_12),
    #                  by = c("id"="root_id")) %>%
    dplyr::left_join(influence.df %>%
                     dplyr::filter(target == super.class),
                     by = c("seed_12"="seed")) %>%
    dplyr::distinct(id, .keep_all = TRUE)
  umap_dn_df.bp$influence_score <- umap_dn_df.bp[[inf.metric]]
  umap_dn_df.bp <- umap_dn_df.bp %>%
    dplyr::arrange(dplyr::desc(influence_score))
  
  # Colour scale
  thresh.high <- quantile(influence.df[[inf.metric]],0.95, na.rm=TRUE)
  thresh.low <- quantile(influence.df[[inf.metric]],0.5, na.rm=TRUE)
  scaled_heatmap_breaks <- seq(thresh.low, thresh.high, length.out = n_breaks)
  scaled_heatmap_palette <- colorRampPalette(c("#1f4e79", "#4a90a4", "#7ba7bc", "#a67c8a", "#c4967d", "#b22222"))(n_breaks - 1)
  umap_dn_df.bp$influence_score[umap_dn_df.bp$influence_score>thresh.high] <- thresh.high
  umap_dn_df.bp$influence_score[umap_dn_df.bp$influence_score<thresh.low] <- thresh.low
  umap_dn_df.bp <- dplyr::arrange(umap_dn_df.bp, influence_score)
  
  # Plot UMAP, DNs scaled for body part
  p_hulls.bp <-  ggplot(data = umap_dn_df.bp, 
                        aes(x = UMAP1, y = UMAP2)) +
    geom_polygon(data = hulls, 
                 aes(x = V1, y = V2, group = factor(cluster)), 
                 alpha = 0.2, 
                 fill = "grey90", 
                 color = "black", 
                 linetype = "dotted") +
    geom_point(data = subset(umap_dn_df.bp, is.na(influence_score)), 
               alpha = 1, 
               size = 2, 
               col = "grey30") +
    geom_point(data = subset(umap_dn_df.bp, !is.na(influence_score)), 
               aes(color=influence_score), 
               alpha = 1, 
               size = 2) +
    scale_color_gradientn(colours = scaled_heatmap_palette,
                          values = scales::rescale(scaled_heatmap_breaks),
                          limits = c(thresh.low, thresh.high),
                          na.value = "grey30") +
    theme_void() +
    labs(title = "",
         x = "UMAP1",
         y = "UMAP2") +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 6), 
      legend.title = element_text(size = 8), 
      legend.key.size = unit(0.5, "cm")
    ) +
    geom_text(data = cluster_centroids,
              aes(label = cluster),
              colour = "black",
              size = 8,
              fontface = "bold") +
    labs(color = paste0(super.class, " : ",inf.metric)) +
    ggplot2::coord_fixed()
  
  # Save
  print(p_hulls.bp)
  ggsave(plot = p_hulls.bp,
         filename = file.path(banc.fig3.path, 
                              sprintf("%s_neck_influence_umap_by_%s.pdf",inf.metric, super.class)),
         width = 8, height = 8, dpi = 300, bg = "transparent")
}

##############################
### WHAT ARE OUR CLUSTERS? ###
##############################

# Write data output
if(recalculate){
  umap.dn.df <- umap.dn.df %>%
    dplyr::mutate(cluster = dplyr::case_when(
      grepl("descending",super_class) ~ gsub("DN_|DN_0","",cluster),
      grepl("ascending",super_class) ~ gsub("AN_|AN_0","",cluster),
      TRUE ~ gsub("other_|other_0","",cluster)
    )) %>%
    dplyr::mutate(cluster = dplyr::case_when(
      grepl("descending",super_class) ~ paste0("DN_",str_pad(cluster,width=2,pad="0")),
      grepl("ascending",super_class) ~ paste0("AN_",str_pad(cluster,width=2,pad="0")),
      TRUE ~ paste0("other_",str_pad(cluster,width=2,pad="0"))
    )) 
  write_csv(x=umap.dn.df,
            file = "data/banc_neck_functional_classes.csv")
  umap.dn.df_n <- banc.meta %>%
    dplyr::filter(grepl("descending|ascending",super_class)) %>%
    dplyr::select(id=root_id,
                  supervoxel_id,
                  composite_cell_type,
                  super_class,
                  cell_type,
                  side) %>%
    dplyr::left_join(umap.dn.df %>%
                       dplyr::distinct(composite_cell_type,UMAP1,UMAP2,cluster,colours) %>%
                       dplyr::distinct(composite_cell_type, .keep_all = TRUE),
                     by = c("composite_cell_type"))
  write_csv(x=umap.dn.df_n,
            file = "data/banc_neck_functional_classes_by_neuron.csv")
  table(umap.dn.df$cluster)  
}

##############################
### WHAT ARE OUR CLUSTERS? ###
##############################

# --- 1. Create hover column for points ---
umap.dn.df <- dplyr::mutate(
  umap.dn.df,
  hover = paste(
    "composite_cell_type:", composite_cell_type,
    "cell_function:", cell_function,
    "<br>root_id:", id
  )
)

# --- 2. Build color palette for clusters ---
cluster_ids <- sort(unique(umap.dn.df$cluster))
n_clust <- length(cluster_ids)
cols <- cerise_limon_palette(n_clust)
names(cols) <- cluster_ids

# --- 3. Prepare plot, add hulls first (NO hover) ---

# 1. Start with an empty plot
p <- plotly::plot_ly()

# 2. Add hull polygons (non-interactive)
for (cl in unique(hulls$cluster)) {
  hull_dat <- dplyr::filter(hulls, cluster == cl)
  hull_dat <- rbind(hull_dat, hull_dat[1, , drop = FALSE])
  hull_dat$hover <- ""  # dummy, keeps plotly happy
  p <- plotly::add_trace(
    p,
    data = hull_dat,
    x = ~V1, y = ~V2,
    type = "scatter", mode = "lines",
    fill = "toself",
    fillcolor = "rgba(220,220,220,0.2)",
    line = list(color = "black", dash = "dot", width = 1),
    showlegend = FALSE,
    hoverinfo = "none",
    text = NULL # ensures no hover on hulls
  )
}

# --- 4. Then points, ON TOP, with interactive hover & togglable legend (by cluster) ---
p <- plotly::add_trace(
  p,
  data = umap.dn.df,
  x = ~UMAP1, y = ~UMAP2,
  type = "scatter",
  mode = "markers",
  color = ~cluster,
  colors = cols,
  text = ~hover,
  hoverinfo = "text",
  marker = list(size = 8, line = list(width = 1, color = "white"))
)
# Now legend toggles clusters, hover shows your labels

# --- 5. Add centroid labels (optional, as black bold text) ---
p <- plotly::add_text(
  p,
  data = cluster_centroids,
  x = ~UMAP1, y = ~UMAP2, text = ~cluster,
  textposition = "middle center",
  textfont = list(color = "black", size = 16, family = "Arial Black"),
  showlegend = FALSE,
  hoverinfo = "none"
)

# --- 6. Final formatting ---
p <- plotly::layout(
  p,
  title = "",
  xaxis = list(title = "", zeroline = FALSE, showline = FALSE, showticklabels = FALSE, showgrid = FALSE),
  yaxis = list(title = "", zeroline = FALSE, showline = FALSE, showticklabels = FALSE, showgrid = FALSE),
  legend = list(orientation = "h", x = 0.1, y = -0.1)
)

# --- 7. Save as HTML ---
htmlwidgets::saveWidget(
  p,
  file.path(banc.fig3.extra.path, "neck_umap_interactive.html"),
  selfcontained = TRUE
)

