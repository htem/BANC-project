##############################
## FIGURE 2: EFFERENT UMAP ##
##############################
# Generates UMAP visualisation of motor effector neurons based on
# connectivity influence patterns. Shows functional clustering of
# motor neurons controlling different body parts and effector types.
# Output: figures/figure2/eff_umap_*.pdf

####################
## STARTUP        ##
####################

source("R/startup/banc-startup.R")
source("R/startup/franken-meta.R")
source("R/startup/banc-meta.R")

####################
## METADATA PREP  ##
####################

# Enhance effector metadata with detailed functional annotations
banc.eff2.meta <- banc.eff.meta %>%
  dplyr::mutate(body_part_effector = dplyr::case_when(
    grepl("power|steering|tension",cell_function) ~ cell_function,
    grepl("pitch|yaw|roll",cell_function_detailed) ~ cell_function_detailed,
    TRUE ~ body_part_effector
  ))

# Set recalculation flag for UMAP generation
recalculate <- FALSE

# Filter problematic neuron types from analysis
weird <- c("DNxl080", "DNge079", "DNg73", "DNg65")
banc.an.dn.meta <- banc.neck.meta %>%
  dplyr::filter(super_class %in% c("ascending","descending")) %>%
  dplyr::filter(!cell_type %in% weird) %>%
  dplyr::filter(!grepl("^SA|^SN|^AN_4|^AN_5",cell_type))
chosen.cts <- banc.an.dn.meta %>%
  distinct(composite_cell_type, side) %>%
  group_by(composite_cell_type) %>%
  summarise(
    sides = list(sort(unique(side)))
  ) %>%
  filter(
    all(c("left", "right") %in% sides)
    |
      (identical(sides[[1]], "center") | identical(sides[[1]], c("center")))
  ) %>%
  pull(composite_cell_type)

####################
## INFLUENCE DATA ##
####################

# Extract neck-to-effector influence data for UMAP generation
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

# ##########################################
# ### NECK TO EFFERENT INFLUENCE HEATMAP ###
# ##########################################
# inf.metric <- "influence_norm_log"
# 
# # Data manipulation and annotation creation
# heatmap_matrix <- reshape2::acast(
#   data = influence.neck.eff.db,
#   formula = id ~ seed,
#   value.var = inf.metric,
#   fun.aggregate = function(x) mean(x, na.rm = TRUE)
# )
# heatmap_matrix[is.na(heatmap_matrix)] <- 0
# heatmap_matrix[is.infinite(heatmap_matrix)] <- 0
# 
# # Create scaled color palette 
# scaled_heatmap_breaks <- seq(quantile(heatmap_matrix,0.01, na.rm=TRUE), quantile(heatmap_matrix,0.999, na.rm=TRUE), length.out = n_breaks)
# scaled_heatmap_palette <- colorRampPalette(c("#1f4e79", "#4a90a4", "#7ba7bc", "#a67c8a", "#c4967d", "#b22222"))(n_breaks - 1)
# 
# # Create annotation for cell types (rows)
# cell_type_annotation <- influence.neck.eff.db %>%
#   dplyr::distinct(id, post_body_part_effector, post_region) %>%
#   dplyr::mutate(post_body_part_effector=ifelse(is.na(post_body_part_effector),"unknown",post_body_part_effector)) %>%
#   dplyr::mutate(
#     post_region = factor(post_region, levels = class.order)
#   ) %>%
#   dplyr::arrange(post_region, post_body_part_effector) %>%
#   dplyr::distinct(id, .keep_all = TRUE) %>%
#   dplyr::filter(!is.na(id), id %in% rownames(heatmap_matrix)) %>%
#   column_to_rownames("id")
# 
# # Reorder rows by super_class and cluster
# heatmap_matrix <- heatmap_matrix[rownames(heatmap_matrix) %in% rownames(cell_type_annotation),]
# cell_type_annotation <- cell_type_annotation[rownames(cell_type_annotation)%in%rownames(heatmap_matrix),]
# 
# # Group cell types by super_class
# groups <- split(rownames(cell_type_annotation), cell_type_annotation$post_body_part_effector)
# 
# # Filter out groups with fewer than two elementshclust_semisupervised
# groups <- lapply(groups, function(g) if(length(g) >= 2) g else NULL)
# groups <- groups[!sapply(groups, is.null)]
# 
# # Apply semi-supervised clustering
# clustering_result <- hclust_semisupervised(data = heatmap_matrix,
#                                            groups = groups,
#                                            dist_method = "euclidean",
#                                            hclust_method = "ward.D2")
# heatmap_matrix_normalized <- clustering_result$data
# cell_type_annotation <- cell_type_annotation[rownames(heatmap_matrix_normalized), , drop = FALSE]
# 
# # Annotation colors
# annotation_colors <- list(
#   post_region = paper.cols[names(paper.cols) %in% unique(cell_type_annotation$post_region)]
# )
# 
# # Cosine similarity
# # cosine_sim_matrix_cols <- lsa::cosine(heatmap_matrix_normalized)
# # cosine_sim_matrix_cols[is.na(cosine_sim_matrix_cols)] <- 0
# # cosine_dist_matrix_cols <- hclust(as.dist(1 - cosine_sim_matrix_cols), method = "ward.D2")
# col_dist <- dist(t(heatmap_matrix_normalized), method = "euclidean")
# euclidean_dist_matrix_cols <- hclust(col_dist, method = "ward.D2")
# 
# # Create the heatmap
# pheatmap(
#   heatmap_matrix_normalized,
#   color = scaled_heatmap_palette,
#   breaks = scaled_heatmap_breaks,
#   annotation_row = cell_type_annotation,
#   annotation_colors = annotation_colors,
#   clustering_method = "ward.D2",
#   cluster_rows = clustering_result$hclust,
#   cluster_cols = euclidean_dist_matrix_cols,
#   treeheight_row = 0,
#   treeheight_col = 0,
#   show_rownames = FALSE,
#   show_colnames = TRUE,
#   fontsize_row = 6,
#   fontsize_col = 10,
#   width = 100,
#   height = 100,
#   annotation_names_col = FALSE,
#   annotation_names_row = FALSE,
#   filename = file.path(banc.fig2.extra.path, sprintf("%s_neck_to_effectors_heatmap.pdf",inf.metric))
# )

##########################
## NECK-EFFECTOR DATA   ##
##########################

# Extract neck neuron influence on effector targets
chosen.cts <- unique(banc.an.dn.meta$seed_12)
con <- DBI::dbConnect(RSQLite::SQLite(),
                      file.path(banc.dropbox.influence.save.path,influence.sqlite))
influence.nn.eff.db <- dplyr::tbl(con, influence.table) %>%
  dplyr::filter(!is_seed,
                level %in% c("seed_12")) %>%
  dplyr::filter(
    seed %in% !!chosen.cts,
    id %in% !!banc.eff2.meta$id
  ) %>%
  dplyr::collect() 
dbDisconnect(con)

########################
## UMAP GENERATION    ##
########################

# Generate UMAP embedding based on connectivity influence patterns

if(recalculate){
 
  # Create influence matrix for UMAP input
  influence.for.m <- reshape2::acast(data = influence.nn.eff.db %>%
                                       calculate_influence_norms(), 
                                     formula = id ~ seed, 
                                     value.var = "influence_norm_log",
                                     fun.aggregate = mean,
                                     fill = 0)
  not_all_na1 <- rowSums(!is.na(influence.for.m)) > 0
  not_all_na2 <- colSums(!is.na(influence.for.m)) > 0
  influence.for.m <- influence.for.m[not_all_na1, not_all_na2]
  influence.m <- influence.for.m
  influence.m[is.na(influence.m)] <- 0
  
  # Clean and prepare similarity matrix
  sim_matrix <- influence.m
  sim_matrix[is.infinite(sim_matrix)] <- 0
  sim_matrix <- sim_matrix[!apply(sim_matrix, 1, function(row) all(is.na(row))), ]
  sim_matrix <- sim_matrix[, !apply(sim_matrix, 2, function(col) all(is.na(col)))]
  sim_matrix <- sim_matrix[!apply(sim_matrix, 1, function(row) all(row==0)), ]
  sim_matrix <- sim_matrix[, !apply(sim_matrix, 2, function(col) all(col==0))]
  
  # Apply PCA to determine optimal dimensionality
  pca_result <- prcomp(sim_matrix, 
                       center = TRUE, 
                       scale. = FALSE)
  
  # Calculate cumulative explained variance ratio
  var_explained <- pca_result$sdev^2 / sum(pca_result$sdev^2)
  cumulative_var_explained <- cumsum(var_explained)
  
  # Find number of components that explain 50% of variance
  n_components <- which(cumulative_var_explained >= 0.95)[1]
  
  # Plot cumulative explained variance
  plot(cumulative_var_explained, 
       xlab = "Number of Components", 
       ylab = "Cumulative Proportion of Variance Explained",
       type = "b")
  abline(h = 0.50, col = "red", lty = 2)
  abline(v = n_components, col = "blue", lty = 2)
  
  # Print the number of components
  print(paste("Number of components explaining 95% of variance:", n_components))
  
  # Generate 2D UMAP embedding for visualisation
  set.seed(42)  
  umap_result <- uwot::umap(sim_matrix,
                            metric = "cosine",
                            n_epochs = 500,
                            n_neighbors = 100, 
                            min_dist = 0.1,
                            n_trees = 100,
                            n_components = 2)
  umap_result_n <- uwot::umap(sim_matrix,
                              metric = "cosine",
                              n_epochs = 500,
                              n_neighbors = 100, 
                              min_dist = 0.1,
                              n_trees = 100,
                              n_components = n_components)
  
  # Combine UMAP coordinates with effector metadata
  umap_eff_df <- data.frame(
    UMAP1 = umap_result[,1],
    UMAP2 = umap_result[,2],
    id = as.character(rownames(sim_matrix))) %>% 
    dplyr::left_join(banc.eff2.meta %>%
                       dplyr::select(id, top_nt, nerve,
                                     region, super_class, side,
                                     hemilineage, cell_function, cell_function_detailed, nerve, body_part_effector,
                                     composite_cell_type, cell_class, cell_sub_class, 
                                     cell_type) %>%
                       dplyr::distinct(id, .keep_all = TRUE),
                     by = "id") %>%
    dplyr::mutate(body_part_effector = gsub("_"," ", body_part_effector)) %>%
    dplyr::mutate(
      label = body_part_effector) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(nerve_side = dplyr::case_when(
      grepl("right|_R|R$",nerve) ~ "right",
      grepl("left|_L|L$",nerve) ~ "left",
      TRUE ~ side
    ))
  
  # Apply hierarchical clustering to UMAP coordinates
  dist_matrix <- dist(umap_result, method = "euclidean")
  hc <- hclust(dist_matrix, method = "ward.D2")
  dynamic_clusters <- cutreeDynamic(hc, 
                                    distM = as.matrix(dist_matrix),
                                    deepSplit = 4,
                                    minClusterSize = 2) 
  umap_eff_df$unordered_cluster <- dynamic_clusters
  
  # Compute cluster centroids for labelling
  centroids <- umap_eff_df %>%
    dplyr::group_by(unordered_cluster) %>%
    dplyr::summarize(UMAP1_centroid = mean(UMAP1),
                     UMAP2_centroid = mean(UMAP2))
  
  # Calculate pairwise distances between centroids
  dist_matrix <- dist(centroids[, c("UMAP1_centroid", "UMAP2_centroid")], method = "euclidean")
  
  # Order clusters based on hierarchical clustering
  hc1 <- hclust(dist_matrix, method = "ward.D2")
  dd1 <- as.dendrogram(hc1)
  ordered_cluster <- 1:length(order.dendrogram(dd1))
  names(ordered_cluster) <- order.dendrogram(dd1)
  
  # Map original cluster numbers to new ordered cluster numbers
  umap_eff_df$cluster <- ordered_cluster[as.character(umap_eff_df$unordered_cluster)]
  umap_eff_df$cluster <- factor(umap_eff_df$cluster, levels = unique(umap_eff_df$cluster))
  
  # Ensure we have enough colors for all clusters
  n_clusters <- length(unique(umap_eff_df$cluster))
  cluster_colors <- cerise_limon_palette(n_clusters)
  names(cluster_colors) <- sort(unique(umap_eff_df$cluster))
  umap_eff_df$colours <- cluster_colors[umap_eff_df$cluster]
  umap_eff_df <- umap.eff.df %>%
    dplyr::mutate(body_part_effector2 = dplyr::case_when(
      grepl("power|steering|tension",cell_function) ~ cell_function,
      grepl("pitch|yaw|roll",cell_function_detailed) ~ cell_function_detailed,
      TRUE ~ body_part_effector
    ))
  
}else{
  # Use pre-computed UMAP data with updated metadata
  umap_eff_df <- umap.eff.df %>%
    dplyr::mutate(nerve_side = dplyr::case_when(
      grepl("right|_R",nerve) ~ "right",
      grepl("left|_L",nerve) ~ "left",
      TRUE ~ side
    )) %>%
    dplyr::mutate(body_part_effector2 = dplyr::case_when(
      grepl("power|steering|tension",cell_function) ~ cell_function,
      grepl("pitch|yaw|roll",cell_function_detailed) ~ cell_function_detailed,
      TRUE ~ body_part_effector
    ))  %>%
    dplyr::mutate(nerve_side = dplyr::case_when(
      grepl("right|_R|R$",nerve) ~ "right",
      grepl("left|_L|L$",nerve) ~ "left",
      TRUE ~ side
    ))
  
}

####################
## VISUALISATION   ##
####################

# Calculate cluster centroids for plot labelling
cluster_centroids <- umap_eff_df %>%
  dplyr::filter(cluster!="0",
                !is.na(UMAP1),
                !is.na(UMAP2)) %>%
  mutate(cluster = gsub("AN_|DN_|EFF_","",cluster)) %>%
  group_by(cluster) %>%
  summarise(UMAP1 = mean(UMAP1),
            UMAP2 = mean(UMAP2))

# Generate cluster boundary hulls for visualisation
hulls <- umap_eff_df %>%
  dplyr::filter(cluster!="0",
                !is.na(UMAP1),
                !is.na(UMAP2)) %>%
  group_by(cluster) %>%
  do({
    cluster_id <- unique(.$cluster)
    hull_data <- concaveman::concaveman(as.matrix(.[, c("UMAP1", "UMAP2")]),
                                        concavity = 2, length_threshold = 0.5)
    as.data.frame(hull_data) %>%
      mutate(cluster = cluster_id)
  }) %>%
  ungroup()

# Generate super cluster boundary hulls
super.hulls <- umap_eff_df %>%
  dplyr::filter(super_cluster!="0",
                super_cluster!="",
                !is.na(super_cluster),
                !is.na(UMAP1),
                !is.na(UMAP2)) %>%
  group_by(super_cluster) %>%
  do({
    cluster_id <- unique(.$super_cluster)
    hull_data <- concaveman::concaveman(as.matrix(.[, c("UMAP1", "UMAP2")]),
                                        concavity = 2, length_threshold = 0.5)
    as.data.frame(hull_data) %>%
      mutate(super_cluster = cluster_id)
  }) %>%
  ungroup()

# Define colour and shape mappings for body parts
body.part.shapes <- c("retrocerebral complex" = 21, 
                "corpus allatum" = 24,
                "enteric complex" = 23, 
                "digestive tract" = 22, 
                "crop" = 25, 
                "salivary gland" = 21, 
                "pharynx" = 24, 
                "proboscis" = 23, 
                "antenna" = 22, 
                "eye" = 25, 
                "neck" = 21, 
                "haltere" = 24, 
                "wing" = 23, 
                "front leg" = 22,
                "middle leg" = 25, 
                "hind leg" = 21,
                "ureter" = 24, 
                "abdomen" = 23, 
                "ovaries" = 22, 
                "uterus" = 25, 
                "neurohemal complex" = 21,
                "haltere power" = 3,
                "haltere steering" = 4,
                "wing power" = 3,
                "wing steering"= 4,
                "wing tension" = 8,
                "neck yaw" = 12,
                "neck pitch" = 7,
                "neck roll" = 9,
                "thoracic abdominal segmental" = 25
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
umap_eff_df$body_part_effector2 <- gsub("_"," ",umap_eff_df$body_part_effector2)
umap_eff_df$body_part_effector2 <- factor(umap_eff_df$body_part_effector2, levels = body.parts)

# Generate main UMAP plot with body part colour coding
p_hulls <- ggplot(data = umap_eff_df, aes(x = UMAP1, y = UMAP2)) +
  geom_polygon(data = hulls, 
               aes(x = V1, y = V2, group = factor(cluster)), 
               alpha = 0.2, fill = "white", color = "black", linetype = "dotted") +
  geom_point(data = subset(umap_eff_df, is.na(body_part_effector2)),
             color = 'lightgrey',
             fill = 'lightgrey',
             shape = 21,
             alpha = 0.5, 
             size = 1) +
  geom_point(data = subset(umap_eff_df, 
                           !is.na(body_part_effector2)), 
             aes(color = body_part_effector2,  
                 fill = body_part_effector2, 
                 shape = body_part_effector2), 
             alpha = 0.95, 
             size = 1.5,
             stroke = 1) +
  geom_point(data = subset(umap_eff_df, 
                           !is.na(body_part_effector2)&super_class=="visceral_circulatory"), 
             aes(color = body_part_effector2, 
                 fill = body_part_effector2, 
                 shape = body_part_effector2), 
             alpha = 0.95, 
             size = 0.5,
             color = "white",
             fill = "white") +
  geom_text(data = cluster_centroids, 
            aes(label = cluster),
            colour = "black",
            size = 6, 
            hjust = -1,
            fontface = "bold") +
  scale_color_manual(values = paper.cols) +
  scale_fill_manual(values = paper.cols) +
  scale_shape_manual(values = body.part.shapes) +
  theme_void() +
  labs(title = "",
       x = "UMAP1",
       y = "UMAP2") +
  guides(
    color = guide_legend(ncol = 3, byrow = TRUE, override.aes = list(size=4)),
    fill = guide_legend(ncol = 3, byrow = TRUE),
    shape = guide_legend(ncol = 3, byrow = TRUE, override.aes = list(size=4))
  ) +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal", 
    legend.title=element_blank(),
    legend.text = element_text(size = 9, color = "black"),
    plot.margin = margin(t = 0, r = 50, b = 0, l = 50, unit = "pt")
  ) +
  ggplot2::coord_fixed()

# Display and export main UMAP visualisation
plot(p_hulls)
ggsave(plot = p_hulls,
       filename = file.path(banc.fig2.path, "eff_umap_influence_norm_log_minmax_euclidean_hulls.pdf"),
       width = 12, height = 12, dpi = 300)
ggsave(plot = convert_to_dark_mode(p_hulls),
       filename = file.path(banc.fig2.extra.path, "dark_mode_eff_umap_influence_norm_log_minmax_euclidean_hulls.pdf"),
       width = 12, height = 12, dpi = 300)

# Generate supplementary UMAP showing nerve laterality
p_hulls.side <- ggplot(data = umap_eff_df, aes(x = UMAP1, y = UMAP2)) +
  geom_polygon(data = hulls, 
               aes(x = V1, y = V2, group = factor(cluster)), 
               alpha = 0.2, fill = "white", color = "black", linetype = "dotted") +
  geom_polygon(data = hulls, 
               aes(x = V1, y = V2, group = factor(cluster)), 
               alpha = 0.2, fill = "white", color = "black", linetype = "dotted") +
  geom_point(data = subset(umap_eff_df, is.na(body_part_effector)), 
             color = 'lightgrey',
             alpha = 0.5, 
             size = 1) +
  geom_point(data = subset(umap_eff_df), 
             aes(color = nerve_side,  fill = nerve_side), 
             alpha = 0.95, 
             size = 1.5,
             stroke = 1) +
  geom_text(data = cluster_centroids, 
            aes(label = cluster),
            colour = "black",
            size = 6, 
            hjust = -1,
            fontface = "bold") +
  scale_color_manual(values = paper.cols) +
  scale_fill_manual(values = paper.cols) +
  theme_void() +
  labs(title = "",
       x = "UMAP1",
       y = "UMAP2") +
  guides(
    color = guide_legend(ncol = 4, byrow = TRUE, override.aes = list(size=4)),
    fill = guide_legend(ncol = 4, byrow = TRUE),
    shape = "none"
  ) +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal", 
    legend.title=element_blank(),
    legend.text = element_text(size = 9, color = "black"),
    plot.margin = margin(t = 0, r = 50, b = 0, l = 50, unit = "pt")
  ) +
  ggplot2::coord_fixed()

# Export laterality-based UMAP
plot(p_hulls.side)
ggsave(plot = p_hulls.side,
       filename = file.path(banc.fig2.supp.path, "eff_umap_influence_norm_log_minmax_euclidean_side.pdf"),
       width = 8, height = 8, dpi = 300)

####################
## ALTERNATIVE VIZ ##
####################

# Generate alternative UMAP visualisation with unified point shapes
p_r <- ggplot(data = umap_eff_df, aes(x = UMAP1, y = UMAP2)) +
  geom_polygon(data = hulls, 
               aes(x = V1, y = V2, group = factor(cluster)), 
               alpha = 0.2, fill = "white", color = "black", linetype = "dotted") +
  geom_point(data = subset(umap_eff_df, is.na(body_part_effector)),
             color = 'lightgrey',
             fill = 'lightgrey',
             shape = 21,
             alpha = 0.5, 
             size = 1) +
  geom_point(data = subset(umap_eff_df, 
                           !is.na(body_part_effector)), 
             aes(color = body_part_effector,  
                 fill = body_part_effector), 
             shape = 21,
             alpha = 0.95, 
             size = 1.5,
             stroke = 1) +
  geom_point(data = subset(umap_eff_df, 
                           !is.na(body_part_effector)&super_class=="visceral_circulatory"), 
             aes(color = body_part_effector, 
                 fill = body_part_effector), 
             shape = 21,
             alpha = 0.95, 
             size = 0.5,
             color = "white",
             fill = "white") +
  geom_text(data = cluster_centroids, 
            aes(label = cluster),
            colour = "black",
            size = 6, 
            hjust = -1,
            fontface = "bold") +
  scale_color_manual(values = paper.cols) +
  scale_fill_manual(values = paper.cols) +
  theme_void() +
  labs(title = "",
       x = "UMAP1",
       y = "UMAP2") +
  guides(
    color = guide_legend(ncol = 3, byrow = TRUE, override.aes = list(size=4)),
    fill = guide_legend(ncol = 3, byrow = TRUE),
    shape = guide_legend(ncol = 3, byrow = TRUE, override.aes = list(size=4))
  ) +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal", 
    legend.title=element_blank(),
    legend.text = element_text(size = 9, color = "black"),
    plot.margin = margin(t = 0, r = 50, b = 0, l = 50, unit = "pt")
  ) +
  ggplot2::coord_fixed()

# Export alternative visualisation
plot(p_r)
ggsave(plot = p_r,
       filename = file.path(banc.fig2.extra.path, "eff_umap_influence_norm_log_minmax_euclidean_for_rachel.pdf"),
       width = 12, height = 12, dpi = 300)

####################
## ICON-BASED VIZ  ##
####################

# Generate UMAP with body part icons instead of colour coding
icon_folder <- "figures/schematics/umap_icons_efferent"
umap_eff_df$image <- ifelse(
  !is.na(umap_eff_df$body_part_effector),
  file.path(icon_folder, paste0(umap_eff_df$body_part_effector, ".svg")),
  NA_character_
)
umap_eff_df$image[!file.exists(umap_eff_df$image) & !is.na(umap_eff_df$image)] <- NA

# Create icon-based UMAP visualisation
g.eff.clusters.icons <- ggplot(umap_eff_df, 
                              aes(x = UMAP1, y = UMAP2)) +
  geom_polygon(
    data = hulls,
    aes(x = V1, y = V2, group = factor(cluster)),
    alpha = 1, 
    fill = "grey90", 
    color = NA, 
    inherit.aes = FALSE
  ) +
  # Plot gray points for NAs
  geom_point(
    data = subset(umap_eff_df, is.na(body_part_effector)), 
    color = 'darkgrey',
    alpha = 0.5, 
    size = 0.5
  ) +
  ggsvg::geom_point_svg(
    data = subset(umap_eff_df, !is.na(body_part_effector)), 
    aes(x = UMAP1, y = UMAP2, svg = image), 
    size = 5
  ) +
  geom_point(data = subset(umap_eff_df, 
                           !is.na(body_part_effector2)&super_class=="visceral_circulatory"), 
             alpha = 0.95, 
             size = 0.3,
             shape = 21,
             color = "black",
             fill = "black") +
  theme_void() +
  labs(title = "",
       x = "UMAP1",
       y = "UMAP2") +
  guides(
    color = guide_legend(ncol = 4, byrow = TRUE, override.aes = list(size=4)),
    fill = guide_legend(ncol = 4, byrow = TRUE),
    shape = "none"
  ) +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal", 
    legend.title=element_blank(),
    legend.text = element_text(size = 9, color = "black"),
    plot.margin = margin(t = 0, r = 50, b = 0, l = 50, unit = "pt")
  ) +
  ggplot2::coord_fixed()

# Export icon-based UMAP
plot(g.eff.clusters.icons)
ggsave(plot = g.eff.clusters.icons,
       filename = file.path(banc.fig2.path,"eff_umap_influence_norm_log_minmax_euclidean_icons.pdf"),
       width = 9, height = 8, dpi = 300)
ggsave(plot = convert_to_dark_mode(g.eff.clusters.icons),
       filename = file.path(banc.fig2.extra.path,"eff_umap_influence_norm_log_minmax_euclidean_icons.pdf"),
       width = 8, height = 8, dpi = 300)

######################
## CLUSTER ANALYSIS ##
######################

# Export cluster assignments and assess biological coherence
if(recalculate){
  umap_eff_df <- umap_eff_df %>%
    dplyr::mutate(cluster = dplyr::case_when(
      TRUE ~ paste0("EFF_",str_pad(cluster,width=2,pad="0"))
    ))   
}

# Quantify cluster-to-body-part associations using statistical tests
if(any(!is.na(umap_eff_df$body_part_effector))) {
  contingency_table <- table(umap_eff_df$cluster, umap_eff_df$body_part_effector)
  
  # Use Fisher's exact test instead of Chi-square
  fisher_test <- fisher.test(contingency_table, simulate.p.value = TRUE, B = 10000)
  print(fisher_test)
  
  # Assess body_part_effector homogeneity within clusters
  cluster_homogeneity <- umap_eff_df %>%
    dplyr::filter(!is.na(body_part_effector)) %>%
    dplyr::group_by(cluster) %>%
    dplyr::mutate(cluster_n = dplyr::n()) %>%
    dplyr::group_by(cluster, body_part_effector) %>%
    dplyr::summarise(
      n = dplyr::n(),
      body_part_pct = round(dplyr::n()/cluster_n * 100)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(cluster, dplyr::desc(body_part_pct)) %>%
    dplyr::distinct(cluster, body_part_effector, n, body_part_pct) %>%
    as.data.frame()
  print(cluster_homogeneity)
  
  # Assess cell_sub_class homogeneity within clusters
  cluster_homogeneity2 <- umap_eff_df %>%
    dplyr::filter(!is.na(cell_sub_class)) %>%
    dplyr::group_by(cluster) %>%
    dplyr::mutate(cluster_n = dplyr::n()) %>%
    dplyr::group_by(cluster, cell_sub_class) %>%
    dplyr::summarise(
      n = dplyr::n(),
      body_part_pct = round(dplyr::n()/cluster_n * 100)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(cluster, dplyr::desc(body_part_pct)) %>%
    dplyr::distinct(cluster, cell_sub_class, n, body_part_pct) %>%
    as.data.frame()
  print(cluster_homogeneity2)
  
  
  # Assess cell_function homogeneity within clusters
  cluster_homogeneity3 <- umap_eff_df %>%
    dplyr::filter(!is.na(cell_function)) %>%
    dplyr::group_by(cluster) %>%
    dplyr::mutate(cluster_n = dplyr::n()) %>%
    dplyr::group_by(cluster, cell_function) %>%
    dplyr::summarise(
      n = dplyr::n(),
      body_part_pct = round(dplyr::n()/cluster_n * 100)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(cluster, dplyr::desc(body_part_pct)) %>%
    dplyr::distinct(cluster, cell_function, n, body_part_pct) %>%
    as.data.frame()
  print(cluster_homogeneity3)
  
  # Assess detailed cell_function homogeneity within clusters
  cluster_homogeneity4 <- umap_eff_df %>%
    dplyr::filter(!is.na(cell_function_detailed)) %>%
    dplyr::group_by(cluster) %>%
    dplyr::mutate(cluster_n = dplyr::n()) %>%
    dplyr::group_by(cluster, cell_function_detailed) %>%
    dplyr::summarise(
      n = dplyr::n(),
      body_part_pct = round(dplyr::n()/cluster_n * 100)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(cluster, dplyr::desc(body_part_pct)) %>%
    dplyr::distinct(cluster, cell_function_detailed, n, body_part_pct) %>%
    as.data.frame()
  print(cluster_homogeneity4)
}

# Export cluster assignments to file
if(recalculate){
  write_csv(x=umap_eff_df,
            file = "data/banc_efferent_functional_classes.csv")
  table(umap_eff_df$cluster)
}

##########################
## INFLUENCE MAPPING   ##
##########################

# Generate UMAP overlays showing neck neuron influence on effectors
dn.clusters <- na.omit(unique(umap.dn.df$seed_07))
dn.clusters <- unique(sort(dn.clusters))
for(dc in dn.clusters){
  
  # Map influence scores to effector UMAP coordinates
  umap_eff_df.dc <- umap_eff_df %>%
    dplyr::left_join(influence.neck.eff.db %>%
                       dplyr::filter(seed%in%dc),
                     by = c("id")) %>%
    dplyr::arrange(influence_norm_log_minmax)
  
  # Apply colour scaling for influence visualisation
  if(nrow(umap_eff_df.dc)<2){
    next
  }
  if(all(is.na(umap_eff_df.dc$influence_norm_log_minmax))){
    next
  }
  scaled_heatmap_palette <- colorRampPalette(c("#1f4e79", "#4a90a4", "#7ba7bc", "#a67c8a", "#c4967d", "#b22222"))(n_breaks - 1)
  score_min <- quantile(influence.neck.eff.db$influence_norm_log_minmax,0.1, na.rm=TRUE)
  score_max <- quantile(influence.neck.eff.db$influence_norm_log_minmax,0.95, na.rm=TRUE)
  if(is.na(score_min)){
    next
  }
  if(score_max==0){
    next
  }
  scaled_heatmap_breaks <- seq(score_min, 
                               score_max, 
                               length.out = n_breaks)
  umap_eff_df.dc$influence_norm_log_minmax[umap_eff_df.dc$influence_norm_log_minmax>score_max] <- score_max
  umap_eff_df.dc$influence_norm_log_minmax[umap_eff_df.dc$influence_norm_log_minmax<score_min] <- score_min
  
  # Generate influence-overlaid UMAP visualisation
  p_hulls.bp <-  ggplot(data = umap_eff_df.dc, 
                        aes(x = UMAP1, y = UMAP2)) +
    geom_polygon(data = hulls, 
                 aes(x = V1, y = V2, group = factor(cluster)), 
                 alpha = 0.2, 
                 fill = "grey90", 
                 color = "grey30", 
                 linetype = "dotted") +
    geom_point(data = subset(umap_eff_df.dc, is.na(influence_norm_log_minmax)), alpha = 0.8, size = 2, col = "grey30") +
    geom_point(data = subset(umap_eff_df.dc, !is.na(influence_norm_log_minmax)), aes(color=influence_norm_log_minmax), alpha = 0.9, size = 2) +
    scale_color_gradientn(colours = scaled_heatmap_palette,
                          values = scales::rescale(scaled_heatmap_breaks),
                          limits = c(score_min, score_max),
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
              colour = "grey30",
              size = 6,
              fontface = "bold") +
    ggplot2::coord_fixed() +
    ggplot2::guides(
      color = guide_legend(
        title = paste0(dc, " influence_norm_log_minmax"),
        nrow = 1,
        byrow = TRUE 
      ))
  
  # Export neck neuron influence UMAP
  fp <- file.path(banc.fig2.extra.path, "neck_eff_influence_umaps")
  dir.create(fp, showWarnings = FALSE, recursive = TRUE)
  ggsave(plot = p_hulls.bp,
         filename = file.path(fp, sprintf("neck_influence_umap_by_%s_influence_norm_log_minmax.pdf",dc)),
         width = 5, height = 5, dpi = 300)
}

####################
## INTERACTIVE VIZ ##
####################

# Generate interactive plotly UMAP with hover information
umap_eff_df <- dplyr::mutate(
  umap_eff_df,
  hover = paste(
    "composite_cell_type:", composite_cell_type,
    "cell_function:", cell_function,
    "<br>root_id:", id
  )
)

# Define colour palette for interactive clusters
cluster_ids <- sort(unique(umap_eff_df$cluster))
n_clust <- length(cluster_ids)
cols <- sample(cerise_limon_palette(n_clust))
names(cols) <- sample(cluster_ids)

# Build layered interactive plot starting with hull boundaries
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
    type = "scatter", 
    mode = "lines",
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
  data = umap_eff_df,
  x = ~UMAP1, y = ~UMAP2,
  type = "scatter",
  mode = "markers",
  color = ~cluster,
  color = cols,
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

# Export interactive UMAP as HTML widget
htmlwidgets::saveWidget(
  p,
  file.path(banc.fig2.extra.path, "effector_umap_interactive.html"),
  selfcontained = TRUE
)

