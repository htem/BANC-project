##########################################
### FOCUS ON UMAP CLUSTERS OF INTEREST ###
##########################################

###############
### STARTUP ###
###############

# load
source("R/startup/banc-startup.R")
source("R/startup/franken-meta.R")
source("R/startup/banc-meta.R")
source("R/startup/banc-edgelist.R")
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
  dplyr::filter(grepl("mushroom_body_dopamin|central_complex_input",cell_class)|
                  grepl("visual_projection",super_class)|
                  root_id%in%!!banc.eff.meta$root_id)
banc.sources <- banc.meta %>%
  dplyr::filter(grepl("mushroom_body_output|central_complex_output",cell_class)|
                  grepl("visual_projection",super_class))
vpn.seeds <- na.omit(unique(banc.vpn.meta$seed_07))
names(vpn.seeds) <- vpn.seeds
sensor.seed.map <- c(sensory.seed.map,vpn.seeds)

# Triaged neck neurons
neck.inclusion <- readr::read_csv(file="data/meta/banc_neck_inclusion.csv", 
                                  col_types = banc.col.types)
banc.in <- subset(neck.inclusion,in_group)$root_id
banc.out <- subset(neck.inclusion,!in_group)$root_id

# Recalculate?
recalculate <- FALSE

########################
### INFLUENCE SCORES ###
########################

# Connect to .sql file
con <- DBI::dbConnect(RSQLite::SQLite(),
                      file.path(banc.dropbox.influence.save.path,influence.sqlite))
chosen.seeds <- na.omit(unique(banc.an.dn.meta$seed_12))
chosen.ids <- unique(c(banc.eff2.meta$root_id,banc.an.dn.meta$root_id))
influence.nn.df <- dplyr::tbl(con, influence.table) %>%
  dplyr::filter(level %in% c("seed_12"),
                seed %in% !!chosen.seeds,
                id %in% chosen.ids) %>%
  dplyr::select(seed, level, id, influence_original, influence_norm_original, influence_syn_norm) %>%
  dplyr::collect()
dbDisconnect(con)

# Format
influence.nn.df <- influence.nn.df %>%
  dplyr::left_join(banc.meta %>%
                     dplyr::distinct(root_id, cell_type, cell_sub_class, cell_class, super_class, cell_function),
                   by = c("id"="root_id")) %>%
  dplyr::ungroup() %>%
  calculate_influence_norms()

# Get alternative dataset for validation (seed_02)
con <- DBI::dbConnect(RSQLite::SQLite(),
                      file.path(banc.dropbox.influence.save.path,influence.sqlite))
influence.sens.df <- dplyr::tbl(con, influence.table) %>%
  dplyr::filter(!is_seed,
                level %in% c("seed_02"),
                id %in% !!banc.an.dn.meta$id) %>%
  dplyr::collect() %>%
  dplyr::filter(!grepl("unknown",seed))
dbDisconnect(con)

# Connect to .sql file
con <- DBI::dbConnect(RSQLite::SQLite(),
                      file.path(banc.dropbox.influence.save.path,influence.sqlite))
chosen.cts <- unique(c(banc.sources$seed_07))
influence.vpn.df <- dplyr::tbl(con, influence.table) %>%
  dplyr::filter(!is_seed,
                level %in% c("seed_07"),
                seed %in% !!chosen.cts,
                id %in% !!banc.an.dn.meta$id) %>%
  dplyr::collect()
dbDisconnect(con)

# Format 
influence.sensor.df <- plyr::rbind.fill(influence.sens.df,
                                        influence.vpn.df) %>%
  dplyr::left_join(banc.meta %>%
                     dplyr::distinct(id, cell_type, cell_sub_class, cell_class, super_class, cell_function),
                   by = c("id"))
influence.sensor.df <- calculate_influence_norms(influence.sensor.df)

#############################
### MAIN SEQUENCE HEATMAPS ###
#############################

super.cluster <- "head and eye orienting"
inf.metric <- "influence_norm_log"
umap.super.clust.df <- umap.dn.df %>%
  dplyr::filter(super_cluster == super.cluster)

# Get chosen cluster IDs
cluster.ids <- banc.an.dn.meta %>%
  dplyr::filter(!is.na(cell_type),
                !grepl("^7",cell_type)) %>%
  dplyr::filter(super_cluster == super.cluster) %>%
  dplyr::distinct(root_id) %>%
  dplyr::pull(root_id)
super.clust <- gsub(" ","_",super.cluster)
banc.fig4.path.clust <- file.path(banc.fig4.path,super.clust)
dir.create(banc.fig4.path.clust)

# Stick together

# Stick together
influence_df <- influence.nn.df %>%
  dplyr::mutate(seed = gsub(".*_","",seed),
                target = cell_sub_class) %>%
  dplyr::filter(!is.na(target),
                seed %in% cluster.ids,
                id %in% banc.eff2.meta$id) %>%
  dplyr::filter(!is.na(target)) %>%
  dplyr::left_join(banc.neck.meta %>%
                     dplyr::select(seed = root_id, seed_cell_type = cell_type) %>%
                     dplyr::distinct(seed, seed_cell_type),
                   by=c("seed")) %>%
  dplyr::mutate(seed = seed_cell_type) %>%
  dplyr::mutate(target = case_when(
    target %in% names(efferent.target.map) ~ efferent.target.map[target],
    TRUE ~ target
  )) %>%
  dplyr::filter(target %in% efferent.target.map) %>%
  calculate_influence_norms() %>%
  dplyr::mutate(seed = target,
                target = seed_cell_type,
                seed_type = "effectors") %>%
  plyr::rbind.fill(influence.sensor.df %>%
                     dplyr::filter(!is.na(seed),
                                   id %in% cluster.ids)  %>%
                     dplyr::mutate(target = cell_type,
                                   seed_type = "sensory") %>%
                     dplyr::filter(!is.na(target)) %>%
                     plyr::rbind.fill(influence.sensor.df %>%
                                        dplyr::filter(!is.na(seed),
                                                      id %in% cluster.ids,
                                                      level=="seed_07")  %>%
                                        dplyr::mutate(target = cell_type) %>%
                                        dplyr::left_join(cns.functions %>%
                                                           dplyr::select(seed = cell_type, vpn_function = response) %>%
                                                           dplyr::distinct(seed, .keep_all = TRUE),
                                                         by = "seed") %>%
                                        dplyr::mutate(seed = vpn_function,
                                                      seed_type = "visual") %>%
                                        dplyr::filter(!is.na(target),
                                                      seed!="",
                                                      !is.na(seed),
                                                      !grepl("polarized",seed))) %>%
                     dplyr::mutate(seed = case_when(
                       seed %in% names(sensory.seed.map) ~ sensory.seed.map[seed],
                       TRUE ~ seed),
                       seed_type = "sensors") %>%
                     dplyr::filter(seed %in% sensory.seed.map) %>%
                     calculate_influence_norms() 
  ) %>%
  dplyr::filter(!is.na(target),
                !is.na(seed),
                seed!="0",
                target!="0")

# 1. Compute group-specific thresholds
qtile_df <- influence_df %>%
  dplyr::group_by(seed_type) %>%
  dplyr::reframe(
    thresh = ifelse(
      seed_type == "sensors",
      stats::quantile(influence_norm_log, 0.95, na.rm = TRUE),
      stats::quantile(influence_norm_log, 0.85, na.rm = TRUE)
    )
  ) %>%
  dplyr::distinct()

# 2. Identify seeds that ever surpass the group's threshold
keepers <- influence_df %>%
  dplyr::left_join(qtile_df, by = "seed_type") %>%
  dplyr::group_by(seed_type, seed) %>%
  dplyr::summarize(any_above_thresh = any(influence_norm_log > thresh, na.rm = TRUE), .groups = "drop") %>%
  dplyr::filter(any_above_thresh)

# 3. Filter original data for these seeds, and recalculate norms if needed
influence_df <- influence_df %>%
  dplyr::semi_join(keepers, by = c("seed_type", "seed")) %>%
  dplyr::group_by(seed_type) %>%
  #calculate_influence_norms() %>%
  dplyr::ungroup()

# Cast
influence_matrix <- influence_df  %>%
  dplyr::distinct(seed, target, .keep_all = TRUE) %>%
  reshape2::dcast(seed ~ target, 
                  fun.aggregate = mean, 
                  value.var = "influence_norm_log", 
                  fill = 0)

# Set row names and remove the seed column
rownames(influence_matrix) <- influence_matrix$seed
influence_matrix$seed <- NULL
nams <- dimnames(influence_matrix)

# Convert to matrix
influence_matrix <- as.matrix(influence_matrix)
influence_matrix <- matrix(as.numeric(as.matrix(influence_matrix)), 
                           nrow = nrow(influence_matrix), 
                           ncol = ncol(influence_matrix))
influence_matrix[is.na(influence_matrix)] <- 0
influence_matrix[is.infinite(influence_matrix)] <- 0
dimnames(influence_matrix) <- nams
influence_matrix <- t(influence_matrix)

# Get col annotations
col.annotation <- "seed_type"
annotation_colors <- list()
if(!is.null(col.annotation)){
  col_annotation <- influence_df %>%
    dplyr::filter(!is.na(seed)) %>%
    dplyr::select(eval(col.annotation), seed) %>%
    dplyr::distinct(seed, .keep_all = TRUE) %>%
    as.data.frame()
  col_annotation[[col.annotation]][is.na(col_annotation[[col.annotation]])] <- "other"
  rownames(col_annotation) <- col_annotation$seed
  col_annotation <- col_annotation[rownames(col_annotation) %in% colnames(influence_matrix),]
  col_annotation$seed <- NULL 
  entries <- na.omit(unique(col_annotation[[col.annotation]]))
  cols <- paper.cols[entries]
  annotation_colors[[col.annotation]] <- cols
}else{
  col_annotation <- NULL
  if(is.null(row.annotation)){
    annotation_colors <- NULL
  }
}
if(!is.null(col.annotation)){
  groups <- split(rownames(col_annotation), col_annotation[[col.annotation]])
  groups <- lapply(groups, function(g) if(length(g) >= 2) g else NULL)
  groups <- groups[!sapply(groups, is.null)]
  clustering_result <- hclust_semisupervised(data = t(influence_matrix),
                                             groups = groups,
                                             dist_method = "euclidean",
                                             hclust_method = "ward.D2")
  influence_matrix <- t(clustering_result$data)
  col_annotation <- col_annotation[rownames(col_annotation) %in% colnames(influence_matrix), , drop = FALSE]
  hclust_cols <- clustering_result$hclust
}else{
  if(method=="cosine"){
    cosine_sim_matrix_cols <- lsa::cosine(influence_matrix)
    cosine_sim_matrix_cols[is.na(cosine_sim_matrix_cols)] <- 0
    hclust_cols <- hclust(as.dist(1 - cosine_sim_matrix_cols), method = "ward.D2")
  }else{
    col_dist <- dist(t(influence_matrix), method = method)
    hclust_cols <- hclust(col_dist, method = "ward.D2")
  }
}
col.dend = hclust_cols
row_dist <- dist(influence_matrix, method = "euclidean")
row.dend <- hclust(row_dist, method = "ward.D2")

# Col.order
col.order <- c(
  "neck roll", 
  "neck yaw", 
  "neck pitch", 
  "neck modulatory",
  "eye motor", 
  "antenna motor",
  "wing steering", 
  "wing tension", 
  "wing power", 
  "wing modulatory", 
  "haltere steering", 
  "haltere power", 
  "haltere motor", 
  "proboscis", 
  "front leg", 
  "front leg modulatory", 
  "subesophageal zone modulatory", 
  "eye bristle", 
  "leg bristle", 
  "leg hair plate", 
  "head bristle", 
  "labellum bristle", 
  "prosternal hair plate",
  "wing campaniform", 
  "haltere campaniform", 
  "johnstons organ B", 
  "johnstons organ C", 
  "johnstons organ E", 
  "johnstons organ F", 
  "johnstons organ other", 
  "visual horizontal widefield motion", 
  "visual small object", 
  "visual loom", 
  "visual vertical widefield motion", 
  "visual ocellar"
)
col.order <- col.order[col.order%in%colnames(influence_matrix)]
col.order <- c(col.order,setdiff(colnames(influence_matrix),col.order))

# Split heatmap
influence_matrix <- influence_matrix[,col.order]
influence_matrix_sensors <- influence_matrix[,colnames(influence_matrix)%in%unname(sensory.seed.map)]
influence_matrix_effectors <- influence_matrix[,colnames(influence_matrix)%in%unname(efferent.target.map)]

# Create scaled color palette 5
n_breaks <- 100
color.min <- quantile(influence_matrix_sensors, 0.01, na.rm=TRUE)
color.max <- quantile(influence_matrix_sensors, 0.95, na.rm=TRUE)
scaled_heatmap_sensor_breaks <- seq(color.min, color.max, length.out = n_breaks)
scaled_heatmap_sensor_palette <- colorRampPalette(c("#1f4e79", "#4a90a4", "#7ba7bc", "#a67c8a", "#c4967d", "#b22222"))(n_breaks - 1)
color.min <- quantile(influence_matrix_effectors, 0.01, na.rm=TRUE)
color.max <- quantile(influence_matrix_effectors, 0.95, na.rm=TRUE)
scaled_heatmap_effector_breaks <- seq(color.min, color.max, length.out = n_breaks)
scaled_heatmap_effector_palette <- colorRampPalette(c("#1f4e79", "#4a90a4", "#7ba7bc", "#a67c8a", "#c4967d", "#b22222"))(n_breaks - 1)

# Plot
ph.influence <- pheatmap(
  targeting_method = "ward.D2",
  t(influence_matrix_sensors),
  cluster_rows = FALSE,
  cluster_cols = row.dend,
  color = scaled_heatmap_sensor_palette,
  breaks = scaled_heatmap_sensor_breaks,
  annotation_col = NULL,
  annotation_row = col_annotation,
  annotation_colors = annotation_colors,
  show_rownames = TRUE,
  show_colnames = TRUE,
  treeheight_row = 0, 
  treeheight_col = 0, 
  fontsize_col = 8,
  fontsize_row = 8,
  cellwidth = 12,
  cellheight = 12,
  width = 12, 
  height = 12,
  border_color = NA,
  annotation_legend = TRUE,
  annotation_names_row = FALSE,
  annotation_names_col = FALSE,
  legend = TRUE,
  filename = file.path(banc.fig4.path,
                       sprintf("concise_sensors_to_%s_cell_types_%s.pdf",gsub(" ","",super.clust),"influence_norm_log")),
  main = paste0(inf.metric, "\n(row: source, col: target)"),
  na_col = "lightgrey"
)

# Plot
ph.influence <- pheatmap(
  targeting_method = "ward.D2",
  t(influence_matrix_effectors),
  cluster_rows = FALSE,
  cluster_cols = row.dend,
  color = scaled_heatmap_effector_palette,
  breaks = scaled_heatmap_effector_breaks,
  annotation_col = NULL,
  annotation_row = col_annotation,
  annotation_colors = annotation_colors,
  show_rownames = TRUE,
  show_colnames = TRUE,
  treeheight_row = 0, 
  treeheight_col = 0, 
  fontsize_col = 8,
  fontsize_row = 8,
  cellwidth = 12,
  cellheight = 12,
  width = 12, 
  height = 12,
  border_color = NA,
  annotation_legend = TRUE,
  annotation_names_row = FALSE,
  annotation_names_col = FALSE,
  legend = TRUE,
  filename = file.path(banc.fig4.path,
                       sprintf("concise_%s_cell_types_to_effector_cell_sub_class_%s.pdf",gsub(" ","",super.clust),"influence_norm_log")),
  main = paste0(inf.metric, "\n(row: target, col: source)"),
  na_col = "lightgrey"
)

#####################################################
### INPUT+OUTPUT CONNECTIVITY UMAP DATA BY NEURON ###
#####################################################
super.clusters <- rev(sort(na.omit(unique(banc.an.dn.meta$super_cluster))))
for(super.cluster in super.clusters){
  message("working on:", super.cluster)
  inf.metric = "influence_norm_log"
  
  # Get chosen cluster IDs
  cluster.ids <- banc.an.dn.meta %>%
    dplyr::filter(!is.na(cell_type),
                  !grepl("^7",cell_type)) %>%
    dplyr::filter(super_cluster == super.cluster) %>%
    dplyr::distinct(root_id) %>%
    dplyr::pull(root_id)
  super.clust <- gsub(" ","_",super.cluster)
  banc.fig4.path.clust <- file.path(banc.fig4.path,super.clust)
  dir.create(banc.fig4.path.clust, showWarnings = FALSE)
  
  # Make influence df
  influence.df <- influence.sensor.df %>%
    dplyr::filter(!is.na(seed),
                  id %in% cluster.ids) %>%
    plyr::rbind.fill(influence.nn.df %>%
                       dplyr::mutate(seed = gsub(".*_","",seed)) %>%
                       dplyr::filter(seed %in% cluster.ids,
                                     id %in% banc.eff2.meta$id) %>%
                       dplyr::mutate(seed = id,
                                     id = cell_type) %>%
                       dplyr::filter(!is.na(seed), !is.na(id))
    ) %>%
    dplyr::filter(!is.na(seed),
                  id %in% cluster.ids)
  
  if(recalculate){
    # Make matrix
    inout_influence_matrix <- influence.df %>%
      plyr::rbind.fill(influence.nn.df %>%
                         dplyr::mutate(seed = gsub(".*_","",seed)) %>%
                         dplyr::filter(seed %in% cluster.ids,
                                       id %in% banc.eff2.meta$id) %>%
                         dplyr::mutate(seed = id,
                                       id = cell_type) %>%
                         dplyr::filter(!is.na(seed), !is.na(id))
      ) %>%
      dplyr::filter(id %in%  cluster.ids) %>%
      reshape2::dcast(seed ~ id,
                      fun.aggregate = mean,
                      value.var = inf.metric,
                      fill = 0)
    rownames(inout_influence_matrix) <- inout_influence_matrix$seed
    inout_influence_matrix$seed <- NULL
    
    # Remove all-zero rows from the original matrix
    non_zero_rows <- which(rowSums(abs(inout_influence_matrix)) > 0.0001)
    inout_influence_matrix <- inout_influence_matrix[non_zero_rows, ]
    non_zero_cols <- which(colSums(abs(inout_influence_matrix)) > 0.0001)
    inout_influence_matrix <- inout_influence_matrix[,non_zero_cols]
    
    # Represent as UMAP
    set.seed(42)  
    umap_result <- uwot::umap(t(inout_influence_matrix),
                              metric = "cosine",
                              n_epochs = 500,
                              n_neighbors = min(100,ncol(inout_influence_matrix)), 
                              min_dist = 0,
                              n_trees = 100,
                              spread = 10,
                              n_components = 2)
    rownames(umap_result) <- colnames(inout_influence_matrix)
    
    # Create a data frame with UMAP coordinates
    umap.super.clust.df <- data.frame(
      UMAP1 = umap_result[,1],
      UMAP2 = umap_result[,2],
      id = rownames(umap_result)) %>% 
      dplyr::left_join(banc.meta %>%
                         dplyr::select(id, top_nt, cluster,
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
  }else{
    umap.super.clust.df <- umap.dn.df %>%
      dplyr::filter(super_cluster == super.cluster)
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
  
  # Plot UMAP with highlight
  g.highlight <- ggplot() +
    geom_polygon(data = hulls, 
                 aes(x = V1, y = V2, group = factor(cluster)), 
                 alpha = 0.2, fill = "grey90", color = "black", linetype = "dotted") +
    geom_point(data = umap.dn.df %>%
                 dplyr::filter(!is.na(super_cluster)), 
               aes(x = UMAP1, y = UMAP2),
               color = "lightgrey",
               fill = "white",
               size = 2,
               shape = 19,
               alpha = 0.9) +
    geom_point(data = umap.dn.df %>%
                 dplyr::filter(super_cluster==super.cluster), 
               aes(x = UMAP1, y = UMAP2,color=super_cluster),
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
  print(g.highlight)
  
  # Save
  ggsave(plot = g.highlight,
         filename = file.path(banc.fig4.path.clust,
                              sprintf("%s_neck_cluster_highlight.pdf",gsub(" ","",super.clust))),
         width = 8, height = 8, dpi = 300)
 
  # Plot UMAP with convex hulls
  g.blowout <- ggplot() +
    geom_density_2d(data = umap.super.clust.df, 
                     aes(x = UMAP1, y = UMAP2),
                    col="grey70", 
                    alpha = 0.9) +
    geom_point(
      data = umap.super.clust.df, 
      aes(x = UMAP1, y = UMAP2, color = cell_type, shape = super_class),
      fill = "white",
      size = 3,
      alpha = 0.7
    ) +
    scale_fill_cerise_limon(guide = guide_legend(title = "cell types:")) +
    scale_color_cerise_limon(guide = guide_legend(title = "cell types:")) +
    scale_shape_manual(values = c("ascending"=17,"descending"=19),
                       guide = guide_legend(title = "cell types:")) + 
    ggrepel::geom_label_repel(
      data = umap.super.clust.df %>%
        dplyr::filter(!is.na(cell_function)) %>%
        dplyr::distinct(cell_type, cell_function, .keep_all = TRUE),
      aes(x = UMAP1, y = UMAP2, label = cell_function),
      color = "white",     
      fill = "darkgrey",    
      box.padding = 0.5,
      point.padding = 0.5,
      segment.color = "darkgrey",
      show.legend = FALSE,  
      max.overlaps = 100,
      size = 1.5
    ) +
    theme_void() +
    theme(
      legend.position = "bottom",
      legend.key.size = unit(0.75, "lines"),
      legend.text = element_text(size = rel(0.75)),
      legend.title = element_text(size = rel(0.75)),
      legend.spacing.x = unit(0.75, "lines"),
      legend.spacing.y = unit(0.75, "lines"),
      legend.margin = margin(t = 3, r = 3, b = 3, l = 3, unit = "pt")
    ) +
    guides(
      color = guide_legend(nrow = 10, byrow = TRUE, override.aes = list(size = 4)),
      fill = "none"
    ) +
    ggplot2::coord_fixed()

  # Show
  print(g.blowout)
  
  # Save
  ggsave(plot = g.blowout,
         filename = file.path(banc.fig4.path.clust,
                              sprintf("%s_neck_cluster_zoom_in.pdf",gsub(" ","",super.clust))),
                  width = 6, height = 6, dpi = 300)
  
  #############################
  ### BY CELL TYPE HEATMAPS ###
  #############################

  # Stick together
  influence_df <- influence.nn.df %>%
    dplyr::mutate(seed = gsub(".*_","",seed),
                  target = cell_sub_class) %>%
    dplyr::filter(!is.na(target),
                  seed %in% cluster.ids,
                  id %in% banc.eff2.meta$id) %>%
    dplyr::filter(!is.na(target)) %>%
    dplyr::left_join(banc.neck.meta %>%
                       dplyr::select(seed = root_id, seed_cell_type = cell_type) %>%
                       dplyr::distinct(seed, seed_cell_type),
                     by=c("seed")) %>%
    dplyr::mutate(seed = seed_cell_type) %>%
    dplyr::mutate(target = case_when(
      target %in% names(efferent.target.map) ~ efferent.target.map[target],
      TRUE ~ target
    )) %>%
    dplyr::filter(target %in% efferent.target.map) %>%
    calculate_influence_norms() %>%
    dplyr::mutate(seed = target,
                  target = seed_cell_type,
                  seed_type = "effectors") %>%
    plyr::rbind.fill(influence.sensor.df %>%
                       dplyr::filter(!is.na(seed),
                                     id %in% cluster.ids)  %>%
                       dplyr::mutate(target = cell_type,
                                     seed_type = "sensory") %>%
                       dplyr::filter(!is.na(target)) %>%
                       plyr::rbind.fill(influence.sensor.df %>%
                                          dplyr::filter(!is.na(seed),
                                                        id %in% cluster.ids,
                                                        level=="seed_07")  %>%
                                          dplyr::mutate(target = cell_type) %>%
                                          dplyr::left_join(cns.functions %>%
                                                             dplyr::select(seed = cell_type, vpn_function = response) %>%
                                                             dplyr::distinct(seed, .keep_all = TRUE),
                                                           by = "seed") %>%
                                          dplyr::mutate(seed = vpn_function,
                                                        seed_type = "visual") %>%
                                          dplyr::filter(!is.na(target),
                                                        seed!="",
                                                        !is.na(seed),
                                                        !grepl("polarized",seed))) %>%
                       dplyr::mutate(seed = case_when(
                         seed %in% names(sensory.seed.map) ~ sensory.seed.map[seed],
                         TRUE ~ seed),
                         seed_type = "sensors") %>%
                       dplyr::filter(seed %in% sensory.seed.map) %>%
                       calculate_influence_norms() 
    ) %>%
    dplyr::filter(!is.na(target),
                  !is.na(seed),
                  seed!="0",
                  target!="0")
  
  # 1. Compute group-specific thresholds
  qtile_df <- influence_df %>%
    dplyr::group_by(seed_type) %>%
    dplyr::reframe(
      thresh = ifelse(
        seed_type == "sensors",
        stats::quantile(influence_norm_log, 0.95, na.rm = TRUE),
        stats::quantile(influence_norm_log, 0.85, na.rm = TRUE)
      )
    ) %>%
    dplyr::distinct()
  
  # 2. Identify seeds that ever surpass the group's threshold
  keepers <- influence_df %>%
    dplyr::left_join(qtile_df, by = "seed_type") %>%
    dplyr::group_by(seed_type, seed) %>%
    dplyr::summarize(any_above_thresh = any(influence_norm_log > thresh, na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(any_above_thresh)
  
  # 3. Filter original data for these seeds, and recalculate norms if needed
  influence_df <- influence_df %>%
    dplyr::semi_join(keepers, by = c("seed_type", "seed")) %>%
    dplyr::group_by(seed_type) %>%
    #calculate_influence_norms() %>%
    dplyr::ungroup()
  
  # Cast
  influence_matrix <- influence_df  %>%
    dplyr::distinct(seed, target, .keep_all = TRUE) %>%
    reshape2::dcast(seed ~ target, 
                    fun.aggregate = mean, 
                    value.var = "influence_norm_log", 
                    fill = 0)
  
  # Set row names and remove the seed column
  rownames(influence_matrix) <- influence_matrix$seed
  influence_matrix$seed <- NULL
  nams <- dimnames(influence_matrix)
  
  # Convert to matrix
  influence_matrix <- as.matrix(influence_matrix)
  influence_matrix <- matrix(as.numeric(as.matrix(influence_matrix)), 
                             nrow = nrow(influence_matrix), 
                             ncol = ncol(influence_matrix))
  influence_matrix[is.na(influence_matrix)] <- 0
  influence_matrix[is.infinite(influence_matrix)] <- 0
  dimnames(influence_matrix) <- nams
  influence_matrix <- t(influence_matrix)
  
  # Get col annotations
  col.annotation <- "seed_type"
  annotation_colors <- list()
  if(!is.null(col.annotation)){
    col_annotation <- influence_df %>%
      dplyr::filter(!is.na(seed)) %>%
      dplyr::select(eval(col.annotation), seed) %>%
      dplyr::distinct(seed, .keep_all = TRUE) %>%
      as.data.frame()
    col_annotation[[col.annotation]][is.na(col_annotation[[col.annotation]])] <- "other"
    rownames(col_annotation) <- col_annotation$seed
    col_annotation <- col_annotation[rownames(col_annotation) %in% colnames(influence_matrix),]
    col_annotation$seed <- NULL 
    entries <- na.omit(unique(col_annotation[[col.annotation]]))
    cols <- paper.cols[entries]
    annotation_colors[[col.annotation]] <- cols
  }else{
    col_annotation <- NULL
    if(is.null(row.annotation)){
      annotation_colors <- NULL
    }
  }
  if(!is.null(col.annotation)){
    groups <- split(rownames(col_annotation), col_annotation[[col.annotation]])
    groups <- lapply(groups, function(g) if(length(g) >= 2) g else NULL)
    groups <- groups[!sapply(groups, is.null)]
    clustering_result <- hclust_semisupervised(data = t(influence_matrix),
                                               groups = groups,
                                               dist_method = "euclidean",
                                               hclust_method = "ward.D2")
    influence_matrix <- t(clustering_result$data)
    col_annotation <- col_annotation[rownames(col_annotation) %in% colnames(influence_matrix), , drop = FALSE]
    hclust_cols <- clustering_result$hclust
  }else{
    if(method=="cosine"){
      cosine_sim_matrix_cols <- lsa::cosine(influence_matrix)
      cosine_sim_matrix_cols[is.na(cosine_sim_matrix_cols)] <- 0
      hclust_cols <- hclust(as.dist(1 - cosine_sim_matrix_cols), method = "ward.D2")
    }else{
      col_dist <- dist(t(influence_matrix), method = method)
      hclust_cols <- hclust(col_dist, method = "ward.D2")
    }
  }
  col.dend = hclust_cols
  row_dist <- dist(influence_matrix, method = "euclidean")
  row.dend <- hclust(row_dist, method = "ward.D2")

  # Split heatmap
  influence_matrix_sensors <- influence_matrix[,colnames(influence_matrix)%in%unname(sensory.seed.map)]
  influence_matrix_effectors <- influence_matrix[,colnames(influence_matrix)%in%unname(efferent.target.map)]
  col.dend.sensors <- hclust(dist(t(influence_matrix_sensors), method = "euclidean"), method = "ward.D2")
  col.dend.effectors <- hclust(dist(t(influence_matrix_effectors), method = "euclidean"), method = "ward.D2")
  
  # Create scaled color palette 5
  n_breaks <- 100
  color.min <- quantile(influence_matrix_sensors, 0.01, na.rm=TRUE)
  color.max <- quantile(influence_matrix_sensors, 0.95, na.rm=TRUE)
  scaled_heatmap_sensor_breaks <- seq(color.min, color.max, length.out = n_breaks)
  scaled_heatmap_sensor_palette <- colorRampPalette(c("#1f4e79", "#4a90a4", "#7ba7bc", "#a67c8a", "#c4967d", "#b22222"))(n_breaks - 1)
  color.min <- quantile(influence_matrix_effectors, 0.01, na.rm=TRUE)
  color.max <- quantile(influence_matrix_effectors, 0.95, na.rm=TRUE)
  scaled_heatmap_effector_breaks <- seq(color.min, color.max, length.out = n_breaks)
  scaled_heatmap_effector_palette <- colorRampPalette(c("#1f4e79", "#4a90a4", "#7ba7bc", "#a67c8a", "#c4967d", "#b22222"))(n_breaks - 1)
  
  # Plot
  ph.influence <- pheatmap(
    targeting_method = "ward.D2",
    t(influence_matrix_sensors),
    cluster_rows = col.dend.sensors,
    cluster_cols = row.dend,
    color = scaled_heatmap_sensor_palette,
    breaks = scaled_heatmap_sensor_breaks,
    annotation_col = NULL,
    annotation_row = col_annotation,
    annotation_colors = annotation_colors,
    show_rownames = TRUE,
    show_colnames = TRUE,
    treeheight_row = 0, 
    treeheight_col = 0, 
    fontsize_col = 8,
    fontsize_row = 8,
    cellwidth = 12,
    cellheight = 12,
    border_color = NA,
    annotation_legend = TRUE,
    annotation_names_row = FALSE,
    annotation_names_col = FALSE,
    legend = TRUE,
    filename = file.path(banc.fig4.path.clust,
                         sprintf("concise_sensors_to_%s_cell_types_%s.pdf",gsub(" ","",super.clust),"influence_norm_log")),
    main = paste0(inf.metric, "\n(row: source, col: target)"),
    na_col = "lightgrey"
  )
  
  # Plot
  ph.influence <- pheatmap(
    targeting_method = "ward.D2",
    t(influence_matrix_effectors),
    cluster_rows = col.dend.effectors,
    cluster_cols = row.dend,
    color = scaled_heatmap_effector_palette,
    breaks = scaled_heatmap_effector_breaks,
    annotation_col = NULL,
    annotation_row = col_annotation,
    annotation_colors = annotation_colors,
    show_rownames = TRUE,
    show_colnames = TRUE,
    treeheight_row = 0, 
    treeheight_col = 0, 
    fontsize_col = 8,
    fontsize_row = 8,
    cellwidth = 12,
    cellheight = 12,
    border_color = NA,
    annotation_legend = TRUE,
    annotation_names_row = FALSE,
    annotation_names_col = FALSE,
    legend = TRUE,
    filename = file.path(banc.fig4.path.clust,
                         sprintf("concise_%s_cell_types_to_effector_cell_sub_class_%s.pdf",gsub(" ","",super.clust),"influence_norm_log")),
    main = paste0(inf.metric, "\n(row: target, col: source)"),
    na_col = "lightgrey"
  )
  
  ########################
  ### ANALYSE EDGELIST ###
  ########################
  
  # Stick together
  influence.together <- influence.nn.df %>%
    dplyr::mutate(seed = gsub(".*_","",seed),
                  target = cell_sub_class) %>%
    dplyr::filter(!is.na(target),
                  seed %in% cluster.ids,
                  id %in% banc.eff2.meta$id) %>%
    dplyr::filter(!is.na(target)) %>%
    dplyr::mutate(target = case_when(
      target %in% names(efferent.target.map) ~ efferent.target.map[target],
      TRUE ~ target
    )) %>%
    dplyr::filter(target %in% efferent.target.map) %>%
    calculate_influence_norms() %>%
    dplyr::mutate(id = seed,
                  seed = target,
                  target = id,
                  seed_type = "effectors") %>%
    plyr::rbind.fill(influence.sensor.df %>%
                       dplyr::filter(!is.na(seed),
                                     id %in% cluster.ids)  %>%
                       dplyr::mutate(target = id) %>%
                       dplyr::filter(!is.na(target)) %>%
                       plyr::rbind.fill(influence.sensor.df %>%
                                          dplyr::filter(!is.na(seed),
                                                        id %in% cluster.ids,
                                                        level=="seed_07")  %>%
                                          dplyr::mutate(target = id) %>%
                                          dplyr::left_join(cns.functions %>%
                                                             dplyr::select(seed = cell_type, vpn_function = response) %>%
                                                             dplyr::distinct(seed, .keep_all = TRUE),
                                                           by = "seed") %>%
                                          dplyr::mutate(seed = vpn_function) %>%
                                          dplyr::filter(!is.na(target),
                                                        seed!="",
                                                        !is.na(seed),
                                                        !grepl("polarized",seed))) %>%
                       dplyr::mutate(seed = case_when(
                         seed %in% names(sensory.seed.map) ~ sensory.seed.map[seed],
                         TRUE ~ seed)
                       ) %>%
                       dplyr::mutate(seed_type = "sensors") %>%
                       calculate_influence_norms()
                       ) %>%
    dplyr::filter(!is.na(target),
                  !is.na(seed),
                  seed!="0",
                  target!="0")

  # Make interpretable plots
  umap.super.clust.df$cluster <- super.clust

  # sensory
  banc_interpret_umaps(
    umap.df = umap.super.clust.df,
    elist.pre = NULL,
    elist.post = NULL,
    influence.df = influence.together %>%
      dplyr::filter(seed %in% colnames(influence_matrix_sensors)),
    inf.metric = "influence_norm_log",
    identifier = "influence_norm_log",
    neuroanatomy = FALSE,
    umaps = TRUE,
    banc.meta  = banc.meta,
    save.path = file.path(banc.fig4.path.clust,"sensors"),
    recalculate = FALSE,
    height = 3,
    width = 3,
    scaled_heatmap_palette = scaled_heatmap_sensor_palette,
    scaled_heatmap_breaks = scaled_heatmap_sensor_breaks
  )

  # effector
  banc_interpret_umaps(
    umap.df = umap.super.clust.df,
    elist.pre = NULL,
    elist.post = NULL,
    influence.df = influence.together %>%
      dplyr::filter(seed %in% colnames(influence_matrix_effectors)),
    inf.metric = "influence_norm_log",
    identifier = "influence_norm_log",
    neuroanatomy = FALSE,
    umaps = TRUE,
    banc.meta  = banc.meta,
    save.path = file.path(banc.fig4.path.clust,"effectors"),
    recalculate = FALSE,
    height = 3,
    width = 3,
    scaled_heatmap_palette = scaled_heatmap_sensor_palette,
    scaled_heatmap_breaks = scaled_heatmap_sensor_breaks
  )
  
  # ##################################
  # ### DIRECT CONNECTION HEATMAPS ###
  # ##################################
  connection.types <- c("all","glutamate","gaba","acetylcholine")
  for(connection.type in connection.types){

    # Get edgelist
    if(connection.type=="all"){
      banc.an.dn.elist <- banc.edgelist.simple %>%
        dplyr::group_by(post_cell_type) %>%
        dplyr::mutate(total = sum(count,na.rm = TRUE)) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(pre_cell_type, post_cell_type) %>%
        dplyr::mutate(count = sum(count,na.rm = TRUE),
                      norm = count/total) %>%
        dplyr::ungroup() %>%
        dplyr::filter(!is.na(pre_cell_type), !is.na(post_cell_type)) %>%
        dplyr::filter(pre %in% cluster.ids,
                      post %in% cluster.ids) %>%
        dplyr::distinct(pre_cell_type, post_cell_type, count, norm)
    }else{
      banc.an.dn.elist <- banc.edgelist.simple %>%
        dplyr::group_by(post_cell_type) %>%
        dplyr::mutate(total = sum(count,na.rm = TRUE)) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(pre_cell_type, post_cell_type) %>%
        dplyr::mutate(count = sum(count,na.rm = TRUE),
                      norm = count/total) %>%
        dplyr::ungroup() %>%
        dplyr::filter(!is.na(pre_cell_type),
                      !is.na(post_cell_type),
                      pre_top_nt == connection.type) %>%
        dplyr::filter(pre %in% cluster.ids,
                      post %in% cluster.ids) %>%
        dplyr::distinct(pre_cell_type, post_cell_type, count, norm)
    }

    # 1. Get all unique clusters from both pre_cell_type and post_cell_type
    if(connection.type=="all"){
      all_clusters <- base::union(banc.an.dn.elist$pre_cell_type, banc.an.dn.elist$post_cell_type)
    }

    # 2. Force matrix to be square with all clusters as rows and columns
    heatmap_matrix <- reshape2::acast(
      data = banc.an.dn.elist,
      formula = pre_cell_type ~ post_cell_type,
      value.var = "norm",
      fun.aggregate = function(x) mean(x, na.rm = TRUE)
    )
    heatmap_matrix[is.na(heatmap_matrix)] <- 0
    heatmap_matrix[is.infinite(heatmap_matrix)] <- 0

    # 3. Add any missing rows
    missing_rows <- setdiff(all_clusters, rownames(heatmap_matrix))
    if(length(missing_rows) > 0) {
      add_rows <- matrix(
        0,
        nrow = length(missing_rows),
        ncol = ncol(heatmap_matrix),
        dimnames = list(missing_rows, colnames(heatmap_matrix))
      )
      heatmap_matrix <- rbind(heatmap_matrix, add_rows)
    }

    # 4. Add any missing columns
    missing_cols <- setdiff(all_clusters, colnames(heatmap_matrix))
    if(length(missing_cols) > 0) {
      add_cols <- matrix(
        0,
        nrow = nrow(heatmap_matrix),
        ncol = length(missing_cols),
        dimnames = list(rownames(heatmap_matrix), missing_cols)
      )
      heatmap_matrix <- cbind(heatmap_matrix, add_cols)
    }

    # 5. Order rows/cols identically and fill any remaining NAs
    heatmap_matrix <- heatmap_matrix[all_clusters, all_clusters]
    heatmap_matrix[is.na(heatmap_matrix)] <- 0
    heatmap_matrix[is.infinite(heatmap_matrix)] <- 0

    if(connection.type=="all"){
      # 3. Choose color palette and breaks (as before)
      n_breaks <- 100
      scaled_heatmap_breaks <- seq(
        stats::quantile(heatmap_matrix, 0.05, na.rm = TRUE),
        stats::quantile(heatmap_matrix, 0.95, na.rm = TRUE),
        length.out = n_breaks
      )
      scaled_heatmap_palette <- grDevices::colorRampPalette(c("#1f4e79", "#4a90a4", "#7ba7bc", "#a67c8a", "#c4967d", "#b22222"))(n_breaks - 1)

      # 4. Compute clustering on the *full square* matrix
      row_col_dist <- stats::dist(heatmap_matrix, method = "euclidean")
      symmetric_clust <- stats::hclust(row_col_dist, method = "ward.D2")
    }

    # 5. Plot, using identical clustering for both rows and columns
    pheatmap::pheatmap(
      heatmap_matrix,
      color = scaled_heatmap_palette,
      breaks = scaled_heatmap_breaks,
      clustering_method = "ward.D2",
      cluster_rows = symmetric_clust,
      cluster_cols = symmetric_clust,
      treeheight_row = 0,
      show_rownames = TRUE,
      show_colnames = TRUE,
      fontsize_row = 8,
      fontsize_col = 8,
      cellwidth = 8,
      cellheight = 8,
      filename = file.path(banc.fig4.path.clust, sprintf("%s_%s_neck_cell_types_direct_connectivity.pdf",gsub(" ","",super.clust),connection.type)),
      main = connection.type
    )
  }

  #-------------------------------------------
  # Prepare transmitter color mapping
  nts <- c("acetylcholine","glutamate", "gaba", "dopamine", "serotonin", "octopamine", "histamine", "tyramine", "peptide")
  nt_colors <- paper.cols[nts]
  
  #-------------------------------------------
  # Prepare edgelist for chord diagram
  chord_df <- banc.edgelist.simple %>%
    dplyr::filter(pre %in% cluster.ids, post %in% cluster.ids) %>%
    dplyr::select(-pre_top_nt) %>%
    dplyr::left_join(
      banc.meta %>%
        dplyr::distinct(id, .keep_all = TRUE) %>%
        dplyr::select(pre=id, pre_top_nt = top_nt),
      by = "pre"
    ) %>%
    dplyr::mutate(pre_top_nt = gsub("; ","",pre_top_nt)) %>%
    dplyr::filter(
      pre_cell_type != post_cell_type,
      !is.na(pre_cell_type), !is.na(post_cell_type),
      !is.na(pre_top_nt), pre_top_nt %in% nts) %>%
    dplyr::group_by(pre_cell_type, post_cell_type, pre_top_nt) %>%
    dplyr::summarise(norm = sum(norm, na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(norm > 0.02)
  
  #-------------------------------------------
  # Assign colors, borders, and widths per link, highlight target ribbon
  highlight_idx <- NULL
  if("DNa01" %in% chord_df$post_cell_type){
    highlight_idx <- which(chord_df$pre_cell_type == "AN06B007" & chord_df$post_cell_type == "DNa01")
  }else if ("DNa06" %in% chord_df$pre_cell_type){
    highlight_idx <- which(chord_df$pre_cell_type == "DNa06" & chord_df$post_cell_type %in% c("DNg89","AN19B018","AN03A002"))
  }

  # All ribbons
  chord_df$pre_top_nt <- factor(chord_df$pre_top_nt, levels = names(nt_colors))
  edge_col <- nt_colors[as.character(chord_df$pre_top_nt)]
  
  # Highlight: Black color, thick, black border
  edge_border <- NA
  edge_lwd <- 1
  if(!is.null(highlight_idx)){
    edge_col[highlight_idx] <- "black"
    edge_border <- rep(NA, nrow(chord_df))
    edge_border[highlight_idx] <- "black"
    edge_lwd <- rep(1, nrow(chord_df))
    edge_lwd[highlight_idx] <- 3 
  }
  
  #-------------------------------------------
  # Summarize total incoming and outgoing strength per cell type
  all_cell_types <- union(chord_df$pre_cell_type, chord_df$post_cell_type)
  outgoing <- dplyr::group_by(chord_df, pre_cell_type) %>%
    dplyr::summarise(total_out = sum(norm, na.rm = TRUE), .groups = "drop")
  incoming <- dplyr::group_by(chord_df, post_cell_type) %>%
    dplyr::summarise(total_in = sum(norm, na.rm = TRUE), .groups = "drop")
  
  cell_sums <- dplyr::tibble(cell_type = all_cell_types) %>%
    dplyr::left_join(outgoing, by = c("cell_type" = "pre_cell_type")) %>%
    dplyr::left_join(incoming, by = c("cell_type" = "post_cell_type")) %>%
    dplyr::mutate(
      total_out = dplyr::if_else(is.na(total_out), 0, total_out),
      total_in = dplyr::if_else(is.na(total_in), 0, total_in)
    )
  
  #-------------------------------------------
  # Cell types to label
  label_threshold <- 0.05
  labelled_cells <- dplyr::filter(cell_sums, total_in > label_threshold | total_out > label_threshold) %>%
    dplyr::pull(cell_type)
  
  #-------------------------------------------
  # Set sector (cell type) colors to greyscale
  all_sectors <- unique(c(chord_df$pre_cell_type, chord_df$post_cell_type))
  # Use a white-black (default) greyscale
  sector_grey <- grDevices::grey.colors(length(all_sectors), start = 0.2, end = 0.8) # 0=black, 1=white
  names(sector_grey) <- all_sectors
  
  #-------------------------------------------
  # Plot with highlighted ribbon and custom labels. Save to PNG.
  grDevices::png(file.path(banc.fig4.path.clust, sprintf("%s_neck_cell_types_direct_connectivity_chord_diagram.pdf",gsub(" ","",super.clust))),
                 width = 2000, height = 2000, res = 300, bg = "white")
  circlize::circos.clear()
  circlize::chordDiagram(
    x = as.data.frame(chord_df[, c("pre_cell_type", "post_cell_type", "norm")]),
    col = edge_col,
    link.lwd = edge_lwd,
    link.border = edge_border,
    directional = 1,
    direction.type = "arrows",
    annotationTrack = "grid",
    link.arr.type = "big.arrow",
    grid.col = "lightgrey",
    preAllocateTracks = 1
  )
  circlize::circos.trackPlotRegion(
    track.index = 1,
    panel.fun = function(x, y) {
      name <- circlize::get.cell.meta.data("sector.index")
      #if(name %in% labelled_cells) {
        circlize::circos.text(
          x = mean(circlize::get.cell.meta.data("xlim")),
          y = circlize::get.cell.meta.data("ylim")[2] * 0.01,
          labels = name,
          facing = "clockwise",
          niceFacing = TRUE,
          adj = c(0, 0.5),
          cex = 1
        )
      #}
    },
    bg.border = NA
  )
  circlize::circos.clear()
  grDevices::dev.off()
}
