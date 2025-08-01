#################################
### DIRECT AN-DN CONNECTIVITY ###
#################################

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

# Weird ones:
weird <- c("DNxl080", "DNge079", "DNg73", "DNg65")
banc.an.dn.meta <- banc.an.dn.meta %>%
  dplyr::filter(super_class %in% c("ascending","descending")) %>%
  dplyr::filter(!cell_type %in% weird) %>%
  dplyr::filter(!grepl("^SA|^SN|^AN_4|^AN_5",cell_type))
  
########################
### ANALYSE EDGELIST ###
########################
connection.types <- c("all","glutamate","gaba","acetylcholine")
for(connection.type in connection.types){
  
  # Get edgelist
  if(connection.type=="all"){
    banc.an.dn.elist <- banc.edgelist.simple %>%
      dplyr::group_by(post_cluster) %>%
      dplyr::mutate(total = sum(count,na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(pre_cluster, post_cluster) %>%
      dplyr::mutate(count = sum(count,na.rm = TRUE),
                    norm = count/total) %>%
      dplyr::ungroup() %>%
      dplyr::filter(!is.na(pre_cluster), !is.na(post_cluster)) %>%
      dplyr::filter(pre %in% !!banc.an.dn.meta$root_id,
                    post %in% !!banc.an.dn.meta$root_id) %>%
      dplyr::distinct(pre_cluster, post_cluster, count, norm)  
  }else{
    banc.an.dn.elist <- banc.edgelist.simple %>%
      dplyr::group_by(post_cluster) %>%
      dplyr::mutate(total = sum(count,na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(pre_cluster, post_cluster) %>%
      dplyr::mutate(count = sum(count,na.rm = TRUE),
                    norm = count/total) %>%
      dplyr::ungroup() %>%
      dplyr::filter(!is.na(pre_cluster), 
                    !is.na(post_cluster), 
                    pre_top_nt == connection.type) %>%
      dplyr::filter(pre %in% !!banc.an.dn.meta$root_id,
                    post %in% !!banc.an.dn.meta$root_id) %>%
      dplyr::distinct(pre_cluster, post_cluster, count, norm)  
  }
  
  # 1. Get all unique clusters from both pre_cluster and post_cluster
  if(connection.type=="all"){
    all_clusters <- base::union(banc.an.dn.elist$pre_cluster, banc.an.dn.elist$post_cluster)
  }
  
  # 2. Force matrix to be square with all clusters as rows and columns
  heatmap_matrix <- reshape2::acast(
    data = banc.an.dn.elist,
    formula = pre_cluster ~ post_cluster,
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
    treeheight_col = 0,
    show_rownames = TRUE,
    show_colnames = TRUE,
    fontsize_row = 8,
    fontsize_col = 8,
    cellwidth = 8,
    cellheight = 8,
    filename = file.path(banc.fig3.path, sprintf("%s_neck_cluster_to_neck_cluster_normalised_direct_connectivity.pdf",connection.type)),
    main = connection.type
  )
}

# 1. Prepare your edgelist long-form for all neurotransmitter types
elist_all_nt <- banc.edgelist.simple %>%
  dplyr::filter(pre_top_nt %in% connection.types[connection.types != "all"]) %>%
  dplyr::group_by(post_cluster) %>%
  dplyr::mutate(total = sum(count,na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(pre_cluster), !is.na(post_cluster)) %>%
  dplyr::filter(pre %in% !!banc.an.dn.meta$root_id,
                post %in% !!banc.an.dn.meta$root_id) %>%
  dplyr::group_by(pre_cluster, post_cluster, pre_top_nt) %>%
  dplyr::summarise(
    count = sum(count, na.rm = TRUE),
    norm = count/unique(total),
    .groups = "drop"
  ) %>% dplyr::filter(!is.na(pre_cluster), !is.na(post_cluster)) %>%
  dplyr::mutate(
    pair = paste0(pre_cluster, " \u2192 ", post_cluster),
    from_num = sub("^.*_", "", pre_cluster),
    to_num = sub("^.*_", "", post_cluster)
  ) %>%
  dplyr::filter(pre_cluster != post_cluster, from_num != to_num)

# Sum norm over NT for ranking
elist_sum <- elist_all_nt %>%
  dplyr::group_by(pre_cluster, post_cluster) %>%
  dplyr::summarise(
    total_norm = sum(norm, na.rm = TRUE),
    .groups = "drop"
  )

# Self-join to get A→B and B→A for directionality analysis
elist_pairs <- elist_sum %>%
  dplyr::left_join(
    elist_sum, 
    by = c("pre_cluster" = "post_cluster", "post_cluster" = "pre_cluster"),
    suffix = c("_ab", "_ba")
  ) %>%
  dplyr::mutate(
    total_norm_ba = ifelse(is.na(total_norm_ba), 0, total_norm_ba),
    dir_ratio = ifelse(total_norm_ba > 0, total_norm_ab / total_norm_ba, Inf),
    sum_norm = total_norm_ab + total_norm_ba,
    pair = paste0(pre_cluster, " \u2192 ", post_cluster),
    pair_recip = paste0(pre_cluster, " \u2194 ", post_cluster)
  )

# Top N
topN <- 60

# Directed: A→B at least 2x B→A, strongest total_norm_ab
directed_pairs <- elist_pairs %>%
  dplyr::filter(dir_ratio >= 2) %>%
  dplyr::arrange(desc(total_norm_ab)) %>%
  dplyr::slice_head(n = topN)

# Reciprocal: directionality < 2, strongest sum_norm (keep all, don't force unique pairs)
reciprocal_pairs <- elist_pairs %>%
  dplyr::filter(dir_ratio < 2, dir_ratio > 0.5) %>%
  dplyr::arrange(desc(sum_norm)) %>%
  dplyr::slice_head(n = topN)

# Get all neurotransmitter entries for those pairs (directed & reciprocal)
elist_dir_plot <- elist_all_nt %>%
  dplyr::filter(pair %in% directed_pairs$pair)

elist_rec_plot <- elist_all_nt %>%
  dplyr::mutate(pair_recip = paste0(pre_cluster, " \u2194 ", post_cluster)) %>%
  dplyr::filter(pair_recip %in% reciprocal_pairs$pair_recip)

# Order factors for plotting (biggest at the top in each plot)
elist_dir_plot$pair <- factor(elist_dir_plot$pair, 
                              levels = rev(directed_pairs$pair))
elist_rec_plot$pair_recip <- factor(elist_rec_plot$pair_recip, 
                                    levels = rev(reciprocal_pairs$pair_recip)) 
# 1. Directed (stacked) plot
g.dir <- ggplot2::ggplot(elist_dir_plot, 
                ggplot2::aes(x = pair, y = norm, fill = pre_top_nt)) +
  ggplot2::geom_col(position = "stack") +
  ggplot2::coord_flip() +
  ggplot2::labs(
    x = "cluster pair",
    y = "normed connection (directed, stacked by neurotransmitter)",
    fill = "presynaptic nt",
    title = "top 30 strongest directed connections by neurotransmitter"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.y = ggplot2::element_text(size = 7)) +
  scale_fill_manual(values = paper.cols)

# 2. Reciprocal (stacked) plot
g.recip <- ggplot2::ggplot(elist_rec_plot, 
                ggplot2::aes(x = pair_recip, y = norm, fill = pre_top_nt)) +
  ggplot2::geom_col(position = "stack") +
  ggplot2::coord_flip() +
  ggplot2::labs(
    x = "cluster pair",
    y = "normed connection (reciprocal, stacked by neurotransmitter)",
    fill = "presynaptic nt",
    title = "top 30 strongest reciprocal connections by neurotransmitter"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.y = ggplot2::element_text(size = 7)) +
  scale_fill_manual(values = paper.cols)

# Show and save
print(g.recip)
print(g.dir)
ggsave(plot = g.recip,
       filename = file.path(banc.fig3.path,
                            "neck_cluster_reciprocal_direct_connectivity.pdf"),
       width = 8, height = 8, dpi = 300)
ggsave(plot = g.dir,
       filename = file.path(banc.fig3.path,
                            "neck_cluster_directed_direct_connectivity.pdf"),
       width = 8, height = 8, dpi = 300)

#################################
### INFLUENCE SCORES ON UMAPS ###
#################################

# Connect to .sql file
con <- DBI::dbConnect(RSQLite::SQLite(),
                      file.path(banc.dropbox.influence.save.path,influence.sqlite))
chosen.seeds <- na.omit(unique(banc.an.dn.meta$seed_12))
influence.neck.df <- dplyr::tbl(con, influence.table) %>%
  dplyr::filter(!is_seed,
                level %in% c("seed_12"),
                seed %in% !!chosen.seeds,
                id %in% !!banc.an.dn.meta$root_id) %>%
  dplyr::collect()
dbDisconnect(con)

# Format
influence.neck.cluster.df <- influence.neck.df %>%
  dplyr::mutate(seed = gsub(".*_",seed)) %>%
  dplyr::left_join(banc.meta.pre %>%
                     dplyr::distinct(root_id, pre_cell_type, pre_cell_sub_class, pre_cell_class, pre_super_class, pre_cell_function, pre_cluster),
                   by = c("id"="pre_root_id")) %>%
  dplyr::left_join(banc.meta.post %>%
                     dplyr::distinct(root_id, post_cell_type, post_cell_sub_class, post_cell_class, post_super_class, post_cell_function, post_cluster),
                   by = c("seed"="post_root_id")) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(seed = pre_cluster, target = post_cluster) %>%
  calculate_influence_norms()

# All by cluster
row.dend = symmetric_clust
col.dend = symmetric_clust
row.dend$labels <- paste0("post_", row.dend$labels)
col.dend$labels <- paste0("pre_", col.dend$labels)
nn.cluster.out.nn.cluster.key.plot <- banc_plot_key_features(
  influence.meta = influence.neck.cluster.df %>%
    dplyr::mutate(seed = pre_cluster,
                  target = post_cluster),
  ###
  inf.metric = "influence_log",
  target.map = NULL,
  width = 14,
  height = 14,
  recalculate = TRUE,
  row.annotation = NULL,
  show.annotation = FALSE,
  influence.level = "seed_12",
  save.path = banc.fig3.path,
  seed.map  = FALSE,
  chosen.seeds = NULL,
  chosen.targets = NULL, 
  #row.thresh = 0.1,
  row.cols = NULL,
  super.class = NULL,
  plot.name = sprintf("neck_clusters_to_neck_clusters_%s.pdf","influence_log"),
  rev = FALSE,
  row.dend = row.dend,
  col.dend = col.dend,
)
























