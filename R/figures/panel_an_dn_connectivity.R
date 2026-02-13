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

# 1. Prepare edgelist long-form for all neurotransmitter types
elist_all_nt <- banc.edgelist.simple %>%
  #dplyr::filter(pre_top_nt %in% connection.types[connection.types != "all"]) %>%
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

# Get super clusters direct connectivity matrix
con.sc.df <- banc.edgelist.simple %>%
  dplyr::group_by(post_super_cluster) %>%
  dplyr::mutate(total = sum(count)) %>%
  dplyr::group_by(pre_super_cluster, post_super_cluster) %>%
  dplyr::mutate(count = sum(count,na.rm = TRUE),
                norm = count/total,
                level = "direct") %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(pre_super_cluster), !is.na(post_super_cluster)) %>%
  dplyr::filter(pre %in% !!banc.an.dn.meta$root_id,
                post %in% !!banc.an.dn.meta$root_id) %>%
  dplyr::select(seed = pre_super_cluster, target = post_super_cluster, level, count, norm)

# Analyse inter-neck super cluster influence patterns
con.sc.key.plot.norm <- banc_plot_key_features(
  influence.meta = con.sc.df,
  ###
  inf.metric = "norm",
  target.map = NULL,
  width = 6,
  height = 6,
  recalculate = FALSE,
  row.annotation = NULL,
  show.annotation = FALSE,
  save.path = banc.fig5.supp.path,
  seed.map  = FALSE,
  chosen.seeds = NULL,
  chosen.targets = NULL, 
  row.cols = NULL,
  super.class = NULL,
  row.order = super.clust.order,
  col.order = super.clust.order,
  plot.name = sprintf("neck_super_clusters_to_neck_super_clusters_direct_%s.pdf","norm"),
  rev = FALSE,
  row.dend = NULL,
  col.dend = NULL,
  method = "euclidean",
  symmetric = TRUE,
  diagonal = FALSE
)

con.sc.key.plot.count <- banc_plot_key_features(
  influence.meta = con.sc.df,
  ###
  inf.metric = "count",
  target.map = NULL,
  width = 6,
  height = 6,
  recalculate = FALSE,
  row.annotation = NULL,
  show.annotation = FALSE,
  save.path = banc.fig5.supp.path,
  seed.map  = FALSE,
  chosen.seeds = NULL,
  chosen.targets = NULL, 
  row.cols = NULL,
  super.class = NULL,
  row.order = super.clust.order,
  col.order = super.clust.order,
  plot.name = sprintf("neck_super_clusters_to_neck_super_clusters_direct_%s.pdf","count"),
  rev = FALSE,
  row.dend = NULL,
  col.dend = NULL,
  method = "euclidean",
  symmetric = TRUE,
  diagonal = FALSE
)

##############
### CHAINS ###
###############

# Chain analysis
ad_keep <- c("ascending","descending")
edge_count_rule <- function(x) x >= 10  
E_ct <- banc.edgelist.simple |>
  dplyr::group_by(pre_cell_type, post_cell_type) |>
  dplyr::summarise(
    count              = sum(count, na.rm = TRUE),
    pre_super_class    = dplyr::first(pre_super_class),
    post_super_class   = dplyr::first(post_super_class),
    pre_super_cluster  = dplyr::first(pre_super_cluster),
    post_super_cluster = dplyr::first(post_super_cluster),
    .groups = "drop"
  ) |>
  dplyr::filter(edge_count_rule(count)) |>
  dplyr::filter(!is.na(pre_super_cluster), !is.na(post_super_cluster)) |>
  dplyr::filter(pre_super_class  %in% ad_keep,
                post_super_class %in% ad_keep) |>
  dplyr::filter(pre_cell_type != post_cell_type)  

V_all <- dplyr::bind_rows(
  dplyr::transmute(E_ct,
                   id            = pre_cell_type,
                   super_class   = pre_super_class,
                   super_cluster = pre_super_cluster),
  dplyr::transmute(E_ct,
                   id            = post_cell_type,
                   super_class   = post_super_class,
                   super_cluster = post_super_cluster)
) |>
  dplyr::distinct(id, .keep_all = TRUE) |>
  dplyr::mutate(
    super_cluster = dplyr::if_else(is.na(super_cluster) | super_cluster == "",
                                   "unknown", super_cluster)
  )

g <- igraph::graph_from_data_frame(
  d = dplyr::rename(E_ct, pre = pre_cell_type, post = post_cell_type),
  directed = TRUE,
  vertices = dplyr::rename(V_all, name = id)
)

node_attr <- igraph::as_data_frame(g, what = "vertices") |>
  dplyr::select(id = name, super_cluster)

src_ids <- node_attr$id
paths_list <- vector("list", 0)
pid <- 0L

pb <- utils::txtProgressBar(min = 0, max = length(src_ids), style = 3)
for (i in seq_along(src_ids)) {
  s <- src_ids[i]
  # distances from s to all (directed, outwards)
  dvec <- igraph::distances(g, v = igraph::V(g)[name == s], mode = "out")
  # dvec is 1 x |V| matrix; coerce to named numeric
  dv <- as.numeric(dvec[1, ])
  names(dv) <- igraph::V(g)$name
  
  # finite (reachable) targets excluding self
  reachable <- names(dv)[is.finite(dv) & dv > 0]
  
  if (length(reachable) > 0) {
    # get one shortest path for each reachable target
    sp <- igraph::shortest_paths(g,
                                 from = igraph::V(g)[name == s],
                                 to   = igraph::V(g)[names(igraph::V(g)) %in% reachable],
                                 mode = "out",
                                 output = "vpath")
    
    for (k in seq_along(sp$vpath)) {
      vseq <- sp$vpath[[k]]
      if (length(vseq) < 2) next
      nodes <- igraph::V(g)$name[as.integer(vseq)]
      # ensure simple (no repeats)
      if (any(duplicated(nodes))) next
      pid <- pid + 1L
      paths_list[[pid]] <- nodes
    }
  }
  utils::setTxtProgressBar(pb, i)
}
close(pb)

# Deduplicate identical sequences (some targets may tie; igraph chooses one, but be safe)
keys <- vapply(paths_list, function(x) paste(x, collapse = "->"), character(1))
paths_list <- paths_list[!duplicated(keys)]

paths_df <- tibble::tibble(
  path_id = seq_along(paths_list),
  nodes   = paths_list
) |>
  tidyr::unnest_longer(nodes, values_to = "id", indices_to = "pos") |>
  dplyr::left_join(node_attr, by = "id")

path_summary <- paths_df |>
  dplyr::group_by(path_id) |>
  dplyr::summarise(
    path_len  = dplyr::n() - 1L,  # edges
    n_clusters = dplyr::n_distinct(super_cluster),
    combo      = paste(sort(unique(super_cluster)), collapse = " + "),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    cluster_mix = dplyr::if_else(n_clusters == 1L, "unitary super cluster", "mixed super cluster")
  )

path_summary_3plus <- path_summary |>
  dplyr::filter(n_clusters >= 3)

# --- replace input to the summarisation with the filtered table ---
combo_by_len <- path_summary_3plus |>
  dplyr::mutate(
    combo_clean = stringr::str_replace_all(combo, "_", " "),
    path_len_f  = factor(path_len, levels = rev(sort(unique(path_len))))
  ) |>
  dplyr::count(cluster_mix, combo_clean, path_len_f) |>
  dplyr::rename(n_paths = n) |>
  dplyr::group_by(cluster_mix, combo_clean) |>
  dplyr::mutate(total = sum(n_paths)) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    combo_in_facet = tidytext::reorder_within(combo_clean, total, cluster_mix)
  )
combo_mixed <- combo_by_len |>
  dplyr::filter(cluster_mix == "mixed super cluster") |>
  dplyr::group_by(combo_clean) |>
  dplyr::mutate(total_combo = sum(n_paths)) |>
  dplyr::ungroup()

top25_levels <- combo_mixed |>
  dplyr::distinct(combo_clean, total_combo) |>
  dplyr::arrange(dplyr::desc(total_combo)) |>
  dplyr::slice_head(n = 25) |>
  dplyr::pull(combo_clean)

combo_top25 <- combo_mixed |>
  dplyr::filter(combo_clean %in% top25_levels) |>
  dplyr::group_by(cluster_mix, combo_clean) |>
  dplyr::mutate(total = sum(n_paths)) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    combo_in_facet = tidytext::reorder_within(combo_clean, total, cluster_mix)
  )

len_levels <- sort(unique((combo_top25$path_len_f)))
grey_pal   <- grDevices::colorRampPalette(c("#000000", "grey95"))
len_cols   <- stats::setNames(grey_pal(length(len_levels)), len_levels)

p_top25_mixed <- ggplot2::ggplot(
  combo_top25,
  ggplot2::aes(x = combo_in_facet, y = n_paths, fill = path_len_f)) +
  ggplot2::geom_col() +
  ggplot2::coord_flip() +
  ggplot2::facet_wrap(~ cluster_mix, nrow = 1, scales = "free_y") +
  ggplot2::scale_fill_manual(values = len_cols, name = "shortest path length") +
  tidytext::scale_x_reordered() +
  ggplot2::labs(
    x = "super_cluster combination",
    y = "number of source→target pairs (shortest paths)",
    title = "mixed super-cluster shortest paths (top 25 combos; ≥3 clusters traversed)"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.position = "bottom",
    strip.text = ggplot2::element_text(face = "bold")
  )

# Save
print(p_top25_mixed)
ggsave(plot = p_top25_mixed,
       filename = file.path(banc.fig5.supp.path,
                            "an_dn_super_cluster_short_paths_top_25.pdf"),
       width = 8, height = 8, dpi = 300)

#############################
### UPSTREAM OF EFFECTORS ###
#############################

# Wrangle
library(dplyr)
library(ggplot2)
library(scales)
library(glue)

# ---- Filter once ----
edl_filt <- banc.edgelist.simple %>%
  dplyr::filter(
    !is.na(pre_super_class),
    !is.na(post_region),
    post_region %in% c("central_brain","ventral_nerve_cord"),
    !pre_super_class %in% c("glia","trachea","not_a_neuron"),
    post_super_class %in% c("motor","visceral_circulatory","ascending_visceral_circulatory")
  )

# Wrangle
eff.up <- edl_filt %>%
  dplyr::group_by(pre_super_class, post_super_class, post_region) %>%
  dplyr::mutate(count = sum(count, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(pre_super_class, post_super_class, count, post_region)

order_pres <- eff.up %>%
  dplyr::group_by(pre_super_class) %>%
  dplyr::summarise(total = sum(count, na.rm = TRUE), .groups = "drop") %>%
  dplyr::arrange(desc(total)) %>%
  dplyr::pull(pre_super_class)

eff.plot <- eff.up %>%
  dplyr::mutate(
    pre_super_class  = factor(pre_super_class, levels = order_pres),
    post_super_class = factor(post_super_class)
  )

# ---- Build labels: integer neurons, newline between lines ----
post_id_col <- intersect(
  c("post_root","post_id","post","post_neuron_id","post_bodyid","post_body_id","post_root_id"),
  names(edl_filt)
)[1]

labels_df <- eff.plot %>%
  dplyr::group_by(post_super_class, post_region) %>%
  dplyr::summarise(total_inputs = sum(count, na.rm = TRUE), .groups = "drop") %>%
  dplyr::left_join(
    {
      if (!is.na(post_id_col)) {
        edl_filt %>%
          dplyr::group_by(post_super_class, post_region) %>%
          dplyr::summarise(n_neurons = dplyr::n_distinct(.data[[post_id_col]]), .groups = "drop")
      } else {
        edl_filt %>%
          dplyr::distinct(post_super_class, post_region) %>%
          dplyr::mutate(n_neurons = NA_integer_)
      }
    },
    by = c("post_super_class","post_region")
  ) %>%
  dplyr::mutate(
    # neurons as integer with commas; inputs SI-cut with no unit suffix
    label_txt  = sprintf(
      "neurons=%s\ninputs=%s",
      scales::comma(n_neurons),
      scales::label_number(accuracy = 1, scale_cut = scales::cut_si(""))(total_inputs)
    ),
    y_lab = 1
  )

# ---- Plot ----
p <- ggplot(eff.plot, aes(x = post_super_class, y = count, fill = pre_super_class)) +
  geom_col(position = "fill", width = 0.8, color = "white", linewidth = 0.2) +
  coord_flip() +
  scale_y_continuous( expand = expansion(mult = c(0, 0.03))) +
  labs(
    x = "post super class",
    y = "proportion of inputs",
    fill = "pre super class"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position    = "none",
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3),
    axis.title.y       = element_text(vjust = 1.2),
    plot.margin        = margin(t = 5.5, r = 30, b = 5.5, l = 5.5)
  ) +
  ggplot2::scale_x_discrete(labels = ~ gsub("_", " ", .x)) +
  scale_fill_manual(values = paper.cols) +
  facet_wrap(~ post_region) +
  geom_text(
    data = labels_df,
    aes(x = post_super_class, y = y_lab, label = label_txt),
    inherit.aes = FALSE,
    color = "white", fontface = "bold", size = 4.8,
    hjust = 1.02, vjust = 0.5, lineheight = 0.98
  )

print(p)

# Save
ggsave(
  plot = p,
  filename = file.path(banc.fig2.supp.path, "upstream_of_efferent_neurons.pdf"),
  width = 7, height = 3, dpi = 300
)

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
























