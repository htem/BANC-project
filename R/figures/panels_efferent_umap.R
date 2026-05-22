#' Effector-neuron UMAP and dynamic-tree-cut grouping (ED Fig. 4f)
#'
#' Embeds all effector neurons in 2D using cosine similarity over the
#' adjusted influence they receive from each AN/DN cell type, then
#' delineates the 15 effector groups with `dynamicTreeCut::cutreeDynamic`
#' (deepSplit = 4) on the cosine UMAP. The resulting groups are the
#' columns of Fig. 3f and the labels in Supplementary Data 7.
#'
#' Caches the underlying influence-vs-effector matrix via the per-seed
#' CSV cache and a per-cell SVG cache (`geom_point_svg`) so re-runs only
#' rebuild what has changed; set `recalculate = TRUE` (or env var
#' `.banc_force_recalculate`) to force a full rebuild.
#'
#' Performance note: skip the full `query_influence()` call when
#' `recalculate = FALSE` — the chromote/webshot2 finalizer crashes R
#' during gc() after a 3.2M-row pull. The skip-when-cached pattern is
#' load-bearing on this machine.
#'
#' @section Reads:
#'   banc.meta, banc.eff.meta, banc.neck.meta, banc.edgelist.simple
#'   data/banc_annotations/v888/banc_neck_functional_classes.csv                                  (cluster labels)
#'   data/influence/.../<seed>_influence.csv                                 (per-seed cache)
#'
#' @section Writes:
#'   figures/figure_3/links/supplement/extended_data_fig_4f_*.pdf            (ED Fig. 4f)
#'   figures/figure_3/links/extra/eff_umap_*.pdf                             (sensitivity sweeps)
#'   data/banc_eff_umap_clusters.csv                                          (effector group per neuron)
#'
#' @section Paper:
#'   ED Fig. 4f — cosine UMAP of effector neurons; functional groups
#'                labeled (ingestion-digestion, flight-energy-power, etc.).
#'   Methods §"Clustering influence by influence and connectivity"
#'   (dynamicTreeCut block); §"Naming effector groups".
#'
#' @section Schema:
#'   The 15-effector-group label list is canonical for v888 — see
#'   Methods §"Naming effector groups" for the cell-type composition
#'   of each group; do not rename without updating that subsection +
#'   the paper.cols entries in settings/paper_colours_lacroix.csv.
#'
#' @section Reproduce:
#'   BANC_NCORES=1 Rscript R/figures/panels_efferent_umap.R

####################
## STARTUP        ##
####################

source("R/startup/banc-startup.R")
source("R/startup/banc-meta.R")
source("R/startup/banc-edgelist.R")

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
if (exists(".banc_force_recalculate") && .banc_force_recalculate) recalculate <- TRUE

# Filter problematic neuron types from analysis
weird <- c("DNxl080", "DNge079", "DNg73", "DNg65") # These cell types have some asymmetries
banc.an.dn.meta <- banc.neck.meta %>%
  dplyr::filter(super_class %in% c("ascending","descending")) %>%
  dplyr::filter(!cell_type %in% weird) %>%
  dplyr::filter(!grepl("^SA|^SN|^AN_4|^AN_5",cell_type))
chosen.cts <- banc.an.dn.meta %>%
  distinct(cell_sub_type, side) %>%
  group_by(cell_sub_type) %>%
  summarise(
    sides = list(sort(unique(side)))
  ) %>%
  filter(
    all(c("left", "right") %in% sides)
    |
      (identical(sides[[1]], "center") | identical(sides[[1]], c("center")))
  ) %>%
  pull(cell_sub_type)

####################
## INFLUENCE DATA ##
####################

# Extract neck-to-effector influence data for UMAP generation
influence.neck.eff.db <- query_influence(
    levels = "seed_07", 
    seeds = chosen.cts, ids = banc.eff2.meta$id
  ) %>%
  dplyr::left_join(banc.meta.post %>%
                     dplyr::distinct(post_root_id, .keep_all = TRUE),
                   by = c("id"="post_root_id")) %>%
  dplyr::left_join(banc.meta.pre%>%
                     dplyr::distinct(pre_id, .keep_all = TRUE),
                   by = c("seed"="pre_cell_type"))

# DISABLED 2026-04-09: standalone neck→efferent influence heatmap.
# Superseded by panels_sensory_motor.R + panels_body_parts.R for
# Fig. 2e and ED Fig. 4b/c (same influence input but with per-body-part
# grouping + matched/unmatched stats). Kept here as a reference for
# the simpler unannotated heatmap layout.
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

####################################################
## SENSORY → EFFECTOR CELL_CLASS HEATMAP (2026-04-10)
####################################################
# Heatmap with individual sensory cell types as columns and effector
# cell_classes as rows, grouped by majority-vote EFF super_cluster.
# Analogous to neck_super_clusters_from_sensor_cell_types_*.pdf but with
# effectors as targets instead of AN/DN super_clusters. Placed BEFORE the
# second query_influence call because that call triggers a persistent
# TypeError in this script (see run logs #4-#6).

message("Querying sensory → effector influence (seed_02)...")
influence.sens.eff.db <- query_influence(
    levels = "seed_02",
    ids = banc.eff2.meta$id, normalize = FALSE
  ) %>%
  dplyr::filter(!grepl("unknown", seed))
gc(verbose = FALSE)

# Build majority-vote super_cluster per effector cell_sub_class (fallback
# to cell_class when cell_sub_class is missing). Updated 2026-04-10:
# rows = cell_sub_class (not cell_class), sensory labels cleaned of "_",
# width increased, annotation colored by paper.cols.
.eff_target_col <- banc.eff.meta %>%
  dplyr::mutate(eff_target = dplyr::case_when(
    !is.na(cell_sub_class) & cell_sub_class != "" ~ cell_sub_class,
    TRUE ~ cell_class
  )) %>%
  dplyr::filter(!is.na(eff_target), eff_target != "")

.eff_sc_majority <- .eff_target_col %>%
  dplyr::filter(!is.na(super_cluster), super_cluster != "") %>%
  dplyr::count(eff_target, super_cluster) %>%
  dplyr::group_by(eff_target) %>%
  dplyr::slice_max(n, n = 1, with_ties = FALSE) %>%
  dplyr::ungroup() %>%
  dplyr::select(eff_target, eff_super_cluster = super_cluster)

for (inf.metric in c("influence_log", "influence_norm_log")) {
  # Sensors simplified via sensory.seed.map: individual sensory cell types
  # are collapsed into the sensor-group labels used elsewhere in fig2 (e.g.
  # "leg campaniform", "head bristle"). banc_plot_key_features applies the
  # seed.map internally and then filters to seeds that matched (see the
  # helper's seed.map handling). Raw seed values are still underscore-form,
  # so we don't pre-`gsub("_", " ")` them here.
  .sens_eff_meta <- influence.sens.eff.db %>%
    dplyr::left_join(banc.meta %>%
                       dplyr::mutate(eff_target = dplyr::case_when(
                         !is.na(cell_sub_class) & cell_sub_class != "" ~ cell_sub_class,
                         TRUE ~ cell_class
                       )) %>%
                       dplyr::distinct(root_id, eff_target, super_cluster) %>%
                       dplyr::filter(!is.na(eff_target), eff_target != ""),
                     by = c("id" = "root_id")) %>%
    dplyr::mutate(target = gsub("_", " ", eff_target)) %>%
    dplyr::filter(!is.na(target), !is.na(seed),
                  seed != "0", target != "") %>%
    # Attach the majority-vote EFF super_cluster for row annotation
    dplyr::left_join(.eff_sc_majority %>%
                       dplyr::mutate(eff_target_clean = gsub("_", " ", eff_target)),
                     by = c("target" = "eff_target_clean"))

  banc_plot_key_features(
    influence.meta = .sens_eff_meta,
    inf.metric = inf.metric,
    save.path = banc.fig2.supp.path,
    target.map = NULL,
    seed.map = sensory.seed.map,
    recalculate = TRUE,
    row.annotation = "eff_super_cluster",
    row.order = TRUE,
    col.annotation = NULL,
    show.annotation = TRUE,
    col.thresh = NULL,
    super.class = NULL,
    width = 70,
    height = 14,
    cellheight = 8,
    cellwidth = 12,
    plot.name = sprintf("effector_cell_classes_from_sensor_cell_types_%s.pdf", inf.metric),
    rev = FALSE,
    method = "euclidean"
  )
}
rm(influence.sens.eff.db); gc(verbose = FALSE)
message("Sensory → effector cell_class heatmaps done.")

##########################
## NECK-EFFECTOR DATA   ##
##########################

# Extract neck neuron influence on effector targets.
# This query (3.2M rows, 3148 seeds) historically triggered a TypeError from
# a chromote finalizer during gc(). The chromote namespace is now unloaded in
# banc-startup.R which should prevent the crash. If it recurs, the tryCatch
# sets influence.nn.eff.db = NULL and downstream code skips gracefully.
# (2026-04-11: simplified from gc-bypass which failed due to locked binding.)
chosen.cts <- unique(banc.an.dn.meta$seed_12)
influence.nn.eff.db <- tryCatch({
  query_influence(
    levels = "seed_12", seeds = chosen.cts,
    ids = banc.eff2.meta$id, normalize = FALSE
  )
}, error = function(e) {
  message("WARNING: query_influence for seed_12→effectors failed: ", conditionMessage(e))
  message("  neck_eff_influence_umaps and downstream plots will be skipped.")
  NULL
})

########################
## UMAP GENERATION    ##
########################

# Guard: if the query_influence call above returned NULL (TypeError), skip
# everything that depends on influence.nn.eff.db.
if (is.null(influence.nn.eff.db)) {
  message("influence.nn.eff.db is NULL — skipping UMAP generation + all downstream influence-dependent plots.")
  message("The sensory→effector heatmaps above were still generated successfully.")
} else {

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
                       dplyr::select(id, neurotransmitter, nerve,
                                     region, super_class, side, super_cluster,
                                     hemilineage, cell_function, cell_function_detailed, nerve, body_part_effector,
                                     cell_sub_type, cell_class, cell_sub_class,
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
  umap_eff_df <- umap_eff_df %>%
    dplyr::mutate(body_part_effector2 = dplyr::case_when(
      grepl("power|steering|tension",cell_function) ~ cell_function,
      grepl("pitch|yaw|roll",cell_function_detailed) ~ cell_function_detailed,
      TRUE ~ body_part_effector
    ))

}else{
  # Use pre-computed UMAP data with SeaTable cluster assignments
  # cluster already = manual_cluster (set in banc-meta.R line 503)
  # super_cluster already from SeaTable (coalesce priority in banc-meta.R line 111)
  umap_eff_df <- umap.eff.df %>%
    dplyr::mutate(
      nerve_side = dplyr::case_when(
        grepl("right|_R|R$",nerve) ~ "right",
        grepl("left|_L|L$",nerve) ~ "left",
        TRUE ~ side
      ),
      body_part_effector2 = dplyr::case_when(
        grepl("power|steering|tension",cell_function) ~ cell_function,
        grepl("pitch|yaw|roll",cell_function_detailed) ~ cell_function_detailed,
        TRUE ~ body_part_effector
      )
    )
  
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
       filename = file.path(banc.fig2.extra.path, "eff_umap_influence_norm_log_minmax_euclidean_hulls.pdf"),
       width = 12, height = 12, dpi = 300)
ggsave(plot = convert_to_dark_mode(p_hulls),
       filename = file.path(banc.fig2.darkmode.path, "dark_mode_eff_umap_influence_norm_log_minmax_euclidean_hulls.pdf"),
       width = 12, height = 12, dpi = 300)

# Generate annotated UMAP with super_cluster labels and body part keys
# (auto-generated version of extended data figure 4 panel F)
super_cluster_contents <- umap_eff_df %>%
  dplyr::filter(!is.na(super_cluster), super_cluster != "0", super_cluster != "",
                !is.na(body_part_effector)) %>%
  dplyr::group_by(super_cluster) %>%
  dplyr::summarise(
    body_parts = paste(sort(unique(body_part_effector)), collapse = ", "),
    n = dplyr::n(),
    UMAP1 = mean(UMAP1, na.rm = TRUE),
    UMAP2 = mean(UMAP2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(label = paste0(super_cluster, " (n=", n, ")\n", body_parts))

p_hulls_annotated <- ggplot(data = umap_eff_df, aes(x = UMAP1, y = UMAP2)) +
  # Super cluster boundary hulls (filled)
  geom_polygon(data = super.hulls,
               aes(x = V1, y = V2, group = factor(super_cluster)),
               alpha = 0.08, fill = "grey50", color = "grey30",
               linetype = "solid", linewidth = 0.5) +
  # Fine cluster boundaries (dotted)
  geom_polygon(data = hulls,
               aes(x = V1, y = V2, group = factor(cluster)),
               alpha = 0, fill = NA, color = "grey70", linetype = "dotted",
               linewidth = 0.3) +
  # Unassigned points
  geom_point(data = subset(umap_eff_df, is.na(body_part_effector2)),
             color = "lightgrey", fill = "lightgrey", shape = 21,
             alpha = 0.3, size = 0.8) +
  # Body part points
  geom_point(data = subset(umap_eff_df, !is.na(body_part_effector2)),
             aes(color = body_part_effector2, fill = body_part_effector2,
                 shape = body_part_effector2),
             alpha = 0.9, size = 1.2, stroke = 0.5) +
  # Super cluster labels with body part key
  ggrepel::geom_label_repel(
    data = super_cluster_contents,
    aes(x = UMAP1, y = UMAP2, label = label),
    size = 2.5, fontface = "bold",
    fill = alpha("white", 0.85), label.size = 0.2,
    box.padding = 1.5, point.padding = 0.5,
    max.overlaps = 30, seed = 42,
    min.segment.length = 0.2,
    segment.color = "grey40", segment.size = 0.3
  ) +
  scale_color_manual(values = paper.cols) +
  scale_fill_manual(values = paper.cols) +
  scale_shape_manual(values = body.part.shapes) +
  theme_void() +
  labs(title = "Efferent neuron UMAP — super cluster annotations") +
  guides(
    color = guide_legend(ncol = 3, byrow = TRUE, override.aes = list(size = 3)),
    fill = guide_legend(ncol = 3, byrow = TRUE),
    shape = guide_legend(ncol = 3, byrow = TRUE, override.aes = list(size = 3))
  ) +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    plot.title = element_text(size = 10, hjust = 0.5),
    plot.margin = margin(t = 10, r = 50, b = 0, l = 50, unit = "pt")
  ) +
  ggplot2::coord_fixed()

plot(p_hulls_annotated)
ggsave(plot = p_hulls_annotated,
       filename = file.path(banc.fig2.extra.path, "eff_umap_influence_norm_log_minmax_euclidean_hulls_annotated.pdf"),
       width = 14, height = 14, dpi = 300)

# Interactive HTML tool REMOVED (2026-04-10) — re-homed to
# R/annotations/rebuild_interactive_tools.R which already has the EFF cluster
# reassignment tool via the shared build_interactive_tool() helper.
# The plotly/crosstalk code that lived here caused persistent TypeError
# crashes that blocked the entire script. Removing it lets the paper-bound
# PDF outputs generate cleanly.

# CSV export kept — banc-meta.R:506 reads this file.
eff_interactive_df_SKIP <- TRUE  # placeholder so the CSV export below still runs
if (FALSE) {
eff_interactive_df <- banc.eff.meta %>%
  dplyr::filter(root_id %in% umap_eff_df$id) %>%
  dplyr::left_join(umap_eff_df %>% dplyr::select(id, UMAP1, UMAP2, calculated_cluster),
                   by = c("root_id" = "id")) %>%
  dplyr::filter(!is.na(UMAP1)) %>%
  dplyr::transmute(
    id = as.character(root_id),
    UMAP1, UMAP2,
    calculated_cluster = ifelse(is.na(calculated_cluster) | calculated_cluster == "", "unassigned",
                                as.character(calculated_cluster)),
    manual_cluster = ifelse(is.na(manual_cluster) | manual_cluster == "", "unassigned",
                            as.character(manual_cluster)),
    super_cluster = ifelse(is.na(super_cluster) | super_cluster == "", "unassigned",
                           as.character(super_cluster)),
    cell_type = ifelse(is.na(cell_type), "", cell_type),
    cell_sub_class = ifelse(is.na(cell_sub_class), "", cell_sub_class),
    body_part = ifelse(is.na(body_part_effector), "unknown", body_part_effector),
    super_class = ifelse(is.na(super_class), "", super_class),
    nerve = ifelse(is.na(nerve), "", nerve)
  )

shared_eff <- crosstalk::SharedData$new(eff_interactive_df, key = ~id)

# Build plotly — points colored by super_cluster
p_eff_interactive <- plotly::plot_ly(
  shared_eff,
  x = ~UMAP1, y = ~UMAP2,
  type = "scatter", mode = "markers",
  color = ~super_cluster,
  height = 750,
  text = ~paste0(cell_type, "\n", cell_sub_class,
                 "\nbody: ", body_part,
                 "\ncalculated: ", calculated_cluster,
                 "\nmanual: ", manual_cluster,
                 "\nsuper_cluster: ", super_cluster,
                 "\nnerve: ", nerve,
                 "\nid: ", id),
  hoverinfo = "text",
  marker = list(size = 7, line = list(width = 0.5, color = "white"))
)

# --- Manual cluster hulls (visible by default, solid lines) ---
eff_manual_pts <- eff_interactive_df %>% dplyr::filter(manual_cluster != "unassigned")
eff_manual_ids <- sort(unique(eff_manual_pts$manual_cluster))
eff_manual_cols <- scales::hue_pal()(length(eff_manual_ids))
names(eff_manual_cols) <- eff_manual_ids
for (cl in eff_manual_ids) {
  pts <- eff_manual_pts %>% dplyr::filter(manual_cluster == cl)
  if (nrow(pts) < 3) next
  hull_dat <- as.data.frame(concaveman::concaveman(
    as.matrix(pts[, c("UMAP1", "UMAP2")]), concavity = 2, length_threshold = 0.5))
  hull_dat <- rbind(hull_dat, hull_dat[1, , drop = FALSE])
  p_eff_interactive <- plotly::add_trace(p_eff_interactive, data = hull_dat,
    x = ~V1, y = ~V2, type = "scatter", mode = "lines",
    fill = "toself", fillcolor = paste0(eff_manual_cols[[cl]], "22"),
    line = list(color = eff_manual_cols[[cl]], width = 1.5),
    name = paste0("manual: ", cl), legendgroup = "manual_hulls",
    showlegend = FALSE, hoverinfo = "none", visible = TRUE, inherit = FALSE)
}
p_eff_interactive <- plotly::add_trace(p_eff_interactive,
  x = NA, y = NA, type = "scatter", mode = "lines",
  line = list(color = "grey40", width = 2),
  name = "Manual clusters (solid)", legendgroup = "manual_hulls",
  showlegend = TRUE, visible = TRUE, inherit = FALSE)
eff_manual_centroids <- eff_manual_pts %>%
  dplyr::group_by(manual_cluster) %>%
  dplyr::summarise(UMAP1 = mean(UMAP1), UMAP2 = mean(UMAP2), .groups = "drop")
p_eff_interactive <- plotly::add_text(p_eff_interactive, data = eff_manual_centroids,
  x = ~UMAP1, y = ~UMAP2, text = ~manual_cluster,
  textposition = "middle center",
  textfont = list(color = "grey30", size = 10, family = "Helvetica Bold"),
  legendgroup = "manual_hulls", showlegend = FALSE, hoverinfo = "none",
  visible = TRUE, inherit = FALSE)

# --- Calculated cluster hulls (hidden by default, dashed lines) ---
eff_calc_pts <- eff_interactive_df %>% dplyr::filter(calculated_cluster != "unassigned")
eff_calc_ids <- sort(unique(eff_calc_pts$calculated_cluster))
eff_calc_cols <- scales::hue_pal(h = c(180, 360))(length(eff_calc_ids))
names(eff_calc_cols) <- eff_calc_ids
for (cl in eff_calc_ids) {
  pts <- eff_calc_pts %>% dplyr::filter(calculated_cluster == cl)
  if (nrow(pts) < 3) next
  hull_dat <- as.data.frame(concaveman::concaveman(
    as.matrix(pts[, c("UMAP1", "UMAP2")]), concavity = 2, length_threshold = 0.5))
  hull_dat <- rbind(hull_dat, hull_dat[1, , drop = FALSE])
  p_eff_interactive <- plotly::add_trace(p_eff_interactive, data = hull_dat,
    x = ~V1, y = ~V2, type = "scatter", mode = "lines",
    fill = "toself", fillcolor = paste0(eff_calc_cols[[cl]], "15"),
    line = list(color = eff_calc_cols[[cl]], width = 1.5, dash = "dash"),
    name = paste0("calc: ", cl), legendgroup = "calc_hulls",
    showlegend = FALSE, hoverinfo = "none", visible = "legendonly", inherit = FALSE)
}
p_eff_interactive <- plotly::add_trace(p_eff_interactive,
  x = NA, y = NA, type = "scatter", mode = "lines",
  line = list(color = "grey40", width = 2, dash = "dash"),
  name = "Calculated clusters (dashed)", legendgroup = "calc_hulls",
  showlegend = TRUE, visible = "legendonly", inherit = FALSE)
eff_calc_centroids <- eff_calc_pts %>%
  dplyr::group_by(calculated_cluster) %>%
  dplyr::summarise(UMAP1 = mean(UMAP1), UMAP2 = mean(UMAP2), .groups = "drop")
p_eff_interactive <- plotly::add_text(p_eff_interactive, data = eff_calc_centroids,
  x = ~UMAP1, y = ~UMAP2, text = ~calculated_cluster,
  textposition = "middle center",
  textfont = list(color = "steelblue", size = 9, family = "Helvetica"),
  legendgroup = "calc_hulls", showlegend = FALSE, hoverinfo = "none",
  visible = "legendonly", inherit = FALSE)

ax_range_eff <- range(c(eff_interactive_df$UMAP1, eff_interactive_df$UMAP2), na.rm = TRUE)
ax_pad_eff <- diff(ax_range_eff) * 0.05
ax_lim_eff <- list(ax_range_eff[1] - ax_pad_eff, ax_range_eff[2] + ax_pad_eff)

p_eff_interactive <- plotly::layout(
  p_eff_interactive,
  dragmode = "lasso",
  xaxis = list(title = "", zeroline = FALSE, showticklabels = FALSE,
               showgrid = FALSE, range = ax_lim_eff),
  yaxis = list(title = "", zeroline = FALSE, showticklabels = FALSE,
               showgrid = FALSE, range = ax_lim_eff),
  legend = list(orientation = "v", x = 1.02, y = 1,
                font = list(size = 9)),
  margin = list(r = 150)
) %>% plotly::highlight(on = "plotly_selected", off = "plotly_deselect")

# Linked table
dt_eff <- DT::datatable(
  shared_eff,
  extensions = "Buttons",
  options = list(
    dom = "Bfrtip",
    buttons = list(
      list(extend = "copy", text = "Copy visible"),
      list(extend = "csv", text = "CSV visible",
           filename = "selected_effector_neurons")
    ),
    pageLength = 50, scrollX = TRUE, scrollY = "650px",
    initComplete = DT::JS(
      "function(settings, json) {",
      "  $(this.api().table().container()).css({'font-size': '11px'});",
      "}"
    )
  ),
  filter = "top",
  rownames = FALSE, selection = "none"
)

# Side-by-side: 2/3 UMAP, 1/3 table
widget_eff <- htmltools::browsable(
  htmltools::tagList(
    htmltools::tags$style("
      .tool-container { display: flex; gap: 10px; width: 100%; height: 90vh; }
      .tool-umap { flex: 2; min-width: 0; }
      .tool-table { flex: 1; min-width: 0; overflow-y: auto; }
      .tool-table .dataTables_wrapper { font-size: 11px; }
      h3 { font-size: 14px; margin: 5px 0; }
    "),
    htmltools::h3("Efferent UMAP \u2014 lasso to filter table. Solid hulls: manual_cluster. Toggle dashed: calculated_cluster."),
    htmltools::div(class = "tool-container",
      htmltools::div(class = "tool-umap", p_eff_interactive),
      htmltools::div(class = "tool-table", dt_eff)
    )
  )
)
}  # end if(FALSE) — interactive tool code disabled, lives in rebuild_interactive_tools.R

# Export tool CSV with both cluster columns for standalone regeneration.
# This CSV is read by banc-meta.R:506, so it must be written even though
# the interactive HTML tool above is disabled.
write_csv(x = umap_eff_df %>% dplyr::select(-any_of("image")),
          file = "data/banc_annotations/v888/banc_efferent_functional_classes.csv")
message("Exported data/banc_annotations/v888/banc_efferent_functional_classes.csv with manual_cluster + calculated_cluster")

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
icon_folder <- "figures/schematics/assets/umap_icons_efferent"
# Build SVG content lookup: read each SVG file once, cache content by body_part
svg_paths <- list.files(icon_folder, pattern = "\\.svg$", full.names = TRUE)
svg_cache <- setNames(
  sapply(svg_paths, function(f) paste(readLines(f, warn = FALSE), collapse = "\n")),
  tools::file_path_sans_ext(basename(svg_paths))
)
umap_eff_df$image <- svg_cache[gsub(" ", "_", umap_eff_df$body_part_effector)]

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
    data = subset(umap_eff_df, !is.na(body_part_effector) & !is.na(image)),
    aes(x = UMAP1, y = UMAP2, svg = image),
    size = 2.5
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
       filename = file.path(banc.fig2.extra.path,"eff_umap_influence_norm_log_minmax_euclidean_icons.pdf"),
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
      body_part_pct = round(dplyr::n()/dplyr::first(cluster_n) * 100)
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
      body_part_pct = round(dplyr::n()/dplyr::first(cluster_n) * 100)
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
      body_part_pct = round(dplyr::n()/dplyr::first(cluster_n) * 100)
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
      body_part_pct = round(dplyr::n()/dplyr::first(cluster_n) * 100)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(cluster, dplyr::desc(body_part_pct)) %>%
    dplyr::distinct(cluster, cell_function_detailed, n, body_part_pct) %>%
    as.data.frame()
  print(cluster_homogeneity4)
}

# Export cluster assignments to file
if(recalculate){
  write_csv(x=umap_eff_df %>% dplyr::select(-any_of("image")),
            file = "data/banc_annotations/v888/banc_efferent_functional_classes.csv")
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
  
  # Generate influence-overlaid UMAP visualisation (no hulls/densities — just
  # points so the influence colour gradient stands alone)
  p_hulls.bp <-  ggplot(data = umap_eff_df.dc,
                        aes(x = UMAP1, y = UMAP2)) +
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

# Interactive plotly viz REMOVED (2026-04-10) — re-homed to
# R/annotations/rebuild_interactive_tools.R. The plotly code here caused
# persistent TypeError crashes that blocked the script. The equivalent
# tool is built by rebuild_interactive_tools.R using build_interactive_tool().

}  # end if (!is.null(influence.nn.eff.db)) guard

