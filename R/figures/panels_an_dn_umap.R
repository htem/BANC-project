#' AN/DN PCA-UMAP and hierarchical clustering (Fig. 3d, ED Fig. 6)
#'
#' Builds the PCA-UMAP embedding of all ANs and DNs from cosine similarity
#' between their typed input + output partners (cell_type + neuromere +
#' nerve aggregation), and the hierarchical Ward.D2 clustering on the
#' Marchenko-Pastur-truncated PCA space that produces the 17 functional
#' clusters used throughout Fig. 3 and Supplementary Data 6/9.
#'
#' Implements the procedure described in Methods §"Clustering influence by
#' influence and connectivity": typed partner aggregation, no minimum-
#' synapse threshold, input-fraction normalization, PCA with Marchenko-
#' Pastur cut-off (319 of 1,228 components retained), Ward.D2 clustering
#' followed by `dynamicTreeCut::cutreeDynamic` with deepSplit = 0 and
#' minClusterSize = 8. Cell-sub-type level clustering (completeness =
#' 1.000 by construction). UMAP parameters: euclidean metric, n_neighbors
#' = 100, min_dist = 0, spread = 10, n_epochs = 500, n_trees = 100,
#' seed = 42.
#'
#' Recalculate branch evaluates four cluster-method variants (id vs
#' celltype partners × brokenstick vs Marchenko-Pastur PCA dim selection)
#' and writes a confusion matrix per method; the published method is the
#' "celltype + MP" variant.
#'
#' @section Reads:
#'   banc.meta, banc.edgelist.simple, banc.eff.meta, paper.cols
#'   data/meta/banc_neck_inclusion.csv                                     (triage list)
#'   data/banc_annotations/v888/banc_neck_functional_classes.csv                                  (cached UMAP coords)
#'
#' @section Writes:
#'   figures/figure_3/links/an_dn_pca_umap*.pdf                            (Fig. 3d)
#'   figures/figure_3/links/supplement/an_dn_pca_umap_*.pdf                (ED Fig. 6 a/b/c)
#'   figures/figure_3/links/extra/cluster_options/                          (4-variant comparison)
#'   data/banc_annotations/v888/banc_neck_functional_classes.csv                                  (UMAP coords + cluster id)
#'   data/cluster_assignments.csv                                           (chosen method per neuron)
#'   data/cluster_assignments_all_methods.csv                               (all 4 method labels)
#'
#' @section Paper:
#'   Fig. 3d — PCA-UMAP of ANs and DNs coloured by cluster membership.
#'   ED Fig. 6a — same UMAP highlighting near-synapse-less ITP ANs.
#'   ED Fig. 6b — behavioural-function overlay on the UMAP.
#'   ED Fig. 6c — predicted-function overlay from prior FAFB DN-DN study.
#'   Methods §"Clustering influence by influence and connectivity".
#'   Methods §"Naming AN/DN clusters" (cluster name → function mapping).
#'   Supplementary Data 6 (cluster assignments per neuron).
#'
#' @section Schema:
#'   The `ct_mp_to_super` mapping (integer cluster → super_cluster label)
#'   is the canonical name table for v888; lives in
#'   R/annotations/banc-cluster-update.R and /tmp/banc_runs/update_paper_cell_types.R.
#'   Retired names: `takeoff-landing` → `postural control`;
#'   `interoceptive` → merged into `taste-touch`. New: `walking steering`.
#'
#' @section Used by:
#'   R/figures/panels_an_dn_connectivity.R, panels_an_dn_influence.R,
#'   panels_efferent_umap.R, panels_cluster_sensory_correlations.R,
#'   panels_cell_type_blowouts.R — all read the cluster column written here.
#'
#' @section Reproduce:
#'   BANC_NCORES=1 Rscript R/figures/panels_an_dn_umap.R

###############
### STARTUP ###
###############

# load
source("R/startup/banc-startup.R")
source("R/startup/franken-meta.R")
if (exists("bc.orig")) rm(bc.orig)  # force fresh SeaTable pull
if (exists("banc.meta")) rm(banc.meta)
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
  dplyr::filter(super_class %in% c("descending","ascending","visual_centrifugal")|
                  grepl("mushroom_body_input|central_complex_input",cell_class)|
                  root_id%in%!!banc.eff.meta$root_id)

# Triaged neck neurons
neck.inclusion <- readr::read_csv(file="data/meta/banc_neck_inclusion.csv", 
                                  col_types = banc.col.types)
banc.in <- subset(neck.inclusion,in_group)$root_id
banc.out <- subset(neck.inclusion,!in_group)$root_id

# Recalculate?
# Set TRUE to recompute UMAP from scratch (slow). FALSE reads cached coords from
# data/banc_annotations/v888/banc_neck_functional_classes.csv via banc-meta.R.
recalculate <- FALSE
if (exists(".banc_force_recalculate") && .banc_force_recalculate) recalculate <- TRUE

# Primary partner grouping for clustering/UMAP pipeline ("id" or "cell_sub_type").
# Both matrices are always built; this flag chooses which one drives the main pipeline.
# "cell_sub_type" groups partner neurons by banc.meta$cell_sub_type (noise-reduced)
# "id" uses individual partner neuron IDs (original).
# NOTE: legacy value "cell_type" is still accepted as a synonym for cell_sub_type.
partner_grouping <- "cell_sub_type"

# PCA dimension selection method ("brokenstick" or "marchenko_pastur").
# Both are always run; this flag chooses which one drives the primary clustering.
dim_method <- "marchenko_pastur"

# AN/DN aggregation level for cell-type clustering. "cell_sub_type" is
# cell_type disambiguated by neuromere where applicable. Sets the row labels
# of cct_matrix and the keys of ct_lookup.
andn_grouping <- "cell_sub_type"

#####################################################
### INPUT+OUTPUT CONNECTIVITY UMAP DATA BY NEURON ###
#####################################################

if(recalculate){
  
  # Make matrix
  an.dn.ids <- unique(banc.an.dn.meta$root_id)
  
  # Get unfiltered edgelist for UMAP (use count > 0, not the count >= 3 default)
  # This matches resubmission_2 behaviour — weak connections contribute to connectivity structure
  banc.edgelist.unfiltered <- arrow::read_feather(.banc_edgelist_cache) %>%
    dplyr::mutate(pre = as.character(pre), post = as.character(post)) %>%
    dplyr::filter(count > 0)
  message(sprintf("UMAP using unfiltered edgelist: %d connections (vs %d at count>=3)",
                  nrow(banc.edgelist.unfiltered), nrow(banc.edgelist.simple)))
  neck.elist.pre <- banc.edgelist.unfiltered %>%
    dplyr::filter(pre %in% an.dn.ids)
  neck.elist.post <- banc.edgelist.unfiltered %>%
    dplyr::filter(post %in% an.dn.ids)
  # Build neuron-level in/out connectivity matrix
  # Compute norm from post_count if not present in unfiltered edgelist
  if (!"norm" %in% names(neck.elist.pre)) {
    neck.elist.pre <- neck.elist.pre %>%
      dplyr::group_by(post) %>%
      dplyr::mutate(norm = count / sum(count, na.rm = TRUE)) %>%
      dplyr::ungroup()
    neck.elist.post <- neck.elist.post %>%
      dplyr::group_by(post) %>%
      dplyr::mutate(norm = count / sum(count, na.rm = TRUE)) %>%
      dplyr::ungroup()
  }
  # --- Partner cell_type composite key (2026-05-01) ---
  # Each typed partner is keyed by cell_type + hemilineage + nerve + neuromere
  # so that serial homologs across VNC neuropils (same cell_type, different
  # neuromere/hemilineage) become distinct partner columns. Partners without
  # cell_type are DROPPED — singleton root_id columns previously fragmented the
  # partner space and let noise dominate the AN/DN clustering.
  partner_ct_map <- banc.meta %>%
    dplyr::select(root_id, cell_type, hemilineage, nerve, neuromere) %>%
    dplyr::filter(!is.na(cell_type) & cell_type != "") %>%
    dplyr::mutate(
      .hl = dplyr::coalesce(hemilineage, ""),
      .nv = dplyr::coalesce(nerve, ""),
      .nm = dplyr::coalesce(neuromere, ""),
      partner_ct = paste(cell_type, .hl, .nv, .nm, sep = "_")
    ) %>%
    dplyr::select(root_id, partner_ct) %>%
    dplyr::distinct(root_id, .keep_all = TRUE)

  build_inout_matrix <- function(grouping = c("id", "cell_sub_type", "cell_type")) {
    grouping <- match.arg(grouping)
    if (grouping == "id") {
      pre_agg <- neck.elist.pre %>%
        dplyr::group_by(id = pre, post) %>%
        dplyr::summarise(count = sum(count, na.rm = TRUE),
                         norm = mean(norm, na.rm = TRUE), .groups = "drop") %>%
        dplyr::mutate(partner_id = paste0("post_", post)) %>%
        dplyr::select(id, partner_id, count, norm)
      post_agg <- neck.elist.post %>%
        dplyr::group_by(id = post, pre) %>%
        dplyr::summarise(count = sum(count, na.rm = TRUE),
                         norm = mean(norm, na.rm = TRUE), .groups = "drop") %>%
        dplyr::mutate(partner_id = paste0("pre_", pre)) %>%
        dplyr::select(id, partner_id, count, norm)
    } else {
      # Join partner cell_type composite key, then aggregate by (AN/DN id, partner key).
      # Partners without cell_type are dropped (filter !is.na(partner_ct)) so they
      # don't contribute singleton columns.
      pre_agg <- neck.elist.pre %>%
        dplyr::left_join(partner_ct_map, by = c("post" = "root_id")) %>%
        dplyr::filter(!is.na(partner_ct)) %>%
        dplyr::group_by(id = pre, partner_ct) %>%
        dplyr::summarise(count = sum(count, na.rm = TRUE),
                         norm = sum(norm, na.rm = TRUE), .groups = "drop") %>%
        dplyr::mutate(partner_id = paste0("post_", partner_ct)) %>%
        dplyr::select(id, partner_id, count, norm)
      post_agg <- neck.elist.post %>%
        dplyr::left_join(partner_ct_map, by = c("pre" = "root_id")) %>%
        dplyr::filter(!is.na(partner_ct)) %>%
        dplyr::group_by(id = post, partner_ct) %>%
        dplyr::summarise(count = sum(count, na.rm = TRUE),
                         norm = sum(norm, na.rm = TRUE), .groups = "drop") %>%
        dplyr::mutate(partner_id = paste0("pre_", partner_ct)) %>%
        dplyr::select(id, partner_id, count, norm)
    }
    neck.cat <- rbind(pre_agg, post_agg) %>%
      dplyr::distinct(id, partner_id, count, norm)
    mat <- neck.cat %>%
      dplyr::filter(id %in% an.dn.ids) %>%
      reshape2::dcast(partner_id ~ id, fun.aggregate = mean, value.var = "norm", fill = 0)
    rownames(mat) <- mat$partner_id
    mat$partner_id <- NULL
    mat <- mat[rowSums(abs(mat)) > 0.0001, , drop = FALSE]
    mat <- mat[, colSums(abs(mat)) > 0.0001, drop = FALSE]
    mat
  }

  message("Building ID-partner connectivity matrix...")
  inout_matrix_id <- build_inout_matrix("id")
  message(sprintf("  %d rows × %d cols", nrow(inout_matrix_id), ncol(inout_matrix_id)))
  message("Building cell_sub_type-partner connectivity matrix...")
  inout_matrix_ct <- build_inout_matrix("cell_sub_type")
  message(sprintf("  %d rows × %d cols", nrow(inout_matrix_ct), ncol(inout_matrix_ct)))

  # Choose primary matrix based on flag (accept legacy "cell_type" as a synonym)
  if (partner_grouping %in% c("cell_sub_type", "cell_type")) {
    inout_connection_matrix <- inout_matrix_ct
  } else {
    inout_connection_matrix <- inout_matrix_id
  }
  message(sprintf("Using %s-partner matrix as primary for main pipeline", partner_grouping))
  
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
  p <- length(var_explained)

  # --- Broken stick model ---
  # Expected proportion of variance for the j-th component under random partitioning
  bstick_expected <- sapply(1:p, function(j) sum(1 / (j:p))) / p
  n_bstick <- max(which(var_explained > bstick_expected))
  message(sprintf("  Broken stick: %d significant components", n_bstick))

  # --- Marchenko-Pastur (random matrix theory) ---
  # Upper bound of noise eigenvalue distribution for n×p matrix
  n_obs <- nrow(pca_result$x)
  gamma <- p / n_obs  # aspect ratio
  eigenvalues <- pca_result$sdev^2
  # Estimate noise variance from the median eigenvalue (robust to signal components)
  sigma2 <- median(eigenvalues)
  mp_upper <- sigma2 * (1 + sqrt(gamma))^2
  n_mp <- sum(eigenvalues > mp_upper)
  message(sprintf("  Marchenko-Pastur: %d signal components (upper bound=%.2f, sigma2=%.2f)",
                  n_mp, mp_upper, sigma2))

  # Use broken stick as primary (more conservative, well-established in biology)
  n_components <- n_bstick
  n_components_50pct <- which(cumulative_var_explained >= 0.50)[1]

  # Plot: observed vs broken stick vs MP threshold
  pca_selection_df <- data.frame(
    component = 1:min(100, p),
    observed = var_explained[1:min(100, p)],
    broken_stick = bstick_expected[1:min(100, p)]
  )
  ggsave(
    ggplot(pca_selection_df) +
      geom_line(aes(component, observed, color = "observed")) +
      geom_line(aes(component, broken_stick, color = "broken stick expected")) +
      geom_hline(aes(yintercept = mp_upper / sum(eigenvalues), color = "Marchenko-Pastur"),
                 linetype = "dashed") +
      geom_vline(xintercept = n_bstick, linetype = "dotted", color = "steelblue") +
      geom_vline(xintercept = n_mp, linetype = "dotted", color = "darkred") +
      annotate("text", x = n_bstick + 1, y = max(var_explained) * 0.9,
               label = sprintf("broken stick: %d", n_bstick), size = 3, hjust = 0, color = "steelblue") +
      annotate("text", x = n_mp + 1, y = max(var_explained) * 0.8,
               label = sprintf("MP: %d", n_mp), size = 3, hjust = 0, color = "darkred") +
      scale_color_manual(values = c("observed" = "black", "broken stick expected" = "steelblue",
                                    "Marchenko-Pastur" = "darkred")) +
      scale_x_continuous(breaks = seq(0, 100, 10)) +
      labs(x = "Principal component", y = "Proportion of variance", color = NULL,
           title = "PCA dimension selection: broken stick vs Marchenko-Pastur") +
      theme_minimal() + theme(legend.position = "bottom"),
    filename = file.path(banc.fig3.extra.path, "pca_dimension_selection.pdf"),
    width = 8, height = 5, dpi = 300)

  print(paste("Number of components (broken stick):", n_bstick))
  print(paste("Number of components (Marchenko-Pastur):", n_mp))
  print(paste("Number of components (50% variance):", n_components_50pct))

  # Save PCA variance summary (UMAP variance appended after UMAP computation below)
  .pca_var_lines <- c(
    "PCA / UMAP variance summary for AN/DN cosine connectivity UMAP",
    paste0("Date: ", Sys.Date()),
    "",
    sprintf("Total PCA dimensions: %d", p),
    sprintf("Components (broken stick): %d (%.1f%% variance)",
            n_bstick, cumulative_var_explained[n_bstick] * 100),
    sprintf("Components (Marchenko-Pastur): %d (%.1f%% variance)",
            n_mp, cumulative_var_explained[n_mp] * 100),
    sprintf("Components (50%% variance): %d", n_components_50pct),
    "",
    sprintf("PC1 variance explained: %.2f%%", var_explained[1] * 100),
    sprintf("PC2 variance explained: %.2f%%", var_explained[2] * 100),
    sprintf("PC1 + PC2 cumulative: %.2f%%", cumulative_var_explained[2] * 100)
  )
  
  # DISABLED 2026-04-08: alternative UMAP variant using a subset of
  # "ref" (in-group) vs "proj" (out-group) neck neurons. Superseded by
  # the canonical full-population PCA-UMAP in the active branch above
  # (Methods §"Clustering influence by influence and connectivity").
  # Kept as a reference for the partial-population variant.
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
  set.seed(23)  
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

  # UMAP variance: how well do the top n PCs (used for clustering) predict UMAP layout?
  pca_top <- pca_result$x[, 1:n_components, drop = FALSE]
  umap1_r2 <- summary(lm(umap_result[,1] ~ pca_top))$r.squared
  umap2_r2 <- summary(lm(umap_result[,2] ~ pca_top))$r.squared

  # Option 3: UMAP from PCA embedding (aligned with clustering space)
  message("Computing PCA-based UMAP (option 3)...")
  set.seed(42)
  umap_pca_result <- uwot::umap(pca_result$x[, 1:n_components, drop = FALSE],
                                metric = "euclidean",
                                n_epochs = 500,
                                n_neighbors = 100,
                                min_dist = 0,
                                n_trees = 100,
                                spread = 10,
                                n_components = 2)
  rownames(umap_pca_result) <- colnames(inout_connection_matrix)

  .pca_var_lines <- c(.pca_var_lines, "",
    sprintf("UMAP variance (R-squared of top %d PCs predicting cosine-UMAP coordinates):", n_components),
    sprintf("  UMAP1 R-squared: %.4f", umap1_r2),
    sprintf("  UMAP2 R-squared: %.4f", umap2_r2),
    "",
    sprintf("PCA-UMAP: UMAP computed from %d PCA dimensions (euclidean metric)", n_components)
  )
  writeLines(.pca_var_lines, file.path(banc.fig3.extra.path, "an_dn_umap_pca_variance_summary.txt"))
  message("Saved PCA/UMAP variance summary to ", file.path(banc.fig3.extra.path, "an_dn_umap_pca_variance_summary.txt"))

  # Create a data frame with UMAP coordinates (cosine UMAP as primary, PCA UMAP stored)
  umap.dn.df <- data.frame(
    UMAP1 = umap_result[,1],
    UMAP2 = umap_result[,2],
    PCA_UMAP1 = umap_pca_result[,1],
    PCA_UMAP2 = umap_pca_result[,2],
    id = rownames(umap_result)) %>% 
    dplyr::left_join(banc.meta %>%
                       dplyr::select(id, neurotransmitter,
                                     side, region, super_class,
                                     hemilineage, cell_function, nerve,
                                     cell_sub_type, cell_class, cell_sub_class,
                                     cell_type, fafb_cell_type, manc_cell_type,
                                     super_cluster, st_cluster = cluster,
                                     manual_cluster, seed_12, pd_width) %>%
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

  # Override super_cluster with v850 snapshot (so comparison plots reflect the older
  # super_cluster demarcations, not ongoing SeaTable edits).
  v850_snapshot_path <- sprintf("data/banc_annotations/v%d/banc_neck_functional_classes.csv", .version_num)
  if (file.exists(v850_snapshot_path)) {
    v850_snapshot <- readr::read_csv(v850_snapshot_path, col_types = readr::cols(.default = "c")) %>%
      dplyr::select(id, super_cluster_v850 = super_cluster) %>%
      dplyr::mutate(id = as.character(id)) %>%
      dplyr::distinct(id, .keep_all = TRUE)
    umap.dn.df <- umap.dn.df %>%
      dplyr::left_join(v850_snapshot, by = "id") %>%
      dplyr::mutate(super_cluster = dplyr::coalesce(super_cluster_v850, super_cluster)) %>%
      dplyr::select(-super_cluster_v850)
    message(sprintf("  Overrode super_cluster with v850 snapshot (%d neurons)",
                    sum(umap.dn.df$id %in% v850_snapshot$id)))
  } else {
    warning("v850 snapshot not found at ", v850_snapshot_path, " — using current SeaTable super_cluster")
  }

  # For AN/DN UMAP visualisation, use modality only (not behaviour/response/valence)
  # cell_function in banc.meta uses a cascade (modality→behaviour→response→valence),
  # but for icon placement we only want modality-based assignments
  modality_lookup <- cns.functions %>%
    dplyr::filter(!is.na(modality), modality != "") %>%
    dplyr::distinct(cell_type, modality)
  # Strip _{single letter} suffix from cell_type to match functions table
  # e.g. AN19A018_c → AN19A018
  umap.dn.df$cell_type_base <- sub("_[a-z]$", "", umap.dn.df$cell_type)
  umap.dn.df <- umap.dn.df %>%
    dplyr::left_join(modality_lookup, by = c("cell_type_base" = "cell_type"))
  umap.dn.df$cell_function_plot <- dplyr::if_else(
    is.na(umap.dn.df$modality) | tolower(umap.dn.df$modality) == "unknown",
    NA_character_, umap.dn.df$modality)

  # --- Old Method 1: cutreeDynamic on 2D UMAP (kept for reference) ---
  # dist_matrix <- dist(umap_result[,1:2], method = "euclidean")
  # hc <- hclust(dist_matrix, method = "ward.D2")
  # method1_clusters <- cutreeDynamic(hc, distM = as.matrix(dist_matrix),
  #                                   deepSplit = 4, minClusterSize = 2)
  # names(method1_clusters) <- colnames(inout_connection_matrix)

  ############################################
  ### CLUSTERING: cutreeDynamic on PCA     ###
  ### + cell-type gerrymandering           ###
  ############################################

  cluster_options_path <- file.path(banc.fig3.extra.path, "cluster_options")
  dir.create(cluster_options_path, showWarnings = FALSE, recursive = TRUE)

  umap_ids <- colnames(inout_connection_matrix)

  # AN/DN aggregation lookup. The chosen column is set by `andn_grouping`
  # at the top of the script ("cell_sub_type" or "cell_sub_class").
  .andn_col <- andn_grouping
  if (!.andn_col %in% colnames(banc.an.dn.meta)) {
    stop(sprintf("andn_grouping = '%s' not found in banc.an.dn.meta columns", .andn_col))
  }
  message(sprintf("AN/DN aggregation column: %s", .andn_col))
  ct_lookup <- setNames(banc.an.dn.meta[[.andn_col]][match(umap_ids, banc.an.dn.meta$root_id)], umap_ids)
  ct_lookup[is.na(ct_lookup) | ct_lookup == ""] <- paste0("unknown_", seq_len(sum(is.na(ct_lookup) | ct_lookup == "")))
  sc_lookup <- setNames(banc.meta$super_cluster[match(umap_ids, banc.meta$root_id)], umap_ids)
  sc_valid <- !is.na(sc_lookup) & sc_lookup != ""

  # Cell-type completeness (information-theoretic)
  ct_completeness <- function(clusters) {
    valid <- !grepl("^unknown_", ct_lookup)
    classes <- ct_lookup[valid]
    clusts <- as.character(clusters[valid])
    n <- length(classes)
    if (n == 0) return(0)
    tab <- table(clusts, classes)
    H <- function(x) { p <- x / sum(x); p <- p[p > 0]; -sum(p * log(p)) }
    H_K <- H(rowSums(tab))
    if (H_K == 0) return(1)
    H_K_given_C <- sum(colSums(tab) / n * apply(tab, 2, function(col) {
      s <- sum(col); if (s == 0) 0 else { p <- col / s; p <- p[p > 0]; -sum(p * log(p)) }
    }))
    1 - H_K_given_C / H_K
  }

  # ============================================================
  # HYBRID CLUSTERING:
  # - UMAP visualization: per-neuron (unchanged, from full cosine matrix)
  # - Clustering: per-cell_sub_type, hardcoded dynamicTreeCut params (ds=0, mcs=8)
  # - Each neuron inherits its cell type's cluster assignment
  # Run on BOTH partner-grouping matrices × {brokenstick, marchenko_pastur}
  # for sensitivity comparison; the primary is selected by `partner_grouping`
  # and `dim_method` script-level flags above.
  # ============================================================

  # --- Step 3: cell-type influence profiles (DECOMMISSIONED 2026-04-08) ---
  #
  # Previously this block built `cct_influence` (cell types × influence features)
  # by running two `query_influence()` calls (sensory→AN/DN with 115 seeds,
  # AN/DN→effector with ~3149 seeds), then computed `cct_influence_dist` and
  # used it as the silhouette scoring space for a (deepSplit, minClusterSize)
  # parameter scan inside `cluster_with_dims()`.
  #
  # We dropped this because:
  #   1) The silhouette landscape is shallow and noise-dominated (best scores
  #      around -0.21 to -0.25 across all variants × all grid cells), so the
  #      "optimal" parameter combo flips around with annotation refreshes
  #      (e.g. on 2026-04-08 the chosen primary jumped from 18 to 41 clusters
  #      after a SeaTable annotation refresh, with no corresponding biological
  #      change — see CLAUDE.md "constrain cluster scan" notes).
  #   2) Removing the two query_influence calls saves ~1 hr of script runtime
  #      (the script now only computes the high-level seed_12 influence at
  #      line ~1949 for the per-super_class umap-overlay plots, which is
  #      independent of clustering).
  #   3) `cluster_with_dims()` now hardcodes deepSplit=0, minClusterSize=8
  #      (the previous run's optimum, kept as a fixed convention).
  #
  # If you want to revive the silhouette-based selection, restore the block
  # below and the scan loop in `cluster_with_dims()`.
  #
  # message("Step 3: Building cell-type influence profiles...")
  #
  # DISABLED 2026-04-08: influence-feature variant of the AN/DN clustering
  # (cluster on per-AN/DN influence-from-sensory + influence-to-effector
  # profiles instead of typed partner connectivity). Superseded by the
  # PCA-UMAP swap (cosine-on-partner-connectivity → primary, PCA on
  # Marchenko-Pastur-truncated connectivity-cosine → embedding;
  # Methods §"Clustering influence by influence and connectivity").
  # Kept as a reference for the influence-feature variant if needed for
  # an ED-supplement comparison.
  # # Sensory → AN/DN influence (seed_02 level)
  # sens_seeds <- na.omit(unique(banc.sens.meta$seed_02))
  # message(sprintf("  Querying sensory→AN/DN influence (%d sensory seeds)...", length(sens_seeds)))
  # inf_sens <- query_influence(
  #   levels = "seed_02", seeds = sens_seeds,
  #   ids = umap_ids, normalize = FALSE, ncores = 1L
  # ) %>% calculate_influence_norms()
  #
  # # Map seed_02 → cell_sub_class_body_part_sensory (majority vote)
  # sens_group_map <- banc.sens.meta %>%
  #   dplyr::mutate(sens_group = paste(
  #     dplyr::coalesce(cell_sub_class, cell_class, "unknown"),
  #     dplyr::coalesce(body_part_sensory, "unknown"),
  #     sep = "_")) %>%
  #   dplyr::filter(!is.na(seed_02), sens_group != "unknown_unknown") %>%
  #   dplyr::count(seed_02, sens_group) %>%
  #   dplyr::group_by(seed_02) %>%
  #   dplyr::slice_max(n, n = 1, with_ties = FALSE) %>%
  #   dplyr::ungroup() %>%
  #   dplyr::distinct(seed_02, sens_group)
  #
  # # Per-neuron sensory profile, then aggregate to cell type
  # sens_profile_neuron <- inf_sens %>%
  #   dplyr::left_join(sens_group_map, by = c("seed" = "seed_02")) %>%
  #   dplyr::filter(!is.na(sens_group)) %>%
  #   dplyr::group_by(id, sens_group) %>%
  #   dplyr::summarise(influence_log = mean(influence_log, na.rm = TRUE), .groups = "drop") %>%
  #   tidyr::pivot_wider(names_from = sens_group, values_from = influence_log, values_fill = 0)
  #
  # # Aggregate per neuron → per cell type
  # sens_profile_cct <- sens_profile_neuron %>%
  #   dplyr::mutate(cct = ct_lookup[id]) %>%
  #   dplyr::filter(!grepl("^unknown_", cct)) %>%
  #   dplyr::group_by(cct) %>%
  #   dplyr::summarise(dplyr::across(-id, mean, na.rm = TRUE), .groups = "drop") %>%
  #   tibble::column_to_rownames("cct")
  #
  # # AN/DN → effector influence (seed_12 level)
  # andn_seeds <- na.omit(unique(banc.an.dn.meta$seed_12))
  # eff_ids <- banc.eff.meta$root_id
  # message(sprintf("  Querying AN/DN→effector influence (%d AN/DN seeds)...", length(andn_seeds)))
  # inf_eff <- query_influence(
  #   levels = "seed_12", seeds = andn_seeds,
  #   ids = eff_ids, normalize = FALSE, ncores = 1L
  # ) %>% calculate_influence_norms()
  #
  # # Map effector neurons to cell_sub_class + body_part_effector
  # eff_class_map <- banc.eff.meta %>%
  #   dplyr::mutate(eff_group = paste(
  #     dplyr::coalesce(cell_sub_class, cell_class, "unknown"),
  #     dplyr::coalesce(body_part_effector, "unknown"),
  #     sep = "_")) %>%
  #   dplyr::filter(eff_group != "unknown_unknown") %>%
  #   dplyr::distinct(root_id, eff_group)
  #
  # # Effector profile per seed_12 (AN/DN group)
  # eff_profile_by_seed <- inf_eff %>%
  #   dplyr::left_join(eff_class_map, by = c("id" = "root_id")) %>%
  #   dplyr::filter(!is.na(eff_group)) %>%
  #   dplyr::group_by(seed, eff_group) %>%
  #   dplyr::summarise(influence_log = mean(influence_log, na.rm = TRUE), .groups = "drop") %>%
  #   tidyr::pivot_wider(names_from = eff_group, values_from = influence_log, values_fill = 0) %>%
  #   tibble::column_to_rownames("seed")
  #
  # # Map seed_12 → AN/DN neuron → cell type
  # seed12_map <- banc.an.dn.meta %>%
  #   dplyr::filter(!is.na(seed_12), root_id %in% umap_ids) %>%
  #   dplyr::distinct(root_id, seed_12) %>%
  #   dplyr::mutate(cct = ct_lookup[root_id]) %>%
  #   dplyr::filter(!grepl("^unknown_", cct))
  #
  # # For each cell type, average effector profile across its neurons (via their seed_12s)
  # eff_profile_cct <- seed12_map %>%
  #   dplyr::filter(seed_12 %in% rownames(eff_profile_by_seed)) %>%
  #   dplyr::mutate(row_idx = match(seed_12, rownames(eff_profile_by_seed)))
  # eff_profile_cct_mat <- do.call(rbind, lapply(unique(eff_profile_cct$cct), function(cct) {
  #   rows <- eff_profile_cct$row_idx[eff_profile_cct$cct == cct]
  #   colMeans(eff_profile_by_seed[rows, , drop = FALSE])
  # }))
  # rownames(eff_profile_cct_mat) <- unique(eff_profile_cct$cct)
  #
  # # Align sens and eff profiles to common cell types
  # # (Restriction to connectivity matrix happens per-matrix inside run_cct_pipeline)
  # common_cct <- intersect(rownames(sens_profile_cct), rownames(eff_profile_cct_mat))
  # message(sprintf("  %d cell types with both sensory and effector influence profiles",
  #                 length(common_cct)))
  #
  # sens_aligned <- matrix(0, nrow = length(common_cct), ncol = ncol(sens_profile_cct),
  #                        dimnames = list(common_cct, colnames(sens_profile_cct)))
  # sens_aligned[rownames(sens_profile_cct)[rownames(sens_profile_cct) %in% common_cct], ] <-
  #   as.matrix(sens_profile_cct[rownames(sens_profile_cct) %in% common_cct, ])
  # eff_aligned <- eff_profile_cct_mat[common_cct[common_cct %in% rownames(eff_profile_cct_mat)], , drop = FALSE]
  # # Pad with zeros for cell types missing from eff
  # missing_eff <- setdiff(common_cct, rownames(eff_aligned))
  # if (length(missing_eff) > 0) {
  #   pad <- matrix(0, nrow = length(missing_eff), ncol = ncol(eff_profile_cct_mat),
  #                 dimnames = list(missing_eff, colnames(eff_profile_cct_mat)))
  #   eff_aligned <- rbind(eff_aligned, pad)[common_cct, , drop = FALSE]
  # }
  #
  # # Concatenate: cell-type influence profile = [sensory | effector]
  # cct_influence <- cbind(sens_aligned, eff_aligned)
  # message(sprintf("  Cell-type influence profiles: %d cell types × %d features (%d sensory + %d effector)",
  #                 nrow(cct_influence), ncol(cct_influence),
  #                 ncol(sens_aligned), ncol(eff_aligned)))
  #
  # # Pre-compute influence distance matrix (cosine, for silhouette)
  # cct_influence_dist <- as.matrix(proxy::dist(cct_influence, method = "cosine"))
  # cct_influence_dist[is.na(cct_influence_dist) | is.infinite(cct_influence_dist)] <- 1

  # --- Step 1+2: Per-matrix collapse + PCA (computed once per partner matrix) ---
  collapse_and_pca <- function(conn_mat, label) {
    message(sprintf("[%s] Collapsing matrix by %s...", label, andn_grouping))
    neuron_to_cct_local <- ct_lookup[colnames(conn_mat)]
    cct_ids_all <- sort(unique(neuron_to_cct_local))
    cct_ids_valid <- cct_ids_all[!grepl("^unknown_", cct_ids_all)]
    cct_mat <- sapply(cct_ids_valid, function(cct) {
      cols <- which(neuron_to_cct_local == cct)
      if (length(cols) == 1) return(conn_mat[, cols])
      rowMeans(conn_mat[, cols, drop = FALSE])
    })
    unk_cols <- which(grepl("^unknown_", neuron_to_cct_local))
    if (length(unk_cols) > 0) {
      unk_mat <- as.matrix(conn_mat[, unk_cols, drop = FALSE])
      colnames(unk_mat) <- neuron_to_cct_local[unk_cols]
      cct_mat <- cbind(cct_mat, unk_mat)
    }
    cct_mat <- cct_mat[rowSums(abs(cct_mat)) > 0.0001, , drop = FALSE]
    message(sprintf("  [%s] %d cell types × %d partners", label, ncol(cct_mat), nrow(cct_mat)))

    cct_sp <- as(as.matrix(t(cct_mat)), "dgCMatrix")
    cct_cs <- cosine_similarity_sparse(t(cct_sp))
    cct_cs[is.infinite(cct_cs)] <- 0
    dimnames(cct_cs) <- list(colnames(cct_mat), colnames(cct_mat))
    cct_pca_local <- prcomp(cct_cs, center = TRUE, scale. = TRUE)
    cct_var_local <- cct_pca_local$sdev^2 / sum(cct_pca_local$sdev^2)
    cct_p_local <- length(cct_var_local)

    # Broken stick
    cct_bstick_local <- sapply(1:cct_p_local, function(j) sum(1 / (j:cct_p_local))) / cct_p_local
    cct_nbs <- max(which(cct_var_local > cct_bstick_local))

    # Marchenko-Pastur
    cct_eigenvalues <- cct_pca_local$sdev^2
    cct_n_obs <- nrow(cct_pca_local$x)
    cct_gamma <- cct_p_local / cct_n_obs
    cct_sigma2 <- median(cct_eigenvalues)
    cct_mp_upper <- cct_sigma2 * (1 + sqrt(cct_gamma))^2
    cct_nmp <- sum(cct_eigenvalues > cct_mp_upper)
    if (cct_nmp < 2) cct_nmp <- max(2, cct_nbs)  # safety

    message(sprintf("  [%s] PCA: %d components, broken stick = %d, Marchenko-Pastur = %d",
                    label, cct_p_local, cct_nbs, cct_nmp))

    list(cct_mat = cct_mat, pca = cct_pca_local,
         n_bstick = cct_nbs, n_mp = cct_nmp, n_total = cct_p_local)
  }

  # --- Cluster: takes pre-computed PCA + a dim count, applies hardcoded params ---
  # Hardcoded deepSplit=0, minClusterSize=8 (the previous run's optimum, kept as
  # the fixed convention since 2026-04-08; influence-silhouette scan removed —
  # see the decommissioned "Step 3" block above for rationale).
  cluster_with_dims <- function(cp, label, n_dims, dim_label) {
    message(sprintf("[%s|%s] Clustering with %d PCA dims (ds=0, mcs=8)...",
                    label, dim_label, n_dims))
    cct_pca_sub <- cp$pca$x[, 1:n_dims, drop = FALSE]
    cct_d <- dist(cct_pca_sub, method = "euclidean")
    cct_hc <- hclust(cct_d, method = "ward.D2")

    cct_cl <- dynamicTreeCut::cutreeDynamic(cct_hc, distM = as.matrix(cct_d),
                                            deepSplit = 0, minClusterSize = 8)
    names(cct_cl) <- colnames(cp$cct_mat)
    n_cl <- length(unique(cct_cl[cct_cl != 0]))
    message(sprintf("  [%s|%s] Hardcoded ds=0, mcs=8 → %d clusters",
                    label, dim_label, n_cl))

    # Stub `best` field so downstream consumers (clustering_validation.txt writer,
    # per-variant message loop) can still read $best$deepSplit, $best$minClusterSize,
    # $best$n_clusters without conditional logic.
    best <- data.frame(deepSplit = 0L, minClusterSize = 8L, n_clusters = n_cl)

    fc <- setNames(rep(0L, length(umap_ids)), umap_ids)
    for (i in seq_along(umap_ids)) {
      cct <- ct_lookup[[umap_ids[i]]]
      if (cct %in% names(cct_cl)) fc[i] <- cct_cl[[cct]]
    }
    list(clusters = fc, cct_clusters = cct_cl, scan = NULL, best = best,
         n_cct = ncol(cp$cct_mat), n_dims = n_dims, dim_label = dim_label,
         partner_label = label, cct_matrix = cp$cct_mat)
  }

  # --- Compute PCA per partner matrix once, then cluster with both dim methods ---
  cp_id <- collapse_and_pca(inout_matrix_id, "id_partners")
  cp_ct <- collapse_and_pca(inout_matrix_ct, "celltype_partners")

  res_id_bs <- cluster_with_dims(cp_id, "id_partners",       cp_id$n_bstick, "brokenstick")
  res_id_mp <- cluster_with_dims(cp_id, "id_partners",       cp_id$n_mp,     "marchenko_pastur")
  res_ct_bs <- cluster_with_dims(cp_ct, "celltype_partners", cp_ct$n_bstick, "brokenstick")
  res_ct_mp <- cluster_with_dims(cp_ct, "celltype_partners", cp_ct$n_mp,     "marchenko_pastur")

  all_variants <- list(
    id_partners_brokenstick       = res_id_bs,
    id_partners_marchenko_pastur  = res_id_mp,
    celltype_partners_brokenstick = res_ct_bs,
    celltype_partners_marchenko_pastur = res_ct_mp
  )

  # Primary = matches partner_grouping × dim_method flags
  primary_key <- paste0(
    if (partner_grouping == "id") "id_partners" else "celltype_partners",
    "_", dim_method
  )
  primary_res <- all_variants[[primary_key]]
  final_clusters <- primary_res$clusters
  best_params <- primary_res$best
  cct_n_bstick <- primary_res$n_dims  # name kept for downstream code
  cct_matrix <- primary_res$cct_matrix
  n_final <- length(unique(final_clusters[final_clusters != 0]))
  message(sprintf("Primary (%s × %s): %d clusters", partner_grouping, dim_method, n_final))
  for (vname in names(all_variants)) {
    v <- all_variants[[vname]]
    n_v <- length(unique(v$clusters[v$clusters != 0]))
    marker <- if (vname == primary_key) "  ★" else "   "
    message(sprintf("%s %s: %d dims, %d clusters",
                    marker, vname, v$n_dims, n_v))
  }

  # --- Validation metrics (not optimization) ---
  val_ari_sc <- mclust::adjustedRandIndex(final_clusters[sc_valid], sc_lookup[sc_valid])
  val_ct_comp <- ct_completeness(final_clusters)
  message(sprintf("  Validation: ARI vs super_cluster=%.3f, cell_type completeness=%.3f",
                  val_ari_sc, val_ct_comp))

  # No need for gerrymandering — clusters are defined at cell-type level, so all
  # neurons of the same cell type automatically share a cluster.
  n_moved_total <- 0

  # Compute validation for all 4 variants
  validation_lines <- c(
    "AN/DN clustering validation metrics (cell-type PCA, hardcoded ds=0/mcs=8)",
    paste0("Date: ", Sys.Date()),
    sprintf("Primary partner grouping: %s × %s", partner_grouping, dim_method),
    sprintf("Cluster params: deepSplit=0, minClusterSize=8 (fixed; no influence-silhouette scan)"),
    ""
  )
  for (vname in names(all_variants)) {
    v <- all_variants[[vname]]
    v_ari <- mclust::adjustedRandIndex(v$clusters[sc_valid], sc_lookup[sc_valid])
    v_comp <- ct_completeness(v$clusters)
    n_v <- length(unique(v$clusters[v$clusters != 0]))
    validation_lines <- c(validation_lines,
      sprintf("=== %s ===", vname),
      sprintf("  Partner grouping: %s", v$partner_label),
      sprintf("  PCA dim selection: %s (%d dims)", v$dim_label, v$n_dims),
      sprintf("  Cell types clustered: %d", v$n_cct),
      sprintf("  Params: deepSplit=%d, minClusterSize=%d",
              v$best$deepSplit, v$best$minClusterSize),
      sprintf("  Final clusters: %d", n_v),
      sprintf("  ARI vs super_cluster: %.3f", v_ari),
      sprintf("  Cell-type completeness: %.3f", v_comp),
      ""
    )
  }
  writeLines(validation_lines, file.path(cluster_options_path, "clustering_validation.txt"))

  # Assign to umap.dn.df
  umap.dn.df$clusterno <- factor(final_clusters[umap.dn.df$id])

  # --- Visualise all methods ---
  message("Generating cluster option plots...")

  # Icon setup (use cell_function_plot = modality, which matches icon filenames)
  icon_folder <- "figures/schematics/assets/umap_icons"
  umap.dn.df$image_path <- ifelse(
    !is.na(umap.dn.df$cell_function_plot) & tolower(umap.dn.df$cell_function_plot) != "unknown",
    file.path(icon_folder, paste0(umap.dn.df$cell_function_plot, ".svg")),
    NA_character_
  )
  umap.dn.df$image_path[!is.na(umap.dn.df$image_path) & !file.exists(umap.dn.df$image_path)] <- NA

  methods <- list(
    "id_partners_brokenstick"            = res_id_bs$clusters,
    "id_partners_marchenko_pastur"       = res_id_mp$clusters,
    "celltype_partners_brokenstick"      = res_ct_bs$clusters,
    "celltype_partners_marchenko_pastur" = res_ct_mp$clusters
  )

  for (method_name in names(methods)) {
    message(sprintf("  Plotting %s...", method_name))
    clust_vec <- methods[[method_name]]

    # Skip methods that failed (all NA)
    if (all(is.na(clust_vec))) {
      message(sprintf("    Skipping %s — no cluster assignments", method_name))
      next
    }

    # Assign clusters to umap.dn.df
    plot_df <- umap.dn.df %>%
      dplyr::mutate(
        method_cluster = as.character(clust_vec[id]),
        method_cluster = dplyr::if_else(is.na(method_cluster) | method_cluster == "0",
                                        "unassigned", method_cluster)
      )

    n_clust <- length(setdiff(unique(plot_df$method_cluster), "unassigned"))
    clust_cols <- cerise_limon_palette(max(n_clust, 1))
    clust_levels <- sort(setdiff(unique(plot_df$method_cluster), "unassigned"))
    names(clust_cols) <- clust_levels

    # Build hulls for this clustering
    hull_df <- plot_df %>%
      dplyr::filter(method_cluster != "unassigned", !is.na(UMAP1), !is.na(UMAP2)) %>%
      dplyr::group_by(method_cluster) %>%
      dplyr::filter(dplyr::n() >= 3) %>%
      dplyr::do({
        cl_id <- unique(.$method_cluster)
        hull_data <- concaveman::concaveman(as.matrix(.[, c("UMAP1", "UMAP2")]),
                                            concavity = 2, length_threshold = 0.5)
        as.data.frame(hull_data) %>% dplyr::mutate(method_cluster = cl_id)
      }) %>%
      dplyr::ungroup()

    # Cluster centroids for labels
    clust_centroids <- plot_df %>%
      dplyr::filter(method_cluster != "unassigned") %>%
      dplyr::group_by(method_cluster) %>%
      dplyr::summarise(UMAP1 = mean(UMAP1, na.rm = TRUE),
                       UMAP2 = mean(UMAP2, na.rm = TRUE), .groups = "drop")

    # Only show hulls for method 1 (cutreeDynamic) — too overlapping for other methods
    use_hulls <- grepl("method1", method_name)

    # (a) UMAP coloured by clustering scheme
    g_a <- ggplot(plot_df, aes(x = UMAP1, y = UMAP2))
    if (use_hulls) {
      g_a <- g_a + ggplot2::stat_density_2d(
        data = subset(plot_df, method_cluster != "unassigned"),
        mapping = aes(group = method_cluster, fill = method_cluster),
        geom = "polygon", colour = NA, alpha = 0.5, contour = TRUE,
        bins = 3, inherit.aes = FALSE)
    }
    g_a <- g_a +
      geom_point(data = subset(plot_df, method_cluster == "unassigned"),
                 color = "grey70", size = 0.3, alpha = 0.3) +
      geom_point(data = subset(plot_df, method_cluster != "unassigned"),
                 aes(color = method_cluster), size = 1, alpha = 0.7) +
      scale_color_manual(values = clust_cols, guide = "none") +
      scale_fill_manual(values = clust_cols, guide = "none") +
      geom_text(data = clust_centroids, aes(label = method_cluster),
                size = 3, fontface = "bold") +
      theme_void() + coord_fixed() +
      labs(title = gsub("_", " ", method_name))

    ggsave(g_a, filename = file.path(cluster_options_path,
           paste0(method_name, "_a_cluster_colours.pdf")),
           width = 8, height = 8, dpi = 300)

    # (b) Cluster boundaries (method 1 only) + super_cluster colours
    g_b <- ggplot(plot_df, aes(x = UMAP1, y = UMAP2))
    if (use_hulls) {
      g_b <- g_b + ggplot2::stat_density_2d(
        data = subset(plot_df, !is.na(super_cluster)),
        mapping = aes(group = method_cluster, fill = super_cluster),
        geom = "polygon", colour = NA, alpha = 0.5, contour = TRUE,
        bins = 3, inherit.aes = FALSE) +
        scale_fill_manual(values = paper.cols, guide = "none") +
        ggnewscale::new_scale_fill()
    }
    g_b <- g_b +
      geom_point(data = subset(plot_df, is.na(super_cluster)),
                 color = "grey70", size = 0.3, alpha = 0.3) +
      geom_point(data = subset(plot_df, !is.na(super_cluster)),
                 aes(color = super_cluster), size = 1, alpha = 0.7) +
      scale_color_manual(values = paper.cols) +
      geom_text(data = clust_centroids, aes(label = method_cluster),
                size = 3, fontface = "bold", color = "black") +
      theme_void() + coord_fixed() +
      labs(title = paste0(gsub("_", " ", method_name), " — super_cluster colours")) +
      theme(legend.position = "bottom", legend.text = element_text(size = 6))

    ggsave(g_b, filename = file.path(cluster_options_path,
           paste0(method_name, "_b_super_cluster_colours.pdf")),
           width = 10, height = 9, dpi = 300)

    # (c) Cluster boundaries (method 1 only) + cell_function icons with legend
    plot_df_known <- subset(plot_df, !is.na(image_path))
    g_c <- ggplot(plot_df, aes(x = UMAP1, y = UMAP2))
    if (use_hulls) {
      g_c <- g_c + ggplot2::stat_density_2d(
        data = subset(plot_df, !is.na(super_cluster)),
        mapping = aes(group = method_cluster, fill = super_cluster),
        geom = "polygon", colour = NA, alpha = 0.5, contour = TRUE,
        bins = 3, inherit.aes = FALSE) +
        scale_fill_manual(values = paper.cols, guide = "none") +
        ggnewscale::new_scale_fill()
    }
    g_c <- g_c +
      geom_point(data = subset(plot_df, is.na(image_path)),
                 color = "grey70", size = 0.3, alpha = 0.75) +
      # Invisible points for legend mapping
      geom_point(data = plot_df_known,
                 aes(color = cell_function_plot),
                 size = 0, alpha = 0) +
      ggimage::geom_image(
        data = plot_df_known,
        aes(image = image_path),
        size = 0.018,
        asp = 1,
        alpha = 0.85, 
        alpha = 0.75
      ) +
      scale_color_cerise_limon() +
      guides(color = guide_legend(title = "cell function",
                                  nrow = 4, byrow = TRUE,
                                  override.aes = list(size = 3, alpha = 1))) +
      geom_text(data = clust_centroids, aes(label = method_cluster),
                size = 3, fontface = "bold", color = "black") +
      theme_void() + coord_fixed() +
      labs(title = paste0(gsub("_", " ", method_name), " — cell function icons")) +
      theme(legend.position = "bottom",
            legend.title = element_text(size = 8),
            legend.text = element_text(size = 6))

    ggsave(g_c, filename = file.path(cluster_options_path,
           paste0(method_name, "_c_cell_function_icons.pdf")),
           width = 10, height = 10, dpi = 300)

    # (d) Confusion matrices: new clusters vs manual_cluster AND vs super_cluster
    for (ref_col in c("manual_cluster", "super_cluster")) {
      conf_df <- plot_df %>%
        dplyr::filter(method_cluster != "unassigned",
                      !is.na(.data[[ref_col]]), .data[[ref_col]] != "",
                      .data[[ref_col]] != "unassigned")
      if (nrow(conf_df) < 10) next
      ct <- table(new_cluster = conf_df$method_cluster,
                  reference = conf_df[[ref_col]])
      ct_norm <- sweep(ct, 2, pmax(colSums(ct), 1), "/")
      pheatmap::pheatmap(
        ct_norm,
        cluster_rows = TRUE, cluster_cols = TRUE,
        clustering_method = "ward.D2",
        color = grDevices::colorRampPalette(c("white", "#1f4e79", "#b22222"))(100),
        main = paste0(gsub("_", " ", method_name), " vs ", ref_col),
        fontsize_row = 7, fontsize_col = 7,
        cellwidth = 10, cellheight = 10,
        filename = file.path(cluster_options_path,
                             paste0(method_name, "_d_vs_", ref_col, ".pdf"))
      )
    }
  }

  # --- PCA-UMAP versions of cluster option plots ---
  message("  Generating PCA-UMAP cluster option plots...")
  for (method_name in names(methods)) {
    clust_vec <- methods[[method_name]]
    if (all(is.na(clust_vec))) next

    plot_df <- umap.dn.df %>%
      dplyr::mutate(
        method_cluster = as.character(clust_vec[id]),
        method_cluster = dplyr::if_else(is.na(method_cluster) | method_cluster == "0",
                                        "unassigned", method_cluster)
      )
    n_clust <- length(setdiff(unique(plot_df$method_cluster), "unassigned"))
    clust_cols <- cerise_limon_palette(max(n_clust, 1))
    clust_levels <- sort(setdiff(unique(plot_df$method_cluster), "unassigned"))
    names(clust_cols) <- clust_levels

    clust_centroids_pca <- plot_df %>%
      dplyr::filter(method_cluster != "unassigned") %>%
      dplyr::group_by(method_cluster) %>%
      dplyr::summarise(PCA_UMAP1 = mean(PCA_UMAP1, na.rm = TRUE),
                       PCA_UMAP2 = mean(PCA_UMAP2, na.rm = TRUE), .groups = "drop")

    pca_prefix <- paste0("pca_umap_", method_name)

    # (a) PCA-UMAP coloured by cluster
    g_pa <- ggplot(plot_df, aes(x = PCA_UMAP1, y = PCA_UMAP2)) +
      geom_point(data = subset(plot_df, method_cluster == "unassigned"),
                 color = "grey70", size = 0.3, alpha = 0.3) +
      geom_point(data = subset(plot_df, method_cluster != "unassigned"),
                 aes(color = method_cluster), size = 1, alpha = 0.7) +
      scale_color_manual(values = clust_cols, guide = "none") +
      geom_text(data = clust_centroids_pca, aes(x = PCA_UMAP1, y = PCA_UMAP2,
                label = method_cluster), size = 3, fontface = "bold") +
      theme_void() + coord_fixed() +
      labs(title = paste0("PCA-UMAP: ", gsub("_", " ", method_name)))
    ggsave(g_pa, filename = file.path(cluster_options_path,
           paste0(pca_prefix, "_a_cluster_colours.pdf")),
           width = 8, height = 8, dpi = 300)

    # (b) PCA-UMAP coloured by super_cluster
    g_pb <- ggplot(plot_df, aes(x = PCA_UMAP1, y = PCA_UMAP2)) +
      geom_point(data = subset(plot_df, is.na(super_cluster)),
                 color = "grey70", size = 0.3, alpha = 0.3) +
      geom_point(data = subset(plot_df, !is.na(super_cluster)),
                 aes(color = super_cluster), size = 1, alpha = 0.7) +
      scale_color_manual(values = paper.cols) +
      geom_text(data = clust_centroids_pca, aes(x = PCA_UMAP1, y = PCA_UMAP2,
                label = method_cluster), size = 3, fontface = "bold", color = "black") +
      theme_void() + coord_fixed() +
      labs(title = paste0("PCA-UMAP: ", gsub("_", " ", method_name), " — super_cluster colours")) +
      theme(legend.position = "bottom", legend.text = element_text(size = 6))
    ggsave(g_pb, filename = file.path(cluster_options_path,
           paste0(pca_prefix, "_b_super_cluster_colours.pdf")),
           width = 10, height = 9, dpi = 300)

    # (c) PCA-UMAP with cell function icons
    plot_df_known <- subset(plot_df, !is.na(image_path))
    g_pc <- ggplot(plot_df, aes(x = PCA_UMAP1, y = PCA_UMAP2)) +
      geom_point(data = subset(plot_df, is.na(image_path)),
                 color = "grey70", size = 0.3, alpha = 0.75) +
      geom_point(data = plot_df_known, aes(color = cell_function_plot),
                 size = 0, alpha = 0) +
      ggimage::geom_image(data = plot_df_known, aes(image = image_path),
                          size = 0.018, asp = 1, alpha = 0.75) +
      scale_color_cerise_limon() +
      guides(color = guide_legend(title = "cell function", nrow = 4, byrow = TRUE,
                                  override.aes = list(size = 3, alpha = 1))) +
      geom_text(data = clust_centroids_pca, aes(x = PCA_UMAP1, y = PCA_UMAP2,
                label = method_cluster), size = 3, fontface = "bold", color = "black") +
      theme_void() + coord_fixed() +
      labs(title = paste0("PCA-UMAP: ", gsub("_", " ", method_name), " — cell function icons")) +
      theme(legend.position = "bottom", legend.title = element_text(size = 8),
            legend.text = element_text(size = 6))
    ggsave(g_pc, filename = file.path(cluster_options_path,
           paste0(pca_prefix, "_c_cell_function_icons.pdf")),
           width = 10, height = 10, dpi = 300)
  }

  # ============================================================
  # COLLAPSED BY COMPOSITE_CELL_TYPE — parallel analysis
  # Cluster cell types instead of individual neurons.
  # ============================================================
  message("Running parallel collapsed-by-cell_sub_type analysis...")
  cc_lookup <- setNames(banc.an.dn.meta$cell_sub_type[match(umap_ids, banc.an.dn.meta$root_id)], umap_ids)
  cc_lookup[is.na(cc_lookup) | cc_lookup == ""] <- paste0("unknown_", seq_len(sum(is.na(cc_lookup) | cc_lookup == "")))

  # Collapse inout_connection_matrix by cell_sub_type (mean across neurons of same type)
  neuron_to_cct <- cc_lookup[colnames(inout_connection_matrix)]
  cct_ids <- sort(unique(neuron_to_cct))
  # Exclude "unknown_*" placeholder types (each is its own category, doesn't aggregate)
  cct_ids_valid <- cct_ids[!grepl("^unknown_", cct_ids)]
  # Build collapsed matrix: partner_ids × cell_types
  cct_matrix <- sapply(cct_ids_valid, function(cct) {
    cols <- which(neuron_to_cct == cct)
    if (length(cols) == 1) return(inout_connection_matrix[, cols])
    rowMeans(inout_connection_matrix[, cols, drop = FALSE])
  })
  # Also include unknown neurons individually (one column each)
  unk_cols <- which(grepl("^unknown_", neuron_to_cct))
  if (length(unk_cols) > 0) {
    unk_mat <- as.matrix(inout_connection_matrix[, unk_cols, drop = FALSE])
    colnames(unk_mat) <- neuron_to_cct[unk_cols]
    cct_matrix <- cbind(cct_matrix, unk_mat)
  }
  message(sprintf("  Collapsed matrix: %d partners × %d cell types (vs %d neurons)",
                  nrow(cct_matrix), ncol(cct_matrix), ncol(inout_connection_matrix)))

  # Remove all-zero rows
  cct_nonzero <- rowSums(abs(cct_matrix)) > 0.0001
  cct_matrix <- cct_matrix[cct_nonzero, ]

  # Cosine similarity on collapsed matrix
  cct_sparse <- as(as.matrix(t(cct_matrix)), "dgCMatrix")
  cct_cos_sim <- cosine_similarity_sparse(t(cct_sparse))
  cct_cos_sim[is.infinite(cct_cos_sim)] <- 0
  dimnames(cct_cos_sim) <- list(colnames(cct_matrix), colnames(cct_matrix))

  # PCA on collapsed cosine similarity
  cct_pca <- prcomp(cct_cos_sim, center = TRUE, scale. = TRUE)
  cct_var <- cct_pca$sdev^2 / sum(cct_pca$sdev^2)
  cct_p <- length(cct_var)
  cct_bstick <- sapply(1:cct_p, function(j) sum(1 / (j:cct_p))) / cct_p
  cct_n_bstick <- max(which(cct_var > cct_bstick))
  message(sprintf("  Collapsed PCA: %d components, broken stick = %d", cct_p, cct_n_bstick))

  # UMAP (cosine) on collapsed matrix
  set.seed(42)
  cct_n <- ncol(cct_matrix)
  cct_umap <- uwot::umap(t(cct_matrix), metric = "cosine",
                         n_epochs = 500, n_neighbors = min(50, cct_n - 1),
                         min_dist = 0, n_trees = 100, spread = 10, n_components = 2)
  rownames(cct_umap) <- colnames(cct_matrix)

  # UMAP from PCA (collapsed)
  cct_umap_pca <- uwot::umap(cct_pca$x[, 1:cct_n_bstick, drop = FALSE],
                             metric = "euclidean",
                             n_epochs = 500, n_neighbors = min(50, cct_n - 1),
                             min_dist = 0, n_trees = 100, spread = 10, n_components = 2)
  rownames(cct_umap_pca) <- colnames(cct_matrix)

  # cutreeDynamic on collapsed PCA
  cct_pca_sub <- cct_pca$x[, 1:cct_n_bstick, drop = FALSE]
  cct_d <- dist(cct_pca_sub, method = "euclidean")
  cct_hc <- hclust(cct_d, method = "ward.D2")
  cct_clusters <- dynamicTreeCut::cutreeDynamic(cct_hc, distM = as.matrix(cct_d),
    deepSplit = 2, minClusterSize = 5)
  names(cct_clusters) <- colnames(cct_matrix)
  n_cct_clusters <- length(unique(cct_clusters[cct_clusters != 0]))
  message(sprintf("  Collapsed clustering: %d clusters", n_cct_clusters))

  # Build df for plotting (one row per cell type)
  cct_df <- data.frame(
    cell_sub_type = colnames(cct_matrix),
    UMAP1 = cct_umap[, 1], UMAP2 = cct_umap[, 2],
    PCA_UMAP1 = cct_umap_pca[, 1], PCA_UMAP2 = cct_umap_pca[, 2],
    cluster = as.character(cct_clusters),
    stringsAsFactors = FALSE
  )
  # Join super_cluster (most common per cell type)
  .mode_chr <- function(x) {
    x <- x[!is.na(x) & x != ""]
    if (length(x) == 0) return(NA_character_)
    tab <- table(x)
    names(tab)[which.max(tab)]
  }
  cct_meta <- banc.an.dn.meta %>%
    dplyr::filter(!is.na(cell_sub_type), cell_sub_type != "") %>%
    dplyr::group_by(cell_sub_type) %>%
    dplyr::summarise(
      super_cluster = .mode_chr(super_cluster),
      super_class   = .mode_chr(super_class),
      .groups = "drop"
    )
  cct_df <- cct_df %>% dplyr::left_join(cct_meta, by = "cell_sub_type")

  # Save collapsed cluster assignments
  write.csv(cct_df, file.path(cluster_options_path, "collapsed_cct_clusters.csv"),
            row.names = FALSE)

  # Plot: cosine-UMAP on collapsed data
  cct_clust_levels <- sort(unique(cct_df$cluster[cct_df$cluster != "0"]))
  cct_clust_cols <- cerise_limon_palette(max(length(cct_clust_levels), 1))
  names(cct_clust_cols) <- cct_clust_levels
  cct_centroids <- cct_df %>%
    dplyr::filter(cluster != "0") %>%
    dplyr::group_by(cluster) %>%
    dplyr::summarise(UMAP1 = mean(UMAP1, na.rm = TRUE),
                     UMAP2 = mean(UMAP2, na.rm = TRUE),
                     PCA_UMAP1 = mean(PCA_UMAP1, na.rm = TRUE),
                     PCA_UMAP2 = mean(PCA_UMAP2, na.rm = TRUE),
                     .groups = "drop")

  g_cct_umap <- ggplot(cct_df, aes(UMAP1, UMAP2)) +
    geom_point(data = subset(cct_df, cluster == "0"),
               color = "grey70", size = 1, alpha = 0.5) +
    geom_point(data = subset(cct_df, cluster != "0"),
               aes(color = cluster), size = 2, alpha = 0.8) +
    scale_color_manual(values = cct_clust_cols, guide = "none") +
    geom_text(data = cct_centroids, aes(label = cluster),
              size = 3, fontface = "bold") +
    theme_void() + coord_fixed() +
    labs(title = "Collapsed by cell_sub_type — cosine UMAP",
         subtitle = sprintf("%d cell types, %d clusters", nrow(cct_df), n_cct_clusters))
  ggsave(g_cct_umap, filename = file.path(cluster_options_path,
         "collapsed_cct_umap_a_cluster_colours.pdf"), width = 8, height = 8, dpi = 300)

  g_cct_pca_umap <- ggplot(cct_df, aes(PCA_UMAP1, PCA_UMAP2)) +
    geom_point(data = subset(cct_df, cluster == "0"),
               color = "grey70", size = 1, alpha = 0.5) +
    geom_point(data = subset(cct_df, cluster != "0"),
               aes(color = cluster), size = 2, alpha = 0.8) +
    scale_color_manual(values = cct_clust_cols, guide = "none") +
    geom_text(data = cct_centroids, aes(x = PCA_UMAP1, y = PCA_UMAP2, label = cluster),
              size = 3, fontface = "bold") +
    theme_void() + coord_fixed() +
    labs(title = "Collapsed by cell_sub_type — PCA-UMAP",
         subtitle = sprintf("%d cell types, %d clusters (broken stick: %d dims)",
                            nrow(cct_df), n_cct_clusters, cct_n_bstick))
  ggsave(g_cct_pca_umap, filename = file.path(cluster_options_path,
         "collapsed_cct_pca_umap_a_cluster_colours.pdf"), width = 8, height = 8, dpi = 300)

  # Plots coloured by super_cluster
  g_cct_umap_sc <- ggplot(cct_df, aes(UMAP1, UMAP2)) +
    geom_point(data = subset(cct_df, is.na(super_cluster)),
               color = "grey70", size = 1, alpha = 0.5) +
    geom_point(data = subset(cct_df, !is.na(super_cluster)),
               aes(color = super_cluster), size = 2, alpha = 0.8) +
    scale_color_manual(values = paper.cols) +
    theme_void() + coord_fixed() +
    labs(title = "Collapsed by cell_sub_type — cosine UMAP, super_cluster colours") +
    theme(legend.position = "bottom", legend.text = element_text(size = 6))
  ggsave(g_cct_umap_sc, filename = file.path(cluster_options_path,
         "collapsed_cct_umap_b_super_cluster_colours.pdf"), width = 10, height = 9, dpi = 300)

  g_cct_pca_umap_sc <- ggplot(cct_df, aes(PCA_UMAP1, PCA_UMAP2)) +
    geom_point(data = subset(cct_df, is.na(super_cluster)),
               color = "grey70", size = 1, alpha = 0.5) +
    geom_point(data = subset(cct_df, !is.na(super_cluster)),
               aes(color = super_cluster), size = 2, alpha = 0.8) +
    scale_color_manual(values = paper.cols) +
    theme_void() + coord_fixed() +
    labs(title = "Collapsed by cell_sub_type — PCA-UMAP, super_cluster colours") +
    theme(legend.position = "bottom", legend.text = element_text(size = 6))
  ggsave(g_cct_pca_umap_sc, filename = file.path(cluster_options_path,
         "collapsed_cct_pca_umap_b_super_cluster_colours.pdf"), width = 10, height = 9, dpi = 300)

  message(sprintf("  Saved collapsed-by-cell-type plots (%d cell types → %d clusters)",
                  nrow(cct_df), n_cct_clusters))

  # Save primary cluster assignments CSV (neuron-level, from main analysis)
  cluster_assignments <- data.frame(
    id = umap_ids,
    cluster = as.character(final_clusters),
    stringsAsFactors = FALSE
  )
  write.csv(cluster_assignments,
            file = file.path(cluster_options_path, "cluster_assignments.csv"),
            row.names = FALSE)
  message(sprintf("Saved %d cluster assignments (%d clusters)", nrow(cluster_assignments), n_final))

  # Save ALL 4 variant cluster assignments (for downstream tool comparisons)
  cluster_assignments_all <- data.frame(
    id = umap_ids,
    id_partners_brokenstick            = as.character(res_id_bs$clusters[umap_ids]),
    id_partners_marchenko_pastur       = as.character(res_id_mp$clusters[umap_ids]),
    celltype_partners_brokenstick      = as.character(res_ct_bs$clusters[umap_ids]),
    celltype_partners_marchenko_pastur = as.character(res_ct_mp$clusters[umap_ids]),
    stringsAsFactors = FALSE
  )
  write.csv(cluster_assignments_all,
            file = file.path(cluster_options_path, "cluster_assignments_all_methods.csv"),
            row.names = FALSE)
  message(sprintf("Saved %d cluster assignments across 4 methods", nrow(cluster_assignments_all)))

  # ============================================================
  # PCA-UMAP becomes the primary downstream representation.
  # Both variants were saved to cluster_options/ above for comparison.
  # From here on, UMAP1/UMAP2 = PCA-UMAP (used by all downstream plots,
  # banc-meta.R loaders, rebuild_interactive_tools.R, panel_an_dn_influence.R, etc.).
  # The cosine UMAP is preserved as cosine_UMAP1/cosine_UMAP2 for reference and CSV.
  # ============================================================
  message("Promoting PCA-UMAP to primary (UMAP1/UMAP2 = PCA-UMAP); cosine UMAP demoted to cosine_UMAP1/2")
  umap.dn.df$cosine_UMAP1 <- umap.dn.df$UMAP1
  umap.dn.df$cosine_UMAP2 <- umap.dn.df$UMAP2
  umap.dn.df$UMAP1 <- umap.dn.df$PCA_UMAP1
  umap.dn.df$UMAP2 <- umap.dn.df$PCA_UMAP2
  umap.dn.df$PCA_UMAP1 <- NULL
  umap.dn.df$PCA_UMAP2 <- NULL

  # Use cluster numbers as-is from cutreeDynamic (no renumbering).
  # Previous code reordered by UMAP centroid proximity, which made it
  # difficult to map back to the confusion-matrix heatmaps in cluster_options/.
  umap.dn.df$calculated_cluster <- umap.dn.df$clusterno
  umap.dn.df$cluster <- umap.dn.df$calculated_cluster
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

# Ensure cell_function_plot exists (modality-only, for icon placement)
# In recalculate=TRUE this is set above; rebuild here so it works in both paths
if (!"cell_function_plot" %in% colnames(umap.dn.df)) {
  modality_lookup <- cns.functions %>%
    dplyr::filter(!is.na(modality), modality != "") %>%
    dplyr::distinct(cell_type, modality)
  if (!"modality" %in% colnames(umap.dn.df)) {
    if (!"cell_type_base" %in% colnames(umap.dn.df)) {
      umap.dn.df$cell_type_base <- sub("_[a-z]$", "", umap.dn.df$cell_type)
    }
    umap.dn.df <- umap.dn.df %>%
      dplyr::left_join(modality_lookup, by = c("cell_type_base" = "cell_type"))
  }
  umap.dn.df$cell_function_plot <- dplyr::if_else(
    is.na(umap.dn.df$modality) | tolower(umap.dn.df$modality) == "unknown",
    NA_character_, umap.dn.df$modality)
}

# Calculate cluster centroids
cluster_centroids <- umap.dn.df %>%
  dplyr::filter(cluster!="0",
                !is.na(UMAP1)) %>%
  mutate(cluster = gsub("AN_|DN_|EFF_","",cluster)) %>%
  group_by(cluster) %>%
  summarise(UMAP1 = mean(UMAP1),
            UMAP2 = mean(UMAP2))

# Calculate concave hulls for each cluster (kept for centroid label positioning)
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

# --- Density-overlay data for cluster outlines ---
# For each neuron, attach the cluster (no prefix) and the cluster's modal super_cluster.
# The fill colour comes from paper.cols via the super_cluster.
.cluster_super <- umap.dn.df %>%
  dplyr::filter(cluster != "0", !is.na(cluster), !is.na(super_cluster), super_cluster != "") %>%
  dplyr::mutate(cluster = gsub("AN_|DN_|EFF_", "", cluster)) %>%
  dplyr::group_by(cluster) %>%
  dplyr::summarise(
    super_cluster = {
      tab <- sort(table(super_cluster), decreasing = TRUE)
      if (length(tab) == 0) NA_character_ else names(tab)[1]
    },
    .groups = "drop"
  )
density_df <- umap.dn.df %>%
  dplyr::filter(cluster != "0", !is.na(cluster), !is.na(UMAP1), !is.na(UMAP2)) %>%
  dplyr::mutate(cluster = gsub("AN_|DN_|EFF_", "", cluster)) %>%
  dplyr::select(-dplyr::any_of("super_cluster")) %>%
  dplyr::left_join(.cluster_super, by = "cluster")
rm(.cluster_super)

# Helper: 2D density outline layer to replace hull polygons.
#
# For each group in `df` (column chosen by `group_by`), computes its OWN 2D KDE
# (MASS::kde2d) and extracts contour rings at one or more density quantiles of
# the group's own neurons. Each connected ring at each level becomes a separate
# polygon with a unique `blob_id`, so a non-contiguous group cleanly produces
# multiple disconnected blobs and multiple nested rings per blob.
#
# This replaces the previous `stat_density_2d(bins = n_levels)` approach, which
# computed contour levels GLOBALLY across the whole dataset — sparse groups
# whose peak density fell below the lowest shared level rendered no polygon at
# all. Per-group levels eliminate that silent drop.
#
# Insert as the FIRST layer so points sit on top.
#
# Args:
#   df              data with UMAP1, UMAP2, and the column named in `group_by`
#   group_by        column to group densities + labels by ("super_cluster" or
#                   "cluster"). Default "super_cluster".
#   level_quantiles vector of density quantiles (0..1) at which to draw rings.
#                   Lower quantile = larger enclosed region. Pass several to get
#                   nested contour bands (default `c(0.25, 0.5, 0.75)` → 3 rings
#                   per blob, going from outer to inner). Pass a single value
#                   for one ring per blob.
#   h               bandwidth for kde2d (default: MASS::bandwidth.nrd per axis)
#   n_grid          kde2d grid resolution
#   min_points      skip groups with fewer than this many points
#   min_blob_pts    drop ring blobs with fewer vertices than this (noise)
#   colours         NULL (default) → assign each group its own greyscale shade
#                   via grDevices::gray.colors, with the assignment ordered so
#                   that SPATIALLY ADJACENT groups receive maximally different
#                   grey values (greedy contrast against k-nearest neighbours
#                   in UMAP-centroid space). Otherwise a NAMED character vector
#                   keyed by group value (e.g. paper.cols). Groups whose names
#                   are missing from the supplied vector fall back to "grey50".
#   line_width      outline linewidth
#   add_labels      if TRUE, append one ggrepel label per outermost blob with a
#                   leader line pointing into the blob — use this when groups
#                   are non-contiguous and centroid-based labels land in gaps.
#                   Labels are coloured to match the group's contour colour.
#   label_size      ggrepel label text size
#   label_colour    NULL (default) → label colour follows the group's contour
#                   colour. Otherwise a single colour string applied to every
#                   label and segment.
#   label_repel     ggrepel repulsion force between overlapping labels. Default
#                   2 (a bit higher than ggrepel's default of 1). Increase to
#                   spread crowded labels further apart; decrease to keep them
#                   closer to their anchors.
#   label_box_padding  Padding around each label box (passed to ggrepel
#                      box.padding). Default 0.6 — bumped from 0.4 to give a
#                      bit more breathing room.
#
# Returns a list of ggproto layers (one polygon layer per group, optionally one
# label layer per group).
density_outline_layer <- function(df = density_df,
                                  group_by = "super_cluster",
                                  level_quantiles = c(0.2, 0.5, 0.8),
                                  h = NULL,
                                  n_grid = 100,
                                  min_points = 5,
                                  min_blob_pts = 6,
                                  colours = NULL,
                                  line_width = 0.4,
                                  add_labels = FALSE,
                                  label_size = 4,
                                  label_colour = NULL,
                                  label_repel = 2,
                                  label_box_padding = 0.6) {
  if (!requireNamespace("MASS", quietly = TRUE))
    stop("density_outline_layer() requires the MASS package")
  if (!group_by %in% names(df))
    stop(sprintf("density_outline_layer(): df has no column '%s'", group_by))

  grp_vec <- df[[group_by]]
  groups <- sort(unique(stats::na.omit(grp_vec)))
  groups <- groups[nzchar(groups)]
  if (length(groups) == 0) return(list())

  # Build a colour map keyed by group value.
  if (is.null(colours)) {
    n_groups <- length(groups)
    grey_shades <- grDevices::gray.colors(n_groups, start = 0.15, end = 0.65)
    if (n_groups <= 2) {
      grp_colours <- setNames(grey_shades, groups)
    } else {
      # Compute UMAP centroids per group, then greedily assign grey indices
      # so that close centroids get maximally different greys. Each candidate
      # placement is scored by inverse-distance-weighted index difference
      # against EVERY already-placed group, so the closer two groups are, the
      # harder they pull each other apart on the grey ramp.
      centroids <- do.call(rbind, lapply(groups, function(g) {
        sub <- df[!is.na(grp_vec) & grp_vec == g, , drop = FALSE]
        data.frame(
          grp = g,
          UMAP1 = mean(sub$UMAP1, na.rm = TRUE),
          UMAP2 = mean(sub$UMAP2, na.rm = TRUE),
          stringsAsFactors = FALSE
        )
      }))
      keep <- is.finite(centroids$UMAP1) & is.finite(centroids$UMAP2)
      if (sum(keep) < 3) {
        # Fall back to alphabetical assignment if too few centroids resolved.
        grp_colours <- setNames(grey_shades, groups)
      } else {
        kept_centroids <- centroids[keep, , drop = FALSE]
        d <- as.matrix(stats::dist(kept_centroids[, c("UMAP1", "UMAP2")]))
        rownames(d) <- colnames(d) <- kept_centroids$grp
        n_keep <- nrow(d)
        # Avoid division by zero for coincident centroids — clamp small d.
        positive_d <- d[d > 0]
        d_floor <- if (length(positive_d) > 0) {
          max(min(positive_d) * 1e-3, .Machine$double.eps)
        } else .Machine$double.eps
        d_safe <- pmax(d, d_floor)
        diag(d_safe) <- Inf  # ignore self in scoring

        # Process most-constrained groups first: those whose total inverse
        # distance to all other groups is largest (i.e. sit in the densest
        # neighbourhood) get to pick early so they can secure extreme greys.
        inv_pull <- rowSums(1 / d_safe)
        process_order <- order(inv_pull, decreasing = TRUE)

        assign_idx <- rep(NA_integer_, n_keep)
        for (gi in process_order) {
          taken_mask <- !is.na(assign_idx)
          taken_indices <- assign_idx[taken_mask]
          available <- setdiff(seq_len(n_groups), taken_indices)
          if (!any(taken_mask)) {
            # First pick: start near the middle of the grey ramp so the next
            # (closest) neighbour has the full range available to push against.
            assign_idx[gi] <- available[ceiling(length(available) / 2)]
            next
          }
          others <- which(taken_mask)
          weights <- 1 / d_safe[gi, others]
          # Score each available grey index by inverse-distance-weighted
          # absolute index difference against every already-placed group.
          scores <- vapply(available, function(cand) {
            sum(abs(cand - assign_idx[others]) * weights)
          }, numeric(1))
          assign_idx[gi] <- available[which.max(scores)]
        }
        grp_colours <- setNames(grey_shades[assign_idx], rownames(d))
        # Any groups dropped by the `keep` filter get a neutral grey.
        missing_grps <- setdiff(groups, names(grp_colours))
        if (length(missing_grps) > 0) {
          grp_colours <- c(grp_colours,
                           setNames(rep("grey50", length(missing_grps)),
                                    missing_grps))
        }
      }
    }
  } else {
    grp_colours <- setNames(rep("grey50", length(groups)), groups)
    common <- intersect(names(colours), groups)
    if (length(common) > 0) grp_colours[common] <- colours[common]
  }

  # Per-group, per-quantile ring extraction.
  ring_list <- list()
  for (grp in groups) {
    sub <- df[!is.na(grp_vec) & grp_vec == grp, , drop = FALSE]
    if (nrow(sub) < min_points) next
    h_use <- if (is.null(h)) {
      c(MASS::bandwidth.nrd(sub$UMAP1), MASS::bandwidth.nrd(sub$UMAP2))
    } else h
    if (any(!is.finite(h_use)) || any(h_use <= 0)) next
    kde <- tryCatch(
      MASS::kde2d(sub$UMAP1, sub$UMAP2, h = h_use, n = n_grid),
      error = function(e) NULL
    )
    if (is.null(kde)) next

    # Density at each group point (nearest grid cell), used to translate
    # quantiles into absolute contour levels for THIS group.
    ix <- pmin(pmax(findInterval(sub$UMAP1, kde$x), 1L), length(kde$x))
    iy <- pmin(pmax(findInterval(sub$UMAP2, kde$y), 1L), length(kde$y))
    point_dens <- kde$z[cbind(ix, iy)]

    for (q in level_quantiles) {
      lvl <- stats::quantile(point_dens, q, na.rm = TRUE)
      if (!is.finite(lvl) || lvl <= 0) next
      rings <- grDevices::contourLines(kde$x, kde$y, kde$z, levels = lvl)
      if (length(rings) == 0) next
      for (i in seq_along(rings)) {
        ring <- rings[[i]]
        if (length(ring$x) < min_blob_pts) next
        ring_list[[length(ring_list) + 1L]] <- data.frame(
          UMAP1 = ring$x,
          UMAP2 = ring$y,
          grp_value = grp,
          quant = q,
          blob_id = paste(grp, q, i, sep = "__"),
          stringsAsFactors = FALSE
        )
      }
    }
  }
  if (length(ring_list) == 0) return(list())
  rings_df <- do.call(rbind, ring_list)

  # Build one geom_polygon per group so each layer carries a constant colour
  # (no need for scale_colour_identity, which would clash with the parent
  # plot's existing colour scales).
  layers <- list()
  for (grp in groups) {
    grp_rings <- rings_df[rings_df$grp_value == grp, , drop = FALSE]
    if (nrow(grp_rings) == 0) next
    layers[[length(layers) + 1L]] <- ggplot2::geom_polygon(
      data = grp_rings,
      mapping = ggplot2::aes(x = UMAP1, y = UMAP2, group = blob_id),
      colour = grp_colours[[grp]],
      fill = NA,
      linewidth = line_width,
      inherit.aes = FALSE
    )
  }

  if (add_labels) {
    if (!requireNamespace("ggrepel", quietly = TRUE))
      stop("density_outline_layer(add_labels=TRUE) requires the ggrepel package")
    # One label per outermost (lowest-quantile) blob. We build a SINGLE
    # ggrepel layer with all labels so cross-group repulsion works — ggrepel
    # only pushes labels apart within the same layer.
    min_q <- min(level_quantiles)
    label_rings <- rings_df[rings_df$quant == min_q, , drop = FALSE]
    if (nrow(label_rings) > 0) {
      label_df <- do.call(rbind, lapply(
        split(label_rings, label_rings$blob_id),
        function(d) data.frame(
          UMAP1 = mean(range(d$UMAP1)),
          UMAP2 = mean(range(d$UMAP2)),
          grp_value = d$grp_value[1],
          blob_id = d$blob_id[1],
          stringsAsFactors = FALSE
        )
      ))
      # Per-row colour: follow the group's contour colour by default, or use
      # the user-supplied single colour. Pass as a vector outside aes() so we
      # don't have to add scale_colour_identity() (which would clash with the
      # parent plot's existing scale_color_manual).
      lab_cols <- if (is.null(label_colour)) {
        unname(grp_colours[label_df$grp_value])
      } else {
        rep(label_colour, nrow(label_df))
      }
      layers[[length(layers) + 1L]] <- ggrepel::geom_label_repel(
        data = label_df,
        mapping = ggplot2::aes(x = UMAP1, y = UMAP2, label = grp_value),
        size = label_size,
        colour = lab_cols,
        fontface = "bold",
        fill = "white",
        label.size = 0,
        label.padding = grid::unit(0.1, "lines"),
        box.padding = label_box_padding,
        point.padding = 0.2,
        force = label_repel,
        force_pull = 1,
        min.segment.length = 0,
        max.overlaps = Inf,
        segment.colour = lab_cols,
        segment.size = 0.3,
        inherit.aes = FALSE
      )
    }
  }

  layers
}

#################
### PLOT UMAP ###
#################

# All points use the same circle size (match the main fig 3 super-cluster UMAP, or
# very slightly smaller). Density outlines removed 2026-04-08 per user request.
g.dn.clusters <- ggplot() +
  geom_point(data = umap.dn.df,
             aes(x = UMAP1, y = UMAP2, color = super_class),
             shape = 19,
             size = 1.75,
             alpha = 0.75) +
  scale_color_manual(values = paper.cols) +
  scale_fill_manual(values = paper.cols, guide = "none") +
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
       filename = file.path(banc.fig3.supp.path,"neck_inout_connectivity_cosine_umap_hull.pdf"),
       width = 8, height = 8, dpi = 300)
ggsave(plot = convert_to_dark_mode(g.dn.clusters),
       filename = file.path(banc.fig3.darkmode.path,"dark_mode_neck_inout_connectivity_cosine_umap_hull.pdf"),
       width = 8, height = 8, dpi = 300)

#################################################
## INTERACTIVE SUPER_CLUSTER REASSIGNMENT TOOL ##
#################################################
#
# Decommissioned: this tool is now built by R/annotations/rebuild_interactive_tools.R.
# That standalone script (a) is much faster (no influence recompute), (b) has the
# correct multi-trace + customdata + direct-event bridge for working lasso → table
# filtering, and (c) pulls fresh SeaTable so super_cluster edits are always current.
# Run it from RStudio after this script finishes:
#   source("R/annotations/rebuild_interactive_tools.R")

################################
### SUPER CLUSTER MEMBERSHIP ###
################################

# Plot UMAP with density outlines
g.super.clusters <- ggplot() +
  #density_outline_layer() +
  geom_point(data = umap.dn.df %>%
               dplyr::filter(!is.na(super_cluster)),
             aes(x = UMAP1, y = UMAP2, color = super_cluster),
             fill = "white",
             size = 2,
             shape = 19,
             alpha = 0.9) +
  scale_color_manual(values = paper.cols) +
  scale_fill_manual(values = paper.cols, guide = "none") +
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
g.super.clusters.no.labels <- ggplot() +
  # density_outline_layer() removed 2026-04-08 per user request
  geom_point(data = umap.dn.df %>%
               dplyr::filter(!is.na(super_cluster)),
             aes(x = UMAP1, y = UMAP2, color = super_cluster),
             fill = "white",
             size = 2,
             shape = 19,
             alpha = 1) +
  scale_color_manual(values = paper.cols) +
  scale_fill_manual(values = paper.cols, guide = "none") +
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
       filename = file.path(banc.fig3.supp.path,"neck_inout_connectivity_cosine_umap_super_clusters.pdf"),
       width = 8, height = 8, dpi = 300)
ggsave(plot = convert_to_dark_mode(g.dn.clusters),
       filename = file.path(banc.fig3.darkmode.path,"dark_mode_neck_inout_connectivity_cosine_umap_super_clusters.pdf"),
       width = 8, height = 8, dpi = 300)
ggsave(plot = g.super.clusters.no.labels,
       filename = file.path(banc.fig3.path,"neck_inout_connectivity_cosine_umap_super_clusters_no_cluster_label.pdf"),
       width = 8, height = 8, dpi = 300)
ggsave(plot = convert_to_dark_mode(g.super.clusters.no.labels),
       filename = file.path(banc.fig3.darkmode.path,"dark_mode_neck_inout_connectivity_cosine_umap_super_clusters_no_cluster_label.pdf"),
       width = 8, height = 8, dpi = 300)

#####################
### CELL FUNCTION ###
#####################

# Update the plot
# Use cell_function_plot (modality only) — the same column mapped into aes(shape=...)
# below. Previous version keyed `shapes` by the cascade `cell_function` values, which
# rarely matched cell_function_plot values, so scale_shape_manual silently dropped
# the shapes. Also prefer FILLED shapes (21-25) so the fill aesthetic actually paints,
# falling back to solid shapes (15-20) only if there are more than 5 modalities.
cfs <- sort(na.omit(unique(umap.dn.df$cell_function_plot)))
n_cfs <- length(cfs)
if (n_cfs == 0) {
  shapes <- integer(0)
} else if (n_cfs <= 5) {
  # Filled shapes with separate fill/colour: 21=circle, 22=square, 23=diamond,
  # 24=triangle up, 25=triangle down
  shapes <- 21:(21 + n_cfs - 1)
} else if (n_cfs <= 11) {
  # 5 filled + up to 6 solid (15-20). Solid shapes use `colour` for both
  # outline and fill, so they remain visible even though `fill` is ignored.
  shapes <- c(21:25, 15:(15 + n_cfs - 6))
} else if (n_cfs <= 25) {
  # Mix of filled/solid/outline; outline-only shapes still display via `colour`
  shapes <- c(21:25, 15:20, sample(setdiff(0:14, c(1, 10)), n_cfs - 11))
} else {
  shapes <- sample(c(21:25, 15:20, 0:14), n_cfs, replace = TRUE)
}
names(shapes) <- cfs

g.dn.clusters.hulls <- ggplot(umap.dn.df,
                              aes(x = UMAP1,
                                  y = UMAP2)) +
  density_outline_layer() +
  scale_fill_manual(values = paper.cols, guide = "none") +
  ggnewscale::new_scale_fill() +
  geom_point(data = subset(umap.dn.df, is.na(cell_function_plot)),
             color = 'darkgrey',
             alpha = 0.3,
             size = 0.3) +
  geom_point(data = subset(umap.dn.df, !is.na(cell_function_plot)),
             aes(color = cell_function_plot, fill = cell_function_plot, shape = cell_function_plot),
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
       filename = file.path(banc.fig3.darkmode.path,"dark_mode_neck_inout_connectivity_umap_hulls_shapes.pdf"),
       width = 8, height = 8, dpi = 300)

# Now with labels (unknown/NA neurons as small grey dots, no labels)
# density_outline_layer() removed 2026-04-08 per user request
g.dn.clusters.hulls <- ggplot(umap.dn.df,
                              aes(x = UMAP1,
                                  y = UMAP2)) +
  scale_fill_manual(values = paper.cols, guide = "none") +
  ggnewscale::new_scale_fill() +
  geom_point(data = subset(umap.dn.df, is.na(cell_function_plot)),
             color = 'darkgrey',
             alpha = 0.3,
             size = 0.3) +
  geom_point(data = subset(umap.dn.df, !is.na(cell_function_plot)),
             aes(color = cell_function_plot, fill = cell_function_plot),
             stroke = 1,
             alpha = 0.99,
             size = 1.5) +
  ggrepel::geom_label_repel(
    data = umap.dn.df %>%
      dplyr::filter(!is.na(label), !is.na(cell_function_plot)) %>%
      dplyr::distinct(cell_type, label, .keep_all = TRUE),
    aes(label=label, color = cell_function_plot, fill = cell_function_plot),
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
       filename = file.path(banc.fig3.darkmode.path,"dark_mode_neck_inout_connectivity_umap_hulls_labels.pdf"),
       width = 8, height = 8, dpi = 300)

#############
### ICONS ###
#############

# Add icons path (use cell_function_plot to exclude "unknown")
icon_folder <- "figures/schematics/assets/umap_icons"
umap.dn.df$image <- ifelse(
  !is.na(umap.dn.df$cell_function_plot),
  file.path(icon_folder, paste0(umap.dn.df$cell_function_plot, ".svg")),
  NA_character_
)
umap.dn.df$image[!file.exists(umap.dn.df$image) & !is.na(umap.dn.df$image)] <- NA

# Plot
g.dn.clusters.icons <- ggplot(umap.dn.df,
                              aes(x = UMAP1, y = UMAP2)) +
  # density_outline_layer() +
  scale_fill_manual(values = paper.cols, guide = "none") +
  # Plot gray points for unknown/NA — slightly larger to make the
  # underlying UMAP look more solid (issue feedback 2026-05-03)
  geom_point(
    data = subset(umap.dn.df, is.na(cell_function_plot)),
    color = 'darkgrey',
    alpha = 0.4,
    size = 0.5
  ) +
  # Plot icon for each known cell function — slightly smaller + more opaque
  # (was size=0.03, alpha=0.9; bumped opacity to 1, dropped size to 0.025)
  ggimage::geom_image(
    data = subset(umap.dn.df, !is.na(cell_function_plot)),
    aes(image = image),
    size = 0.025,
    asp = 1,
    alpha = 1
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
       filename = file.path(banc.fig3.supp.path,"neck_inout_connectivity_umap_hulls_icons.pdf"),
       width = 9, height = 8, dpi = 300)
ggsave(plot = convert_to_dark_mode(g.dn.clusters.icons),
       filename = file.path(banc.fig3.darkmode.path,"dark_mode_neck_inout_connectivity_umap_hulls_icons.pdf"),
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
# density_outline_layer() removed 2026-04-08 per user request
g.dn.clusters.braun <- ggplot(umap.braun.dn.df,
                              aes(x = UMAP1, y = UMAP2)) +
  scale_fill_manual(values = paper.cols, guide = "none") +
  ggnewscale::new_scale_fill() +
  geom_point(
    data = subset(umap.braun.dn.df, is.na(behavior)|grepl("unknown",behavior)),
    color = 'darkgrey',
    alpha = 0.5,
    size = 0.5
  ) +
  geom_point(
    data = subset(umap.braun.dn.df, !is.na(behavior)&!grepl("unknown",behavior)),
    mapping = aes(fill = behavior, color = behavior),
    alpha = 1,
    size = 2.5
  ) +
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
  scale_color_manual(values = paper.cols) +
  scale_fill_manual(values = paper.cols)

# Show
print(g.dn.clusters.braun)

# Save
ggsave(plot = g.dn.clusters.braun,
       filename = file.path(banc.fig3.supp.path,"neck_inout_connectivity_umap_hulls_braun.pdf"),
       width = 9, height = 8, dpi = 300)

############################
### HIGH LEVEL INFLUENCE ###
############################

# NOTE (2026-04-09): the per-super_class UMAP overlays that used to live here
# (motor/visceral_circulatory/visual_centrifugal) have been moved to
# panel_an_dn_influence.R, which already runs the seed_12 influence query for
# its own per-cell_sub_class overlays. Keeping the call out of this script
# avoids running query_influence() twice (it OOM'd here on a 64GB machine when
# computing 17M+ rows on top of all the cached UMAP/PCA state).

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
            file = "data/banc_annotations/v888/banc_neck_functional_classes.csv")
  table(umap.dn.df$cluster)
}

###########################################
### EVALUATE NEW CLUSTERS vs OLD GROUPS ###
###########################################

# Only meaningful after recalculation — st_cluster only exists in recalculate path
if (recalculate) {

# Compare newly computed UMAP clusters against SeaTable annotations:
# super_cluster, st_cluster (SeaTable "cluster"), manual_cluster
eval.df <- umap.dn.df %>%
  dplyr::filter(!is.na(cluster), cluster != "0") %>%
  dplyr::mutate(
    super_cluster = dplyr::if_else(
      is.na(super_cluster) | super_cluster %in% c("", "0", "NA"),
      "unassigned", super_cluster),
    st_cluster = dplyr::if_else(
      is.na(st_cluster) | st_cluster %in% c("", "0", "NA"),
      "unassigned", st_cluster),
    manual_cluster = dplyr::if_else(
      is.na(manual_cluster) | manual_cluster %in% c("", "0", "NA"),
      "unassigned", manual_cluster)
  )

# Adjusted Rand Index for each comparison
comparisons <- list(
  super_cluster = eval.df$super_cluster,
  st_cluster = eval.df$st_cluster,
  manual_cluster = eval.df$manual_cluster
)
ari_results <- sapply(comparisons, function(old) {
  # Only compare non-unassigned neurons
  keep <- old != "unassigned"
  if (sum(keep) < 10) return(NA_real_)
  mclust::adjustedRandIndex(eval.df$cluster[keep], old[keep])
})
cat("\n=== Adjusted Rand Index (new UMAP cluster vs SeaTable groups) ===\n")
print(round(ari_results, 3))

# Contingency heatmaps
for (ref_col in c("super_cluster", "st_cluster", "manual_cluster")) {
  ref_vals <- eval.df[[ref_col]]
  keep <- ref_vals != "unassigned"
  if (sum(keep) < 10) {
    message(sprintf("Skipping %s contingency — too few assigned neurons (%d)", ref_col, sum(keep)))
    next
  }

  ct <- table(new_cluster = eval.df$cluster[keep], reference = ref_vals[keep])
  # Normalise by column (what fraction of each reference group lands in each new cluster)
  ct_norm <- sweep(ct, 2, colSums(ct), "/")
  ct_norm[is.nan(ct_norm)] <- 0

  pheatmap::pheatmap(
    ct_norm,
    cluster_rows = TRUE,
    cluster_cols = TRUE,
    clustering_method = "ward.D2",
    color = grDevices::colorRampPalette(c("white", "#1f4e79", "#b22222"))(100),
    main = sprintf("New UMAP cluster vs %s (column-normalised)", ref_col),
    fontsize_row = 7,
    fontsize_col = 7,
    cellwidth = 10,
    cellheight = 10,
    filename = file.path(banc.fig3.supp.path,
                         sprintf("cluster_concordance_new_vs_%s.pdf", ref_col))
  )
}

# Write ARI summary
ari_txt <- c(
  "=== Cluster concordance: new UMAP clusters vs SeaTable annotations ===",
  sprintf("N neurons evaluated: %d", nrow(eval.df)),
  "",
  "Adjusted Rand Index (1 = perfect agreement, 0 = random):",
  paste0("  new vs super_cluster:  ", round(ari_results["super_cluster"], 3)),
  paste0("  new vs st_cluster:     ", round(ari_results["st_cluster"], 3)),
  paste0("  new vs manual_cluster: ", round(ari_results["manual_cluster"], 3)),
  "",
  "Contingency heatmaps saved to figure_3/links/supplement/"
)
writeLines(ari_txt, file.path(banc.fig3.supp.path, "cluster_concordance_summary.txt"))

} # end if(recalculate) for evaluation block

