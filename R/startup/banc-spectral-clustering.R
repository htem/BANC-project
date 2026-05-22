###########################################################
### Spectral Clustering of BANC CNS Network
###
### R port of python/spectral_clustering/banc_spectral_clustering_final.ipynb
###
### Performs spectral clustering on the neuron-level connectivity
### graph for central brain, VNC, neck connective, and visual neurons
### (excluding sensory/motor/visceral).
###
### Algorithm:
### 1. Filter edgelist to CNS interneurons (by clustering_set)
### 2. Build sparse adjacency matrix, column-normalize, symmetrize
### 3. Compute normalized graph Laplacian
### 4. Eigendecomposition (bottom-k eigenvectors)
### 5. Row-normalize eigenvector embedding
### 6. K-means clustering
### 7. UMAP of eigenvector embedding
### 8. Save results
###
### Parameters (matching Python notebook, updated 2026-04-18):
###   min_connection_strength = 2   (was 1 pre-2026-04-18)
###   cluster_count           = 14  (was 13 pre-2026-04-18)
###   cluster_seed            = 10
###   embedding_seed          = 3
###   flavour                 = "v2" (consumer default flipped from v3 to v2
###                                   on 2026-04-21; appended as filename suffix)
###   UMAP: n_neighbors=100, metric=cosine, min_dist=0, n_components=2
###
### Input:
###   - banc.meta (from banc-meta.R / GCS)
###   - banc.edgelist.simple (from banc-edgelist.R / GCS)
###
### Output:
###   - data/cns_network/spectral_clustering_min_connection_strength_2_banc_version_<NNN>_cluster_count_14_cluster_seed_10_embedding_seed_3_v2.csv
###########################################################

spectral_cluster_banc <- function(
  meta = banc.meta,
  edgelist = banc.edgelist.simple,
  min_connection_strength = 2,
  cluster_count = 14,
  cluster_seed = 10,
  embedding_seed = 3,
  umap_n_neighbors = 100,
  dataset_version = 888,
  flavour = "v2",
  output_dir = "data/cns_network"
) {

  message("=== BANC Spectral Clustering ===")

  # ---------------------------------------------------------------
  # 1. Define clustering_set (replicating Python notebook logic)
  # ---------------------------------------------------------------
  # Regions eligible for clustering
  clustering_regions <- c("central_brain", "neck_connective", "cervical_connective", "ventral_nerve_cord")
  # Super classes to EXCLUDE from region-based assignment
  excluded_sc <- c("sensory", "motor", "efferent", "afferent", "visceral")

  meta <- meta %>%
    dplyr::mutate(
      clustering_set = dplyr::case_when(
        # Visual projection / centrifugal always go to "visual"
        super_class %in% c("visual_centrifugal", "visual_projection") ~ "visual",
        # AN/DN neurons (ascending/descending) — include regardless of region
        grepl("ascending|descending", super_class) &
          !grepl(paste(excluded_sc, collapse = "|"), super_class) ~ "neck_connective",
        # Region-based, but exclude sensory/motor/etc.
        region %in% clustering_regions &
          !grepl(paste(excluded_sc, collapse = "|"), super_class) ~ region,
        TRUE ~ NA_character_
      )
    )

  clustering_values <- c("central_brain", "neck_connective", "cervical_connective", "ventral_nerve_cord", "visual")
  cluster_ids <- meta %>%
    dplyr::filter(clustering_set %in% clustering_values) %>%
    dplyr::pull(id) %>%
    unique()

  message(sprintf("Neurons eligible for clustering: %d", length(cluster_ids)))

  # ---------------------------------------------------------------
  # 2. Filter edgelist
  # ---------------------------------------------------------------
  # Ensure pre/post are character for matching
  el <- edgelist %>%
    dplyr::filter(
      count >= min_connection_strength,
      pre != post,
      pre %in% cluster_ids,
      post %in% cluster_ids
    )

  message(sprintf("Edges after filtering (count >= %d, within clustering set): %d",
                  min_connection_strength, nrow(el)))

  # ---------------------------------------------------------------
  # 3. Iteratively prune to strongly connected component
  #    (both pre and post must appear, matching Python's prune loop)
  # ---------------------------------------------------------------
  prev_n <- -1
  while (TRUE) {
    both <- intersect(unique(el$pre), unique(el$post))
    if (length(both) == prev_n) break
    prev_n <- length(both)
    el <- el %>% dplyr::filter(pre %in% both, post %in% both)
    message(sprintf("  Pruning: %d neurons in connected component", prev_n))
  }

  neuron_ids <- sort(both)
  n <- length(neuron_ids)
  message(sprintf("Final neuron count: %d, edge count: %d", n, nrow(el)))

  # ---------------------------------------------------------------
  # 4. Build sparse adjacency matrix
  # ---------------------------------------------------------------
  id_map <- setNames(seq_along(neuron_ids), neuron_ids)
  i_idx <- id_map[el$pre]
  j_idx <- id_map[el$post]

  # Aggregate weights (sum of count per pre-post pair)
  el_agg <- el %>%
    dplyr::group_by(pre, post) %>%
    dplyr::summarise(weight = sum(count), .groups = "drop")

  i_idx <- id_map[el_agg$pre]
  j_idx <- id_map[el_agg$post]

  adj <- Matrix::sparseMatrix(
    i = i_idx, j = j_idx, x = el_agg$weight,
    dims = c(n, n),
    dimnames = list(neuron_ids, neuron_ids)
  )

  # Column-normalize (A / colSums)
  col_sums <- Matrix::colSums(adj)
  col_sums[col_sums == 0] <- 1  # avoid division by zero
  adj_norm <- adj %*% Matrix::Diagonal(n, 1 / col_sums)

  # Symmetrize: 0.5 * (A_norm + A_norm^T)
  adj_sym <- 0.5 * (adj_norm + Matrix::t(adj_norm))

  message("Adjacency matrix built and symmetrized")

  # ---------------------------------------------------------------
  # 5. Spectral clustering
  #    - Normalized Laplacian
  #    - Bottom-k eigenvectors (smallest eigenvalues)
  #    - Row-normalize embedding
  #    - K-means
  # ---------------------------------------------------------------
  message(sprintf("Computing spectral clustering (k=%d)...", cluster_count))

  # Normalized Laplacian: L = I - D^{-1/2} A D^{-1/2}
  # Using scipy.sparse.csgraph.laplacian(normed=True) equivalent
  deg <- Matrix::rowSums(adj_sym)
  deg[deg == 0] <- 1
  deg_inv_sqrt <- 1 / sqrt(deg)
  D_inv_sqrt <- Matrix::Diagonal(n, deg_inv_sqrt)
  laplacian <- Matrix::Diagonal(n) - D_inv_sqrt %*% adj_sym %*% D_inv_sqrt

  # Bottom-k eigenvectors (smallest eigenvalues)
  # eigsh with which='SM' → smallest magnitude eigenvalues
  eig <- RSpectra::eigs_sym(laplacian, k = cluster_count, which = "SM")
  eigvec <- eig$vectors

  # Row-normalize the eigenvector matrix
  row_norms <- sqrt(rowSums(eigvec^2))
  row_norms[row_norms == 0] <- 1
  embedding <- eigvec / row_norms

  # K-means clustering
  set.seed(cluster_seed)
  km <- kmeans(embedding, centers = cluster_count, nstart = 25, iter.max = 100)
  unordered_clusters <- km$cluster  # 1-based

  message(sprintf("Spectral clustering done. Cluster sizes:"))
  for (cl in sort(unique(unordered_clusters))) {
    message(sprintf("  Cluster %d: %d neurons", cl, sum(unordered_clusters == cl)))
  }

  # ---------------------------------------------------------------
  # 6. UMAP on eigenvector embedding
  # ---------------------------------------------------------------
  message("Computing UMAP embedding...")

  umap_result <- uwot::umap(
    embedding,
    n_neighbors = umap_n_neighbors,
    metric = "cosine",
    min_dist = 0,
    n_components = 2,
    seed = embedding_seed,
    n_threads = 1
  )

  # ---------------------------------------------------------------
  # 7. Map unordered clusters to named clusters
  #
  # Primary: majority-vote from v626 results (match neurons by root_id)
  # Fallback: heuristic scoring by super_class/region/side composition
  # ---------------------------------------------------------------
  cluster_names_canonical <- c(
    "dorsal VNC", "leg VNC", "abdominal VNC",
    "posterior brain", "lateral brain", "flange/GNG/median bundle",
    "inferior brain", "superior brain",
    "left visual", "right visual",
    "left olfactory", "right olfactory",
    "central complex related"
  )

  # Build a result data frame with unordered clusters
  result <- data.frame(
    root_id = neuron_ids,
    unordered_cluster = unordered_clusters,
    umap_x = umap_result[, 1],
    umap_y = umap_result[, 2],
    stringsAsFactors = FALSE
  )

  # Add metadata for cluster assignment
  result <- result %>%
    dplyr::left_join(
      meta %>% dplyr::select(id, super_class, region, side) %>% dplyr::distinct(id, .keep_all = TRUE),
      by = c("root_id" = "id")
    )

  # Try majority-vote mapping from a previous version's results.
  # Lookup order: flavour-suffixed v888/v850 (new convention, strength=2,
  # count=14) → legacy unsuffixed v850/v746/v626 (old convention, strength=1,
  # count=13). The legacy fallbacks are kept so a fresh-clone consumer with
  # only old reference files can still bootstrap a name mapping.
  flavour_suffix <- if (nzchar(flavour)) paste0("_", flavour) else ""
  ref_candidates <- c(
    file.path(output_dir, sprintf(
      "spectral_clustering_min_connection_strength_2_banc_version_888_cluster_count_14_cluster_seed_10_embedding_seed_3%s.csv",
      flavour_suffix)),
    file.path(output_dir, sprintf(
      "spectral_clustering_min_connection_strength_2_banc_version_850_cluster_count_14_cluster_seed_10_embedding_seed_3%s.csv",
      flavour_suffix)),
    file.path(output_dir,
      "spectral_clustering_min_connection_strength_1_banc_version_850_cluster_count_13_cluster_seed_10_embedding_seed_3.csv"),
    file.path(output_dir,
      "spectral_clustering_min_connection_strength_1_banc_version_746_cluster_count_13_cluster_seed_10_embedding_seed_3.csv"),
    file.path(output_dir,
      "spectral_clustering_min_connection_strength_1_banc_version_626_cluster_count_13_cluster_seed_10_embedding_seed_3.csv")
  )
  ref_file <- ref_candidates[file.exists(ref_candidates)][1]
  if (is.na(ref_file)) ref_file <- ref_candidates[1]  # keep prior NA-vs-missing behaviour

  if (file.exists(ref_file)) {
    message(sprintf("Mapping clusters via majority-vote from reference: %s", basename(ref_file)))
    ordered_clusters <- map_clusters_from_reference(result, ref_file, cluster_names_canonical)
  } else {
    message("No reference clustering found. Mapping clusters by heuristic scoring...")
    ordered_clusters <- assign_cluster_names(result, cluster_names_canonical)
  }

  result$spectral_cluster <- ordered_clusters$spectral_cluster[match(result$unordered_cluster,
                                                                      ordered_clusters$unordered_cluster)]
  result$unofficial_cluster_name <- ordered_clusters$unofficial_cluster_name[match(result$unordered_cluster,
                                                                                    ordered_clusters$unordered_cluster)]

  message("\nCluster mapping (unordered -> named):")
  for (i in seq_len(nrow(ordered_clusters))) {
    message(sprintf("  %d -> %d (%s, n=%d, method=%s)",
                    ordered_clusters$unordered_cluster[i],
                    ordered_clusters$spectral_cluster[i],
                    ordered_clusters$unofficial_cluster_name[i],
                    ordered_clusters$n[i],
                    ordered_clusters$method[i]))
  }

  # ---------------------------------------------------------------
  # 8. Save output
  # ---------------------------------------------------------------
  output_file <- file.path(
    output_dir,
    sprintf("spectral_clustering_min_connection_strength_%d_banc_version_%d_cluster_count_%d_cluster_seed_%d_embedding_seed_%d%s.csv",
            min_connection_strength, dataset_version, cluster_count, cluster_seed, embedding_seed,
            if (nzchar(flavour)) paste0("_", flavour) else "")
  )

  output_df <- result %>%
    dplyr::select(root_id, spectral_cluster, umap_x, umap_y, unofficial_cluster_name)

  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  readr::write_csv(output_df, output_file)
  message(sprintf("\nSaved: %s (%d neurons, %d clusters)",
                  output_file, nrow(output_df), cluster_count))

  invisible(output_df)
}


#' Map unordered clusters to named clusters using v626 reference
#'
#' For each new unordered cluster, finds the most common v626 cluster name
#' among neurons shared between v626 and v746 results (majority-vote).
#' Falls back to heuristic scoring for clusters that can't be matched.
#'
#' @param result data.frame with root_id, unordered_cluster, super_class, region, side
#' @param v626_file path to v626 spectral clustering CSV
#' @param cluster_names canonical cluster names
#' @param threshold minimum fraction for majority-vote match (default 0.4)
#' @return data.frame mapping unordered_cluster -> spectral_cluster + unofficial_cluster_name
map_clusters_from_reference <- function(result, v626_file, cluster_names, threshold = 0.4) {

  v626 <- readr::read_csv(v626_file, col_types = readr::cols(.default = "c")) %>%
    dplyr::mutate(root_id = as.character(root_id))

  # Match neurons by root_id
  matched <- result %>%
    dplyr::inner_join(
      v626 %>% dplyr::select(root_id, old_name = unofficial_cluster_name),
      by = "root_id"
    )

  message(sprintf("  Matched %d/%d neurons to v626 assignments", nrow(matched), nrow(result)))

  n_clusters <- length(cluster_names)
  clusters <- sort(unique(result$unordered_cluster))

  # For each new cluster, find majority old name
  votes <- matched %>%
    dplyr::count(unordered_cluster, old_name) %>%
    dplyr::group_by(unordered_cluster) %>%
    dplyr::mutate(total = sum(n), fraction = n / total) %>%
    dplyr::arrange(dplyr::desc(fraction)) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::ungroup()

  # Build mapping with greedy assignment (highest fraction first, no duplicate names)
  mapping <- data.frame(
    unordered_cluster = integer(0),
    spectral_cluster = integer(0),
    unofficial_cluster_name = character(0),
    n = integer(0),
    method = character(0),
    stringsAsFactors = FALSE
  )

  assigned_names <- c()

  # Sort by fraction descending so best matches get priority
  votes <- votes %>% dplyr::arrange(dplyr::desc(fraction))

  for (i in seq_len(nrow(votes))) {
    name <- votes$old_name[i]
    cl <- votes$unordered_cluster[i]
    frac <- votes$fraction[i]

    if (frac >= threshold && !name %in% assigned_names && name %in% cluster_names) {
      idx <- which(cluster_names == name)
      mapping <- rbind(mapping, data.frame(
        unordered_cluster = cl,
        spectral_cluster = idx,
        unofficial_cluster_name = name,
        n = sum(result$unordered_cluster == cl),
        method = sprintf("majority-vote (%.0f%%)", frac * 100),
        stringsAsFactors = FALSE
      ))
      assigned_names <- c(assigned_names, name)
    }
  }

  # For unmapped clusters, fall back to heuristic scoring
  unmapped <- setdiff(clusters, mapping$unordered_cluster)
  if (length(unmapped) > 0) {
    remaining_names <- setdiff(cluster_names, assigned_names)
    message(sprintf("  %d clusters unmapped, using heuristic fallback for: %s",
                    length(unmapped), paste(remaining_names, collapse = ", ")))

    # Use heuristic on unmapped subset
    sub_result <- result %>% dplyr::filter(unordered_cluster %in% unmapped)
    heuristic <- assign_cluster_names(sub_result, remaining_names)
    heuristic$method <- "heuristic"
    mapping <- rbind(mapping, heuristic)
  }

  # Sort by spectral_cluster for clean output
  mapping <- mapping %>% dplyr::arrange(spectral_cluster)
  mapping
}


#' Assign canonical cluster names to unordered spectral clusters (heuristic)
#'
#' Uses super_class, region, and side composition to match each
#' unordered cluster to the most likely canonical name.
#'
#' @param result data.frame with columns: unordered_cluster, super_class, region, side
#' @param cluster_names character vector of canonical cluster names (length = n_clusters)
#' @return data.frame mapping unordered_cluster -> spectral_cluster + unofficial_cluster_name
assign_cluster_names <- function(result, cluster_names) {

  n_clusters <- length(cluster_names)
  clusters <- sort(unique(result$unordered_cluster))

  # Compute features for each cluster
  cluster_features <- lapply(clusters, function(cl) {
    sub <- result %>% dplyr::filter(unordered_cluster == cl)
    n <- nrow(sub)
    sc_tab <- table(sub$super_class)
    reg_tab <- table(sub$region)
    side_tab <- table(sub$side)

    list(
      n = n,
      # Super class fractions
      frac_vnc = sum(sc_tab[grepl("ventral_nerve_cord", names(sc_tab))]) / n,
      frac_cb = sum(sc_tab[grepl("central_brain", names(sc_tab))]) / n,
      frac_visual = sum(sc_tab[grepl("visual", names(sc_tab))]) / n,
      frac_ascending = sum(sc_tab[grepl("ascending", names(sc_tab))]) / n,
      frac_descending = sum(sc_tab[grepl("descending", names(sc_tab))]) / n,
      # Region
      frac_vnc_region = sum(reg_tab[grepl("ventral_nerve_cord", names(reg_tab))]) / n,
      frac_cb_region = sum(reg_tab[grepl("central_brain", names(reg_tab))]) / n,
      frac_neck_region = sum(reg_tab[grepl("neck", names(reg_tab))]) / n,
      # Side
      frac_left = sum(side_tab[names(side_tab) == "left"], na.rm = TRUE) / n,
      frac_right = sum(side_tab[names(side_tab) == "right"], na.rm = TRUE) / n,
      frac_center = sum(side_tab[names(side_tab) %in% c("center", "midline")], na.rm = TRUE) / n
    )
  })
  names(cluster_features) <- clusters

  # Score each cluster against each canonical name
  # Higher score = better match
  score_cluster <- function(feat, name) {
    s <- 0
    if (name == "dorsal VNC") {
      s <- feat$frac_vnc * 2 + feat$frac_descending * 3 + feat$frac_ascending * 2 - feat$frac_cb * 2
    } else if (name == "leg VNC") {
      s <- feat$frac_vnc * 3 + feat$n / 10000  # largest VNC cluster
    } else if (name == "abdominal VNC") {
      s <- feat$frac_vnc * 2 - feat$n / 10000  # smallest VNC cluster
    } else if (name == "posterior brain") {
      s <- feat$frac_cb * 2 + feat$frac_visual * 1.5 - feat$frac_vnc * 2
    } else if (name == "lateral brain") {
      s <- feat$frac_cb * 1.5 + feat$frac_vnc * 0.5 + feat$frac_ascending * 1.5 - feat$frac_visual * 1
    } else if (name == "flange/GNG/median bundle") {
      s <- feat$frac_cb * 1 + feat$frac_neck_region * 3 + feat$frac_descending * 1
    } else if (name == "inferior brain") {
      s <- feat$frac_cb * 1 + feat$frac_visual * 2 - feat$frac_vnc * 2 - feat$frac_left * 1 - feat$frac_right * 1
    } else if (name == "superior brain") {
      s <- feat$frac_cb * 2 - feat$frac_visual * 1 - feat$frac_vnc * 2 + feat$frac_center * 1
    } else if (name == "left visual") {
      s <- feat$frac_visual * 3 + feat$frac_left * 3 - feat$frac_right * 3
    } else if (name == "right visual") {
      s <- feat$frac_visual * 3 + feat$frac_right * 3 - feat$frac_left * 3
    } else if (name == "left olfactory") {
      s <- feat$frac_cb * 2 + feat$frac_left * 3 - feat$frac_visual * 2 - feat$frac_vnc * 2
    } else if (name == "right olfactory") {
      s <- feat$frac_cb * 2 + feat$frac_right * 3 - feat$frac_visual * 2 - feat$frac_vnc * 2
    } else if (name == "central complex related") {
      s <- feat$frac_cb * 2 + feat$frac_center * 3 - feat$frac_visual * 2 - feat$frac_vnc * 2
    }
    s
  }

  # Build score matrix: clusters x names
  score_mat <- matrix(0, nrow = length(clusters), ncol = n_clusters,
                      dimnames = list(clusters, cluster_names))
  for (i in seq_along(clusters)) {
    for (j in seq_along(cluster_names)) {
      score_mat[i, j] <- score_cluster(cluster_features[[i]], cluster_names[j])
    }
  }

  # Greedy assignment: highest score first, no duplicates
  mapping <- data.frame(
    unordered_cluster = integer(n_clusters),
    spectral_cluster = seq_len(n_clusters),
    unofficial_cluster_name = cluster_names,
    n = integer(n_clusters),
    method = rep("heuristic", n_clusters),
    stringsAsFactors = FALSE
  )

  assigned_clusters <- c()
  assigned_names <- c()

  for (iter in seq_len(n_clusters)) {
    # Find best unassigned match
    remaining_rows <- setdiff(seq_along(clusters), assigned_clusters)
    remaining_cols <- setdiff(seq_len(n_clusters), assigned_names)

    if (length(remaining_rows) == 0 || length(remaining_cols) == 0) break

    sub_mat <- score_mat[remaining_rows, remaining_cols, drop = FALSE]
    best <- which(sub_mat == max(sub_mat), arr.ind = TRUE)[1, ]

    row_idx <- remaining_rows[best[1]]
    col_idx <- remaining_cols[best[2]]

    mapping$unordered_cluster[col_idx] <- clusters[row_idx]
    mapping$n[col_idx] <- cluster_features[[row_idx]]$n

    assigned_clusters <- c(assigned_clusters, row_idx)
    assigned_names <- c(assigned_names, col_idx)
  }

  mapping
}
