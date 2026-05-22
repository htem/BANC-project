#' Within-cluster AN/DN heterogeneity blow-outs (Fig. 4a–e)
#'
#' Zooms in on individual AN/DN clusters to show that even within a
#' single behaviourally-named cluster, cell types diverge in their
#' sensory inputs and effector targets. Produces:
#'
#'   - Fig. 4a — same-cell-type / same-cluster / different-cluster
#'               cosine-similarity boxplots (pairwise Wilcoxon with Holm
#'               correction; 4,947,085 sensory→AN/DN and 4,054,128
#'               AN/DN→effector pairs).
#'   - Fig. 4b — PCA-UMAP with the head-orienting cluster highlighted and
#'               example cell types labelled.
#'   - Fig. 4c — min-max-normalised single-source / single-target
#'               influence overlays on the PCA-UMAP.
#'   - Fig. 4d — heatmap of normalised sensory→cell type influence for
#'               selected cell types in the head-orienting cluster, with
#'               only sensory sub classes above the 95th percentile shown.
#'   - Fig. 4e — heatmap of normalised cell type→effector influence,
#'               sub classes above the 99th percentile shown.
#'
#' Influence is min-max normalised across all AN/DNs for ease of comparison
#' across panels; the underlying value is adjusted influence (Eq. 10).
#'
#' @section Reads:
#'   banc.meta, banc.eff.meta, banc.edgelist.simple, franken.meta,
#'   paper.cols, banc.vpn.meta
#'   data/banc_annotations/v888/banc_neck_functional_classes.csv                                  (cluster labels + UMAP coords)
#'
#' @section Writes:
#'   figures/figure_4/links/cluster_blowout_*.pdf                            (Fig. 4a–e)
#'   figures/figure_4/links/extra/                                            (per-cluster zoom variants)
#'   figures/figure_4/links/*.txt                                              (pairwise Wilcoxon summaries)
#'
#' @section Paper:
#'   Fig. 4a–e — within-cluster heterogeneity panels.
#'   Methods §"Influence" Eqs. 9–10; §"Naming AN/DN clusters" (cell-type roster).
#'
#' @section Schema:
#'   `bc.meta <- banc.meta` alias retained for legacy panels still keyed
#'   on `bc.meta` (do not remove without auditing all downstream uses).
#'   `franken-meta.R` is sourced here because the blow-out panels join
#'   on franken cell-function labels for legend rows.
#'
#' @section Notes:
#'   pheatmap font / size unification has been parked (see CLAUDE.md
#'   "pheatmap font + size unification"); the 5 pheatmap calls here are
#'   on the parked-task scope.
#'
#' @section Reproduce:
#'   BANC_NCORES=1 Rscript R/figures/panels_cell_type_blowouts.R

###############
### STARTUP ###
###############

# load
source("R/startup/banc-startup.R")
source("R/startup/banc-meta.R")
source("R/startup/banc-edgelist.R")
source("R/startup/franken-meta.R")
source("R/startup/banc_an_dn_data.R")

# Alias for compatibility with scripts that define bc.meta
bc.meta <- banc.meta

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
if (exists(".banc_force_recalculate") && .banc_force_recalculate) recalculate <- TRUE

########################
### INFLUENCE SCORES ###
########################
ic_banc <- influence_calculator_py(edgelist_simple = banc.edgelist.simple,
                                   meta = banc.meta,
                                   count_thresh = 5)

# Pre-index banc.meta by each seed level ONCE (avoid subset/filter on full
# meta per inner iteration). Also pre-compute the two id-sets we filter to.
.seed12_to_ids <- split(banc.meta$root_id, banc.meta$seed_12)
.seed02_to_ids <- split(banc.meta$root_id, banc.meta$seed_02)
.seed07_to_ids <- split(banc.meta$root_id, banc.meta$seed_07)
.dn_plus_eff_ids <- unique(c(banc.eff2.meta$root_id, banc.an.dn.meta$root_id))
.andn_ids <- unique(banc.an.dn.meta$id)

# Helper: compute influence for each cell type in `cts`, filter to `target_ids`,
# accumulate into a pre-allocated list, rbindlist at the end (O(n) vs the old
# O(n^2) do.call(rbind, ...) pattern).
.run_influence_loop <- function(cts, level_name, id_index, target_ids) {
  n <- length(cts)
  out <- vector("list", n)
  pb <- progress::progress_bar$new(
    format = paste0(level_name, " [:bar] :current/:total (:percent) eta: :eta"),
    total  = n, clear = FALSE, width = 70
  )
  for (i in seq_along(cts)) {
    ct <- cts[[i]]
    try({
      banc.ct.ids <- unique(id_index[[ct]])
      if (length(banc.ct.ids) == 0) { pb$tick(); next }
      res <- calculate_influence_py(ic_banc, banc.ct.ids) %>%
        dplyr::filter(id %in% target_ids)
      res$seed <- ct
      res$level <- level_name
      res$influence_norm_original <-
        res$`Influence_score_(unsigned)` / length(banc.ct.ids)
      out[[i]] <- res
    }, silent = TRUE)
    pb$tick()
  }
  as.data.frame(data.table::rbindlist(out, fill = TRUE)) %>%
    dplyr::mutate(influence_original = `Influence_score_(unsigned)`)
}

# DN/AN
cts <- na.omit(unique(banc.meta$seed_12))
influence.nn.df <- .run_influence_loop(cts, "seed_12", .seed12_to_ids, .dn_plus_eff_ids)

# Sensory
cts <- na.omit(unique(banc.meta$seed_02))
influence.sens.df <- .run_influence_loop(cts, "seed_02", .seed02_to_ids, .andn_ids)

# Visual projection influence
cts <- na.omit(unique(banc.meta$seed_07))
influence.vpn.df <- .run_influence_loop(cts, "seed_07", .seed07_to_ids, .andn_ids)

# DISABLED 2026-04-09 (v850 migration): direct SQLite query for
# seed_12 influence. Superseded by .run_influence_loop() above
# (calls query_influence() against GCS feathers).
# # Connect to .sql file
# con <- DBI::dbConnect(RSQLite::SQLite(),
#                       file.path(banc.dropbox.influence.save.path,influence.sqlite))
# chosen.seeds <- na.omit(unique(banc.an.dn.meta$seed_12))
# chosen.ids <- unique(c(banc.eff2.meta$root_id,banc.an.dn.meta$root_id))
# influence.nn.df <- dplyr::tbl(con, influence.table) %>%
#   dplyr::filter(level %in% c("seed_12"),
#                 seed %in% !!chosen.seeds,
#                 id %in% chosen.ids) %>%
#   dplyr::select(seed, level, id, influence_original, influence_norm_original, influence_syn_norm) %>%
#   dplyr::collect()
# dbDisconnect(con)
# # Get alternative dataset for validation (seed_02)
# con <- DBI::dbConnect(RSQLite::SQLite(),
#                       file.path(banc.dropbox.influence.save.path,influence.sqlite))
# influence.sens.df <- dplyr::tbl(con, influence.table) %>%
#   dplyr::filter(!is_seed,
#                 level %in% c("seed_02"),
#                 id %in% !!banc.an.dn.meta$id) %>%
#   dplyr::collect() %>%
#   dplyr::filter(!grepl("unknown",seed))
# dbDisconnect(con)
# # Connect to .sql file
# con <- DBI::dbConnect(RSQLite::SQLite(),
#                       file.path(banc.dropbox.influence.save.path,influence.sqlite))
# chosen.cts <- unique(c(banc.sources$seed_07))
# influence.vpn.df <- dplyr::tbl(con, influence.table) %>%
#   dplyr::filter(!is_seed,
#                 level %in% c("seed_07"),
#                 seed %in% !!chosen.cts,
#                 id %in% !!banc.an.dn.meta$id) %>%
#   dplyr::collect()
# dbDisconnect(con)

# Format
# Hoist invariant transforms on influence.nn.df (added 2026-04-09):
#   1. Strip the "seed_XX_" prefix from `seed` so it matches root_id for joins.
#   2. Attach cell_type metadata.
#   3. Normalise once.
# The main super_cluster loop used to re-apply gsub("seed") + join per iteration
# (3x the work per cluster, 4 times across the loop).
influence.nn.df <- influence.nn.df %>%
  dplyr::mutate(seed = gsub(".*_","",seed)) %>%
  dplyr::left_join(banc.meta %>%
                     dplyr::distinct(root_id, cell_type, cell_sub_class, cell_class, super_class, cell_function),
                   by = c("id"="root_id")) %>%
  dplyr::left_join(banc.neck.meta %>%
                     dplyr::distinct(root_id, seed_cell_type = cell_type),
                   by = c("seed" = "root_id")) %>%
  dplyr::ungroup() %>%
  calculate_influence_norms()

# Format — sensors + VPN combined. Use rbindlist(fill=TRUE) instead of
# plyr::rbind.fill (much faster).
influence.sensor.df <- as.data.frame(
  data.table::rbindlist(list(influence.sens.df, influence.vpn.df), fill = TRUE)
) %>%
  dplyr::left_join(banc.meta %>%
                     dplyr::distinct(id, cell_type, cell_sub_class, cell_class, super_class, cell_function),
                   by = c("id"))
influence.sensor.df <- calculate_influence_norms(influence.sensor.df)

#############################
### MAIN SEQUENCE HEATMAPS ###
#############################

# --- Concise cell-type sampling ----------------------------------------------
# As super_clusters grow, the "concise" heatmaps would otherwise become
# unreadable on the AN/DN axis. We sample a fixed number of cell types per
# super_cluster (n_per_class ANs + n_per_class DNs), pinning a required set
# of cell types of interest if present and filling the remainder by greedy
# farthest-point sampling on per-cell-type centroids in 2D UMAP space — so
# the visible cell types span the diversity of the cluster. The same sample
# is used by BOTH the sensors→AN/DN and AN/DN→effectors concise heatmaps.
#
# `required` is built per-super_cluster from the union of literature-validated
# cell types (cns.functions + manuscript/print/paper_cell_type_references.csv)
# that have super_cluster == the cluster being plotted, plus a small baseline
# set retained for backward compatibility. This forces well-known cell types
# into every blowout so viewers can find them on the heatmap.
.concise_baseline_cts <- c("DNg89", "DNa06", "AN19B018", "AN18B023", "AN03A002")
.concise_exclude_cts  <- c("AN07B043")  # blank influence columns at count_thresh=5
.concise_n_per_class  <- 15L

# Load the curated literature reference CSV once; cache as .paper_ct_refs.
.paper_ct_refs <- tryCatch(
  readr::read_csv("manuscript/print/paper_cell_type_references.csv",
                  col_types = readr::cols(.default = readr::col_character())),
  error = function(e) {
    message("[concise] could not load paper_cell_type_references.csv: ",
            conditionMessage(e))
    tibble::tibble(cell_type = character(0), super_cluster = character(0))
  }
)

# Return literature cell types whose super_cluster equals the given name.
# Pulls from BOTH cns.functions (live SeaTable) and the curated paper refs.
.concise_required_for <- function(super_cluster_name) {
  if (is.null(super_cluster_name) || is.na(super_cluster_name)) return(character(0))
  lit_cns <- if (exists("cns.functions") &&
                 "super_cluster" %in% colnames(cns.functions)) {
    cns.functions %>%
      dplyr::filter(.data$super_cluster == super_cluster_name,
                    !is.na(.data$cell_type), .data$cell_type != "") %>%
      dplyr::pull(.data$cell_type)
  } else character(0)
  lit_pcr <- if (nrow(.paper_ct_refs) > 0 &&
                 "super_cluster" %in% colnames(.paper_ct_refs)) {
    .paper_ct_refs %>%
      dplyr::filter(.data$super_cluster == super_cluster_name,
                    !is.na(.data$cell_type), .data$cell_type != "") %>%
      dplyr::pull(.data$cell_type)
  } else character(0)
  unique(c(.concise_baseline_cts, lit_cns, lit_pcr))
}

.concise_cell_type_sample <- function(umap_df, super_cluster_name,
                                       required = NULL,
                                       n_per_class = .concise_n_per_class) {
  if (is.null(required)) required <- .concise_required_for(super_cluster_name)
  # Greedy farthest-point sampling on a 2D coordinate matrix.
  # `required` cell types (any present in `coords`) seed the selection;
  # the rest are added one at a time by maximising min distance to the
  # already-selected set. O(n*k) total.
  fps <- function(coords, required, n_total) {
    nm <- rownames(coords); n <- nrow(coords)
    if (n == 0) return(character(0))
    sel <- which(nm %in% required)
    if (length(sel) == 0) {
      sel <- which.max(rowSums(scale(coords, scale = FALSE)^2))
    }
    md <- rep(Inf, n)
    for (i in sel) {
      md <- pmin(md,
                 sqrt((coords[, 1] - coords[i, 1])^2 +
                      (coords[, 2] - coords[i, 2])^2))
    }
    md[sel] <- -Inf
    while (length(sel) < min(n_total, n)) {
      nx <- which.max(md)
      if (!is.finite(md[nx])) break
      sel <- c(sel, nx)
      md <- pmin(md,
                 sqrt((coords[, 1] - coords[nx, 1])^2 +
                      (coords[, 2] - coords[nx, 2])^2))
      md[nx] <- -Inf
    }
    nm[sel]
  }

  ct <- umap_df %>%
    dplyr::filter(super_cluster == super_cluster_name,
                  !is.na(cell_type), cell_type != "",
                  !cell_type %in% .concise_exclude_cts,
                  !is.na(UMAP1), !is.na(UMAP2)) %>%
    dplyr::group_by(super_class, cell_type) %>%
    dplyr::summarise(UMAP1 = mean(UMAP1, na.rm = TRUE),
                     UMAP2 = mean(UMAP2, na.rm = TRUE),
                     .groups = "drop")

  to_mat <- function(df) {
    if (nrow(df) == 0) {
      return(matrix(numeric(0), nrow = 0, ncol = 2,
                    dimnames = list(NULL, c("UMAP1", "UMAP2"))))
    }
    m <- as.matrix(df[, c("UMAP1", "UMAP2")])
    rownames(m) <- df$cell_type
    m
  }

  c(fps(to_mat(ct[ct$super_class == "ascending",  ]), required, n_per_class),
    fps(to_mat(ct[ct$super_class == "descending", ]), required, n_per_class))
}

# Subset the rows of `mat` (AN/DN cell types) to a sampled set, in place.
# Returns the (possibly reduced) matrix; emits a message with the sample size.
.apply_concise_ct_sample <- function(mat, chosen, label = "") {
  keep <- intersect(chosen, rownames(mat))
  if (length(keep) == 0) {
    message(sprintf("  concise %s: no chosen cell types in matrix — leaving as-is",
                    label))
    return(mat)
  }
  message(sprintf("  concise %s: kept %d/%d chosen AN/DN cell types in matrix",
                  label, length(keep), length(chosen)))
  mat[keep, , drop = FALSE]
}

#####################################################
### INPUT+OUTPUT CONNECTIVITY UMAP DATA BY NEURON ###
#####################################################

# Pre-compute the AN/DN → effector influence dataframe with normalisation
# done across the FULL BANC dataset (every AN/DN seed → every effector
# cell_sub_class, no super_cluster filter). This way `influence_log_minmax`
# is a per-effector min-max over the WHOLE AN/DN population, so the heatmap
# colour for a given effector column is comparable across super_clusters
# rather than re-normalised within each cluster.
# (Originally calculate_influence_norms() ran per-cluster inside
# build_blowout_influence_df, restricting min-max to the cluster's AN/DNs.)
.influence_eff_norm_full <- influence.nn.df %>%
  dplyr::mutate(target = cell_sub_class) %>%
  dplyr::filter(!is.na(target),
                id %in% banc.eff2.meta$id) %>%
  dplyr::mutate(target = dplyr::case_when(
    target %in% names(efferent.target.map) ~ efferent.target.map[target],
    TRUE ~ target
  )) %>%
  dplyr::filter(target %in% efferent.target.map) %>%
  calculate_influence_norms()

# Shared helper: assemble the per-cluster "stick together" influence_df from
# the cached sensory / VPN / AN/DN→effector dataframes.
build_blowout_influence_df <- function(cluster.ids) {
  # AN/DN → effectors branch — pulled from the whole-dataset pre-norm above.
  # We only filter to this cluster's AN/DN sources (after norm) and swap the
  # axes for plotting.
  influence_eff <- .influence_eff_norm_full %>%
    dplyr::filter(seed %in% cluster.ids) %>%
    dplyr::mutate(seed = target,
                  target = seed_cell_type,
                  seed_type = "effectors")

  # Sensory branch
  influence_sens <- influence.sensor.df %>%
    dplyr::filter(!is.na(seed),
                  id %in% cluster.ids) %>%
    dplyr::mutate(target = cell_type,
                  seed_type = "sensory") %>%
    dplyr::filter(!is.na(target))

  # Visual branch (remap seed through cns.functions response)
  influence_vis <- influence.sensor.df %>%
    dplyr::filter(!is.na(seed),
                  id %in% cluster.ids,
                  level == "seed_07") %>%
    dplyr::mutate(target = cell_type) %>%
    dplyr::left_join(cns.functions %>%
                       dplyr::select(seed = cell_type, vpn_function = response) %>%
                       dplyr::distinct(seed, .keep_all = TRUE),
                     by = "seed") %>%
    dplyr::mutate(seed = vpn_function,
                  seed_type = "visual") %>%
    dplyr::filter(!is.na(target),
                  seed != "",
                  !is.na(seed),
                  !grepl("polarized", seed))

  sens_combined <- as.data.frame(
    data.table::rbindlist(list(influence_sens, influence_vis), fill = TRUE)
  ) %>%
    dplyr::mutate(seed = dplyr::case_when(
      seed %in% names(sensory.seed.map) ~ sensory.seed.map[seed],
      TRUE ~ seed
    ),
    seed_type = "sensors") %>%
    dplyr::filter(seed %in% sensory.seed.map) %>%
    calculate_influence_norms()

  as.data.frame(
    data.table::rbindlist(list(influence_eff, sens_combined), fill = TRUE)
  ) %>%
    dplyr::filter(!is.na(target),
                  !is.na(seed),
                  seed != "0",
                  target != "0")
}

# super.clusters <- sort(na.omit(unique(banc.an.dn.meta$super_cluster)))
super.clusters <- "head orienting"
for(super.cluster in super.clusters){
  message("working on:", super.cluster)
  inf.metric = "influence_norm_log"
  .chosen_cell_types <- .concise_cell_type_sample(umap.dn.df, super.cluster)

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
                         dplyr::select(id, neurotransmitter, cluster,
                                       side, region, super_class, 
                                       hemilineage, cell_function, nerve, 
                                       cell_sub_type, cell_class, cell_sub_class, 
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

  # Concise variant: three-layer UMAP — non-cluster points light grey,
  # super_cluster points (not FPS-chosen) dark grey, FPS-chosen cell types
  # in this super_cluster's paper.cols colour. Restores the broader UMAP
  # context that was previously suppressed (issue feedback 2026-05-03).
  .sc_col <- paper.cols[[super.cluster]]
  if (is.null(.sc_col) || is.na(.sc_col)) .sc_col <- paper.cols[["highlight"]]
  g.highlight.concise <- ggplot() +
    # Layer 1: all other super_clusters as faint background
    geom_point(data = umap.dn.df %>%
                 dplyr::filter(is.na(super_cluster) |
                               super_cluster != super.cluster),
               aes(x = UMAP1, y = UMAP2),
               color = "grey70",
               fill  = "white",
               size  = 1.5,
               shape = 19,
               alpha = 0.5) +
    # Layer 2: this super_cluster's non-chosen members
    geom_point(data = umap.dn.df %>%
                 dplyr::filter(super_cluster == super.cluster,
                               !cell_type %in% .chosen_cell_types),
               aes(x = UMAP1, y = UMAP2),
               color = "grey10",
               fill  = "white",
               size  = 2,
               shape = 19,
               alpha = 0.85) +
    # Layer 3: FPS-chosen cell types, in the super_cluster's colour
    geom_point(data = umap.dn.df %>%
                 dplyr::filter(super_cluster == super.cluster,
                               cell_type %in% .chosen_cell_types),
               aes(x = UMAP1, y = UMAP2),
               color = .sc_col,
               fill  = "white",
               size  = 2.5,
               shape = 19,
               alpha = 0.95) +
    theme_void() +
    theme(legend.position = "bottom",
          legend.key.size = unit(0.75, "lines"),
          legend.text = element_text(size = rel(0.75)),
          legend.title = element_text(size = rel(0.75)),
          legend.spacing.x = unit(0.75, "lines"),
          legend.spacing.y = unit(0.75, "lines"),
          legend.margin = margin(t = 3, r = 3, b = 3, l = 3, unit = "pt")) +
    ggplot2::coord_fixed()

  print(g.highlight.concise)
  ggsave(plot = g.highlight.concise,
         filename = file.path(banc.fig4.path.clust,
                              sprintf("%s_neck_cluster_highlight_concise.pdf",
                                      gsub(" ","",super.clust))),
         width = 8, height = 8, dpi = 300)

  # Plot UMAP with convex hulls. Only color + name the cells listed in
  # cns.functions (= those with non-NA cell_function in banc.meta). All
  # other cells in the super_cluster render as a grey background so the
  # documented cell types are visually findable (2026-05-05).
  .umap_named <- umap.super.clust.df %>%
    dplyr::filter(!is.na(cell_function), cell_function != "")
  .umap_unnamed <- umap.super.clust.df %>%
    dplyr::filter(is.na(cell_function) | cell_function == "")
  g.blowout <- ggplot() +
    geom_density_2d(data = umap.super.clust.df,
                     aes(x = UMAP1, y = UMAP2),
                    col = "grey70",
                    alpha = 0.9) +
    # Background: cells without documented function — uniform grey
    geom_point(
      data = .umap_unnamed,
      aes(x = UMAP1, y = UMAP2),
      color = "grey75",
      fill  = "grey90",
      shape = 19,
      size  = 2,
      alpha = 0.5
    ) +
    # Foreground: cells listed in cns.functions — colored by cell_type
    geom_point(
      data = .umap_named,
      aes(x = UMAP1, y = UMAP2, color = cell_type),
      fill = "white",
      shape = 19,
      size = 3,
      alpha = 0.9
    ) +
    scale_color_cerise_limon(guide = guide_legend(title = "cell types:")) +
    ggrepel::geom_label_repel(
      data = .umap_named %>%
        dplyr::distinct(cell_type, .keep_all = TRUE),
      aes(x = UMAP1, y = UMAP2, label = cell_type),
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

  # Build the per-cluster influence df via the shared helper — same output as
  # the old 50-line inline pipeline, just deduplicated (2026-04-09).
  influence_df_shared <- build_blowout_influence_df(cluster.ids)

  # Iterate over BOTH metrics. Previously only influence_norm_log was produced
  # inside this loop; influence_log came from the 380-line demo block above,
  # and only for head orienting. Now every cluster gets both variants; for
  # head orienting, the influence_log PDFs are also file.copy'd to the top-level
  # fig4 path after rendering, preserving the paper filenames.
  for (inf.metric in c("influence_log", "influence_norm_log", "influence_log_minmax")) {
  # influence_log_minmax = per-target (AN/DN) min-max normalised influence_log,
  # pre-computed by calculate_influence_norms() (by = target). Shows "which
  # sensor does this cell type care about most?" — 0–1 per target column.
  # Already exists in influence_df_shared, no manual normalisation needed.
  .base_metric <- inf.metric

  # Seed (column) filter: keep only seeds whose peak value on any target
  # exceeds the per-seed_type quantile. Effector quantile is more aggressive
  # than sensors (0.995 vs 0.99) so the effector heatmap has roughly half
  # the rows of the sensor heatmap (issue feedback 2026-05-03).
  #
  # The keep-filter always works on raw `influence_log` (absolute scale) —
  # we plot whatever .base_metric is, but the *decision* of which seeds to
  # keep is made on absolute strength. influence_log_minmax can't be filtered
  # on directly because each target column has a 1.0 by construction, so any
  # quantile-based "is the seed dominant somewhere" test passes nearly every
  # seed.
  .seed_qtile <- function(st) dplyr::case_when(
    st == "effectors" ~ 0.99,
    TRUE              ~ 0.98
  )
  qtile_df <- influence_df_shared %>%
    dplyr::group_by(seed_type) %>%
    dplyr::reframe(
      thresh = stats::quantile(.data$influence_log, .seed_qtile(seed_type[1]), na.rm = TRUE)
    ) %>%
    dplyr::distinct()

  keepers <- influence_df_shared %>%
    dplyr::left_join(qtile_df, by = "seed_type") %>%
    dplyr::group_by(seed_type, seed) %>%
    dplyr::summarize(any_above_thresh = any(influence_log >= thresh, na.rm = TRUE),
                     .groups = "drop") %>%
    dplyr::filter(any_above_thresh)

  influence_df <- influence_df_shared %>%
    dplyr::semi_join(keepers, by = c("seed_type", "seed")) %>%
    dplyr::ungroup()

  if (nrow(influence_df) == 0) {
    message(sprintf("  skipping %s + %s — empty influence_df", super.cluster, inf.metric))
    next
  }

  # Cast
  influence_matrix <- influence_df  %>%
    dplyr::distinct(seed, target, .keep_all = TRUE) %>%
    reshape2::dcast(seed ~ target,
                    fun.aggregate = mean,
                    value.var = .base_metric,
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

  # No manual normalisation needed — influence_log_minmax is already 0–1
  # per target from calculate_influence_norms(). The cast fills missing
  # seed×target combos with 0, which is correct (no influence = 0).
  influence_matrix[is.infinite(influence_matrix)] <- 0
  dimnames(influence_matrix) <- nams
  influence_matrix <- t(influence_matrix)

  # Restrict AN/DN cell types (rows) to the FPS-chosen sample.
  influence_matrix <- .apply_concise_ct_sample(influence_matrix, .chosen_cell_types,
                                                label = super.cluster)

  # Replace any concise-sampled AN/DN cell type whose row is all-zero
  # (no influence across any seed) with the best alternate from the same
  # super_class in the super_cluster, ranked by row-sum in this metric.
  # Alternates must not already be present in influence_matrix rows.
  .eps <- 1e-12
  .zero_rows <- rownames(influence_matrix)[rowSums(abs(influence_matrix), na.rm = TRUE) <= .eps]
  if (length(.zero_rows) > 0) {
    .ct_sc <- banc.an.dn.meta %>%
      dplyr::filter(super_cluster == super.cluster,
                    !is.na(cell_type), cell_type != "",
                    !cell_type %in% .concise_exclude_cts) %>%
      dplyr::distinct(cell_type, super_class)
    .full_rowsums <- tapply(
      influence_df_shared[[inf.metric]],
      influence_df_shared$id,
      function(x) sum(abs(x), na.rm = TRUE)
    )
    for (.bad in .zero_rows) {
      .bad_super_class <- .ct_sc$super_class[.ct_sc$cell_type == .bad][1]
      if (is.na(.bad_super_class)) next
      .candidates <- setdiff(
        .ct_sc$cell_type[.ct_sc$super_class == .bad_super_class],
        rownames(influence_matrix)
      )
      .cand_scores <- .full_rowsums[.candidates]
      .cand_scores <- .cand_scores[!is.na(.cand_scores) & .cand_scores > .eps]
      if (length(.cand_scores) == 0) next
      .repl <- names(.cand_scores)[which.max(.cand_scores)]
      # Build replacement row from influence_df_shared for this metric
      .repl_row <- influence_df_shared %>%
        dplyr::filter(id == .repl) %>%
        dplyr::distinct(seed, .keep_all = TRUE) %>%
        { stats::setNames(.[[inf.metric]], .$seed) }
      .new_row <- stats::setNames(rep(0, ncol(influence_matrix)),
                                  colnames(influence_matrix))
      .common <- intersect(names(.repl_row), names(.new_row))
      .new_row[.common] <- .repl_row[.common]
      .idx <- match(.bad, rownames(influence_matrix))
      influence_matrix[.idx, ] <- .new_row
      rownames(influence_matrix)[.idx] <- .repl
      message(sprintf("  concise %s (%s): replaced all-zero %s with %s",
                      super.cluster, inf.metric, .bad, .repl))
    }
  }

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
    # Guard: hclust_semisupervised needs ≥ 2 groups with ≥ 2 members each.
    # With aggressive filtering (e.g. influence_log_minmax at 99th pctl),
    # groups can end up empty → subscript out of bounds in .merge_hclust.
    if (length(groups) >= 2) {
      clustering_result <- hclust_semisupervised(data = t(influence_matrix),
                                                 groups = groups,
                                                 dist_method = "euclidean",
                                                 hclust_method = "ward.D2")
      influence_matrix <- t(clustering_result$data)
      col_annotation <- col_annotation[rownames(col_annotation) %in% colnames(influence_matrix), , drop = FALSE]
      hclust_cols <- clustering_result$hclust
    } else {
      message(sprintf("  %s: < 2 seed_type groups for semi-supervised clustering, using plain hclust",
                      inf.metric))
      col_dist <- dist(t(influence_matrix), method = "euclidean")
      hclust_cols <- hclust(col_dist, method = "ward.D2")
    }
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
  if (nrow(influence_matrix) >= 2) {
    row_dist <- dist(influence_matrix, method = "euclidean")
    row.dend <- hclust(row_dist, method = "ward.D2")
  } else {
    row.dend <- NULL
  }

  # Split heatmap
  influence_matrix_sensors <- influence_matrix[,colnames(influence_matrix)%in%unname(sensory.seed.map), drop=FALSE]
  influence_matrix_effectors <- influence_matrix[,colnames(influence_matrix)%in%unname(efferent.target.map), drop=FALSE]
  col.dend.sensors <- if (ncol(influence_matrix_sensors) >= 2) {
    hclust(dist(t(influence_matrix_sensors), method = "euclidean"), method = "ward.D2")
  } else NULL
  col.dend.effectors <- if (ncol(influence_matrix_effectors) >= 2) {
    hclust(dist(t(influence_matrix_effectors), method = "euclidean"), method = "ward.D2")
  } else NULL
  
  # Color scale anchored to the FULL super_cluster's influence data (all
  # AN/DN × all seeds), not just the concise-filtered matrix. This ensures
  # the heatmap and UMAP overlays show the same scale, and the scale
  # reflects the full dynamic range — not just the top-end variation among
  # already-strong connections. 5th–99th percentile of non-zero values
  # (updated 2026-04-11).
  n_breaks <- 100
  .full_vals <- na.omit(influence_df_shared[[inf.metric]])
  .full_nz <- .full_vals[.full_vals > 0]
  color.min <- if (length(.full_nz) > 0) quantile(.full_nz, 0.05, na.rm = TRUE) else 0
  color.max <- if (length(.full_nz) > 0) quantile(.full_nz, 0.99, na.rm = TRUE) else 1
  scaled_heatmap_sensor_breaks <- seq(color.min, color.max, length.out = n_breaks)
  scaled_heatmap_sensor_palette <- colorRampPalette(c("#1f4e79", "#4a90a4", "#7ba7bc", "#a67c8a", "#c4967d", "#b22222"))(n_breaks - 1)
  # Use the same scale for effectors (shared across both panels for this cluster)
  scaled_heatmap_effector_breaks <- scaled_heatmap_sensor_breaks
  scaled_heatmap_effector_palette <- scaled_heatmap_sensor_palette
  
  # Plot: sensors → AN/DN concise heatmap.
  .sens_file <- file.path(banc.fig4.path.clust,
                          sprintf("concise_sensors_to_%s_cell_types_%s.pdf",
                                  gsub(" ","",super.clust), inf.metric))
  if (nrow(influence_matrix_sensors) >= 2 && ncol(influence_matrix_sensors) >= 2) {
    pheatmap(
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
      filename = .sens_file,
      main = paste0(inf.metric, "\n(row: source, col: target)"),
      na_col = "lightgrey"
    )
  } else {
    message(sprintf("  skipping sensors concise pheatmap for %s + %s — matrix too small (%dx%d)",
                    super.cluster, inf.metric,
                    nrow(influence_matrix_sensors), ncol(influence_matrix_sensors)))
  }

  # Plot: AN/DN → effectors concise heatmap.
  .eff_file <- file.path(banc.fig4.path.clust,
                         sprintf("concise_%s_cell_types_to_effector_cell_sub_class_%s.pdf",
                                 gsub(" ","",super.clust), inf.metric))
  if (nrow(influence_matrix_effectors) >= 2 && ncol(influence_matrix_effectors) >= 2) {
    pheatmap(
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
      filename = .eff_file,
      main = paste0(inf.metric, "\n(row: target, col: source)"),
      na_col = "lightgrey"
    )
  } else {
    message(sprintf("  skipping effectors concise pheatmap for %s + %s — matrix too small (%dx%d)",
                    super.cluster, inf.metric,
                    nrow(influence_matrix_effectors), ncol(influence_matrix_effectors)))
  }

  # ---------------------------------------------------------------
  # FULL (un-thresholded, all cell types in this super_cluster)
  # ---------------------------------------------------------------
  # Same color scale as the concise variant (uses scaled_heatmap_*_breaks /
  # _palette computed from the FULL super_cluster's range), but skips the
  # keep-filter and the FPS sampling so reviewers can see the entire data.
  full_mat <- influence_df_shared %>%
    dplyr::distinct(seed, target, .keep_all = TRUE) %>%
    reshape2::dcast(seed ~ target,
                    fun.aggregate = mean,
                    value.var = .base_metric,
                    fill = 0)
  rownames(full_mat) <- full_mat$seed
  full_mat$seed <- NULL
  .full_nams <- list(rownames(full_mat), colnames(full_mat))
  full_mat <- as.matrix(full_mat)
  full_mat <- matrix(as.numeric(full_mat), nrow = nrow(full_mat), ncol = ncol(full_mat),
                     dimnames = .full_nams)
  full_mat[is.na(full_mat)]       <- 0
  full_mat[is.infinite(full_mat)] <- 0
  full_mat <- t(full_mat)   # rows = AN/DN cell_types, cols = seeds

  full_mat_sensors   <- full_mat[, colnames(full_mat) %in% unname(sensory.seed.map),    drop = FALSE]
  full_mat_effectors <- full_mat[, colnames(full_mat) %in% unname(efferent.target.map), drop = FALSE]

  # Plot: full sensors → AN/DN heatmap.
  .sens_full_file <- file.path(banc.fig4.path.clust,
                               sprintf("full_sensors_to_%s_cell_types_%s.pdf",
                                       gsub(" ","",super.clust), inf.metric))
  if (nrow(full_mat_sensors) >= 2 && ncol(full_mat_sensors) >= 2) {
    pheatmap(
      t(full_mat_sensors),
      color = scaled_heatmap_sensor_palette,
      breaks = scaled_heatmap_sensor_breaks,
      cluster_rows = TRUE,
      cluster_cols = TRUE,
      clustering_method = "ward.D2",
      show_rownames = TRUE,
      show_colnames = TRUE,
      treeheight_row = 0,
      treeheight_col = 0,
      fontsize_row = 6,
      fontsize_col = 6,
      cellwidth = 6,
      cellheight = 6,
      border_color = NA,
      filename = .sens_full_file,
      main = paste0("full ", inf.metric, "\n(rows: sensors, cols: AN/DN cell types)"),
      na_col = "lightgrey"
    )
  } else {
    message(sprintf("  skipping full sensors pheatmap for %s + %s — matrix too small (%dx%d)",
                    super.cluster, inf.metric,
                    nrow(full_mat_sensors), ncol(full_mat_sensors)))
  }

  # Plot: full AN/DN → effectors heatmap.
  .eff_full_file <- file.path(banc.fig4.path.clust,
                              sprintf("full_%s_cell_types_to_effector_cell_sub_class_%s.pdf",
                                      gsub(" ","",super.clust), inf.metric))
  if (nrow(full_mat_effectors) >= 2 && ncol(full_mat_effectors) >= 2) {
    pheatmap(
      t(full_mat_effectors),
      color = scaled_heatmap_effector_palette,
      breaks = scaled_heatmap_effector_breaks,
      cluster_rows = TRUE,
      cluster_cols = TRUE,
      clustering_method = "ward.D2",
      show_rownames = TRUE,
      show_colnames = TRUE,
      treeheight_row = 0,
      treeheight_col = 0,
      fontsize_row = 6,
      fontsize_col = 6,
      cellwidth = 6,
      cellheight = 6,
      border_color = NA,
      filename = .eff_full_file,
      main = paste0("full ", inf.metric, "\n(rows: effectors, cols: AN/DN cell types)"),
      na_col = "lightgrey"
    )
  } else {
    message(sprintf("  skipping full effectors pheatmap for %s + %s — matrix too small (%dx%d)",
                    super.cluster, inf.metric,
                    nrow(full_mat_effectors), ncol(full_mat_effectors)))
  }

  }  # end metric loop

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

  # Make interpretable plots.
  # Use the FULL umap.dn.df (all AN/DN neurons) so the UMAP cutouts show
  # all points as context — the super_cluster's neurons get colored by
  # influence while all others appear as grey background (2026-04-11).
  umap.super.clust.df$cluster <- super.clust

  # influence_log_minmax is already computed by calculate_influence_norms()
  # in influence.together (per-target normalisation, by = target). No need
  # to recompute — just use it directly as a metric for UMAP overlays.

  # Lock the UMAP zoom box to the concise AN/DN sample so the sensors and
  # effectors panels (and all metrics) render with identical axes. Without
  # this, banc_interpret_umaps computes the bbox per-seed from non-NA
  # influence points, which drifts between seed sets.
  .concise_pts <- umap.dn.df %>%
    dplyr::filter(cell_type %in% .chosen_cell_types,
                  !is.na(UMAP1), !is.na(UMAP2))
  if (nrow(.concise_pts) >= 2) {
    .x_rng_c <- range(.concise_pts$UMAP1, na.rm = TRUE)
    .y_rng_c <- range(.concise_pts$UMAP2, na.rm = TRUE)
    .pad_c   <- 0.05
    .concise_xlim <- c(.x_rng_c[1] - diff(.x_rng_c) * .pad_c,
                       .x_rng_c[2] + diff(.x_rng_c) * .pad_c)
    .concise_ylim <- c(.y_rng_c[1] - diff(.y_rng_c) * .pad_c,
                       .y_rng_c[2] + diff(.y_rng_c) * .pad_c)
  } else {
    .concise_xlim <- NULL
    .concise_ylim <- NULL
  }

  # sensory + effector UMAP overlays — 3 metrics
  for (.umap_metric in c("influence_log", "influence_norm_log", "influence_log_minmax")) {
    banc_interpret_umaps(
      umap.df = umap.dn.df,
      elist.pre = NULL,
      elist.post = NULL,
      influence.df = influence.together %>%
        dplyr::filter(seed %in% colnames(influence_matrix_sensors)),
      inf.metric = .umap_metric,
      identifier = .umap_metric,
      neuroanatomy = FALSE,
      umaps = TRUE,
      banc.meta  = banc.meta,
      save.path = file.path(banc.fig4.path.clust, "sensors"),
      recalculate = FALSE,
      height = 6,
      width = 6,
      dpi = 150,
      ncores = 1L,  # must be sequential — mclapply fork crashes PETSc/MPI
      scaled_heatmap_palette = scaled_heatmap_sensor_palette,
      scaled_heatmap_breaks = scaled_heatmap_sensor_breaks,
      xlim_fixed = .concise_xlim,
      ylim_fixed = .concise_ylim
    )

    banc_interpret_umaps(
      umap.df = umap.dn.df,
      elist.pre = NULL,
      elist.post = NULL,
      influence.df = influence.together %>%
        dplyr::filter(seed %in% colnames(influence_matrix_effectors)),
      inf.metric = .umap_metric,
      identifier = .umap_metric,
      neuroanatomy = FALSE,
      umaps = TRUE,
      banc.meta  = banc.meta,
      save.path = file.path(banc.fig4.path.clust, "effectors"),
      recalculate = FALSE,
      height = 6,
      width = 6,
      dpi = 150,
      ncores = 1L,  # must be sequential — mclapply fork crashes PETSc/MPI
      scaled_heatmap_palette = scaled_heatmap_sensor_palette,
      scaled_heatmap_breaks = scaled_heatmap_sensor_breaks,
      xlim_fixed = .concise_xlim,
      ylim_fixed = .concise_ylim
  )
  }  # end for (.umap_metric)

}
