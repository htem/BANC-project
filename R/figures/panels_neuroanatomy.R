#' panels_neuroanatomy.R — 3D neuroanatomy renderings (Fig. 1, 3, 4, 6, ED Fig. 8)
#'
#' Brain top + VNC side projections of every AN/DN cluster, effector
#' cluster, and CNS network, in JRC2018 template space. Drives the
#' neuroanatomy column in Figs. 3, 4, 6 and the per-cluster highlights
#' in ED Fig. 8.
#'
#' @section Reads:
#'   * banc.meta + umap.dn.df + umap.eff.df   via R/startup/banc-meta.R
#'   * Neuron meshes from gs://lee-lab.../neuron_meshes/  (pulled on demand)
#'   * Template-space registrations in nat.jrcbrains (JRC2018 brain + VNC).
#'
#' @section Writes:
#'   * figures/figure_{1,3,4,6}/links/neuroanatomy/*.pdf and *.png
#'   * figures/figure_4/links/<cluster>/<cluster>_neck_cluster_highlight.pdf
#'
#' @section Paper:
#'   * Fig. 1 region overlays, Fig. 3 AN/DN cluster anatomies,
#'     Fig. 4 head-orienting blowouts, Fig. 6 CNS-network anatomies,
#'     ED Fig. 8 per-cluster highlights.
#'   * Methods §"BANC neuropil mesh generation", §"Neuropils and
#'     template alignment".
#'
#' @section Notes:
#'   * LONGEST script in the pipeline — 12–20 h end-to-end for a full
#'     cluster sweep. Run ALONE (OOM-killed at ~23 GB when running
#'     alongside panels_an_dn_influence.R).
#'   * Default `recalculate <- FALSE` skips clusters whose PNGs already
#'     exist; flip to TRUE (or `BANC_RECALCULATE=TRUE`) to force re-render.
#'   * Has [N/total] progress logging across four sections.
#'
#' @section Reproduce: BANC_NCORES=1 Rscript R/figures/panels_neuroanatomy.R

source("R/startup/banc-startup.R")
source("R/startup/banc-meta.R")
source("R/startup/banc-functions.R")
library(nat.ggplot)

source("R/startup/gcs-helpers.R")

# Set to FALSE to skip super_clusters/clusters whose output PNGs already exist.
# Allows resuming after a crash without re-rendering completed plots.
recalculate <- FALSE
if (exists(".banc_force_recalculate") && .banc_force_recalculate) recalculate <- TRUE

# combine
cns.clusters <- plyr::rbind.fill(umap.dn.df,
                                 umap.eff.df) %>%
  dplyr::mutate(cluster = dplyr::case_when(
    grepl("sensory",super_class) ~ cell_sub_class,
    TRUE ~ cluster
  ))

# Make neurooanatomy plot template
g.anat <- ggplot2::ggplot() +
  ggplot2::coord_fixed() +
  ggplot2::theme_void() +
  ggplot2::guides(fill = "none", color = "none") +
  ggplot2::theme(legend.position = "none",
                 plot.title = ggplot2::element_text(hjust = 0, size = 8,
                                                    face = "bold",
                                                    colour = "black"),
                 axis.title.x = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_blank(),
                 axis.ticks.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_blank(),
                 axis.ticks.y = ggplot2::element_blank(),
                 axis.line = ggplot2::element_blank(),
                 panel.grid.major = ggplot2::element_blank(),
                 panel.grid.minor = ggplot2::element_blank(),
                 plot.margin = ggplot2::margin(0, 0, 0, 0),
                 panel.spacing = ggplot2::unit(0, "cm"),
                 panel.border = ggplot2::element_blank(),
                 panel.background = ggplot2::element_blank(),
                 plot.background = ggplot2::element_blank()) +
  ggplot2::labs(title = '')
g.anat.main <- g.anat +
  geom_neuron(x = banc_neuropil.surf,
              cols = c("grey60", "grey30"),
              rotation_matrix = bancr:::banc_rotation_matrices[["main"]],
              alpha = 0.1)
g.anat.brain <- g.anat +
  geom_neuron(x = banc_brain_neuropil.surf,
              cols = c("grey60", "grey30"),
              rotation_matrix = bancr:::banc_rotation_matrices[["front"]],
              alpha = 0.1)
g.anat.vnc <- g.anat +
  geom_neuron(x = banc_vnc_neuropil.surf,
              cols = c("grey60", "grey30"),
              rotation_matrix = bancr:::banc_rotation_matrices[["vnc"]],
              alpha = 0.1)

################################
### THIN STACKED BAR PLOTS  ####
################################
# Per-super_cluster composition bars (added 2026-04-09).
# Visual style modelled on the cns_network bars in panel_super_clusters.R
# (lines 197-253). Cheap to generate (just dplyr + ggplot, no mesh loading)
# so they live ahead of the heavy mesh-rendering loops below — that way they
# get written even if the rendering loops error out late.
#
# Two flavours:
#   1. AN/DN super_clusters: proportion of ascending vs descending neurons
#      within each AN/DN super_cluster.
#   2. EFF (efferent) super_clusters: proportion of motor vs
#      visceral_circulatory neurons within each EFF super_cluster.
# Both go to figures/figure_4/links/supplement/ as one PDF per super_cluster.

dir.create(banc.fig4.supp.path, recursive = TRUE, showWarnings = FALSE)

# Helper: render one thin stacked bar for a single super_cluster's composition.
# `df_sub`        — data with columns `super_class` (factor, fill levels in
#                   plot order) and `n` (counts, will be normalised inside)
# `total_n`       — count label drawn beside the bar
# `out_path`      — full path to write the PDF
.thin_bar_plot <- function(df_sub, total_n, out_path) {
  df_sub$prop <- df_sub$n / sum(df_sub$n)
  g <- ggplot2::ggplot(df_sub,
                       ggplot2::aes(x = factor(1), y = prop,
                                    fill = super_class, group = super_class)) +
    ggplot2::geom_bar(stat = "identity", width = 0.2) +
    ggplot2::geom_text(
      data = data.frame(x = 1.25, y = 0, label = total_n),
      mapping = ggplot2::aes(x = x, y = y, label = label),
      inherit.aes = FALSE,
      angle = 90,
      size = 8,
      hjust = 0,
      vjust = 0.5,
      fontface = "bold"
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::percent_format(accuracy = 1),
      expand = ggplot2::expansion(mult = c(0, 0.08))
    ) +
    ggplot2::scale_fill_manual(values = paper.cols, drop = FALSE) +
    ggplot2::labs(title = "", x = "", y = "", fill = "") +
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
  ggsave(plot = g, filename = out_path,
         width = 1.5, height = 4, dpi = 300)
  invisible(out_path)
}

# --- (1) AN/DN super_cluster: proportion of ascending vs descending --------
.an_dn_sc_df <- banc.meta %>%
  dplyr::filter(super_class %in% c("ascending", "descending"),
                !is.na(super_cluster), super_cluster != "") %>%
  dplyr::count(super_cluster, super_class) %>%
  dplyr::mutate(super_class = factor(super_class,
                                     levels = c("ascending", "descending")))

for (sc in na.omit(unique(.an_dn_sc_df$super_cluster))) {
  df_sub <- .an_dn_sc_df %>% dplyr::filter(super_cluster == sc)
  if (nrow(df_sub) == 0) next
  total_n <- sum(df_sub$n)
  fname <- paste0(gsub(" |\\/", "_", sc), "_an_dn_bar.pdf")
  message("[bar] ", fname, " (n=", total_n, ")")
  try(.thin_bar_plot(df_sub, total_n, file.path(banc.fig4.extra.path, fname)))
}

# --- (2) EFF super_cluster: proportion of motor vs visceral_circulatory ----
.eff_sc_df <- banc.meta %>%
  dplyr::filter(super_class %in% c("motor", "visceral_circulatory"),
                !is.na(super_cluster), super_cluster != "") %>%
  dplyr::count(super_cluster, super_class) %>%
  dplyr::mutate(super_class = factor(super_class,
                                     levels = c("motor", "visceral_circulatory")))

for (sc in na.omit(unique(.eff_sc_df$super_cluster))) {
  df_sub <- .eff_sc_df %>% dplyr::filter(super_cluster == sc)
  if (nrow(df_sub) == 0) next
  total_n <- sum(df_sub$n)
  fname <- paste0(gsub(" |\\/", "_", sc), "_motor_visceral_bar.pdf")
  message("[bar] ", fname, " (n=", total_n, ")")
  try(.thin_bar_plot(df_sub, total_n, file.path(banc.fig4.extra.path, fname)))
}

##############################################
### PLOT SUPER CLUSTERS + PER-CLUSTER MESHES ##
##############################################
# Merged loop (2026-04-11): download meshes once per super_cluster, then
# render BOTH the super_cluster plot (colored by super_class → fig4) AND
# the per-cluster plots (colored by cell_type → fig3) from the same meshes.
# Previously these were two separate loops that each downloaded all meshes
# independently — this halves the total mesh downloads.
# Order from smallest to largest super_cluster (less memory pressure early,
# fail-fast on the big ones at the end).
.sc_sizes <- banc.meta %>%
  dplyr::filter(!is.na(super_cluster)) %>%
  dplyr::count(super_cluster, sort = FALSE) %>%
  dplyr::arrange(n)
.all_super_clusters <- .sc_sizes$super_cluster
.n_sc <- length(.all_super_clusters)
message(sprintf("=== SECTION 1/3: Super cluster + per-cluster meshes (%d super_clusters) ===", .n_sc))

# Pre-sample random cell_type colors (reproducible)
set.seed(42)
.all_cts <- na.omit(unique(banc.meta$cell_type))
.paper_pool <- unique(unname(paper.cols))
.paper_pool <- .paper_pool[!is.na(.paper_pool) & .paper_pool != ""]
.ct_colors <- sample(.paper_pool, length(.all_cts), replace = TRUE)
names(.ct_colors) <- .all_cts

# Templates for 3 views (reused every iteration)
.tmpl_main  <- g.anat + geom_neuron(x = banc_neuropil.surf,
  cols = c("grey60","grey30"),
  rotation_matrix = bancr:::banc_rotation_matrices[["main"]], alpha = 0.1)
.tmpl_brain <- g.anat + geom_neuron(x = banc_brain_neuropil.surf,
  cols = c("grey60","grey30"),
  rotation_matrix = bancr:::banc_rotation_matrices[["front"]], alpha = 0.1)
.tmpl_vnc   <- g.anat + geom_neuron(x = banc_vnc_neuropil.surf,
  cols = c("grey60","grey30"),
  rotation_matrix = bancr:::banc_rotation_matrices[["vnc"]], alpha = 0.1)

for (.sci in seq_along(.all_super_clusters)) {
  clust <- .all_super_clusters[.sci]
  try({
    message(sprintf("[super_cluster %d/%d] %s", .sci, .n_sc, clust))

    # Skip if output already exists and recalculate=FALSE
    .sc_out <- file.path(banc.fig4.anat.path, paste0(clust, "_main_neuroanatomy.png"))
    if (!recalculate && file.exists(.sc_out)) {
      message(sprintf("  SKIP (output exists): %s", basename(.sc_out)))
      next
    }

    banc.cns.clust <- banc.meta %>%
      dplyr::filter(super_cluster == clust)

    # Filter + side coverage check
    .n_total <- nrow(banc.cns.clust)
    ct.meta <- banc.cns.clust %>%
      dplyr::filter(side %in% c("right","midline","center"),
                    !is.na(super_class), super_class != "") %>%
      dplyr::arrange(cell_type)
    .side_coverage <- if (.n_total > 0) nrow(ct.meta) / .n_total else 0
    if (.side_coverage < 0.9) {
      message(sprintf("  WARNING: side coverage for %s is %.0f%% (%d/%d)",
                      clust, .side_coverage * 100, nrow(ct.meta), .n_total))
    }
    if (!nrow(ct.meta)) next

    # --- Streaming mesh rendering (2026-04-11) ---
    # geom_neuron converts mesh → polygon data.frame at layer-creation time,
    # so the mesh can be freed immediately after adding the layer. We stream
    # one neuron at a time: download, add to ALL ggplot objects that need it,
    # then discard the mesh. Only polygon coords accumulate in the plots.
    #
    # For each neuron we build layers for:
    #   - Super_cluster plot (3 views, colored by super_class → fig4)
    #   - Per-cluster plot (3 views, colored by cell_type → fig3)
    # The neuron's cluster determines which per-cluster plots it goes into.

    .clusters_in_sc <- na.omit(unique(ct.meta$cluster))
    .n_neurons <- nrow(ct.meta)

    # Initialize ggplot objects: super_cluster (3 views) + per-cluster (3 views each)
    g_sc_main  <- .tmpl_main
    g_sc_brain <- .tmpl_brain
    g_sc_vnc   <- .tmpl_vnc
    g_cl <- list()
    for (.cl in .clusters_in_sc) {
      g_cl[[.cl]] <- list(main = .tmpl_main, brain = .tmpl_brain, vnc = .tmpl_vnc)
    }

    message(sprintf("  streaming %d neurons...", .n_neurons))
    .pb <- progress::progress_bar$new(
      format = sprintf("  %s [:bar] :current/:total (:percent) eta: :eta", clust),
      total = .n_neurons, clear = FALSE, width = 70
    )
    .n_ok <- 0L
    for (.ni in seq_len(.n_neurons)) {
      .rid <- ct.meta$root_id[.ni]
      .sc  <- ct.meta$super_class[.ni]
      .ct  <- ct.meta$cell_type[.ni]
      .cl  <- ct.meta$cluster[.ni]

      # Download ONE mesh
      mesh <- tryCatch(
        banc_read_neuron_meshes(.rid, OmitFailures = TRUE),
        error = function(e) NULL
      )
      if (is.null(mesh) || !length(mesh)) next
      .n_ok <- .n_ok + 1L

      # Color for super_class (super_cluster plot)
      sc_col <- if (.sc %in% names(paper.cols)) paper.cols[[.sc]] else "#888888"
      sc_cols <- c(adjust_color_brightness(sc_col, 1.1),
                   adjust_color_brightness(sc_col, 0.9))

      # Color for cell_type (per-cluster plot)
      ct_col <- if (.ct %in% names(.ct_colors)) .ct_colors[[.ct]] else "#888888"
      ct_cols <- c(adjust_color_brightness(ct_col, 1.1),
                   adjust_color_brightness(ct_col, 0.9))

      # Add to super_cluster plots (main view)
      g_sc_main <- g_sc_main + geom_neuron(x = mesh,
        cols = sc_cols,
        rotation_matrix = bancr:::banc_rotation_matrices[["main"]], alpha = 0.5)

      # Add to per-cluster plot (main view)
      if (!is.null(.cl) && !is.na(.cl) && .cl %in% names(g_cl)) {
        g_cl[[.cl]]$main <- g_cl[[.cl]]$main + geom_neuron(x = mesh,
          cols = ct_cols,
          rotation_matrix = bancr:::banc_rotation_matrices[["main"]], alpha = 0.5)
      }

      # Brain split
      mesh_brain <- tryCatch(banc_decapitate(mesh, invert = TRUE, OmitFailures = TRUE),
                             error = function(e) NULL)
      if (!is.null(mesh_brain) && length(mesh_brain)) {
        g_sc_brain <- g_sc_brain + geom_neuron(x = mesh_brain,
          cols = sc_cols,
          rotation_matrix = bancr:::banc_rotation_matrices[["front"]], alpha = 0.5)
        if (!is.null(.cl) && !is.na(.cl) && .cl %in% names(g_cl)) {
          g_cl[[.cl]]$brain <- g_cl[[.cl]]$brain + geom_neuron(x = mesh_brain,
            cols = ct_cols,
            rotation_matrix = bancr:::banc_rotation_matrices[["front"]], alpha = 0.5)
        }
      }
      rm(mesh_brain)

      # VNC split
      mesh_vnc <- tryCatch(banc_decapitate(mesh, invert = FALSE, OmitFailures = TRUE),
                           error = function(e) NULL)
      if (!is.null(mesh_vnc) && length(mesh_vnc)) {
        g_sc_vnc <- g_sc_vnc + geom_neuron(x = mesh_vnc,
          cols = sc_cols,
          rotation_matrix = bancr:::banc_rotation_matrices[["vnc"]], alpha = 0.5)
        if (!is.null(.cl) && !is.na(.cl) && .cl %in% names(g_cl)) {
          g_cl[[.cl]]$vnc <- g_cl[[.cl]]$vnc + geom_neuron(x = mesh_vnc,
            cols = ct_cols,
            rotation_matrix = bancr:::banc_rotation_matrices[["vnc"]], alpha = 0.5)
        }
      }
      rm(mesh, mesh_vnc)
      # Periodic gc every 20 neurons to release mesh fragments
      if (.ni %% 20 == 0) gc(verbose = FALSE)
      .pb$tick()
    }
    message(sprintf("  %d/%d meshes loaded successfully", .n_ok, .n_neurons))

    # Save super_cluster plots → fig4 (then free immediately)
    ggsave(g_sc_main,  filename = file.path(banc.fig4.anat.path, paste0(clust, "_main_neuroanatomy.png")),
           width = 10, height = 10, dpi = 300)
    rm(g_sc_main)
    ggsave(g_sc_brain, filename = file.path(banc.fig4.anat.path, paste0(clust, "_brain_neuroanatomy.png")),
           width = 10, height = 10, dpi = 300)
    rm(g_sc_brain)
    ggsave(g_sc_vnc,   filename = file.path(banc.fig4.anat.path, paste0(clust, "_vnc_neuroanatomy.png")),
           width = 10, height = 10, dpi = 300)
    rm(g_sc_vnc)

    # Save per-cluster plots → fig3 (free each immediately after saving)
    for (.cl in .clusters_in_sc) {
      message(sprintf("  [cluster] %s", .cl))
      ggsave(g_cl[[.cl]]$main,  filename = file.path(banc.fig3.anat.path, paste0(.cl, "_main_neuroanatomy.png")),
             width = 10, height = 10, dpi = 300)
      ggsave(g_cl[[.cl]]$brain, filename = file.path(banc.fig3.anat.path, paste0(.cl, "_brain_neuroanatomy.png")),
             width = 10, height = 10, dpi = 300)
      ggsave(g_cl[[.cl]]$vnc,   filename = file.path(banc.fig3.anat.path, paste0(.cl, "_vnc_neuroanatomy.png")),
             width = 10, height = 10, dpi = 300)
      g_cl[[.cl]] <- NULL  # free each cluster's plots immediately
    }
    rm(g_cl)
    gc(verbose = FALSE)
    .mem_mb <- sum(gc(verbose = FALSE)[, 2])
    message(sprintf("  memory after gc: %.0f MB", .mem_mb))
  })
}

# NOTE: the old separate "PLOT MESHES" (per-cluster) section below is now
# only needed for clusters NOT covered by the super_cluster loop above
# (e.g. EFF clusters from umap.eff.df, or sensory clusters from
# cns.clusters that have no super_cluster). Filter to those only.
clusters <- na.omit(unique(cns.clusters$cluster))
# Remove clusters already rendered above
.rendered_clusters <- unlist(lapply(.all_super_clusters, function(sc) {
  na.omit(unique(banc.meta$cluster[banc.meta$super_cluster == sc]))
}))
clusters <- setdiff(clusters, .rendered_clusters)
.n_clusters <- length(clusters)
message(sprintf("=== SECTION 2/4: Remaining per-cluster meshes not in any super_cluster (%d clusters) ===", .n_clusters))
for(.ci in seq_along(clusters)){
  clust <- clusters[.ci]
  try({
  message(sprintf("[cluster %d/%d] %s", .ci, .n_clusters, clust))

  # Skip if output already exists and recalculate=FALSE
  .cl_out <- file.path(banc.fig3.anat.path, paste0(clust, "_main_neuroanatomy.png"))
  if (!recalculate && file.exists(.cl_out)) {
    message(sprintf("  SKIP (output exists): %s", basename(.cl_out)))
    next
  }

  banc.neck.sp.meta <- cns.clusters %>%
    dplyr::filter(cluster==clust)

  # All cell sub classes
  cts <- na.omit(unique(banc.neck.sp.meta$cell_type))
  
  ##### NBLAST clustering
  # DISABLED 2025-12: morphology-only clustering of neck-passing neurons
  # via NBLAST allbyall + Ward.D2 hierarchical clustering. Replaced by
  # the connectivity-PCA-UMAP cell-type clustering in
  # panels_an_dn_umap.R (Methods §"Clustering influence by influence and
  # connectivity"). Kept here as a reproducible reference for the
  # NBLAST dendrogram + leaf-ordered cerise-limon palette if anyone
  # wants to rebuild a morphology-only super-cluster view.
  # # get neuron skeletons
  # l2 <- banc_read_l2skel(unique(banc.neck.sp.meta$root_id), OmitFailures = TRUE)
  # dps <- dotprops(l2/1000, OmitFailures = TRUE)
  # banc.neck.sp.meta <- banc.neck.sp.meta %>%
  #   dplyr::filter(root_id %in% names(dps))
  # 
  # # run NBLAST
  # nb <- nat.nblast::nblast_allbyall(dps)
  # 
  # # amalgamate over cluster
  # rownames(nb) <- banc.neck.sp.meta$cell_type[match(banc.neck.sp.meta$root_id,rownames(nb))]
  # colnames(nb) <- banc.neck.sp.meta$cell_type[match(banc.neck.sp.meta$root_id,colnames(nb))]
  # nb = apply(nb, 2, function(i) tapply(i, rownames(nb), max, na.rm = TRUE))
  # nb = t(apply(t(nb), 2, function(i) tapply(i, colnames(nb), max, na.rm = TRUE)))
  # 
  # # cluster
  # hckcs <- nhclust(scoremat=nb, method = "ward.D2")
  # dend <- as.dendrogram(hckcs)
  # grps <- cutree(hckcs,k=8)
  # 
  # # Get the order of leaves
  # ordered_leaves <- labels(dend)
  # 
  # # Generate a color palette with the required number of colors
  # pcols <- cerise_limon_palette(length(cts))
  # ct.cols <- as.vector(pcols)
  # names(ct.cols) <- c(ordered_leaves,setdiff(cts,ordered_leaves))
  # 
  # ##### plot dendrogram
  # 
  # # Convert dendrogram to a data frame that ggplot can use
  # dendr_data <- dendro_data(dend)
  # 
  # # Get the leaf labels
  # leaf_labels <- label(dendr_data)
  # 
  # # Create a data frame for the labels, including their colors
  # label_data <- leaf_labels %>%
  #   mutate(color = ct.cols[label])
  # 
  # # Create the plot
  # g.dend <- ggplot() +
  #   geom_segment(data = segment(dendr_data), linewidth = 1,
  #                aes(x = x, y = y, xend = xend, yend = yend)) +
  #   geom_text(data = label_data,
  #             aes(x = x, y = y, label = label, color = color),
  #             hjust = 1.2, angle = 90, size = 4, vjust = 0.5) +
  #   scale_color_identity() +
  #   scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  #   coord_cartesian(clip = "off") +
  #   theme_minimal() +
  #   theme(
  #     axis.text = element_blank(),
  #     axis.ticks = element_blank(),
  #     panel.grid = element_blank(),
  #     plot.margin = margin(t = 5, r = 5, b = 50, l = 5, unit = "pt")
  #   ) +
  #   labs(x = NULL, y = NULL, title = "")
  # 
  # # Save the plot
  # ggsave(plot = g.dend,
  #        filename = file.path(banc.fig3.anat.path,paste0(clust,"_cluster_nblast.pdf")),
  #        width = 30, height = 2, dpi = 300)
  
  ##### Plot neuroanatomy
    g.anat.main <- g.anat +
      geom_neuron(x = banc_neuropil.surf,
                  cols = c("grey60", "grey30"),
                  rotation_matrix = bancr:::banc_rotation_matrices[["main"]],
                  alpha = 0.1)
    g.anat.brain <- g.anat +
      geom_neuron(x = banc_brain_neuropil.surf,
                  cols = c("grey60", "grey30"),
                  rotation_matrix = bancr:::banc_rotation_matrices[["front"]],
                  alpha = 0.1)
    g.anat.vnc <- g.anat +
      geom_neuron(x = banc_vnc_neuropil.surf,
                  cols = c("grey60", "grey30"),
                  rotation_matrix = bancr:::banc_rotation_matrices[["vnc"]],
                  alpha = 0.1)
    .n_total <- nrow(banc.neck.sp.meta)
    ct.meta <- banc.neck.sp.meta %>%
      dplyr::filter(side %in% c("right","midline","center")) %>%
      dplyr::arrange(cell_type)
    .side_coverage <- if (.n_total > 0) nrow(ct.meta) / .n_total else 0
    if (.side_coverage < 0.9) {
      message(sprintf("  WARNING: side coverage for %s is %.0f%% (%d/%d)",
                      clust, .side_coverage * 100, nrow(ct.meta), .n_total))
    }
    if(!nrow(ct.meta)){
      next
    }
    neurons.plot <- banc_read_neuron_meshes(ct.meta$root_id, OmitFailures = TRUE)
    if(!length(neurons.plot)){
      next
    }
    # plot.cts <- ct.meta$cell_type[match(ct.meta$root_id,names(neurons.plot))]
    # plot.cols <- ct.cols[plot.cts]
    plot.col <- if (clust %in% names(paper.cols)) paper.cols[[clust]] else "#888888"
    g.anat.main <- g.anat.main +
      geom_neuron(x = neurons.plot,
                  cols = c(adjust_color_brightness(plot.col,1.1),adjust_color_brightness(plot.col,0.9)),
                  rotation_matrix = bancr:::banc_rotation_matrices[["main"]],
                  alpha = 0.5)
    ggsave(plot = g.anat.main,
           filename = file.path(banc.fig3.anat.path,paste0(clust,"_main_neuroanatomy.png")),
           width = 10, height = 10, dpi = 300)
    neurons.plot.brain <- banc_decapitate(neurons.plot, invert = TRUE, OmitFailures = TRUE)
    if(length(neurons.plot.brain)){
      g.anat.brain <- g.anat.brain +
        geom_neuron(x = neurons.plot.brain,
                    cols = c(adjust_color_brightness(plot.col,1.1),adjust_color_brightness(plot.col,0.9)),
                    rotation_matrix = bancr:::banc_rotation_matrices[["front"]],
                    alpha = 0.5)
      ggsave(plot = g.anat.brain,
             filename = file.path(banc.fig3.anat.path,paste0(clust,"_brain_neuroanatomy.png")),
             width = 10, height = 10, dpi = 300)
    }
    neurons.plot.vnc <-  banc_decapitate(neurons.plot, invert = FALSE, OmitFailures = TRUE)
    if(length(neurons.plot.vnc)){
      g.anat.vnc <- g.anat.vnc +
        geom_neuron(x = neurons.plot.vnc,
                    cols = c(adjust_color_brightness(plot.col,1.1),adjust_color_brightness(plot.col,0.9)),
                    rotation_matrix = bancr:::banc_rotation_matrices[["vnc"]],
                    alpha = 0.5)
      ggsave(plot = g.anat.vnc,
             filename = file.path(banc.fig3.anat.path,paste0(clust,"_vnc_neuroanatomy.png")),
             width = 10, height = 10, dpi = 300)
    }
  })
}

###########################
### PLOT CNS CLUSTERS ###
#########################
proof.ids <- na.omit(unique(banc.meta %>%
                              dplyr::filter(!is.na(cns_network)) %>%
                              dplyr::pull(root_id)))

# Try GCS synapses_v2_enriched parquet first, fall back to local v626 CSV.
# History: 5-min timeout (2026-04-11) caught a hung GCS connection pool;
# 15-min (2026-05-19) wasn't enough because the post-Python dplyr filter
# (169M rows × ~17k proof.ids %in% checks) was inside the same withTimeout
# block and triggered the timeout even after the parquet read finished.
# Fix: only wrap the Python parquet read in withTimeout (15 min); run the
# mutate/rename/filter without a wall clock.
synapses <- FALSE
gcs.syn.path <- construct_path(banc.gcs.bucket, banc.version, "synapses")
message("Attempting GCS synapse load (15 min timeout on parquet read only)...")
banc.syns.raw <- tryCatch(
  R.utils::withTimeout({
    gcs_fs <- setup_gcs_filesystem()
    query_parquet_gcs(
      path = gcs.syn.path,
      gcs_filesystem = gcs_fs,
      columns = c("id", "size", "pre_root_id", "post_root_id", "ctr_x", "ctr_y", "ctr_z")
    )
  }, timeout = 900),  # 15 min timeout
  error = function(e) {
    message("GCS parquet read failed (", conditionMessage(e), ")")
    NULL
  })

if (!is.null(banc.syns.raw)) {
  message("GCS read done; filtering ", nrow(banc.syns.raw), " rows to proofread pairs...")
  banc.syns <- banc.syns.raw %>%
    dplyr::mutate(pre_root_id = as.character(pre_root_id),
                  post_root_id = as.character(post_root_id)) %>%
    dplyr::rename(X = ctr_x, Y = ctr_y, Z = ctr_z) %>%
    dplyr::filter(pre_root_id %in% proof.ids,
                  post_root_id %in% proof.ids,
                  post_root_id != pre_root_id)
  rm(banc.syns.raw); gc()
  synapses <- TRUE
  message("Loaded synapses from GCS: ", gcs.syn.path,
          " (", nrow(banc.syns), " proofread-pair synapses)")
}

# If the GCS read errored / returned NULL, try the local v626 CSV fallback.
if (!synapses && exists("banc.save.path") && !is.null(banc.save.path)) {
  version.path <- file.path(banc.save.path, "v626")
  local.syn.file <- file.path(version.path, "synapses_250226_human_readable.csv")
  if (file.exists(local.syn.file)) {
    message("Trying local synapse fallback: ", local.syn.file)
    column_names <- c('id', 'pre_x', 'pre_y', 'pre_z', 'post_x', 'post_y', 'post_z',
                      'ctr_x', 'ctr_y', 'ctr_z', 'size', 'pre_supervoxel_id',
                      'pre_root_id', 'post_supervoxel_id', 'post_root_id')
    desired_columns <- c('id', 'size', 'pre_root_id', 'post_root_id', 'ctr_x', 'ctr_y', 'ctr_z')
    banc.syns <- vroom::vroom(local.syn.file,
                              col_names = column_names,
                              col_select = dplyr::all_of(desired_columns),
                              col_types = readr::cols(.default = col_double(),
                                id = col_character(), pre_root_id = col_character(),
                                post_root_id = col_character()),
                              skip = 1) %>%
      dplyr::rename(X = ctr_x, Y = ctr_y, Z = ctr_z) %>%
      dplyr::filter(pre_root_id %in% proof.ids,
                    post_root_id %in% proof.ids,
                    post_root_id != pre_root_id) %>%
      tibble::as_tibble()
    synapses <- TRUE
    message("Loaded synapses from local fallback (", nrow(banc.syns), " rows)")
  }
}
clusters <- sort(na.omit(unique(banc.meta$cns_network)))
.n_cns <- length(clusters)
# CNS network rendering always recalculates because the spectral CSV (which
# repopulates cns_network) can move neurons between networks without changing
# super_cluster/cluster/cell_class. Sections 1, 2, 4 keep their cached PNGs.
# Override with .banc_cns_keep_cache = TRUE to revert to the file.exists skip.
.cns_recalculate <- !(exists(".banc_cns_keep_cache") && .banc_cns_keep_cache)
message(sprintf("=== SECTION 3/4: CNS cluster meshes (%d clusters%s) ===",
                .n_cns, if (.cns_recalculate) ", forced recalculate" else ""))
for(.cnsi in seq_along(clusters)){
  clust <- clusters[.cnsi]
  try({
    message(sprintf("[cns_network %d/%d] %s", .cnsi, .n_cns, clust))
    clust.nam <- gsub(" |\\/","_",clust)

    # Skip only when .cns_recalculate is FALSE (opt-in cache).
    .cns_out <- file.path(banc.fig6.anat.path, paste0(clust.nam, "_brain_neuroanatomy.pdf"))
    if (!.cns_recalculate && file.exists(.cns_out)) {
      message(sprintf("  SKIP (output exists): %s", basename(.cns_out)))
      next
    }

    banc.cns.clust <- banc.meta %>%
      dplyr::filter(cns_network==clust)

    ##### NBLAST clustering
    
    # get neuron skeletons
    if(synapses){
      neurons.plot <- banc.syns %>%
        dplyr::filter(
          pre_root_id %in% !!unique(banc.cns.clust$root_id) |
            post_root_id %in% !!unique(banc.cns.clust$root_id),
          size > 5
        ) %>%
        dplyr::slice_sample(n = 1e5, replace = FALSE) %>%
        nat::xyzmatrix()
    }else{
      l2 <- banc_read_l2skel(unique(banc.cns.clust$root_id), OmitFailures = TRUE)
      e <- nlapply(l2,function(x) xyzmatrix(x)[nat::endpoints(x),])
      neurons.plot <- do.call(rbind,e)
      n_rows <- nrow(neurons.plot)
      sampled_idx <- sample(n_rows, size = floor(n_rows / 10))
      neurons.plot <- neurons.plot[sampled_idx, , drop = FALSE]
    }
    neurons.plot <- neurons.plot[pointsinside(neurons.plot,banc_neuropil.surf),]
    
    # Find neuropil inclusion
    message("making: neurons.np")
    chunk_size <- 10000
    neurons.np <- neurons.plot %>%
      as.data.frame() %>%
      dplyr::mutate(id = row_number()) %>%
      dplyr::mutate(neuropil = NA,
                    region = NA,
                    side = NA,
                    chunk = ceiling(dplyr::row_number() / chunk_size)
                    ) %>%
      dplyr::group_by(chunk) %>%
      dplyr::group_split()
    neurons.np <- purrr::map(neurons.np, function(chunk) {
      message("running: pointsinside_banc")
      data <-tryCatch(pointsinside_banc(chunk), error = function(e) data)
      message("running: pointsnearby_banc")
      data <- tryCatch(pointsnearby_banc(data), error = function(e) data)
      data
    })
    neurons.np.df <- bind_rows(neurons.np) %>%
      dplyr::mutate(neuropil = gsub("ITO_optic_|ITO_midbrain_|COURT_vnc_|_L|_R|_right|_left|MANC_.*|\\,.*","",neuropil)) %>%
      dplyr::mutate(neuropil = gsub("MB_.*","MB",neuropil)) %>%
      dplyr::arrange(region, neuropil)
    
    top_neuropils <- neurons.np.df %>%
      dplyr::count(neuropil, sort = TRUE) %>%
      dplyr::top_n(3, n) %>%
      dplyr::pull(neuropil)
    
    data_plot <- neurons.np.df %>%
      dplyr::mutate(
        neuropil_plot = ifelse(neuropil %in% top_neuropils, neuropil, "other"),
        region_plot   = ifelse(neuropil %in% top_neuropils, region, "other")
      ) %>%
      dplyr::count(neuropil_plot, region_plot) %>%
      dplyr::mutate(percent = n / sum(n) * 100)
    
    g.np <- ggplot2::ggplot(data_plot, ggplot2::aes(x = reorder(neuropil_plot, n), y = percent, fill = region_plot)) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::scale_fill_manual(values = paper.cols) +
      ggplot2::labs(x = "", y = "", fill = "") +
      ggplot2::theme_minimal() +
      theme(legend.position = "none")
    
    # Plot pie chart
    ggsave(plot = g.np,
           filename = file.path(banc.fig6.anat.path,paste0(clust.nam,"_neuropil_bar.pdf")),
           width = 2, height = 2, dpi = 300)
    
    ##### Plot neuroanatomy
    # Uniform light-grey surface (alpha 0.3) instead of the depth-gradient
    # render that reads as a wire-mesh outline. Both `cols` endpoints set
    # to the same value collapses geom_neuron.mesh3d's
    # scale_fill_gradient() to a single fill across all triangles.
    g.anat.main <- g.anat +
      geom_neuron(x = banc_neuropil.surf,
                  cols = c("grey80", "grey80"),
                  rotation_matrix = bancr:::banc_rotation_matrices[["main"]],
                  alpha = 0.3)
    g.anat.brain <- g.anat +
      geom_neuron(x = banc_brain_neuropil.surf,
                  cols = c("grey80", "grey80"),
                  rotation_matrix = bancr:::banc_rotation_matrices[["front"]],
                  alpha = 0.3)
    g.anat.vnc <- g.anat +
      geom_neuron(x = banc_vnc_neuropil.surf,
                  cols = c("grey80", "grey80"),
                  rotation_matrix = bancr:::banc_rotation_matrices[["vnc"]],
                  alpha = 0.3)
    ggsave(plot = g.anat.brain,
           filename = file.path(banc.fig6.anat.path,"empty_brain.png"),
           width = 10, height = 10, dpi = 300)
    ggsave(plot = g.anat.vnc,
           filename = file.path(banc.fig6.anat.path,"empty_vnc.png"),
           width = 10, height = 10, dpi = 300)
    if(!nrow(banc.cns.clust)){
      next
    }
    if(!length(neurons.plot)){
      next
    }
    x <- as.data.frame(nat::xyzmatrix(neurons.plot))
    x <- as.data.frame(t(bancr:::banc_rotation_matrices[["main"]][, 1:3] %*% t(nat::xyzmatrix(x))))
    x <- x[, -4]
    colnames(x) <- c("X", "Y", "Z")
    g.anat.main <- g.anat.main +
      stat_density_2d(data = x,
                      aes(x = X, 
                          y = Y, 
                          fill = after_stat(level)), 
                      n = 100,
                      geom = "polygon", 
                      alpha = 0.5) +
      scale_fill_viridis_c(option = "C") +
      ggplot2::theme_void() +
      ggplot2::guides(fill = "none", color = "none") 
    ggsave(plot = g.anat.main,
           filename = file.path(banc.fig6.anat.path,paste0(clust.nam,"_main_neuroanatomy.pdf")),
           width = 10, height = 10, dpi = 300)
    Sys.sleep(2)
    # --- Decapitate first so we can share a KDE colour scale across brain/vnc.
    neurons.plot.brain <- banc_decapitate(neurons.plot, invert = TRUE,  OmitFailures = TRUE)
    neurons.plot.vnc   <- banc_decapitate(neurons.plot, invert = FALSE, OmitFailures = TRUE)
    .rotated_xy <- function(neurs, mat_key) {
      if (!length(neurs)) return(NULL)
      .x <- as.data.frame(nat::xyzmatrix(neurs))
      .x <- as.data.frame(t(bancr:::banc_rotation_matrices[[mat_key]][, 1:3] %*% t(nat::xyzmatrix(.x))))
      .x <- .x[, -4]
      colnames(.x) <- c("X", "Y", "Z")
      .x
    }
    x_brain <- .rotated_xy(neurons.plot.brain, "front")
    x_vnc   <- .rotated_xy(neurons.plot.vnc,   "vnc")
    # Shared KDE max across brain/vnc so scale_fill_viridis_c is comparable
    # within a CNS network. Compute via MASS::kde2d at the same n as
    # stat_density_2d below; if either dataset is too small, fall back to
    # whichever is available.
    .kde_top <- function(df) {
      if (is.null(df) || nrow(df) < 5) return(NA_real_)
      tryCatch(max(MASS::kde2d(df$X, df$Y, n = 100)$z, na.rm = TRUE),
               error = function(e) NA_real_)
    }
    .kde_shared_max <- suppressWarnings(max(c(.kde_top(x_brain), .kde_top(x_vnc)),
                                            na.rm = TRUE))
    if (!is.finite(.kde_shared_max)) .kde_shared_max <- NA_real_
    if (!is.null(x_brain)) {
      g.anat.brain <- g.anat.brain +
        stat_density_2d(data = x_brain,
                        aes(x = X,
                            y = Y,
                            fill = after_stat(level)),
                        n = 100,
                        geom = "polygon",
                        alpha = 0.5) +
        scale_fill_viridis_c(option = "C",
                             limits = if (is.finite(.kde_shared_max))
                               c(0, .kde_shared_max) else NULL,
                             oob = scales::squish) +
        ggplot2::theme_void() +
        ggplot2::guides(fill = "none", color = "none")
      ggsave(plot = g.anat.brain,
             filename = file.path(banc.fig6.anat.path,paste0(clust.nam,"_brain_neuroanatomy.pdf")),
             width = 10, height = 10, dpi = 300)
      Sys.sleep(2)
    }
    if (!is.null(x_vnc)) {
      g.anat.vnc <- g.anat.vnc +
        stat_density_2d(data = x_vnc,
                        aes(x = X,
                            y = Y,
                            fill = after_stat(level)),
                        n = 100,
                        geom = "polygon",
                        alpha = 0.5) +
        scale_fill_viridis_c(option = "C",
                             limits = if (is.finite(.kde_shared_max))
                               c(0, .kde_shared_max) else NULL,
                             oob = scales::squish) +
        ggplot2::theme_void() +
        ggplot2::guides(fill = "none", color = "none")
      ggsave(plot = g.anat.vnc,
             filename = file.path(banc.fig6.anat.path,paste0(clust.nam,"_vnc_neuroanatomy.pdf")),
             width = 10, height = 10, dpi = 300)
      Sys.sleep(2)
    }
  })
}

###################################################
### PLOT MESHES PER CELL_CLASS — SENSORY + EFF ###
###################################################
# Added 2026-04-09. One image trio (main / brain / vnc) per cell_class for
# sensory and effector neurons. Saves to banc.fig2.anat.path
# (figures/figure_2/links/neuroanatomy). Within each cell_class, neurons are
# coloured by cell_type — every cell_type gets a random hex value sampled
# from paper.cols (one geom_neuron layer per cell_type so the per-cell-type
# colour mapping carries through to the rendered mesh).

dir.create(banc.fig2.anat.path, recursive = TRUE, showWarnings = FALSE)

# Combine sensory + effector meta. distinct() guards against any neuron that
# matches both filters (shouldn't happen but cheap safety).
.sens_eff_meta <- dplyr::bind_rows(
  banc.sens.meta,
  banc.eff.meta
) %>%
  dplyr::distinct(root_id, .keep_all = TRUE) %>%
  dplyr::filter(!is.na(cell_class), cell_class != "")

cell_classes <- na.omit(unique(.sens_eff_meta$cell_class))
.n_cc <- length(cell_classes)
message(sprintf("=== SECTION 4/4: Per-cell_class meshes (%d cell_classes, sensory + effector) to %s ===",
                .n_cc, banc.fig2.anat.path))

# Reproducible random sampling: setting a seed once outside the loop means a
# given cell_type → colour assignment is stable across runs (matters for
# version-controlled figure outputs).
set.seed(42)

# Pool of paper.cols values to sample from. Strip duplicates and any NA / "".
.paper_pool <- unique(unname(paper.cols))
.paper_pool <- .paper_pool[!is.na(.paper_pool) & .paper_pool != ""]

for (.cci in seq_along(cell_classes)) {
  cls <- cell_classes[.cci]
  try({
    message(sprintf("[cell_class %d/%d] %s", .cci, .n_cc, cls))

    # Skip if output already exists and recalculate=FALSE
    .cc_out <- file.path(banc.fig2.anat.path, paste0(gsub(" |\\/", "_", cls), "_main_neuroanatomy.png"))
    if (!recalculate && file.exists(.cc_out)) {
      message(sprintf("  SKIP (output exists): %s", basename(.cc_out)))
      next
    }
    .n_total_cc <- sum(.sens_eff_meta$cell_class == cls, na.rm = TRUE)
    cls.meta <- .sens_eff_meta %>%
      dplyr::filter(cell_class == cls,
                    side %in% c("right", "midline", "center")) %>%
      dplyr::arrange(cell_type)
    .side_coverage_cc <- if (.n_total_cc > 0) nrow(cls.meta) / .n_total_cc else 0
    if (.side_coverage_cc < 0.9) {
      message(sprintf("  WARNING: side coverage for %s is %.0f%% (%d/%d)",
                      cls, .side_coverage_cc * 100, nrow(cls.meta), .n_total_cc))
    }
    if (!nrow(cls.meta)) next

    # Reset the anatomy templates each iteration (otherwise the previous
    # cell_class's neurons remain baked in — same idiom as the existing
    # per-cluster loop above).
    g.anat.main <- g.anat +
      geom_neuron(x = banc_neuropil.surf,
                  cols = c("grey60", "grey30"),
                  rotation_matrix = bancr:::banc_rotation_matrices[["main"]],
                  alpha = 0.1)
    g.anat.brain <- g.anat +
      geom_neuron(x = banc_brain_neuropil.surf,
                  cols = c("grey60", "grey30"),
                  rotation_matrix = bancr:::banc_rotation_matrices[["front"]],
                  alpha = 0.1)
    g.anat.vnc <- g.anat +
      geom_neuron(x = banc_vnc_neuropil.surf,
                  cols = c("grey60", "grey30"),
                  rotation_matrix = bancr:::banc_rotation_matrices[["vnc"]],
                  alpha = 0.1)

    neurons.plot <- banc_read_neuron_meshes(cls.meta$root_id, OmitFailures = TRUE)
    if (!length(neurons.plot)) next

    # Build per-cell_type colour map. Each cell_type within this class gets
    # a random hex from paper.cols. Sampling without replacement when there
    # are enough colours, with replacement when there are more cell_types
    # than colours (rare for sensory/effector classes but possible).
    .ct_present <- na.omit(unique(cls.meta$cell_type[
      cls.meta$root_id %in% names(neurons.plot)
    ]))
    if (length(.ct_present) == 0L) {
      .ct_colors <- character(0)
    } else if (length(.ct_present) <= length(.paper_pool)) {
      .ct_colors <- sample(.paper_pool, length(.ct_present), replace = FALSE)
    } else {
      .ct_colors <- sample(.paper_pool, length(.ct_present), replace = TRUE)
    }
    names(.ct_colors) <- .ct_present

    # ID → cell_type lookup so we can subset neurons.plot per cell_type.
    .id_to_ct <- setNames(cls.meta$cell_type, cls.meta$root_id)

    # Add one geom_neuron layer per cell_type (each with its own colour).
    .add_layers <- function(g, mesh_set, rot_key, alpha_val = 0.5) {
      if (!length(mesh_set)) return(g)
      mesh_ids <- names(mesh_set)
      for (ct in .ct_present) {
        ct_ids <- intersect(mesh_ids, names(.id_to_ct)[.id_to_ct == ct])
        if (length(ct_ids) == 0L) next
        ct_meshes <- mesh_set[ct_ids]
        col <- .ct_colors[[ct]]
        g <- g + geom_neuron(
          x = ct_meshes,
          cols = c(adjust_color_brightness(col, 1.1),
                   adjust_color_brightness(col, 0.9)),
          rotation_matrix = bancr:::banc_rotation_matrices[[rot_key]],
          alpha = alpha_val
        )
      }
      g
    }

    .file_base <- gsub(" |\\/", "_", cls)

    g.anat.main <- .add_layers(g.anat.main, neurons.plot, "main")
    ggsave(plot = g.anat.main,
           filename = file.path(banc.fig2.anat.path,
                                paste0(.file_base, "_main_neuroanatomy.png")),
           width = 10, height = 10, dpi = 300)

    neurons.plot.brain <- banc_decapitate(neurons.plot, invert = TRUE, OmitFailures = TRUE)
    if (length(neurons.plot.brain)) {
      g.anat.brain <- .add_layers(g.anat.brain, neurons.plot.brain, "front")
      ggsave(plot = g.anat.brain,
             filename = file.path(banc.fig2.anat.path,
                                  paste0(.file_base, "_brain_neuroanatomy.png")),
             width = 10, height = 10, dpi = 300)
    }

    neurons.plot.vnc <- banc_decapitate(neurons.plot, invert = FALSE, OmitFailures = TRUE)
    if (length(neurons.plot.vnc)) {
      g.anat.vnc <- .add_layers(g.anat.vnc, neurons.plot.vnc, "vnc")
      ggsave(plot = g.anat.vnc,
             filename = file.path(banc.fig2.anat.path,
                                  paste0(.file_base, "_vnc_neuroanatomy.png")),
             width = 10, height = 10, dpi = 300)
    }
  })
}

# ###########################
# ### MAKE SKELETON PLOTS ###
# ###########################
# 
# # Make interpretable plots
# banc_interpret_umaps(
#   umap.df = umap.dn.df,
#   elist.pre = NULL,
#   elist.post = NULL,
#   influence.df = NULL,
#   identifier = "",
#   neuroanatomy = TRUE,
#   umaps = TRUE,
#   banc.meta  = banc.meta,
#   save.path = banc.fig3.extra.path
# )
# 
# # Make interpretable plots
# banc_interpret_umaps(
#   umap.df = umap.eff.df,
#   elist.pre = NULL,
#   elist.post = NULL,
#   influence.df = NULL,
#   identifier = "",
#   neuroanatomy = TRUE,
#   umaps = TRUE,
#   banc.meta  = banc.meta,
#   save.path = banc.fig2.extra.path
# )
