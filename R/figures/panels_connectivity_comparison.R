#' panels_connectivity_comparison.R — edge-recapture vs FAFB / MANC / maleCNS (ED Fig. 1)
#'
#' For each (other-dataset, synapse-count threshold) pair, computes the
#' BANC-to-other density-recovery ratio per cell-type-pair (capped at 1)
#' and the cell-type presence/absence hit rate. Anchors the cross-dataset
#' validation columns of ED Fig. 1.
#'
#' @section Reads:
#'   * banc.meta, banc.edgelist.simple        via R/startup/banc-{meta,edgelist}.R
#'   * franken.meta, franken.edgelist         via R/startup/franken-{meta,edgelist}.R
#'   * Unfiltered BANC edgelist
#'     (data/cache/<banc.gcs.dataset>_edgelist_simple.feather) — re-loaded
#'     here because banc-edgelist.R applies a count >= 3 + proofread
#'     prefilter for memory, and this panel needs the low-threshold curves
#'     to slope correctly.
#'
#' @section Writes:
#'   * figures/figure_1/links/supplement/{fafb,manc}_edges_in_banc.pdf
#'   * figures/figure_1/links/supplement/{fafb,manc}_edges_in_banc_by_cell_type.pdf
#'   * figures/figure_1/links/extra/{fafb,manc,malecns}_edges_in_banc_member_weighted.pdf
#'
#' @section Paper:
#'   * ED Fig. 1f–h — edge-recapture and cell-type hit-rate panels.
#'   * Methods §"Cell type matching and annotation".
#'
#' @section Notes:
#'   The "Option C" variant (capped-at-1 density ratio, weighted by
#'   other-dataset neuron-pair count) is computed alongside the
#'   presence/absence hit rate and saved to extra/ for side-by-side
#'   comparison in supplement decisions.
#'
#' @section Reproduce: BANC_NCORES=1 Rscript R/figures/panels_connectivity_comparison.R

source("R/startup/banc-startup.R")
source("R/startup/franken-meta.R")
source("R/startup/banc-meta.R")
source("R/startup/banc-edgelist.R")
source("R/startup/franken-edgelist.R")
source("R/startup/gcs-helpers.R")

# IMPORTANT: this panel needs the UNFILTERED BANC edgelist so the BANC
# threshold = 1 and = 2 curves work correctly. banc-edgelist.R applies a
# global count >= 3 prefilter AND a proofread filter for memory; we override
# both here by reloading the raw cached feather. resubmission_2 had neither
# filter and produced smooth hit-rate curves. Other panels keep the filtered
# version.
.unfiltered_edgelist_cache <- file.path("data", "cache",
                                         paste0(banc.gcs.dataset, "_edgelist_simple.feather"))
if (file.exists(.unfiltered_edgelist_cache)) {
  message("Reloading UNFILTERED BANC edgelist from cache for connectivity comparison: ",
          .unfiltered_edgelist_cache)
  banc.edgelist.simple <- arrow::read_feather(.unfiltered_edgelist_cache) %>%
    dplyr::mutate(pre = as.character(pre), post = as.character(post))
  # Re-attach pre/post metadata (lost when reloading from raw cache).
  # No count or proofread filters applied — match resubmission_2 behaviour.
  banc.edgelist.simple <- banc.edgelist.simple %>%
    dplyr::left_join(banc.meta.post %>%
                       dplyr::select(post_id, post_super_class, post_cell_class,
                                     post_cell_sub_class, post_cell_type,
                                     post_cell_sub_type, post_side) %>%
                       dplyr::distinct(post_id, .keep_all = TRUE),
                     by = c("post" = "post_id")) %>%
    dplyr::left_join(banc.meta.pre %>%
                       dplyr::select(pre_id, pre_super_class, pre_cell_class,
                                     pre_cell_sub_class, pre_cell_type,
                                     pre_cell_sub_type, pre_side) %>%
                       dplyr::distinct(pre_id, .keep_all = TRUE),
                     by = c("pre" = "pre_id"))
  message(sprintf("Unfiltered edgelist for this panel: %d connections",
                  nrow(banc.edgelist.simple)))
} else {
  warning("Unfiltered edgelist cache not found at ", .unfiltered_edgelist_cache,
          " — BANC threshold = 1 curves will be inaccurate (still pre-filtered to count >= 3)")
}
rm(.unfiltered_edgelist_cache)

# Load maleCNS data from GCS
malecns.gcs.path <- "gs://lee-lab_brain-and-nerve-cord-fly-connectome/compiled_data/malecns_09"
malecns.meta <- read_feather_gcs(file.path(malecns.gcs.path, "malecns_09_meta.feather"))
malecns.edgelist.simple <- read_feather_gcs(file.path(malecns.gcs.path, "malecns_09_simple_edgelist.feather"))

# # Read meta
# fw.meta <- suppressWarnings(readr::read_csv(file = file.path(banc.meta.save.path,"flywire_meta.csv"), 
#                                             col_types = hemibrainr:::sql_col_types))
# mc.meta <- suppressWarnings(readr::read_csv(file = file.path(banc.meta.save.path,"manc_meta.csv"), 
#                                             col_types = hemibrainr:::sql_col_types))
fw.meta <- franken.meta %>%
  dplyr::filter(!is.na(fafb_id))
mc.meta <- franken.meta %>%
  dplyr::filter(!is.na(manc_id))

# --- Fast helper: compute hit rates for all (other × banc) threshold combinations ---
# Pre-computes per-threshold groups ONCE per threshold (not per combination), then
# joins them. Uses data.table for ~10-100x speedup over the dplyr group_by/summarise
# loop. Takes named "other" data (FAFB / MANC / maleCNS) and BANC data.
compute_hit_rate_table <- function(other_edges, banc_edges,
                                    thresholds_other, thresholds_banc) {
  suppressMessages(library(data.table))
  other_dt <- data.table::as.data.table(other_edges)[
    , .(pre_cell_type, post_cell_type, pre, post, count)]
  banc_dt  <- data.table::as.data.table(banc_edges)[
    , .(pre_cell_type, post_cell_type, pre, post, count)]

  # Pre-compute grouped stats for each "other" threshold
  message(sprintf("  Pre-computing groups for %d 'other' thresholds...", length(thresholds_other)))
  other_groups_by_t <- lapply(thresholds_other, function(t) {
    other_dt[count >= t,
             .(n_links = .N,
               denom   = data.table::uniqueN(post) * data.table::uniqueN(pre)),
             by = .(pre_cell_type, post_cell_type)]
  })
  names(other_groups_by_t) <- as.character(thresholds_other)

  # Pre-compute grouped stats for each BANC threshold
  message(sprintf("  Pre-computing groups for %d BANC thresholds...", length(thresholds_banc)))
  banc_groups_by_t <- lapply(thresholds_banc, function(t) {
    banc_dt[count >= t,
            .(n_links = .N,
              denom   = data.table::uniqueN(post) * data.table::uniqueN(pre)),
            by = .(pre_cell_type, post_cell_type)]
  })
  names(banc_groups_by_t) <- as.character(thresholds_banc)

  # Combine: for each (other_t, banc_t) join the pre-computed pieces
  results <- expand.grid(other_threshold = thresholds_other,
                         banc_threshold = thresholds_banc)
  results$hit_rate              <- NA_real_
  results$ct_hit_rate           <- NA_real_
  results$weighted_hit_rate     <- NA_real_  # Option C: proper weighted mean

  for (i in seq_len(nrow(results))) {
    ot <- results$other_threshold[i]
    bt <- results$banc_threshold[i]
    other_g <- other_groups_by_t[[as.character(ot)]]
    banc_g  <- banc_groups_by_t[[as.character(bt)]]
    if (nrow(other_g) == 0) {
      results$hit_rate[i]          <- 0
      results$ct_hit_rate[i]       <- 0
      results$weighted_hit_rate[i] <- 0
      next
    }
    # Left join: every other cell-type pair, with matching BANC counts (NA if missing)
    res <- merge(
      other_g[, .(pre_cell_type, post_cell_type,
                  n_links_other = n_links, other_denom = denom)],
      banc_g[, .(pre_cell_type, post_cell_type,
                 n_links_banc = n_links, banc_denom = denom)],
      by = c("pre_cell_type", "post_cell_type"),
      all.x = TRUE
    )
    res[is.na(n_links_banc), n_links_banc := 0]
    res[is.na(banc_denom), banc_denom := NA_real_]

    # Original "weighted by member count" metric — kept for backward
    # compatibility with the supplement panel.
    f <- (res$n_links_banc / res$banc_denom) /
         (res$n_links_other / res$other_denom)
    f <- f * res$other_denom
    f[is.na(f)] <- 0
    f[f > 1] <- 1
    results$hit_rate[i] <- mean(f)

    # Cell-type presence/absence hit rate (unchanged)
    results$ct_hit_rate[i] <- mean(res$n_links_banc > 0)

    # Option C: density ratio capped at 1, then weighted mean by FAFB/MANC
    # neuron-pair count. This is a principled member-weighted recovery —
    # values are bounded in [0, 1] and interpretable as "mean fraction of
    # FAFB/MANC density recovered in BANC, weighted by FAFB/MANC neuron-pair
    # count". Saved to extra/ for side-by-side comparison with `hit_rate`.
    ratio <- (res$n_links_banc / res$banc_denom) /
             (res$n_links_other / res$other_denom)
    ratio[!is.finite(ratio)] <- 0
    ratio <- pmin(ratio, 1)
    w <- res$other_denom
    w[!is.finite(w)] <- 0
    if (sum(w) > 0) {
      results$weighted_hit_rate[i] <- sum(ratio * w) / sum(w)
    } else {
      results$weighted_hit_rate[i] <- 0
    }
  }
  results
}

##############################################
## MISSING CONNECTIVITY ANALYSIS W.R.T FAFB ##
##############################################

fw.filtered <- fw.meta %>%
  dplyr::group_by(cell_type) %>%
  dplyr::filter(all(c("left", "right") %in% side)) %>%
  dplyr::ungroup()

banc.filtered <- banc.meta %>%
  dplyr::group_by(cell_type) %>%
  dplyr::filter(all(c("left", "right") %in% side)) %>%
  dplyr::ungroup()

common_cell_types <- na.omit(intersect(unique(fw.filtered$cell_type), unique(banc.filtered$cell_type)))
common_cell_types <- setdiff(common_cell_types,c("NA",""))

fafb_edges <- dplyr::filter(franken.edgelist.simple,
                             pre_cell_type %in% common_cell_types,
                             post_cell_type %in% common_cell_types) %>%
  dplyr::mutate(pre_cell_type=gsub("_.*","",pre_cell_type),
                post_cell_type=gsub("_.*","",post_cell_type))
banc_edges <- dplyr::filter(banc.edgelist.simple,
                            pre_cell_type %in% common_cell_types,
                            post_cell_type %in% common_cell_types) %>%
  dplyr::mutate(pre_cell_type=gsub("_.*","",pre_cell_type),
                post_cell_type=gsub("_.*","",post_cell_type))

# Make a vector of thresholds
thresholds_fafb <- 1:50
thresholds_banc <- c(1,3,5,10)

message("Computing FAFB hit rates (data.table fast path)...")
results <- compute_hit_rate_table(other_edges = fafb_edges,
                                   banc_edges = banc_edges,
                                   thresholds_other = thresholds_fafb,
                                   thresholds_banc = thresholds_banc)
results$fafb_threshold <- results$other_threshold

# Plot: one curve per BANC threshold, viridis color
g <- ggplot2::ggplot(results, ggplot2::aes(x = fafb_threshold, y = hit_rate, color = factor(banc_threshold))) +
  ggplot2::geom_line(size = 1.1) +
  ggplot2::scale_color_manual(values = c(`1`=adjust_color_brightness(paper.cols[["fafb"]],1),
                                         `3`=adjust_color_brightness(paper.cols[["fafb"]],0.8),
                                         `5`=adjust_color_brightness(paper.cols[["fafb"]],0.6),
                                         `10`=adjust_color_brightness(paper.cols[["fafb"]],0.4)),
                              name = "BANC min synapses") +
   ggplot2::labs(
    # title = expression(
    #   paste("normalised P(connection in BANC" >= T[BANC], 
    #         " | connection in FAFB" >= T[FAFB], 
    #         ")")
    # )
    x = "minimum synapses in FAFB",
    y = "proportion of FAFB edges found in BANC \n(cell-type-to-cell-type weighted by number of members of pre and post cell type)",
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "none") +
  ggplot2::ylim(c(0,1))
ggsave(plot = g, 
       filename = file.path(banc.fig1.supp.path,"fafb_edges_in_banc.pdf"), 
       width = 6, height =3, dpi = 300)

# Make plot
g <- ggplot2::ggplot(results, ggplot2::aes(x = fafb_threshold, y = ct_hit_rate, color = factor(banc_threshold))) +
  ggplot2::geom_line(size = 1.1) +
  ggplot2::scale_color_manual(values = c(`1`=adjust_color_brightness(paper.cols[["fafb"]],1),
                                         `3`=adjust_color_brightness(paper.cols[["fafb"]],0.8),
                                         `5`=adjust_color_brightness(paper.cols[["fafb"]],0.6),
                                         `10`=adjust_color_brightness(paper.cols[["fafb"]],0.4)), 
                              name = "BANC min synapses") +
  ggplot2::labs(
    # title = expression(
    #   paste("normalised P(connection in BANC" >= T[BANC], 
    #         " | connection in FAFB" >= T[FAFB], 
    #         ")"),
    x = "minimum synapses in FAFB",
    y = "proportion of FAFB edges found in BANC \n(cell-type-to-cell-type)"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "none")+
  ggplot2::ylim(c(0,1))

# Save
ggsave(plot = g,
       filename = file.path(banc.fig1.supp.path,"fafb_edges_in_banc_by_cell_type.pdf"),
       width = 6, height =3, dpi = 300)

# --- Option C plot: density ratio capped at 1, weighted mean by FAFB neuron-pair count ---
g <- ggplot2::ggplot(results, ggplot2::aes(x = fafb_threshold, y = weighted_hit_rate, color = factor(banc_threshold))) +
  ggplot2::geom_line(size = 1.1) +
  ggplot2::scale_color_manual(values = c(`1`=adjust_color_brightness(paper.cols[["fafb"]],1),
                                         `3`=adjust_color_brightness(paper.cols[["fafb"]],0.8),
                                         `5`=adjust_color_brightness(paper.cols[["fafb"]],0.6),
                                         `10`=adjust_color_brightness(paper.cols[["fafb"]],0.4)),
                              name = "BANC min synapses") +
  ggplot2::labs(
    x = "minimum synapses in FAFB",
    y = "mean BANC-to-FAFB density ratio (capped at 1)\nweighted by FAFB neuron-pair count"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "none") +
  ggplot2::ylim(c(0,1))
ggsave(plot = g,
       filename = file.path(banc.fig1.extra.path, "fafb_edges_in_banc_member_weighted.pdf"),
       width = 6, height = 3, dpi = 300)


##############################################
## MISSING CONNECTIVITY ANALYSIS W.R.T MANC ##
##############################################

mc.filtered <- mc.meta %>%
  dplyr::group_by(cell_type) %>%
  dplyr::filter(all(c("left", "right") %in% side)) %>%
  ungroup()

banc.filtered <- banc.meta %>%
  dplyr::group_by(cell_type) %>%
  dplyr::filter(all(c("left", "right") %in% side)) %>%
  ungroup()

common_cell_types <- na.omit(intersect(unique(mc.filtered$cell_type), unique(banc.filtered$cell_type)))
common_cell_types <- setdiff(common_cell_types,c("NA",""))

manc_edges <- dplyr::filter(franken.edgelist.simple,
                            pre_cell_type %in% common_cell_types,
                            post_cell_type %in% common_cell_types) %>%
  dplyr::mutate(pre_cell_type=gsub("_.*","",pre_cell_type),
                post_cell_type=gsub("_.*","",post_cell_type))
banc_edges <- dplyr::filter(banc.edgelist.simple,
                            pre_cell_type %in% common_cell_types,
                            post_cell_type %in% common_cell_types) %>%
  dplyr::mutate(pre_cell_type=gsub("_.*","",pre_cell_type),
                post_cell_type=gsub("_.*","",post_cell_type))

# Make a vector of thresholds
thresholds_manc <- 1:50
thresholds_banc <- c(1,3,5,10)

message("Computing MANC hit rates (data.table fast path)...")
results.mc <- compute_hit_rate_table(other_edges = manc_edges,
                                      banc_edges = banc_edges,
                                      thresholds_other = thresholds_manc,
                                      thresholds_banc = thresholds_banc)
results.mc$manc_threshold <- results.mc$other_threshold

# Plot: one curve per BANC threshold, viridis color
g <- ggplot2::ggplot(results.mc, ggplot2::aes(x = manc_threshold, y = hit_rate, color = factor(banc_threshold))) +
  ggplot2::geom_line(size = 1.1) +
  ggplot2::scale_color_manual(values = c(`1`=adjust_color_brightness(paper.cols[["manc"]],1),
                                         `3`=adjust_color_brightness(paper.cols[["manc"]],0.8),
                                         `5`=adjust_color_brightness(paper.cols[["manc"]],0.6),
                                         `10`=adjust_color_brightness(paper.cols[["manc"]],0.4)), 
                              name = "BANC min synapses") +  
  ggplot2::labs(
    # title = expression(
    #   paste("normalised P(connection in BANC" >= T[BANC], 
    #         " | connection in MANC" >= T[manc], 
    #         ")")
    # )
    x = "minimum synapses in MANC",
    y = "proportion of MANC edges found in BANC \n(cell-type-to-cell-type weighted by number of members of pre and post cell type)",
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "none") +
  ggplot2::ylim(c(0,1))
ggsave(plot = g, 
       filename = file.path(banc.fig1.supp.path,"manc_edges_in_banc.pdf"), 
       width = 6, height = 3, dpi = 300)

# Plot: one curve per BANC threshold, viridis color
g <- ggplot2::ggplot(results.mc, ggplot2::aes(x = manc_threshold, y = ct_hit_rate, color = factor(banc_threshold))) +
  ggplot2::geom_line(size = 1.1) +
  ggplot2::scale_color_manual(values = c(`1`=adjust_color_brightness(paper.cols[["manc"]],1),
                                         `3`=adjust_color_brightness(paper.cols[["manc"]],0.8),
                                         `5`=adjust_color_brightness(paper.cols[["manc"]],0.6),
                                         `10`=adjust_color_brightness(paper.cols[["manc"]],0.4)), 
                              name = "BANC min synapses") +   
  ggplot2::labs(
    # title = expression(
    #   paste("Normalised P(connection in BANC" >= T[BANC], 
    #         " | connection in manc" >= T[manc], 
    #         ")")
    # ),
    x = "minimum synapses in MANC",
    y = "proportion of MANC edges found in BANC (cell-type-to-cell-type)",
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "none") +
  ggplot2::ylim(c(0,1))
ggsave(plot = g,
       filename = file.path(banc.fig1.supp.path,"manc_edges_in_banc_by_cell_type.pdf"),
       width = 6, height = 3, dpi = 300)

# --- Option C plot: density ratio capped at 1, weighted mean by MANC neuron-pair count ---
g <- ggplot2::ggplot(results.mc, ggplot2::aes(x = manc_threshold, y = weighted_hit_rate, color = factor(banc_threshold))) +
  ggplot2::geom_line(size = 1.1) +
  ggplot2::scale_color_manual(values = c(`1`=adjust_color_brightness(paper.cols[["manc"]],1),
                                         `3`=adjust_color_brightness(paper.cols[["manc"]],0.8),
                                         `5`=adjust_color_brightness(paper.cols[["manc"]],0.6),
                                         `10`=adjust_color_brightness(paper.cols[["manc"]],0.4)),
                              name = "BANC min synapses") +
  ggplot2::labs(
    x = "minimum synapses in MANC",
    y = "mean BANC-to-MANC density ratio (capped at 1)\nweighted by MANC neuron-pair count"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "none") +
  ggplot2::ylim(c(0,1))
ggsave(plot = g,
       filename = file.path(banc.fig1.extra.path, "manc_edges_in_banc_member_weighted.pdf"),
       width = 6, height = 3, dpi = 300)

###################################################
## MISSING CONNECTIVITY ANALYSIS W.R.T maleCNS   ##
###################################################

# Add cell type annotations to maleCNS edgelist
message("Annotating maleCNS edgelist with cell types...")
malecns.ct.lookup <- malecns.meta %>%
  dplyr::select(malecns_09_id, cell_type) %>%
  dplyr::filter(!is.na(cell_type), cell_type != "", cell_type != "NA") %>%
  dplyr::distinct(malecns_09_id, .keep_all = TRUE)

malecns_edges <- malecns.edgelist.simple %>%
  dplyr::inner_join(malecns.ct.lookup, by = c("pre" = "malecns_09_id")) %>%
  dplyr::rename(pre_cell_type = cell_type) %>%
  dplyr::inner_join(malecns.ct.lookup, by = c("post" = "malecns_09_id")) %>%
  dplyr::rename(post_cell_type = cell_type)

mcns.filtered <- malecns.meta %>%
  dplyr::filter(!is.na(cell_type), cell_type != "", cell_type != "NA") %>%
  dplyr::group_by(cell_type) %>%
  dplyr::filter(all(c("left", "right") %in% side)) %>%
  dplyr::ungroup()

banc.filtered <- banc.meta %>%
  dplyr::group_by(cell_type) %>%
  dplyr::filter(all(c("left", "right") %in% side)) %>%
  dplyr::ungroup()

common_cell_types <- na.omit(intersect(unique(mcns.filtered$cell_type), unique(banc.filtered$cell_type)))
common_cell_types <- setdiff(common_cell_types, c("NA", ""))

malecns_edges <- dplyr::filter(malecns_edges,
                                pre_cell_type %in% common_cell_types,
                                post_cell_type %in% common_cell_types) %>%
  dplyr::mutate(pre_cell_type = gsub("_.*", "", pre_cell_type),
                post_cell_type = gsub("_.*", "", post_cell_type))
banc_edges <- dplyr::filter(banc.edgelist.simple,
                            pre_cell_type %in% common_cell_types,
                            post_cell_type %in% common_cell_types) %>%
  dplyr::mutate(pre_cell_type = gsub("_.*", "", pre_cell_type),
                post_cell_type = gsub("_.*", "", post_cell_type))

thresholds_malecns <- 1:50
thresholds_banc <- c(1, 3, 5, 10)

message("Computing maleCNS hit rates (data.table fast path)...")
results.mcns <- compute_hit_rate_table(other_edges = malecns_edges,
                                        banc_edges = banc_edges,
                                        thresholds_other = thresholds_malecns,
                                        thresholds_banc = thresholds_banc)
results.mcns$malecns_threshold <- results.mcns$other_threshold

# Plot: normalised hit rate
g <- ggplot2::ggplot(results.mcns, ggplot2::aes(x = malecns_threshold, y = hit_rate, color = factor(banc_threshold))) +
  ggplot2::geom_line(size = 1.1) +
  ggplot2::scale_color_manual(values = c(`1` = adjust_color_brightness(paper.cols[["malecns"]], 1),
                                         `3` = adjust_color_brightness(paper.cols[["malecns"]], 0.8),
                                         `5` = adjust_color_brightness(paper.cols[["malecns"]], 0.6),
                                         `10` = adjust_color_brightness(paper.cols[["malecns"]], 0.4)),
                              name = "BANC min synapses") +
  ggplot2::labs(
    x = "minimum synapses in maleCNS",
    y = "proportion of maleCNS edges found in BANC \n(cell-type-to-cell-type weighted by number of members of pre and post cell type)",
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "none") +
  ggplot2::ylim(c(0, 1))
ggsave(plot = g,
       filename = file.path(banc.fig1.extra.path, "malecns_edges_in_banc.pdf"),
       width = 6, height = 3, dpi = 300)

# Plot: cell-type hit rate
g <- ggplot2::ggplot(results.mcns, ggplot2::aes(x = malecns_threshold, y = ct_hit_rate, color = factor(banc_threshold))) +
  ggplot2::geom_line(size = 1.1) +
  ggplot2::scale_color_manual(values = c(`1` = adjust_color_brightness(paper.cols[["malecns"]], 1),
                                         `3` = adjust_color_brightness(paper.cols[["malecns"]], 0.8),
                                         `5` = adjust_color_brightness(paper.cols[["malecns"]], 0.6),
                                         `10` = adjust_color_brightness(paper.cols[["malecns"]], 0.4)),
                              name = "BANC min synapses") +
  ggplot2::labs(
    x = "minimum synapses in maleCNS",
    y = "proportion of maleCNS edges found in BANC \n(cell-type-to-cell-type)",
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "none") +
  ggplot2::ylim(c(0, 1))
ggsave(plot = g,
       filename = file.path(banc.fig1.extra.path, "malecns_edges_in_banc_by_cell_type.pdf"),
       width = 6, height = 3, dpi = 300)

# --- Option C plot: density ratio capped at 1, weighted mean by maleCNS neuron-pair count ---
g <- ggplot2::ggplot(results.mcns, ggplot2::aes(x = malecns_threshold, y = weighted_hit_rate, color = factor(banc_threshold))) +
  ggplot2::geom_line(size = 1.1) +
  ggplot2::scale_color_manual(values = c(`1` = adjust_color_brightness(paper.cols[["malecns"]], 1),
                                         `3` = adjust_color_brightness(paper.cols[["malecns"]], 0.8),
                                         `5` = adjust_color_brightness(paper.cols[["malecns"]], 0.6),
                                         `10` = adjust_color_brightness(paper.cols[["malecns"]], 0.4)),
                              name = "BANC min synapses") +
  ggplot2::labs(
    x = "minimum synapses in maleCNS",
    y = "mean BANC-to-maleCNS density ratio (capped at 1)\nweighted by maleCNS neuron-pair count"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "none") +
  ggplot2::ylim(c(0, 1))
ggsave(plot = g,
       filename = file.path(banc.fig1.extra.path, "malecns_edges_in_banc_member_weighted.pdf"),
       width = 6, height = 3, dpi = 300)

###################################
## CONNECTIVITY SCATTER ANALYSIS ##
###################################

# Map BANC cell types to fafb_cell_type for matching against franken — used in
# the scatter section only (not in the hit-rate sections, where plain cell_type
# matching matches resubmission_2 and gives more cell-type pairs).
banc_fafb_ct_map <- banc.meta %>%
  dplyr::distinct(root_id, .keep_all = TRUE) %>%
  dplyr::transmute(id = root_id,
                   fafb_ct = dplyr::case_when(
                     !is.na(fafb_cell_type) & fafb_cell_type != "" ~ fafb_cell_type,
                     TRUE ~ cell_type))

# Generate scatter plots comparing connectivity strengths across datasets
franken.chosen.cts <- unique(c(subset(franken.meta,grepl("neck",region))$cell_type,
                               subset(franken.meta,grepl("neck",region))$fafb_cell_type,
                               subset(franken.meta,grepl("neck",region))$manc_cell_type))
bc.fafb.cts <- unique(fw.meta$cell_type)
bc.manc.cts <- unique(mc.meta$cell_type)

franken.edgelist.simple.ct <- franken.edgelist.simple %>%
  dplyr::group_by(pre_cell_type, post_cell_type) %>%
  dplyr::group_by(pre_cell_type, post_cell_type) %>%
  dplyr::mutate(franken_count_mean = mean(count),
                franken_norm_mean = mean(norm)) %>%
  dplyr::distinct(pre_cell_type, post_cell_type,franken_count_mean,franken_norm_mean) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(connection = paste0(pre_cell_type,"_",post_cell_type)) %>%
  dplyr::ungroup()

# Map BANC cell types to fafb_cell_type for matching against franken
banc.edgelist.simple.ct <- banc.edgelist.simple %>%
  dplyr::left_join(banc_fafb_ct_map, by = c("pre" = "id")) %>%
  dplyr::rename(pre_fafb_ct = fafb_ct) %>%
  dplyr::left_join(banc_fafb_ct_map, by = c("post" = "id")) %>%
  dplyr::rename(post_fafb_ct = fafb_ct) %>%
  dplyr::mutate(pre_cell_type = dplyr::coalesce(pre_fafb_ct, pre_cell_type),
                post_cell_type = dplyr::coalesce(post_fafb_ct, post_cell_type)) %>%
  dplyr::group_by(pre_cell_type, post_cell_type) %>%
  dplyr::mutate(banc_count_mean = mean(count),
                banc_norm_mean = mean(norm)) %>%
  dplyr::distinct(pre_cell_type, post_cell_type,banc_count_mean,banc_norm_mean) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(connection = paste0(pre_cell_type,"_",post_cell_type)) %>%
  dplyr::ungroup()

el.chosen.ct <- dplyr::left_join(
  franken.edgelist.simple.ct,
  banc.edgelist.simple.ct %>%
    dplyr::select(banc_count_mean,banc_norm_mean,connection),
  by = "connection"
) %>%
  dplyr::filter(!is.na(banc_count_mean)) %>%
  dplyr::distinct(connection, .keep_all = TRUE)

el.chosen.ct.downstream <- el.chosen.ct %>%
  dplyr::filter(! post_cell_type %in% franken.chosen.cts) %>%
  dplyr::mutate(dataset = dplyr::case_when(
    post_cell_type %in% bc.fafb.cts ~ "FAFB",
    post_cell_type %in% bc.manc.cts ~ "MANC",
    TRUE ~ NA
  )) %>%
  dplyr::filter(!is.na(dataset)) %>%
  dplyr::mutate(direction = "downstream")

el.chosen.ct.upstream <- el.chosen.ct %>%
  dplyr::filter(! post_cell_type %in% franken.chosen.cts) %>%
  dplyr::mutate(dataset = dplyr::case_when(
    pre_cell_type %in% bc.fafb.cts ~ "FAFB",
    pre_cell_type %in% bc.manc.cts ~ "MANC",
    TRUE ~ NA
  )) %>%
  dplyr::filter(!is.na(dataset)) %>%
  dplyr::mutate(direction = "upstream")

el.chosen.ct.updown <- rbind(el.chosen.ct.downstream,
                             el.chosen.ct.upstream) %>%
  dplyr::filter(banc_count_mean >= 10, 
                franken_count_mean >= 10) %>%
  dplyr::mutate(dataset = factor(dataset, levels = c("FAFB","MANC")))

# Create the scatter plot
g1 <- ggplot(el.chosen.ct.updown, aes(x = banc_count_mean, 
                                      y = franken_count_mean,
                                      color = dataset)) +
  geom_point(alpha = 0.1) + 
  geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed", linewidth = 1) +  # Add a linear regression line
  #geom_text_repel(aes(label = connection), size = 2, max.overlaps = 5) +  # Add labels for points
  scale_x_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  scale_y_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  stat_poly_eq(
    aes(label = paste(after_stat(eq.label), after_stat(rr.label), sep = "~~~")),
    formula = y ~ x, 
    parse = TRUE,
    label.x = "left",
    label.y = "top",
    size = 6,
    color = "black"
  ) +
  facet_wrap(~ dataset) +  # Create facets based on dataset
  labs(
    title = "",
    subtitle = "",
    x = "BANC connetion count (log10 scale)",
    y = "comparison connection count (log10 scale)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.title = element_text(size = 20, color = "black"),
    axis.text = element_text(size = 18, color = "black"),
    strip.text = element_text(size = 20, color = "black"),
    strip.background = element_rect(fill = "white", color = NA)
  )  +
  scale_color_manual(values=paper.cols)  +
  coord_fixed()

# Save the plot 
ggsave(plot = g1, 
       filename = file.path(banc.fig1.extra.path,"franken_vs_banc_count_all.pdf"), 
       width = 8, height = 8, dpi = 300)
ggsave(plot = convert_to_dark_mode(g1), 
       filename = file.path(banc.fig1.darkmode.path, "dark_mode_franken_vs_banc_count_all.pdf"), 
       width = 8, height = 8, dpi = 300)

# Create the scatter plot
g2 <- ggplot(el.chosen.ct.updown, aes(x = banc_norm_mean, 
                                      y = franken_norm_mean,
                                      color = dataset)) +
  geom_point(alpha = 0.1) +  # Add points with some transparency
  geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed", linewidth = 1) + 
  #geom_text_repel(aes(label = connection), size = 2, max.overlaps = 5) +  # Add labels for points
  scale_x_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  scale_y_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  stat_poly_eq(
    aes(label = paste(after_stat(eq.label), after_stat(rr.label), sep = "~~~")),
    formula = y ~ x, 
    parse = TRUE,
    label.x = "left",
    label.y = "top",
    size = 6,
    fontface = "bold",
    color = "black"
  ) +
  facet_wrap( ~ dataset) +  
  labs(
    title = "",
    subtitle = "",
    x = "BANC norm. connection (log10 scale)",
    y = "norm. connection (log10 scale)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.title = element_text(size = 20, color = "black"),
    axis.text = element_text(size = 18, color = "black"),
    strip.text = element_text(size = 20, color = "black"),
    strip.background = element_rect(fill = "white", color = NA)
  )  +
  scale_color_manual(values=paper.cols) +
  coord_fixed()

# Save the plot 
ggsave(plot = g2, 
       filename = file.path(banc.fig1.supp.path,"franken_vs_banc_norm_all.pdf"), 
       width = 8, height = 6, dpi = 300)
ggsave(plot = convert_to_dark_mode(g2), 
       filename = file.path(banc.fig1.darkmode.path, "dark_mode_franken_vs_banc_norm_all.pdf"), 
       width = 8, height = 6, dpi = 300)

################
### Outliers ###
################

# Analysis of outliers
outliers <- el.chosen.ct.updown %>%
  dplyr::mutate(norm_diff = banc_norm_mean-franken_norm_mean,
                count_diff = banc_count_mean-franken_count_mean) %>%
  dplyr::arrange(dplyr::desc(norm_diff))

# Create histogram for norm_diff
p1 <- ggplot(outliers, aes(x = norm_diff)) +
  geom_histogram(binwidth = 0.005, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Norm Difference",
       x = "Norm Difference",
       y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Create histogram for count_diff
p2 <- ggplot(outliers, aes(x = count_diff)) +
  geom_histogram(binwidth = 50, fill = "red", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Count Difference",
       x = "Count Difference",
       y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Arrange both plots side by side
g3 <- gridExtra::grid.arrange(p1, p2, ncol = 2)
ggsave(filename = file.path(banc.fig1.extra.path,"franken_vs_banc_diff_andn.pdf"), 
       plot = gridExtra::arrangeGrob(p1, p2, ncol = 2), 
       width = 12, height = 6, dpi = 300)

# What pairs are in the largest norm diff?
largest.outliers <- outliers %>%
  dplyr::filter(norm_diff >= quantile(outliers$norm_diff,0.95)) %>%
  dplyr::left_join(franken.meta[,c("super_class","cell_type")], by = c("post_cell_type"="cell_type")) %>%
  dplyr::rename(post_super_class=super_class) %>%
  dplyr::left_join(franken.meta[,c("super_class","cell_type")], by = c("pre_cell_type"="cell_type")) %>%
  dplyr::rename(pre_super_class=super_class)

# Function to create binned data
create_binned_data <- function(data, cell_type_col) {
  data %>%
    mutate(norm_diff_bin = cut(norm_diff, 
                               breaks = seq(min(norm_diff), max(norm_diff) + 0.05, by = 0.05),
                               include.lowest = TRUE)) %>%
    group_by(norm_diff_bin, direction, dataset, !!sym(cell_type_col)) %>%
    dplyr::summarise(count = n(), .groups = 'drop') %>%
    group_by(norm_diff_bin, direction, dataset) %>%
    mutate(total = sum(count),
           proportion = count / total)
}

# Create binned data for pre and post cell types
pre_binned_data <- create_binned_data(largest.outliers, "pre_super_class") %>%
  dplyr::rename(super_class = pre_super_class) %>%
  dplyr::filter(direction=="upstream")
post_binned_data <- create_binned_data(largest.outliers, "post_super_class") %>%
  dplyr::rename(super_class = post_super_class) %>%
  dplyr::filter(direction=="downstream")

# Function to create the plot
create_plot <- function(data, title) {
  ggplot(data, aes(x = norm_diff_bin, y = proportion, fill = super_class)) +
    geom_bar(stat = "identity", position = "stack") +
    labs(title = title,
         x = "norm_diff (binned)",
         y = "Proportion",
         fill = "Super Class") +
    theme_minimal() +
    facet_grid(direction ~ dataset) +
    coord_flip()
}

# Create plots
pre_plot <- create_plot(pre_binned_data, "Distribution of pre_super_class across norm_diff bins")
post_plot <- create_plot(post_binned_data, "Distribution of post_super_class across norm_diff bins")

# Save the plot 
ggsave(plot = post_plot, filename = file.path(banc.fig1.extra.path,"andn_post_norm_diff_pre_cell_type_distribution.pdf"), 
       width = 16, height = 8, dpi = 300)
ggsave(plot = pre_plot, filename = file.path(banc.fig1.extra.path,"andn_pre_norm_diff_pre_cell_type_distribution.pdf"),
       width = 16, height = 8, dpi = 300)

#############################################
## SENSORY CELL CLASS COMPARISON BAR CHART ##
#############################################

# Count sensory neurons by cell_class + body_part_sensory in each dataset
banc.sensory <- banc.meta %>%
  dplyr::filter(grepl("sensory", super_class)) %>%
  dplyr::mutate(body_part_sensory = dplyr::if_else(
    is.na(body_part_sensory) | body_part_sensory == "", "unknown", body_part_sensory)) %>%
  dplyr::count(cell_class, body_part_sensory, name = "n") %>%
  dplyr::mutate(dataset = "BANC")

fafb.sensory <- fw.meta %>%
  dplyr::filter(grepl("sensory", super_class)) %>%
  dplyr::mutate(body_part_sensory = dplyr::if_else(
    is.na(body_part_sensory) | body_part_sensory == "", "unknown", body_part_sensory)) %>%
  dplyr::count(cell_class, body_part_sensory, name = "n") %>%
  dplyr::mutate(dataset = "FAFB")

manc.sensory <- mc.meta %>%
  dplyr::filter(grepl("sensory", super_class)) %>%
  dplyr::mutate(body_part_sensory = dplyr::if_else(
    is.na(body_part_sensory) | body_part_sensory == "", "unknown", body_part_sensory)) %>%
  dplyr::count(cell_class, body_part_sensory, name = "n") %>%
  dplyr::mutate(dataset = "MANC")

malecns.sensory <- malecns.meta %>%
  dplyr::filter(grepl("sensory", super_class)) %>%
  dplyr::mutate(body_part_sensory = dplyr::if_else(
    is.na(body_part_sensory) | body_part_sensory == "", "unknown", body_part_sensory)) %>%
  dplyr::count(cell_class, body_part_sensory, name = "n") %>%
  dplyr::mutate(dataset = "maleCNS")

sensory.counts <- dplyr::bind_rows(banc.sensory, fafb.sensory, manc.sensory, malecns.sensory)

# Scale relative to BANC counts (100% = BANC count per cell_class + body_part)
banc.ref <- banc.sensory %>% dplyr::select(cell_class, body_part_sensory, banc_n = n)
sensory.counts <- sensory.counts %>%
  dplyr::left_join(banc.ref, by = c("cell_class", "body_part_sensory")) %>%
  dplyr::filter(!is.na(banc_n), banc_n > 0) %>%
  dplyr::mutate(
    scaled_pct = n / banc_n * 100,
    scaled_pct_clipped = pmin(scaled_pct, 300),
    diff_from_banc = n - banc_n,
    diff_label = dplyr::case_when(
      dataset == "BANC" ~ "",
      diff_from_banc >= 0 ~ paste0("+", diff_from_banc),
      TRUE ~ as.character(diff_from_banc)
    ),
    dataset = factor(dataset, levels = c("BANC", "FAFB", "MANC", "maleCNS"))
  )

# Save one plot per body_part_sensory into a dedicated folder
sensory_folder <- file.path(banc.fig1.extra.path, "sensory_cell_class_counts_comparison")
dir.create(sensory_folder, showWarnings = FALSE, recursive = TRUE)

for (bp in unique(sensory.counts$body_part_sensory)) {
  bp_data <- dplyr::filter(sensory.counts, body_part_sensory == bp)
  g.sensory <- ggplot2::ggplot(bp_data,
                               ggplot2::aes(x = scaled_pct_clipped, y = cell_class, fill = dataset)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), width = 0.7) +
    ggplot2::geom_text(
      data = dplyr::filter(bp_data, dataset != "BANC"),
      ggplot2::aes(label = diff_label),
      position = ggplot2::position_dodge(width = 0.8),
      hjust = -0.1, size = 2
    ) +
    ggplot2::geom_vline(xintercept = 100, linetype = "dashed", color = "grey40", linewidth = 0.5) +
    ggplot2::scale_fill_manual(values = c("BANC" = "white",
                                          "FAFB" = paper.cols[["FAFB"]],
                                          "MANC" = paper.cols[["MANC"]],
                                          "maleCNS" = paper.cols[["maleCNS"]])) +
    ggplot2::labs(
      x = "% of BANC count",
      y = "",
      title = bp,
      fill = "Dataset"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = 8),
      legend.position = "bottom",
      plot.title = ggplot2::element_text(face = "bold", size = 12)
    ) +
    ggplot2::scale_x_continuous(limits = c(0, 300), breaks = seq(0, 300, 100),
                                expand = ggplot2::expansion(mult = c(0, 0)))

  bp_filename <- gsub("[^A-Za-z0-9_-]", "_", bp)
  ggsave(plot = g.sensory,
         filename = file.path(sensory_folder, paste0(bp_filename, ".pdf")),
         width = 8, height = max(3, length(unique(bp_data$cell_class)) * 0.6), dpi = 300)
}

###############################################
## EFFECTOR CELL CLASS COMPARISON BAR CHART ##
###############################################

# Count effector neurons by cell_class + body_part_effector in each dataset
effector_filter <- "motor|visceral|efferent"

banc.effector <- banc.meta %>%
  dplyr::filter(grepl(effector_filter, super_class)) %>%
  dplyr::mutate(body_part_effector = dplyr::if_else(
    is.na(body_part_effector) | body_part_effector == "", "unknown", body_part_effector)) %>%
  dplyr::count(cell_class, body_part_effector, name = "n") %>%
  dplyr::mutate(dataset = "BANC")

fafb.effector <- fw.meta %>%
  dplyr::filter(grepl(effector_filter, super_class)) %>%
  dplyr::mutate(body_part_effector = dplyr::if_else(
    is.na(body_part_effector) | body_part_effector == "", "unknown", body_part_effector)) %>%
  dplyr::count(cell_class, body_part_effector, name = "n") %>%
  dplyr::mutate(dataset = "FAFB")

manc.effector <- mc.meta %>%
  dplyr::filter(grepl(effector_filter, super_class)) %>%
  dplyr::mutate(body_part_effector = dplyr::if_else(
    is.na(body_part_effector) | body_part_effector == "", "unknown", body_part_effector)) %>%
  dplyr::count(cell_class, body_part_effector, name = "n") %>%
  dplyr::mutate(dataset = "MANC")

malecns.effector <- malecns.meta %>%
  dplyr::filter(grepl(effector_filter, super_class)) %>%
  dplyr::mutate(body_part_effector = dplyr::if_else(
    is.na(body_part_effector) | body_part_effector == "", "unknown", body_part_effector)) %>%
  dplyr::count(cell_class, body_part_effector, name = "n") %>%
  dplyr::mutate(dataset = "maleCNS")

effector.counts <- dplyr::bind_rows(banc.effector, fafb.effector, manc.effector, malecns.effector)

# Scale relative to BANC counts
banc.eff.ref <- banc.effector %>% dplyr::select(cell_class, body_part_effector, banc_n = n)
effector.counts <- effector.counts %>%
  dplyr::left_join(banc.eff.ref, by = c("cell_class", "body_part_effector")) %>%
  dplyr::filter(!is.na(banc_n), banc_n > 0) %>%
  dplyr::mutate(
    scaled_pct = n / banc_n * 100,
    scaled_pct_clipped = pmin(scaled_pct, 300),
    diff_from_banc = n - banc_n,
    diff_label = dplyr::case_when(
      dataset == "BANC" ~ "",
      diff_from_banc >= 0 ~ paste0("+", diff_from_banc),
      TRUE ~ as.character(diff_from_banc)
    ),
    dataset = factor(dataset, levels = c("BANC", "FAFB", "MANC", "maleCNS"))
  )

# Horizontal grouped bar chart faceted by body_part_effector
g.effector <- ggplot2::ggplot(effector.counts,
                              ggplot2::aes(x = scaled_pct_clipped, y = cell_class, fill = dataset)) +
  ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), width = 0.7) +
  ggplot2::geom_text(
    data = dplyr::filter(effector.counts, dataset != "BANC"),
    ggplot2::aes(label = diff_label),
    position = ggplot2::position_dodge(width = 0.8),
    hjust = -0.1, size = 2
  ) +
  ggplot2::geom_vline(xintercept = 100, linetype = "dashed", color = "grey40", linewidth = 0.5) +
  ggplot2::facet_wrap(~ body_part_effector, scales = "free_y") +
  ggplot2::scale_fill_manual(values = c("BANC" = "white",
                                        "FAFB" = paper.cols[["FAFB"]],
                                        "MANC" = paper.cols[["MANC"]],
                                        "maleCNS" = paper.cols[["maleCNS"]])) +
  ggplot2::labs(
    x = "% of BANC count",
    y = "",
    fill = "Dataset"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.text.y = ggplot2::element_text(size = 8),
    legend.position = "bottom",
    strip.text = ggplot2::element_text(face = "bold", size = 9)
  ) +
  ggplot2::scale_x_continuous(limits = c(0, 300), breaks = seq(0, 300, 100),
                              expand = ggplot2::expansion(mult = c(0, 0)))

ggsave(plot = g.effector,
       filename = file.path(banc.fig1.extra.path, "effector_cell_class_counts_comparison.pdf"),
       width = 16, height = 10, dpi = 300)
