#' Validation of adjusted influence against hop count and cascade output (Fig. 2b–c, ED Fig. 4a)
#'
#' Confirms that the adjusted-influence score (Methods §"Influence", Eqs.
#' 4–10) behaves as expected on the FAFB v783 reference graph. Two checks:
#'
#'   (1) Adjusted influence is approximately linear in the network-layer
#'       count of Schlegel et al. 2024 (Fig. 2b). Sources are FAFB
#'       olfactory receptor neurons; targets are all other FAFB neurons.
#'   (2) Adjusted influence is proportional to the output of the signal-
#'       cascade algorithm of Winding et al. 2023 (ED Fig. 4a).
#'   (3) BANC-only validation: distribution of direct vs. indirect
#'       sensory influence on all other neurons, with the chosen
#'       count_thresh ≥ 5 (Fig. 2c top, ge5-only outputs).
#'
#' Sidecar histograms for the count-threshold sweep run in raw-count space
#' so the lookup tables remain accurate after downsampling for plotting
#' (`.write_validation_sidecar`).
#'
#' @section Reads:
#'   distance.meta (banc-distances.R) — FAFB hops + cascade output joined
#'                                       to adjusted influence per source seed
#'   banc.meta, franken.meta                                              (snapshots)
#'
#' @section Writes:
#'   figures/figure_2/links/influence_norm_log_vs_*.pdf                   (Fig. 2b–c)
#'   figures/figure_2/links/supplement/extended_data_fig_4a_*.pdf         (ED Fig. 4a cascade)
#'   figures/figure_2/links/*.txt                                          (regression + n's)
#'
#' @section Paper:
#'   Fig. 2b — adjusted influence vs. graph-layer count, R² = 0.94, n = 94,278 pairs.
#'   Fig. 2c — direct vs. indirect sensory and AN/DN influence distributions, ≥ 5 syn.
#'   ED Fig. 4a — adjusted influence vs. signal-cascade output.
#'   Methods §"Influence" Eqs. 1–10.
#'
#' @section Used by:
#'   R/text/numbers.R reads the `_ge5.txt` sidecars for
#'   `sensory_ge5_p25_direct_count`, `sensory_ge5_p25_indirect_count`,
#'   raw direct/indirect counts, etc.
#'
#' @section Reproduce:
#'   BANC_NCORES=1 Rscript R/figures/panels_influence_validation.R

###################
## LOAD PACKAGES ##
###################

# Load required packages and data for influence validation
source("R/startup/banc-startup.R")
source("R/startup/banc-meta.R")
source("R/startup/franken-meta.R")
source("R/startup/banc-distances.R")

#######################################
### INFLUENCE vs. CASCADE/INFECTION ### 
#######################################

# Main panel influence metric
inf.primary.metric <- "influence_norm_log"

# Process data
olf.df <- distance.meta %>%
  dplyr::filter(distance != 0,
                influence > 0.000000002) %>%
  dplyr::filter(grepl("olfactory",seed),
                !grepl("sensory|ascending",super_class)) %>%
  dplyr::filter(!is.na(influence),
                !is.na(distance),
                !is.na(layer_mean))  %>% 
  dplyr::arrange(count,
                 super_class) 

# Get the number of unique distances
n_distances <- length(unique(olf.df$distance))

# Make the plot!
inf.metrics <- colnames(olf.df)[grepl("influence",colnames(olf.df))]
inf.metrics <- inf.primary.metric
for(inf.metric in inf.metrics){
  
  # Plot 1: Scatter plot of influence_score against layer_mean
  olf.df$influence_score <- olf.df[[inf.metric]]
  g.olf1 <- ggplot(olf.df, aes(x = layer_mean, 
                               y = influence_score,
                               color=norm,
                               fill=norm)) +
    geom_point(alpha = 0.25,
               size = 2) +
    scale_color_gradientn(
      colors = c("lightgrey", highlight.col2, highlight.col),
      values = rescale(c(0, max(olf.df$norm)))
    ) +
    scale_fill_gradientn(
      colors = c("lightgrey", highlight.col2, highlight.col),
      values = rescale(c(0, max(olf.df$norm)))
    ) +
    geom_smooth(aes(color = NULL, group = 1),
                formula = 'y ~ x', 
                method = "lm", 
                se = FALSE, 
                color = "black", 
                linetype = "solid") +
    # stat_poly_eq(aes(color = NULL, 
    #                  label = paste(after_stat(eq.label), 
    #                                after_stat(rr.label), sep = "~~~")),
    #              formula = y ~ x, 
    #              parse = TRUE, 
    #              label.x = 0.75, 
    #              label.y = 0.95, 
    #              fontface = "bold",
    #              size =5) +
    labs(
      title = "",
      x = "layer mean, graph traversal",
      y = inf.metric,
      color = "class"
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5, size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 12),
      axis.title = element_text(size = 18),
      axis.text = element_text(size = 16),
      strip.text = element_blank(), 
      strip.background = element_blank(),  
      panel.spacing = unit(1, "lines")  
    ) 
  
  # Save the plot
  print(g.olf1)
  
  # Calculate mean and standard deviation for each group
  summary_data <- olf.df %>%
    dplyr::filter(distance != 0) %>%
    dplyr::group_by(distance) %>%
    dplyr::summarise(
      mean_influence = mean(influence_score, na.rm = TRUE),
      sd_influence = sd(influence_score, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Create a position dodge object to ensure consistent dodging
  dodge <- position_dodge(width = 0.8)
  
  # Plot 3: Grouped jitter plot of influence_log against distance with mean and SD
  g.olf2 <- ggplot(olf.df, 
                   aes(x = factor(distance), 
                       y = influence_score)) +
    geom_point(aes(color=norm,
                   fill=norm),
               position = position_jitterdodge(jitter.width = 0.2),
               alpha = 0.1, 
               size = 2) +
    scale_color_gradientn(
      colors = c("lightgrey", highlight.col2, highlight.col),
      values = rescale(c(0, max(olf.df$norm)))
    ) +
    scale_fill_gradientn(
      colors = c("lightgrey", highlight.col2, highlight.col),
      values = rescale(c(0, max(olf.df$norm)))
    ) +
    geom_pointrange(data = summary_data,
                    aes(y = mean_influence, 
                        ymin = mean_influence - sd_influence, 
                        ymax = mean_influence + sd_influence), 
                    size = 0.5, 
                    fatten = 3, 
                    shape = 21, 
                    color = "black", 
                    stroke = 1) + 
    geom_smooth(aes(color = 1, group = 1), 
                formula = 'y ~ x', 
                method = "lm", 
                se = FALSE, 
                color = "black", 
                linetype = "solid") +
    stat_poly_eq(aes(group = 1, 
                     label = paste(after_stat(eq.label), 
                                   after_stat(rr.label), sep = "~~~")),
                 formula = y ~ x, 
                 parse = TRUE,
                 label.x = 0.75, 
                 label.y = 0.95,
                 fontface = "bold", 
                 size = 5) +
    labs(
      title = "",
      x = "cascade hop",
      y = inf.metric
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5, size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 12),
      axis.title = element_text(size = 18),
      axis.text = element_text(size = 16),
      strip.text = element_blank(), 
      strip.background = element_blank(),  
      panel.spacing = unit(1, "lines")  
    ) 
  
  # Save the plot
  print(g.olf2)

  # Save
  if(inf.metric == inf.primary.metric){
    ggsave(g.olf1, 
           filename = file.path(banc.fig2.path, sprintf("%s_vs_layer_mean_super_class.pdf",inf.metric)), 
           width = 6, height = 3, dpi = 300)
    ggsave(g.olf2,
           filename = file.path(banc.fig2.extra.path, sprintf("%s_vs_cascade_mean.png",inf.metric)),
           width = 6, height = 3, dpi = 300)
    ggsave(convert_to_dark_mode(g.olf1), 
           filename = file.path(banc.fig2.darkmode.path, sprintf("dark_mode_%s_vs_layer_mean_super_class.png",inf.metric)), 
           width = 6, height = 3, dpi = 300)
    ggsave(convert_to_dark_mode(g.olf2), 
           filename = file.path(banc.fig2.darkmode.path, sprintf("dark_mode_%s_vs_cascade_mean.png",inf.metric)), 
           width = 6, height = 3, dpi = 300)
  }else{
    ggsave(g.olf1, 
           filename = file.path(banc.fig2.extra.path, sprintf("%s_vs_layer_mean_super_class.png",inf.metric)), 
           width = 6, height = 3, dpi = 300)
    ggsave(g.olf2, 
           filename = file.path(banc.fig2.extra.path, sprintf("%s_vs_cascade_mean.png",inf.metric)), 
           width = 6, height = 3, dpi = 300)
  }
}

##################################
### INFLUENCE vs. CONNECTIVITY ### 
##################################

# Main panel influence metric
inf.primary.metric <- "influence_norm_log"

# Validation: per-sensory-neuron influence on all non-sensory neurons,
# split by direct connectivity (no count threshold)
source("R/startup/banc-edgelist.R")

# Load the raw edgelist once at the parent level. The IC itself is built
# inside .run_validation_histograms (per-call, with the right
# ic_count_thresh) — so we no longer pre-build a global IC here.
message("Loading raw edgelist for validation passes...")
banc.edgelist.raw <- arrow::read_feather(.banc_edgelist_cache)

# Identify seed sets (sensory and AN/DN) and their complements.
# Ensure root_id is character — the feather cache may store pre/post as
# numeric, causing %in% to silently return all FALSE if types don't match.
# Use super_class to define sensory neurons (body_part_sensory is now
# universally populated and can't be used for a sensory filter).
.valid_super <- banc.meta %>%
  dplyr::filter(!is.na(super_class),
                !super_class %in% c("glia", "", "trachea", "not_a_neuron"))
sensory_ids <- as.character(.valid_super %>%
  dplyr::filter(grepl("sensory", super_class)) %>%
  dplyr::pull(root_id) %>%
  unique())
nonsensory_ids <- as.character(.valid_super %>%
  dplyr::filter(!grepl("sensory", super_class)) %>%
  dplyr::pull(root_id) %>%
  unique())
an_dn_ids <- as.character(.valid_super %>%
  dplyr::filter(super_class %in% c("ascending", "descending")) %>%
  dplyr::pull(root_id) %>%
  unique())
non_an_dn_ids <- as.character(.valid_super %>%
  dplyr::filter(!super_class %in% c("ascending", "descending")) %>%
  dplyr::pull(root_id) %>%
  unique())

# Seed counts (2026-05-20, revised): ALL sensory and ALL AN/DN seeds drive
# their respective facets; only the random-BANC facet is downsampled (to
# 1,000 below). Reason: with the stricter Pass B (IC count_thresh = 5),
# capping at 1,000 sensory/AN+DN seeds left the "direct" tail too sparse
# relative to the indirect mass — the curves looked offset rather than
# convincingly shifted. Using every sensory and every AN/DN seed restores
# enough direct pairs to outweigh the indirect tail at the right of the
# distribution. Wall-clock cost: ~3-5x the 1k-cap run depending on the
# number of valid sensory + AN/DN neurons (~30k + ~3k respectively).
message(sprintf("Sensory seeds: %d (ALL) -> %d non-sensory targets",
                length(sensory_ids), length(nonsensory_ids)))
message(sprintf("AN/DN seeds:   %d (ALL) -> %d non-AN/DN targets",
                length(an_dn_ids), length(non_an_dn_ids)))

# Third seed set: random sample of 1,000 PROOFREAD BANC neurons regardless
# of super_class. Mirrors the modal pairwise block in numbers.R:81-230
# (set.seed(888), .modal_pool = proofread). Target set = all proofread +
# roughly_proofread neurons except the seed itself.
.random_n <- 1000L
.proofread_pool <- as.character(.valid_super %>%
  dplyr::filter(as.logical(proofread) %in% TRUE) %>%
  dplyr::pull(root_id) %>% unique())
random_banc_targets <- as.character(.valid_super %>%
  dplyr::filter(as.logical(proofread) %in% TRUE |
                  as.logical(roughly_proofread) %in% TRUE) %>%
  dplyr::pull(root_id) %>% unique())
set.seed(888)
random_banc_ids <- if (length(.proofread_pool) > .random_n) {
  sample(.proofread_pool, .random_n)
} else {
  .proofread_pool
}
message(sprintf("Random BANC seeds: %d -> %d proofread+rough targets",
                length(random_banc_ids), length(random_banc_targets)))
rm(.proofread_pool)

# Pre-compute direct connectivity for both seed sets (seed → set of
# directly-connected target ids).
message("Pre-computing per-seed direct connectivity...")
banc.edgelist.raw$pre <- as.character(banc.edgelist.raw$pre)
banc.edgelist.raw$post <- as.character(banc.edgelist.raw$post)
.build_direct_conn <- function(seed_ids, target_ids, thresh = 0L) {
  # "Connected" = direct edge with count > `thresh`. thresh = 0 (default)
  # matches the no-threshold influence calculator; thresh = 4 gives the
  # canonical count >= 5 used for the modal pairwise calculation.
  dt <- data.table::as.data.table(banc.edgelist.raw)[
    pre %in% seed_ids & post %in% target_ids & count > thresh,
    .(post = unique(post)), by = pre
  ]
  split(dt$post, dt$pre)
}
direct_conn_list_sens       <- .build_direct_conn(sensory_ids,    nonsensory_ids)
direct_conn_list_andn       <- .build_direct_conn(an_dn_ids,      non_an_dn_ids)
direct_conn_list_random     <- .build_direct_conn(random_banc_ids, random_banc_targets)
# Stricter classifications for the count >= 5 plot variants. The matching
# IC for these plots also uses count_thresh = 5 (built inside the worker).
direct_conn_list_sens_ge5   <- .build_direct_conn(sensory_ids,    nonsensory_ids,      thresh = 4L)
direct_conn_list_andn_ge5   <- .build_direct_conn(an_dn_ids,      non_an_dn_ids,       thresh = 4L)
direct_conn_list_random_ge5 <- .build_direct_conn(random_banc_ids, random_banc_targets, thresh = 4L)
message(sprintf("  sensory:     %d neurons with direct non-sensory targets", length(direct_conn_list_sens)))
message(sprintf("  AN/DN:       %d neurons with direct non-AN/DN targets",   length(direct_conn_list_andn)))
message(sprintf("  random BANC: %d neurons with direct (proofread+rough) targets",
                length(direct_conn_list_random)))

# Histogram binning parameters (shared across both seed sets)
const <- -24
inf.threshold <- exp(const)
n_bins <- 500
floor_cutoff <- 0.5  # exclude values at/near the floor
bin_range <- c(floor_cutoff, 24)
bin_breaks <- seq(bin_range[1], bin_range[2], length.out = n_bins + 1)

# Worker closure: computes (conn_hist, not_conn_hist) for a given seed set +
# target set + direct-connectivity lookup. Parallel (PSOCK) when BANC_NCORES>1.
.val_ncores <- {
  env_nc <- Sys.getenv("BANC_NCORES", unset = NA)
  if (!is.na(env_nc) && nzchar(env_nc)) suppressWarnings(as.integer(env_nc))
  else max(1L, min(4L, parallel::detectCores() - 1L))
}

# Modal integer tabulations span the full possible adj_influence range
# (floor 0.5 → const 24). We bucket on round(adj_influence) which produces
# integers 1..MODAL_MAX_INT. We sum across seeds and across workers.
MODAL_MAX_INT <- 26L

.run_validation_histograms <- function(seed_ids, target_ids, direct_conn_list,
                                       label,
                                       ic_count_thresh = 0L,
                                       record_modal = FALSE) {
  n_seeds <- length(seed_ids)
  conn_hist <- numeric(n_bins)
  not_conn_hist <- numeric(n_bins)
  modal_direct_tab   <- integer(MODAL_MAX_INT)
  modal_indirect_tab <- integer(MODAL_MAX_INT)
  if (.val_ncores > 1L && n_seeds >= 50) {
    message(sprintf("[validation:%s] parallel per-neuron influence (%d neurons, %d workers)...",
                    label, n_seeds, .val_ncores))
    .chunks <- parallel::splitIndices(n_seeds, .val_ncores)
    .sid_chunks <- lapply(.chunks, function(idx) seed_ids[idx])
    .cl <- parallel::makeCluster(.val_ncores)
    on.exit(parallel::stopCluster(.cl), add = TRUE)
    .results <- parallel::parLapply(.cl, .sid_chunks,
      function(chunk_sids, target_ids_w, direct_conn_list_w,
               elist_w, meta_w, const_w, inf_thresh_w,
               floor_cut_w, bin_breaks_w, n_bins_w, modal_max_int_w,
               ic_count_thresh_w, record_modal_w) {
        # Filter the edgelist to count >= ic_count_thresh before handing it
        # to the influence calculator. count_thresh = 0 keeps everything.
        elist_filt <- if (ic_count_thresh_w > 0) {
          elist_w[elist_w$count >= ic_count_thresh_w, , drop = FALSE]
        } else elist_w
        ic_w <- influencer::influence_calculator_py(
          edgelist_simple = elist_filt, meta = meta_w,
          count_thresh = ic_count_thresh_w
        )
        conn_h <- numeric(n_bins_w)
        not_conn_h <- numeric(n_bins_w)
        m_d <- integer(modal_max_int_w)
        m_i <- integer(modal_max_int_w)
        for (sid in chunk_sids) {
          tryCatch({
            inf_raw <- influencer::calculate_influence_py(ic_w, sid)
            scores <- inf_raw$`Influence_score_(unsigned)`[inf_raw$id %in% target_ids_w & inf_raw$id != sid]
            tids <- inf_raw$id[inf_raw$id %in% target_ids_w & inf_raw$id != sid]
            scores <- log(pmax(scores, inf_thresh_w)) - const_w
            above <- scores > floor_cut_w
            scores <- scores[above]; tids <- tids[above]
            direct_targets <- direct_conn_list_w[[sid]]
            if (is.null(direct_targets)) direct_targets <- character(0)
            conn_mask <- tids %in% direct_targets
            if (any(conn_mask)) {
              h <- findInterval(scores[conn_mask], bin_breaks_w, left.open = TRUE)
              h <- h[h >= 1 & h <= n_bins_w]
              if (length(h) > 0) conn_h <- conn_h + tabulate(h, nbins = n_bins_w)
            }
            if (any(!conn_mask)) {
              h <- findInterval(scores[!conn_mask], bin_breaks_w, left.open = TRUE)
              h <- h[h >= 1 & h <= n_bins_w]
              if (length(h) > 0) not_conn_h <- not_conn_h + tabulate(h, nbins = n_bins_w)
            }
            if (record_modal_w && length(scores) > 0) {
              scores_int <- round(scores)
              scores_int <- pmin(pmax(scores_int, 1L), modal_max_int_w)
              if (any(conn_mask))  m_d <- m_d + tabulate(scores_int[ conn_mask], nbins = modal_max_int_w)
              if (any(!conn_mask)) m_i <- m_i + tabulate(scores_int[!conn_mask], nbins = modal_max_int_w)
            }
          }, error = function(e) NULL)
        }
        list(conn = conn_h, not_conn = not_conn_h, m_d = m_d, m_i = m_i)
      },
      target_ids_w = target_ids, direct_conn_list_w = direct_conn_list,
      elist_w = banc.edgelist.raw, meta_w = as.data.frame(banc.meta),
      const_w = const, inf_thresh_w = inf.threshold,
      floor_cut_w = floor_cutoff, bin_breaks_w = bin_breaks, n_bins_w = n_bins,
      modal_max_int_w = MODAL_MAX_INT,
      ic_count_thresh_w = ic_count_thresh, record_modal_w = record_modal
    )
    for (r in .results) {
      conn_hist <- conn_hist + r$conn
      not_conn_hist <- not_conn_hist + r$not_conn
      modal_direct_tab   <- modal_direct_tab   + r$m_d
      modal_indirect_tab <- modal_indirect_tab + r$m_i
    }
    rm(.results)
  } else {
    message(sprintf("[validation:%s] sequential per-neuron influence (%d neurons)...",
                    label, n_seeds))
    pb <- progress::progress_bar$new(
      format = paste0("  ", label, " [:bar] :current/:total (:percent) eta: :eta"),
      total = n_seeds, clear = FALSE, width = 70
    )
    # Sequential branch builds the IC once at the parent level so we don't
    # rebuild it per seed.
    .seq_elist <- if (ic_count_thresh > 0) {
      banc.edgelist.raw[banc.edgelist.raw$count >= ic_count_thresh, , drop = FALSE]
    } else banc.edgelist.raw
    .seq_ic <- influencer::influence_calculator_py(
      edgelist_simple = .seq_elist, meta = as.data.frame(banc.meta),
      count_thresh = ic_count_thresh
    )
    for (i in seq_along(seed_ids)) {
      sid <- seed_ids[i]
      tryCatch({
        inf_raw <- calculate_influence_py(.seq_ic, sid)
        scores <- inf_raw$`Influence_score_(unsigned)`[inf_raw$id %in% target_ids & inf_raw$id != sid]
        tids <- inf_raw$id[inf_raw$id %in% target_ids & inf_raw$id != sid]
        scores <- log(pmax(scores, inf.threshold)) - const
        above_floor <- scores > floor_cutoff
        scores <- scores[above_floor]; tids <- tids[above_floor]
        direct_targets <- direct_conn_list[[sid]]
        if (is.null(direct_targets)) direct_targets <- character(0)
        conn_mask <- tids %in% direct_targets
        if (any(conn_mask)) {
          h <- hist(scores[conn_mask], breaks = bin_breaks, plot = FALSE)
          conn_hist <- conn_hist + h$counts
        }
        if (any(!conn_mask)) {
          h <- hist(scores[!conn_mask], breaks = bin_breaks, plot = FALSE)
          not_conn_hist <- not_conn_hist + h$counts
        }
        if (record_modal && length(scores) > 0) {
          scores_int <- round(scores)
          scores_int <- pmin(pmax(scores_int, 1L), MODAL_MAX_INT)
          if (any(conn_mask))  modal_direct_tab   <- modal_direct_tab   + tabulate(scores_int[ conn_mask], nbins = MODAL_MAX_INT)
          if (any(!conn_mask)) modal_indirect_tab <- modal_indirect_tab + tabulate(scores_int[!conn_mask], nbins = MODAL_MAX_INT)
        }
      }, error = function(e) NULL)
      pb$tick()
    }
  }
  message(sprintf("[validation:%s] totals: %d connected, %d not-connected",
                  label, sum(conn_hist), sum(not_conn_hist)))
  list(conn = conn_hist, not_conn = not_conn_hist,
       m_d = modal_direct_tab,   m_i = modal_indirect_tab,
       ic_count_thresh = ic_count_thresh)
}

# Pass A — IC count_thresh = 0, "direct" classification = count >= 1 (any edge).
# DISABLED 2026-05-20: paper now only cites the ge5 (Pass B) numbers. Skipping
# Pass A here saves ~half the wall-clock and avoids writing the unused Pass A
# PDFs / sidecars / CSV rows.
if (FALSE) {
.hist_sens   <- .run_validation_histograms(sensory_ids,    nonsensory_ids,      direct_conn_list_sens,   "sensory (IC0)",     ic_count_thresh = 0L)
.hist_andn   <- .run_validation_histograms(an_dn_ids,      non_an_dn_ids,       direct_conn_list_andn,   "AN/DN (IC0)",       ic_count_thresh = 0L)
.hist_random <- .run_validation_histograms(random_banc_ids, random_banc_targets, direct_conn_list_random, "random BANC (IC0)", ic_count_thresh = 0L,
                                            record_modal = TRUE)
}  # end Pass A disable

# Pass B — IC count_thresh = 5, "direct" classification = count >= 5. The
# stricter graph propagates fewer signals, so curves shift compared to
# Pass A. Plot files get a _ge5 suffix; modal CSV gains two rows tagged
# ic_count_thresh = 5.
.hist_sens_ge5   <- .run_validation_histograms(sensory_ids,    nonsensory_ids,      direct_conn_list_sens_ge5,   "sensory (IC5)",     ic_count_thresh = 5L)
.hist_andn_ge5   <- .run_validation_histograms(an_dn_ids,      non_an_dn_ids,       direct_conn_list_andn_ge5,   "AN/DN (IC5)",       ic_count_thresh = 5L)
.hist_random_ge5 <- .run_validation_histograms(random_banc_ids, random_banc_targets, direct_conn_list_random_ge5, "random BANC (IC5)", ic_count_thresh = 5L,
                                                record_modal = TRUE)

# Write modal pairwise CSV consumed by R/text/numbers.R (replaces the
# in-script modal block that previously ran in numbers.R). Mode = integer
# value with highest count in the per-pair tabulation. Threshold variants:
#   ge5 — pair classified as direct iff a count >= 5 edge exists
#   ge1 — pair classified as direct iff a count >= 1 edge exists
.modal_mode_of_tab <- function(tab) {
  if (sum(tab) == 0) return(NA_integer_)
  which.max(tab)  # integer 1..MODAL_MAX_INT == rounded adjusted influence
}
.modal_csv_dir <- "data/determined_thresholds"
dir.create(.modal_csv_dir, recursive = TRUE, showWarnings = FALSE)
.modal_csv <- file.path(.modal_csv_dir, "pairwise_modal_influence.csv")
.modal_n <- length(random_banc_ids)
.modal_df <- data.frame(
  metric = c(
    # Pass B only: IC count_thresh = 5, direct = count >= 5. (Pass A rows
    # were dropped 2026-05-20 — paper now only cites the ge5 numbers.)
    "pairwise_adjusted_influence_modal_direct_ge5",
    "pairwise_adjusted_influence_modal_indirect_ge5"
  ),
  value = c(
    .modal_mode_of_tab(.hist_random_ge5$m_d),
    .modal_mode_of_tab(.hist_random_ge5$m_i)
  ),
  n_pairs = c(
    sum(.hist_random_ge5$m_d),
    sum(.hist_random_ge5$m_i)
  ),
  seed_n = .modal_n,
  ic_count_thresh = c(5L, 5L),
  direct_thresh   = c("count_ge5", "count_ge5"),
  pair_kind       = c("direct", "indirect"),
  date = format(Sys.Date()),
  stringsAsFactors = FALSE
)
readr::write_csv(.modal_df, .modal_csv)
message("Wrote modal pairwise CSV: ", .modal_csv)
print(.modal_df)
rm(banc.edgelist.raw); gc()

# Reconstruct data frame from histograms for ggplot density.
# Downsample per-facet if total counts exceed 1M to keep plotting fast.
bin_mids <- (bin_breaks[-1] + bin_breaks[-length(bin_breaks)]) / 2
# 2026-05-21: removed the 1M-point downsampling + materialise-one-row-per-
# data-point pattern. The earlier approach turned 94k direct + 1.3B indirect
# (sensory IC5) into 68 + 999,928 points and then ran geom_density's KDE on
# the result — the "direct" curve was bandwidth-distorted and the sidecar
# counts derived from the downsampled df were off by ~1,400×. We now keep
# the raw 500-bin histograms and plot them directly (geom_line over bin
# midpoints, peak-normalised per class to mimic `after_stat(scaled)`).
.histograms_to_df <- function(conn_h, not_conn_h, facet_label) {
  conn_peak <- max(conn_h)
  notc_peak <- max(not_conn_h)
  conn_norm <- if (conn_peak > 0) conn_h / conn_peak else conn_h
  not_norm  <- if (notc_peak > 0) not_conn_h / notc_peak else not_conn_h
  df <- rbind(
    data.frame(influence_norm_log = bin_mids,
               density            = conn_norm,
               connectivity       = "connected",
               facet              = facet_label,
               stringsAsFactors   = FALSE),
    data.frame(influence_norm_log = bin_mids,
               density            = not_norm,
               connectivity       = "not connected",
               facet              = facet_label,
               stringsAsFactors   = FALSE)
  )
  df$connectivity <- factor(df$connectivity,
                            levels = c("not connected", "connected"))
  df
}
# Pass A dataframes — DISABLED 2026-05-20 (paper now only cites the ge5
# variants; see Pass B block immediately below).
if (FALSE) {
.df_sens   <- .histograms_to_df(.hist_sens$conn,   .hist_sens$not_conn,   "sensory")
.df_random <- .histograms_to_df(.hist_random$conn, .hist_random$not_conn, "random BANC")
.df_andn   <- .histograms_to_df(.hist_andn$conn,   .hist_andn$not_conn,   "AN/DN")

.df_main <- dplyr::bind_rows(.df_sens, .df_andn)
.df_main$facet <- factor(.df_main$facet, levels = c("sensory", "AN/DN"))

.df_random_only <- .df_random
.df_random_only$facet <- factor(.df_random_only$facet, levels = "random BANC")

.df_full <- dplyr::bind_rows(.df_sens, .df_random, .df_andn)
.df_full$facet <- factor(.df_full$facet, levels = c("sensory", "random BANC", "AN/DN"))
}  # end Pass A dataframes

# Build matching dataframes for Pass B (IC count_thresh = 5, direct = count >= 5).
.df_sens_ge5   <- .histograms_to_df(.hist_sens_ge5$conn,   .hist_sens_ge5$not_conn,   "sensory")
.df_random_ge5 <- .histograms_to_df(.hist_random_ge5$conn, .hist_random_ge5$not_conn, "random BANC")
.df_andn_ge5   <- .histograms_to_df(.hist_andn_ge5$conn,   .hist_andn_ge5$not_conn,   "AN/DN")

.df_main_ge5 <- dplyr::bind_rows(.df_sens_ge5, .df_andn_ge5)
.df_main_ge5$facet <- factor(.df_main_ge5$facet, levels = c("sensory", "AN/DN"))

.df_random_only_ge5 <- .df_random_ge5
.df_random_only_ge5$facet <- factor(.df_random_only_ge5$facet, levels = "random BANC")

.df_full_ge5 <- dplyr::bind_rows(.df_sens_ge5, .df_random_ge5, .df_andn_ge5)
.df_full_ge5$facet <- factor(.df_full_ge5$facet, levels = c("sensory", "random BANC", "AN/DN"))

# Stash the raw Pass B histograms for the sidecar — we still drop the
# dataframes (small) but keep the histograms alive until the for-loop
# below has emitted all sidecars.
.raw_hists_ge5 <- list(
  sensory       = list(conn = .hist_sens_ge5$conn,    not_conn = .hist_sens_ge5$not_conn),
  `random BANC` = list(conn = .hist_random_ge5$conn,  not_conn = .hist_random_ge5$not_conn),
  `AN/DN`       = list(conn = .hist_andn_ge5$conn,    not_conn = .hist_andn_ge5$not_conn)
)
rm(.df_sens_ge5, .df_random_ge5, .df_andn_ge5,
   .hist_sens_ge5, .hist_random_ge5, .hist_andn_ge5); gc()

# ggplot helper — single-column faceted plot of the raw bin-mid×count
# histogram, peak-normalised per class. Mimics `geom_density(aes(y =
# after_stat(scaled)))` visually but is computed directly from the raw
# histograms, so the "connected" curve is faithful even when direct and
# indirect classes have wildly different total counts.
.make_validation_plot <- function(df, inf.metric) {
  df$influence_score <- df[[inf.metric]]
  ggplot2::ggplot(
    df,
    ggplot2::aes(x = influence_score, y = density,
                 color = connectivity, group = connectivity)
  ) +
    ggplot2::geom_line(linewidth = 1.2, na.rm = TRUE) +
    ggplot2::scale_color_manual(
      values = c("not connected" = "lightgrey", "connected" = highlight.col),
      name = "Direct connectivity"
    ) +
    ggplot2::facet_wrap(~ facet, ncol = 1, scales = "fixed") +
    ggplot2::labs(
      x = inf.metric,
      y = "scaled count (max=1)",
      color = "direct connectivity"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "right",
      axis.title = ggplot2::element_text(size = 18),
      axis.text = ggplot2::element_text(size = 16),
      plot.title = ggplot2::element_text(hjust = 0.5, size = 14),
      strip.text = ggplot2::element_text(size = 14)
    ) +
    theme(legend.position = "none")
}

# Sidecar helper — per-facet quantile / count breakdown matching the PDF's
# facets. Header lists only the seed counts relevant to those facets.
.write_validation_sidecar <- function(facet_labels, raw_hists,
                                       inf.metric, out_path) {
  # facet_labels: character vector (e.g. c("sensory", "AN/DN"))
  # raw_hists:    named list keyed by facet_label, each list(conn=numeric,
  #               not_conn=numeric) of length n_bins; uses bin_mids and
  #               bin_breaks from the enclosing scope.
  .hist_q <- function(h, q) {
    if (sum(h) == 0) return(NA_real_)
    cums <- cumsum(h) / sum(h)
    bin_mids[which(cums >= q)[1]]
  }
  .hist_ge <- function(h, thresh) {
    if (is.na(thresh)) return(0L)
    sum(h[bin_mids >= thresh])
  }
  .lines <- c(
    sprintf("Influence validation sidecar — %s", inf.metric),
    sprintf("Date: %s", format(Sys.time(), "%Y-%m-%d %H:%M")),
    "Counts are RAW (post-2026-05-21 fix; no downsampling). Connected =",
    "direct edge above the threshold used for this pass (count >= 5 for",
    "ge5 outputs; count >= 1 otherwise)."
  )
  if ("sensory" %in% facet_labels) {
    .lines <- c(.lines,
      sprintf("Sensory seeds sampled:    %d (targets: %d non-sensory)",
              length(sensory_ids), length(nonsensory_ids)))
  }
  if ("random BANC" %in% facet_labels) {
    .lines <- c(.lines,
      sprintf("Random BANC seeds:        %d (targets: %d proofread + roughly_proofread)",
              length(random_banc_ids), length(random_banc_targets)))
  }
  if ("AN/DN" %in% facet_labels) {
    .lines <- c(.lines,
      sprintf("AN/DN seeds sampled:      %d (targets: %d non-AN/DN)",
              length(an_dn_ids), length(non_an_dn_ids)))
  }
  .lines <- c(.lines, "")
  for (.facet in facet_labels) {
    .raw <- raw_hists[[.facet]]
    if (is.null(.raw)) next
    .tot_dir <- sum(.raw$conn)
    .tot_ind <- sum(.raw$not_conn)
    .lines <- c(.lines,
      sprintf("== Facet: %s ==", .facet),
      sprintf("Total direct interactions (connected):   %d", .tot_dir),
      sprintf("Total indirect interactions (not conn'd): %d", .tot_ind)
    )
    if (.tot_dir > 0) {
      .q25 <- .hist_q(.raw$conn, 0.25)
      .q50 <- .hist_q(.raw$conn, 0.50)
      .q75 <- .hist_q(.raw$conn, 0.75)
      .lines <- c(.lines,
        "Quantile thresholds of the DIRECT distribution:",
        sprintf("  25th pctl: %.4f  50th pctl: %.4f  75th pctl: %.4f",
                .q25, .q50, .q75),
        sprintf("  Direct   >= 25th (%.4f):  %d / %d (%.1f%%)",
                .q25, .hist_ge(.raw$conn, .q25), .tot_dir,
                100 * .hist_ge(.raw$conn, .q25) / max(1, .tot_dir)),
        sprintf("  Indirect >= 25th (%.4f):  %d / %d (%.1f%%)",
                .q25, .hist_ge(.raw$not_conn, .q25), .tot_ind,
                100 * .hist_ge(.raw$not_conn, .q25) / max(1, .tot_ind)),
        sprintf("  Direct   >= 50th (%.4f):  %d / %d (%.1f%%)",
                .q50, .hist_ge(.raw$conn, .q50), .tot_dir,
                100 * .hist_ge(.raw$conn, .q50) / max(1, .tot_dir)),
        sprintf("  Indirect >= 50th (%.4f):  %d / %d (%.1f%%)",
                .q50, .hist_ge(.raw$not_conn, .q50), .tot_ind,
                100 * .hist_ge(.raw$not_conn, .q50) / max(1, .tot_ind)),
        sprintf("  Direct   >= 75th (%.4f):  %d / %d (%.1f%%)",
                .q75, .hist_ge(.raw$conn, .q75), .tot_dir,
                100 * .hist_ge(.raw$conn, .q75) / max(1, .tot_dir)),
        sprintf("  Indirect >= 75th (%.4f):  %d / %d (%.1f%%)",
                .q75, .hist_ge(.raw$not_conn, .q75), .tot_ind,
                100 * .hist_ge(.raw$not_conn, .q75) / max(1, .tot_ind))
      )
    } else {
      .lines <- c(.lines, "  no direct mass — skipping quantile table")
    }
    .lines <- c(.lines, "")
  }
  writeLines(.lines, out_path)
  message("Wrote sidecar: ", out_path)
}

# Render the three output variants.
inf.metrics <- inf.primary.metric
for (inf.metric in inf.metrics) {
  # Pass A plots — DISABLED 2026-05-20 (only ge5 PDFs/sidecars are used).
  if (FALSE) {
  # Main — sensory + AN/DN (2 facets, stacked vertically).
  # Per-facet size matches the preprint single-density plot (6 x 2.5).
  g.main <- .make_validation_plot(.df_main, inf.metric)
  plot(g.main)
  ggsave(g.main,
         filename = file.path(banc.fig2.path,
                              sprintf("%s_vs_direct_connectivity.pdf", inf.metric)),
         width = 6, height = 5, dpi = 300)
  ggsave(convert_to_dark_mode(g.main),
         filename = file.path(banc.fig2.darkmode.path,
                              sprintf("dark_mode_%s_vs_direct_connectivity.pdf", inf.metric)),
         width = 6, height = 5, dpi = 300)
  .write_validation_sidecar(.df_main, inf.metric,
    file.path(banc.fig2.path,
              sprintf("%s_vs_direct_connectivity.txt", inf.metric)))

  # Random sample — random BANC only (1 facet, preprint dims 6 x 2.5)
  g.random <- .make_validation_plot(.df_random_only, inf.metric)
  ggsave(g.random,
         filename = file.path(banc.fig2.extra.path,
                              sprintf("%s_vs_direct_connectivity_random_sample.pdf", inf.metric)),
         width = 6, height = 2.5, dpi = 300)
  .write_validation_sidecar(.df_random_only, inf.metric,
    file.path(banc.fig2.extra.path,
              sprintf("%s_vs_direct_connectivity_random_sample.txt", inf.metric)))

  # Full — sensory + random + AN/DN (3 facets, stacked vertically; 6 x 2.5 each)
  g.full <- .make_validation_plot(.df_full, inf.metric)
  ggsave(g.full,
         filename = file.path(banc.fig2.extra.path,
                              sprintf("%s_vs_direct_connectivity_full.pdf", inf.metric)),
         width = 6, height = 7.5, dpi = 300)
  .write_validation_sidecar(.df_full, inf.metric,
    file.path(banc.fig2.extra.path,
              sprintf("%s_vs_direct_connectivity_full.txt", inf.metric)))
  }  # end Pass A plot disable

  # Pass B variants — IC count_thresh = 5, direct = count >= 5. Saved with
  # _ge5 suffix; same panel dimensions as Pass A so the two sets are
  # visually comparable side-by-side.
  g.main.ge5 <- .make_validation_plot(.df_main_ge5, inf.metric)
  ggsave(g.main.ge5,
         filename = file.path(banc.fig2.path,
                              sprintf("%s_vs_direct_connectivity_ge5.pdf", inf.metric)),
         width = 6, height = 5, dpi = 300)
  ggsave(convert_to_dark_mode(g.main.ge5),
         filename = file.path(banc.fig2.darkmode.path,
                              sprintf("dark_mode_%s_vs_direct_connectivity_ge5.pdf", inf.metric)),
         width = 6, height = 5, dpi = 300)
  .write_validation_sidecar(c("sensory", "AN/DN"),
    .raw_hists_ge5[c("sensory", "AN/DN")], inf.metric,
    file.path(banc.fig2.path,
              sprintf("%s_vs_direct_connectivity_ge5.txt", inf.metric)))

  g.random.ge5 <- .make_validation_plot(.df_random_only_ge5, inf.metric)
  ggsave(g.random.ge5,
         filename = file.path(banc.fig2.extra.path,
                              sprintf("%s_vs_direct_connectivity_random_sample_ge5.pdf", inf.metric)),
         width = 6, height = 2.5, dpi = 300)
  .write_validation_sidecar("random BANC",
    .raw_hists_ge5["random BANC"], inf.metric,
    file.path(banc.fig2.extra.path,
              sprintf("%s_vs_direct_connectivity_random_sample_ge5.txt", inf.metric)))

  g.full.ge5 <- .make_validation_plot(.df_full_ge5, inf.metric)
  ggsave(g.full.ge5,
         filename = file.path(banc.fig2.extra.path,
                              sprintf("%s_vs_direct_connectivity_full_ge5.pdf", inf.metric)),
         width = 6, height = 7.5, dpi = 300)
  .write_validation_sidecar(c("sensory", "random BANC", "AN/DN"),
    .raw_hists_ge5, inf.metric,
    file.path(banc.fig2.extra.path,
              sprintf("%s_vs_direct_connectivity_full_ge5.txt", inf.metric)))
}


#########################################################################
## VALIDATION — 10k PR/RPR seeds → all PR/RPR targets (GCS cache route) ##
#########################################################################
# Variant of the validation above, but the seed pool is a random 10,000-
# neuron sample from the full proofread + roughly_proofread set, and the
# target pool is the same full PR/RPR set. Uses bancr::banc_influence()
# to pull the cached influence values from
# gs://lee-lab.../compiled_data/banc_888/influence/all_to_all (orders of
# magnitude faster than the per-neuron influence loop above).

.run_pr10k <- isTRUE(as.logical(Sys.getenv("BANC_PR_10K", "FALSE")))
if (!.run_pr10k) {
  message("\n=== PR/RPR 10k-seed validation — SKIPPED (set BANC_PR_10K=1) ===")
} else {
  message("\n=== PR/RPR 10k-seed validation — RUNNING (streaming via GCS) ===")
}
# Gated via BANC_PR_10K env var (2026-05-15). Streams influence pairs from
# the all_to_all parquet directory on GCS using query_parquet_gcs_isin() so
# no local cache is required (same approach as panel_all_to_all_influence.R).
# Produces a 10k-seed version of influence_norm_log_vs_direct_connectivity.pdf
# that is far less gerrymandered than the 1k-seed default block above.
if (.run_pr10k) {
if (!exists("banc.edgelist.raw")) {
  message("[PR-10k] reloading edgelist cache for direct-connectivity lookup...")
  banc.edgelist.raw <- arrow::read_feather(.banc_edgelist_cache)
  banc.edgelist.raw$pre  <- as.character(banc.edgelist.raw$pre)
  banc.edgelist.raw$post <- as.character(banc.edgelist.raw$post)
}
.pr_ids <- as.character(banc.meta %>%
  dplyr::filter(as.logical(proofread) %in% TRUE |
                  as.logical(roughly_proofread) %in% TRUE) %>%
  dplyr::pull(root_id) %>% unique())
message(sprintf("PR/RPR pool size: %d", length(.pr_ids)))

set.seed(2026)
.pr10k_seeds <- if (length(.pr_ids) > 10000L) sample(.pr_ids, 10000L) else .pr_ids
message(sprintf("Sampled seeds: %d", length(.pr10k_seeds)))

# Direct connectivity per seed (subset of the existing edgelist).
.pr_dconn_list <- .build_direct_conn(.pr10k_seeds, .pr_ids)
message(sprintf("Seeds with direct PR/RPR targets: %d", length(.pr_dconn_list)))

# Batch the seed pool: 10k × 150k targets = 1.5B rows would not fit in
# memory, so we slice into 100-seed batches (~150M rows each = ~few GB)
# and accumulate histograms incrementally.
.batch_size <- 100L
.seed_batches <- split(.pr10k_seeds,
                        ceiling(seq_along(.pr10k_seeds) / .batch_size))
conn_hist_pr     <- numeric(n_bins)
not_conn_hist_pr <- numeric(n_bins)

message(sprintf("Querying all_to_all influence (streaming from GCS) across %d batches (%d seeds each) ...",
                length(.seed_batches), .batch_size))
# Stream from gs://lee-lab_.../compiled_data/banc_888/influence/all_to_all/
# via pyarrow predicate-pushdown rather than downloading the full ~287 GB
# cache. Same approach as panel_all_to_all_influence.R; no local disk
# required.
source("R/startup/gcs-helpers.R")
.a2a_gcs_dir <- file.path(banc.gcs.bucket, banc.version, "influence", "all_to_all")
.t0_pr <- Sys.time()
for (.b in seq_along(.seed_batches)) {
  .seed_b <- .seed_batches[[.b]]
  inf_b <- query_parquet_gcs_isin(
    gcs_dir       = .a2a_gcs_dir,
    upstream_ids  = .seed_b,
    columns       = c("upstream_id", "downstream_id", "raw_influence"),
    upstream_col  = "upstream_id"
  )
  dt_b <- data.table::as.data.table(inf_b)
  dt_b <- dt_b[downstream_id %in% .pr_ids]    # restrict to PR/RPR targets
  dt_b <- dt_b[upstream_id != downstream_id]  # drop self-loops
  # Adjusted influence = max(0, log(raw) + const). Const = 24 matches the
  # paper convention; floor at zero is the bancr default.
  dt_b[, adjusted_influence := pmax(0, log(raw_influence) + 24)]
  dt_b <- dt_b[adjusted_influence >= floor_cutoff]

  # Tag direct vs indirect using this batch's seeds only.
  pairs_b <- data.table::rbindlist(
    lapply(intersect(.seed_b, names(.pr_dconn_list)), function(s) {
      data.table::data.table(upstream_id = s,
                              downstream_id = .pr_dconn_list[[s]])
    }), fill = TRUE
  )
  if (nrow(pairs_b) > 0) {
    pairs_b[, direct := TRUE]
    dt_b[pairs_b, direct := i.direct,
         on = c("upstream_id", "downstream_id")]
  } else {
    dt_b[, direct := NA]
  }
  dt_b[, direct := !is.na(direct)]

  conn_hist_pr <- conn_hist_pr +
    dt_b[direct == TRUE,
         tabulate(findInterval(adjusted_influence, bin_breaks, left.open = TRUE),
                  nbins = n_bins)]
  not_conn_hist_pr <- not_conn_hist_pr +
    dt_b[direct == FALSE,
         tabulate(findInterval(adjusted_influence, bin_breaks, left.open = TRUE),
                  nbins = n_bins)]
  message(sprintf("  batch %d/%d (%d seeds): %d direct, %d indirect rows",
                  .b, length(.seed_batches), length(.seed_b),
                  sum(dt_b$direct == TRUE),
                  sum(dt_b$direct == FALSE)))
  rm(inf_b, dt_b, pairs_b); gc()
}
message(sprintf("PR/RPR validation done in %.1f min: %d direct, %d indirect total",
                as.numeric(Sys.time() - .t0_pr, units = "mins"),
                sum(conn_hist_pr), sum(not_conn_hist_pr)))

# Build plotting dataframe (single facet, matches the existing helper).
influence.pr.db <- .histograms_to_df(conn_hist_pr, not_conn_hist_pr,
                                       "PR/RPR (10k sample)")

inf.metric <- inf.primary.metric
influence.pr.db$influence_score <- influence.pr.db[[inf.metric]]
g.banc.check.pr <- ggplot2::ggplot(
  influence.pr.db,
  ggplot2::aes(x = influence_score, color = connectivity, group = connectivity)
) +
  ggplot2::geom_density(aes(y = after_stat(scaled)), size = 1.2, na.rm = TRUE) +
  ggplot2::scale_color_manual(
    values = c("not connected" = "lightgrey", "connected" = highlight.col),
    name = "Direct connectivity"
  ) +
  ggplot2::facet_wrap(~ facet, ncol = 1, scales = "fixed") +
  ggplot2::labs(
    x = inf.metric,
    y = "scaled density (max=1)",
    color = "direct connectivity"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.position = "none",
    axis.title = ggplot2::element_text(size = 18),
    axis.text  = ggplot2::element_text(size = 16),
    plot.title = ggplot2::element_text(hjust = 0.5, size = 14),
    strip.text = ggplot2::element_text(size = 14)
  )

.pr_pdf <- file.path(banc.fig2.extra.path,
  sprintf("%s_vs_direct_connectivity_pr_10k.pdf", inf.metric))
ggsave(g.banc.check.pr, filename = .pr_pdf, width = 6, height = 2.5, dpi = 300)
message("Wrote: ", .pr_pdf)
ggsave(convert_to_dark_mode(g.banc.check.pr),
       filename = file.path(banc.fig2.darkmode.path,
         sprintf("dark_mode_%s_vs_direct_connectivity_pr_10k.pdf", inf.metric)),
       width = 6, height = 2.5, dpi = 300)

# Sidecar quantile summary for this PR/RPR variant.
.pr_sidecar_path <- file.path(banc.fig2.extra.path,
  sprintf("%s_vs_direct_connectivity_pr_10k.txt", inf.metric))
.conn_scores_pr    <- influence.pr.db[[inf.metric]][influence.pr.db$connectivity == "connected"]
.notconn_scores_pr <- influence.pr.db[[inf.metric]][influence.pr.db$connectivity == "not connected"]
.pr_sidecar_lines <- c(
  sprintf("Influence validation sidecar — %s (PR/RPR 10k sample, GCS cache)", inf.metric),
  sprintf("Date: %s", format(Sys.time(), "%Y-%m-%d %H:%M")),
  sprintf("Seed sample: %d / %d PR+RPR neurons",
          length(.pr10k_seeds), length(.pr_ids)),
  sprintf("Target set:  %d PR+RPR neurons", length(.pr_ids)),
  sprintf("Direct interactions:   %d", length(.conn_scores_pr)),
  sprintf("Indirect interactions: %d", length(.notconn_scores_pr)),
  ""
)
if (length(.conn_scores_pr) > 0) {
  .q <- stats::quantile(.conn_scores_pr, probs = c(0.25, 0.50, 0.75), na.rm = TRUE)
  .pr_sidecar_lines <- c(.pr_sidecar_lines,
    "Quantile thresholds of the DIRECT distribution:",
    sprintf("  25th pctl: %.4f  50th pctl: %.4f  75th pctl: %.4f",
            .q[1], .q[2], .q[3]),
    sprintf("  Direct   >= 25th (%.4f):  %d / %d (%.1f%%)",
            .q[1], sum(.conn_scores_pr    >= .q[1]), length(.conn_scores_pr),
            100 * sum(.conn_scores_pr    >= .q[1]) / length(.conn_scores_pr)),
    sprintf("  Indirect >= 25th (%.4f):  %d / %d (%.1f%%)",
            .q[1], sum(.notconn_scores_pr >= .q[1]), length(.notconn_scores_pr),
            100 * sum(.notconn_scores_pr >= .q[1]) / max(1, length(.notconn_scores_pr))),
    sprintf("  Direct   >= 50th (%.4f):  %d / %d (%.1f%%)",
            .q[2], sum(.conn_scores_pr    >= .q[2]), length(.conn_scores_pr),
            100 * sum(.conn_scores_pr    >= .q[2]) / length(.conn_scores_pr)),
    sprintf("  Indirect >= 50th (%.4f):  %d / %d (%.1f%%)",
            .q[2], sum(.notconn_scores_pr >= .q[2]), length(.notconn_scores_pr),
            100 * sum(.notconn_scores_pr >= .q[2]) / max(1, length(.notconn_scores_pr))),
    sprintf("  Direct   >= 75th (%.4f):  %d / %d (%.1f%%)",
            .q[3], sum(.conn_scores_pr    >= .q[3]), length(.conn_scores_pr),
            100 * sum(.conn_scores_pr    >= .q[3]) / length(.conn_scores_pr)),
    sprintf("  Indirect >= 75th (%.4f):  %d / %d (%.1f%%)",
            .q[3], sum(.notconn_scores_pr >= .q[3]), length(.notconn_scores_pr),
            100 * sum(.notconn_scores_pr >= .q[3]) / max(1, length(.notconn_scores_pr)))
  )
}
writeLines(.pr_sidecar_lines, .pr_sidecar_path)
message("Wrote sidecar: ", .pr_sidecar_path)
rm(conn_hist_pr, not_conn_hist_pr, influence.pr.db,
   g.banc.check.pr, .conn_scores_pr, .notconn_scores_pr); gc()
}  # end if (.run_pr10k) — gated on BANC_PR_10K env var





