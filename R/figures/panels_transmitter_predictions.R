#' panels_transmitter_predictions.R — neurotransmitter prediction panels (ED Fig. 3)
#'
#' Cross-dataset evaluation of the BANC per-synapse neurotransmitter
#' classifier: confidence densities, per-super-class ground-truth bars,
#' predicted-NT stacked bars by super_class × region, and cell-type-level
#' confusion matrices against FAFB, hemibrain, and MANC.
#'
#' @section Reads:
#'   * banc.meta                              via R/startup/banc-meta.R
#'   * franken.meta                           via R/startup/franken-meta.R
#'   * Per-neuron NT columns in banc.meta: neurotransmitter_predicted,
#'     neurotransmitter_score, neurotransmitter_verified.
#'
#' @section Writes:
#'   * figures/figure_1/links/supplement/banc_nt_groundtruth_by_superclass.pdf
#'   * figures/figure_1/links/supplement/banc_nt_confusion.pdf
#'   * figures/figure_1/links/supplement/banc_nt_prediction_score_densities.pdf
#'   * figures/figure_1/links/supplement/banc_nt_predicted_by_superclass_by_region.pdf
#'   * figures/figure_1/links/supplement/banc_nt_confusion_celltype_BANC_vs_{FAFB,Hemibrain,MANC}_colnorm.pdf
#'   * figures/figure_1/links/extra/*_rownorm.pdf  (row-normalised variants)
#'
#' @section Paper:
#'   * ED Fig. 3a–e — the five panels above.
#'   * Methods §"Neurotransmitter prediction". Per-neuron call =
#'     argmax of summed per-NT probabilities across the neuron's
#'     presynaptic sites; classifier from Drugowitsch lab, transferred
#'     from Eckstein et al. 2024 *Cell*.
#'
#' @section Reproduce: BANC_NCORES=1 Rscript R/figures/panels_transmitter_predictions.R

source("R/startup/banc-startup.R")
source("R/startup/franken-meta.R")
source("R/startup/banc-meta.R")

########################
## OUTPUT DESTINATIONS ##
########################
# Keep with Figure 1 auxiliaries; create if needed
banc.fig1.path       <- "figures/figure_1/links"
banc.fig1.extra.path <- file.path(banc.fig1.path, "extra")
banc.fig1.supp.path  <- file.path(banc.fig1.path, "supplement")
if (!dir.exists(banc.fig1.extra.path)) dir.create(banc.fig1.extra.path, recursive = TRUE)
if (!dir.exists(banc.fig1.supp.path))  dir.create(banc.fig1.supp.path,  recursive = TRUE)

#############################
## HELPER: NT CLEAN & COLORS
#############################
.clean_nt <- function(x) {
  x <- tolower(as.character(x))
  x <- gsub(";.*$", "", x)  # strip multi-assignments
  x <- dplyr::case_when(
    is.na(x) ~ "undetermined",
    x %in% c("na","none","0","unknown","unclear","undetermined") ~ "undetermined",
    x == "gaba" ~ "gaba",
    x %in% c("glutamate","glu") ~ "glutamate",
    x %in% c("acetylcholine","ach","cholinergic") ~ "acetylcholine",
    x %in% c("dopamine","da","dopaminergic") ~ "dopamine",
    x %in% c("octopamine","oa","octopaminergic") ~ "octopamine",
    x %in% c("serotonin","5ht","serotonergic") ~ "serotonin",
    x %in% c("histamine","his","histaminergic") ~ "histamine",
    TRUE ~ x
  )
  x
}

# Function to process neurotransmitter_verified column
filter_words <- function(input_string, words_to_keep, invert = FALSE){
  words <- unlist(strsplit(input_string, ",|, |;|; "))
  words <- gsub("^ | $","",words)
  if (invert){
    filtered_words <- words[! words %in% words_to_keep]
  }else{
    filtered_words <- words[words %in% words_to_keep]
  }
  paste(unique(filtered_words), collapse = ", ")
}

# Canonical level order (append undetermined at end)
nt_levels <- c("acetylcholine","glutamate","gaba","serotonin","dopamine","octopamine","histamine", "tyramine")

# Cerise-Limon categorical palette (used in several figure scripts)
cerise_limon_base    <- c("#EE5B32", "#F6B83C", "#4BA747", "#5BB6E4", "#7C378A")
cerise_limon_palette <- grDevices::colorRampPalette(cerise_limon_base)
nt_vals              <- unique(nt_levels)
nt_cols              <- stats::setNames(cerise_limon_palette(length(nt_vals)), nt_vals)

# Heatmap gradient (matches your warm/cool range used elsewhere)
heatmap_palette <- grDevices::colorRampPalette(
  c("#1f4e79", "#2e6f95", "#4a8abf", "#7ba7bc", "#a67c8a", "#c4967d", "#b22222")
)

###########################################
## (A) BANC GROUND-TRUTH (VERIFIED) SUMMARY
###########################################
gt.data <- read_csv("/Users/GD/LMBD/Papers/synister/drosophila_neurotransmitters/gt_data.csv", 
                    col_types = banc.col.types)
nt_cols <- base::intersect(
  c("acetylcholine","glutamate","gaba","dopamine",
    "serotonin","octopamine","tyramine","histamine"),
  names(gt.data)
)
gt.df <- gt.data %>%
  dplyr::select(cell_type, dplyr::all_of(nt_cols)) %>%
  tidyr::pivot_longer(cols = dplyr::all_of(nt_cols),
                      names_to = "neurotransmitter", 
                      values_to = "value") %>%
  dplyr::group_by(cell_type, neurotransmitter) %>%
  dplyr::summarise(has_pos = any(value > 0, na.rm = TRUE), 
                   .groups = "drop") %>%
  dplyr::filter(has_pos) %>%
  dplyr::group_by(cell_type) %>%
  dplyr::filter(dplyr::n() == 1L) %>%        
  dplyr::ungroup() %>%
  dplyr::transmute(
    cell_type,
    neurotransmitter_verified = neurotransmitter
  )
banc_gt <- banc.meta %>%
  dplyr::select(-neurotransmitter_verified) %>%
  dplyr::left_join(gt.df, by = "cell_type") %>%
  dplyr::filter(!is.na(neurotransmitter_verified), 
                !is.na(super_class),
                !is.na(region),
                !is.na(cell_type),
                !super_class %in% c("glia","not_a_neuron","trachea","motor","visceral_circulatory","ascending_visceral_circulatory"),
                neurotransmitter_verified != "undetermined") %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    super_class = as.character(super_class),
    neurotransmitter_verified = filter_words(neurotransmitter_verified, nt_levels, invert = FALSE),
    nt_gt = .clean_nt(neurotransmitter_verified),
    super_class = gsub("_"," ", super_class)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(nt_gt), 
                !grepl("\\,",nt_gt), 
                nt_gt != "undetermined", 
                nt_gt != "") 

# Order transmitter groups by their overall proportion (descending)
nt_order <- banc_gt %>%
  dplyr::count(nt_gt, name = "n") %>%
  dplyr::mutate(prop = n / sum(n)) %>%
  dplyr::arrange(dplyr::desc(prop)) %>%
  dplyr::pull(nt_gt)

banc_gt <- banc_gt %>%
  dplyr::mutate(
    nt_gt = forcats::fct_drop(factor(nt_gt, levels = nt_order))
  ) %>%
  dplyr::add_count(super_class, name = "n_per_super") %>%
  # Drop super_class categories with fewer than 10 ground-truth neurons
  # (small categories add noise without informing the bar comparison).
  dplyr::filter(n_per_super >= 10) %>%
  dplyr::mutate(
    # Order super classes by total count (descending)
    super_class = forcats::fct_reorder(super_class, n_per_super, .desc = TRUE)
  )

# Data for labels at the top of each bar
super_totals <- banc_gt %>%
  dplyr::distinct(super_class, n_per_super)

# Make plot
p_gt_bar <- ggplot2::ggplot(banc_gt, ggplot2::aes(x = super_class, fill = nt_gt)) +
  ggplot2::geom_bar(position = "fill") +
  # headroom for labels above the bars
  ggplot2::scale_y_continuous(
    breaks = seq(0, 1, by = 0.25),
    expand = ggplot2::expansion(mult = c(0, 0.08))
  ) +
  ggplot2::scale_fill_manual(values = paper.cols, drop = FALSE) +
  ggplot2::labs(x = NULL, y = NULL, fill = "verified neurotransmitter", title = "") +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.position = "none",
    axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 1, size = 9),
    axis.text.y = ggplot2::element_text(size = 9),
    legend.text = ggplot2::element_text(size = 9),
    legend.title = ggplot2::element_text(size = 9),
    plot.margin = ggplot2::margin(t = 10, r = 10, b = 10, l = 10)
  ) +
  # totals above each bar (uses y>1 due to expand headroom)
  ggplot2::geom_text(
    data = super_totals,
    ggplot2::aes(x = super_class, y = 1.02, label = scales::comma(n_per_super)),
    inherit.aes = FALSE, vjust = 0, size = 3.5
  ) +
  ggplot2::coord_cartesian(clip = "off")

# Save
ggplot2::ggsave(
  filename = file.path(banc.fig1.supp.path, "banc_nt_groundtruth_by_superclass.pdf"),
  plot = p_gt_bar, width = 6, height = 7, dpi = 300
)

############################
### NT prediction matrix ###
############################

banc.nt.counts.csv <- "data/synapse_nt/v2/nt_prediction_confusion_matrix_on_gt_normalized_22072025_test_set.csv"
banc.nt.confusion <- readr::read_csv(banc.nt.counts.csv)
nt.cols <- c("acetylcholine", "glutamate", "GABA", "dopamine", "serotonin", "octopamine", "tyramine", "histamine")

# Prepare matrix: set rownames, remove first column
mat_counts <- as.data.frame(banc.nt.confusion)
rownames(mat_counts) <- mat_counts[[1]]
mat_counts <- mat_counts[ , -1]
mat_counts <- as.matrix(mat_counts)[nt.cols,nt.cols]

# Row-normalize to get proportions [0, 1]
mat_pct <- t(apply(mat_counts, 1, function(x) x / sum(x)))

# Create cell labels: 0.XX\n(count)
labels <- matrix(
  paste0(sprintf("%.2f", as.numeric(mat_pct)), "\n(", as.integer(mat_counts), ")"),
  nrow = nrow(mat_counts),
  ncol = ncol(mat_counts),
  dimnames = dimnames(mat_counts)
)

# Scaled color palette
n_breaks <- 100
scaled_heatmap_palette <- colorRampPalette(
  c("#1f4e79", "#4a90a4", "#7ba7bc", "#a67c8a", "#c4967d", "#b22222")
)(n_breaks - 1)

# Plot with pheatmap
pheatmap::pheatmap( 
  mat_pct,
  color = scaled_heatmap_palette,
  display_numbers = labels,
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  fontsize_number = 10,
  height = 6,
  width = 6,
  cellwidth = 36,
  cellheight = 36,
  fontsize_col = 12,
  fontsize_row = 12,
  border_color = NA,
  number_color = "white", 
  filename = file.path(banc.fig1.supp.path, "banc_nt_confusion.pdf")
)

###########################################################
## (B2) BANC PREDICTED NT by super_class, facetted by region
###########################################################

# Pick region column if present; otherwise everything becomes CNS
region_col <- if ("region" %in% names(banc.meta)) "region" else if ("cns_region" %in% names(banc.meta)) "cns_region" else NULL
region_levels <- c("optic_lobe","central_brain","ventral_nerve_cord","CNS")
region_levels_disp <- c("optic lobe","central brain","ventral nerve cord","CNS")
banc_pred <- banc.meta %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    pred_raw = ifelse(is.na(neurotransmitter_predicted), "", neurotransmitter_predicted),
    nt_pred = neurotransmitter_predicted
  ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(
    proofread == TRUE,
    region != "brain",
    region != "neck_connective",   # retired from v888 SeaTable; drop any stragglers
    !super_class %in% c("glia","not_a_neuron","trachea","visceral_circulatory","motor","ascending_visceral_circulatory"),
    !is.na(super_class),
    !is.na(nt_pred),
    nt_pred != "undetermined",
    nt_pred != "uncertain",
    nt_pred != "",
    !grepl(",", nt_pred),
    nt_pred %in% nt_levels
  ) %>%
  dplyr::mutate(
    score_for_pred = neurotransmitter_score/max(neurotransmitter_score,na.rm=TRUE),
    nt_pred = dplyr::case_when(
      is.na(cell_class) ~ nt_pred,
      #cell_class=="kenyon_cell" ~ "kenyon_cell",
      TRUE ~ nt_pred
    ) 
  )

# Add CNS aggregate as another "region" AND collapse to single bar 'CNS'
banc_pred_all <- dplyr::bind_rows(
  banc_pred,
  banc_pred %>% dplyr::mutate(region = "CNS", 
                              super_class = "CNS")
) %>%
  dplyr::mutate(
    region = factor(region, levels = region_levels)
  )

# Order transmitter stacks by overall frequency (like A)
nt_order_pred <- banc_pred_all %>%
  dplyr::count(nt_pred, name = "n") %>%
  dplyr::arrange(dplyr::desc(n)) %>%
  dplyr::pull(nt_pred)

banc_pred_all <- banc_pred_all %>%
  dplyr::mutate(
    nt_pred = forcats::fct_drop(factor(nt_pred, levels = nt_order_pred))
  )

# Order super_class globally by totals OUTSIDE the CNS aggregate (stable across facets)
super_order <- banc_pred %>%                                   # <- use non-aggregate data
  dplyr::count(super_class, name = "n") %>%
  dplyr::arrange(dplyr::desc(n)) %>%
  dplyr::pull(super_class)

banc_pred_all <- banc_pred_all %>%
  dplyr::mutate(
    super_class = forcats::fct_relevel(super_class, super_order)
  )

# Display-only columns: replace "_" with " "
banc_pred_all <- banc_pred_all %>%
  dplyr::mutate(
    super_class_disp = gsub("_"," ", as.character(super_class)),
    region_disp      = gsub("_"," ", as.character(region)),
    region_disp      = factor(region_disp, levels = region_levels_disp)
  )

# Per-bar totals for labels, per (region_disp, super_class_disp)
bar_totals <- banc_pred_all %>%
  dplyr::count(region_disp, super_class_disp, name = "n_per_super")

# Drop categories with fewer than 10 neurons (per-region per-super_class)
.kept <- bar_totals %>% dplyr::filter(n_per_super >= 10)
banc_pred_all <- banc_pred_all %>%
  dplyr::semi_join(.kept, by = c("region_disp", "super_class_disp"))
bar_totals <- .kept

# Color mapping as in A (safe for any extra levels like 'kenyon_cell')
fill_vals <- if (!is.null(names(paper.cols))) {
  vals <- paper.cols[levels(banc_pred_all$nt_pred)]
  # optional fallback if some levels not in paper.cols
  vals[is.na(vals)] <- "#999999"
  stats::setNames(vals, levels(banc_pred_all$nt_pred))
} else {
  stats::setNames(paper.cols[seq_along(levels(banc_pred_all$nt_pred))], levels(banc_pred_all$nt_pred))
}

p_pred_bar <- ggplot2::ggplot(banc_pred_all, ggplot2::aes(x = super_class_disp, fill = nt_pred)) +
  ggplot2::geom_bar(position = "fill") +
  ggplot2::scale_y_continuous(
    breaks = seq(0, 1, by = 0.25),
    expand = ggplot2::expansion(mult = c(0, 0.08))
  ) +
  ggplot2::scale_fill_manual(values = fill_vals, drop = FALSE) +
  ggplot2::labs(x = NULL, y = NULL, fill = "predicted neurotransmitter", title = "") +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.position = "none",
    axis.text.x  = ggplot2::element_text(angle = 35, hjust = 1, vjust = 1, size = 10),
    axis.text.y  = ggplot2::element_text(size = 10),
    legend.text  = ggplot2::element_text(size = 10),
    legend.title = ggplot2::element_text(size = 10),
    plot.margin  = ggplot2::margin(t = 10, r = 10, b = 10, l = 10),
    strip.text   = ggplot2::element_text(size = 10, face = "bold")
  ) +
  # totals above each bar within each facet
  ggplot2::geom_text(
    data = bar_totals,
    ggplot2::aes(x = super_class_disp, y = 1.02, label = scales::comma(n_per_super)),
    inherit.aes = FALSE, vjust = 0, size = 3.5
  ) +
  ggplot2::coord_cartesian(clip = "off") +
  ggplot2::facet_grid(~ region_disp, scales = "free_x", space = "free_x")

# Save
print(p_pred_bar)
ggplot2::ggsave(
  filename = file.path(banc.fig1.supp.path, "banc_nt_predicted_by_superclass_by_region.pdf"),
  plot = p_pred_bar, width = 14, height = 5, dpi = 300
)

###################################################
## (B) PREDICTION CONFIDENCE DENSITIES (BY NT PRED)
###################################################
banc_pred <- banc.meta %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    pred_raw = ifelse(is.na(neurotransmitter_predicted), "", neurotransmitter_predicted),
    nt_pred = neurotransmitter_predicted
  ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(
    proofread == TRUE,
    region != "brain",
    flow != "efferent",
    #cell_class != "kenyon_cell",
    !super_class %in% c("glia","not_a_neuron","trachea","visceral_circulatory","motor","ascending_visceral_circulatory"),
    !is.na(super_class),
    !is.na(nt_pred),
    nt_pred != "undetermined",
    nt_pred != "uncertain",
    nt_pred != "",
    !grepl(",", neurotransmitter_predicted),
    nt_pred %in% nt_levels
  ) %>%
  dplyr::mutate(
    score_for_pred = neurotransmitter_score/max(neurotransmitter_score,na.rm=TRUE),
  )

# Order legend/lines by overall frequency (descending)
nt_order_pred <- banc_pred %>%
  dplyr::count(nt_pred, name = "n") %>%
  dplyr::arrange(dplyr::desc(n)) %>%
  dplyr::pull(nt_pred)

banc_pred <- banc_pred %>%
  dplyr::mutate(
    nt_pred = forcats::fct_drop(factor(nt_pred, levels = nt_order_pred))
  )
# Color mapping (unchanged)
col_vals <- if (!is.null(names(paper.cols))) {
  paper.cols[levels(banc_pred$nt_pred)]
} else {
  stats::setNames(paper.cols[seq_along(levels(banc_pred$nt_pred))], levels(banc_pred$nt_pred))
}

p_nt_density <- ggplot2::ggplot(
  banc_pred,
  ggplot2::aes(x = score_for_pred, colour = nt_pred)
) +
  # NORMALIZED: peak=1 per predicted NT
  ggplot2::geom_density(ggplot2::aes(y = ggplot2::after_stat(scaled)), size = 1) +
  ggplot2::scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) +
  ggplot2::scale_color_manual(values = col_vals, drop = FALSE) +
  ggplot2::labs(
    x = "prediction score",
    y = "normalized density (peak = 1)",
    colour = "predicted neurotransmitter",
    title = ""
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.position = "none",
    axis.title.x = ggplot2::element_text(size = 12),
    axis.title.y = ggplot2::element_text(size = 12),
    legend.text  = ggplot2::element_text(size = 12),
    legend.title = ggplot2::element_text(size = 12)
  )

ggplot2::ggsave(
  filename = file.path(banc.fig1.supp.path, "banc_nt_prediction_score_densities.pdf"),
  plot = p_nt_density, width = 5, height = 4, dpi = 300
)

######################################################################
## (C) CROSS-DATASET NT AGREEMENT (CONFUSION HEATMAPS; BANC vs FAFB/MANC)
######################################################################

# Wrangle predictions
bc.meta <- banc.meta %>%
  dplyr::filter(!is.na(neurotransmitter_predicted),
                neurotransmitter_predicted %in% nt_levels) %>%
  dplyr::filter(
    proofread == TRUE,
    region != "brain",
    flow != "efferent",
    #cell_class != "kenyon_cell",
    !super_class %in% c("glia","not_a_neuron","trachea","visceral_circulatory","motor","ascending_visceral_circulatory"),
    !is.na(super_class),
    !is.na(neurotransmitter_predicted),
    neurotransmitter_predicted != "undetermined",
    neurotransmitter_predicted != "uncertain",
    neurotransmitter_predicted != "",
    !grepl(",", neurotransmitter_predicted),
    neurotransmitter_predicted %in% nt_levels
  ) %>%
  dplyr::distinct(id = root_id, cell_type, fafb_cell_type, manc_cell_type, hemibrain_cell_type, neurotransmitter_predicted, dataset = "BANC")
hb.meta <- read_csv("data/meta/hemibrain_nt_meta_2024-02-01.csv", col_types = banc.col.types) %>%
  dplyr::mutate(neurotransmitter_predicted = top_nt) %>%
  dplyr::filter(!is.na(neurotransmitter_predicted), !is.na(cell_type),
                neurotransmitter_predicted %in% nt_levels) %>%
  dplyr::distinct(id = bodyid, cell_type,neurotransmitter_predicted, dataset = "hemibrain")
fw.meta <- franken.meta %>%
  dplyr::mutate(neurotransmitter_predicted = top_nt) %>%
  dplyr::filter(!is.na(fafb_id), !is.na(cell_type),
                !is.na(neurotransmitter_predicted),
                neurotransmitter_predicted %in% nt_levels) %>%
  dplyr::distinct(id = fafb_id, cell_type,neurotransmitter_predicted, dataset = "FAFB")
mc.meta <- franken.meta %>%
  dplyr::mutate(neurotransmitter_predicted = top_nt) %>%
  dplyr::filter(!is.na(manc_id), !is.na(cell_type),
                !is.na(neurotransmitter_predicted),
                neurotransmitter_predicted %in% c("acetylcholine","gaba","glutamate"),
                neurotransmitter_predicted %in% nt_levels) %>%
  dplyr::distinct(id = manc_id, cell_type,neurotransmitter_predicted, dataset = "MANC")
# maleCNS
source("R/startup/gcs-helpers.R")
malecns.gcs.path <- "gs://lee-lab_brain-and-nerve-cord-fly-connectome/compiled_data/malecns_09"
malecns.nt.meta <- read_feather_gcs(file.path(malecns.gcs.path, "malecns_09_meta.feather")) %>%
  dplyr::filter(!is.na(cell_type), cell_type != "",
                !is.na(neurotransmitter_predicted),
                neurotransmitter_predicted %in% nt_levels) %>%
  dplyr::distinct(id = malecns_09_id, cell_type, neurotransmitter_predicted, dataset = "maleCNS")

# Clean to canonical transmitter tokens (lowercase/collapse) for safety
canonize_nt <- function(df) {
  df %>%
    dplyr::mutate(
      nt_pred = .clean_nt(neurotransmitter_predicted)
    ) %>%
    dplyr::filter(!is.na(nt_pred), nt_pred %in% nt_levels) %>%
    dplyr::select(dplyr::any_of(c("id","cell_type","fafb_cell_type","manc_cell_type","hemibrain_cell_type")), nt_pred)
}

banc_df  <- canonize_nt(bc.meta)
fw_df    <- canonize_nt(fw.meta)
mc_df    <- canonize_nt(mc.meta)
hb_df    <- canonize_nt(hb.meta)
mcns_df  <- canonize_nt(malecns.nt.meta)

# ---------- helpers ----------
modal_per_celltype <- function(df) {
  # ties broken by your nt_levels order
  df %>%
    dplyr::filter(!is.na(cell_type)) %>%
    dplyr::count(cell_type, nt_pred, name = "n") %>%
    dplyr::group_by(cell_type) %>%
    dplyr::arrange(dplyr::desc(n), match(nt_pred, nt_levels), .by_group = TRUE) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(cell_type, nt_mode = nt_pred)
}

# pheatmap-style confusion with row-normalized percentages
build_confusion_celltype <- function(banc_mode, other_mode, axis_levels = nt_levels) {
  common_ct <- base::intersect(banc_mode$cell_type, other_mode$cell_type)
  
  joined <- banc_mode %>%
    dplyr::filter(cell_type %in% common_ct) %>%
    dplyr::rename(nt_mode_banc = nt_mode) %>%
    dplyr::inner_join(
      other_mode %>% dplyr::filter(cell_type %in% common_ct) %>% dplyr::rename(nt_mode_other = nt_mode),
      by = "cell_type"
    )
  
  counts <- joined %>%
    dplyr::transmute(
      nt_banc  = factor(nt_mode_banc,  levels = axis_levels),
      nt_other = factor(nt_mode_other, levels = axis_levels)
    ) %>%
    dplyr::count(nt_banc, nt_other, name = "n")
  
  mat_counts <- stats::xtabs(n ~ nt_banc + nt_other, data = counts)
  mat_counts <- mat_counts[axis_levels, axis_levels, drop = FALSE]
  
  # Row- and column-normalized percentages
  rs <- rowSums(mat_counts)
  cs <- colSums(mat_counts)
  
  mat_pct_row <- matrix(0, nrow = nrow(mat_counts), ncol = ncol(mat_counts),
                        dimnames = dimnames(mat_counts))
  for (i in seq_len(nrow(mat_counts))) {
    if (rs[i] > 0) mat_pct_row[i, ] <- mat_counts[i, ] / rs[i]
  }

  mat_pct_col <- matrix(0, nrow = nrow(mat_counts), ncol = ncol(mat_counts),
                        dimnames = dimnames(mat_counts))
  for (j in seq_len(ncol(mat_counts))) {
    if (cs[j] > 0) mat_pct_col[, j] <- mat_counts[, j] / cs[j]
  }
  
  # Pretty display names (uppercase GABA for display only)
  disp <- function(x) { x2 <- x; x2[x2 == "gaba"] <- "GABA"; x2 }
  rownames(mat_counts) <- disp(rownames(mat_counts))
  colnames(mat_counts) <- disp(colnames(mat_counts))
  rownames(mat_pct_row) <- disp(rownames(mat_pct_row))
  colnames(mat_pct_row) <- disp(colnames(mat_pct_row))
  rownames(mat_pct_col) <- disp(rownames(mat_pct_col))
  colnames(mat_pct_col) <- disp(colnames(mat_pct_col))
  
  # Label matrices: "0.XX\n(count)"
  labels_row <- matrix(
    paste0(sprintf("%.2f", as.numeric(mat_pct_row)), "\n(", as.integer(mat_counts), ")"),
    nrow = nrow(mat_counts), ncol = ncol(mat_counts),
    dimnames = dimnames(mat_counts)
  )
  labels_col <- matrix(
    paste0(sprintf("%.2f", as.numeric(mat_pct_col)), "\n(", as.integer(mat_counts), ")"),
    nrow = nrow(mat_counts), ncol = ncol(mat_counts),
    dimnames = dimnames(mat_counts)
  )
  
  list(
    mat_counts   = mat_counts,
    mat_pct_row  = mat_pct_row,
    mat_pct_col  = mat_pct_col,
    labels_row   = labels_row,
    labels_col   = labels_col
  )
}

plot_heatmap_conf <- function(conf, outfile,
                              rows_name = "BANC", cols_name = "FAFB",
                              normalize = c("row","col")) {
  normalize <- match.arg(normalize)
  
  palette <- grDevices::colorRampPalette(
    c("#1f4e79", "#4a90a4", "#7ba7bc", "#a67c8a", "#c4967d", "#b22222")
  )(99)
  
  mat <- if (normalize == "row") conf$mat_pct_row else conf$mat_pct_col
  lab <- if (normalize == "row") conf$labels_row  else conf$labels_col
  
  n_ct <- sum(conf$mat_counts)  # number of overlapping cell types
  
  main_title <- paste0(
    "Rows: ", rows_name, " cell-type modal NT  \n  ",
    "Columns: ", cols_name, " cell-type modal NT  \n  ",
    if (normalize == "row") "Row-normalized proportion" else "Column-normalized proportion", "  \n  ",
    "n = ", scales::comma(n_ct), " cell types"
  )
  
  pheatmap::pheatmap(
    mat,
    color           = palette,
    display_numbers = lab,
    cluster_rows    = FALSE,
    cluster_cols    = FALSE,
    fontsize_number = 10,
    height          = 6,
    width           = 6,
    cellwidth       = 36,
    cellheight      = 36,
    fontsize_col    = 12,
    fontsize_row    = 12,
    border_color    = NA,
    number_color    = "white",
    filename        = outfile,
    main            = main_title
  )
}

# ---------- map BANC neurons to each dataset's cell_type namespace ----------
# FAFB precedence: cell_type > fafb_cell_type > hemibrain_cell_type
banc_fafb_ct <- banc_df %>%
  dplyr::mutate(cell_type = dplyr::case_when(
    cell_type %in% fw_df$cell_type ~ cell_type,
    fafb_cell_type %in% fw_df$cell_type ~ fafb_cell_type,
    hemibrain_cell_type %in% fw_df$cell_type ~ hemibrain_cell_type,
    TRUE ~ NA_character_
  )) %>%
  dplyr::filter(!is.na(cell_type), cell_type %in% fw_df$cell_type) %>%
  dplyr::select(cell_type, nt_pred)

# MANC precedence: cell_type > manc_cell_type
banc_manc_ct <- banc_df %>%
  dplyr::mutate(cell_type = dplyr::case_when(
    cell_type %in% mc_df$cell_type ~ cell_type,
    manc_cell_type %in% mc_df$cell_type ~ manc_cell_type,
    TRUE ~ NA_character_
  )) %>%
  dplyr::filter(!is.na(cell_type), cell_type %in% mc_df$cell_type) %>%
  dplyr::select(cell_type, nt_pred)

# Hemibrain precedence: cell_type > hemibrain_cell_type > fafb_cell_type
banc_hb_ct <- banc_df %>%
  dplyr::mutate(cell_type = dplyr::case_when(
    cell_type %in% hb_df$cell_type ~ cell_type,
    hemibrain_cell_type %in% hb_df$cell_type ~ hemibrain_cell_type,
    fafb_cell_type %in% hb_df$cell_type ~ fafb_cell_type,
    TRUE ~ NA_character_
  )) %>%
  dplyr::filter(!is.na(cell_type), cell_type %in% hb_df$cell_type) %>%
  dplyr::select(cell_type, nt_pred)

# ---------- modal per cell type on both sides ----------
banc_fafb_mode <- modal_per_celltype(banc_fafb_ct)
banc_manc_mode <- modal_per_celltype(banc_manc_ct)
banc_hb_mode   <- modal_per_celltype(banc_hb_ct)
fw_mode <- modal_per_celltype(fw_df)
mc_mode <- modal_per_celltype(mc_df)
hb_mode <- modal_per_celltype(hb_df)

# ---------- build, plot, export ----------
conf_fafb_ct <- build_confusion_celltype(banc_fafb_mode, fw_mode, axis_levels = nt_levels)
conf_manc_ct <- build_confusion_celltype(banc_manc_mode, mc_mode, axis_levels = nt_levels)
conf_hb_ct   <- build_confusion_celltype(banc_hb_mode,   hb_mode, axis_levels = nt_levels)
plot_heatmap_conf(conf_fafb_ct,
                  file.path(banc.fig1.supp.path, "banc_nt_confusion_celltype_BANC_vs_FAFB_colnorm.pdf"),
                  rows_name = "BANC", cols_name = "FAFB", normalize = "col"
)
plot_heatmap_conf(conf_manc_ct,
                  file.path(banc.fig1.supp.path, "banc_nt_confusion_celltype_BANC_vs_MANC_colnorm.pdf"),
                  rows_name = "BANC", cols_name = "MANC", normalize = "col"
)
plot_heatmap_conf(conf_hb_ct,
                  file.path(banc.fig1.supp.path, "banc_nt_confusion_celltype_BANC_vs_Hemibrain_colnorm.pdf"),
                  rows_name = "BANC", cols_name = "Hemibrain", normalize = "col"
)
plot_heatmap_conf(conf_fafb_ct,
                  file.path(banc.fig1.extra.path, "banc_nt_confusion_celltype_BANC_vs_FAFB_rownorm.pdf"),
                  rows_name = "BANC", cols_name = "FAFB", normalize = "row"
)
plot_heatmap_conf(conf_manc_ct,
                  file.path(banc.fig1.extra.path, "banc_nt_confusion_celltype_BANC_vs_MANC_rownorm.pdf"),
                  rows_name = "BANC", cols_name = "MANC", normalize = "row"
)
plot_heatmap_conf(conf_hb_ct,
                  file.path(banc.fig1.extra.path, "banc_nt_confusion_celltype_BANC_vs_Hemibrain_rownorm.pdf"),
                  rows_name = "BANC", cols_name = "Hemibrain", normalize = "row"
)

# maleCNS precedence: cell_type > manc_cell_type (maleCNS shares MANC namespace)
banc_mcns_ct <- banc_df %>%
  dplyr::mutate(cell_type = dplyr::case_when(
    cell_type %in% mcns_df$cell_type ~ cell_type,
    manc_cell_type %in% mcns_df$cell_type ~ manc_cell_type,
    TRUE ~ NA_character_
  )) %>%
  dplyr::filter(!is.na(cell_type), cell_type %in% mcns_df$cell_type) %>%
  dplyr::select(cell_type, nt_pred)

banc_mcns_mode <- modal_per_celltype(banc_mcns_ct)
mcns_mode <- modal_per_celltype(mcns_df)

conf_mcns_ct <- build_confusion_celltype(banc_mcns_mode, mcns_mode, axis_levels = nt_levels)
plot_heatmap_conf(conf_mcns_ct,
                  file.path(banc.fig1.extra.path, "banc_nt_confusion_celltype_BANC_vs_maleCNS_colnorm.pdf"),
                  rows_name = "BANC", cols_name = "maleCNS", normalize = "col"
)
plot_heatmap_conf(conf_mcns_ct,
                  file.path(banc.fig1.extra.path, "banc_nt_confusion_celltype_BANC_vs_maleCNS_rownorm.pdf"),
                  rows_name = "BANC", cols_name = "maleCNS", normalize = "row"
)
