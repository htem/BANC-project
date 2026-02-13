############################################
## INFLUENCE VS. SHIU ET AL. FIRING MODEL ##
############################################

###################
## LOAD PACKAGES ##
###################

# Load required packages and data for influence validation
source("R/startup/banc-startup.R")
source("R/startup/franken-meta.R")
source("R/startup/banc-meta.R")
library(influencer)

# Get flywire FAFB data
con <- DBI::dbConnect(RSQLite::SQLite(),
                      file.path(banc.dropbox.connectivity.save.path,'fafb_783_data.sqlite'))
fw.elist <- dplyr::tbl(con, "edgelist_simple") %>%
  dplyr::filter(count > 0) %>%
  dplyr::collect() 
fw.meta <- dplyr::tbl(con, "meta") %>%
  dplyr::collect() 
dbDisconnect(con)

# Set up for influence calculation
ic_fafb <- influence_calculator_py(edgelist_simple = fw.elist, 
                                   meta = fw.meta)
# ic_fafb.signed <- influence_calculator_py(edgelist_simple = fw.elist, 
#                                           signed = TRUE,
#                                           meta = fw.meta)

#########
## MN9 ##
#########

# SEZ neurons
data <- readr::read_csv(file = "data/shiu_et_al_2025/sez_neurons.csv", 
                        col_types = cols(.default = col_character()))
names(data) <- snakecase::to_snake_case(names(data))
names(data) <- snakecase::to_snake_case(names(data))
data <- data %>%
  dplyr::mutate(
    flywire_id = as.character(flywire_id),
    neuron_name = as.character(neuron_name),
    across(-c(flywire_id, neuron_name), as.numeric)
  ) 

# Join to get more meta data
data <- data %>%
  dplyr::left_join(fw.meta,
                   by = c("flywire_id"="root_630"))

# GT data
data.gt <- readxl::read_excel(
  "/Users/GD/LMBD/Papers/dcv/data/shiu_et_al_2025/41586_2024_7763_MOESM2_ESM.xlsx",
  sheet = "Sup Table 3 Predicted MN9 vs. o"
)
names(data.gt) <- snakecase::to_snake_case(names(data.gt))
data.gt$neuron_name <- data.gt$`1`

# Join
data.plot <- dplyr::left_join(data.gt,
                              data,
                              by = "neuron_name")

#########################
## Influence from FAFB ##
#########################

# Get influence results for SEZ neurons to MN9
seed_neuron_ids.783 <- data$root_783
mn9.630 <- c(MN9_r = "720575940660219265", MN9_l = "720575940645521262")
mn9.783 <- fw.meta$root_783[match(mn9.630,fw.meta$root_630)]
names(mn9.783) <- names(mn9.630)
mn9_influence <- mn9_influence.signed <- data.frame()
for(id in seed_neuron_ids.783){
  try({
    mn9_influence.id <- calculate_influence_py(ic_fafb, id) %>%
      dplyr::filter(id %in% mn9.783)
    mn9_influence.id$source <- id
    mn9_influence <- rbind(mn9_influence,
                           mn9_influence.id)
  })
}
mn9_influence <- mn9_influence %>%
  dplyr::mutate(target = names(mn9.783)[match(id,mn9.783)])
# for(id in seed_neuron_ids.783){
#   mn9_influence.signed.id <- calculate_influence_py(ic_fafb.isgned, id) %>%
#     dplyr::filter(id %in% mn9.783)
#   mn9_influence.signed.id$source <- id
#   mn9_influence.signed <- rbind(mn9_influence.signed,
#                                 mn9_influence.signed.id)
# }
# mn9_influence.signed <- mn9_influence.signed %>%
#   dplyr::mutate(target = names(mn9.783)[match(id,mn9.783)])

###############
## Make plot ##
###############

###########################################
## SHIU FREQUENCIES, GT, INFLUENCE, LABELS
###########################################

# -- 1) Shiu predictions across ALL frequencies for MN9 -------------------------
# Pattern matches like "10_hz_mn_9_left_13" / "50_hz_mn_9_right_31" etc.
all_names <- base::names(data.plot)
m <- stringr::str_match(all_names, "^(\\d+)_hz_mn_9_(left|right)(?:_[0-9]+)?$")
shi_map <- tibble::tibble(col = all_names, freq = base::as.integer(m[, 2]), side = m[, 3]) %>%
  dplyr::filter(!is.na(freq), side %in% c("left", "right"))

freqs <- base::sort(base::unique(shi_map$freq))
if (base::length(freqs) == 0L) base::stop("No Shiu MN9 frequency columns found in data.plot.")

rowmean_or_na <- function(df, cols) {
  if (base::length(cols) == 0L) return(base::rep(NA_real_, nrow(df)))
  matrixStats::rowMeans2(base::as.matrix(dplyr::select(df, dplyr::all_of(cols))), na.rm = TRUE)
}

shi_mat_list <- base::list()
for (f in freqs) {
  f_left_cols  <- shi_map$col[shi_map$freq == f & shi_map$side == "left"]
  f_right_cols <- shi_map$col[shi_map$freq == f & shi_map$side == "right"]
  left_mean  <- rowmean_or_na(data.plot, f_left_cols)
  right_mean <- rowmean_or_na(data.plot, f_right_cols)
  shi_mat_list[[base::paste0("shiu_", f, "hz")]] <- (left_mean + right_mean) / 2
}

shi_wide <- dplyr::bind_cols(
  tibble::tibble(neuron_name = data.plot$neuron_name),
  tibble::as_tibble(shi_mat_list)
) %>%
  # Collapse any duplicate rows per neuron_name introduced upstream
  dplyr::group_by(neuron_name) %>%
  dplyr::summarise(dplyr::across(dplyr::starts_with("shiu_"),
                                 ~ base::mean(.x, na.rm = TRUE)),
                   .groups = "drop")

# -- 2) GT (single series) and Influence per neuron -----------------------------
gt_by_neuron <- data.plot %>%
  dplyr::group_by(neuron_name) %>%
  dplyr::summarise(gt = base::mean(mn_9_optogenetic_activation_rate, na.rm = TRUE),
                   .groups = "drop")

infl_by_seed <- mn9_influence %>%
  dplyr::group_by(source) %>%
  dplyr::summarise(influence_adj = base::mean(adjusted_influence, na.rm = TRUE),
                   .groups = "drop")

seed_key <- data %>%
  dplyr::select(root_783, neuron_name, cell_type) %>%
  dplyr::distinct()

infl_by_neuron <- dplyr::left_join(infl_by_seed, seed_key, by = c("source" = "root_783")) %>%
  dplyr::group_by(neuron_name) %>%
  dplyr::summarise(influence_adj = base::mean(influence_adj, na.rm = TRUE),
                   cell_type_seed = dplyr::first(stats::na.omit(cell_type), default = NA_character_),
                   .groups = "drop")

# -- 3) Labels: "cell_type (neuron_name)" unless identical ---------------------
label_key <- data %>%
  dplyr::select(neuron_name, cell_type) %>%
  dplyr::distinct() %>%
  dplyr::group_by(neuron_name) %>%
  dplyr::summarise(cell_type = dplyr::first(stats::na.omit(cell_type), default = NA_character_),
                   .groups = "drop") %>%
  dplyr::mutate(
    cell_type = ifelse(is.na(cell_type) | cell_type == "", neuron_name, cell_type),
    label_x   = ifelse(cell_type == neuron_name, cell_type,
                       base::paste0(cell_type, " (", neuron_name, ")"))
  )

# -- 4) Combine + ordering: GT → Influence → Shiu(50 Hz) (with fallback) -------
comp <- gt_by_neuron %>%
  dplyr::left_join(infl_by_neuron, by = "neuron_name") %>%
  dplyr::left_join(shi_wide,      by = "neuron_name") %>%
  dplyr::left_join(label_key,     by = "neuron_name")

# Prefer Shiu 50 Hz for tie-break; fallback to mean across Shiu freqs if absent
shi_all_mat <- base::as.matrix(dplyr::select(comp, dplyr::starts_with("shiu_")))
shi_mean    <- matrixStats::rowMeans2(shi_all_mat, na.rm = TRUE)
if (!("shiu_50hz" %in% base::names(comp))) comp$shiu_50hz <- NA_real_
comp <- comp %>%
  dplyr::mutate(shiu_50hz_fallback = ifelse(base::is.na(.data[["shiu_50hz"]]),
                                            shi_mean, .data[["shiu_50hz"]]))

comp_ord <- comp %>%
  dplyr::arrange(dplyr::desc(gt), dplyr::desc(influence_adj), dplyr::desc(shiu_50hz_fallback)) %>%
  dplyr::mutate(label_x = base::factor(label_x, levels = label_x)) %>%
  dplyr::filter(!base::is.na(label_x))  # drop NA x-labels if any


###############################################
## DIRECT CONNECTION FLAG (SEED → MN9 SYNAPSE)
###############################################

# Robustly resolve pre/post id & cell_type columns in fw.elist
.pick_first <- function(cands, cols) {
  cand <- cands[cands %in% cols]
  if (base::length(cand)) cand[[1]] else NA_character_
}

elist_cols <- base::names(fw.elist)
pre_col  <- .pick_first(c("pre_root_783","pre_root_630","pre_root","pre_id","pre","source"), elist_cols)
post_col <- .pick_first(c("post_root_783","post_root_630","post_root","post_id","post","target"), elist_cols)
if (base::is.na(pre_col) || base::is.na(post_col))
  base::stop("Could not find pre/post id columns in `fw.elist`.")

# Choose MN9 ids in the same id-space as post_col
mn9_targets <- if (base::grepl("630", post_col)) unname(mn9.630) else unname(mn9.783)

# Get presynaptic cell_type (use existing column or join from fw.meta)
pre_ct_col <- .pick_first(c("pre_cell_type","cell_type","presyn_cell_type","pre_type"), elist_cols)
if (base::is.na(pre_ct_col)) {
  pre_meta_key <- if (base::grepl("630", pre_col)) "root_630" else "root_783"
  fw.elist_ct <- fw.elist %>%
    dplyr::left_join(
      fw.meta %>% dplyr::select(dplyr::all_of(c(pre_meta_key, "cell_type"))),
      by = stats::setNames(pre_meta_key, pre_col)
    )
  pre_ct_col <- "cell_type"
} else {
  fw.elist_ct <- fw.elist
}

direct_cell_types <- fw.elist_ct %>%
  dplyr::filter(.data[[post_col]] %in% mn9_targets) %>%
  dplyr::pull(.data[[pre_ct_col]]) %>%
  base::unique() %>%
  stats::na.omit() %>%
  base::as.character()

direct_flag <- comp_ord %>%
  dplyr::transmute(label_x, cell_type, has_direct = cell_type %in% direct_cell_types)


#####################
## LONG FORM + SCALE
#####################

# Long format: GT + Influence + all Shiu freqs
gt_long <- comp_ord %>%
  dplyr::select(label_x, cell_type, gt) %>%
  tidyr::pivot_longer(gt, names_to = "series", values_to = "value") %>%
  dplyr::mutate(series = "GT")

infl_long <- comp_ord %>%
  dplyr::select(label_x, cell_type, influence_adj) %>%
  tidyr::pivot_longer(influence_adj, names_to = "series", values_to = "value") %>%
  dplyr::mutate(series = "Influence (adjusted)")

shiu_long <- comp_ord %>%
  dplyr::select(label_x, cell_type, dplyr::starts_with("shiu_")) %>%
  tidyr::pivot_longer(dplyr::starts_with("shiu_"), names_to = "series", values_to = "value") %>%
  dplyr::mutate(series = stringr::str_replace(series, "^shiu_(\\d+)hz$", "Shiu \\1 Hz"))

plot_long <- dplyr::bind_rows(gt_long, infl_long, shiu_long) %>%
  dplyr::left_join(direct_flag, by = c("label_x","cell_type"))

# Z-score per series so all lines share a scale
plot_long_z <- plot_long %>%
  dplyr::group_by(series) %>%
  dplyr::mutate(value = base::as.numeric(base::scale(value))) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!base::is.na(series), !base::is.na(label_x))  # drop NA series/x


################
## COLOR MAPS ##
################

# Legend order: GT, Influence, then Shiu freqs (ascending)
legend_levels <- base::c("GT", "Influence (adjusted)", base::paste0("Shiu ", freqs, " Hz"))
plot_long_z$series <- base::factor(plot_long_z$series, levels = legend_levels)

# Shiu colors: reversed gradient (lightgrey → paper.cols[["TRUE"]])
shiu_cols <- stats::setNames(
  grDevices::colorRampPalette(c("lightgrey", paper.cols[["TRUE"]]))(base::length(freqs)),
  base::paste0("Shiu ", freqs, " Hz")
)
col_map <- base::c(
  "GT" = "black",
  "Influence (adjusted)" = paper.cols[["highlight"]],
  shiu_cols
)


###############
## MAKE PLOT ##
###############

# Split by layer so we can force draw order: Shiu (back) → Influence → GT (top)
df_shiu <- plot_long_z %>% dplyr::filter(stringr::str_starts(base::as.character(series), "Shiu "))
df_infl <- plot_long_z %>% dplyr::filter(series == "Influence (adjusted)")
df_gt   <- plot_long_z %>% dplyr::filter(series == "GT")

p_mn9_z <- ggplot2::ggplot() +
  # Shiu frequency lines first (background)
  ggplot2::geom_line(
    data = df_shiu,
    mapping = ggplot2::aes(x = label_x, y = value, color = series, group = series),
    linewidth = 0.9
  ) +
  # Influence line in front
  ggplot2::geom_line(
    data = df_infl,
    mapping = ggplot2::aes(x = label_x, y = value, color = series, group = series),
    linewidth = 1.15
  ) +
  # GT line on top
  ggplot2::geom_line(
    data = df_gt,
    mapping = ggplot2::aes(x = label_x, y = value, color = series, group = series),
    linewidth = 1.15
  ) +
  # Circles to mark cell types with a direct synapse to MN9 (on Influence only)
  ggplot2::geom_point(
    data = df_infl %>% dplyr::filter(has_direct),
    mapping = ggplot2::aes(x = label_x, y = value),
    shape = 21, stroke = 1.2, size = 2.8, fill = NA, color = "black"
  ) +
  ggplot2::labs(
    title = "",
    subtitle = "",
    x = "",
    y = "z-score",
    color = "series"
  ) +
  ggplot2::scale_color_manual(values = col_map, drop = FALSE, na.translate = FALSE) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1),
    panel.grid.minor = ggplot2::element_blank(),
    legend.position = "bottom"
  ) +
  ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(linewidth = 2)))

# Plot
print(p_mn9_z)
ggplot2::ggsave(
  filename = file.path(banc.fig2.supp.path, "shiu_vs_influence.pdf"),
  plot = p_mn9_z, width = 25, height = 8, dpi = 300
)

# Smallest influence score for which a positive effect is seen
# Same query but on raw units (gt, influence_adj) in comp_ord
min_infl_raw <- comp_ord %>%
  dplyr::filter(!is.na(influence_adj), !is.na(gt), gt != 0) %>%
  dplyr::slice_min(order_by = influence_adj, n = 1, with_ties = FALSE) %>%
  dplyr::select(cell_type_seed, neuron_name, gt, influence_adj)
message(min_infl_raw$influence_adj) # 16.4888352312606

# --- Optional quick correlations on the raw (pre z-score) comp table -----------
# cors <- dplyr::bind_rows(
#   tibble::tibble(
#     series   = "Influence (adjusted)",
#     pearson  = stats::cor(comp$influence_adj, comp$gt, use = "complete.obs"),
#     spearman = stats::cor(comp$influence_adj, comp$gt, method = "spearman", use = "complete.obs")
#   ),
#   tibble::tibble(
#     series   = "Shiu 50 Hz (fallback if missing)",
#     pearson  = stats::cor(comp$shiu_50hz_fallback, comp$gt, use = "complete.obs"),
#     spearman = stats::cor(comp$shiu_50hz_fallback, comp$gt, method = "spearman", use = "complete.obs")
#   )
# )
# base::print(cors)





