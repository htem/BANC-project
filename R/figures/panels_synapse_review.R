#' panels_synapse_review.R — synapse-prediction QC + region sample (ED Fig. 1)
#'
#' Quality-control panels for the BANC synapse-prediction model:
#' precision / recall / F-score on the manual review sample, per-neuropil
#' sample sizes, captured-synapse proportion by cell. Drives the synapse
#' evaluation columns of ED Fig. 1.
#'
#' @section Reads:
#'   * data/synapses/2024-09-20_aelysia_synapse_sample_complete.csv
#'     (the manual review sample; 4,648 rows × 6 cols; Tags ∈
#'     {True, False, Ambiguous})
#'   * banc_brain_neuropils.surf, banc_vnc_neuropil.surf,
#'     banc_neck_connective.surf  (from bancr; alpha-shape meshes)
#'
#' @section Writes:
#'   * figures/figure_1/links/supplement/banc_synapse_capture.pdf
#'   * figures/figure_1/links/supplement/banc_synapse_proportion_on_cell.png
#'   * figures/figure_1/links/supplement/banc_synapse_region_sample_by_neuropil.pdf
#'
#' @section Paper:
#'   * ED Fig. 1b–d — precision/recall/F-score by region, matched vs
#'     orphan share, per-neuropil sample sizes.
#'   * Methods §"Synapse detection evaluation".
#'
#' @section Used by:
#'   * R/text/numbers.R reads the Tag tallies of this same CSV to
#'     populate `synapse_v2_sample_fiveorless_{true,false,ambiguous}`
#'     in numbers.csv.
#'
#' @section Reproduce: BANC_NCORES=1 Rscript R/figures/panels_synapse_review.R

source("R/startup/banc-startup.R")

#################
### load data ###
#################

# Get regions
optic <- as.hxsurf(as.mesh3d(subset(banc_brain_neuropils.surf,"optic")))
midbrain <- as.hxsurf(as.mesh3d(subset(banc_brain_neuropils.surf,"midbrain")))
vnc <- banc_vnc_neuropil.surf
neck <- banc_neck_connective.surf

# Read results
data <- read_csv(file.path(banc.path,"data","synapses","2024-09-20_aelysia_synapse_sample_complete.csv")) %>%
  dplyr::mutate(region2 = dplyr::case_when(
    neuropil %in% gsub(".*optic_","",banc_brain_neuropils.surf$RegionList[grepl("optic",banc_brain_neuropils.surf$RegionList)]) ~ "optic_lobe",
    grepl('CAN|FLA|GNG|AMMC|SAD|PRW',neuropil) ~ "central_brain",
    neuropil %in% gsub(".*midbrain_","",banc_brain_neuropils.surf$RegionList[grepl("midbrain",banc_brain_neuropils.surf$RegionList)]) ~ "central_brain",
    neuropil %in% gsub(".*nerve_","",banc_vnc_nerves.surf$RegionList[grepl("nerve",banc_vnc_nerves.surf$RegionList)]) ~ "nerve",
    neuropil %in% gsub(".*vnc_","",banc_vnc_neuropils.surf$RegionList[grepl("vnc",banc_vnc_neuropils.surf$RegionList)]) ~ "ventral_nerve_cord",
    TRUE ~ region
  )) %>%
  dplyr::mutate(region = dplyr::case_when(
    neuropil %in% gsub(".*optic_","",banc_brain_neuropils.surf$RegionList[grepl("optic",banc_brain_neuropils.surf$RegionList)]) ~ "optic",
    neuropil %in% gsub(".*midbrain_","",banc_brain_neuropils.surf$RegionList[grepl("midbrain",banc_brain_neuropils.surf$RegionList)]) ~ "central_brain",
    neuropil %in% gsub(".*nerve_","",banc_vnc_nerves.surf$RegionList[grepl("nerve",banc_vnc_nerves.surf$RegionList)]) ~ "nerve",
    neuropil %in% gsub(".*vnc_","",banc_vnc_neuropils.surf$RegionList[grepl("vnc",banc_vnc_neuropils.surf$RegionList)]) ~ "ventral_nerve_cord",
    TRUE ~ region
  )) %>%
  dplyr::mutate(Tags = ifelse(Tags=="True","TRUE",Tags)) %>%
  dplyr::mutate(Tags = ifelse(Tags=="False","FALSE",Tags)) %>%
  dplyr::mutate(region2 = factor(region2, levels = c("optic_lobe","central_brain", "ventral_nerve_cord", "nerve")),
                region = factor(region, levels = c("optic_lobe","central_brain", "ventral_nerve_cord", "nerve")))

# get synaptic completion data for BANC. v2-flavored capture rates with
# size_thresh=5 are the consumer default (flipped from _v3 to _v2 on
# 2026-04-21). Legacy unsuffixed / date-stamped fallbacks have been removed.
.capture_rate_file <- .banc_completion_capture_csv("region", size_thresh = 5)
synaptic.completion <- readr::read_csv(.capture_rate_file) %>%
  dplyr::mutate(status = dplyr::case_when(
    pre_status %in% c("neuron","identified","proofread") & post_status %in% c("neuron","identified","proofread") ~ "double_identified",
    pre_status %in% c("neuron","identified","proofread") & !(post_status %in% c("neuron","identified","proofread")) ~ "pre_identified",
    !(pre_status %in% c("neuron","identified","proofread")) & post_status %in% c("neuron","identified","proofread") ~ "post_identified",
    TRUE ~ "unidentified"
  ))

#################################
### synaptic completion rates ###
#################################

# Summarize the data (combine left/right)
summary_data <- synaptic.completion %>%
  dplyr::mutate(region = dplyr::case_when(
    region %in% c("outside","outside_optic_lobes","optic_lobes") ~ "optic_lobe",
    region == "vnc" ~ "ventral_nerve_cord",
    TRUE ~ region
  )) %>%
  dplyr::filter(region %in% c("optic_lobe","central_brain","ventral_nerve_cord")) %>%
  dplyr::group_by(region, status) %>%
  dplyr::summarise(count = sum(n), .groups = "drop") %>% 
  dplyr::group_by(region) %>%
  dplyr::mutate(
    proportion = count / sum(count),
    label = paste0(scales::comma(count), "\n(",
                   scales::percent(proportion, accuracy = 0.1), ")"),
    ypos = cumsum(proportion) - proportion/2
  ) %>%
  dplyr::ungroup()

# Format to use M for million
summary_data <- summary_data %>%
  dplyr::mutate(
    label = paste0(
      scales::number(count / 1e6, accuracy = 0.01, suffix = " M")
    ) 
  ) %>%
  dplyr::mutate(label = ifelse(proportion>0.1,label,NA))

# Create the stacked bar plot
summary_data <- summary_data %>% dplyr::filter(region != "nerve")
summary_data$region <- factor(summary_data$region, levels = rev(c("optic_lobe","central_brain", "ventral_nerve_cord")))
summary_data$status <- factor(summary_data$status, levels = rev(c("double_identified","pre_identified", "post_identified", "unidentified")))
completion.region <- ggplot(summary_data, aes(x = region, y = proportion, fill = status)) +
  geom_bar(stat = "identity", 
           width = 0.7, 
           color = "white") +
  geom_text(aes(label = label), 
            position = position_stack(vjust = 0.5),
            size = 8) +
  scale_fill_manual(values = paper.cols) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(
    legend.position = "none", 
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 1, size = 20),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20, unit = "pt")
  ) +
  labs(title = "",
       y = "proportion",
       fill = "") +
  coord_flip()

# save
print(completion.region)
ggsave(plot = completion.region,
       filename = file.path(banc.fig1.path, "banc_completed_synapses_by_region.pdf"), 
       width = 10, height = 4, dpi = 300) 

############################
### false positive rates ###
############################

# By-neuropil plot uses the original sample (size > 5 — paper figure). The
# size<=5 review was merged into the master CSV on 2026-05-06; explicitly
# filter here so this panel keeps showing the original sample.
plot.data <- data %>%
  dplyr::filter(size > 5) %>%
  dplyr::mutate(neuropil = ifelse(region2=="nerve","nerve",neuropil)) %>%
  dplyr::mutate(Tags = factor(Tags, levels = c("TRUE", "Ambiguous", "FALSE"))) %>%
  dplyr::group_by(neuropil, Tags, region2) %>%
  dplyr::summarise(count = dplyr::n()) %>%
  dplyr::group_by(neuropil) %>%
  dplyr::mutate(proportion = count / sum(count))

# Create the plot with proportions
review.roi <- ggplot(plot.data, aes(x = neuropil, y = proportion, fill = Tags)) +
  facet_grid(~ region2, scales = "free_x", space = "free_x") +
  geom_col(position = "stack") +
  labs(x = "neuropil", y = "proportion", title = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_fill_manual(values = paper.cols)

# Save
print(review.roi)
ggsave(plot = review.roi,
       filename = file.path(banc.fig1.extra.path, "banc_reviewed_synapses_by_neuropil.pdf"),
       width = 14, height = 4, dpi = 300, limitsize = FALSE)
ggsave(plot = convert_to_dark_mode(review.roi),
       filename = file.path(banc.fig1.darkmode.path, "dark_mode_banc_reviewed_synapses_by_neuropil.pdf"),
       width = 14, height = 4, dpi = 300, limitsize = FALSE)

###############################
### True/False vs size bin  ###
###############################
# Companion view to the by-neuropil plot: how does the True/False/Ambiguous
# split track synapse size (voxel count of the postsynaptic detection)?
# Sizes 1-5 come from the 2026-05-06 size<=5 review merge; 6+ come from the
# original sample. Same colour scheme as `review.roi` so the two plots can
# be read together.
# Bins chosen from the actual size distribution: per-integer 3/4/5 sit in
# the size<=5 review (sharp ~74% False rate), bin 6-10 is where False rate
# drops to ~22%, and the long tail is grouped into log-ish bins. Sizes 1-2
# lumped because each integer has <30 synapses (sparse).
.size_bin_breaks <- c(0, 2, 3, 4, 5, 10, 20, 50, 100, Inf)
.size_bin_labels <- c("1-2","3","4","5","6-10","11-20","21-50","51-100","101+")

size_plot.data <- data %>%
  dplyr::mutate(
    Tags     = factor(Tags, levels = c("TRUE", "Ambiguous", "FALSE")),
    size_bin = cut(size, breaks = .size_bin_breaks, labels = .size_bin_labels,
                   include.lowest = TRUE, right = TRUE)
  ) %>%
  dplyr::filter(!is.na(size_bin), !is.na(Tags)) %>%
  dplyr::group_by(size_bin, Tags) %>%
  dplyr::summarise(count = dplyr::n(), .groups = "drop") %>%
  dplyr::group_by(size_bin) %>%
  dplyr::mutate(
    proportion = count / sum(count),
    bin_total  = sum(count)
  ) %>%
  dplyr::ungroup()

.bin_totals <- size_plot.data %>%
  dplyr::distinct(size_bin, bin_total)

review.size <- ggplot(size_plot.data,
                      aes(x = size_bin, y = proportion, fill = Tags)) +
  geom_col(position = "stack", color = "white", width = 0.85) +
  geom_text(data = .bin_totals,
            aes(x = size_bin, y = 1.02,
                label = scales::comma(bin_total)),
            inherit.aes = FALSE, size = 3, vjust = 0) +
  scale_fill_manual(values = paper.cols) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     expand = ggplot2::expansion(mult = c(0, 0.10))) +
  labs(x = "synapse size (voxels)", y = "proportion",
       fill = "review tag", title = "") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor   = element_blank(),
        axis.text.x = element_text(angle = 0))

print(review.size)
ggsave(plot = review.size,
       filename = file.path(banc.fig1.extra.path, "banc_reviewed_synapses_by_size_bin.pdf"),
       width = 8, height = 4, dpi = 300)
ggsave(plot = convert_to_dark_mode(review.size),
       filename = file.path(banc.fig1.darkmode.path, "dark_mode_banc_reviewed_synapses_by_size_bin.pdf"),
       width = 8, height = 4, dpi = 300)

# Summarize the data (keep this part as is)
summary_data <- data %>%
  dplyr::mutate(Tags = ifelse(Tags=="Ambiguous","FALSE","TRUE")) %>%
  dplyr::group_by(region, Tags) %>%
  dplyr::summarise(count = n(), .groups = "drop") %>%
  dplyr::group_by(region) %>%
  dplyr::mutate(
    proportion = count / sum(count),
    label = paste0(count,"\n(",scales::percent(proportion, accuracy = 0.1), ")"),
    ypos = cumsum(proportion) - proportion/2,
    label = ifelse(Tags=="Ambiguous","",label)) %>%
  dplyr::ungroup()

# Create the stacked bar plot
summary_data$region <- factor(summary_data$region, levels = c("optic_lobe","central_brain", "ventral_nerve_cord", "nerve"))
review.region <- ggplot(summary_data, aes(x = region, y = proportion, fill = Tags)) +
  geom_bar(stat = "identity", 
           width = 0.7, 
           color = "white") +
  geom_text(aes(label = label), 
            position = position_stack(vjust = 0.5),
            size = 6) +
  scale_fill_manual(values = paper.cols) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(
    legend.position = "none", 
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 1, size = 20),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20, unit = "pt")
  ) +
  labs(title = "",
       y = "proportion",
       fill = "") +
  coord_flip()

# save
print(review.region)
ggsave(plot = review.region,
       filename = file.path(banc.fig1.extra.path, "banc_reviewed_synapses_by_region.pdf"),
       width = 10, height = 4, dpi = 300) 
ggsave(plot = convert_to_dark_mode(review.region),
       filename = file.path(banc.fig1.darkmode.path, "dark_mode_banc_reviewed_synapses_by_region.pdf"), 
       width = 10, height = 4, dpi = 300) 

# Summarize the data
summary_data <- data %>%
  dplyr::filter(region != "nerve") %>%
  dplyr::group_by(Tags) %>%
  dplyr::summarise(count = n(), .groups = "drop") %>%
  dplyr::ungroup() %>%
  dplyr::mutate(percentage = count / sum(count) * 100)
total_count <- sum(summary_data$count)

# Create the pie chart
review.total <- ggplot(summary_data, aes(x = "", y = count, fill = Tags)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = paper.cols) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5),
            size = 4) +
  geom_text(aes(x = 0, y = 0, label = paste("synapse sample:", format(total_count, big.mark = ","))),
            size = 6) +
  theme_void() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 16)
  ) +
  labs(
    title = "",
    fill = ""
  )

# Save the plot
print(review.total)
ggsave(
  plot = review.total,
  filename = file.path(banc.fig1.extra.path, "banc_reviewed_synapses_pie_chart_total.pdf"),
  width = 10,
  height = 10,
  dpi = 300
)
ggsave(
  plot = convert_to_dark_mode(review.total),
  filename = file.path(banc.fig1.darkmode.path, "dark_mode_banc_reviewed_synapses_pie_chart_total.pdf"),
  width = 10,
  height = 10,
  dpi = 300
)

#################################
### Synaptic completion rates ###
#################################

# Read v2-flavored neuropil capture rates (size_thresh=5 default).
banc.synapse.capture.csv <- .banc_completion_capture_csv("neuropil", size_thresh = 5)
banc.synapse.capture <- readr::read_csv(banc.synapse.capture.csv)

# Aggregate to pre/post proofread % per (region, side, neuropil)
# Use raw counts (n) to compute percentages correctly when rows span
# both inside-neuropil and outside-neuropil entries for the same region.
plot_df <- banc.synapse.capture %>%
  # Normalize neuropil: strip side suffixes (AL_L -> AL) and comma-separated multi-neuropils
  # The `side` column is authoritative for sidedness.
  dplyr::mutate(
    neuropil = gsub("^outside_", "", neuropil),
    neuropil = sub(",.*", "", neuropil),
    neuropil = sub("_[LR]$", "", neuropil)
  ) %>%
  dplyr::mutate(region = dplyr::case_when(
    region %in% c("central_brain","optic_lobe","ventral_nerve_cord") ~ region,
    region %in% c("optic_lobes") ~ "optic_lobe",
    region == "vnc" ~ "ventral_nerve_cord",
    neuropil %in% gsub("_[LR]$","", gsub("ITO_midbrain_","",banc_brain_neuropils.surf$RegionList)) ~ "central_brain",
    neuropil %in% gsub("_[LR]$","", gsub("ITO_optic_","",banc_brain_neuropils.surf$RegionList)) ~ "optic_lobe",
    neuropil %in% gsub("_[LR]$","", gsub("COURT_vnc_|MANC_vnc_","",banc_vnc_neuropils.surf$RegionList)) ~ "ventral_nerve_cord",
    TRUE ~ NA_character_
  )) %>%
  dplyr::filter(!stringr::str_detect(neuropil, "unassigned"), !is.na(region)) %>%
  dplyr::group_by(region, side, neuropil) %>%
  dplyr::summarise(
    total_n = sum(n, na.rm = TRUE),
    pre_proofread_pct  = sum(n[pre_status  %in% c("neuron","identified","proofread")], na.rm = TRUE) / sum(n, na.rm = TRUE),
    post_proofread_pct = sum(n[post_status %in% c("neuron","identified","proofread")], na.rm = TRUE) / sum(n, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Drop nerve neuropils and low-n neuropils
  dplyr::filter(!grepl("nerve", neuropil, ignore.case = TRUE),
                total_n >= 1000) %>%
  dplyr::mutate(
    neuropil_label = paste0(neuropil, " ", substr(toupper(side), 1, 1)),
    # bar_order used for sorting: each bar by its own pre_proofread_pct
    bar_order = pre_proofread_pct
  ) %>%
  tidyr::pivot_longer(
    cols = c(pre_proofread_pct, post_proofread_pct),
    names_to = "proofread_type",
    values_to = "proofread_perc"
  ) %>%
  dplyr::mutate(
    proofread_type = dplyr::if_else(proofread_type == "pre_proofread_pct", "pre", "post"),
    proofread_perc = dplyr::if_else(proofread_type == "post", -proofread_perc, proofread_perc)
  )

# Factor order region → optic_lobe, central_brain, ventral_nerve_cord
plot_df$region <- factor(plot_df$region,
                         levels = c("optic_lobe","central_brain","ventral_nerve_cord"))

# Build factor levels for neuropil_label: group by region, keep L/R variants
# of the same neuropil adjacent by computing a base-level order from the mean
# pre_proofread_pct across sides; within a base group sort label alphabetically
# (so AL L comes before AL R).
level_order <- plot_df %>%
  dplyr::distinct(region, neuropil, neuropil_label, bar_order) %>%
  dplyr::group_by(region, neuropil) %>%
  dplyr::mutate(base_order = mean(bar_order, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(region, dplyr::desc(base_order), neuropil, neuropil_label) %>%
  dplyr::pull(neuropil_label)
plot_df$neuropil_label <- factor(plot_df$neuropil_label, levels = unique(level_order))

g.capture <- ggplot2::ggplot(plot_df,
                             ggplot2::aes(x = neuropil_label,
                                          y = proofread_perc,
                                          fill = proofread_type)) +
  ggplot2::geom_col(width = 0.8) +
  ggplot2::labs(x = "", y = "proofread proportion", fill = NULL) +
  ggplot2::facet_grid(~region, scales = "free_x", space = "free_x") +
  ggplot2::scale_fill_manual(
    values = paper.cols,
    labels = c(pre = "Pre proofread", post = "Post proofread")
  ) +
  ggplot2::scale_y_continuous(
    labels = function(x) sprintf("%.2f", abs(x)),
    breaks = seq(-1, 1, by = 0.25)
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    panel.grid.major.x = ggplot2::element_blank(),
    panel.grid.minor.x = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(size = 6, angle = 90, hjust = 1),
    strip.text = ggplot2::element_text(face = "bold")
  )

# Save the plot
print(g.capture)
ggplot2::ggsave(
  plot = g.capture,
  filename = file.path(banc.fig1.supp.path, "banc_synapse_capture.pdf"),
  width = 15, height = 4, dpi = 300, bg = "transparent"
)

###############################
### Synaptic manual review  ###
###############################

df_raw <- readr::read_csv("data/synapses/251013_synapse_evaluation.csv", show_col_types = FALSE)

df <- df_raw %>%
  dplyr::transmute(
    neuropil,
    region,
    tp = as.numeric(true_positives),
    fp = as.numeric(false_positives),
    fn = as.numeric(false_negatives),
    precision = as.numeric(precision),
    recall    = as.numeric(recall),
    f1        = as.numeric(f1_score)
  ) %>%
  dplyr::mutate(total_eval = tp + fp + fn)

# Clean display strings
df2 <- df %>%
  dplyr::mutate(
    neuropil_clean = gsub("_", " ", neuropil),
    region_pretty  = dplyr::case_when(region == "CNS" ~ "CNS", TRUE ~ gsub("_", " ", region)),
    neuropil_clean = stringr::str_squish(neuropil_clean),
    region_pretty  = stringr::str_squish(region_pretty)
  )

# We'll exclude the pooled CNS/BANC bar here (that's the separate horizontal plot you made)

# Order neuropils within each region by TP (descending)
tp_order <- df2 %>%
  dplyr::group_by(region_pretty, neuropil_clean) %>%
  dplyr::summarise(tp_sum = sum(tp, na.rm = TRUE), .groups = "drop") %>%
  dplyr::arrange(region_pretty, dplyr::desc(tp_sum))

# Long format for grouped bars
bars <- df2 %>%
  tidyr::pivot_longer(c(tp, fp, fn), names_to = "class", values_to = "count") %>%
  dplyr::mutate(
    class = dplyr::recode(class,
                          tp = "true positive",
                          fp = "false positive",
                          fn = "false negative"),
    proportion = count / total_eval
  ) %>%
  dplyr::left_join(tp_order, by = c("region_pretty","neuropil_clean")) %>%
  dplyr::group_by(region_pretty) %>%
  dplyr::mutate(
    neuropil_ord = factor(
      neuropil_clean,
      levels = tp_order$neuropil_clean[tp_order$region_pretty == unique(region_pretty)][
        order(match(tp_order$neuropil_clean[tp_order$region_pretty == unique(region_pretty)], unique(neuropil_clean)))
      ]
    )
  ) %>%
  dplyr::ungroup()

# Per-neuropil labels placed above the tallest of TP/FP/FN (as a proportion)
lab_top <- df2 %>%
  dplyr::left_join(tp_order, by = c("region_pretty","neuropil_clean")) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    y_top = 1.05 * max(c(tp, fp, fn), na.rm = TRUE) / total_eval,
    toplab = paste0(
      "P=", scales::number(precision, 0.01), "\n",
      "R=", scales::number(recall,    0.01), "\n",
      "F1=", scales::number(f1,       0.01), "\n",
      "n=", scales::comma(total_eval)
    )
  ) %>%
  dplyr::ungroup()

# Colors
fill_cols <- c(
  "true positive"  = paper.cols[["TRUE"]],
  "false positive" = paper.cols[["FALSE"]],
  "false negative" = paper.cols[["other"]]
)

# Plot (grouped bars by class; ordered by TP within each region)
p_by_neuropil <- ggplot2::ggplot(
  bars,
  ggplot2::aes(x = neuropil_ord, y = proportion, fill = class)
) +
  ggplot2::geom_col(
    position = ggplot2::position_dodge(width = 0.75),
    width = 0.65, colour = "white"
  ) +
  ggplot2::geom_text(
    data = lab_top,
    ggplot2::aes(x = neuropil_clean, y = y_top, label = toplab),
    inherit.aes = FALSE, size = 3.4, lineheight = 0.98, vjust = 0
  ) +
  ggplot2::facet_grid(  ~ region_pretty,
                        scales = "free_x", space = "free_x",
                        labeller = ggplot2::labeller(.cols = function(x) rep("", length(x)))) +
  ggplot2::scale_fill_manual(values = fill_cols, guide = "none") +
  ggplot2::scale_y_continuous(limits = c(0, 1.1),
                              breaks = seq(0, 1, by = 0.25),
                              expand = ggplot2::expansion(mult = c(0.02, 0.15))) +
  ggplot2::labs(x = NULL, y = "proportion of synapses") +
  ggplot2::coord_cartesian(clip = "off") +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    strip.text = ggplot2::element_blank(),
    strip.background = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_blank(),
    panel.grid.minor   = ggplot2::element_blank(),
    axis.text.x        = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8),
    plot.margin        = ggplot2::margin(t = 8, r = 8, b = 36, l = 8),
    strip.text.x       = ggplot2::element_text(face = "bold")
  )

print(p_by_neuropil)
ggplot2::ggsave(
  plot = p_by_neuropil,
  filename = file.path(banc.fig1.supp.path, "banc_synapse_region_sample_by_neuropil.pdf"),
  width = 12, height = 4, dpi = 300
)




