#############################
### REPORT SYNAPSE REVIEW ###
#############################
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

# Assuming your data frame is named 'data'
plot.data <- data %>%
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
       filename = file.path(banc.fig1.supp.path, "banc_reviewed_synapses_by_neuropil.pdf"), 
       width = 14, height = 4, dpi = 300, limitsize = FALSE)
ggsave(plot = convert_to_dark_mode(review.roi),
       filename = file.path(banc.fig1.extra.path, "dark_mode_banc_reviewed_synapses_by_neuropil.pdf"), 
       width = 14, height = 4, dpi = 300, limitsize = FALSE)

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
       filename = file.path(banc.fig1.path, "banc_reviewed_synapses_by_region.pdf"), 
       width = 10, height = 4, dpi = 300) 
ggsave(plot = convert_to_dark_mode(review.region),
       filename = file.path(banc.fig1.path, "dark_mode_banc_reviewed_synapses_by_region.pdf"), 
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
  filename = file.path(banc.fig1.extra.path, "dark_mode_banc_reviewed_synapses_pie_chart_total.pdf"),
  width = 10,
  height = 10,
  dpi = 300
)

#################################
### Synaptic completion rates ###
#################################

# Read
banc.synapse.capture.csv <- "data/synapse_capture/BANCv610_synapse_capture_by_neuropil.csv"
banc.synapse.capture <- read_csv(banc.synapse.capture.csv)

# Create an 'order_col' for pre_proofread_% before pivoting
plot_df <- banc.synapse.capture %>%
  dplyr::filter(!stringr::str_detect(neuropil, "unassigned")) %>%
  dplyr::mutate(
    neuropil_simple = stringr::str_replace(neuropil, "^(CB_|OL_|VNC_)", ""),
    region = dplyr::case_when(
      stringr::str_starts(neuropil, "CB_") ~ "central_brain",
      stringr::str_starts(neuropil, "OL_") ~ "optic_lobe",
      stringr::str_starts(neuropil, regex("VNC_", ignore_case = TRUE)) ~ "ventral_nerve_cord",
      TRUE ~ "other"
    ),
    order_col = `pre_proofread_%`
  ) %>%
  tidyr::pivot_longer(
    cols = c(`pre_proofread_%`, `post_proofread_%`),
    names_to = "proofread_type",
    values_to = "proofread_perc"
  ) %>%
  dplyr::group_by(region) %>%
  dplyr::mutate(
    neuropil_simple = forcats::fct_reorder(neuropil_simple, order_col, .desc = TRUE)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(proofread_type = gsub("_.*","",proofread_type)) %>%
  dplyr::mutate(proofread_perc = dplyr::case_when(
    proofread_type=="post" ~ -proofread_perc,
    TRUE ~ proofread_perc
  ))

# Now plot
plot_df$region <- factor(plot_df$region, levels = c("optic_lobe","central_brain", "ventral_nerve_cord"))
g.capture <- ggplot2::ggplot(plot_df, ggplot2::aes(x = neuropil_simple, y = proofread_perc, fill = proofread_type)) +
  ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge(width = 0.8)) +
  ggplot2::labs(x = "", y = "proofread %", fill = NULL) +
  ggplot2::facet_grid(~region, 
                      scales = "free_x", 
                      space = "free_x") +
  ggplot2::scale_fill_manual(
    values = c("pre_proofread_%" = "#1f77b4", "post_proofread_%" = "#ff7f0e"),
    labels = c("Pre proofread %", "Post proofread %")
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = ggplot2::element_text(angle = 90, hjust = 1),
    strip.text = ggplot2::element_text(face = "bold")
  ) +
  scale_fill_manual(values = paper.cols)

# Save the plot
print(g.capture)
ggsave(
  plot = g.capture,
  filename = file.path(banc.fig1.supp.path, "banc_synapse_capture.pdf"),
  width = 14,
  height = 4,
  dpi = 300,
  bg = "transparent"
)

###########################
### NT prediction matrix ###
###########################

banc.nt.counts.csv <- "data/synapse_nt/nt_prediction_confusion_matrix_on_gt_normalized_22072025_test_set.csv"
banc.nt.confusion <- readr::read_csv(banc.nt.counts.csv)
nt.cols <- c("acetylcholine", "glutamate", "GABA", "dopamine", "serotonin", "octopamine", "tyramine", "histamine")

# Prepare matrix: set rownames, remove first column
mat_counts <- as.data.frame(banc.nt.confusion)
rownames(mat_counts) <- mat_counts[[1]]
mat_counts <- mat_counts[ , -1]
mat_counts <- as.matrix(mat_counts)[nt.cols,nt.cols]

# Row-normalize to get percentages
mat_pct <- t(apply(mat_counts, 1, function(x) 100 * x / sum(x)))

# Create cell labels: X%\n(count)
labels <- matrix(
  paste0(sprintf("%.1f%%", as.numeric(mat_pct)), "\n(", as.integer(mat_counts), ")"),
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





























