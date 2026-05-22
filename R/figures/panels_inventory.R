#' panels_inventory.R — dataset inventory plots (Fig. 1 + ED Fig. 1)
#'
#' Categorical breakdowns of the BANC v888 metadata used for the "what's
#' in this connectome" overview in Figure 1: stacked bars and tables of
#' proofread / roughly-proofread counts by region, flow direction,
#' super_class, cell_class, nerve, neuromere, hemilineage, and predicted
#' neurotransmitter.
#'
#' @section Reads:
#'   * banc.meta                              via R/startup/banc-meta.R
#'
#' @section Writes:
#'   * figures/figure_1/links/extra/banc_inventory.pdf
#'   * figures/figure_1/links/extra/banc_meta_inventory{,_bar}.pdf
#'   * figures/figure_1/links/extra/banc_flow_inventory.pdf
#'   * figures/figure_1/links/extra/banc_proofread_by_super_class.pdf
#'   * figures/figure_1/links/extra/annotation_coverage_{overall,by_super_class}.pdf
#'   * figures/figure_1/links/extra/effector_cell_class_counts_comparison.pdf
#'   * Several neurotransmitter / neuropeptide tables under links/extra/.
#'
#' @section Paper:
#'   Numbers cited in the Fig. 1 inventory paragraph come from
#'   R/text/numbers.R; this script provides the visual underlay placed
#'   inside figure_1.ai. Methods §"Annotation taxonomy".
#'
#' @section Reproduce: BANC_NCORES=1 Rscript R/figures/panels_inventory.R

source("R/startup/banc-startup.R")
source("R/startup/banc-meta.R")

# Define output paths for Figure 1
banc.fig1.path <- "figures/figure_1/links/"
banc.fig1.extra.path <- "figures/figure_1/links/extra"
banc.fig1.supp.path <- "figures/figure_1/links/supplement"

# Load required libraries
library(dplyr)
library(gt)
library(webshot2) 
library(tidyr)
library(stringr)

###############################
## METADATA STACKED BAR PLOT ##
###############################

# Backfill super_class from fafb_alignment_super_class (SeaTable column;
# falls back to mapping fafb_cell_type -> super_class via franken.meta when
# the GCS cache is in use and the SeaTable column is absent). Mirrors the
# same fallback used in R/text/numbers.R for cross-table consistency.
.bm <- banc.meta
if (!"fafb_alignment_super_class" %in% colnames(.bm)) {
  if ("fafb_cell_type" %in% colnames(.bm) &&
        exists("franken.meta") &&
        all(c("fafb_cell_type", "super_class") %in% colnames(franken.meta))) {
    .fa_lookup <- franken.meta %>%
      dplyr::filter(!is.na(fafb_cell_type), fafb_cell_type != "",
                    !is.na(super_class), super_class != "") %>%
      dplyr::distinct(fafb_cell_type, .keep_all = TRUE) %>%
      dplyr::select(fafb_cell_type, super_class) %>%
      tibble::deframe()
    .bm$fafb_alignment_super_class <- unname(.fa_lookup[.bm$fafb_cell_type])
  } else {
    .bm$fafb_alignment_super_class <- NA_character_
  }
}

# Prepare metadata to visualise anatomical distribution and functional classification
# of proofread neurons across the BANC connectome
plot.data <- .bm %>%
  filter_valid_neurons(only_proofread = TRUE) %>%
  dplyr::mutate(region = dplyr::case_when(
    super_class %in% c("visual_centrifugal") ~ "central_brain",
    region %in% c("midbrain","brain","central_brain") ~ "central_brain",
    region %in% c("vnc","ventral_nerve_cord") ~ "ventral_nerve_cord",
    region %in% c("optic") ~ "optic_lobe",
    grepl('neck_connective|cervical_connective', region) | grepl("ascending|descending", super_class) ~ "neck_connective",
    grepl('CAN|FLA|GNG|AMMC|SAD|PRW',root_region)|region=="sez" ~ "central_brain",
    grepl('_midbrain_',root_region) ~ "central_brain",
    grepl('optic', root_region) ~ "optic_lobe",
    grepl('optic', region) ~ "optic_lobe",
    TRUE ~ region
  )) %>%
  dplyr::mutate(neurotransmitter_predicted = dplyr::case_when(
    neurotransmitter_predicted =="unknown" ~ "undetermined",
    is.na(neurotransmitter_predicted) ~ "undetermined",
    grepl("unclear",neurotransmitter_predicted)  ~ "undetermined",
    TRUE ~ gsub(";.*","",neurotransmitter_predicted)
  )) %>%
  dplyr::mutate(side = dplyr::case_when(
    is.na(side) ~ "undetermined",
    TRUE ~ gsub(";.*","",side)
  )) %>%
  dplyr::mutate(flow = dplyr::case_when(
    is.na(flow) ~ "intrinsic",
    !flow%in%c("afferent","efferent","intrinsic") ~ "intrinsic",
    TRUE ~ gsub(";.*","",flow)
  )) %>%
  dplyr::mutate(super_class = dplyr::case_when(
    !is.na(super_class) & super_class != "" ~ super_class,
    !is.na(fafb_alignment_super_class) & fafb_alignment_super_class != "" ~ fafb_alignment_super_class,
    region == "optic_lobe" ~ "optic_lobe_intrinsic",
    TRUE ~ "undetermined"
  )) %>%
  dplyr::select(id, flow, super_class, side, region, neurotransmitter_predicted) %>%
  pivot_longer(
    cols = c(flow, super_class, side, region, neurotransmitter_predicted),
    names_to = "category",
    values_to = "value"
  ) %>%
  dplyr::mutate(value = ifelse(is.na(value) | value == "", "undetermined", value)) %>%
  dplyr::count(category, value)

# Define categorical hierarchy from anatomy to neurotransmitter identity
category_order <- c("side", "region", "flow", "super_class", "neurotransmitter_predicted", "hemilineage")
plot.data$category <- factor(plot.data$category, levels = category_order)

# Calculate segment positions for proportional stacked bar visualisation
positioned_data <- plot.data %>%
  # First calculate proportions
  group_by(category) %>%
  mutate(
    total = sum(n),
    proportion = n/total
  ) %>%
  # Order by proportion descending within each category
  arrange(category, desc(proportion)) %>%
  # Calculate positions for each segment
  mutate(
    ymin = lag(cumsum(proportion), default = 0),
    ymax = cumsum(proportion),
    ymid = (ymin + ymax)/2,
    label = gsub("_","\n",as.character(value)),
    show_label = proportion > 0.05
  ) %>%
  ungroup()

# Extract all categorical values for colour mapping consistency
all_values <- unique(positioned_data$value)

# Generate consistent colour scheme across all neuronal categories
color_mapping <- rep(NA, length(all_values))
names(color_mapping) <- all_values

# Apply predefined paper colour scheme where available
for(val in names(paper.cols)) {
  if(val %in% all_values) {
    color_mapping[val] <- paper.cols[val]
  }
}

# Generate additional colours for uncategorised values using cerise-limon palette
missing_values <- all_values[is.na(color_mapping)]
if(length(missing_values) > 0) {
  additional_colors <- cerise_limon_palette(length(missing_values))
  color_mapping[missing_values] <- additional_colors
}

# Construct stacked bar plot with precise segment positioning and labelling
g.meta <- ggplot() +
  geom_rect(
    data = positioned_data,
    aes(
      xmin = as.numeric(category) - 0.35,  # Width of 0.7 centered on category
      xmax = as.numeric(category) + 0.35,
      ymin = ymin,
      ymax = ymax,
      fill = value
    ),
    color = "white",
    linewidth = 0.1
  ) +
  geom_text(
    data = filter(positioned_data, show_label == TRUE),
    aes(
      x = as.numeric(category),
      y = ymid,
      label = label
    ),
    color = "black",
    angle = 0,
    size = 4
  ) +
  scale_fill_manual(values = color_mapping) +
  scale_x_continuous(
    breaks = 1:length(category_order),
    labels = category_order
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "",
    x = "",
    y = "",
    fill = ""
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(angle = 0, hjust = 1.75, size = 15),
    axis.text.x = element_text(angle = 0, hjust = 0, size = 12),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20, unit = "pt")
  )

print(g.meta)

ggsave(plot = g.meta,
       filename = file.path(banc.fig1.extra.path,"banc_meta_inventory_bar.pdf"), 
       width = 9, height = 7, dpi = 300)

# Reorder categories by proportion for heatmap display with undetermined values last
positioned_data <- positioned_data %>%
  group_by(category) %>%
  mutate(
    value_ordered = value %>%
      forcats::fct_reorder(proportion, .desc = TRUE) %>%
      forcats::fct_relevel("undetermined", after = Inf)
  ) %>%
  mutate(value_ordered = gsub("ventral_nerve_cord","VNC",value_ordered)) %>%
  mutate(value_ordered = gsub("central_brain","CB",value_ordered)) %>%
  mutate(value_ordered = gsub("brain","CB",value_ordered)) %>%
  mutate(value_ordered = gsub("optic_lobe","OL",value_ordered)) %>%
  ungroup()

# Generate heatmap tiles showing proportional representation of each category
g.hm <- ggplot(positioned_data, aes(x = 1, y = value_ordered, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_manual(values = color_mapping) +
  geom_text(aes(label = scales::percent(proportion, accuracy = 0.01)), hjust = 0.5, color = "black") +
  facet_wrap(~category, scales = "free_y", nrow = 1) +
  labs(x = "", y = "", fill = "") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "none") +
  scale_y_discrete(labels = function(x) gsub("_", "\n", x)) +
  theme(
    axis.text.x = element_blank(), 
    axis.text.y = element_text(color = "black", angle = 0, hjust = 1, size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

plot(g.hm)
ggsave(plot = g.hm,
       filename = file.path(banc.fig1.extra.path,"banc_meta_inventory.pdf"), 
       width = 9, height = 7, dpi = 300)


############
## TABLE  ##
############

# Create horizontal metadata summary table
meta_table_data <- positioned_data %>%
  # Remove undetermined values
  dplyr::filter(value != "undetermined") %>%
  # Clean up value names for better presentation
  dplyr::mutate(
    value_clean = dplyr::case_when(
      value == "ventral_nerve_cord" ~ "ventral nerve cord",
      value == "central_brain" ~ "central brain", 
      value == "brain" ~ "central brain",
      value == "optic_lobe" ~ "optic lobe",
      TRUE ~ gsub("_", " ", value)
    ),
    # Combine count and proportion into one column
    count_prop = paste0(n, " (", scales::percent(proportion, accuracy = 0.1), ")"),
    # Clean category names
    category_clean = dplyr::case_when(
      category == "neurotransmitter_predicted" ~ "neurotransmitter",
      category == "super_class" ~ "super class",
      TRUE ~ gsub("_", " ", category)
    )
  ) %>%
  # Order by category and then by count descending
  dplyr::arrange(category, dplyr::desc(n)) %>%
  # Select columns for the table
  dplyr::select(Category = category_clean, Value = value_clean, `Count (%)` = count_prop)

# Pivot to horizontal layout - categories become columns
table_data <- meta_table_data %>%
  dplyr::group_by(Category) %>%
  dplyr::mutate(row_id = dplyr::row_number()) %>%
  dplyr::ungroup() %>%
  # Create combined value_count column for each category
  dplyr::mutate(value_count = paste0(Value, ": ", `Count (%)`)) %>%
  dplyr::select(row_id, Category, value_count) %>%
  # Pivot wider so categories become columns
  tidyr::pivot_wider(
    names_from = Category,
    values_from = value_count,
    values_fill = ""
  ) %>%
  dplyr::select(-row_id)

# Create the horizontal table
meta_table <- table_data %>%
  gt::gt() %>%
  gt::tab_header(
    title = "BANC connectome metadata summary",
    subtitle = "distribution of proofread neurons (count and proportion) - excluding undetermined"
  ) %>%
  # Style the table for readability with Helvetica font
  gt::tab_style(
    style = gt::cell_text(size = gt::px(9), font = "Helvetica"),
    locations = gt::cells_body()
  ) %>%
  gt::tab_style(
    style = gt::cell_text(weight = "bold", size = gt::px(10), font = "Helvetica"),
    locations = gt::cells_column_labels()
  ) %>%
  # Style the title and subtitle with Helvetica
  gt::tab_style(
    style = gt::cell_text(font = "Helvetica"),
    locations = gt::cells_title(groups = "title")
  ) %>%
  gt::tab_style(
    style = gt::cell_text(font = "Helvetica"),
    locations = gt::cells_title(groups = "subtitle")
  ) %>%
  # Style the source note with Helvetica
  gt::tab_style(
    style = gt::cell_text(font = "Helvetica"),
    locations = gt::cells_source_notes()
  ) %>%
  # Make columns equal width
  gt::cols_width(
    everything() ~ gt::px(120)
  ) %>%
  # Add source note
  gt::tab_source_note(
    source_note = "data filtered for proofread neurons excluding glia, trachea, non-neuronal cells, and undetermined values"
  ) %>%
  # Table options for horizontal layout with Helvetica as default font
  gt::tab_options(
    table.font.size = gt::px(9),
    table.font.names = "Helvetica",
    heading.title.font.size = gt::px(14),
    heading.subtitle.font.size = gt::px(11),
    data_row.padding = gt::px(2),
    column_labels.padding = gt::px(3)
  )

# Display the table
print(meta_table)

# Save as PDF (requires Chrome) with HTML fallback
tryCatch(
  gt::gtsave(meta_table,
             filename = file.path(banc.fig1.extra.path, "banc_meta_inventory_table.pdf"),
             vwidth = 800,
             vheight = 600),
  error = function(e) {
    message("PDF save failed (", e$message, "), saving as HTML instead")
    gt::gtsave(meta_table,
               filename = file.path(banc.fig1.extra.path, "banc_meta_inventory_table.html"))
  }
)

#####################
## FLOW DIRECTION  ##
#####################

# Classify neurons by information flow direction through the nervous system:
# afferent (sensory input), efferent (motor/endocrine output), intrinsic (local processing)
processed_data <- banc.meta %>%
  dplyr::filter(proofread == TRUE) %>%
  dplyr::distinct(root_id, .keep_all = TRUE) %>%
  dplyr::mutate(flow = dplyr::case_when(
    grepl("sensory",super_class) ~ "afferent",
    grepl("motor|endocrine|efferent",super_class) ~ "efferent",
    TRUE ~ "intrinsic"
  )) %>%
  dplyr::filter(!is.na(flow)) %>%
  dplyr::group_by(flow) %>%
  dplyr::summarise(count = dplyr::n()) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    proportion = count / sum(count),
    label = paste0(gsub("_"," ",flow),"\n",count," (",scales::percent(proportion, accuracy = 0.1), ")")
  )

# Set flow direction order from sensory input to motor output
processed_data$flow <- factor(processed_data$flow, levels = c("afferent","intrinsic","efferent"))

# Position pie chart labels with connecting lines for clarity
processed_data <- processed_data %>%
  dplyr::arrange(flow) %>%
  dplyr::mutate(
    fraction = count / sum(count),
    ymax = cumsum(fraction),
    ymin = c(0, head(ymax, n=-1)),
    position = (ymax + ymin) / 2,
    position_label = 1.05  # This will place the labels just outside the pie
  )

# Generate pie chart showing proportional distribution of flow directions
pie_chart <- ggplot(processed_data, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = flow)) +
  geom_rect(color = "white") +
  ggrepel::geom_text_repel(aes(label = label, x=4, y=position), 
                           nudge_x = .5,
                           size = 2) +
  geom_segment(aes(x = 4, y = position, xend = 4.05, yend = position), color = "black", size = 0.5) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = paper.cols) + 
  xlim(c(0, 4.5)) + 
  theme_void() +
  theme(legend.position = "none") +
  labs(fill = "region and side", 
       title = "")

print(pie_chart)

ggsave(plot = pie_chart,
       filename = file.path(banc.fig1.extra.path,"banc_flow_inventory.pdf"), width = 6, height = 6, dpi = 300)

############################
## ANATOMICAL REGIONS     ##
############################

# Analyse bilateral distribution of neurons across major brain and VNC regions
# to assess anatomical coverage and laterality patterns
processed_data <- banc.meta %>%
  dplyr::filter(proofread == TRUE,
                side %in% c("left","right")) %>%
  dplyr::distinct(root_id, .keep_all = TRUE) %>%
  dplyr::mutate(region = dplyr::case_when(
    super_class %in% c("visual_centrifugal") ~ "brain",
    region %in% c("midbrain","brain","central_brain","spz","sez") ~ "brain",
    region %in% c("vnc","ventral_nerve_cord") ~ "vnc",
    region %in% c("optic","optic_lobe") ~ "optic",
    grepl('neck_connective|cervical_connective|neck', region) | grepl("ascending|descending", super_class) ~ "neck",
    grepl('CAN|FLA|GNG|AMMC|SAD|PRW', root_region) ~ "brain",
    grepl('_midbrain_', root_region) ~ "brain",
    grepl('optic', root_region) ~ "optic",
    TRUE ~ region
  )) %>%
  dplyr::filter(!is.na(region)) %>%
  dplyr::group_by(region, side) %>%
  dplyr::summarise(count = dplyr::n()) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    group = interaction(region,side, sep="_")) %>%
  dplyr::mutate(
    proportion = count / sum(count),
    label = paste0(gsub("_"," ",group),"\n",count," (",scales::percent(proportion, accuracy = 0.1), ")")
  )

# Apply region-specific colours from paper colour scheme
region_colors <- paper.cols[unique(processed_data$region)]

# Create colour variations to distinguish left/right hemisphere pairs
lighten_color <- function(color, factor = 1.6) {
  col_rgb <- col2rgb(color)
  col_rgb <- pmin(col_rgb * factor, 255)
  rgb(t(col_rgb), maxColorValue = 255)
}

# Generate hemisphere-specific colour palette with graduated intensities
color_palette <- c(
  sapply(region_colors, function(color) color),
  sapply(region_colors, lighten_color, factor = 1.2),
  sapply(region_colors, lighten_color, factor = 1.4)
)
names(color_palette) <- c(
  paste0(names(region_colors), "_left"),
  paste0(names(region_colors), "_right"),
  paste0(names(region_colors), "_undetermined")
)

# Order regions along neuraxis from optic lobes to ventral nerve cord
processed_data$region <- factor(processed_data$region, c("optic","brain","neck","vnc"))
processed_data$group <- factor(processed_data$group, c("optic_left","optic_right",
                                                       "brain_left", "brain_right",
                                                       "neck_left", "neck_right",
                                                       "vnc_left", "vnc_right"))

# Position pie chart labels to show regional counts and proportions
processed_data <- processed_data %>%
  dplyr::arrange(group) %>%
  dplyr::mutate(
    fraction = count / sum(count),
    ymax = cumsum(fraction),
    ymin = c(0, head(ymax, n=-1)),
    position = (ymax + ymin) / 2,
    position_label = 1.05  # This will place the labels just outside the pie
  )

# Generate pie chart displaying regional and bilateral neuron distribution
pie_chart <- ggplot(processed_data, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = group)) +
  geom_rect(color = "white") +
  ggrepel::geom_text_repel(aes(label = label, x=4, y=position), 
                           nudge_x = .5,
                           size = 2) +
  geom_segment(aes(x = 4, y = position, xend = 4.05, yend = position), color = "black", size = 0.5) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = color_palette) +  # Assuming you've defined color_palette as before
  xlim(c(0, 4.5)) +  # Adjust this to leave more space for labels
  theme_void() +
  theme(legend.position = "none") +
  labs(fill = "region and side", 
       title = "")

print(pie_chart)

ggsave(plot = pie_chart,
       filename = file.path(banc.fig1.extra.path,"banc_inventory.pdf"), width = 6, height = 6, dpi = 300)

# ###########################
# #### META DATA SUMMARY ####
# ###########################
# 
# # Summarize the data
# franken.meta <- franken_meta()
# banc.meta <- banctable_query()
# summary_table <- banc.meta %>%
#   dplyr::filter(!is.na(super_class),
#                 super_class != "glia",
#                 !is.na(region)) %>%
#   dplyr::mutate( = dplyr::case_when(
#     grepl("^KC",cell_type) ~ "acetylcholine",
#     is.na() ~ "unclear",
#      %in% c("unclear","unknown")  ~ "unclear",
#     TRUE ~ 
#   )) %>%
#   group_by(region, super_class) %>%
#   summarise(
#     total = n(),
#     .groups = "keep"
#   ) %>%
#   left_join(
#     banc.meta %>%
#       dplyr::filter(!is.na(super_class),
#                     super_class != "glia",
#                     !is.na(region)) %>%
#       dplyr::mutate( = dplyr::case_when(
#         grepl("^KC",cell_type) ~ "acetylcholine",
#         is.na() ~ "unclear",
#          %in% c("unclear","unknown")  ~ "unclear",
#         TRUE ~ 
#       )) %>%
#       group_by(region, super_class, ) %>%
#       dplyr::summarise(count = n(), .groups = "drop") %>%
#       group_by(region, super_class) %>%
#       mutate(percentage = count / sum(count) * 100) %>%
#       select(-count) %>%
#       pivot_wider(names_from = , values_from = percentage, values_fill = 0),
#     by = c("region", "super_class")
#   ) %>%
#   dplyr::mutate(super_class = gsub("^brain_|^ventral_|^optic_|^neck_","",super_class)) %>%
#   dplyr::ungroup() %>%
#   dplyr::mutate(across(where(is.numeric), ~round(., 1))) %>%
#   dplyr::select(region, super_class, total, everything()) %>%
#   dplyr::mutate(super_class = gsub("_|neuron$"," ", super_class),
#                 region = gsub("_|neuron$"," ", region)) %>%
#   dplyr::mutate(region =  factor(region, levels = c("optic lobes","midbrain","sez","neck connective","vnc"))) %>%
#   dplyr::arrange(region, dplyr::desc(total))
# colnames(summary_table) <-  gsub("_"," ", colnames(summary_table))
# 
# # Custom color gradient function
# color_gradient <- function(colors, n) {
#   colorRampPalette(colors)(n)
# }
# 
# # Define regions for row grouping
# region_groups <- summary_table %>%
#   dplyr::group_by(region) %>%
#   dplyr::summarise(n = n()) %>%
#   deframe()
# 
# # Define the column spec
# nt_cols <- ncol(summary_table) - 3
# 
# # Short-hand names for the headers
# header_names <- c("region" = "region",
#                   "super class" = "super class", 
#                   "total" = "total",
#                   "acetylcholine" = "ach",
#                   "dopamine" = "dop",
#                   "gaba" = "gaba",
#                   "glutamate" = "glut",
#                   "serotonin" = "ser",
#                   "octopamine" = "oct",
#                   "unclear" = "unclear")
# 
# # Create the table
# kable_table <- summary_table %>%
#   rename_with(~ header_names[.], .cols = everything()) %>%
#   select(-region) %>%
#   kbl(format = "latex",  booktabs = TRUE, longtable = TRUE, linesep = "") %>%
#   kable_styling(latex_options = c("repeat_header"), font_size = 5) %>%
#   column_spec(1, width = "2cm") %>%
#   column_spec(2, width = "0.3cm") %>%
#   column_spec(3:ncol(summary_table), width = "0.2cm") %>%
#   add_header_above(c(" " = 1, "n." = 1, "(%)" = nt_cols)) %>%
#   pack_rows(index = region_groups, 
#             indent = FALSE, 
#             latex_gap_space = "0.3em")
# 
# # Apply color
# total_colors <- color_gradient(c("white", "#4D4D4D"), 100)
# kable_table <- kable_table %>%
#   column_spec(2, color = "black", background = total_colors[cut(summary_table$total, breaks = 100)])
# for (nt in names(paper.cols)) {
#   if (nt %in% colnames(summary_table)) {
#     col_index <- which(colnames(summary_table) == nt) - 1
#     nt_colors <- color_gradient(c("white", paper.cols[[nt]]), 100)
#     kable_table <- kable_table %>%
#       column_spec(col_index, 
#                   width = "0.25cm",
#                   color = "black", 
#                   background = nt_colors[cut(summary_table[[nt]], breaks = 100)])
#   }
# }
# 
# # Custom LaTeX preamble
# custom_preamble <- "
# \\documentclass[]{article}
# \\usepackage[scaled]{helvet}
# \\renewcommand{\\familydefault}{\\sfdefault}
# \\usepackage[T1]{fontenc}
# \\usepackage[margin=0.5cm,landscape,paperwidth=5in,paperheight=7.5in]{geometry}
# \\usepackage{booktabs}
# \\usepackage{longtable}
# \\usepackage{array}
# \\usepackage{multirow}
# \\usepackage{colortbl}
# \\setlength{\\tabcolsep}{0.5pt}
# \\renewcommand{\\arraystretch}{1.1}
# "
# 
# # Save as .pdf"
# kable_table %>%
#   save_kable(file.path(banc.fig1.supp.path,"banc_inventory_table.pdf"),
#              latex_engine = "xelatex", 
#              preamble = custom_preamble)
