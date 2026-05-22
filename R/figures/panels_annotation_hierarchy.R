#' Annotation hierarchy coverage + Sankey diagram (ED Fig. 1)
#'
#' Visualises the BANC hierarchical annotation taxonomy described in
#' Methods §"Annotation taxonomy":
#'   flow → super_class → cell_class → cell_sub_class
#' plus the auxiliary labels nerve, hemilineage, body_part,
#' peripheral_target, region, cell_function.
#'
#' Produces (1) a faceted bar plot of annotation coverage per super_class
#' / category combination, and (2) an interactive plotly Sankey diagram
#' of the hierarchy. The Sankey is exploratory; the bar plot is the panel
#' that lands in ED Fig. 1.
#'
#' @section Reads:
#'   banc.meta                              (via R/startup/banc-meta.R)
#'
#' @section Writes:
#'   figures/figure_1/links/extra/annotation_coverage_*.pdf                  (bar plot)
#'   figures/figure_1/links/extra/annotation_sankey.html                      (interactive)
#'
#' @section Paper:
#'   ED Fig. 1 — annotation coverage by super_class.
#'   Methods §"Annotation taxonomy" — full taxonomy + Supplementary Data 1
#'   for the term list per category.
#'
#' @section Reproduce:
#'   BANC_NCORES=1 Rscript R/figures/panels_annotation_hierarchy.R

source("R/startup/banc-startup.R")
source("R/startup/banc-meta.R")

library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

# Output paths
out_path <- "figures/figure_1/links/extra"
dir.create(out_path, showWarnings = FALSE, recursive = TRUE)

#############################
## 1. ANNOTATION COVERAGE  ##
#############################

# Filter to valid neurons (those with super_class assigned)
valid <- banc.meta %>%
  filter_valid_neurons(only_proofread = FALSE, deduplicate = FALSE)

cat("Valid neurons:", nrow(valid), "\n")

# Simplify super_class for display
valid <- valid %>%
  dplyr::mutate(
    super_class_display = dplyr::case_when(
      grepl("sensory", super_class) ~ "sensory",
      grepl("ascending", super_class) ~ "ascending",
      grepl("descending", super_class) ~ "descending",
      grepl("motor|efferent", super_class) ~ "motor/efferent",
      grepl("visual_projection", super_class) ~ "visual projection",
      grepl("visual_centrifugal", super_class) ~ "visual centrifugal",
      grepl("optic_lobe", super_class) ~ "optic lobe",
      grepl("central", super_class) ~ "central",
      grepl("visceral", super_class) ~ "visceral/circulatory",
      grepl("endocrine", super_class) ~ "endocrine",
      TRUE ~ super_class
    )
  )

# Compute annotation coverage by super_class
annotation_cols <- c(
  "cell_class", "cell_sub_class", "cell_type",
  "nerve", "hemilineage",
  "body_part_sensory", "body_part_effector",
  "peripheral_target_type",
  "region", "cell_function"
)

# Prettify label names for display
label_map <- c(
  cell_class = "cell class",
  cell_sub_class = "cell sub-class",
  cell_type = "cell type",
  nerve = "nerve",
  hemilineage = "hemilineage",
  body_part_sensory = "body part (sensory)",
  body_part_effector = "body part (effector)",
  peripheral_target_type = "peripheral target",
  region = "region",
  cell_function = "cell function"
)

coverage <- valid %>%
  dplyr::group_by(super_class_display) %>%
  dplyr::summarise(
    n_total = dplyr::n(),
    across(all_of(annotation_cols),
           ~sum(!is.na(.) & . != "", na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  tidyr::pivot_longer(
    cols = all_of(annotation_cols),
    names_to = "annotation",
    values_to = "n_annotated"
  ) %>%
  dplyr::mutate(
    pct = 100 * n_annotated / n_total,
    annotation_label = label_map[annotation],
    annotation_label = factor(annotation_label, levels = rev(label_map))
  )

# Order super_class by total count
sc_order <- coverage %>%
  dplyr::distinct(super_class_display, n_total) %>%
  dplyr::arrange(desc(n_total)) %>%
  dplyr::pull(super_class_display)
coverage$super_class_display <- factor(coverage$super_class_display, levels = sc_order)

# Bar plot: annotation coverage by super_class
p_coverage <- ggplot(coverage,
                     aes(x = pct, y = annotation_label, fill = annotation_label)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.0f%%", pct)),
            hjust = -0.1, size = 2.5, colour = "grey30") +
  facet_wrap(~super_class_display, scales = "free_x") +
  scale_x_continuous(limits = c(0, 115), breaks = c(0, 50, 100)) +
  scale_fill_manual(values = rep(cerise_limon_palette(length(label_map)),
                                 length.out = length(label_map))) +
  labs(
    x = "% of neurons annotated",
    y = NULL,
    title = "Annotation coverage by super class",
    subtitle = sprintf("Valid neurons: %s", format(nrow(valid), big.mark = ","))
  ) +
  theme_minimal(base_size = 10) +
  theme(
    strip.text = element_text(face = "bold", size = 9),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 8)
  )

ggsave(file.path(out_path, "annotation_coverage_by_super_class.pdf"),
       p_coverage, width = 14, height = 10)
cat("Saved annotation coverage bar plot\n")

# Also make a compact overall version (not faceted)
overall_coverage <- valid %>%
  dplyr::summarise(
    n_total = dplyr::n(),
    across(all_of(annotation_cols),
           ~sum(!is.na(.) & . != "", na.rm = TRUE))
  ) %>%
  tidyr::pivot_longer(
    cols = all_of(annotation_cols),
    names_to = "annotation",
    values_to = "n_annotated"
  ) %>%
  dplyr::mutate(
    pct = 100 * n_annotated / n_total,
    annotation_label = label_map[annotation],
    annotation_label = factor(annotation_label, levels = label_map)
  )

p_overall <- ggplot(overall_coverage,
                    aes(x = pct, y = annotation_label, fill = annotation_label)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = sprintf("%s (%.0f%%)",
                                format(n_annotated, big.mark = ","), pct)),
            hjust = -0.05, size = 3, colour = "grey30") +
  scale_x_continuous(limits = c(0, 120), breaks = c(0, 25, 50, 75, 100)) +
  scale_fill_manual(values = cerise_limon_palette(length(label_map))) +
  labs(
    x = "% of neurons annotated",
    y = NULL,
    title = "Annotation coverage across all valid BANC neurons",
    subtitle = sprintf("n = %s neurons with super_class assigned",
                       format(nrow(valid), big.mark = ","))
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

ggsave(file.path(out_path, "annotation_coverage_overall.pdf"),
       p_overall, width = 8, height = 5)
cat("Saved overall annotation coverage plot\n")


########################################
## 2. HIERARCHICAL SANKEY (PLOTLY)    ##
########################################

library(plotly)

# Build hierarchy: flow -> super_class -> cell_class -> cell_sub_class
# Only use neurons that have at least super_class
sankey_data <- valid %>%
  dplyr::mutate(
    flow = ifelse(is.na(flow) | flow == "", "unassigned", flow),
    cell_class = ifelse(is.na(cell_class) | cell_class == "", "unassigned", cell_class),
    cell_sub_class = ifelse(is.na(cell_sub_class) | cell_sub_class == "", "unassigned", cell_sub_class)
  ) %>%
  dplyr::count(flow, super_class_display, cell_class, cell_sub_class, name = "count")

# Build node list: unique values at each level
flow_nodes <- sort(unique(sankey_data$flow))
sc_nodes <- sort(unique(sankey_data$super_class_display))
cc_nodes <- sort(unique(sankey_data$cell_class))
csc_nodes <- sort(unique(sankey_data$cell_sub_class))

# Prefix to avoid collisions between levels
all_nodes <- c(
  paste0("flow: ", flow_nodes),
  paste0("super_class: ", sc_nodes),
  paste0("cell_class: ", cc_nodes),
  paste0("cell_sub_class: ", csc_nodes)
)
node_idx <- setNames(seq_along(all_nodes) - 1L, all_nodes)

# Build links: flow -> super_class
links_f_sc <- sankey_data %>%
  dplyr::group_by(flow, super_class_display) %>%
  dplyr::summarise(value = sum(count), .groups = "drop") %>%
  dplyr::mutate(
    source = node_idx[paste0("flow: ", flow)],
    target = node_idx[paste0("super_class: ", super_class_display)]
  )

# Links: super_class -> cell_class
links_sc_cc <- sankey_data %>%
  dplyr::group_by(super_class_display, cell_class) %>%
  dplyr::summarise(value = sum(count), .groups = "drop") %>%
  dplyr::mutate(
    source = node_idx[paste0("super_class: ", super_class_display)],
    target = node_idx[paste0("cell_class: ", cell_class)]
  )

# Links: cell_class -> cell_sub_class
links_cc_csc <- sankey_data %>%
  dplyr::group_by(cell_class, cell_sub_class) %>%
  dplyr::summarise(value = sum(count), .groups = "drop") %>%
  dplyr::mutate(
    source = node_idx[paste0("cell_class: ", cell_class)],
    target = node_idx[paste0("cell_sub_class: ", cell_sub_class)]
  )

# Combine all links
all_links <- dplyr::bind_rows(links_f_sc, links_sc_cc, links_cc_csc)

# Clean node labels for display (remove prefix)
node_labels <- gsub("^(flow|super_class|cell_class|cell_sub_class): ", "", all_nodes)

# Assign colors by level
n_levels <- c(length(flow_nodes), length(sc_nodes), length(cc_nodes), length(csc_nodes))
level_colors <- c(
  rep("#4E79A7", n_levels[1]),  # flow: blue
  rep("#E15759", n_levels[2]),  # super_class: red
  rep("#59A14F", n_levels[3]),  # cell_class: green
  rep("#F28E2B", n_levels[4])   # cell_sub_class: orange
)

fig <- plot_ly(
  type = "sankey",
  orientation = "h",
  node = list(
    label = node_labels,
    color = level_colors,
    pad = 15,
    thickness = 20,
    line = list(color = "black", width = 0.5)
  ),
  link = list(
    source = all_links$source,
    target = all_links$target,
    value = all_links$value,
    color = "rgba(200,200,200,0.3)"
  )
) %>%
  layout(
    title = list(
      text = "BANC Annotation Hierarchy: flow → super_class → cell_class → cell_sub_class",
      font = list(size = 14)
    ),
    font = list(size = 10)
  )

# Save as interactive HTML
html_path <- file.path(out_path, "annotation_hierarchy_sankey.html")
htmlwidgets::saveWidget(fig, html_path, selfcontained = TRUE)
cat("Saved interactive Sankey diagram to", html_path, "\n")

cat("\nDone.\n")
