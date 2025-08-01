##################################
## FIGURE 1: CROSS-DATASET   ##
##################################
# Compare connectivity patterns between BANC, FAFB, and MANC datasets
# Validates integration accuracy across different connectome datasets
# Output: figures/figure_1/links/connectivity_comparison.pdf
source("R/startup/banc-startup.R")
source("R/startup/franken-meta.R")
source("R/startup/banc-meta.R")
source("R/startup/banc-edgelist.R")
source("R/startup/franken-edgelist.R")

# # Read meta
# fw.meta <- suppressWarnings(readr::read_csv(file = file.path(banc.meta.save.path,"flywire_meta.csv"), 
#                                             col_types = hemibrainr:::sql_col_types))
# mc.meta <- suppressWarnings(readr::read_csv(file = file.path(banc.meta.save.path,"manc_meta.csv"), 
#                                             col_types = hemibrainr:::sql_col_types))
fw.meta <- franken.meta %>%
  dplyr::filter(!is.na(fafb_id))
mc.meta <- franken.meta %>%
  dplyr::filter(!is.na(manc_id))

###################################
## CONNECTIVITY SCATTER ANALYSIS ##
###################################
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

banc.edgelist.simple.ct <- banc.edgelist.simple %>%
  dplyr::group_by(pre_cell_type, post_cell_type) %>%
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
       filename = file.path(banc.fig1.extra.path,"dark_mode_franken_vs_banc_count_all.pdf"), 
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

# Save the plot (optional)
ggsave(plot = g2, 
       filename = file.path(banc.fig1.supp.path,"franken_vs_banc_norm_all.pdf"), 
       width = 8, height = 6, dpi = 300)
ggsave(plot = convert_to_dark_mode(g2), 
       filename = file.path(banc.fig1.supp.path,"dark_mode_franken_vs_banc_norm_all.pdf"), 
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

# Save the plot (optional)
ggsave(plot = post_plot, filename = file.path(banc.fig1.extra.path,"andn_post_norm_diff_pre_cell_type_distribution.pdf"), 
       width = 16, height = 8, dpi = 300)
ggsave(plot = pre_plot, filename = file.path(banc.fig1.extra.path,"andn_pre_norm_diff_pre_cell_type_distribution.pdf"), 
       width = 16, height = 8, dpi = 300)
