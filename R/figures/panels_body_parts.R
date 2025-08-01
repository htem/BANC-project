####################################
####################################
### DN TO EFFECTORS THRESHOLDING ###
####################################
####################################

###############
### STARTUP ###
###############

# load
source("R/startup/banc-startup.R")
source("R/startup/franken-meta.R")
source("R/startup/banc-meta.R")
source("R/startup/banc-edgelist.R")

# Create a mapping for DN categories
dn_categories <- c(
  "DNa02" = "DNa02: turning", 
  "DNa01" = "DNa01: turning",
  "DNp01" = "DNp01: escape", 
  "DNp02" = "DNp02: escape",
  "MDN" = "MDN: backwards walking", 
  "DNp50" = "DNp50: backwards walking", 
  "DNp42" = "DNp42: backwards walking",
  "DNg97" = "DNg97: forwards walking", 
  "DNg100" = "DNg100: forwards walking", 
  "DNg12" = "DNg12: grooming", 
  "DNg62" = "DNg62: grooming",
  "DNp07" = "DNp07: landing", 
  "DNp10" = "DNp10: landing",
  "DNa12" = "DNa12: abdomen curl", 
  "DNg14" = "DNg14: abdomen curl",
  "DNa15" = "DNa15: flight", 
  "DNb01" = "DNb01: flight",
  "DNp37" = "DNp37: oviposition", 
  "oviDNb" = "oviDNb: oviposition",
  "DNp20" = "DNp20: ocellar", 
  "DNp22" = "DNp22: ocellar",
  "DNp25" = "DNp25: hygrosensation", 
  "DNp44" = "DNp44: hygrosensation",
  "DNpe043" = "DNpe043: circadian", 
  "DNp27" = "DNp27: circadian",
  "AN17A026" = "AN17A026: walking", 
  "AN19A018" = "AN19A018: halting",
  "AN19A018_a" = "AN19A018: halting",
  "AN19A018_b" = "AN19A018: halting",
  "AN19A018_c" = "AN19A018: halting",
  "AN19A018_d" = "AN19A018: halting",
  "AN19A018_A3" = "AN19A018: halting",
  "AN19A018_a_A3" = "AN19A018: halting",
  "AN19A018_b_A3" = "AN19A018: halting",
  "AN19A018_c_A3" = "AN19A018: halting",
  "AN19A018_d_A3" = "AN19A018: halting",
  "AN07B043" = "AN07B043: grooming",
  "AN06B011" = "AN06B011: walking"
)
causal.dns <- c(names(dn_categories))
causal.dn.search <- paste(causal.dns,collapse="|")
chosen.seeds <- unique(banc.meta$seed_07[grepl(causal.dn.search,banc.meta$seed_07)])
chosen.seeds <- chosen.seeds[!grepl("auto",chosen.seeds)]

# Get alternative dataset for validation (seed_07)
con <- DBI::dbConnect(RSQLite::SQLite(),
                      file.path(banc.dropbox.influence.save.path,influence.sqlite))
key.neck.db <- dplyr::tbl(con, influence.table) %>%
  dplyr::filter(!is_seed,
                seed %in% causal.dns,
                level %in% c("seed_07"),
                id %in% !!banc.eff.meta$root_id) %>%
  dplyr::collect()
dbDisconnect(con)

# Calculate scores
causal.dns <- unique(key.neck.db$seed)
dn_categories <- dn_categories[names(dn_categories)%in%causal.dns]
key.neck.db <- key.neck.db %>%
  calculate_influence_norms() %>%
  dplyr::left_join(banc.meta %>%
                     dplyr::distinct(id, .keep_all = TRUE),
                   by = c("id"))

# What would direct connectivity look like?
banc.chosen.dn.el <- banc.edgelist.simple %>%
  dplyr::filter(pre_cell_type %in% causal.dns,
                post %in% !!banc.eff.meta$id)

# First, prepare a summary of direct connections for each DN-target pair
# Using norm instead of count for direct connection strength
direct_conn_summary <- banc.chosen.dn.el %>%
  # Match pre_cell_type to seed and post to target_id
  dplyr::mutate(seed = pre_cell_type,
                target_id = post) %>%
  # Group by seed and target_id
  dplyr::group_by(seed, target_id) %>%
  # Sum norms for each DN-target pair (instead of counts)
  dplyr::summarize(direct_connections_count = sum(count, na.rm = TRUE),
                   direct_connections = sum(norm, na.rm = TRUE),
                   .groups = 'drop')

# Data manipulation and annotation creation
inf.metric <- "influence_norm_log_minmax"
heatmap_matrix <- reshape2::acast(
  data = key.neck.db %>%
    dplyr::filter(body_part_effector!="unknown"),
  formula = id ~ seed,
  value.var = "influence_norm_log",
  fun.aggregate = function(x) mean(x, na.rm = TRUE)
)
heatmap_matrix[is.na(heatmap_matrix)] <- 0
heatmap_matrix[is.infinite(heatmap_matrix)] <- 0

# Create scaled color palette 
scaled_heatmap_breaks <- seq(quantile(heatmap_matrix,0.01, na.rm=TRUE), quantile(heatmap_matrix,0.999, na.rm=TRUE), length.out = n_breaks)
scaled_heatmap_palette <- colorRampPalette(c("#1f4e79", "#4a90a4", "#7ba7bc", "#a67c8a", "#c4967d", "#b22222"))(n_breaks - 1)

# Create annotation for cell types (rows)
cell_type_annotation <- key.neck.db %>%
  dplyr::distinct(id, body_part_effector, region) %>%
  dplyr::mutate(body_part_effector=ifelse(is.na(body_part_effector),"unknown",body_part_effector)) %>%
  dplyr::mutate(
    region = factor(region, levels = class.order)
  ) %>%
  dplyr::arrange(region, body_part_effector) %>%
  dplyr::distinct(id, .keep_all = TRUE) %>%
  dplyr::filter(!is.na(id), id %in% rownames(heatmap_matrix)) %>%
  column_to_rownames("id")

# Reorder rows by super_class and region
heatmap_matrix <- heatmap_matrix[rownames(heatmap_matrix) %in% rownames(cell_type_annotation),]
cell_type_annotation <- cell_type_annotation[rownames(cell_type_annotation)%in%rownames(heatmap_matrix),]

# Group cell types by super_class
groups <- split(rownames(cell_type_annotation), cell_type_annotation$body_part_effector)

# Filter out groups with fewer than two elementshclust_semisupervised
groups <- lapply(groups, function(g) if(length(g) >= 2) g else NULL)
groups <- groups[!sapply(groups, is.null)]

# Apply semi-supervised clustering
clustering_result <- hclust_semisupervised(data = heatmap_matrix,
                                           groups = groups,
                                           dist_method = "euclidean",
                                           hclust_method = "ward.D2")
heatmap_matrix_normalized <- clustering_result$data
cell_type_annotation <- cell_type_annotation[rownames(heatmap_matrix_normalized), , drop = FALSE]

# Annotation colors
annotation_colors <- list(
  super_class = paper.cols[names(paper.cols) %in% unique(cell_type_annotation$super_class)]
)

# Cosine similarity
col_dist <- dist(t(heatmap_matrix_normalized), method = "euclidean")
euclidean_dist_matrix_cols <- hclust(col_dist, method = "ward.D2")

# Create the heatmap
pheatmap(
  heatmap_matrix_normalized,
  color = scaled_heatmap_palette,
  breaks = scaled_heatmap_breaks,
  annotation_row = cell_type_annotation,
  annotation_colors = annotation_colors,
  clustering_method = "ward.D2",
  cluster_rows = clustering_result$hclust,
  cluster_cols = euclidean_dist_matrix_cols,
  treeheight_row = 0,
  treeheight_col = 0,
  show_rownames = FALSE,
  show_colnames = TRUE,
  fontsize_row = 6,
  fontsize_col = 10,
  cellwidth = 12,
  cellheight = 1,
  annotation_names_col = FALSE,
  annotation_names_row = FALSE,
  filename = file.path(banc.fig2.extra.path, sprintf("%s_causal_dns_to_effectors_heatmap.pdf",inf.metric))
)

# Convert the heatmap matrix to a long format dataframe suitable for plotting
inf.metric <- "influence_norm_log"
heatmap_df <- as.data.frame(heatmap_matrix_normalized) %>%
  tibble::rownames_to_column("target_id") %>%
  tidyr::pivot_longer(cols = -target_id, 
                      names_to = "seed", 
                      values_to = inf.metric) %>%
  # Join with annotation data to get body_part_effector info
  dplyr::left_join(cell_type_annotation %>% 
                     rownames_to_column("target_id") %>%
                     dplyr::select(target_id, body_part_effector, region),
                   by = "target_id") %>%
  # Filter for only the causal DNs we're interested in
  dplyr::filter(seed %in% causal.dns) %>%
  # Add the category labels
  dplyr::mutate(seed_category = factor(dn_categories[seed], levels = dn_categories[causal.dns]))

# Sort values for elbow method visualization
elbow_df <- heatmap_df %>%
  arrange(desc(influence_norm_log)) %>%
  mutate(rank = row_number())

# Function to find the most significant angle change in a specific range
find_angle_change_in_range <- function(ranks, values, start_rank, end_rank, window_size = 100) {
  # Filter data to the specified range
  idx_in_range <- which(ranks >= start_rank & ranks <= end_rank)
  ranks_subset <- ranks[idx_in_range]
  values_subset <- values[idx_in_range]
  
  # Skip if too few points
  if(length(ranks_subset) <= 2*window_size) {
    return(list(rank = NA, value = NA))
  }
  
  # Calculate angles
  angles <- numeric(length(ranks_subset) - 2*window_size)
  for(i in (window_size+1):(length(ranks_subset)-window_size)) {
    p1 <- c(ranks_subset[i-window_size], values_subset[i-window_size])
    p2 <- c(ranks_subset[i], values_subset[i])
    p3 <- c(ranks_subset[i+window_size], values_subset[i+window_size])
    
    # Calculate vectors
    v1 <- c(p2[1] - p1[1], p2[2] - p1[2])
    v2 <- c(p3[1] - p2[1], p3[2] - p2[2])
    
    # Normalize vectors
    v1 <- v1 / sqrt(sum(v1^2))
    v2 <- v2 / sqrt(sum(v2^2))
    
    # Calculate angle (in degrees)
    angles[i-window_size] <- acos(sum(v1 * v2)) * (180/pi)
  }
  
  # Find maximum angle
  max_angle_idx <- which.max(angles) + window_size
  max_angle_rank <- ranks_subset[max_angle_idx]
  max_angle_value <- values_subset[max_angle_idx]
  
  return(list(rank = max_angle_rank, value = max_angle_value))
}

# Sample the curve at regular intervals
sample_indices <- round(seq(1, nrow(elbow_df), length.out = 1000))
ranks <- elbow_df$rank[sample_indices]
values <- elbow_df$influence_norm_log[sample_indices]

# Find the angle change specifically in the range where we expect the first bend to end
first_bend_end <- find_angle_change_in_range(ranks, values, 500, 2000, window_size = 20)

# Join elbow_df with direct connection data
elbow_df_with_direct <- elbow_df %>%
  dplyr::left_join(direct_conn_summary, by = c("seed", "target_id")) %>%
  dplyr::mutate(
    direct_connections = ifelse(is.na(direct_connections), 0, direct_connections),
    direct_connections_count = ifelse(is.na(direct_connections_count), 0, direct_connections_count),
    has_direct_conn = ifelse(direct_connections > 0, "direct connection", "no direct connection")
  ) %>%
  dplyr::arrange(dplyr::desc(direct_connections_count))

# Create the main elbow plot
elbow_main_plot <- ggplot(elbow_df_with_direct, 
                          aes(x = rank, y = influence_norm_log)) +
  # Add a line connecting all points
  geom_line(color = "gray70", alpha = 0.6) +
  
  # Points with no direct connections (norm == 0) in black
  geom_point(data = elbow_df_with_direct %>% filter(direct_connections == 0),
             color = "lightgrey", 
             size = 0.5, 
             alpha = 0.6) +
  
  # Points with direct connections colored by their norm value (blue to red)
  geom_point(data = elbow_df_with_direct %>% 
               dplyr::filter(direct_connections > 0),
             aes(color = direct_connections), 
             size = 0.5, 
             alpha = 0.8) +
  
  # Use a color scale from blue to red (no white)
  scale_color_gradientn(
    colors = colorRampPalette(c("#1BB6AF", "#EE4244"))(100),
    name = "direct connections\n/ total postsynapses",
    trans = "log10",
    breaks = c(0.01, 0.05, 0.1, 0.2, 0.5),
    labels = c("0.01", "0.05", "0.1", "0.2", "0.5")
  ) +
  
  # Add threshold lines
  geom_vline(xintercept = first_bend_end$rank, color = "black", linetype = "dashed") +
  geom_hline(yintercept = first_bend_end$value, color = "black", linetype = "dashed") +
  
  # Set x-axis limits to ensure consistency with density plot
  xlim(0, max(elbow_df_with_direct$rank)) +
  
  # Improve theme
  theme_minimal(base_size = 12) +
  theme( 
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray95"),
    legend.position = "right",
    legend.key.height = unit(1, "cm"),
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 12),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8),
    panel.border = element_rect(color = "gray80", fill = NA)
  ) +
  
  # Add labels
  labs(
    title = "",
    subtitle = "",
    y = "influence_norm_log"
  ) +
  
  # Add annotation for threshold
  annotate("text", 
           x = 5000, 
           y = first_bend_end$value + 0.5,
           label = paste("threshold =", round(first_bend_end$value, 3)),
           color = "black", 
           fontface = "bold")

# Create the density plot
density_plot <- ggplot(elbow_df_with_direct, 
                       aes(x = rank, fill = has_direct_conn, color = has_direct_conn)) +
  geom_density(alpha = 0.7, position = "identity") +
  scale_fill_manual(values = c("direct connection" = "#EE4244", 
                               "no direct connection" = "darkgrey"),
                    name = "connection type") +
  scale_color_manual(values = c("direct connection" = "#EE4244", 
                               "no direct connection" = "darkgrey"),
                    name = "connection type") +
  geom_vline(xintercept = first_bend_end$rank, color = "black", linetype = "dashed") +
  
  # Set x-axis limits to match main plot
  xlim(0, max(elbow_df_with_direct$rank)) +
  
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray95"),
    legend.position = "bottom",
    panel.border = element_rect(color = "gray80", fill = NA)
  ) +
  labs(
    x = "rank (sorted influence values)",
    y = "density"
  )

# Create a combined plot using cowplot
# Step 1: Create the plots without legends for alignment
elbow_main_no_legend <- elbow_main_plot + theme(legend.position = "none")
density_no_legend <- density_plot + theme(legend.position = "none")

# Step 2: Combine the main plots
combined_plots <- plot_grid(
  elbow_main_no_legend,
  density_no_legend,
  ncol = 1,
  align = "v",
  rel_heights = c(3, 1),
  labels = c("", "")
)

# Step 3: Extract legends
legend_elbow <- get_legend(
  elbow_main_plot + theme(legend.box.margin = margin(0, 0, 0, 12))
)

legend_density <- get_legend(
  density_plot + theme(legend.position = "bottom")
)

# Step 4: Combine legends vertically
legend_combined <- plot_grid(
  legend_elbow, 
  legend_density, 
  ncol = 1,
  rel_heights = c(3, 1)
)

# Step 5: Add the legends to the right side of the combined plot
final_plot <- plot_grid(
  combined_plots, 
  legend_combined,
  ncol = 2,
  rel_widths = c(4, 1)
)

# Print and save the plot
print(final_plot)
ggsave(file.path(banc.fig2.supp.path, sprintf("%s_dn_to_effector_influence_elbow_maximum_curvature.pdf",inf.metric)), 
       final_plot, width = 8, height = 8, dpi = 300)

# Get the order of targets from the heatmap clustering
target_order <- rownames(heatmap_matrix_normalized)[clustering_result$hclust$order]

# Calculate global y-limits for consistent scale
y_min <- min(heatmap_df$influence_norm_log, na.rm = TRUE)
y_max <- max(heatmap_df$influence_norm_log, na.rm = TRUE)

# Calculate potential threshold
threshold.inf.value <- first_bend_end$value

# Join this information with your heatmap_df
heatmap_df_with_direct <- heatmap_df %>%
  dplyr::left_join(direct_conn_summary, by = c("seed", "target_id")) %>%
  # Replace NA with 0 for targets without direct connections
  dplyr::mutate(
    direct_connections = ifelse(is.na(direct_connections), 0, direct_connections),
    direct_connections_count = ifelse(is.na(direct_connections_count), 0, direct_connections_count),
    # Create a log-transformed version for better visualization (adding small constant to avoid log(0))
    log_direct_connections = ifelse(direct_connections > 0, 
                                    log1p(direct_connections), 0),
    # Create a categorical variable for connection type
    connection_type = case_when(
      direct_connections > 0 & influence_norm_log >= threshold.inf.value ~ "both",
      direct_connections > 0 ~ "direct only",
      influence_norm_log >= threshold.inf.value ~ "indirect only",
      TRUE ~ "no significant connection"
    )
  )

# Order body parts
body.parts <- c("retrocerebral_complex", "digestive_tract", "enteric", "crop", "salivary_gland", 
                "pharynx", "proboscis", "antenna", "eye", "neck", 
                "haltere", "wing", "front_leg", "middle_leg", "hind_leg", "ureter", 
                "abdomen", "reproductive_tract", "ovaries", "uterus", "neurohemal_complex")
heatmap_df_with_direct$body_part_effector <- factor(heatmap_df_with_direct$body_part_effector,
                                                    body.parts)
heatmap_df_with_direct <- heatmap_df_with_direct %>%
  dplyr::filter(!is.na(body_part_effector)) %>%
  dplyr::arrange(body_part_effector)
target_order <- unlist(groups[body.parts])

# Modified plot with log size encoding for direct connections and shape for connection type
dn_line_plots_improved <- ggplot(heatmap_df_with_direct, 
                                 aes(x = factor(target_id,levels = target_order), 
                                     y = influence_norm_log,
                                     color = body_part_effector,
                                     group = 1)) +
  # Add line for overall influence pattern
  geom_line(linewidth = 0.3, 
            alpha = 0.6) +
  # Add points with size proportional to direct connection strength and shape by connection type
  geom_point(aes(size = log_direct_connections + 0.1, 
                 shape = connection_type), 
             alpha = 0.8) +
  # Add threshold line
  geom_hline(yintercept = threshold.inf.value, 
             linetype = "dashed", 
             color = "black", 
             alpha = 0.7) +
  # Use custom shape scale
  scale_shape_manual(values = c(
    "both" = 16,          
    "direct only" = 16,    
    "indirect only" = 1,  
    "no significant connection" = 1  
  )) +
  # Use log scale for size (adding small constant to show points with zero direct connections)
  scale_size_continuous(
    name = "normalised direct connections", 
    breaks = log1p(c(0.01, 0.05, 0.1, 0.2, 0.5)),
    labels = c(0.01, 0.05, 0.1, 0.2, 0.5),
    range = c(0.5, 3)
  ) +
  scale_color_manual(values = paper.cols) +
  #scale_color_cerise_limon() +
  # Facet by DN type
  facet_wrap(~ seed_category, ncol = 3) +
  theme_minimal() +
  coord_cartesian(ylim = c(y_min, y_max)) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position = "right",
    strip.text = element_text(size = 12, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 14)
  ) +
  labs(
    title = "",
    subtitle = paste0("point size: direct connection strength (norm) | ",
                      "shapes: connection type | ",
                      "threshold = ", round(threshold.inf.value, 3)),
    x = "efferent neuron targets",
    y = inf.metric,
    color = "body part effector",
    shape = "connection type"
  )

# Save the plot with updated facet labels and consistent y-scale
print(dn_line_plots_improved)
ggsave(file.path(banc.fig2.extra.path, sprintf("%s_dn_to_effector_influence_lineplot_consistent_scale.pdf",inf.metric)), 
       dn_line_plots_improved, width = 30, height = 15, dpi = 300)

# Modified plot with log size encoding for direct connections and shape for connection type
select_dn_line_plots_improved <- ggplot(heatmap_df_with_direct %>%
                                   dplyr::filter(seed %in% c("DNa02","DNa15","oviDNb","AN19A018_d")), 
                                 aes(x = factor(target_id, 
                                                levels = target_order), 
                                     y = influence_norm_log,
                                     color = body_part_effector,
                                     group = 1)) +
  # Add line for overall influence pattern
  geom_line(linewidth = 0.3, 
            alpha = 0.6) +
  # Add points with size proportional to direct connection strength and shape by connection type
  geom_point(aes(size = log_direct_connections + 0.1, 
                 shape = connection_type), 
             alpha = 0.8) +
  geom_hline(yintercept = threshold.inf.value, 
             linetype = "dashed", 
             color = "black", 
             alpha = 0.7) +
  scale_shape_manual(values = c(
    "both" = 16,          
    "direct only" = 16,    
    "indirect only" = 1,  
    "no significant connection" = 1  
  )) +
  scale_size_continuous(
    name = "normalised direct connections", 
    breaks = log1p(c(0.01, 0.05, 0.1, 0.2, 0.5)),
    labels = c(0.01, 0.05, 0.1, 0.2, 0.5),
    range = c(0.5, 3)
  ) +
  scale_color_manual(values = paper.cols) +
  #scale_color_cerise_limon() +
  facet_wrap(~ seed_category, ncol = 2) +
  theme_minimal() +
  coord_cartesian(ylim = c(y_min, y_max)) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position = "right",
    strip.text = element_text(size = 12, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 14)
  ) +
  labs(
    title = "",
    subtitle = paste0("point size: direct connection strength (norm) | ",
                      "shapes: connection type | ",
                      "threshold = ", round(threshold.inf.value, 3)),
    x = "efferent neuron targets",
    y = inf.metric,
    color = "body part effector",
    shape = "connection type"
  ) +
  guides(shape="none",size="none")

# Save the plot with updated facet labels and consistent y-scale
print(select_dn_line_plots_improved)
ggsave(file.path(banc.fig2.extra.path, sprintf("%s_select_dn_to_effector_influence_lineplot.pdf",inf.metric)), 
       select_dn_line_plots_improved, width = 16, height = 8, dpi = 300)

# Make plot
g.jitter <- ggplot(heatmap_df_with_direct  %>%
                     dplyr::filter(seed %in% c("DNa02","DNa15","oviDNb","AN06B011")), 
                   aes(x = body_part_effector, 
                           y = influence_norm_log, 
                           group = body_part_effector,
                           color = connection_type)) +
  geom_jitter(width = 0.2, alpha = 0.7, size = 2) + 
  stat_summary(fun = mean, geom = "point", shape = 23, size = 4, 
               fill = "black", 
               color = "black", 
               position = position_nudge(x = 0)) +
  stat_summary(fun.data = mean_sdl, 
               fun.args = list(mult = 1), 
               geom = "errorbar", 
               width = 0.15, 
               color = "black") +
  labs(x = "", y = inf.metric) +
  scale_color_manual(values = c(
    "both" = highlight.col,
    "direct only" = highlight.col2,
    "indirect only" = "lightgrey",
    "no significant connection"="darkgrey"
  )) +
  geom_hline(yintercept = threshold.inf.value, 
             linetype = "solid", 
             color = "black", 
             alpha = 0.7) +
  facet_wrap(~ seed_category, ncol = 1) +
  scale_x_discrete(labels = function(x) gsub("_", " ", x)) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = 1, hjust=1))

# Save the plot with updated facet labels and consistent y-scale
print(g.jitter)
ggsave(file.path(banc.fig2.supp.path, sprintf("%s_select_dn_to_effector_influence_jitterplot.png",inf.metric)), 
       g.jitter, width = 4, height = 14, dpi = 300)

###########################
###########################
### ASSESS TRIAGED POOL ###
###########################
###########################

# Get all neuron seeds
chosen.seeds <- unique(banc.meta$seed_12[grepl("^AN|^DN",banc.meta$seed_12)])

# Get alternative dataset for validation (seed_03)
con <- DBI::dbConnect(RSQLite::SQLite(),
                      file.path(banc.dropbox.influence.save.path, influence.sqlite))
inf.neck.db <- dplyr::tbl(con, influence.table) %>%
  dplyr::filter(!is_seed,
                seed %in% chosen.seeds,
                level %in% c("seed_12"), 
                id %in% !!banc.eff.meta$root_id) %>%
  dplyr::collect() 
DBI::dbDisconnect(con)

# Calculate norms
inf.neck.db <- inf.neck.db %>%
  calculate_influence_norms() %>%
  dplyr::left_join(banc.meta %>%
                     dplyr::distinct(id, .keep_all = TRUE),
                   by = c("id"))

# Extract seed type (AN or DN) and rename to full names
inf.neck.db <- inf.neck.db %>%
  dplyr::filter(grepl("^AN", seed) | grepl("^DN", seed)) %>%
  dplyr::mutate(seed_type = dplyr::case_when(
    grepl("^AN", seed) ~ "ascending",
    grepl("^DN", seed) ~ "descending",
    TRUE ~ "other"
  ))

# Group by seed and determine if any influence score is above threshold
seed_groups <- inf.neck.db %>%
  dplyr::group_by(seed, seed_type) %>%
  dplyr::summarize(
    max_influence_log = max(influence_norm_log, na.rm = TRUE),
    in_group = max_influence_log >= threshold.inf.value,
    .groups = "drop"
  ) %>%
  dplyr::mutate(group_status = ifelse(in_group, "above threshold", "below threshold"))

# Create a combined grouping variable for easier visualization
seed_groups <- seed_groups %>%
  dplyr::mutate(combined_group = paste(seed_type, group_status),
                root_id = gsub(".*_","",seed)) 

# Count entries in each group
group_counts <- seed_groups %>%
  dplyr::group_by(seed_type, group_status, combined_group) %>%
  dplyr::summarize(count = n(), .groups = "drop")

# Print summary table
print("Summary of Seeds by Group:")
print(group_counts)
readr::write_csv(x=seed_groups,file="data/meta/banc_neck_inclusion.csv")

# ----------------------
# 1a. Create density plots for the four possible groupings
# ----------------------

# Join the group information back to the original data
inf.neck.db_with_groups <- inf.neck.db %>%
  dplyr::left_join(seed_groups %>% dplyr::select(seed, group_status), by = "seed")

# Create the density plot with updated colors and lowercase text
density_plot <- ggplot2::ggplot(inf.neck.db_with_groups, 
                                ggplot2::aes(x = influence_norm_log, fill = group_status)) +
  ggplot2::geom_density(alpha = 0.7) +
  ggplot2::geom_vline(xintercept = threshold.inf.value, linetype = "dashed", color = "red") +
  ggplot2::facet_wrap(~ seed_type, scales = "free_y") +
  ggplot2::scale_fill_manual(values = c("above threshold" = paper.cols[["ascending"]], 
                                        "below threshold" = paper.cols[["descending"]])) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    legend.position = "bottom",
    panel.spacing = ggplot2::unit(1, "lines"),
    strip.text = ggplot2::element_text(face = "bold", size = 12),
    plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")
  ) +
  ggplot2::labs(
    title = "distribution of influence scores by neuron type and group",
    subtitle = paste("threshold =", round(threshold.inf.value, 3)),
    x = "influence score (log)",
    y = "density",
    fill = "group status"
  )

# ----------------------
# 1b. Create bar plot of counts
# ----------------------

# Create a bar plot showing the number of seeds in each group
bar_plot <- ggplot2::ggplot(group_counts, 
                            ggplot2::aes(x = seed_type, y = count, fill = group_status)) +
  ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge()) +
  ggplot2::geom_text(ggplot2::aes(label = count), 
                     position = ggplot2::position_dodge(width = 0.9), 
                     vjust = -0.5, size = 4) +
  ggplot2::scale_fill_manual(values = c("above threshold" = paper.cols[["ascending"]], 
                                        "below threshold" = paper.cols[["descending"]])) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    legend.position = "bottom",
    plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")
  ) +
  ggplot2::labs(
    title = "",
    x = "neuron type",
    y = "count",
    fill = "group status"
  )

# Combine the density and bar plots
group_analysis_grid <- gridExtra::grid.arrange(
  density_plot, bar_plot,
  ncol = 1,
  heights = c(2, 1)
)

# Save the combined plot
plot(group_analysis_grid)
ggplot2::ggsave(file.path(banc.fig2.extra.path, "seed_group_analysis.pdf"), 
                group_analysis_grid, width = 8, height = 8, dpi = 300)

# Create additional summary statistics
summary_stats <- seed_groups %>%
  dplyr::group_by(seed_type, group_status) %>%
  dplyr::summarize(count = n(),
                   avg_max_influence = mean(max_influence_log),
                   median_max_influence = median(max_influence_log),
                   .groups = "drop")

print("Summary Statistics by Group:")
print(summary_stats)

# ----------------------
# 2. Create an enhanced heatmap using semi-supervised clustering, only showing neurons above threshold
# ----------------------

# Define number of color breaks
n_breaks <- 100

# First, identify seeds that are above threshold
seeds_above_threshold <- seed_groups %>%
  dplyr::filter(group_status == "above threshold") %>%
  dplyr::pull(seed)

# Prepare data for the heatmap - calculate mean influence score for each seed-body part combination
# Only include seeds that are above threshold
heatmap_data <- inf.neck.db %>%
  dplyr::filter(!is.na(body_part_effector),
                body_part_effector != "unknown",
                seed %in% seeds_above_threshold) %>%
  dplyr::group_by(seed, body_part_effector) %>%
  dplyr::summarize(
    mean_influence_log = mean(influence_norm_log, na.rm = TRUE),
    .groups = "drop"
  )

# Only proceed if there's data to plot
if(nrow(heatmap_data) > 0) {
  # Convert to a wide format for the heatmap
  heatmap_matrix <- reshape2::acast(
    heatmap_data,
    seed ~ body_part_effector, 
    value.var = "mean_influence_log",
    fun.aggregate = function(x) mean(x, na.rm = TRUE)
  )
  
  # Handle NA and infinite values
  heatmap_matrix[is.na(heatmap_matrix)] <- 0
  heatmap_matrix[is.infinite(heatmap_matrix)] <- 0
  
  # Create scaled color palette with specified colors
  scaled_heatmap_breaks <- seq(quantile(heatmap_matrix, 0.01, na.rm = TRUE), 
                               quantile(heatmap_matrix, 0.99, na.rm = TRUE), 
                               length.out = n_breaks)
  scaled_heatmap_palette <- colorRampPalette(c("#1f4e79", "#4a90a4", "#7ba7bc", 
                                               "#a67c8a", "#c4967d", "#b22222"))(n_breaks - 1)
  
  # Create annotation for rows (seeds) with lowercase names
  row_annotation <- seed_groups %>%
    dplyr::filter(seed %in% rownames(heatmap_matrix)) %>%
    dplyr::select(seed, seed_type) %>%  # No need for group_status as all are above threshold now
    tibble::column_to_rownames("seed")
  
  # Define annotation colors using paper colors - now just for seed type
  annotation_colors <- list(
    seed_type = c(`ascending` = paper.cols[["ascending"]], 
                  `descending` = paper.cols[["descending"]])
  )
  
  # Group seeds by seed type (ascending/descending) for semi-supervised clustering
  seed_type_groups <- split(rownames(row_annotation), row_annotation$seed_type)
  
  # Filter out groups with fewer than two elements
  seed_type_groups <- lapply(seed_type_groups, function(g) if(length(g) >= 2) g else NULL)
  seed_type_groups <- seed_type_groups[!sapply(seed_type_groups, is.null)]
  
  # Apply semi-supervised clustering
  clustering_result <- hclust_semisupervised(
    data = heatmap_matrix,
    groups = seed_type_groups,
    dist_method = "euclidean",
    hclust_method = "ward.D2"
  )
  
  # Store the reordered data and clustering
  heatmap_matrix_reordered <- clustering_result$data
  row_annotation_reordered <- row_annotation[rownames(heatmap_matrix_reordered), , drop = FALSE]
  
  # For column clustering, use standard hierarchical clustering
  col_dist <- dist(t(heatmap_matrix_reordered), method = "euclidean")
  col_hclust <- hclust(col_dist, method = "ward.D2")
  
  # Create the enhanced heatmap
  enhanced_heatmap <- pheatmap::pheatmap(
    heatmap_matrix_reordered,
    color = scaled_heatmap_palette,
    breaks = scaled_heatmap_breaks,
    annotation_row = row_annotation_reordered,
    annotation_colors = annotation_colors,
    cluster_rows = clustering_result$hclust,
    cluster_cols = col_hclust,
    treeheight_row = 20,
    treeheight_col = 20,
    show_rownames = FALSE,
    show_colnames = TRUE,
    fontsize_row = 6,
    fontsize_col = 10,
    cellwidth = 10,
    cellheight = 0.15,
    main = "clustering of neurons by body part effector influence\n(above threshold ascending and descendings only)",
    annotation_names_col = FALSE,
    annotation_names_row = FALSE,
    filename = file.path(banc.fig2.extra.path, "influence_norm_log_seed_body_part_heatmap_filtered.pdf")
  )
  
  # Also create a PNG version
  png_file <- file.path(banc.fig2.extra.path, "influence_norm_log_seed_body_part_heatmap_filtered.pdf")
  png(png_file, width = 3000, height = 2400, res = 300)
  print(enhanced_heatmap)
  dev.off()
} else {
  cat("No neurons above threshold for heatmap\n")
}

# ----------------------
# 4. Create combinations visualizations
# ----------------------

# Extract combinations by seed
effector_combinations <- inf.neck.db %>%
  dplyr::filter(influence_norm_log >= threshold.inf.value) %>%
  dplyr::mutate(region = dplyr::case_when(
    is.na(body_part_effector)|body_part_effector=="" ~ NA,
    region=="neck_connective" ~ "central_brain",
    region=="optic_lobe" ~ "central_brain",
    grepl("brain",region) ~ "central_brain",
    grepl("vnc",region) ~ "ventral_nerve_cord",
    TRUE ~ region
  )) %>%
  dplyr::mutate(seed_cell_type = gsub("_.*","",seed)) %>%
  dplyr::group_by(seed_cell_type, seed_type) %>%
  dplyr::summarize(
    body_parts = list(sort(unique(body_part_effector[!is.na(body_part_effector)]))),
    combination = paste(sort(unique(body_part_effector[!is.na(body_part_effector)])), collapse = "+"),
    regions = paste(sort(unique(region[!is.na(region)])), collapse = "+"),
    super_classes = paste(sort(unique(super_class[!is.na(super_class)])), collapse = "+"),
    above_threshold = TRUE,
    .groups = "drop"
  ) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(body_part_n = length(unique(body_parts))) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(super_classes = ifelse(combination=="",NA,super_classes),
                regions = ifelse(regions=="",NA,regions))

# Also add seeds with no combinations above threshold
seeds_missing <- inf.neck.db %>%
  dplyr::mutate(seed_cell_type = gsub("_.*","",seed)) %>%
  dplyr::anti_join(effector_combinations, by = "seed_cell_type") %>%
  dplyr::distinct(seed_cell_type, seed_type) %>%
  dplyr::mutate(
    body_parts = list(character(0)),
    combination = "",
    above_threshold = FALSE
  )

# Combine and return
effector_combinations <- dplyr::bind_rows(effector_combinations, 
                                          seeds_missing)

combo_counts <- effector_combinations %>%
  dplyr::filter(above_threshold, body_part_n>0) %>%
  dplyr::group_by(regions, combination, seed_type) %>%
  dplyr::mutate(
    num_parts = sapply(strsplit(as.character(combination), "\\+"), length)
  ) %>%
  dplyr::group_by(seed_type, regions, num_parts) %>%
  dplyr::summarize(count = dplyr::n(), 
                   .groups = "drop")

g.combo <- ggplot(combo_counts,
       aes(x = factor(num_parts), y = count, fill = regions)) +
  geom_col(color = "white") +                        
  facet_wrap(~ seed_type) +                      
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.title = element_text(),
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  ) +
  labs(
    title = "",
    x = "number of combined body parts",
    y = "number of neck neurons",
    fill = "regions"
  ) +
  ggplot2::scale_fill_manual(values = c(ventral_nerve_cord=paper.cols[["ventral_nerve_cord"]],
                                        central_brain=paper.cols[["central_brain"]],
                                        `central_brain+ventral_nerve_cord`=paper.cols[["neck_connective"]]),
                             na.value = "lightgrey")

# Show
print(g.combo)

# Save
ggsave(file.path(banc.fig2.supp.path, sprintf("%s_body_part_combination_region_histogram.pdf",inf.metric)), 
       g.combo, 
       width = 8, height = 4, dpi = 300)

combo_counts <- effector_combinations %>%
  dplyr::filter(above_threshold, body_part_n>0) %>%
  dplyr::group_by(super_classes, combination, seed_type) %>%
  dplyr::mutate(
    num_parts = sapply(strsplit(as.character(combination), "\\+"), length)
  ) %>%
  dplyr::group_by(seed_type, super_classes, num_parts) %>%
  dplyr::summarize(count = dplyr::n(), 
                   .groups = "drop")

g.combo2 <- ggplot(combo_counts,
                  aes(x = factor(num_parts), y = count, fill = super_classes)) +
  geom_col(color = "white") +                        
  facet_wrap(~ seed_type) +                      
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.title = element_text(),
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  ) +
  labs(
    title = "",
    x = "number of combined body parts",
    y = "number of neck neurons",
    fill = "effector super classes"
  ) +
  ggplot2::scale_fill_manual(values = c(motor=paper.cols[["motor"]],
                                        visceral_circulatory=paper.cols[["endocrine"]],
                                        `motor+visceral_circulatory`=paper.cols[["neck_connective"]]),
                             na.value = "lightgrey")

# Show
print(g.combo2)

# Save
ggsave(file.path(banc.fig2.supp.path, sprintf("%s_body_part_combination_super_classes_histogram.pdf",inf.metric)), 
       g.combo2, 
       width = 8, height = 4, dpi = 300)

# 1. Count by combination and seed_type
combo_freq <- effector_combinations %>%
  dplyr::filter(combination != "") %>%
  dplyr::count(combination, seed_type, name = "freq")

# 2. Find total counts per combination, pick top 
top_singles <- combo_freq %>%
  dplyr::filter(!grepl("\\+",combination)) %>%
  dplyr::group_by(combination) %>%
  dplyr::summarize(total = sum(freq), .groups = "drop") %>%
  dplyr::slice_max(order_by = total, n = 20) %>%
  dplyr::pull(combination)

# 3. Restrict to top combinations and set order
combo_top <- combo_freq %>%
  dplyr::filter(combination %in% top_singles) %>%
  dplyr::mutate(combination = factor(combination, levels = rev(top_singles)))

# 4. Make the dodged barplot
g.bp.unique.freq <- ggplot2::ggplot(combo_top, ggplot2::aes(x = combination, y = freq, fill = seed_type)) +
  ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8)) +
  ggplot2::coord_flip() +
  ggplot2::theme_minimal() +
  ggplot2::labs(
    x = "",
    y = "frequency",
    fill = "seed type",
    title = ""
  ) +
  ggplot2::theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.position = "none",
    axis.text.y = ggplot2::element_text(size = 7)
  ) +
  ylim(c(0,35)) +
  scale_fill_manual(values = paper.cols)

# Save
print(g.bp.unique.freq)
ggsave(file.path(banc.fig2.supp.path, sprintf("%s_body_part_unique_frequencies.pdf",inf.metric)), 
       g.bp.unique.freq, 
       width =  8, 
       height = 8, 
       dpi = 300)

# 2. Find total counts per combination, pick top 
top_combos <- combo_freq %>%
  dplyr::filter(grepl("\\+",combination)) %>%
  dplyr::group_by(combination) %>%
  dplyr::summarize(total = sum(freq), .groups = "drop") %>%
  dplyr::slice_max(order_by = total, n = length(top_singles)) %>%
  dplyr::pull(combination)

# 3. Restrict to top combinations and set order
combo_top <- combo_freq %>%
  dplyr::filter(combination %in% top_combos) %>%
  dplyr::mutate(combination = factor(combination, levels = rev(top_combos)))

# 4. Make the dodged barplot
g.bp.freq <- ggplot2::ggplot(combo_top, ggplot2::aes(x = combination, y = freq, fill = seed_type)) +
  ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8)) +
  ggplot2::coord_flip() +
  ggplot2::theme_minimal() +
  ggplot2::labs(
    x = "",
    y = "frequency",
    fill = "seed type",
    title = ""
  ) +
  ggplot2::theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.position = "none",
    axis.text.y = ggplot2::element_text(size = 7)
  ) +
  ylim(c(0,35)) +
  scale_fill_manual(values = paper.cols)

# Save
print(g.bp.freq)
ggsave(file.path(banc.fig2.supp.path, sprintf("%s_body_part_combination_frequencies.pdf",inf.metric)), 
       g.bp.freq, 
       width =  8, 
       height = 8, 
       dpi = 300)

# 1. For each seed_type, flag combinations as unique (singleton)
singletons <- effector_combinations %>%
  dplyr::filter(combination != "") %>%
  dplyr::group_by(seed_type, combination) %>%
  dplyr::summarise(ct = dplyr::n_distinct(seed_cell_type), .groups = "drop") %>%
  dplyr::mutate(is_singleton = ct == 1)

# 1. For each seed_type, flag combinations as unique (singleton)
singletons <- effector_combinations %>%
  dplyr::filter(combination != "") %>%
  dplyr::group_by(seed_type, combination) %>%
  dplyr::summarise(ct = dplyr::n_distinct(seed_cell_type), .groups = "drop") %>%
  dplyr::mutate(is_singleton = ct == 1)

# 2. For each seed_cell_type, flag if their combination is unique
singleton_flag <- effector_combinations %>%
  dplyr::filter(combination != "") %>%
  dplyr::left_join(
    singletons %>% dplyr::select(seed_type, combination, is_singleton), 
    by = c("seed_type", "combination")
  ) %>%
  dplyr::distinct(seed_type, seed_cell_type, is_singleton)

# 3. Count number of unique/shared per seed_type
count_df <- singleton_flag %>%
  dplyr::group_by(seed_type, is_singleton) %>%
  dplyr::summarise(n = n(), .groups = "drop")

# 4. Clean up labels for plot legend
count_df <- count_df %>%
  dplyr::mutate(singleton_label = dplyr::if_else(is_singleton, "unique", "shared"))

# 5. Plot raw counts as a stacked bar plot by seed_type
g.unique <- ggplot2::ggplot(count_df, ggplot2::aes(x = seed_type, y = n, fill = singleton_label)) +
  ggplot2::geom_col(position = "stack") +   # <--- STACKED!
  ggplot2::labs(
    x = "seed type",
    y = "count of seed_cell_type",
    fill = "combination status",
    title = ""
  ) +
  ggplot2::theme_minimal() +
  scale_fill_manual(values = c(shared = "lightgrey", unique = highlight.col))

# SHow
print(g.unique)

# Save
ggsave(file.path(banc.fig2.extra.path, sprintf("%s_body_part_unique_versus_shared_combinations.pdf",inf.metric)), 
       g.unique, 
       width = 4, 
       height = 4, 
       dpi = 300)


