##############################
## INFLUENCE METRIC VALIDATION ##
##############################
# Validate influence metrics against layer distance and cascade models
# Output: figures/figure_2/links/influence_validation.pdf

###################
## LOAD PACKAGES ##
###################

# Load required packages and data for influence validation
source("R/startup/banc-startup.R")
source("R/startup/franken-meta.R")
source("R/startup/banc-meta.R")
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
    stat_poly_eq(aes(color = NULL, 
                     label = paste(after_stat(eq.label), 
                                   after_stat(rr.label), sep = "~~~")),
                 formula = y ~ x, 
                 parse = TRUE, 
                 label.x = 0.75, 
                 label.y = 0.95, 
                 fontface = "bold",
                 size =5) +
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
           filename = file.path(banc.fig2.path, sprintf("%s_vs_layer_mean_super_class.png",inf.metric)), 
           width = 6, height = 3, dpi = 300)
    ggsave(g.olf2, 
           filename = file.path(banc.fig2.path, sprintf("%s_vs_cascade_mean.png",inf.metric)), 
           width = 6, height = 3, dpi = 300)
    ggsave(convert_to_dark_mode(g.olf1), 
           filename = file.path(banc.fig2.extra.path, sprintf("dark_mode_%s_vs_layer_mean_super_class.png",inf.metric)), 
           width = 6, height = 3, dpi = 300)
    ggsave(convert_to_dark_mode(g.olf2), 
           filename = file.path(banc.fig2.extra.path, sprintf("dark_mode_%s_vs_cascade_mean.png",inf.metric)), 
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

# Get alternative dataset for validation (seed_02)
con <- DBI::dbConnect(RSQLite::SQLite(),
                      file.path(banc.dropbox.influence.save.path,
                                influence.sqlite))
influence.02.orig <- dplyr::tbl(con, influence.table) %>%
  dplyr::filter(
    !is_seed,
    level=="seed_07",
    id %in% !!banc.meta$root_id
  ) %>%
  dplyr::collect() 
dbDisconnect(con)

# Apply direct connectivity
source("R/startup/banc-edgelist.R")
banc.direct.conns <- banc.edgelist.simple %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(pre_cell_type),
                pre_cell_type %in% !!influence.02.orig$seed) %>%
  dplyr::distinct(id=post, cell_type = pre_cell_type, norm, count) %>%
  dplyr::group_by(id, cell_type) %>%
  dplyr::summarise(count = sum(count,na.rm=TRUE),
                   norm = sum(norm,na.rm=TRUE))
thresh <- min(influence.02.orig$influence_norm_log)
influence.02.db <- influence.02.orig %>%
  dplyr::filter(influence_norm_log > thresh) %>%
  dplyr::left_join(banc.direct.conns,
                   by = c("id",
                          "seed"="cell_type")) %>%
  dplyr::mutate(count = ifelse(is.na(count),0,count),
                norm = ifelse(is.na(norm),0,norm))

# Create the binary connectivity variable
influence.02.db <- dplyr::mutate(
  influence.02.db,
  connectivity = ifelse(norm > 0.001, "connected", "not connected")
)

# Optionally, set as factor and control level order:
influence.02.db$connectivity <- factor(
  influence.02.db$connectivity,
  levels = c("not connected", "connected")
)

# Create the heatmap
inf.metrics <- inf.primary.metric
for (inf.metric in inf.metrics) {
  influence.02.db$influence_score <- influence.02.db[[inf.metric]]
  
  g.banc.check <- ggplot2::ggplot(
    influence.02.db,
    ggplot2::aes(x = influence_score, color = connectivity, group = connectivity)
  ) +
    ggplot2::geom_density(aes(y = after_stat(scaled)), size = 1.2, na.rm = TRUE) +
    ggplot2::scale_color_manual(
      values = c("not connected" = "lightgrey", "connected" = highlight.col),
      name = "Direct connectivity"
    ) +
    ggplot2::labs(
      x = inf.metric,
      y = "scaled density (max=1)",
      color = "direct connectivity"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "right",
      axis.title = ggplot2::element_text(size = 18),
      axis.text = ggplot2::element_text(size = 16),
      plot.title = ggplot2::element_text(hjust = 0.5, size = 14)
    ) +
    theme(legend.position = "none")
  
  # Show
  plot(g.banc.check)
  
  # Save
  if (inf.metric == inf.primary.metric) {
    ggsave(g.banc.check,
           filename = file.path(banc.fig2.path, sprintf("%s_vs_direct_connectivity.pdf", inf.metric)),
           width = 6, height = 2.5, dpi = 300)
    ggsave(convert_to_dark_mode(g.banc.check),
           filename = file.path(banc.fig2.extra.path, sprintf("dark_mode_%s_vs_direct_connectivity.pdf", inf.metric)),
           width = 6, height = 2.5, dpi = 300)
  } else {
    ggsave(g.banc.check,
           filename = file.path(banc.fig2.extra.path, sprintf("%s_vs_direct_connectivity.pdf", inf.metric)),
           width = 6, height = 2.5, dpi = 300)
  }
}





