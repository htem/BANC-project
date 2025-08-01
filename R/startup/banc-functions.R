##########################
## BANC ANALYSIS FUNCTIONS ##
##########################
# Core utility functions for processing BANC connectome data
# including influence calculations, visualisation helpers, and data transforms

calculate_influence_norms <- function(influence.df,
                                      const = -24,
                                      quantile = FALSE){
  if(!"target"%in%colnames(influence.df)){
    influence.df$target <- influence.df$id
    orig.target = FALSE
  }else{
    orig.target = TRUE
  }
  # check <- influence.df %>%
  #   dplyr::ungroup() %>%
  #   dplyr::mutate(no_seeds = influence_original/influence_norm_original,
  #                 no_seeds = ifelse(is.na(no_seeds),1,no_seeds),
  #                 no_synapses = influence_original/influence_syn_norm) %>%
  #   dplyr::group_by(target) %>%
  #   dplyr::mutate(no_targets = length(unique(id))) %>%
  #   dplyr::ungroup() %>%
  #   dplyr::mutate(
  #     no_seeds = as.numeric(no_seeds),
  #     no_targets = as.numeric(no_targets)
  #   ) %>%
  #   dplyr::group_by(seed) %>%
  #   dplyr::mutate(influence_per_seed = influence_original*no_seeds,
  #                 influence_per_synapse = influence_original*no_synapses) %>%
  #   dplyr::ungroup() %>%
  #   dplyr::distinct(seed, seed_original, target, no_seeds, no_targets, .keep_all = TRUE) %>%
  #   dplyr::arrange(seed, seed_original, target) %>%
  #   as.data.frame()
  inf.threshold <- exp(const)
  if(!"influence_syn_norm"%in%colnames(influence.df)){
    influence.df$influence_syn_norm <- 1
  }
  if(!"influence_syn_norm_original"%in%colnames(influence.df)){
    influence.df$influence_syn_norm <- 1
  }
  if(!"influence_original"%in%colnames(influence.df)){
    influence.df$influence_original <- influence.df$influence
  }
  if(!"influence_norm_original"%in%colnames(influence.df)){
    influence.df$influence_norm_original <- influence.df$influence_norm
  }
  influence.df <- influence.df %>%
    dplyr::ungroup() %>%
    dplyr::mutate(no_seeds = influence_original/influence_norm_original,
                  no_seeds = ifelse(is.na(no_seeds),1,no_seeds),
                  no_synapses = influence_original/influence_syn_norm) %>%
    dplyr::group_by(target) %>%
    dplyr::mutate(no_targets = length(unique(id))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      no_seeds = as.numeric(no_seeds),
      no_targets = as.numeric(no_targets)
    ) %>%
    dplyr::group_by(seed) %>%
    dplyr::mutate(influence_per_seed = influence_original*no_seeds,
                  influence_per_synapse = influence_original*no_synapses) %>%
    dplyr::group_by(target, seed) %>%
    dplyr::mutate(influence = sum(influence_original,na.rm = TRUE),
                  total_seeds = sum(no_seeds,na.rm=TRUE),
                  total_synapses = sum(no_synapses,na.rm=TRUE),
                  influence_norm = sum(influence_per_seed,na.rm = TRUE)/(total_seeds*no_targets),
                  influence = ifelse(influence<inf.threshold,inf.threshold,influence),
                  influence_norm = ifelse(influence_norm<inf.threshold,inf.threshold,influence_norm),
                  influence_norm = sum(influence_norm,na.rm = TRUE),
                  influence_syn_norm =  sum(influence_per_synapse,na.rm = TRUE)/(no_targets*total_synapses),
                  influence_syn_norm = ifelse(influence_syn_norm<inf.threshold,inf.threshold,influence_syn_norm),
                  influence_syn_norm = sum(influence_syn_norm,na.rm = TRUE),
                  influence_norm_log = log(influence_norm),
                  influence_log = log((influence/no_targets)),
                  influence_syn_norm_log = log(influence_syn_norm)) %>%
    { 
      if (!is.null(quantile)) {
        dplyr::mutate(.,
                      influence_quantile = stats::quantile(influence_original, quantile, na.rm = TRUE),
                      influence_quantile = signif(influence_quantile, 4)
        )
      } else {
        .
      }
    } %>%
    dplyr::ungroup() %>%
    dplyr::mutate(influence_norm_log = influence_norm_log-const,
                  influence_log = influence_log-const,
                  influence_syn_norm_log = influence_syn_norm_log-const) %>%
    dplyr::group_by(seed) %>%
    dplyr::mutate(influence_log = ifelse(is.na(influence),0,influence_log)) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(target, 
                    seed, 
                    .keep_all = TRUE) %>%
    dplyr::group_by(target) %>%
    dplyr::mutate(influence_norm_log_minmax = (influence_norm_log-min(influence_norm_log,na.rm=TRUE))/(max(influence_norm_log,na.rm=TRUE)-min(influence_norm_log,na.rm=TRUE)),
                  influence_log_minmax = (influence_log-min(influence_log,na.rm=TRUE))/(max(influence_log,na.rm=TRUE)-min(influence_log,na.rm=TRUE)),
                  influence_syn_norm_log_minmax = (influence_syn_norm_log-min(influence_syn_norm_log,na.rm=TRUE))/(max(influence_syn_norm_log,na.rm=TRUE)-min(influence_syn_norm_log,na.rm=TRUE))) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(seed) %>%
    dplyr::mutate(influence_norm_log_minmax_seed = (influence_norm_log-min(influence_norm_log,na.rm=TRUE))/(max(influence_norm_log,na.rm=TRUE)-min(influence_norm_log,na.rm=TRUE)),
                  influence_log_minmax_seed = (influence_log-min(influence_log,na.rm=TRUE))/(max(influence_log,na.rm=TRUE)-min(influence_log,na.rm=TRUE)),
                  influence_syn_norm_log_minmax_seed = (influence_syn_norm_log-min(influence_syn_norm_log,na.rm=TRUE))/(max(influence_syn_norm_log,na.rm=TRUE)-min(influence_syn_norm_log,na.rm=TRUE))) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(target,
                    seed, 
                    .keep_all = TRUE) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(influence = signif(influence,4),
                  influence_log = signif(influence_log,4),
                  influence_norm = signif(influence_norm,4),
                  influence_syn_norm = signif(influence_syn_norm,4),
                  influence_norm_log = signif(influence_norm_log,4),
                  influence_syn_norm_log = signif(influence_syn_norm_log,4),
                  influence_log_minmax = signif(influence_log_minmax,4),
                  influence_norm_log_minmax = signif(influence_norm_log_minmax,4),
                  influence_syn_norm_log_minmax = signif(influence_syn_norm_log_minmax,4),
                  influence_log_minmax_seed  = signif(influence_log_minmax_seed,4),
                  influence_norm_log_minmax_seed  = signif(influence_norm_log_minmax_seed,4),
                  influence_syn_norm_log_minmax_seed  = signif(influence_syn_norm_log_minmax_seed,4)
    ) 
  if(!orig.target){
    influence.df$target <- NULL
  }
  influence.df
}

extract_three_letters <- function(text) {
  sapply(text, function(t) {
    three_letters <- stringr::str_extract(t, "^[A-Za-z]{3}")
    if (!is.na(three_letters)) {
      return(three_letters)
    }
    two_letters <- stringr::str_extract(t, "^[A-Za-z]{2}")
    if (!is.na(two_letters)) {
      return(two_letters)
    }
    one_letter <- stringr::str_extract(t, "^[A-Za-z]{1}")
    return(one_letter)
  })
}

# Function to calculate cosine similarity for sparse matrices
cosine_similarity_sparse <- function(mat) {
  # Calculate the norm of each column
  col_norms <- sqrt(colSums(mat^2))
  
  # Normalize the matrix
  mat_normalized <- mat %*% Diagonal(x = 1 / col_norms)
  
  # Calculate cosine similarity
  sim <- t(mat_normalized) %*% mat_normalized
  
  return(as.matrix(sim))
}

# A dorsal view of the BANC brain
dorsal <- structure(c(0.997957646846771, -0.0199870802462101, 
                      0.0606706738471985, 0, 0.055451937019825, 0.742548227310181, 
                      -0.667493462562561, 0, -0.0317096672952175, 0.66949450969696, 
                      0.742140114307404, 0, 0, 0, 0, 1), dim = c(4L, 4L))

# Help merge
.merge_hclust <- function(hclist) {
  #-- Merge
  d <- as.dendrogram(hclist[[1]])
  for (i in 2:length(hclist)) {
    d <- merge(d, as.dendrogram(hclist[[i]]))
  }
  as.hclust(d)
}

# Define the hclust_semisupervised and .merge_hclust functions
hclust_semisupervised <- function(data, groups, 
                                  dist_method = "cosine",
                                  dist_p = 2, 
                                  hclust_method = "ward.D2") {
  hclist <- lapply(groups, function (group) {
    if(dist_method=="cosine"){
      datag <- data[match(group,rownames(data)),]
      cosine_sim_matrix <- cosine_similarity_sparse(t(datag))
      colnames(cosine_sim_matrix) <- rownames(cosine_sim_matrix) <- rownames(datag)
      cosine_sim_matrix[is.na(cosine_sim_matrix)] <- 0
      hclust(as.dist(1 - cosine_sim_matrix), 
             method = hclust_method)
    }else{
      hclust(dist(data[match(group,rownames(data)),], 
                  method = dist_method, 
                  p = dist_p), 
             method = hclust_method) 
    }
  })
  hc <- .merge_hclust(hclist)
  data_reordered <- data[match(unlist(groups),rownames(data)),]
  return(list(data = data_reordered, hclust = hc))
}

adjust_color_brightness <- function(color, factor) {
  col <- col2rgb(color)
  col <- pmin(pmax(col * factor, 0), 255)  
  col <- rgb(t(col), maxColorValue = 255)
  return(col)
}

#' Convert a ggplot object to dark mode while preserving original formatting
#'
#' @param plot A ggplot2 object to convert to dark mode
#' @param bg_color Background color for the plot
#' @param text_color Text color for labels, titles, etc.
#' @param grid_color Color for grid lines
#' @param line_color Color for trend lines that were previously black
#' @param preserve_colors Logical; whether to preserve original point/line colors
#'        or adjust them for better visibility on dark background
#' @param brighten_factor Factor by which to brighten colors if preserve_colors=FALSE
#'
#' @return A ggplot2 object with dark mode theme applied while preserving original formatting
#'
#' Convert a ggplot object to dark mode with transparent background
#'
#' @param plot A ggplot2 object to convert to dark mode
#' @param text_color Text color for labels, titles, etc.
#' @param grid_color Color for grid lines
#' @param line_color Color for trend lines that were previously black
#' @param preserve_colors Logical; whether to preserve original point/line colors
#'        or adjust them for better visibility on dark background
#' @param brighten_factor Factor by which to brighten colors if preserve_colors=FALSE
#' @param panel_bg_alpha Alpha transparency for panel background (0=fully transparent)
#'
#' @return A ggplot2 object with dark mode theme applied with transparent background
#'
convert_to_dark_mode <- function(plot, 
                                 text_color = "#FFFFFF",
                                 grid_color = "#555555",
                                 line_color = "#FFFFFF",
                                 preserve_colors = TRUE,
                                 brighten_factor = 1.3,
                                 panel_bg_alpha = 0.5) {
  
  library(ggplot2)
  
  # Function to brighten colors if needed
  brighten_color <- function(color, factor = brighten_factor) {
    # Convert to RGB
    rgb_vals <- col2rgb(color) / 255
    
    # Brighten RGB values
    rgb_vals <- pmin(rgb_vals * factor, 1)
    
    # Convert back to hex
    hex <- rgb(rgb_vals[1], rgb_vals[2], rgb_vals[3])
    return(hex)
  }
  
  # Extract current colors if we need to adjust them
  if (!preserve_colors && !is.null(plot$scales$get_scales("colour"))) {
    # Try to get the scale
    color_scale <- plot$scales$get_scales("colour")
    
    # If it's a discrete scale with a palette
    if (inherits(color_scale, "ScaleDiscrete") && !is.null(color_scale$palette)) {
      # Get number of levels
      n_colors <- length(unique(ggplot2::ggplot_build(plot)$data[[1]]$colour))
      if (n_colors > 0) {
        # Get current colors
        current_colors <- color_scale$palette(n_colors)
        
        # Brighten existing colors
        brightened_colors <- sapply(current_colors, brighten_color)
        
        # Replace with brightened colors
        plot <- plot + scale_color_manual(values = brightened_colors)
      }
    }
  }
  
  # Find any black line color specifications in geom_smooth and change to white
  for (i in seq_along(plot$layers)) {
    if ("GeomSmooth" %in% class(plot$layers[[i]]$geom)) {
      if (!is.null(plot$layers[[i]]$aes_params$colour) && 
          plot$layers[[i]]$aes_params$colour == "black") {
        plot$layers[[i]]$aes_params$colour <- line_color
      }
    }
    
    # Also check for black points, lines or text
    if (any(c("GeomPoint", "GeomLine", "GeomText", "GeomLabel") %in% class(plot$layers[[i]]$geom))) {
      if (!is.null(plot$layers[[i]]$aes_params$colour) && 
          plot$layers[[i]]$aes_params$colour == "black") {
        plot$layers[[i]]$aes_params$colour <- line_color
      }
    }
  }
  
  # Create a semi-transparent panel background (dark with transparency)
  panel_bg_color <- rgb(0.1, 0.1, 0.1, panel_bg_alpha)  # Dark gray with transparency
  
  # Create a new theme with transparent backgrounds
  dark_theme <- theme(
    # Full transparency for plot background
    plot.background = element_rect(fill = "transparent", color = NA),
    
    # Semi-transparent or transparent panel background
    panel.background = element_rect(fill = panel_bg_color, color = NA),
    
    # Text colors only (preserve sizes, angles, etc.)
    axis.title = element_text(color = text_color),
    axis.text = element_text(color = text_color),
    plot.title = element_text(color = text_color),
    plot.subtitle = element_text(color = text_color),
    legend.text = element_text(color = text_color),
    legend.title = element_text(color = text_color),
    strip.text = element_text(color = text_color),
    
    # Grid lines
    panel.grid.major = element_line(color = grid_color),
    panel.grid.minor = element_line(color = grid_color),
    
    # Facet backgrounds - transparent
    strip.background = element_rect(fill = "transparent", color = NA),
    
    # Legend - transparent background
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.key = element_rect(fill = "transparent", color = NA),
    
    # Make sure plot area is transparent
    panel.border = element_rect(color = NA, fill = NA)
  )
  
  # Apply the dark theme
  dark_plot <- plot + dark_theme
  
  return(dark_plot)
}

banc_interpret_umaps <- function(
    umap.df,  
    influence.df,
    elist.pre = NULL,
    elist.post = NULL,
    identifier,
    umaps = TRUE,
    neuroanatomy = TRUE,
    neuroanatomy.xyz = FALSE,
    anatomy.group = "cluster",
    banc.meta  = NULL,
    save.path = NULL,
    cluster.colors = NULL,
    inf.metric = "influence_log",
    cluster.centroids = FALSE,
    seed.map = NULL,
    target.map = NULL,
    recalculate = FALSE,
    width = 10, 
    height = 10,
    scaled_heatmap_palette = NULL,
    scaled_heatmap_breaks = NULL
){
  
  # create save folder
  dir.create(file.path(save.path,identifier), recursive = TRUE, showWarnings = FALSE)
  
  # Calculate cluster centroids
  if(cluster.centroids){
    cluster_centroids <- umap.df %>%
      group_by(cluster) %>%
      summarise(UMAP1 = mean(UMAP1, na.rm = TRUE),
                UMAP2 = mean(UMAP2, na.rm = TRUE)) 
  }else{
    cluster_centroids <- data.frame()
  }
  
  # Create a function to generate n colors
  cerise_limon_base <- c("#EE5B32", "#F6B83C", "#4BA747", "#5BB6E4", "#7C378A")
  cerise_limon_palette <- grDevices::colorRampPalette(cerise_limon_base)
  
  ##############################
  ### WHAT ARE OUR CLUSTERS? ###
  ##############################
  if(umaps){
    
    # Iterate over influence
    # Rename seeds
    if(!is.null(names(seed.map))){
      influence.df <- influence.df %>%
        dplyr::mutate(seed = case_when(
          seed %in% names(seed.map) ~ seed.map[seed],
          TRUE ~ seed
        )) %>%
        dplyr::filter(seed %in% unname(seed.map))
    }
    if(!is.null(names(target.map))){
      influence.df <- influence.df %>%
        dplyr::mutate(target = case_when(
          target %in% names(target.map) ~ target.map[target],
          TRUE ~ target
        )) %>%
        dplyr::filter(target %in% unname(target.map))
    }
    
      # normalisations
      if(recalculate){
        influence.df <- calculate_influence_norms(influence.df)
      }
      entries <- na.omit(unique(influence.df$seed))
      if(all(is.na(influence.df[[inf.metric]]))){
        next
      }
      thresh.high <- quantile(na.omit(influence.df[[inf.metric]]),0.95)
      thresh.low <- quantile(na.omit(influence.df[[inf.metric]]),0.05)
      for(entry in entries){
        message("Working on influence seed: ", entry)
        inf.entry <- influence.df %>%
          dplyr::filter(seed==entry)
        if(max(inf.entry[[inf.metric]],na.rm=TRUE)==0){
          message("no data for entry: ", entry)
          next
        }
        if("id"%in%colnames(umap.df)){
          umap.df.entry <- dplyr::left_join(umap.df,
                                            inf.entry[,c("id",inf.metric)] %>%
                                              dplyr::distinct(),
                                            by = "id") 
        }else{
          umap.df.entry <- dplyr::left_join(umap.df,
                                            inf.entry[,c("cell_type",inf.metric)] %>%
                                              dplyr::distinct(),
                                            by = "cell_type")
        }
        umap.df.entry$norm <- umap.df.entry[[inf.metric]]
        if(all(is.na(umap.df.entry[[inf.metric]]))){
          next
        }
        umap.df.entry <- umap.df.entry %>%
          dplyr::filter(!is.na(norm)) %>%
          dplyr::arrange(norm) 
        umap.df.entry$norm <- ifelse(umap.df.entry$norm>thresh.high,thresh.high,umap.df.entry$norm)
        umap.df.entry$norm <- ifelse(umap.df.entry$norm<thresh.low,thresh.low,umap.df.entry$norm)
        
        # scale colors
        n_breaks <- 100
        if(is.null(scaled_heatmap_palette)){
          scaled_heatmap_palette <- grDevices::colorRampPalette(c("#1f4e79", "#4a90a4", "#7ba7bc", "#a67c8a", "#c4967d", "#b22222"))(n_breaks - 1)
        }
        if(is.null(scaled_heatmap_breaks)){
          scaled_heatmap_breaks <- seq(
            stats::quantile(influence.df[[inf.metric]], 0, na.rm = TRUE), 
            stats::quantile(influence.df[[inf.metric]], 1, na.rm = TRUE), 
            length.out = n_breaks)
          thresh.high <- max(scaled_heatmap_breaks,na.rm=TRUE)
          thresh.low <- min(scaled_heatmap_breaks,na.rm=TRUE)
          umap.df.entry$norm <- ifelse(umap.df.entry$norm>thresh.high,thresh.high,umap.df.entry$norm)
          umap.df.entry$norm <- ifelse(umap.df.entry$norm<thresh.low,thresh.low,umap.df.entry$norm)
        }
        
        # Make plot
        if(is.null(umap.df.entry$super_class)){
          p_entry <- ggplot(umap.df.entry, aes(x = UMAP1, 
                                               y = UMAP2, 
                                               color = norm)) +
            #geom_density_2d(aes(group = 1), col="grey70", alpha = 0.5) +
            geom_point(data = subset(umap.df.entry, is.na(norm)), 
                       alpha = 0.9, 
                       size = 3, 
                       col = "grey30") +
            geom_point(data = subset(umap.df.entry, !is.na(norm)), 
                       alpha = 0.9, 
                       size = 3) +
            scale_color_gradientn(colours = scaled_heatmap_palette,
                                  values = scales::rescale(scaled_heatmap_breaks),
                                  limits = c(thresh.low, thresh.high),
                                  na.value = "grey30") +
            theme_void() +
            labs(title = "",
                 x = "UMAP1",
                 y = "UMAP2") +
            theme(
              legend.position = "bottom",
              legend.text = element_text(size = 6),
              legend.title = element_text(size = 8),  
              legend.key.size = unit(0.5, "cm") 
            ) +
            labs(color = paste0(entry,": norm")) +
            ggplot2::coord_fixed()
          
          if(nrow(cluster_centroids)){
            p_entry <- p_entry + 
              geom_text(data = cluster_centroids,
                        aes(label = cluster),
                        colour = "black",
                        size = 4,
                        fontface = "bold")
          }
        }else{
          p_entry <- ggplot(umap.df.entry, aes(x = UMAP1, 
                                               y = UMAP2, 
                                               color = norm, 
                                               shape = super_class)) +
            #geom_density_2d(aes(group = 1), col="grey70", alpha = 0.5) +
            scale_shape_manual(values = c("ascending"=17,"descending"=19,"motor"=19,"visceral_circulatory"=17,"sensory"=19),
                               guide = guide_legend(title = "points:")) +
            geom_point(data = subset(umap.df.entry, is.na(norm)), 
                       alpha = 0.9, 
                       size = 3, 
                       col = "grey30") +
            geom_point(data = subset(umap.df.entry, !is.na(norm)), 
                       alpha = 0.9, 
                       size = 3) +
            scale_color_gradientn(colours = scaled_heatmap_palette,
                                  values = scales::rescale(scaled_heatmap_breaks),
                                  limits = c(thresh.low, thresh.high),
                                  na.value = "grey30",
                                  guide = guide_legend(title = "points:")) +
            theme_void() +
            labs(title = "",
                 x = "UMAP1",
                 y = "UMAP2") +
            theme(
              legend.position = "bottom",
              legend.text = element_text(size = 6),
              legend.title = element_text(size = 8),  
              legend.key.size = unit(0.5, "cm") 
            ) +
            labs(color = paste0(entry,": norm")) +
            guides(shape = "none") +
            ggplot2::coord_fixed()
          
          if(nrow(cluster_centroids)){
            p_entry <- p_entry + 
              geom_text(data = cluster_centroids,
                        aes(label = cluster),
                        colour = "black",
                        size = 4,
                        fontface = "bold")
          }
        }
        if(!is.null(umap.df.entry$super_class)&all("ascendng","descending")%in%umap.df.entry$super_class){
          p_entry <- p_entry +
            scale_shape_manual(values = c("ascending"=17,"descending"=19),
                             guide = guide_legend(title = "cell types:")) +
            guides(shape = "none")
        }
        
        # Save
        dir.create(file.path(save.path,identifier), recursive = TRUE, showWarnings = FALSE)
        ggsave(plot = p_entry,
               filename = file.path(save.path,identifier,paste0(inf.metric,"_",gsub("_|m ","",entry),"_umap.png")),
               width = width, height = height, dpi = 300)
    }
    
    # Iterate over connectivity features, looking at direct outputs to postsynaptic targets
    if(!is.null(elist.pre)){
      cols <- c("post_super_class","post_cell_function", "post_nerve",
                "post_cell_class", "post_cell_sub_class",
                "post_body_part_effector", "post_body_part_sensory", "post_origin")
      cols <- intersect(cols,colnames(elist.pre))
      for(col in cols){
        message("Working on outputs from: ", col)
        elist.pre.select <- elist.pre
        entries <- na.omit(unique(elist.pre.select[[col]]))
        entries <- entries[!grepl(">|/",entries)]
        if(col=="post_origin"){
          entries <- c(entries,"Leg|leg","Wing|wing", "neck|notum", "IntTct","ANm|abdomen")
        }
        for(entry in entries){
          if(col=="pre_origin"){
            dn.elist.entry <- elist.post[!is.na(elist.post[[col]]),]
            dn.elist.entry <- dn.elist.entry[grepl(entry,dn.elist.entry[[col]]),]
          }else{
            dn.elist.entry <- elist.pre[!is.na(elist.pre[[col]]),]
            dn.elist.entry <- dn.elist.entry[dn.elist.entry[[col]]==entry,] 
          }
          if(max(dn.elist.entry$norm,na.rm=TRUE)<0.005|nrow(dn.elist.entry)<25){
            next
          }
          umap.df.entry <- dplyr::left_join(umap.df,
                                            dn.elist.entry %>%
                                              dplyr::select(pre, post, norm) %>%
                                              dplyr::mutate(norm = ifelse(norm>0.02,0.02,norm)),
                                            by = c("id"="pre")) %>%
            dplyr::arrange(dplyr::desc(norm))
          
          # Make plot
          p_entry <- ggplot(umap.df.entry, aes(x = UMAP1, y = UMAP2, color = norm)) +
            #geom_density_2d(col="grey70", alpha = 0.5) +
            geom_point(data = subset(umap.df.entry, is.na(norm)), alpha = 0.9, size = 2, col = "grey30") +
            geom_point(data = subset(umap.df.entry, norm==0), alpha = 0.9, size = 2, col = "grey30") +
            geom_point(data = subset(umap.df.entry, !is.na(norm)&norm!=0), alpha = 0.9, size = 2) +
            scale_color_gradientn(colours = rev(cerise_limon_palette(100)),
                                  #values = scales::rescale(connection_heatmap_breaks),
                                  limits = c(0, 0.02),
                                  na.value = "grey30") +
            theme_void() +
            labs(title = "",
                 x = "UMAP1",
                 y = "UMAP2") +
            theme(
              legend.position = "bottom",
              legend.text = element_text(size = 5),  # Adjust this value to change label size
              legend.title = element_text(size = 8),  # Adjust this value to change title size
              legend.key.size = unit(0.5, "cm")  # Adjust this value to change the size of the color bar
            ) +
            geom_text(data = cluster_centroids,
                      aes(label = cluster),
                      colour = "grey70",
                      size = 4,
                      fontface = "bold") +
            ggplot2::coord_fixed()
          
          # Save
          dir.create(file.path(save.path,identifier,col), recursive = TRUE, showWarnings = FALSE)
          ggsave(plot = p_entry,
                 filename = file.path(save.path,identifier,col,paste0(col,"_",entry,"_umap.png")),
                 width = 10, height = 10, dpi = 300)
        }
      }
      
    }
    
    # Iterate over connectivity features, looking at direct inputs from presynaptic targets
    if(!is.null(elist.post)){
      cols <- c("pre_super_class","pre_cell_function", "pre_nerve",
                "pre_cell_class", "pre_cell_sub_class",
                "pre_body_part_effector", "pre_body_part_sensory", "pre_origin")
      cols <- intersect(cols,colnames(elist.post))
      for(col in cols){
        message("Working on inputs from: ", col)
        elist.post.select <- elist.post
        entries <- na.omit(unique(elist.post.select[[col]]))
        entries <- entries[!grepl(">|/",entries)]
        if(col=="pre_origin"){
          entries <- c(entries,"Leg|leg","Wing|wing", "neck|notum", "IntTct","ANm|abdomen")
        }
        for(entry in entries){
          if(col=="pre_origin"){
            dn.elist.entry <- elist.post[!is.na(elist.post[[col]]),]
            dn.elist.entry <- dn.elist.entry[grepl(entry,dn.elist.entry[[col]]),]
          }else{
            dn.elist.entry <- elist.post[!is.na(elist.post[[col]]),]
            dn.elist.entry <- dn.elist.entry[dn.elist.entry[[col]]==entry,]
          }
          if(max(dn.elist.entry$norm,na.rm=TRUE)<0.005|nrow(dn.elist.entry)<25){
            next
          }
          umap.df.entry <- dplyr::left_join(umap.df,
                                            dn.elist.entry %>%
                                              dplyr::select(pre, post, norm) %>%
                                              dplyr::mutate(norm = ifelse(norm>0.02,0.02,norm)),
                                            by = c("id"="post")) %>%
            dplyr::arrange(dplyr::desc(norm))
          
          # Make plot
          p_entry <- ggplot(umap.df.entry, aes(x = UMAP1, y = UMAP2, color = norm)) +
            #geom_density_2d(col="grey70", alpha = 0.5) +
            geom_point(data = subset(umap.df.entry, is.na(norm)), alpha = 0.9, size = 2, col = "grey30") +
            geom_point(data = subset(umap.df.entry, norm==0), alpha = 0.9, size = 2, col = "grey30") +
            geom_point(data = subset(umap.df.entry, !is.na(norm)&norm!=0), alpha = 0.9, size = 2) +
            scale_color_gradientn(colours = rev(cerise_limon_palette(100)),
                                  #values = scales::rescale(connection_heatmap_breaks),
                                  limits = c(0, 0.02),
                                  na.value = "grey30") +
            theme_void() +
            labs(title = "",
                 x = "UMAP1",
                 y = "UMAP2") +
            theme(
              legend.position = "bottom",
              legend.text = element_text(size = 5),  # Adjust this value to change label size
              legend.title = element_text(size = 8),  # Adjust this value to change title size
              legend.key.size = unit(0.5, "cm")  # Adjust this value to change the size of the color bar
            ) +
            geom_text(data = cluster_centroids,
                      aes(label = cluster),
                      colour = "grey70",
                      size = 4,
                      fontface = "bold") +
            ggplot2::coord_fixed()
          
          # Save
          dir.create(file.path(save.path,identifier,col), recursive = TRUE, showWarnings = FALSE)
          ggsave(plot = p_entry,
                 filename = file.path(save.path,identifier,col, paste0(col,"_",entry,"_umap.png")),
                 width = 10, height = 10, dpi = 300)
        }
      } 
    }
  }
  
  ###############################
  ### VISUALISE OUR CLUSTERS? ###
  ###############################
  
  if(neuroanatomy){
    # plot DN UMAP clusters neuropils
    g.anat <- ggplot2::ggplot() +
      ggplot2::coord_fixed() +
      ggplot2::theme_void() +
      ggplot2::guides(fill = "none", color = "none") +
      ggplot2::theme(legend.position = "none",
                     plot.title = ggplot2::element_text(hjust = 0, size = 8,
                                                        face = "bold",
                                                        colour = "black"),
                     axis.title.x = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_blank(),
                     axis.text.y = ggplot2::element_blank(),
                     axis.ticks.y = ggplot2::element_blank(),
                     axis.line = ggplot2::element_blank(),
                     panel.grid.major = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank(),
                     plot.margin = ggplot2::margin(0, 0, 0, 0),
                     panel.spacing = ggplot2::unit(0, "cm"),
                     panel.border = ggplot2::element_blank(),
                     panel.background = ggplot2::element_blank(),
                     plot.background = ggplot2::element_blank()) +
      ggplot2::labs(title = '')
    
    # Plot over clusters
    for(clust in sort(unique(umap.df[[anatomy.group]]))){
      try({
        message("Working on neuroanatomy for: ",clust)
        
        # Get neuron data
        neuron.meta <- umap.df[umap.df[[anatomy.group]]==clust,]
        neuron.meta <- neuron.meta %>%
          dplyr::arrange(cell_type)
        cts <- na.omit(unique(neuron.meta$cell_type))
        banc.meta.cluster <- banc.meta %>%
          dplyr::filter(cell_type %in% cts | fafb_cell_type %in% cts | manc_cell_type %in% cts) %>%
          dplyr::arrange(cell_type)
        if(!nrow(banc.meta)){
          next
        }
        neuron.ids <- unique(banc.meta.cluster$root_id)
        neuron.ids <- na.omit(neuron.ids)
        plot.neurons <- banc_read_l2skel(neuron.ids, OmitFailures = TRUE)
        if(!length(plot.neurons)){
          next
        }
        if(neuroanatomy.xyz){
          plot.neurons <- nat::xyzmatrix(plot.neurons)
        }else{
          plot.neurons <- plot.neurons[sample(1:length(plot.neurons),min(300,length(plot.neurons)))]
          plot.neurons <-bancr:::banc_reroot(plot.neurons, roots = banc.meta.cluster)
        }
        
        # MAIN
        g.dn.main <- g.anat +
          geom_neuron(x = banc_neuropil.surf,
                      cols = c("grey60", "grey30"),
                      rotation_matrix = bancr:::banc_rotation_matrices[["main"]],
                      alpha = 0.1) +
          geom_neuron(x = plot.neurons,
                      root = TRUE,
                      cols = cerise_limon_palette(length(plot.neurons)),
                      #cols = c(adjust_color_brightness(cluster.colors[[clust]], 0.3),adjust_color_brightness(cluster.colors[[clust]], 1.7)),
                      rotation_matrix = bancr:::banc_rotation_matrices[["main"]],
                      alpha = 0.3)
        dir.create(file.path(save.path,identifier,"neuroanatomy"), recursive = TRUE, showWarnings = FALSE)
        ggsave(plot = g.dn.main,
               filename = file.path(save.path, identifier,"neuroanatomy",paste0(clust,"_neuroanatomy_main.png")),
               width = 10, height = 10, dpi = 72)
        
        # VNC
        plot.neurons.vnc <- banc_decapitate(plot.neurons, invert = FALSE, OmitFailures = TRUE)
        if(length(plot.neurons.vnc)){
          plot.neurons.vnc <- plot.neurons.vnc[unlist(lapply(plot.neurons.vnc,function(x) !is.null(x)))]
          plot.neurons.vnc <-bancr:::banc_reroot(plot.neurons.vnc, roots = banc.meta.cluster, OmitFailures = TRUE)
          if(!length(plot.neurons.vnc)){
            next
          }
          g.dn.vnc <- g.anat +
            geom_neuron(x = banc_vnc_neuropil.surf,
                        cols = c("grey60", "grey30"),
                        rotation_matrix = bancr:::banc_rotation_matrices[["vnc"]],
                        alpha = 0.1) +
            geom_neuron(x = plot.neurons.vnc,
                        root = TRUE,
                        cols = cerise_limon_palette(length(plot.neurons.vnc)),
                        #cols = c(adjust_color_brightness(cluster.colors[[clust]], 0.3),adjust_color_brightness(cluster.colors[[clust]], 1.7)),
                        rotation_matrix = bancr:::banc_rotation_matrices[["vnc"]],
                        alpha = 0.3)
          g.dn.vnc.side <- g.anat +
            geom_neuron(x = banc_vnc_neuropil.surf,
                        root = TRUE,
                        cols = c("grey60", "grey30"),
                        rotation_matrix = bancr:::banc_rotation_matrices[["vnc_side"]],
                        alpha = 0.1) +
            geom_neuron(x = plot.neurons.vnc,
                        root = TRUE,
                        cols = cerise_limon_palette(length(plot.neurons.vnc)),
                        #cols = c(adjust_color_brightness(cluster.colors[[clust]], 0.3),adjust_color_brightness(cluster.colors[[clust]], 1.7)),
                        rotation_matrix = bancr:::banc_rotation_matrices[["vnc_side"]],
                        alpha = 0.3)
          # Save
          ggsave(plot = g.dn.vnc,
                 filename = file.path(save.path,identifier,"neuroanatomy",paste0(clust,"_neuroanatomy_vnc.png")),
                 width = 10, height = 10, dpi = 72)
          ggsave(plot = g.dn.vnc.side,
                 filename = file.path(save.path,identifier,"neuroanatomy",paste0(clust,"_neuroanatomy_vnc_side.png")),
                 width = 10, height = 10, dpi = 72)
        } 
        
        # BRAIN
        plot.neurons.brain <- banc_decapitate(plot.neurons, invert = TRUE, OmitFailures = TRUE)
        if(length(plot.neurons.brain)){
          plot.neurons.brain <- plot.neurons.brain[unlist(lapply(plot.neurons.brain,function(x) !is.null(x)))]
          plot.neurons.brain <- bancr:::banc_reroot(plot.neurons.brain, roots = banc.meta.cluster, OmitFailures = TRUE)
          if(!length(plot.neurons.brain)){
            next
          }
          g.dn.brain <- g.anat +
            geom_neuron(x = banc_brain_neuropil.surf,
                        cols = c("grey60", "grey30"),
                        rotation_matrix = bancr:::banc_rotation_matrices[["front"]],
                        alpha = 0.1) +
            geom_neuron(x = plot.neurons.brain,
                        root = TRUE,
                        cols = cerise_limon_palette(length(plot.neurons.brain)),
                        #cols = c(adjust_color_brightness(cluster.colors[[clust]], 0.3),adjust_color_brightness(cluster.colors[[clust]], 1.7)),
                        rotation_matrix = bancr:::banc_rotation_matrices[["front"]],
                        alpha = 0.3)
          g.dn.brain.side <- g.anat +
            geom_neuron(x = banc_brain_neuropil.surf,
                        cols = c("grey60", "grey30"),
                        rotation_matrix = bancr:::banc_rotation_matrices[["brain_side"]],
                        alpha = 0.1) +
            geom_neuron(x = plot.neurons.brain,
                        root = TRUE,
                        cols = cerise_limon_palette(length(plot.neurons.brain)),
                        #cols = c(adjust_color_brightness(cluster.colors[[clust]], 0.3),adjust_color_brightness(cluster.colors[[clust]], 1.7)),
                        rotation_matrix = bancr:::banc_rotation_matrices[["brain_side"]],
                        alpha = 0.3)
          g.dn.brain.dorsal <- g.anat +
            geom_neuron(x = banc_brain_neuropil.surf,
                        cols = c("grey60", "grey30"),
                        rotation_matrix = dorsal,
                        alpha = 0.1) +
            geom_neuron(x = plot.neurons.brain,
                        root = TRUE,
                        cols = cerise_limon_palette(length(plot.neurons.brain)),
                        #cols = c(adjust_color_brightness(cluster.colors[[clust]], 0.3),adjust_color_brightness(cluster.colors[[clust]], 1.7)),
                        rotation_matrix = dorsal,
                        alpha = 0.3)
          ggsave(plot = g.dn.brain,
                 filename = file.path(save.path,identifier,"neuroanatomy",paste0(clust,"_neuroanatomy_brain.png")),
                 width = 10, height = 10, dpi = 72)
          ggsave(plot = g.dn.brain.side,
                 filename = file.path(save.path,identifier,"neuroanatomy",paste0(clust,"_neuroanatomy_brain_side.png")),
                 width = 10, height = 10, dpi = 72)
          ggsave(plot = g.dn.brain.dorsal,
                 filename = file.path(save.path,identifier,"neuroanatomy",paste0(clust,"_neuroanatomy_brain_dorsal.png")),
                 width = 10, height = 10, dpi = 72)
        }
      })
    }
  }
  return(NULL)
}

banc_plot_key_features <- function(
    influence.meta,
    save.path,
    inf.metric = "influence_norm_log",
    col.annotation = NULL,
    row.annotation = NULL,
    show.annotation = TRUE,
    row.thresh = NULL,
    col.thresh = NULL,
    col.order = FALSE,
    row.order = FALSE,
    row.select = NULL,
    col.select = NULL,
    recalculate = FALSE,
    seed.map = NULL,
    target.map = NULL,
    chosen.seeds = unique(seed.map),
    chosen.targets = unique(target.map),
    super.class = NULL,
    influence.level = NULL,
    row.dend = NULL,
    col.dend = NULL,
    row.cols = NULL,
    dend.cols = NULL,
    plot.name = NULL,
    rev = FALSE,
    width = 24, 
    height = 24,
    symmetric = FALSE,
    show.rownames = TRUE,
    show.colnames = TRUE,
    cellheight = 12,
    cellwidth = 12,
    color.min = NULL, 
    color.max = NULL,
    autocorrelation = FALSE,
    diagonal = TRUE,
    method = "cosine",
    quantile = NULL
){
  
  # Reshape the data
  influence_df <- influence.meta %>%
    dplyr::filter(!is.na(seed), 
                  !is.na(target))
  if(!is.null(influence.level)){
    influence_df <- influence_df %>%
      dplyr::filter(level %in% influence.level)
  }
  if(!is.null(super.class)){
    influence_df <- influence_df %>%
      dplyr::filter(grepl(super.class,super_class))
  }
  
  # Rename seeds
  if(!is.null(names(seed.map))){
    influence_df <- influence_df %>%
      dplyr::mutate(seed = case_when(
        seed %in% names(seed.map) ~ seed.map[seed],
        TRUE ~ seed
      ))
  }
  if(!is.null(names(target.map))){
    influence_df <- influence_df %>%
      dplyr::mutate(target = case_when(
        target %in% names(target.map) ~ target.map[target],
        TRUE ~ target
      ))
  }
  
  # normalisations
  if(recalculate){
    influence_df <- calculate_influence_norms(influence_df, quantile=quantile)
  }

  # Choose metric
  influence_df$influence_score <- influence_df[[inf.metric]]
  
  # Filter seeds
  if(!is.null(chosen.seeds)){
    influence_df <- influence_df %>%
      dplyr::filter(seed %in% chosen.seeds)
  }
  if(!is.null(chosen.targets)){
    influence_df <- influence_df %>%
      dplyr::filter(target %in% chosen.targets)
  }

  # Cast
  influence_matrix <- influence_df  %>%
    dplyr::distinct(seed, target, .keep_all = TRUE) %>%
    reshape2::dcast(seed ~ target, 
                    fun.aggregate = mean, 
                    value.var = "influence_score", 
                    fill = 0)
  
  # Set row names and remove the seed column
  rownames(influence_matrix) <- influence_matrix$seed
  influence_matrix$seed <- NULL
  nams <- dimnames(influence_matrix)
  
  # Convert to matrix
  influence_matrix <- as.matrix(influence_matrix)
  influence_matrix <- matrix(as.numeric(as.matrix(influence_matrix)), 
                             nrow = nrow(influence_matrix), 
                             ncol = ncol(influence_matrix))
  influence_matrix[is.na(influence_matrix)] <- 0
  influence_matrix[is.infinite(influence_matrix)] <- 0
  dimnames(influence_matrix) <- nams
  influence_matrix <- t(influence_matrix)
  
  # Remove all-zero rows from the original matrix
  if(!diagonal){
    diag(influence_matrix) <- min(influence_matrix, na.rm = TRUE)
  }

  # Change to autocorrelation matrix:
  if(autocorrelation){
    if(rev){
      dims <- colnames(influence_matrix)
      sparse_matrix <- as(as.matrix(t(influence_matrix)), "dgCMatrix")
      influence_matrix <- cosine_similarity_sparse(t(sparse_matrix))
      rownames(influence_matrix) <- colnames(influence_matrix) <- dims
    }else{
      dims <- rownames(influence_matrix)
      sparse_matrix <- as(as.matrix(t(influence_matrix)), "dgCMatrix")
      influence_matrix <- cosine_similarity_sparse(sparse_matrix)
      rownames(influence_matrix) <- colnames(influence_matrix) <- dims
    }
    if(symmetric){
      col.annotation <- row.annotation
      col.order <- row.order
    }
  }
  if(symmetric){
    row.select <- col.select <- intersect(rownames(influence_matrix),colnames(influence_matrix))
  }
  if(!is.null(row.select)){
    influence_matrix <- influence_matrix[rownames(influence_matrix)%in%row.select,]
  }
  if(!is.null(col.select)){
    influence_matrix <- influence_matrix[,colnames(influence_matrix)%in%col.select]
  }
  
  # remove with thresh
  if(!is.null(row.thresh)){
    row.thresh.real <- quantile(influence_matrix, row.thresh, na.rm = TRUE)
  }
  if(!is.null(col.thresh)){
    col.thresh.real <- quantile(influence_matrix, col.thresh, na.rm = TRUE)
  }
  if(!is.null(row.thresh)){
    influence_matrix <- influence_matrix[apply(influence_matrix, 1, function(row) any(row > row.thresh.real)),]
  }
  if(!is.null(col.thresh)){
    influence_matrix <- influence_matrix[,apply(influence_matrix, 2, function(col) any(col > col.thresh.real))]
  }
  
  # Get col annotations
  annotation_colors <- list()
  if(!is.null(row.annotation)){
    row_annotation <- influence_df %>%
      dplyr::filter(!is.na(target)) %>%
      dplyr::select(eval(row.annotation), target) %>%
      dplyr::distinct(target, .keep_all = TRUE) %>%
      as.data.frame()
    row_annotation[[row.annotation]][is.na(row_annotation[[row.annotation]])] <- "other"
    rownames(row_annotation) <- row_annotation$target
    row_annotation <- row_annotation[rownames(row_annotation) %in% rownames(influence_matrix),]
    row_annotation$target <- NULL 
    entries <- na.omit(unique(row_annotation[[row.annotation]]))
    cols <- rainbow(length(entries))
    names(cols) <- entries
    annotation_colors[[row.annotation]] <- cols
  }else{
    row_annotation <- NULL
    if(is.null(col.annotation)){
      annotation_colors <- NULL
    }
  }
  
  # Get col annotations
  if(!is.null(col.annotation)){
    col_annotation <- influence_df %>%
      dplyr::filter(!is.na(seed)) %>%
      dplyr::select(eval(col.annotation), seed) %>%
      dplyr::distinct(seed, .keep_all = TRUE) %>%
      as.data.frame()
    col_annotation[[col.annotation]][is.na(col_annotation[[col.annotation]])] <- "other"
    rownames(col_annotation) <- col_annotation$seed
    col_annotation <- col_annotation[rownames(col_annotation) %in% colnames(influence_matrix),]
    col_annotation$seed <- NULL 
    entries <- na.omit(unique(col_annotation[[col.annotation]]))
    cols <- rainbow(length(entries))
    names(cols) <- entries
    annotation_colors[[col.annotation]] <- cols
  }else{
    col_annotation <- NULL
    if(is.null(row.annotation)){
      annotation_colors <- NULL
    }
  }
  
  # Order
  if(isTRUE(row.order)&!is.null(row.annotation)){
    # Apply semi-supervised clustering
    groups <- split(rownames(row_annotation), row_annotation[[row.annotation]])
    groups <- lapply(groups, function(g) if(length(g) >= 2) g else NULL)
    groups <- groups[!sapply(groups, is.null)]
    clustering_result <- hclust_semisupervised(data = influence_matrix,
                                               groups = groups,
                                               dist_method = "euclidean",
                                               hclust_method = "ward.D2")
    influence_matrix <- clustering_result$data
    row_annotation <- row_annotation[rownames(row_annotation) %in% rownames(influence_matrix), , drop = FALSE]
    hclust_rows <- clustering_result$hclust
  }else if(is.character(row.order)){
    row.order <- intersect(row.order,rownames(influence_matrix))
    influence_matrix <- influence_matrix[row.order,]
    hclust_rows <- FALSE
    row.dend <- FALSE
  }else{
    if(method=="cosine"){
      cosine_sim_matrix_rows <- lsa::cosine(t(influence_matrix))
      cosine_sim_matrix_rows[is.na(cosine_sim_matrix_rows)] <- 0
      hclust_rows <- hclust(as.dist(1 - cosine_sim_matrix_rows), 
                            method = "ward.D2")
    }else{
      row_dist <- dist(influence_matrix, method = method)
      hclust_rows <- hclust(row_dist, method = "ward.D2")
    }
  }
  if(isTRUE(col.order)&!is.null(col.annotation)){
    groups <- split(rownames(col_annotation), col_annotation[[col.annotation]])
    groups <- lapply(groups, function(g) if(length(g) >= 2) g else NULL)
    groups <- groups[!sapply(groups, is.null)]
    clustering_result <- hclust_semisupervised(data = t(influence_matrix),
                                               groups = groups,
                                               dist_method = "euclidean",
                                               hclust_method = "ward.D2")
    influence_matrix <- t(clustering_result$data)
    col_annotation <- col_annotation[rownames(col_annotation) %in% colnames(influence_matrix), , drop = FALSE]
    hclust_cols <- clustering_result$hclust
  }else if(is.character(col.order)){
    col.order <- intersect(col.order,colnames(influence_matrix))
    influence_matrix <- influence_matrix[,col.order]
    hclust_cols <- FALSE
    col.dend <- FALSE
  }else{
    if(method=="cosine"){
      cosine_sim_matrix_cols <- lsa::cosine(influence_matrix)
      cosine_sim_matrix_cols[is.na(cosine_sim_matrix_cols)] <- 0
      hclust_cols <- hclust(as.dist(1 - cosine_sim_matrix_cols), method = "ward.D2")
    }else{
      col_dist <- dist(t(influence_matrix), method = method)
      hclust_cols <- hclust(col_dist, method = "ward.D2")
    }
  }

  # target rows and columns
  if(symmetric){
    hclust_rows = hclust_cols
    row.dend = col.dend
  }
  if(is.null(row.dend)){
    row.dend = hclust_rows
  }else if (!isFALSE(row.dend)){
    missing <- setdiff(labels(row.dend),rownames(influence_matrix))
    for(m in missing){
      mrow <- matrix(NA,ncol=ncol(influence_matrix),nrow=1)
      rownames(mrow) <- m
      influence_matrix <- rbind(influence_matrix,mrow)
    }
  }
  if(is.null(col.dend)){
    col.dend = hclust_cols
  }else if (!isFALSE(col.dend)){
    missing <- setdiff(labels(col.dend),colnames(influence_matrix))
    for(m in missing){
      mrow <- matrix(NA,nrow=nrow(influence_matrix),ncol=1)
      colnames(mrow) <- m
      influence_matrix <- cbind(influence_matrix,mrow)
    }
  }
  
  # Remove diagonal
  if(!diagonal){
    diag(influence_matrix) <- NA
  }
  
  # Create scaled color palette 5
  n_breaks <- 100
  if(is.null(color.min)){
    color.min <- quantile(influence_matrix, 0.1, na.rm=TRUE)
  }
  if(is.null(color.max)){
    color.max <- quantile(influence_matrix, 0.99, na.rm=TRUE)
  }
  scaled_heatmap_breaks <- seq(color.min, color.max, length.out = n_breaks)
  scaled_heatmap_palette <- colorRampPalette(c("#1f4e79", "#4a90a4", "#7ba7bc", "#a67c8a", "#c4967d", "#b22222"))(n_breaks - 1)
  
  # Toggle showing annotation bars
  if(!show.annotation){
    row_annotation <- NULL
    col_annotation <- NULL
    annotation_colors <- NULL
  }
  
  # Create the heatmap
  if(is.null(plot.name)){
    plot.name <- inf.metric
  }
  if(rev){
    ph.influence <- pheatmap(
      targeting_method = "ward.D2",
      t(influence_matrix),
      cluster_rows = col.dend,
      cluster_cols = row.dend,
      color = scaled_heatmap_palette,
      breaks = scaled_heatmap_breaks,
      annotation_col = row_annotation,
      annotation_row = col_annotation,
      annotation_colors = annotation_colors,
      show_rownames = show.rownames,
      show_colnames = show.colnames,
      treeheight_row = 0, 
      treeheight_col = 0, 
      fontsize_col = 8,
      fontsize_row = 8,
      cellwidth = cellwidth,
      cellheight = cellheight,
      width = width, 
      height = height,
      border_color = NA,
      annotation_legend = TRUE,
      annotation_names_row = FALSE,
      annotation_names_col = FALSE,
      legend = TRUE,
      filename = file.path(save.path,plot.name),
      main = paste0(inf.metric, "\n(row: source, col: target)"),
      na_col = "lightgrey"
    )
  }else{
    ph.influence <- pheatmap(
      targeting_method = "ward.D2",
      influence_matrix,
      cluster_rows = row.dend,
      cluster_cols = col.dend,
      color = scaled_heatmap_palette,
      breaks = scaled_heatmap_breaks,
      annotation_col = col_annotation,
      annotation_row = row_annotation,
      annotation_colors = annotation_colors,
      show_rownames = show.rownames,
      show_colnames = show.colnames,
      treeheight_row = 0, 
      treeheight_col = 0, 
      fontsize_col = 8,
      fontsize_row = 8,
      cellwidth = cellwidth,
      cellheight = cellheight,
      width = width, 
      height = height,
      border_color = NA,
      annotation_legend = TRUE,
      annotation_names_row = FALSE,
      annotation_names_col = FALSE,
      legend = TRUE,
      filename = file.path(save.path,plot.name),
      main = paste0(inf.metric, "\n(row: target, col: source)"),
      na_col = "lightgrey"
      )    
  }

  # Return some useful data
  return(list(
    influence.matrix = influence_matrix,
    row.dend = row.dend,
    col.dend = col.dend
  ))
}



# Rvcg
find_closest_region <- function(df, mesh_list, max.dist = 5000) {
  
  # Function to find distance from point to mesh
  point_to_mesh_distance <- function(point, mesh) {
    distances <- Rvcg::vcgClostKD(mesh=mesh, x=point)
    distances <- distances$quality
    point$distances <- distances
    return(point)
  }
  
  # Iterate through rows where region is NA
  distances <- pbapply::pblapply(mesh_list$RegionList, function(reg){
    p <- point_to_mesh_distance(xyzmatrix(df), mesh = as.mesh3d(subset(mesh_list,reg)))
    p$distances
  })
  
  # Find nearest mesh
  distances.m <- do.call(cbind,distances)
  colnames(distances.m) <- mesh_list$RegionList
  chosen <- apply(abs(distances.m), 1, which.min)
  min.dists <- apply(abs(distances.m), 1, function(row) min(row)<max.dist)
  df$neuropil <- colnames(distances.m)[chosen]
  
  # Assign
  df <- df %>%
    dplyr::mutate(region = dplyr::case_when(
      grepl("vnc",neuropil) ~ "vnc",
      grepl("optic",neuropil) ~ "optic_lobes",
      grepl("GNG|CAN|FLA|AMMC|SAD|PRW",neuropil) ~ "sez",
      grepl("midbrain",neuropil) ~ "central_brain",
    ))
  
  # Determine that some are outside
  df$neuropil[!min.dists] <- paste0("outside_",df$neuropil[!min.dists])
  df$region[!min.dists] <- paste0("outside_",df$region[!min.dists])
  
  # Return
  return(df)
}

# Find which neuropil surfaces synapses are nearest to
pointsnearby_banc <- function(x,id="id"){
  
  # Get volume list
  volumes <- c(subset(banc_vnc_neuropils.surf,"COURT"),subset(banc_brain_neuropils.surf,"ITO"))
  
  # Neuropil missing
  x.no.neuropil <- x %>%
    dplyr::filter((is.na(region)|is.na(neuropil)|grepl("^brain|outside",region))|grepl("outside",neuropil))
  x.neuropil <- x %>%
    anti_join(x.no.neuropil, by=id)
  x.corrected <- find_closest_region(x.no.neuropil, volumes)
  
  # Re-combine and return
  rbind(x.neuropil,x.corrected)
  
}

# Find which neuropil synapses are inside of
pointsinside_banc <- function(x,
                              neuropils = list(banc_brain_neuropils.surf,
                                               banc_vnc_neuropils.surf),
                              volumes = list(neck = banc_neck_connective.surf,
                                             brain = banc_brain_neuropil.surf,
                                             optic_lobes = as.hxsurf(as.mesh3d(subset(banc_brain_neuropils.surf,"optic"))),
                                             sez = as.hxsurf(as.mesh3d(subset(banc_brain_neuropils.surf,"GNG|CAN|FLA|AMMC|SAD|PRW"))),
                                             central_brain = as.hxsurf(as.mesh3d(subset(banc_brain_neuropils.surf,"midbrain"))),
                                             vnc = banc_vnc_neuropil.surf),
                              alpha = 50000,
                              scaling = NULL){
  df = as.data.frame(x)
  df$neuropil = NA
  df$region = NA
  df$side = NA
  df$neuropil <- ""
  df$region <- ""
  df$side <- ""
  points = nat::xyzmatrix(df)
  if(!is.null(scaling)){
    points = points/scaling
  } 
  lrdiffs <- bancr:::banc_lr_position(points,units = "nm")
  sides <- ifelse(lrdiffs>0,"right","left")
  df$side <- sides
  for(vol in 1:length(volumes)){
    neuropil = volumes[[vol]]
    reg = names(volumes)[vol]
    if (!is.null(alpha)) {
      neuropil = alphashape3d::ashape3d(nat::xyzmatrix(neuropil), 
                                        alpha = alpha)
      a = alphashape3d::inashape3d(points = points, 
                                   as3d = neuropil, 
                                   indexAlpha = "ALL")
    }
    else {
      a = nat::pointsinside(x = points, surf = neuropil)
    }
    if(sum(a)) df$region[which(a == T)] = reg
  } 
  for(brain in neuropils){
    nps = sort(brain$RegionList)
    for (np in nps) {
      neuropil <- subset(brain, np)
      region <- NA
      if(is.na(region)) region = ifelse(np %in%banc_vnc_neuropils.surf$RegionList,"vnc",NA)
      if(is.na(region)) region = ifelse(grepl("^LO|^ME|^AME|^LOP",np),"optic_lobes",NA)
      if(is.na(region)) region = ifelse(grepl("^CAN|^GNG|^FLA|^AMMC|^SAD|^PRW",np),"optic_lobes",NA)
      if(is.na(region)) region = ifelse(np %in%banc_brain_neuropils.surf$RegionList,"central_brain",NA)
      if (!is.null(alpha)) {
        neuropil = alphashape3d::ashape3d(nat::xyzmatrix(neuropil), 
                                          alpha = alpha)
        a = alphashape3d::inashape3d(points = points, 
                                     as3d = neuropil, 
                                     indexAlpha = "ALL")
      }
      else {
        a = nat::pointsinside(x = points, surf = neuropil)
      }
      if(sum(a)){
        df$neuropil[which(a)] = sapply(df$neuropil[which(a)], function(x) paste(unique(unlist(strsplit(paste(x,np,sep=","),split=","))),sep=",",collapse=","))
        df$region[which(a)] = region
      }
    } 
  }
  df <- df %>%
    dplyr::mutate(neuropil = ifelse(neuropil=="","outside",neuropil),
                  region = ifelse(region=="","outside",region)) %>%
    dplyr::mutate(neuropil = gsub("^,","",neuropil),
                  region = gsub("^,","",region)) 
  df
}

# round numbers
round_dataframe <- function(x, exclude=NULL, digits = 4, ...) {
  numcols <- names(x)[sapply(x, function(c) is.numeric(c) && !inherits(c, 'integer64'))]
  numcols <- setdiff(numcols, exclude)
  for(i in numcols) {
    col=x[[i]]
    # does it look like an int, if so, make it one
    intcol=try(checkmate::asInteger(col), silent = TRUE)
    if((sum(is.na(col))==length(col))){
      x[[i]]=col
    }else if(is.integer(intcol)){
      x[[i]]=intcol
    }
    else{
      x[[i]]= signif(col, digits)
    }
  }
  x
}

