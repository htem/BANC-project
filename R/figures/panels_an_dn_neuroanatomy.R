################################
### NECK NEURON NEUROANATOMY ###
################################

###############
### STARTUP ###
###############

# load
source("R/startup/banc-startup.R")
source("R/startup/franken-meta.R")
source("R/startup/banc-meta.R")
source("R/startup/banc-functions.R")

# combine
cns.clusters <- plyr::rbind.fill(umap.dn.df,
                                 umap.eff.df) %>%
  dplyr::mutate(cluster = dplyr::case_when(
    grepl("sensory",super_class) ~ cell_sub_class,
    TRUE ~ cluster
  ))

# Make neurooanatomy plot template
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
g.anat.main <- g.anat +
  geom_neuron(x = banc_neuropil.surf,
              cols = c("grey60", "grey30"),
              rotation_matrix = bancr:::banc_rotation_matrices[["main"]],
              alpha = 0.1)
g.anat.brain <- g.anat +
  geom_neuron(x = banc_brain_neuropil.surf,
              cols = c("grey60", "grey30"),
              rotation_matrix = bancr:::banc_rotation_matrices[["front"]],
              alpha = 0.1)
g.anat.vnc <- g.anat +
  geom_neuron(x = banc_vnc_neuropil.surf,
              cols = c("grey60", "grey30"),
              rotation_matrix = bancr:::banc_rotation_matrices[["vnc"]],
              alpha = 0.1)

###################
### PLOT MESHES ###
###################
clusters <- na.omit(unique(cns.clusters$cluster))
for(clust in clusters){
  try({
  message("working on:", clust)
  banc.neck.sp.meta <- cns.clusters %>%
    dplyr::filter(cluster==clust)

  # All cell sub classes
  cts <- na.omit(unique(banc.neck.sp.meta$cell_type))
  
  ##### NBLAST clustering
  
  # # get neuron skeletons
  # l2 <- banc_read_l2skel(unique(banc.neck.sp.meta$root_id), OmitFailures = TRUE)
  # dps <- dotprops(l2/1000, OmitFailures = TRUE)
  # banc.neck.sp.meta <- banc.neck.sp.meta %>%
  #   dplyr::filter(root_id %in% names(dps))
  # 
  # # run NBLAST
  # nb <- nat.nblast::nblast_allbyall(dps)
  # 
  # # amalgamate over cluster
  # rownames(nb) <- banc.neck.sp.meta$cell_type[match(banc.neck.sp.meta$root_id,rownames(nb))]
  # colnames(nb) <- banc.neck.sp.meta$cell_type[match(banc.neck.sp.meta$root_id,colnames(nb))]
  # nb = apply(nb, 2, function(i) tapply(i, rownames(nb), max, na.rm = TRUE))
  # nb = t(apply(t(nb), 2, function(i) tapply(i, colnames(nb), max, na.rm = TRUE)))
  # 
  # # cluster
  # hckcs <- nhclust(scoremat=nb, method = "ward.D2")
  # dend <- as.dendrogram(hckcs)
  # grps <- cutree(hckcs,k=8)
  # 
  # # Get the order of leaves
  # ordered_leaves <- labels(dend)
  # 
  # # Generate a color palette with the required number of colors
  # pcols <- cerise_limon_palette(length(cts))
  # ct.cols <- as.vector(pcols)
  # names(ct.cols) <- c(ordered_leaves,setdiff(cts,ordered_leaves))
  # 
  # ##### plot dendrogram
  # 
  # # Convert dendrogram to a data frame that ggplot can use
  # dendr_data <- dendro_data(dend)
  # 
  # # Get the leaf labels
  # leaf_labels <- label(dendr_data)
  # 
  # # Create a data frame for the labels, including their colors
  # label_data <- leaf_labels %>%
  #   mutate(color = ct.cols[label])
  # 
  # # Create the plot
  # g.dend <- ggplot() +
  #   geom_segment(data = segment(dendr_data), linewidth = 1,
  #                aes(x = x, y = y, xend = xend, yend = yend)) +
  #   geom_text(data = label_data,
  #             aes(x = x, y = y, label = label, color = color),
  #             hjust = 1.2, angle = 90, size = 4, vjust = 0.5) +
  #   scale_color_identity() +
  #   scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  #   coord_cartesian(clip = "off") +
  #   theme_minimal() +
  #   theme(
  #     axis.text = element_blank(),
  #     axis.ticks = element_blank(),
  #     panel.grid = element_blank(),
  #     plot.margin = margin(t = 5, r = 5, b = 50, l = 5, unit = "pt")
  #   ) +
  #   labs(x = NULL, y = NULL, title = "")
  # 
  # # Save the plot
  # ggsave(plot = g.dend,
  #        filename = file.path(banc.fig3.anat.path,paste0(clust,"_cluster_nblast.pdf")),
  #        width = 30, height = 2, dpi = 300)
  
  ##### Plot neuroanatomy
    g.anat.main <- g.anat +
      geom_neuron(x = banc_neuropil.surf,
                  cols = c("grey60", "grey30"),
                  rotation_matrix = bancr:::banc_rotation_matrices[["main"]],
                  alpha = 0.1)
    g.anat.brain <- g.anat +
      geom_neuron(x = banc_brain_neuropil.surf,
                  cols = c("grey60", "grey30"),
                  rotation_matrix = bancr:::banc_rotation_matrices[["front"]],
                  alpha = 0.1)
    g.anat.vnc <- g.anat +
      geom_neuron(x = banc_vnc_neuropil.surf,
                  cols = c("grey60", "grey30"),
                  rotation_matrix = bancr:::banc_rotation_matrices[["vnc"]],
                  alpha = 0.1)
    ct.meta <- banc.neck.sp.meta %>%
      dplyr::filter(side %in% c("right","midline","center")) %>%
      dplyr::arrange(cell_type)
    if(!nrow(ct.meta)){
      next
    }
    neurons.plot <- banc_read_neuron_meshes(ct.meta$root_id, OmitFailures = TRUE)
    if(!length(neurons.plot)){
      next
    }
    # plot.cts <- ct.meta$cell_type[match(ct.meta$root_id,names(neurons.plot))]
    # plot.cols <- ct.cols[plot.cts]
    plot.col <- paper.cols[[clust]]
    g.anat.main <- g.anat.main +
      geom_neuron(x = neurons.plot,
                  cols = c(adjust_color_brightness(plot.col,1.1),adjust_color_brightness(plot.col,0.9)),
                  rotation_matrix = bancr:::banc_rotation_matrices[["main"]],
                  alpha = 0.5)
    ggsave(plot = g.anat.main,
           filename = file.path(banc.fig3.anat.path,paste0(clust,"_main_neuroanatomy.png")),
           width = 10, height = 10, dpi = 300)
    neurons.plot.brain <- banc_decapitate(neurons.plot, invert = TRUE, OmitFailures = TRUE)
    if(length(neurons.plot.brain)){
      g.anat.brain <- g.anat.brain +
        geom_neuron(x = neurons.plot.brain,
                    cols = c(adjust_color_brightness(plot.col,1.1),adjust_color_brightness(plot.col,0.9)),
                    rotation_matrix = bancr:::banc_rotation_matrices[["front"]],
                    alpha = 0.5)
      ggsave(plot = g.anat.brain,
             filename = file.path(banc.fig3.anat.path,paste0(clust,"_brain_neuroanatomy.png")),
             width = 10, height = 10, dpi = 300)
    }
    neurons.plot.vnc <-  banc_decapitate(neurons.plot, invert = FALSE, OmitFailures = TRUE)
    if(length(neurons.plot.vnc)){
      g.anat.vnc <- g.anat.vnc +
        geom_neuron(x = neurons.plot.vnc,
                    cols = c(adjust_color_brightness(plot.col,1.1),adjust_color_brightness(plot.col,0.9)),
                    rotation_matrix = bancr:::banc_rotation_matrices[["vnc"]],
                    alpha = 0.5)
      ggsave(plot = g.anat.vnc,
             filename = file.path(banc.fig3.anat.path,paste0(clust,"_vnc_neuroanatomy.png")),
             width = 10, height = 10, dpi = 300)
    }
  })
}

###########################
### PLOT SUPER CLUSTERS ###
###########################
clusters <- rev(sort(na.omit(unique(banc.meta$super_cluster))))
for(clust in clusters){
  try({
    message("working on:", clust)
    banc.cns.clust <- banc.meta %>%
      dplyr::filter(super_cluster==clust) 
    ##### Plot neuroanatomy
    g.anat.main <- g.anat +
      geom_neuron(x = banc_neuropil.surf,
                  cols = c("grey60", "grey30"),
                  rotation_matrix = bancr:::banc_rotation_matrices[["main"]],
                  alpha = 0.1)
    g.anat.brain <- g.anat +
      geom_neuron(x = banc_brain_neuropil.surf,
                  cols = c("grey60", "grey30"),
                  rotation_matrix = bancr:::banc_rotation_matrices[["front"]],
                  alpha = 0.1)
    g.anat.vnc <- g.anat +
      geom_neuron(x = banc_vnc_neuropil.surf,
                  cols = c("grey60", "grey30"),
                  rotation_matrix = bancr:::banc_rotation_matrices[["vnc"]],
                  alpha = 0.1)
    ct.meta <- banc.cns.clust %>%
      dplyr::filter(side %in% c("right","midline","center")) %>%
      dplyr::arrange(cell_type)
    if(!nrow(ct.meta)){
      next
    }
    neurons.plot <- banc_read_neuron_meshes(ct.meta$root_id, OmitFailures = TRUE)
    if(!length(neurons.plot)){
      next
    }
    plot.col <- paper.cols[[clust]]
    g.anat.main <- g.anat.main +
      geom_neuron(x = neurons.plot,
                  cols = c(adjust_color_brightness(plot.col,1.1),adjust_color_brightness(plot.col,0.9)),
                  rotation_matrix = bancr:::banc_rotation_matrices[["main"]],
                  alpha = 0.5)
    ggsave(plot = g.anat.main,
           filename = file.path(banc.fig3.anat.path,paste0(clust,"_main_neuroanatomy.png")),
           width = 10, height = 10, dpi = 300)
    neurons.plot.brain <- banc_decapitate(neurons.plot, invert = TRUE, OmitFailures = TRUE)
    if(length(neurons.plot.brain)){
      g.anat.brain <- g.anat.brain +
        geom_neuron(x = neurons.plot.brain,
                    cols = c(adjust_color_brightness(plot.col,1.1),adjust_color_brightness(plot.col,0.9)),
                    rotation_matrix = bancr:::banc_rotation_matrices[["front"]],
                    alpha = 0.5)
      ggsave(plot = g.anat.brain,
             filename = file.path(banc.fig3.anat.path,paste0(clust,"_brain_neuroanatomy.png")),
             width = 10, height = 10, dpi = 300)
    }
    neurons.plot.vnc <-  banc_decapitate(neurons.plot, invert = FALSE, OmitFailures = TRUE)
    if(length(neurons.plot.vnc)){
      g.anat.vnc <- g.anat.vnc +
        geom_neuron(x = neurons.plot.vnc,
                    cols = c(adjust_color_brightness(plot.col,1.1),adjust_color_brightness(plot.col,0.9)),
                    rotation_matrix = bancr:::banc_rotation_matrices[["vnc"]],
                    alpha = 0.5)
      ggsave(plot = g.anat.vnc,
             filename = file.path(banc.fig3.anat.path,paste0(clust,"_vnc_neuroanatomy.png")),
             width = 10, height = 10, dpi = 300)
    }
  })
}

#########################
### PLOT CNS CLUSTERS ###
#########################
clusters <- sort(na.omit(unique(banc.meta$cns_network)))
# # Colours
# paper.cols2 <- paper.cols
# paper.cols2[["central_brain"]] = "grey50"
# paper.cols2[["ventral_nerve_cord"]] = "grey10"
# paper.cols2[["optic_lobe"]] = "grey90"
for(clust in clusters){
  try({
    message("working on:", clust)
    banc.cns.clust <- banc.meta %>%
      dplyr::filter(cns_network==clust) 

    ##### NBLAST clustering
    
    # get neuron skeletons
    synapses <- FALSE
    if(synapses){
      # On O2
      neck.synapses.list <- pbapply::pblapply(unique(banc.cns.clust$root_id), function(x)
        bancr::banc_partners(x,
                             partners = "output"))
      neck.synapses.list.select <- pbapply::pblapply(neck.synapses.list, function(x){
        tryCatch({points <- nat::xyzmatrix(x$pre_pt_position)
        x$pre_pt_root_id <- as.character(x$pre_pt_root_id)
        cbind(x,points) %>%
          dplyr::select(pre_pt_root_id,X,Y,Z)
        }, function(e)
          NULL)
      }
      )
      neurons.plot <- do.call(rbind, neck.synapses.list.select) %>%
        dplyr::mutate(X=as.numeric(X),
                      Y=as.numeric(Y),
                      Z=as.numeric(Z)) %>%
        dplyr::filter(!is.na(X)) %>%
        nat::xyzmatrix()
    }else{
      l2 <- banc_read_l2skel(unique(banc.cns.clust$root_id), OmitFailures = TRUE)
      e <- nlapply(l2,function(x) xyzmatrix(x)[nat::endpoints(x),])
      neurons.plot <- do.call(rbind,e)
    }
    neurons.plot <- neurons.plot[pointsinside(neurons.plot,banc_neuropil.surf),]
    n_rows <- nrow(neurons.plot)
    sampled_idx <- sample(n_rows, size = floor(n_rows / 10))
    neurons.plot <- neurons.plot[sampled_idx, , drop = FALSE]
    
    # Find neuropil inclusion
    # Define the chunk size
    message("making: neurons.np")
    chunk_size <- 1000
    
    # Split the dataframe into a list of chunks
    neurons.np <- neurons.plot %>%
      as.data.frame() %>%
      dplyr::mutate(id = row_number()) %>%
      dplyr::mutate(neuropil = NA,
                    region = NA,
                    side = NA,
                    chunk = ceiling(dplyr::row_number() / chunk_size)
                    ) %>%
      dplyr::group_by(chunk) %>%
      dplyr::group_split()

    # Process each chunk
    neurons.np <- purrr::map(neurons.np, function(chunk) {
      message("running: pointsinside_banc")
      data <-pointsinside_banc(chunk)
      message("running: pointsnearby_banc")
      data <- pointsnearby_banc(data)
      data
    })
    neurons.np.df <- bind_rows(neurons.np) %>%
      dplyr::mutate(neuropil = gsub("ITO_optic_|ITO_midbrain_|COURT_vnc_|_L|_R|_right|_left","",neuropil)) %>%
      dplyr::mutate(neuropil = gsub("MB_.*","MB",neuropil)) %>%
      dplyr::arrange(region, neuropil)
    
    top_neuropils <- neurons.np.df %>%
      dplyr::count(neuropil, sort = TRUE) %>%
      dplyr::top_n(3, n) %>%
      dplyr::pull(neuropil)
    
    data_plot <- neurons.np.df %>%
      dplyr::mutate(
        neuropil_plot = ifelse(neuropil %in% top_neuropils, neuropil, "other"),
        region_plot   = ifelse(neuropil %in% top_neuropils, region, "other")
      ) %>%
      dplyr::count(neuropil_plot, region_plot) %>%
      dplyr::mutate(percent = n / sum(n) * 100)
    
    g.np <- ggplot2::ggplot(data_plot, ggplot2::aes(x = reorder(neuropil_plot, n), y = percent, fill = region_plot)) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::scale_fill_manual(values = paper.cols) +
      ggplot2::labs(x = "", y = "", fill = "") +
      ggplot2::theme_minimal() +
      theme(legend.position = "none")
    
    # Plot pie chart
    ggsave(plot = g.np,
           filename = file.path(banc.fig6.anat.path,paste0(clust,"_neuropil_bar.pdf")),
           width = 2, height = 2, dpi = 300)
    
    ##### Plot neuroanatomy
    g.anat.main <- g.anat +
      geom_neuron(x = banc_neuropil.surf,
                  cols = c("grey60", "grey30"),
                  rotation_matrix = bancr:::banc_rotation_matrices[["main"]],
                  alpha = 0.1)
    g.anat.brain <- g.anat +
      geom_neuron(x = banc_brain_neuropil.surf,
                  cols = c("grey60", "grey30"),
                  rotation_matrix = bancr:::banc_rotation_matrices[["front"]],
                  alpha = 0.1)
    g.anat.vnc <- g.anat +
      geom_neuron(x = banc_vnc_neuropil.surf,
                  cols = c("grey60", "grey30"),
                  rotation_matrix = bancr:::banc_rotation_matrices[["vnc"]],
                  alpha = 0.1)
    ggsave(plot = g.anat.brain,
           filename = file.path(banc.fig6.anat.path,"empty_brain.png"),
           width = 10, height = 10, dpi = 300)
    ggsave(plot = g.anat.vnc,
           filename = file.path(banc.fig6.anat.path,"empty_vnc.png"),
           width = 10, height = 10, dpi = 300)
    if(!nrow(banc.cns.clust)){
      next
    }
    if(!length(neurons.plot)){
      next
    }
    x <- as.data.frame(nat::xyzmatrix(neurons.plot))
    x <- as.data.frame(t(bancr:::banc_rotation_matrices[["main"]][, 1:3] %*% t(nat::xyzmatrix(x))))
    x <- x[, -4]
    colnames(x) <- c("X", "Y", "Z")
    g.anat.main <- g.anat.main +
      stat_density_2d(data = x,
                      aes(x = X, 
                          y = Y, 
                          fill = after_stat(level)), 
                      n = 100,
                      geom = "polygon", 
                      alpha = 0.5) +
      scale_fill_viridis_c(option = "C") +
      ggplot2::theme_void() +
      ggplot2::guides(fill = "none", color = "none") 
    ggsave(plot = g.anat.main,
           filename = file.path(banc.fig6.anat.path,paste0(clust,"_main_neuroanatomy.png")),
           width = 10, height = 10, dpi = 300)
    Sys.sleep(2)
    neurons.plot.brain <- banc_decapitate(neurons.plot, invert = TRUE, OmitFailures = TRUE)
    if(length(neurons.plot.brain)){
      x <- as.data.frame(nat::xyzmatrix(neurons.plot.brain))
      x <- as.data.frame(t(bancr:::banc_rotation_matrices[["front"]][, 1:3] %*% t(nat::xyzmatrix(x))))
      x <- x[, -4]
      colnames(x) <- c("X", "Y", "Z")
      g.anat.brain <- g.anat.brain +
        stat_density_2d(data = x,
                        aes(x = X, 
                            y = Y, 
                            fill = after_stat(level)), 
                        n = 100,
                        geom = "polygon", 
                        alpha = 0.5) +
        scale_fill_viridis_c(option = "C") +
        ggplot2::theme_void() +
        ggplot2::guides(fill = "none", color = "none") 
      ggsave(plot = g.anat.brain,
             filename = file.path(banc.fig6.anat.path,paste0(clust,"_brain_neuroanatomy.png")),
             width = 10, height = 10, dpi = 300)
      Sys.sleep(2)
    }
    neurons.plot.vnc <-  banc_decapitate(neurons.plot, invert = FALSE, OmitFailures = TRUE)
    if(length(neurons.plot.vnc)){
      x <- as.data.frame(nat::xyzmatrix(neurons.plot.vnc))
      x <- as.data.frame(t(bancr:::banc_rotation_matrices[["vnc"]][, 1:3] %*% t(nat::xyzmatrix(x))))
      x <- x[, -4]
      colnames(x) <- c("X", "Y", "Z")
      g.anat.vnc <- g.anat.vnc +
        stat_density_2d(data = x,
                        aes(x = X, 
                            y = Y, 
                            fill = after_stat(level)), 
                        n = 100,
                        geom = "polygon", 
                        alpha = 0.5) +
        scale_fill_viridis_c(option = "C") +
        ggplot2::theme_void() +
        ggplot2::guides(fill = "none", color = "none") 
      ggsave(plot = g.anat.vnc,
             filename = file.path(banc.fig6.anat.path,paste0(clust,"_vnc_neuroanatomy.png")),
             width = 10, height = 10, dpi = 300)
      Sys.sleep(2)
    }
  })
}

# ###########################
# ### MAKE SKELETON PLOTS ###
# ###########################
# 
# # Make interpretable plots
# banc_interpret_umaps(
#   umap.df = umap.dn.df,
#   elist.pre = NULL,
#   elist.post = NULL,
#   influence.df = NULL,
#   identifier = "",
#   neuroanatomy = TRUE,
#   umaps = TRUE,
#   banc.meta  = banc.meta,
#   save.path = banc.fig3.extra.path
# )
# 
# # Make interpretable plots
# banc_interpret_umaps(
#   umap.df = umap.eff.df,
#   elist.pre = NULL,
#   elist.post = NULL,
#   influence.df = NULL,
#   identifier = "",
#   neuroanatomy = TRUE,
#   umaps = TRUE,
#   banc.meta  = banc.meta,
#   save.path = banc.fig2.extra.path
# )
