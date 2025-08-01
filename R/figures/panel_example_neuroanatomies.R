#######################
### REPORT MATCHING ###
#######################
source("R/startup/banc-startup.R")
banc.version <- NULL
source("R/startup/franken-meta.R")
source("R/startup/banc-meta.R")

####################
### Meta example ###
####################

# LB1a meta data
LB1a.meta <- banc.meta %>%
  dplyr::filter(cell_type=="LB1a", side == "right")
LB1a.banc.id <- LB1a.meta$root_id
sens.banc.neuron <- bancr::banc_read_neuron_meshes(LB1a.banc.id)

# DVMn meta data
DVMn.meta <- banc.meta %>%
  dplyr::filter(cell_type=="DVM1a-c", side == "right")
DVMn.banc.id <- DVMn.meta$root_id
mn.banc.neuron <- bancr::banc_read_neuron_meshes(DVMn.banc.id)

# Plot example DN neuroanatomy
g.anat.banc <- g.anat +
  geom_neuron(x = banc_neuropil.surf,
              cols = c("grey60", "grey30"),
              rotation_matrix = bancr:::banc_rotation_matrices[["main"]],
              alpha = 0.1) + 
  geom_neuron(x = sens.banc.neuron,
              cols = c(adjust_color_brightness(paper.cols[["sensory"]],1), 
                       adjust_color_brightness(paper.cols[["sensory"]],0.5)),
              rotation_matrix = bancr:::banc_rotation_matrices[["main"]],
              alpha = 1) +
  geom_neuron(x = mn.banc.neuron,
              cols = c(adjust_color_brightness(paper.cols[["motor"]],1), 
                       adjust_color_brightness(paper.cols[["motor"]],0.5)),
              rotation_matrix = bancr:::banc_rotation_matrices[["main"]],
              alpha = 1)

# Save the plot
ggsave(plot = g.anat.banc,
       filename = file.path(banc.fig1.path,"banc_meta_example.png"),
       width = 10, height = 10, dpi = 300)

#############################
#### REGION NEUROANATOMY ####
#############################

# Get regions
optic <- as.hxsurf(as.mesh3d(subset(banc_brain_neuropils.surf,"optic")))
sez <- as.hxsurf(as.mesh3d(subset(banc_brain_neuropils.surf,"CAN|FLA|GNG|AMMC|SAD|PRW")))
spz <- as.hxsurf(as.mesh3d(subset(banc_brain_neuropils.surf,banc_brain_neuropils.surf$RegionList[!grepl("CAN|FLA|GNG|AMMC|SAD|PRW|optic",banc_brain_neuropils.surf$RegionList)])))
cb <- as.hxsurf(as.mesh3d(subset(banc_brain_neuropils.surf,banc_brain_neuropils.surf$RegionList[grepl("midbrain",banc_brain_neuropils.surf$RegionList)])))
vnc <- banc_vnc_neuropil.surf
neck <- banc_neck_connective.surf

# Make plot of regions
front <- bancr:::banc_rotation_matrices[["main"]]
g.anat <- ggplot2::ggplot() +
  geom_neuron(x = banc.surf,
              cols = "skyblue",
              rotation_matrix = front,
              alpha = 0.1) +
  geom_neuron(x = neck,
              cols = c(adjust_color_brightness(paper.cols[["neck_connective"]], 0.95),adjust_color_brightness(paper.cols[["neck_connective"]], 1.05)),
              rotation_matrix = front,
              alpha = 1) +
  geom_neuron(x = optic,
              cols = c(adjust_color_brightness(paper.cols[["optic_lobe"]], 0.7),adjust_color_brightness(paper.cols[["optic_lobe"]], 1.3)),
              rotation_matrix = front,
              alpha = 1) +
  geom_neuron(x = cb,
              cols = c(adjust_color_brightness(paper.cols[["central_brain"]], 0.7),adjust_color_brightness(paper.cols[["central_brain"]], 1.3)),
              rotation_matrix = front,
              alpha = 1) +
  # geom_neuron(spz,
  #             cols = c(adjust_color_brightness(paper.cols[["central_brain"]], 0.7),adjust_color_brightness(paper.cols[["central_brain"]], 1.3)),
  #             rotation_matrix = front,
  #             alpha = 1) +
  # geom_neuron(sez,
  #             cols = c(adjust_color_brightness(paper.cols[["suboesophageal_zone"]], 0.7),adjust_color_brightness(paper.cols[["suboesophageal_zone"]], 1.3)),
  #             rotation_matrix = front,
  #             alpha = 1) +
  geom_neuron(vnc,
              cols = c(adjust_color_brightness(paper.cols[["ventral_nerve_cord"]], 0.95),adjust_color_brightness(paper.cols[["ventral_nerve_cord"]], 1.05)),
              rotation_matrix = front,
              alpha = 1) +
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

# Save plot
ggsave(g.anat,
       filename = file.path(banc.fig1.path,"banc_regions.png"),
       height = 8, width = 8)

##################
### FETCH DATA ###
##################

# DNa02 meta data
dna02.meta <- franken.meta %>%
  dplyr::filter(cell_type=="DNa02", side == "right")
dna02.fafb.id <- gsub(",","",dna02.meta$fafb_id)
dna02.manc.id <-  gsub(",","",dna02.meta$manc_id)
dna02.banc.id <- dna02.meta$banc_id

# Get neuron mesh data
fafb.neuron <- fafbseg::read_cloudvolume_meshes(dna02.fafb.id)
manc.neuron <- malevnc::read_manc_meshes(dna02.manc.id)
banc.neuron <- bancr::banc_read_neuron_meshes(dna02.banc.id)

# Transform FAFB neuron
fafb.neuron.jrc2018f <- nat.templatebrains::xform_brain(fafb.neuron, sample = "FAFB14", reference = "JRC2018F")
fafb.neuron.jrc2018f <- nat.templatebrains::mirror_brain(fafb.neuron.jrc2018f, brain = nat.flybrains::JRC2018F, transform = "flip")
fafb.neuron.banc <- bancr::banc_to_JRC2018F(fafb.neuron.jrc2018f, method = "tpsreg", inverse = TRUE)

# Transform MANC neuron
manc.neuron.jrcvnc2018f <- xform_brain(manc.neuron/1e3, reference = "JRCVNC2018F", sample="MANC")
manc.neuron.banc <- banc_to_JRC2018F(manc.neuron.jrcvnc2018f, region="vnc", method="tpsreg", banc.units = "nm", inverse = TRUE)

# Set up a neurooanatomy plot
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

##################
### FAFB DNaO2 ###
##################

# Plot example DN neuroanatomy
fafb.view <- structure(c(0.995097458362579, 0.00146017596125603, 
                         -0.0988877862691879, 0, 0.00619349256157875, -0.998848617076874, 
                         0.0475753247737885, 0, -0.09870445728302, -0.0479544699192047, 
                         -0.993960678577423, 0, 0, 0, 0, 1), dim = c(4L, 4L))
g.anat.fafb <- g.anat +
  geom_neuron(x = FAFB14.surf,
              cols = c("grey60", "grey30"),
              rotation_matrix = fafb.view,
              alpha = 0.1) + 
  geom_neuron(x = fafb.neuron,
              cols = c(adjust_color_brightness(paper.cols[["FAFB"]],1.1), 
                       adjust_color_brightness(paper.cols[["FAFB"]],0.9)),
              rotation_matrix = fafb.view,
              alpha = 1)

# Save the plot
ggsave(plot = g.anat.fafb,
       filename = file.path(banc.fig1.path,"DNa02_fafb.png"),
       width = 10, height = 10, dpi = 300)

##################
### MANC DNaO2 ###
##################

# Plot example DN neuroanatomy
manc.view <- structure(c(1, 0, 0, 0, 0, 0.342020143325668, 
                         -0.939692620785909, 0, 0, 0.939692620785909, 0.342020143325668, 
                         0, 0, 0, 0, 1), dim = c(4L, 4L))
g.anat.manc <- g.anat +
  geom_neuron(x = malevnc::MANC.surf,
              cols = c("grey60", "grey30"),
              rotation_matrix = manc.view,
              alpha = 0.05) + 
  geom_neuron(x = manc.neuron/1000,
              cols = c(adjust_color_brightness(paper.cols[["MANC"]],1.1), 
                       adjust_color_brightness(paper.cols[["MANC"]],0.9)),
              rotation_matrix = manc.view,
              alpha = 1)

# Save the plot
ggsave(plot = g.anat.manc,
       filename = file.path(banc.fig1.path,"DNa02_manc.png"),
       width = 10, height = 10, dpi = 300)

##################
### BANC DNaO2 ###
##################

# Plot example DN neuroanatomy
g.anat.banc <- g.anat +
  geom_neuron(x = banc_neuropil.surf,
              cols = c("grey60", "grey30"),
              rotation_matrix = bancr:::banc_rotation_matrices[["main"]],
              alpha = 0.1) + 
  geom_neuron(x = banc.neuron,
              cols = c(adjust_color_brightness(paper.cols[["BANC"]],1.1), 
                       adjust_color_brightness(paper.cols[["BANC"]],0.9)),
              rotation_matrix = bancr:::banc_rotation_matrices[["main"]],
              alpha = 1)

# Save the plot
ggsave(plot = g.anat.banc,
       filename = file.path(banc.fig1.path,"DNa02_banc.png"),
       width = 10, height = 10, dpi = 300)

#####################
### Co-plot DNaO2 ###
#####################

# Plot example DN neuroanatomy
g.anat.coplot <- g.anat +
  geom_neuron(x = banc_neuropil.surf,
              cols = c("grey60", "grey30"),
              rotation_matrix = bancr:::banc_rotation_matrices[["main"]],
              alpha = 0.1) + 
  geom_neuron(x = fafb.neuron.banc,
              cols = c(adjust_color_brightness(paper.cols[["FAFB"]],1.1), 
                       adjust_color_brightness(paper.cols[["FAFB"]],0.9)),
              rotation_matrix = bancr:::banc_rotation_matrices[["main"]],
              alpha = 0.5) +
  geom_neuron(x = banc_decapitate(manc.neuron.banc),
              cols = c(adjust_color_brightness(paper.cols[["MANC"]],1.1), 
                       adjust_color_brightness(paper.cols[["MANC"]],0.9)),
              rotation_matrix = bancr:::banc_rotation_matrices[["main"]],
              alpha = 0.5) +
  geom_neuron(x = banc.neuron,
              cols = c(adjust_color_brightness(paper.cols[["BANC"]],1.1), 
                       adjust_color_brightness(paper.cols[["BANC"]],0.9)),
              rotation_matrix = bancr:::banc_rotation_matrices[["main"]],
              alpha = 0.5)

# Save the plot
ggsave(plot = g.anat.coplot,
       filename = file.path(banc.fig1.path,"DNa02_coplot.png"),
       width = 10, height = 10, dpi = 300)

########################
### Base for Graphic ###
########################

# Plot example DN neuroanatomy
g.anat.brain <- g.anat +
  geom_neuron(x = banc_brain_neuropil.surf,
              cols = c("grey70"),
              rotation_matrix = bancr:::banc_rotation_matrices[["front"]],
              alpha = 1) + 
  geom_neuron(x = banc_decapitate(banc.neuron, invert = TRUE),
              cols = paper.cols[["descending"]],
              rotation_matrix = bancr:::banc_rotation_matrices[["front"]],
              alpha = 1)

# Plot example DN neuroanatomy
g.anat.vnc <- g.anat +
  geom_neuron(x = banc_vnc_neuropil.surf,
              cols = c("grey70"),
              rotation_matrix = bancr:::banc_rotation_matrices[["vnc"]],
              alpha = 1) + 
  geom_neuron(x = banc_decapitate(banc.neuron),
              cols = paper.cols[["descending"]],
              rotation_matrix = bancr:::banc_rotation_matrices[["vnc"]],
              alpha = 1)

# Save the plot
ggsave(plot = g.anat.brain,
       filename = file.path(banc.fig1.path,"dn_brain_graphic.png"),
       width = 10, height = 10, dpi = 300)
ggsave(plot = g.anat.vnc,
       filename = file.path(banc.fig1.path,"dn_vnc_graphic.png"),
       width = 10, height = 10, dpi = 300)

# DNa02 meta data
AN06B011.meta <- franken.meta %>%
  dplyr::filter(cell_type=="AN06B011", side == "left")
AN06B011.fafb.id <- gsub(",","",AN06B011.meta$fafb_id)
AN06B011.manc.id <-  gsub(",","",AN06B011.meta$manc_id)
AN06B011.banc.id <- AN06B011.meta$banc_id
an.banc.neuron <- bancr::banc_read_neuron_meshes(AN06B011.banc.id[1])

# Plot example DN neuroanatomy
g.anat.abrain <- g.anat +
  geom_neuron(x = banc_brain_neuropil.surf,
              cols = c("grey70"),
              rotation_matrix = bancr:::banc_rotation_matrices[["front"]],
              alpha = 1) + 
  geom_neuron(x = banc_decapitate(an.banc.neuron, invert = TRUE),
              cols = paper.cols[["ascending"]],
              rotation_matrix = bancr:::banc_rotation_matrices[["front"]],
              alpha = 1)

# Plot example DN neuroanatomy
g.anat.avnc <- g.anat +
  geom_neuron(x = banc_vnc_neuropil.surf,
              cols = c("grey70"),
              rotation_matrix = bancr:::banc_rotation_matrices[["vnc"]],
              alpha = 1) + 
  geom_neuron(x = banc_decapitate(an.banc.neuron),
              cols = paper.cols[["ascending"]],
              rotation_matrix = bancr:::banc_rotation_matrices[["vnc"]],
              alpha = 1)

# Save the plot
ggsave(plot = g.anat.abrain,
       filename = file.path(banc.fig1.path,"an_brain_graphic.png"),
       width = 10, height = 10, dpi = 300)
ggsave(plot = g.anat.avnc,
       filename = file.path(banc.fig1.path,"an_vnc_graphic.png"),
       width = 10, height = 10, dpi = 300)

# Plot example DN neuroanatomy
g.anat.abrain <- g.anat +
  geom_neuron(x = JRC2018F.surf,
              cols = c("grey70"),
              rotation_matrix = structure(c(0.999742150306702, -0.00396554544568062, 
                                            -0.0223585069179535, 0, -0.00922277569770813, -0.970672488212585, 
                                            -0.240229427814484, 0, -0.0207501109689474, 0.240373536944389, 
                                            -0.970458626747131, 0, 0, 0, 0, 1), dim = c(4L, 4L)),
              alpha = 1) 

# Plot example DN neuroanatomy
g.anat.avnc <- g.anat +
  geom_neuron(x = JRCVNC2018F.surf,
              cols = c("grey70"),
              rotation_matrix = structure(c(0.997295141220093, -0.0176540389657021, 
                                            0.071349173784256, 0, -0.00978143326938152, -0.993970096111298, 
                                            -0.109216496348381, 0, 0.0728468745946884, 0.108223155140877, 
                                            -0.991454064846039, 0, 0, 0, 0, 1), dim = c(4L, 4L)),
              alpha = 1) 

# Save the plot
ggsave(plot = g.anat.abrain,
       filename = file.path(banc.fig1.path,"jrc2018_brain_graphic.png"),
       width = 10, height = 10, dpi = 300)
ggsave(plot = g.anat.avnc,
       filename = file.path(banc.fig1.path,"jrc2018_vnc_graphic.png"),
       width = 10, height = 10, dpi = 300)

################
### Vignette ###
################

# DNp10
AN06B002.neuron <- banc_read_neuron_meshes(banc_latestid(c("720575941498639010",
                                             "720575941459897926",
                                             "720575941603718329",
                                             "720575941454888755",
                                             "720575941653553169",
                                             "720575941564244743")))
DNp10.neuron <- banc_read_neuron_meshes(banc_latestid(c("720575941515678211", "720575941567461678")))
IN12B08.neuron <- banc_read_neuron_meshes(banc_latestid(c("720575941535087594", "720575941589905548", 
                                            "720575941447656270", "720575941651034389", 
                                            "720575941482245318",
                                            "720575941681004477")))
tiMN.neuron <- banc_read_neuron_meshes(banc_latestid(c("720575941519971511",
                                         "720575941583323358",
                                         "720575941562972055",
                                         "720575941478281440",
                                         "720575941468687581",
                                         "720575941432965902",
                                         "720575941470212471",
                                         "720575941488287797",
                                         "720575941493231536",
                                         "720575941497863497",
                                         "720575941605019582",
                                         "720575941546374761",
                                         "720575941525308582")))
SNTa21.neurons <- banc_read_neuron_meshes(banc.meta %>%
                                            dplyr::filter(cell_type=="SNta21") %>%
                                            dplyr::pull(root_id))

# Plot neurooanatomy
g.v.brain <- g.anat +
  geom_neuron(x = banc_brain_neuropil.surf,
              cols = c("grey60", "grey30"),
              rotation_matrix = bancr:::banc_rotation_matrices[["front"]],
              alpha = 0.1) +
  geom_neuron(x = banc_decapitate(AN06B002.neuron, invert = TRUE),
              cols = c(adjust_color_brightness(paper.cols[["ascending"]], 0.7),adjust_color_brightness(paper.cols[["ascending"]], 1.3)),
              rotation_matrix = bancr:::banc_rotation_matrices[["front"]],
              alpha = 0.3) +
  geom_neuron(x = banc_decapitate(DNp10.neuron, invert = TRUE),
              cols = c(adjust_color_brightness(paper.cols[["descending"]], 0.7),adjust_color_brightness(paper.cols[["descending"]], 1.3)),
              rotation_matrix = bancr:::banc_rotation_matrices[["front"]],
              alpha = 0.5)

g.v.vnc <- g.anat +
  geom_neuron(x = banc_vnc_neuropil.surf,
              cols = c("grey60", "grey30"),
              rotation_matrix = bancr:::banc_rotation_matrices[["vnc"]],
              alpha = 0.1) +
  geom_neuron(x = banc_decapitate(AN06B002.neuron, invert = FALSE),
              cols = c(adjust_color_brightness(paper.cols[["ascending"]], 0.7),adjust_color_brightness(paper.cols[["ascending"]], 1.3)),
              rotation_matrix = bancr:::banc_rotation_matrices[["vnc"]],
              alpha = 0.3) +
  geom_neuron(x = banc_decapitate(DNp10.neuron, invert = FALSE),
              cols = c(adjust_color_brightness(paper.cols[["descending"]], 0.7),adjust_color_brightness(paper.cols[["descending"]], 1.3)),
              rotation_matrix = bancr:::banc_rotation_matrices[["vnc"]],
              alpha = 0.5) +
  geom_neuron(x = banc_decapitate(IN12B08.neuron, invert = FALSE),
              cols = c(adjust_color_brightness(paper.cols[["ventral_nerve_cord"]], 0.7),adjust_color_brightness(paper.cols[["ventral_nerve_cord"]], 1.3)),
              rotation_matrix = bancr:::banc_rotation_matrices[["vnc"]],
              alpha = 0.5) +
  geom_neuron(x = banc_decapitate(tiMN.neuron, invert = FALSE),
              cols = c(adjust_color_brightness(paper.cols[["motor"]], 0.9),adjust_color_brightness(paper.cols[["motor"]], 1.1)),
              rotation_matrix = bancr:::banc_rotation_matrices[["vnc"]],
              alpha = 0.5) +
  geom_neuron(x = banc_decapitate(SNTa21.neurons, invert = FALSE),
              cols = c(adjust_color_brightness(paper.cols[["sensory"]], 0.9),adjust_color_brightness(paper.cols[["sensory"]], 1.1)),
              rotation_matrix = bancr:::banc_rotation_matrices[["vnc"]],
              alpha = 0.5)

# Save
ggsave(plot = g.v.brain,
       filename = file.path(banc.fig4.path, "brain_landing_vignette.png"),
       width = 10, height = 10, dpi = 300)
ggsave(plot = g.v.vnc,
       filename = file.path(banc.fig4.path,"vnc_landing_vignette.png"),
       width = 10, height = 10, dpi = 300)




