################################
## BANC PROJECT CONFIGURATION ##
################################
# Set up file paths, database connections, and core parameters
# for BANC connectome analysis across different computing environments
source("R/startup/banc-functions.R")
banc.version <- NULL
banc.version <- "banc_626"
banc.connectivity.version <- "banc_626_data.sqlite"
influence.sqlite <- "influence_banc_626.sqlite"
influence.table <- "influence" 
franken.influence.sqlite <- "influence_v_1_6.sqlite"
franken.influence.table <- "frankenbrain_v_1_6_influence"

if(Sys.info()['effective_user']=="ab714"){
  # Run on server
  message("You are Alexander Bates on O2")
  banc.path <- "/n/data1/hms/neurobio/wilson/banc/BANC-project"
  banc.save.path <- "/n/data1/hms/neurobio/wilson/banc"
  banc.obj.save.path <- file.path(banc.save.path,"obj")
  banc.swc.save.path <- file.path(banc.save.path,"swc")
  banc.l2swc.save.path <- file.path(banc.save.path,"l1")
  banc.split.save.path <- file.path(banc.save.path,"split")
  banc.l2split.save.path <- file.path(banc.save.path,"l2split")
  banc.synapses.save.path <- file.path(banc.save.path,"synapses")
  banc.metrics.save.path <- file.path(banc.save.path,"metrics")
  banc.meta.save.path <- file.path(banc.save.path,"meta")
  banc.influence.save.path <- file.path(banc.save.path,"influence")
  banc.connectivity.save.path <- file.path(banc.save.path,"connectivity")
  banc.dropbox.connectivity.save.path <- banc.connectivity.save.path
  banc.dropbox.influence.save.path <- banc.influence.save.path
} else if (Sys.info()['effective_user']=="Diego" | 
           Sys.info()['effective_user']=="diegopinedo" ) {
    message("You are Diego Pacheco")
    if (.Platform$OS.type == "windows") {
    # Run locally
    banc.meta.save.path <- "//research.files.med.harvard.edu/Neurobio/wilsonlab/banc/meta"
    banc.connectivity.save.path <- "//research.files.med.harvard.edu/Neurobio/wilsonlab/banc/connectivity"
    banc.dropbox.connectivity.save.path <- "C:/Users/Diego/HMS Dropbox/Diego Pacheco Pinedo/neuroanat/connectomes"
    banc.dropxbox.influence.save.path <- "C:/Users/Diego/HMS Dropbox/Diego Pacheco Pinedo/neuroanat/influence"
    banc.influence.save.path <- "//research.files.med.harvard.edu/Neurobio/wilsonlab/banc/influence"
    banc.dropbox.influence.save.path <- "C:/Users/Diego/HMS Dropbox/Diego Pacheco Pinedo/neuroanat/influence"
    banc.dropbox.meta.save.path <- "C:/Users/Diego/HMS Dropbox/Diego Pacheco Pinedo/neuroanat/connectomes"
    banc.path <- "C:/Users/papers/BANC-project/"
  } else if (.Platform$OS.type == "unix") {
    banc.meta.save.path <- "/Volumes/wilsonlab/banc/meta"
    banc.connectivity.save.path <- "/Volumes/wilsonlab/banc/connectivity"
    banc.dropbox.connectivity.save.path <- "/Users/diegopinedo/HMS Dropbox/Diego Pacheco Pinedo/neuroanat/connectomes"
    banc.dropxbox.influence.save.path <- "/Users/diegopinedo/HMS Dropbox/Diego Pacheco Pinedo/neuroanat/influence"
    banc.influence.save.path <- "/Volumes/wilsonlab/banc/influence"
    banc.dropbox.influence.save.path <- "/Users/diegopinedo/HMS Dropbox/Diego Pacheco Pinedo/neuroanat/influence"
    banc.dropbox.meta.save.path <- "/Users/diegopinedo/HMS Dropbox/Diego Pacheco Pinedo/neuroanat/connectomes"
    banc.path <- "/Users/papers/BANC-project/"
  }
}else if (Sys.info()['effective_user']=="sophiarenauld"){
  # Run locally
  message("You are Sophia Renauld")
  banc.meta.save.path <- "/Volumes/neurobio/wilsonlab/banc/meta"
  banc.connectivity.save.path <- "/Volumes/neurobio/wilsonlab/banc/connectivity"
  banc.dropbox.connectivity.save.path <- "/Users/sophiarenauld/HMS Dropbox/Sophia Renauld/connectomes"
  banc.dropxbox.influence.save.path <- "/Users/sophiarenauld/HMS Dropbox/Sophia Renauld/influence"
  banc.influence.save.path <- "/Volumes/neurobio/wilsonlab/banc/influence"
  banc.dropbox.influence.save.path <- "/Users/sophiarenauld/HMS Dropbox/Sophia Renauld/influence"
  banc.dropbox.meta.save.path <- "/Users/sophiarenauld/HMS Dropbox/Sophia Renauld/connectomes"
  banc.path <- "/Users/papers/BANC-project/"
}else if(Sys.info()[["user"]]=="abates"){
  # Run locally
  message("You are Alexander Bates")
  banc.meta.save.path <- "/Volumes/neurobio/wilsonlab/banc/meta"
  banc.connectivity.save.path <- "/Volumes/neurobio/wilsonlab/banc/connectivity"
  banc.dropbox.connectivity.save.path <- "/Users/abates/HMS Dropbox/Alexander Bates/neuroanat/connectomes"
  banc.dropxbox.influence.save.path <- "/Users/abates/HMS Dropbox/Alexander Bates/neuroanat/influence"
  banc.influence.save.path <- "/Volumes/neurobio/wilsonlab/banc/influence"
  banc.dropbox.influence.save.path <- "/Users/abates/HMS Dropbox/Alexander Bates/neuroanat/influence"
  banc.dropbox.meta.save.path <- "/Users/abates/HMS Dropbox/Alexander Bates/neuroanat/connectomes"
  banc.path <- "/Users/papers/BANC-project/"
  banc.dropbox.cascade.save.path <- "/Users/abates/HMS Dropbox/Alexander Bates/neuroanat/cascade/frankenbrain_v1.6/"
}else if(Sys.info()[["user"]]=="wclee"){
  # Run locally
  message("You are Wei Lee")
  banc.meta.save.path <- "/Volumes/Neurobio/wilsonlab/banc/meta"
  banc.connectivity.save.path <- "/Volumes/Neurobio/wilsonlab/banc/connectivity"
  banc.dropbox.connectivity.save.path <- "/Users/wclee/HMS Dropbox/Wei-Chung Lee/connectomes"
  banc.dropxbox.influence.save.path <- "/Users/wclee/HMS Dropbox/Wei-Chung Lee/influence"
  banc.influence.save.path <- "/Volumes/Neurobio/wilsonlab/banc/influence"
  banc.dropbox.influence.save.path <- "/Users/wclee/HMS Dropbox/Wei-Chung Lee/influence"
  banc.dropbox.meta.save.path <- "/Users/wclee/HMS Dropbox/Wei-Chung Lee/connectomes"
  banc.path <- "/Users/papers/BANC-project/"
}else if(Sys.info()[["user"]]=="hyang"){
  # Run locally
  message("You are Helen Yang")
  banc.meta.save.path <- "/Volumes/neurobio/wilsonlab/banc/meta"
  banc.connectivity.save.path <- "/Volumes/neurobio/wilsonlab/banc/connectivity"
  banc.dropbox.connectivity.save.path <- "/Users/hyang/HMS Dropbox/Helen Yang/connectomes"
  banc.dropxbox.influence.save.path <- "/Users/hyang/HMS Dropbox/Helen Yang/influence"
  banc.influence.save.path <- "/Volumes/neurobio/wilsonlab/banc/influence"
  banc.dropbox.influence.save.path <- "/Users/hyang/HMS Dropbox/Helen Yang/influence"
  banc.dropbox.meta.save.path <- "/Users/hyang/HMS Dropbox/Helen Yang/connectomes"
  banc.path <- "/Users/papers/BANC-project/"
}

###################
##### OPTIONS #####
###################
options(pillar.sigfig=15)
options(scipen = 999)

#####################
##### LIBRARIES #####
#####################

# load required libraires
library(bancr)
suppress <- function (x, ...) 
{
  suppressWarnings(suppressMessages(x, ...), ...)
}

tryCatch({
  suppress(library(pheatmap))
  suppress(library(hemibrainr))
  suppress(library(malevnc))
  suppress(library(nat.nblast))
  suppress(library(fafbseg))
  suppress(library(jsonlite))
  suppress(library(foreach))
  suppress(library(nat.jrcbrains))
  suppress(library(doMC))
  suppress(library(doParallel))
  suppress(library(progressr))
  suppress(library(elmr))
  suppress(library(dplyr))
  suppress(library(tidyverse))
  suppress(library(bit64))
  suppress(library(reticulate))
  suppress(library(RSQLite))
  suppress(library(plyr))
  suppress(library(slackr))
  suppress(library(ggforce))
  suppress(library(natcpp))
  suppress(library(lubridate))
  suppress(library(doSNOW))
  suppress(library(fs))
  suppress(library(purrr))
  suppress(library(dplyr))
  suppress(library(processx))
  suppress(library(ggpmisc))
  suppress(library(plotly))
  suppress(library(scales))
  suppress(library(ggsankey))
  suppress(library(paletteer))
  suppress(library(kableExtra))
  suppress(library(reshape2))
  suppress(library(ggdendro))
  suppress(library(Matrix))
  suppress(library(tidygraph))
  suppress(library(ggraph))
  suppress(library(igraph))
  suppress(library(progress))
  suppress(library(uwot))
  suppress(library(cowplot))
  suppress(library(dynamicTreeCut))
  suppress(library(formattable))
  suppress(library(htmltools))
  suppress(library(htmlwidgets))
  suppress(library(googlesheets4))
  try(suppress(library(webshot2)))
}, warning = function(w) {
  warning("A warning occurred: ", conditionMessage(w))
}, error = function(e) {
  message("An error occurred: ", conditionMessage(e))
})
nullToNA <- hemibrainr:::nullToNA

# SQL column types
inf.col.types <- readr::cols(
  id = readr::col_character(),
  is_seed = readr::col_logical(),
  .default = readr::col_number())

# SQL column types
banc.col.types <- readr::cols(
  .default = readr::col_character(),
  cleft_segid  = readr::col_character(),
  centroid_x = readr::col_number(),
  centroid_y = readr::col_number(),
  centroid_z = readr::col_number(),
  bbox_bx = readr::col_number(),
  bbox_by = readr::col_number(),
  bbox_bz = readr::col_number(),
  bbox_ex = readr::col_number(),
  bbox_ey = readr::col_number(),
  bbox_ez = readr::col_number(),
  presyn_segid = readr::col_character(),
  postsyn_segid  = readr::col_character(),
  presyn_x = readr::col_integer(),
  presyn_y = readr::col_integer(),
  presyn_z = readr::col_integer(),
  postsyn_x = readr::col_integer(),
  postsyn_y = readr::col_integer(),
  postsyn_z = readr::col_integer(),
  clefthash = readr::col_number(),
  partnerhash = readr::col_integer(),
  size = readr::col_number(),
  l2_root = readr::col_number(),
  l2_nodes = readr::col_number(),
  l2_segments = readr::col_number(),
  l2_branchpoints = readr::col_number(),
  l2_endpoints = readr::col_number(),
  l2_cable_length = readr::col_number(),
  l2_n_trees = readr::col_number(),
  nb = readr::col_number(),
  score = readr::col_number(),
  hemibrain_nblast = readr::col_number(),
  fafb_nblast = readr::col_number(),
  manc_nblast = readr::col_number(),
  X = readr::col_number(),
  Y = readr::col_number(),
  Z = readr::col_number(),
  x = readr::col_number(),
  y = readr::col_number(),
  z = readr::col_number(),
  dcv_density = readr::col_number(),
  dcv_count = readr::col_integer(),
  acetylcholine= readr::col_number() , 
  glutamate= readr::col_number() , 
  gaba= readr::col_number() , 
  glycine= readr::col_number() , 
  dopamine= readr::col_number() , 
  serotonin= readr::col_number() , 
  octopamine= readr::col_number() , 
  tyramine= readr::col_number() , 
  histamine= readr::col_number() , 
  nitric_oxide= readr::col_number() ,
  `allatostatin-a`= readr::col_number() , 
  `allatostatin-c`= readr::col_number() , 
  amnesiac= readr::col_number() , 
  bursicon= readr::col_number() , 
  capability= readr::col_number() , 
  ccap= readr::col_number() , 
  ccha1= readr::col_number() , 
  cnma= readr::col_number() , 
  corazonin= readr::col_number() , 
  darc1= readr::col_number() , 
  dh31= readr::col_number() , 
  dh331= readr::col_number() , 
  dh44= readr::col_number() , 
  dilp2= readr::col_number() , 
  dilp3= readr::col_number() , 
  dilp5= readr::col_number() , 
  dnpf= readr::col_number() , 
  drosulfakinin= readr::col_number() , 
  eclosion_hormone= readr::col_number() , 
  fmrf= readr::col_number() , 
  fmrfa= readr::col_number() , 
  hugin= readr::col_number() ,
  itp= readr::col_number() , 
  leucokinin= readr::col_number() , 
  mip= readr::col_number() , 
  myosuppressin= readr::col_number() , 
  myosupressin= readr::col_number() , 
  natalisin= readr::col_number() , 
  negative= readr::col_number() , 
  neuropeptide= readr::col_number() , 
  neuropeptides= readr::col_number() , 
  npf= readr::col_number() , 
  nplp1= readr::col_number() , 
  orcokinin= readr::col_number() , 
  pdf= readr::col_number() , 
  proctolin= readr::col_number() , 
  sifamide= readr::col_number() , 
  snpf= readr::col_number() , 
  space_blanket= readr::col_number() , 
  tachykinin= readr::col_number() , 
  trissin= readr::col_number(),
  input_connections = readr::col_number(),
  output_connections = readr::col_number(),
  total_connections = readr::col_number(),
  UMAP1 = readr::col_number(),
  UMAP2 = readr::col_number(),
  umap_x = readr::col_number(),
  umap_y = readr::col_number(),
  prepost = readr::col_number(),
  l2_cable_length_um  = readr::col_number(),
  l2_nodes  = readr::col_number(),
  input_side_index = readr::col_number(),
  output_side_index = readr::col_number(),
  pd_width = readr::col_number(),
  segregation_index = readr::col_number(),
  neurotransmitter_probability = readr::col_number(),
  neurotransmitter_predicted = readr::col_character(),
  count = readr::col_integer(),
  count_valid = readr::col_integer(),
  in_group = readr::col_logical()
)

#############
### PATHS ###
#############

# paths
banc.fig1.path <- "figures/figure_1/links/"
banc.fig1.supp.path <- "figures/figure_1/links/supplement"
banc.fig1.extra.path <- "figures/figure_1/links/extra"
banc.fig2.path <- "figures/figure_2/links/"
banc.fig2.supp.path <- "figures/figure_2/links/supplement"
banc.fig2.anat.path <- "figures/figure_2/links/neuroanatomy"
banc.fig2.extra.path <- "figures/figure_2/links/extra"
banc.fig3.path <- "figures/figure_3/links/"
banc.fig3.supp.path <- "figures/figure_3/links/supplement"
banc.fig3.anat.path <- "figures/figure_3/links/neuroanatomy"
banc.fig3.extra.path <- "figures/figure_3/links/extra"
banc.fig3.extra.heatmaps.path <- "figures/figure_3/links/extra/heatmaps"
banc.fig4.path <- "figures/figure_4/links/"
banc.fig4.supp.path <- "figures/figure_4/links/supplement"
banc.fig4.anat.path <- "figures/figure_4/links/neuroanatomy"
banc.fig4.extra.path <- "figures/figure_4/links/extra"
banc.fig4.extra.heatmaps.path <- "figures/figure_4/links/extra/heatmaps"
banc.fig5.path <- "figures/figure_5/links/"
banc.fig5.supp.path <- "figures/figure_5/links/supplement"
banc.fig5.anat.path <- "figures/figure_5/links/neuroanatomy"
banc.fig5.extra.path <- "figures/figure_5/links/extra"
banc.fig5.extra.heatmaps.path <- "figures/figure_5/links/extra/heatmaps"
banc.fig6.path <- "figures/figure_6/links/"
banc.fig6.supp.path <- "figures/figure_6/links/supplement"
banc.fig6.anat.path <- "figures/figure_6/links/neuroanatomy"
banc.fig6.extra.path <- "figures/figure_6/links/extra"
banc.fig6.extra.heatmaps.path <- "figures/figure_6/links/extra/heatmaps"

###############
### Colours ###
###############

paper.cols.df <- read.csv("settings/paper_colours_lacroix.csv") %>%
  dplyr::distinct(label, .keep_all = TRUE)
paper.cols <- paper.cols.df$hex
names(paper.cols) <- paper.cols.df$label
class.order <- unique(names(paper.cols))

# Create a function to generate n colors
# Original base colors
cerise_limon_base <- c("#ee3232", "#EE5B32", "#F6B83C", "#4BA747", "#47a789", "#5BB6E4", "#7C378A")

# Expanded color palette with more discriminable colors
cerise_limon_expanded <- c(
  # Browns, from darkest to lightest
  "#695b52", # dark brown 
  "#8f7059", # forest/wood brown
  "#9e958a", # latte brown
  "#c9b89f", # creamy beige
  
  # Reds, from darkest to brightest
  "#D1512D", # Deeper orange-red
  "#D92929", # Darker red
  "#ee3232", # Original red
  "#FF4747", # Brighter red
  
  # Oranges
  "#EE5B32", # Original orange-red
  "#FF7242", # Brighter orange
  
  # Yellows, from darkest to lightest
  "#E6A82C", # Deeper yellow
  "#F6B83C", # Original yellow
  "#FFCB45", # Brighter yellow
  "#F8D070", # Lighter yellow
  
  # Greens/Teals, darkest to lightest
  "#389636", # Darker green
  "#4BA747", # Original green
  "#66C261", # Lighter green
  "#47a789", # Original teal-green
  
  # Blues, darkest to lightest
  "#2889BA", # Deep blue
  "#3A9FD1", # Darker blue
  "#5BB6E4", # Original blue
  "#7DCCF5", # Lighter blue
  
  # Purples, darkest to lightest
  "#652673", # Deeper purple
  "#7C378A", # Original purple
  "#963DA8", # Brighter purple
  "#B56FCC"  # Lighter purple
)

#' LaCroix-inspired color scales for ggplot2
#' 
#' A set of color scales based on a LaCroix-inspired palette
#' 
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#' 
#' @return A ggproto object defining a color scale for ggplot2
#' @export
#' @examples
#' library(ggplot2)
#' 
#' # For discrete data
#' ggplot(iris, aes(x = Sepal.Width, y = Sepal.Length, color = Species)) +
#'   geom_point(size = 3) +
#'   scale_color_cerise_limon()
#'   
#' # For continuous data
#' ggplot(iris, aes(x = Sepal.Width, y = Sepal.Length, color = Sepal.Length)) +
#'   geom_point(size = 3) +
#'   scale_color_cerise_limon(discrete = FALSE)
scale_color_cerise_limon <- function(discrete = TRUE, ...) {
  if (discrete) {
    ggplot2::discrete_scale("colour", "cerise_limon", 
                            palette = cerise_limon_discrete_palette, ...)
  } else {
    ggplot2::scale_color_gradientn(colors = cerise_limon_expanded, ...)
  }
}

#' @rdname scale_color_cerise_limon
#' @export
scale_colour_cerise_limon <- scale_color_cerise_limon

#' @rdname scale_color_cerise_limon
#' @export
scale_fill_cerise_limon <- function(discrete = TRUE, ...) {
  if (discrete) {
    ggplot2::discrete_scale("fill", "cerise_limon", 
                            palette = cerise_limon_discrete_palette, ...)
  } else {
    ggplot2::scale_fill_gradientn(colors = cerise_limon_expanded, ...)
  }
}

# Helper function to create discrete palette
cerise_limon_discrete_palette <- function(n) {
  if (n <= length(cerise_limon_expanded)) {
    # For small n, pick evenly spaced colors from the expanded palette
    colors <- cerise_limon_expanded[seq(1, length(cerise_limon_expanded), 
                                        length.out = n)]
  } else {
    # For larger n, use the colorRampPalette function
    colors <- grDevices::colorRampPalette(cerise_limon_expanded)(n)
  }
  return(colors)
}

# For backward compatibility with existing code
cerise_limon_palette <- grDevices::colorRampPalette(cerise_limon_expanded)

#####################
### COLOUR SCALES ###
#####################
highlight.col <- paper.cols[["highlight"]] # "#EE4244"
highlight.col2 <-  paper.cols[["accent"]] # "#FC6882"
  
# Colour breaks
n_breaks <- 200

# Logarithmic color scale
connection_heatmap_breaks <- seq(0, 
                                    500, 
                                    length.out = 101)
connection_heatmap_palette <- colorRampPalette(rev(c("grey20", "grey35", "grey30", "grey40", "grey60", "white")))(n_breaks-1)

# Create cosine color palette
cosine_heatmap_breaks <- seq(0, 1, length.out = n_breaks)
cosine_heatmap_palette <- colorRampPalette(c("#007BC3", "white",  "#D72000"))(n_breaks - 1)

# Create scaled color palette
scaled_heatmap_breaks <- seq(-5, 5, length.out = n_breaks)
scaled_heatmap_palette <- colorRampPalette(c("#0054c3","#007BC3", "#58b6ed","grey90","#f08c7a","#D72000","#D72000"))(n_breaks - 1)

##############################################
##### Functional annotations for neurons #####
##############################################

cns.functions <- bancr::banctable_query(sql = "SELECT * FROM functions") %>%
  dplyr::select(-starts_with("_")) %>%
  dplyr::filter(!is.na(cell_type))

#########################
##### ANNOUNCEMENTS #####
#########################

# Messages for debugging
message("R_MAX_VSIZE: ", Sys.getenv("R_MAX_VSIZE"))
message(".libPaths: ", print(.libPaths()))
message("bancr: ", packageVersion("bancr"))
print("##### SESSION INFO #####")
print(sessionInfo())
print("##### SESSION INFO #####")

# Constant to add to log score
log.const <- 0.0000000000000000000000001
inf.threshold <- 0.000000002
inf.threshold <- 0.00000000015
inf.norm.threshold <- 0.0000000001
threshold.inf.value <- 15.903
threshold.sens.inf.value <- 12.14715

########################
##### PLOT COLOURS #####
########################

# # Read the CSV file
# colors_df <- read.csv("settings/paper_colours_lacroix.csv")
# 
# # Clean and prepare the data
# colors_clean <- colors_df %>%
#   # Remove duplicates (keep first occurrence of each label-hex combination)
#   distinct(label, hex, type, .keep_all = TRUE) %>%
#   # Clean up labels for better display
#   mutate(
#     label_clean = str_replace_all(label, "_", " "),
#     label_clean = str_to_title(label_clean)
#   ) %>%
#   # Order types by frequency for better layout
#   mutate(type = fct_infreq(type))
# 
# # Create a function to determine text color based on background
# get_text_color <- function(hex_color) {
#   # Convert hex to RGB
#   rgb_vals <- col2rgb(hex_color)
#   # Calculate luminance
#   luminance <- (0.299 * rgb_vals[1] + 0.587 * rgb_vals[2] + 0.114 * rgb_vals[3]) / 255
#   # Return white for dark backgrounds, black for light backgrounds
#   ifelse(luminance < 0.5, "white", "black")
# }
# 
# # Add text color column
# colors_clean$text_color <- sapply(colors_clean$hex, get_text_color)
# 
# # Create the main visualization
# p1 <- ggplot(colors_clean, aes(x = fct_reorder(label_clean, hex), y = 1)) +
#   geom_tile(fill = colors_clean$hex, color = "white", size = 0.5) +
#   geom_text(aes(label = label_clean), 
#             color = colors_clean$text_color, 
#             size = 2.5, 
#             angle = 45, 
#             hjust = 1, 
#             vjust = 0.5) +
#   facet_wrap(~type, scales = "free_x", ncol = 3) +
#   labs(
#     title = "Color Mapping by Type",
#     subtitle = "Hex codes mapped to labels, grouped by category",
#     x = NULL,
#     y = NULL
#   ) +
#   theme_minimal() +
#   theme(
#     axis.text.x = element_blank(),
#     axis.text.y = element_blank(),
#     axis.ticks = element_blank(),
#     panel.grid = element_blank(),
#     strip.text = element_text(face = "bold", size = 12),
#     plot.title = element_text(size = 16, face = "bold"),
#     plot.subtitle = element_text(size = 12),
#     plot.margin = margin(20, 20, 20, 20)
#   )
# 
# # Create a summary table
# summary_stats <- colors_clean %>%
#   group_by(type) %>%
#   summarise(
#     count = n(),
#     unique_colors = n_distinct(hex),
#     .groups = 'drop'
#   ) %>%
#   arrange(desc(count))
# 
# print("Summary of color mappings by type:")
# print(summary_stats)
# 
# # Create a color palette visualization for each type
# create_palette_plot <- function(type_name) {
#   type_data <- colors_clean %>% filter(type == type_name)
#   
#   ggplot(type_data, aes(x = 1:nrow(type_data), y = 1)) +
#     geom_tile(fill = type_data$hex, width = 0.9, height = 0.8) +
#     geom_text(aes(label = paste0(label_clean, "\n", hex)), 
#               color = type_data$text_color, 
#               size = 2.5) +
#     labs(title = paste("Color Palette:", str_to_title(type_name)),
#          x = NULL, y = NULL) +
#     theme_void() +
#     theme(
#       plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
#       axis.text = element_blank(),
#       axis.ticks = element_blank()
#     ) +
#     coord_fixed()
# }
# 
# # Create individual palette plots for each type (uncomment to generate)
# # unique_types <- unique(colors_clean$type)
# # for(type_name in unique_types) {
# #   p <- create_palette_plot(type_name)
# #   print(p)
# # }
# 
# # Alternative compact visualization showing hex codes
# p2 <- colors_clean %>%
#   group_by(type) %>%
#   mutate(row_num = row_number()) %>%
#   ggplot(aes(x = row_num, y = fct_rev(type))) +
#   geom_tile(fill = colors_clean$hex, color = "white", size = 0.3) +
#   geom_text(aes(label = str_sub(hex, 2, 7)), 
#             color = colors_clean$text_color, 
#             size = 1.8) +
#   labs(
#     title = "Hex Color Codes by Type",
#     subtitle = "Each tile shows the actual color with its hex code",
#     x = "Color Index",
#     y = "Type"
#   ) +
#   theme_minimal() +
#   theme(
#     panel.grid = element_blank(),
#     plot.title = element_text(size = 16, face = "bold"),
#     plot.subtitle = element_text(size = 12),
#     axis.text.y = element_text(size = 10),
#     axis.text.x = element_text(size = 8)
#   )
# 
# # Save
# ggsave(plot = p1,
#        filename = file.path("settings", "paper_colours_1.png"),
#        width = 12, height = 12, dpi = 300)
# ggsave(plot = p2,
#        filename = file.path("settings", "paper_colours_2.png"),
#        width = 12, height = 12, dpi = 300)




