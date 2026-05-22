#' BANC project session bootstrap
#'
#' Centralised configuration sourced at the top of every figure / text /
#' annotation script. Sets up file paths, GCS bucket pointers, version
#' switches, the per-figure output-path variables (`banc.figN.*`),
#' library loading, palette loading, and the `BANC_NCORES` /
#' `BANC_LIVE` / `BANC_RECALCULATE` env-var overrides.
#'
#' Hard ordering: sourcing this file does NOT trigger a SeaTable hit; the
#' metadata is loaded lazily by `R/startup/banc-meta.R` (dispatcher).
#'
#' Env vars (all optional):
#'   BANC_VERSION       — defaults to "banc_888"; controls dataset slug.
#'   BANC_NCORES=1      — forces sequential influence calculation.
#'   BANC_LIVE=1        — forces banc-meta.R to refresh via SeaTable + GCS.
#'   BANC_RECALCULATE=TRUE — forces panel scripts to recompute caches.
#'
#' Sources:
#'   R/startup/banc-functions.R                                                (helpers + roxygen2-documented fns)
#'   R/startup/gcs-helpers.R                                                    (GCS auth, parquet/feather readers)
#'   R/startup/load-keys.R                                                      (private Drive IDs from data/private/keys.csv)
#'
#' @section Used by:
#'   Every R/figures/panels_*.R, R/text/*.R, and R/annotations/*.R script.
source("R/startup/banc-functions.R")
source("R/startup/gcs-helpers.R")
source("R/startup/load-keys.R")
banc.version <- Sys.getenv("BANC_VERSION", unset = "banc_888")
banc.gcs.bucket <- "gs://lee-lab_brain-and-nerve-cord-fly-connectome/compiled_data"
banc.gcs.dataset <- banc.version
.version_num <- as.integer(sub("^banc_", "", banc.version))
if (is.na(.version_num)) stop("BANC_VERSION must be 'banc_NNN' (e.g. 'banc_850'). Got: ", banc.version)
message("BANC version: ", banc.version, " (", .version_num, ")")

.banc_force_recalculate <- identical(toupper(Sys.getenv("BANC_RECALCULATE", unset = "FALSE")), "TRUE")
if (.banc_force_recalculate) message("BANC_RECALCULATE=TRUE: forcing recalculation")

# Mirror .version_num under the canonical name banc.version.num so figure
# scripts that reference banc.version.num pick up the same integer without
# scattering re-derivations of it. Both names point at the same value.
banc.version.num <- .version_num

.banc_edgelist_cache <- file.path("data", "cache",
  if (.version_num >= 850) paste0(banc.version, "_edgelist_simple_v2.feather")
  else paste0(banc.version, "_simple_edgelist.feather"))

# Spectral clustering producer (bancpipeline) writes
#   spectral_clustering_min_connection_strength_1_banc_version_<NNN>_cluster_count_13_cluster_seed_10_embedding_seed_3_v2.csv
# 2026-05-18: cluster_count 14 -> 13 — k=14 split leg VNC into two unlabeled
# clusters; k=13 collapses them and labels every cluster (including central
# complex related). The producer also moved from Hungarian bijective
# cluster->label matching to plain per-cluster majority vote (allows the same
# OLD label on multiple clusters when a region genuinely splits).
.banc_spectral_csv <- sprintf(
  "data/cns_network/spectral_clustering_min_connection_strength_1_banc_version_%d_cluster_count_13_cluster_seed_10_embedding_seed_3_v2.csv",
  .version_num)

# Betweenness centrality CSVs (v2-flavored).
# Nested under data/betweenness/<version>/ since 2026-05-21 (typo fix +
# per-version subdirectory). Bancpipeline writes here; only the current
# version is checked into the repo.
.banc_betweenness_afferent_csv <- sprintf(
  "data/betweenness/%d/betweenness_afferent_to_efferent_banc_%d_v2.csv",
  .version_num, .version_num)
.banc_betweenness_all_csv <- sprintf(
  "data/betweenness/%d/betweenness_all_to_all_banc_%d_v2.csv",
  .version_num, .version_num)

# Completion / capture-rate CSVs (v2-flavored). Default size threshold = 5;
# override by reading the explicit constants below.
.banc_capture_size_thresh <- 5
.banc_completion_capture_csv <- function(metric, size_thresh = .banc_capture_size_thresh) {
  # metric is one of "gross", "inout", "region", "neuropil"
  if (is.null(size_thresh) || is.na(size_thresh)) {
    sprintf("data/completion/banc_%d_v2_%s_capture_rates.csv", .version_num, metric)
  } else {
    sprintf("data/completion/banc_%d_v2_%s_capture_rates_size_thresh_%d.csv",
            .version_num, metric, size_thresh)
  }
}
.banc_completion_v1_v2_v3_summary_csv <- function(size_thresh = .banc_capture_size_thresh) {
  if (is.null(size_thresh) || is.na(size_thresh)) {
    sprintf("data/completion/banc_%d_v1_v2_v3_summary.csv", .version_num)
  } else {
    sprintf("data/completion/banc_%d_v1_v2_v3_summary_size_thresh_%d.csv",
            .version_num, size_thresh)
  }
}
.banc_completion_v1_v2_v3_region_summary_csv <- function(size_thresh = .banc_capture_size_thresh) {
  if (is.null(size_thresh) || is.na(size_thresh)) {
    sprintf("data/completion/banc_%d_v1_v2_v3_region_summary.csv", .version_num)
  } else {
    sprintf("data/completion/banc_%d_v1_v2_v3_region_summary_size_thresh_%d.csv",
            .version_num, size_thresh)
  }
}
# v3-only artifact — the mean_score → id+proof curve. v2 has no mean_score
# analogue, so these helpers stay pointed at v3 regardless of the consumer
# default (if a panel wants the v3 QC plot it reads these explicitly).
.banc_completion_score_threshold_curve_csv <- sprintf(
  "data/completion/banc_%d_v3_score_threshold_curve.csv", .version_num)
.banc_completion_score_threshold_curve_png <- sprintf(
  "data/completion/banc_%d_v3_score_threshold_curve.png", .version_num)

# Legacy SQLite paths (kept for scripts not yet migrated to GCS)
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
    banc.influence.save.path <- "//research.files.med.harvard.edu/Neurobio/wilsonlab/banc/influence"
    banc.dropbox.influence.save.path <- "C:/Users/Diego/HMS Dropbox/Diego Pacheco Pinedo/neuroanat/influence"
    banc.dropbox.meta.save.path <- "C:/Users/Diego/HMS Dropbox/Diego Pacheco Pinedo/neuroanat/connectomes"
    banc.path <- "C:/Users/papers/BANC-project/"
  } else if (.Platform$OS.type == "unix") {
    banc.meta.save.path <- "/Volumes/wilsonlab/banc/meta"
    banc.connectivity.save.path <- "/Volumes/wilsonlab/banc/connectivity"
    banc.dropbox.connectivity.save.path <- "/Users/diegopinedo/HMS Dropbox/Diego Pacheco Pinedo/neuroanat/connectomes"
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
  suppress(library(glue))
  suppress(library(doMC))
  suppress(library(doParallel))
  suppress(library(progressr))
  suppress(library(elmr))
  suppress(library(dplyr))
  suppress(library(tidyverse))
  suppress(library(bit64))
  suppress(library(reticulate))
  # Suppress pandas FutureWarning spam (added 2026-04-09). Each
  # calculate_influence_py() call emits one warning per invocation; in loops
  # (blowouts, cluster_sensory_correlations) these can flood the log with
  # thousands of identical lines and make diagnostics unreadable. Applied once
  # at startup so every downstream reticulate call inherits the filter.
  tryCatch(
    reticulate::py_run_string(
      "import warnings\nwarnings.filterwarnings('ignore', category=FutureWarning)"
    ),
    error = function(e) {
      message("Could not install Python FutureWarning filter: ", conditionMessage(e))
    }
  )
  # Prevent chromote/webshot2 from eagerly opening a headless Chrome session
  # during library loading or on first use. In non-interactive Rscript runs,
  # stale chromote sessions throw JS TypeError crashes that kill the script.
  # Setting CHROMOTE_CHROME to empty disables Chrome detection; the interactive
  # HTML tool sections already have try() guards so they'll gracefully skip
  # if chromote can't launch. (Added 2026-04-10.)
  Sys.setenv(CHROMOTE_CHROME = "/dev/null")
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
  suppress(library(influencer))
  suppress(library(ggpubr))
  suppress(library(rstatix))
  try(suppress(library(webshot2)))
}, warning = function(w) {
  warning("A warning occurred: ", conditionMessage(w))
}, error = function(e) {
  message("An error occurred: ", conditionMessage(e))
})

# Unload chromote/webshot2 namespaces AFTER library loading (2026-04-10).
# Root cause of the persistent TypeError in panel_efferent_umap.R and
# potentially other scripts: chromote registers C-level finalizers on its
# headless-Chrome session objects. When R's garbage collector fires (e.g.
# the gc() inside query_influence), the finalizer sends a Chrome DevTools
# Protocol command to a dead/broken session, producing:
#   "TypeError: Cannot read properties of undefined (reading '0')"
# This bypasses R's tryCatch because it comes from a C callback, killing
# the R process. Unloading the namespace deregisters the finalizers.
# No figure script needs chromote — interactive HTML tools live in
# R/annotations/rebuild_interactive_tools.R (run separately, not in the pipeline).
for (.pkg in c("webshot2", "chromote")) {
  if (.pkg %in% loadedNamespaces()) {
    tryCatch(unloadNamespace(.pkg),
             error = function(e) message("Could not unload ", .pkg, ": ", e$message))
  }
}

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
  cosine_UMAP1 = readr::col_number(),
  cosine_UMAP2 = readr::col_number(),
  PCA_UMAP1 = readr::col_number(),
  PCA_UMAP2 = readr::col_number(),
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
banc.fig1.darkmode.path <- "figures/figure_1/links/extra/darkmode"
banc.fig2.path <- "figures/figure_2/links/"
banc.fig2.supp.path <- "figures/figure_2/links/supplement"
banc.fig2.anat.path <- "figures/figure_2/links/neuroanatomy"
banc.fig2.extra.path <- "figures/figure_2/links/extra"
banc.fig2.darkmode.path <- "figures/figure_2/links/extra/darkmode"
banc.fig3.path <- "figures/figure_3/links/"
banc.fig3.supp.path <- "figures/figure_3/links/supplement"
banc.fig3.anat.path <- "figures/figure_3/links/neuroanatomy"
banc.fig3.extra.path <- "figures/figure_3/links/extra"
banc.fig3.extra.heatmaps.path <- "figures/figure_3/links/extra/heatmaps"
banc.fig3.darkmode.path <- "figures/figure_3/links/extra/darkmode"
banc.fig4.path <- "figures/figure_4/links/"
banc.fig4.supp.path <- "figures/figure_4/links/supplement"
banc.fig4.anat.path <- "figures/figure_4/links/neuroanatomy"
banc.fig4.extra.path <- "figures/figure_4/links/extra"
banc.fig4.extra.heatmaps.path <- "figures/figure_4/links/extra/heatmaps"
banc.fig4.darkmode.path <- "figures/figure_4/links/extra/darkmode"
banc.fig5.path <- "figures/figure_5/links/"
banc.fig5.supp.path <- "figures/figure_5/links/supplement"
banc.fig5.anat.path <- "figures/figure_5/links/neuroanatomy"
banc.fig5.extra.path <- "figures/figure_5/links/extra"
banc.fig5.extra.heatmaps.path <- "figures/figure_5/links/extra/heatmaps"
banc.fig5.darkmode.path <- "figures/figure_5/links/extra/darkmode"
banc.fig6.path <- "figures/figure_6/links/"
banc.fig6.supp.path <- "figures/figure_6/links/supplement"
banc.fig6.anat.path <- "figures/figure_6/links/neuroanatomy"
banc.fig6.extra.path <- "figures/figure_6/links/extra"
banc.fig6.extra.heatmaps.path <- "figures/figure_6/links/extra/heatmaps"
banc.fig6.darkmode.path <- "figures/figure_6/links/extra/darkmode"

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

cns.functions <- tryCatch({
  bancr::banctable_query(sql = "SELECT * FROM functions") %>%
    dplyr::select(-starts_with("_")) %>%
    dplyr::filter(!is.na(cell_type))
}, error = function(e) {
  warning("Could not load functions table from SeaTable: ", e$message)
  data.frame(cell_type = character(0), modality = character(0),
             behaviour = character(0), response = character(0),
             valence = character(0), stringsAsFactors = FALSE)
})

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
# Read influence threshold from elbow analysis (generated by panels_body_parts.R)
# Falls back to hardcoded value if file not yet generated
.inf_thresh_file <- "data/determined_thresholds/influence_norm_log_elbow_threshold.csv"
if (file.exists(.inf_thresh_file)) {
  threshold.inf.value <- read.csv(.inf_thresh_file)$threshold_value[1]
  message(sprintf("Influence threshold loaded from file: %.4f", threshold.inf.value))
} else {
  threshold.inf.value <- 17.15
  message("Using default influence threshold: 17.15 (run panels_body_parts.R to update)")
}
threshold.sens.inf.value <- 12.14715


# 
# 
# 
