##############################
## NEUROGLANCER LINK BUILDER ##
##############################
# Generate URLs for 3D neuron visualisation in Neuroglancer and Codex
# Creates interactive links for paper supplementary materials

# Generate Codex network visualisation URLs for specified cell types
banc_codex_network <- function(cell.types = NULL,
                                ids = NULL,
                                codex.url = "https://codex.flywire.ai/app/connectivity?dataset=banc",
                                open = FALSE,
                                min_syn_cnt = 3,
                                edge_syn_cap = 50){
  if(!is.null(cell.types)){
    cell.types.search <- paste(cell.types,collapse="+%7C%7C+cell_type+%3D%3D+")
    cell.types.search <- paste0("cell_type+%3D%3D+",gsub("\\+\\%7C\\%7C\\+cell_type\\+\\%3D\\%3D\\+$","",cell.types.search))
    cell.types.search <- paste(cell.types,collapse="+%7C%7C+cell_type+%3D%3D+")
    cell.types.search <- paste0("cell_type+%3D%3D+",gsub("\\+\\%7C\\%7C\\+cell_type\\+\\%3D\\%3D\\+$","",cell.types.search))
  }
  if(!is.null(ids)){
    ids.search <- paste(ids,collapse="+%7C%7C+root_id+%3D%3D+")
    ids.search <- paste0("root_id+%3D%3D+",gsub("\\+\\%7C\\%7C\\+root_id\\+\\%3D\\%3D\\+$","",ids.search))
    ids.search <- paste(ids,collapse="+%7C%7C+root_id+%3D%3D+")
    ids.search <- paste0("root_id+%3D%3D+",gsub("\\+\\%7C\\%7C\\+root_id\\+\\%3D\\%3D\\+$","",ids.search))
  }
  if(is.null(ids)&!is.null(cell.types)){
    search <- cell.types.search 
  }else if(!is.null(ids)&is.null(cell.types)){
    search <- ids.search 
  }else if(!is.null(ids)&!is.null(cell.types)){
    search <- paste(ids.search,"+%7C%7C+",cell.types.search)
  }else{
    stop("please provide argument cell.types or ids")
  }
  url <- sprintf("%s&cell_names_or_ids=%s&download=&group_by=type&edge_filter=all&cap=%d&min_syn_cnt=%d",
                 codex.url,
                 search,
                 edge_syn_cap,
                 min_syn_cnt)
  if(open){
    utils::browseURL(url)
    invisible()
  }else{
    url 
  }
}

# BANC codex search
banc_codex_search <- function(cell.types = NULL,
                               ids = NULL,
                               codex.url = "https://codex.flywire.ai/app/search?dataset=banc",
                               open = FALSE,
                              page.size = 100){
  if(!is.null(cell.types)){
    cell.types.search <- paste(cell.types,collapse="+%7C%7C+cell_type+%3D%3D+")
    cell.types.search <- paste0("cell_type+%3D%3D+",gsub("\\+\\%7C\\%7C\\+cell_type\\+\\%3D\\%3D\\+$","",cell.types.search))
    cell.types.search <- paste(cell.types,collapse="+%7C%7C+cell_type+%3D%3D+")
    cell.types.search <- paste0("cell_type+%3D%3D+",gsub("\\+\\%7C\\%7C\\+cell_type\\+\\%3D\\%3D\\+$","",cell.types.search))
  }
  if(!is.null(ids)){
    ids.search <- paste(ids,collapse="+%7C%7C+root_id+%3D%3D+")
    ids.search <- paste0("root_id+%3D%3D+",gsub("\\+\\%7C\\%7C\\+root_id\\+\\%3D\\%3D\\+$","",ids.search))
    ids.search <- paste(ids,collapse="+%7C%7C+root_id+%3D%3D+")
    ids.search <- paste0("root_id+%3D%3D+",gsub("\\+\\%7C\\%7C\\+root_id\\+\\%3D\\%3D\\+$","",ids.search))
  }
  if(is.null(ids)&!is.null(cell.types)){
    search <- cell.types.search 
  }else if(!is.null(ids)&is.null(cell.types)){
    search <- ids.search 
  }else if(!is.null(ids)&!is.null(cell.types)){
    search <- paste(ids.search,"+%7C%7C+",cell.types.search)
  }else{
    stop("please provide argument cell.types or ids")
  }
  url <- sprintf("%s&filter_string=%s&sort_by=&page_size=%d",
                 codex.url,
                 search,
                 page.size)
  if(open){
    utils::browseURL(url)
    invisible()
  }else{
    url 
  }
}


###############
### STARTUP ###
###############

# load
source("R/startup/banc-startup.R")
banc.meta <- banctable_query() %>%
  dplyr::filter(!super_class%in%c("glia","trachea","not_a_neuron"))

# spreadsheet for root_ids for vignettes
vignette_spreadsheet <- "https://docs.google.com/spreadsheets/d/11Y-ojAfcXHS1gjl9slwthNigIejJ_v_PhhqYTylw3Cs/edit?usp=sharing"

# Starter links
starter.links <-  c("https://spelunker.cave-explorer.org/#!middleauth+https://global.daf-apis.com/nglstate/api/v1/6615245819740160", 
                           "https://spelunker.cave-explorer.org/#!middleauth+https://global.daf-apis.com/nglstate/api/v1/6590872282988544",
                           "https://spelunker.cave-explorer.org/#!middleauth+https://global.daf-apis.com/nglstate/api/v1/5501382827180032", 
                           "https://spelunker.cave-explorer.org/#!middleauth+https://global.daf-apis.com/nglstate/api/v1/5251862407151616")
for(l in 1:length(starter.links)){
  url <- sub("#!middleauth+", "?", starter.links[l], fixed = T)
  parts <- unlist(strsplit(url, "?", fixed = T))
  json <- try(fafbseg::flywire_fetch(parts[2], token = bancr:::banc_token(),
                                     return = "text", cache = TRUE))
  new.url <- ngl_encode_url(json, baseurl = parts[1])
  starter.links[l] <- new.url
}
# Strucute
ngl.df <- data.frame(
  entry = c("neck-connective", 
            "example-synapse", 
            "example-nuclei", 
            "example-mitochondria"),
  ngl_link = starter.links,
  codex_link = c(NA,NA,NA,NA)
)

################
### FIGURE 1 ###
################

# Figure 1 DNa02
data <- banc.meta %>%
  dplyr::filter(cell_type=="DNa01")
ngl.link <- bancr::bancsee(
  url = NULL, short = FALSE,
                    banc_static_ids = c(na.omit(data$root_626))
                       #manc_ids  = (na.omit(data$manc_match)),
                       #fafb_ids = (na.omit(data$fafb_match)),
                       #hemibrain_ids = (na.omit(data$hemibrain_match))
                       )
ngl.link <- rbind(ngl.df,
                data.frame(entry = "DNa02",
                           ngl_link = ngl.link,
                           codex_link = banc_codex_search(cell.type="DNa02")))

# Figure 1 LB1a
data <- banc.meta %>%
  dplyr::filter(cell_type=="LB1a")
ngl.link <- bancsee(
                    banc_static_ids = c(na.omit(data$root_626))
                    #manc_ids  = (na.omit(data$manc_match)),
                    #fafb_ids = (na.omit(data$fafb_match)),
                    #hemibrain_ids = (na.omit(data$hemibrain_match))
                    )
ngl.df <- rbind(ngl.df,
                data.frame(entry = "LB1a",
                           ngl_link = ngl.link,
                           codex_link = banc_codex_search(cell.type="LB1a")))

# Figure 1 LB1a
data <- banc.meta %>%
  dplyr::filter(cell_type=="DVM1a-c")
ngl.link <- bancsee(
                    banc_static_ids = c(na.omit(data$root_626)),
                    #manc_ids  = (na.omit(data$manc_match)),
                    #fafb_ids = (na.omit(data$fafb_match)),
                    #fhemibrain_ids = (na.omit(data$hemibrain_match))
                    )
ngl.df <- rbind(ngl.df,
                data.frame(entry = "DVM1ac",
                           ngl_link = ngl.link,
                           codex_link = banc_codex_search(cell.type="DVM1a-c")))

################
### FIGURE 2 ###
################

# Figure 2h
sheet_name <- "Figure2h"
fig2h <- googlesheets4::read_sheet(vignette_spreadsheet, sheet = sheet_name, col_types = "cccc")
fig2h_ids <- na.omit(fig2h$root_id)

data <- banc.meta %>%
#  dplyr::filter(grepl("DNpe013|AN19B025|DproN|CvN4-7|CvN1-3|CB0810",cell_type))
  dplyr::filter(root_626 %in% fig2h_ids)
banc.cols <- paper.cols[data$super_class]
banc.cols[is.na(banc.cols)] <- "#FFFFFF"
ngl.link <- bancsee(
                    banc_static_ids = c(na.omit(data$root_626)),
                    banc.cols = banc.cols)
ngl.df <- rbind(ngl.df,
                data.frame(entry = "figure-2h",
                           ngl_link = ngl.link,
                           codex_link = banc_codex_network(cell.types  = unique(na.omit(fig2h$cell_type)))))

# Figure 2g ascending
data <- banc.meta %>%
  dplyr::filter(super_class=="ascending")
ngl.link <- bancsee(
                    banc_static_ids = c(na.omit(data$root_626)),
                    banc.cols = banc.cols)
ngl.df <- rbind(ngl.df,
                data.frame(entry = "ascending",
                           ngl_link = ngl.link,
                           codex_link = banc_codex_search(cell.types  = unique(na.omit(data$cell_type)))))

# Figure 2g descending
data <- banc.meta %>%
  dplyr::filter(super_class=="descending")
ngl.link <- bancsee(
                    banc_static_ids = c(na.omit(data$root_626)),
                    banc.cols = banc.cols)
ngl.df <- rbind(ngl.df,
                data.frame(entry = "descending",
                           ngl_link = ngl.link,
                           codex_link = banc_codex_search(cell.types  = unique(na.omit(data$cell_type)))))

# Figure 2i
data <- banc.meta %>%
  dplyr::filter(grepl("EFF",cluster))
banc.cols <- paper.cols[data$cluster]
ngl.link <- bancsee(
                    banc_static_ids = c(na.omit(data$root_626)),
                    banc.cols = banc.cols)
ngl.df <- rbind(ngl.df,
                data.frame(entry = "efferent_clusters",
                           ngl_link = ngl.link,
                           codex_link = banc_codex_search(cell.types  = unique(na.omit(data$cell_type)))))

# Figure 2d
data <- banc.meta %>%
  dplyr::filter(!is.na(body_part_effector))
banc.cols <- paper.cols[data$body_part_effector]
banc.cols[is.na(banc.cols)] <- "#FFFFFF"
ngl.link <- bancsee(
                    banc_static_ids = c(na.omit(data$root_626)),
                    banc.cols = banc.cols)
ngl.df <- rbind(ngl.df,
                data.frame(entry = "efferent_body_parts",
                           ngl_link = ngl.link,
                           codex_link = banc_codex_search(cell.types  = unique(na.omit(data$cell_type)))))

################
### FIGURE 3 ###
################

# Figure UMAP
data <- banc.meta %>%
  dplyr::filter(super_class %in% c("ascending","descending")) %>%
  dplyr::filter(!is.na(super_cluster))
banc.cols <- paper.cols[data$super_cluster]
banc.cols[is.na(banc.cols)] <- "#FFFFFF"
ngl.link <- bancsee(
                    banc_static_ids = c(na.omit(data$root_626)),
                    banc.cols = banc.cols)
ngl.df <- rbind(ngl.df,
                data.frame(entry = "an_dn_super_clusters",
                           ngl_link = ngl.link,
                           codex_link = NA))

# Each super cluster
for(spc in na.omit(unique(banc.meta$super_cluster))){
  data <- banc.meta %>%
    dplyr::filter(super_cluster==spc)
  banc.cols <- paper.cols[data$super_class]
  banc.cols[is.na(banc.cols)] <- "#FFFFFF"
  ngl.link <- bancsee(
                      banc_static_ids = c(na.omit(data$root_626)),
                      banc.cols = banc.cols)
  ngl.df <- rbind(ngl.df,
                  data.frame(entry = spc,
                             ngl_link = ngl.link,
                             codex_link = banc_codex_search(cell.types  = unique(na.omit(data$cell_type)))))
}

# Figure 3g
sheet_name <- "Figure3g"
fig3g <- googlesheets4::read_sheet(vignette_spreadsheet, sheet = sheet_name, col_types = "cccc")
fig3g_ids <- na.omit(fig3g$root_id)
data <- banc.meta %>%
#  dplyr::filter(grepl("m_NSC_DILP|AN27X017|DNp65|MNad21|CB0991|SAxx01|ISN|BiT",cell_type))
  dplyr::filter(root_626 %in% fig3g_ids)
banc.cols <- paper.cols[data$super_class]
banc.cols[is.na(banc.cols)] <- "#FFFFFF"
ngl.link <- bancsee(
                    banc_static_ids = c(na.omit(data$root_626)),
                    banc.cols = banc.cols)
ngl.df <- rbind(ngl.df,
                data.frame(entry = "figure-3g",
                           ngl_link = ngl.link,
                           codex_link = banc_codex_network(cell.types  = unique(na.omit(data$cell_type)))))

# Figure 3h
sheet_name <- "Figure3h"
fig3h <- googlesheets4::read_sheet(vignette_spreadsheet, sheet = sheet_name, col_types = "cccc")
fig3h_ids <- na.omit(fig3h$root_id)

data <- banc.meta %>%
#  dplyr::filter(grepl("DNg27|l_NSC_CRZ|ANXXX139|DLM5|DLM1-4|ISN",cell_type))
  dplyr::filter(root_626 %in% fig3h_ids)
banc.cols <- paper.cols[data$super_class]
banc.cols[is.na(banc.cols)] <- "#FFFFFF"
ngl.link <- bancsee(
                    banc_static_ids = c(na.omit(data$root_626)),
                    banc.cols = banc.cols)
ngl.df <- rbind(ngl.df,
                data.frame(entry = "figure-3h",
                           ngl_link = ngl.link,
                           codex_link = banc_codex_network(cell.types  = unique(na.omit(data$cell_type)))))

# Figure 3i
sheet_name <- "Figure3i"
fig3i <- googlesheets4::read_sheet(vignette_spreadsheet, sheet = sheet_name, col_types = "cccc")
fig3i_ids <- na.omit(fig3i$root_id)

data <- banc.meta %>%
#  dplyr::filter(grepl("pC1a|SAG|oviDNa_a|DNp37|ANXXX986|^SPSN$|^PU$|^CMU$",cell_type))
  dplyr::filter(root_626 %in% fig3i_ids)
banc.cols <- paper.cols[data$super_class]
banc.cols[is.na(banc.cols)] <- "#FFFFFF"
ngl.link <- bancsee(
                    banc_static_ids = c(na.omit(data$root_626)),
                    banc.cols = banc.cols)
ngl.df <- rbind(ngl.df,
                data.frame(entry = "figure-3i",
                           ngl_link = ngl.link,
                           codex_link = banc_codex_network(cell.types  = unique(na.omit(data$cell_type)))))

# Figure 3j
sheet_name <- "Figure3j"
fig3j <- googlesheets4::read_sheet(vignette_spreadsheet, sheet = sheet_name)
fig3j_ids <- na.omit(unlist(fig3j$root_id))
data <- banc.meta %>%
#  dplyr::filter(grepl("DNge104|AN08B023|AN05B056|AN09A007|SNta02|SNta07|SNta11|BM_lnOm|BM_vib|BM_ant|SNta11|SNta20|SNta11|SNta20|SNxx01|SNxx03|SNxx04|SNxx05",cell_type))
  dplyr::filter(root_626 %in% fig3j_ids)
banc.cols <- paper.cols[data$super_class]
banc.cols[is.na(banc.cols)] <- "#FFFFFF"
ngl.link <- bancsee(
                    banc_static_ids = c(na.omit(data$root_626)),
                    banc.cols = banc.cols)
ngl.df <- rbind(ngl.df,
                data.frame(entry = "figure-3j",
                           ngl_link = ngl.link,
                           codex_link = banc_codex_network(cell.types  = unique(na.omit(data$cell_type)))))

# Figure 3k
sheet_name <- "Figure3k"
fig3k <- googlesheets4::read_sheet(vignette_spreadsheet, sheet = sheet_name)
fig3k_ids <- unlist(na.omit(fig3k$root_id))
data <- banc.meta %>%
  dplyr::filter(root_626 %in% fig3k_ids)
banc.cols <- paper.cols[data$super_class]
banc.cols[is.na(banc.cols)] <- "#FFFFFF"
ngl.link <- bancsee( banc_static_ids = c(na.omit(data$root_626)),
                    banc.cols = banc.cols)
ngl.df <- rbind(ngl.df,
                data.frame(entry = "figure-3k",
                           ngl_link = ngl.link,
                           codex_link = banc_codex_network(cell.types  = unique(na.omit(data$cell_type)))))
################
### FIGURE 4 ###
################

# Figure head and eye orienting
data <- banc.meta %>%
  dplyr::filter(super_class %in% c("ascending","descending")) %>%
  dplyr::filter(super_cluster=="head and eye orienting")
ngl.link <- bancsee(
                    banc_static_ids = c(na.omit(data$root_626)),
                    banc.cols = cerise_limon_palette(nrow(data)))
ngl.df <- rbind(ngl.df,
                data.frame(entry = "an-dn-super-cluster-head-and-eye-orienting",
                           ngl_link = ngl.link,
                           codex_link = banc_codex_search(cell.types  = unique(na.omit(data$cell_type)))))

# Figure 4e
sheet_name <- "Figure4e"
fig4e <- read_sheet(vignette_spreadsheet, sheet = sheet_name, col_types = "ccc")
fig4e_ids <- na.omit(fig4e$root_id)
data <- banc.meta %>%
  #  dplyr::filter(grepl("DNg89|DNa06|AN03A002|AN18B023|AN19B018|CvN5|CvN6|CvN7|CvN8|DProN_v|DProN_d|CvN3|CvN1|CvN2|CvN_A1|CvN1|CvN_A2|CvN1|VCvN",cell_type))
  dplyr::filter(root_626 %in% fig4e_ids)
banc.cols <- paper.cols[data$super_class]
banc.cols[is.na(banc.cols)] <- "#FFFFFF"
ngl.link <- bancsee(
                    banc_static_ids = c(na.omit(data$root_626)),
                    banc.cols = banc.cols)
ngl.df <- rbind(ngl.df,
                data.frame(entry = "figure-4e",
                           ngl_link = ngl.link,
                           codex_link = banc_codex_network(cell.types  = unique(na.omit(data$cell_type)))))

# Figure 4f
sheet_name <- "Figure4f"
fig4f <- read_sheet(vignette_spreadsheet, sheet = sheet_name)
fig4f_ids <- unlist(na.omit(fig4f$root_id))
data <- banc.meta %>%
  #  dplyr::filter(grepl("DN1p10|IN12B018|tibia_flexor|tibia_extensor|AN06B002|AN06B005|SNpp45|SNta21|SNpp39",cell_type))
  dplyr::filter(root_626 %in% fig4f_ids)
banc.cols <- paper.cols[data$super_class]
banc.cols[is.na(banc.cols)] <- "#FFFFFF"
ngl.link <- bancsee(
                    banc_static_ids = c(na.omit(data$root_626)),
                    banc.cols = banc.cols)
ngl.df <- rbind(ngl.df,
                data.frame(entry = "figure-4f",
                           ngl_link = ngl.link,
                           codex_link = banc_codex_network(cell.types  = unique(na.omit(data$cell_type)))))
# # Figure 4g
# sheet_name <- "Figure4g"
# fig4g <- read_sheet(vignette_spreadsheet, sheet = sheet_name)
# fig4g_ids <- unlist(na.omit(fig4g$root_id))
# data <- banc.meta %>%
#   #  dplyr::filter(grepl("DN1p10|IN12B018|tibia_flexor|tibia_extensor|AN06B002|AN06B005|SNpp45|SNta21|SNpp39",cell_type))
#   dplyr::filter(root_626 %in% fig4g_ids)
# banc.cols <- paper.cols[data$super_class]
# banc.cols[is.na(banc.cols)] <- "#FFFFFF"
# ngl.link <- bancsee(
#                     banc_static_ids = c(na.omit(data$root_626)),
#                     banc.cols = banc.cols)
# ngl.df <- rbind(ngl.df,
#                 data.frame(entry = "figure-4g",
#                            ngl_link = ngl.link),
#                 codex_link = banc_codex_network(cell.types  = unique(na.omit(data$cell_type))))

################
### FIGURE 5 ###
################

# Figure 5c
sheet_name <- "Figure5c"
fig5c <- read_sheet(vignette_spreadsheet, sheet = sheet_name, col_types = "cccc")
fig5c_ids <- na.omit(fig5c$root_id)
data <- banc.meta %>%
  # dplyr::filter(grepl("CL210|SMP461|CL209|DNp38|AN09B029|IN1B011|AN02A002|DNg100|WPNb|SNta33|SNta15|SNta05|tibia_flexor|tibia_extensor|sternal_posterior_rotator|sternal_anterior_rotator",cell_type))
  dplyr::filter(root_626 %in% fig5c_ids)
banc.cols <- paper.cols[data$super_class]
banc.cols[is.na(banc.cols)] <- "#FFFFFF"
ngl.link <- bancsee(
                    banc_static_ids = c(na.omit(data$root_626)),
                    banc.cols = banc.cols)
ngl.df <- rbind(ngl.df,
                data.frame(entry = "figure-5c",
                           ngl_link = ngl.link,
                           codex_link = banc_codex_network(cell.types  = unique(na.omit(data$cell_type)))))

################
### FIGURE 6 ###
################

# Figure 6g
sheet_name <- "Figure6g"
fig6g <- read_sheet(vignette_spreadsheet, sheet = sheet_name, col_types = "cccc")
fig6g_ids <- na.omit(fig6g$root_id)
data <- banc.meta %>%
  #  dplyr::filter(grepl("MBON20|LPLC2|LB1|AVLP445|DNp42|IN06B032|IN07B010|AN19B001|DNp103|DNp01|IN11A001",cell_type))
  dplyr::filter(root_626 %in% fig6g_ids)
banc.cols <- paper.cols[data$super_class]
banc.cols[is.na(banc.cols)] <- "#FFFFFF"
ngl.link <- bancsee(
                    banc_static_ids = c(na.omit(data$root_626)),
                    banc.cols = banc.cols)
ngl.df <- rbind(ngl.df,
                data.frame(entry = "figure-6g",
                           ngl_link = ngl.link,
                           codex_link = banc_codex_network(cell.types  = unique(na.omit(data$cell_type)))))

# Figure 6h
sheet_name <- "Figure6h-in"
fig6h <- read_sheet(vignette_spreadsheet, sheet = sheet_name, col_types = "ccc")
fig6h_ids <- na.omit(fig6h$root_id)

data <- banc.meta %>%
# dplyr::filter(grepl("EPG|PEN1|GLNO|DNa16|DNa05|AN07B037",cell_type))
  dplyr::filter(root_626 %in% fig6h_ids)
banc.cols <- paper.cols[data$super_class]
banc.cols[is.na(banc.cols)] <- "#FFFFFF"
ngl.link <- bancsee(
                    banc_static_ids = c(na.omit(data$root_626)),
                    banc.cols = banc.cols)
ngl.df <- rbind(ngl.df,
                data.frame(entry = "figure-6h-in",
                           ngl_link = ngl.link,
                           codex_link = banc_codex_network(cell.types  = unique(na.omit(data$cell_type)))))

# Figure 6h
sheet_name <- "Figure6h-out"
fig6h <- read_sheet(vignette_spreadsheet, sheet = sheet_name, col_types = "cccc")
fig6h_ids <- na.omit(fig6h$root_id)

data <- banc.meta %>%
  #  dplyr::filter(grepl("EPG|PEN1|GLNO|DNa16|DNa05|AN07B037",cell_type))
  dplyr::filter(root_626 %in% fig6h_ids)
banc.cols <- paper.cols[data$super_class]
banc.cols[is.na(banc.cols)] <- "#FFFFFF"
ngl.link <- bancsee(
                    banc_static_ids = c(na.omit(data$root_626)),
                    banc.cols = banc.cols)
ngl.df <- rbind(ngl.df,
                data.frame(entry = "figure-6h-out",
                           ngl_link = ngl.link,
                           codex_link = banc_codex_network(cell.types  = unique(na.omit(data$cell_type)))))

# CNS networks
for(cnsn in na.omit(unique(banc.meta$cns_network))){
  data <- banc.meta %>%
    dplyr::filter(cns_network==cnsn)
  banc.cols <- paper.cols[data$super_class]
  banc.cols[is.na(banc.cols)] <- "#FFFFFF"
  ngl.link <- bancsee(
                      banc_static_ids = c(na.omit(data$root_626)),
                      banc.cols = banc.cols)
  ngl.df <- rbind(ngl.df,
                  data.frame(entry = cnsn,
                             ngl_link = ngl.link,
                             codex_link = banc_codex_search(cell.types  = unique(na.omit(data$cell_type)))))
}

############
### SAVE ###
############

# save
readr::write_csv(x = ngl.df %>%
                   dplyr::distinct(), 
                 "submission/ngl-links.csv")
# ngl.df <- "https://ng.banc.community/2025a/"

# NGL state links
ngl.state.location <- "/Users/GD/LMBD/Papers/banc/the-BANC-fly-connectome/neuroglancer_states/2025a"
for(i in 1:nrow(ngl.df)){
  nam <- ngl.df$entry[i]
  link <- ngl.df$ngl_link[i]
  json <- fafbseg::ngl_decode_scene(link,
                                    return.json = TRUE)
  pretty_json <- jsonlite::prettify(json)
  nam <- gsub(" |_","-",nam)
  writeLines(pretty_json, file.path(ngl.state.location,paste0(nam,".json")))
}








