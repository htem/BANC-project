#' Neuroglancer + Codex link builder for paper figure legends
#'
#' Generates every `https://ng.banc.community/2026a/<slug>` and
#' Codex `https://codex.flywire.ai/app/connectivity?...` URL referenced
#' in the figure legends, captions, and inline citations of the doc. Uses
#' `bancr::bancsee()` (wrapped in `safe_bancsee()` to suppress per-call
#' failures) to build the Neuroglancer JSON; Codex links are constructed
#' from cell_type filters.
#'
#' Vignette neuron rosters come from the curated CSVs at
#' `figures/vignette_neuron_lists/` (produced by panels_vignette_networks.R).
#'
#' @section Reads:
#'   banc.meta, paper.cols
#'   manuscript/print/paper_cell_type_references.csv                          (canary cell-type list)
#'   figures/vignette_neuron_lists/network_<vig>_neurons.csv                  (per-vignette rosters)
#'
#' @section Writes:
#'   manuscript/print/ngl_links.csv                                            (one row per (entry, ngl, codex))
#'
#' @section Paper:
#'   Every Neuroglancer / Codex hyperlink in the figure legends + Methods +
#'   Supplementary captions (e.g. "[Neuroglancer](https://ng.banc.community/2026a/figure-3c)").
#'
#' @section Used by:
#'   The doc's hyperlinks are authored by hand, but this CSV is the
#'   single source-of-truth for which slugs exist and what they resolve to.
#'
#' @section Reproduce:
#'   Rscript R/text/ngl_links.R

source("R/startup/banc-startup.R")
source("R/startup/banc-meta.R")

ngl.df <- data.frame(entry = character(), ngl_link = character(),
                     codex_link = character(), stringsAsFactors = FALSE)

add_entry <- function(entry, ngl_link, codex_link = NA_character_) {
  ngl.df <<- rbind(ngl.df, data.frame(entry = entry, ngl_link = ngl_link,
                                       codex_link = codex_link, stringsAsFactors = FALSE))
}

safe_bancsee <- function(...) {
  tryCatch(bancsee(...), error = function(e) {
    message(sprintf("  bancsee error: %s", e$message))
    NA_character_
  })
}

#####################
### STARTER LINKS ###
#####################

starter.links <- c(
  "https://spelunker.cave-explorer.org/#!middleauth+https://global.daf-apis.com/nglstate/api/v1/6615245819740160",
  "https://spelunker.cave-explorer.org/#!middleauth+https://global.daf-apis.com/nglstate/api/v1/6590872282988544",
  "https://spelunker.cave-explorer.org/#!middleauth+https://global.daf-apis.com/nglstate/api/v1/5501382827180032",
  "https://spelunker.cave-explorer.org/#!middleauth+https://global.daf-apis.com/nglstate/api/v1/5251862407151616",
  "https://spelunker.cave-explorer.org/#!middleauth+https://global.daf-apis.com/nglstate/api/v1/5970228973404160"
)
starter.entries <- c("neck-connective", "example-synapse", "example-nuclei",
                     "example-mitochondria", "DNa01-match")
for (l in seq_along(starter.links)) {
  url <- sub("#!middleauth+", "?", starter.links[l], fixed = TRUE)
  parts <- unlist(strsplit(url, "?", fixed = TRUE))
  json <- try(fafbseg::flywire_fetch(parts[2], token = bancr:::banc_token(),
                                     return = "text", cache = TRUE))
  if (!inherits(json, "try-error")) {
    starter.links[l] <- ngl_encode_url(json, baseurl = parts[1])
  }
}
for (l in seq_along(starter.links)) {
  add_entry(starter.entries[l], starter.links[l])
}

# Build base — bancsee's `banc_scene()` only handles middleauth+nglstate URLs,
# so we let it fetch the auth-protected canvas, then strip the auth-requiring
# layers from each exported JSON below (search for `.strip_middleauth_layers`).
# The output states display via the public gs:// segmentation only.
base.url <- "https://spelunker.cave-explorer.org/#!middleauth+https://global.daf-apis.com/nglstate/api/v1/5506684867837952"

#######################
### NECK ANNOTATION ###
#######################

# Same — middleauth canvas, stripped on export.
neck.base.url <- "https://spelunker.cave-explorer.org/#!middleauth+https://global.daf-apis.com/nglstate/api/v1/6727918582497280"

data <- banc.meta %>%
  dplyr::filter(grepl("ascending|descending|motor", super_class) |
                  grepl("neck", region) | grepl("neck", cell_class)) %>%
  dplyr::filter(!is.na(super_class))
banc.cols <- paper.cols[data$super_class]
banc.cols[is.na(banc.cols)] <- "#000000"
add_entry("neck_an_dns",
          safe_bancsee(url = neck.base.url,
                       banc_static_ids = na.omit(data$root_888),
                       banc.cols = banc.cols))

data <- banc.meta %>%
  dplyr::filter(grepl("ascending|descending|motor", super_class) |
                  grepl("neck", region) | grepl("neck", cell_class)) %>%
  dplyr::mutate(super_cluster = ifelse(flow == "efferent", super_class, super_cluster)) %>%
  dplyr::filter(!is.na(super_cluster))
banc.cols <- paper.cols[data$super_cluster]
banc.cols[is.na(banc.cols)] <- "#000000"
add_entry("neck_an_dn_super_clusters",
          safe_bancsee(url = neck.base.url,
                       banc_static_ids = na.omit(data$root_888),
                       banc.cols = banc.cols))

################
### FIGURE 1 ###
################

for (ct in c("DNa02", "LB1a", "DVM1a-c")) {
  data <- banc.meta %>% dplyr::filter(cell_type == ct)
  add_entry(ct,
            safe_bancsee(url = base.url, banc_static_ids = na.omit(data$root_888)),
            banc_codex_search(cell.types = ct))
}

################
### FIGURE 2 ###
################

data <- banc.meta %>% dplyr::filter(super_class == "ascending")
banc.cols <- paper.cols[data$super_class]
add_entry("ascending",
          safe_bancsee(url = base.url, banc_static_ids = na.omit(data$root_888), banc.cols = banc.cols),
          banc_codex_search(cell.types = unique(na.omit(data$cell_type))))

data <- banc.meta %>% dplyr::filter(super_class == "descending")
banc.cols <- paper.cols[data$super_class]
add_entry("descending",
          safe_bancsee(url = base.url, banc_static_ids = na.omit(data$root_888), banc.cols = banc.cols),
          banc_codex_search(cell.types = unique(na.omit(data$cell_type))))

data <- banc.meta %>% dplyr::filter(grepl("EFF", cluster))
banc.cols <- paper.cols[data$cluster]
add_entry("efferent_clusters",
          safe_bancsee(url = base.url, banc_static_ids = na.omit(data$root_888), banc.cols = banc.cols),
          banc_codex_search(cell.types = unique(na.omit(data$cell_type))))

data <- banc.meta %>% dplyr::filter(!is.na(body_part_effector))
banc.cols <- paper.cols[data$body_part_effector]
banc.cols[is.na(banc.cols)] <- "#FFFFFF"
add_entry("efferent_body_parts",
          safe_bancsee(url = base.url, banc_static_ids = na.omit(data$root_888), banc.cols = banc.cols),
          banc_codex_search(cell.types = unique(na.omit(data$cell_type))))

################
### FIGURE 3 ###
################

data <- banc.meta %>%
  dplyr::filter(super_class %in% c("ascending", "descending"), !is.na(super_cluster))
banc.cols <- paper.cols[data$super_cluster]
banc.cols[is.na(banc.cols)] <- "#FFFFFF"
add_entry("an_dn_super_clusters",
          safe_bancsee(url = base.url, banc_static_ids = na.omit(data$root_888), banc.cols = banc.cols))

for (spc in na.omit(unique(banc.meta$super_cluster))) {
  data <- banc.meta %>% dplyr::filter(super_cluster == spc)
  banc.cols <- paper.cols[data$super_class]
  banc.cols[is.na(banc.cols)] <- "#FFFFFF"
  add_entry(spc,
            safe_bancsee(url = base.url, banc_static_ids = na.omit(data$root_888), banc.cols = banc.cols),
            banc_codex_search(cell.types = unique(na.omit(data$cell_type))))
}

################
### FIGURE 4 ###
################

data <- banc.meta %>%
  dplyr::filter(super_class %in% c("ascending", "descending"),
                super_cluster == "head orienting")
add_entry("an-dn-super-cluster-head-orienting",
          safe_bancsee(url = base.url, banc_static_ids = na.omit(data$root_888),
                       banc.cols = cerise_limon_palette(nrow(data))),
          banc_codex_search(cell.types = unique(na.omit(data$cell_type))))

################
### FIGURE 6 ###
################

for (cnsn in na.omit(unique(banc.meta$cns_network))) {
  data <- banc.meta %>% dplyr::filter(cns_network == cnsn)
  banc.cols <- paper.cols[data$super_class]
  banc.cols[is.na(banc.cols)] <- "#FFFFFF"
  add_entry(cnsn,
            safe_bancsee(url = base.url, banc_static_ids = na.omit(data$root_888), banc.cols = banc.cols),
            banc_codex_search(cell.types = unique(na.omit(data$cell_type))))
}

########################
### VIGNETTE LINKS   ###
########################

# Source curated neuron lists directly (these now drive panel_vignette_networks.R
# too). Authoritative for display_name / cell_type / super_class / super_cluster
# per neuron. Earlier this read from figures/vignettes/ — the rendered output —
# which lagged the curated input on every manual fix-up.
vignette.neurons.path <- "figures/vignette_neuron_lists/"
vignette_csvs <- list.files(vignette.neurons.path, pattern = "_neurons\\.csv$", full.names = TRUE)

for (csv_file in vignette_csvs) {
  vig_name <- gsub("network_|_neurons\\.csv", "", basename(csv_file))
  neurons <- readr::read_csv(csv_file, show_col_types = FALSE,
                              col_types = readr::cols(root_id = readr::col_character()))
  if (nrow(neurons) == 0) next

  ids <- unique(na.omit(neurons$root_id))
  cts <- unique(na.omit(neurons$cell_type))

  meta_idx <- match(ids, banc.meta$root_id)
  meta_idx <- meta_idx[!is.na(meta_idx)]
  r626 <- banc.meta$root_888[meta_idx]
  sc <- banc.meta$super_class[meta_idx]
  banc.cols <- paper.cols[sc]
  banc.cols[is.na(banc.cols)] <- "#FFFFFF"
  keep <- !is.na(r626)
  r626 <- r626[keep]
  banc.cols <- banc.cols[keep]

  fig_panel <- gsub("^fig", "figure-", vig_name)
  add_entry(fig_panel,
            safe_bancsee(url = base.url, banc_static_ids = r626, banc.cols = banc.cols),
            banc_codex_network(cell.types = cts))
  message(sprintf("  Vignette %s: %d neurons, %d cell types", vig_name, length(r626), length(cts)))

  simple <- neurons %>%
    dplyr::mutate(bp = dplyr::coalesce(body_part_sensory, body_part_effector, "")) %>%
    dplyr::distinct(cell_type, side, bp, .keep_all = TRUE)
  s_ids <- unique(na.omit(simple$root_id))
  s_idx <- match(s_ids, banc.meta$root_id)
  s_idx <- s_idx[!is.na(s_idx)]
  s_r626 <- banc.meta$root_888[s_idx]
  s_sc <- banc.meta$super_class[s_idx]
  s_cols <- paper.cols[s_sc]
  s_cols[is.na(s_cols)] <- "#FFFFFF"
  s_keep <- !is.na(s_r626)
  add_entry(paste0(fig_panel, "_simple"),
            safe_bancsee(url = base.url, banc_static_ids = s_r626[s_keep],
                         banc.cols = s_cols[s_keep]),
            banc_codex_network(cell.types = cts))
  message(sprintf("  Vignette %s_simple: %d neurons", vig_name, sum(s_keep)))
}

############
### COOL ###
############

data <- banc.meta %>%
  dplyr::filter(grepl("wing", cell_sub_class) | grepl("wing", cell_class))
banc.cols <- paper.cols[data$super_class]
banc.cols[is.na(banc.cols)] <- "#FFFFFF"
add_entry("wing-neurons",
          safe_bancsee(url = base.url, banc_static_ids = na.omit(data$root_888), banc.cols = banc.cols))

############
### SAVE ###
############

ngl.df <- ngl.df %>% dplyr::distinct()
utils::write.csv(ngl.df, "manuscript/print/ngl_links.csv", row.names = FALSE)
message(sprintf("Saved %d links to manuscript/print/ngl_links.csv", nrow(ngl.df)))

# Strip layers whose source uses CAVE/middleauth so the exported states load
# without an authentication prompt. We parse the JSON string (NOT via
# ngl_decode_scene, whose ngscene class converts the layers array into a
# named list) so `layers` stays as a JSON array on re-serialise — neuroglancer
# requires an array.
.strip_middleauth_in_json <- function(json_str) {
  parsed <- jsonlite::fromJSON(json_str,
                               simplifyVector = FALSE,
                               simplifyDataFrame = FALSE,
                               simplifyMatrix = FALSE)
  if (is.null(parsed$layers)) return(json_str)
  parsed$layers <- Filter(function(L) {
    src <- L$source
    txt <- if (is.character(src)) src
           else if (is.list(src)) paste(unlist(src), collapse = " ")
           else ""
    !grepl("middleauth", txt, fixed = TRUE)
  }, parsed$layers)
  # `parsed$layers` is now an unnamed list — toJSON emits it as an array.
  jsonlite::toJSON(parsed, auto_unbox = TRUE, pretty = TRUE,
                   null = "null", na = "null")
}

# Export NGL state JSONs
ngl.state.location <- "manuscript/print/neuroglancer_states/2026a"
dir.create(ngl.state.location, showWarnings = FALSE, recursive = TRUE)
for (i in seq_len(nrow(ngl.df))) {
  nam <- gsub(" |_", "-", ngl.df$entry[i])
  link <- ngl.df$ngl_link[i]
  if (is.na(link)) next
  json_str <- tryCatch(
    fafbseg::ngl_decode_scene(link, return.json = TRUE),
    error = function(e) NULL
  )
  if (!is.null(json_str)) {
    json_str <- .strip_middleauth_in_json(json_str)
    writeLines(json_str, file.path(ngl.state.location, paste0(nam, ".json")))
  }
}
message(sprintf("Exported NGL state JSONs to %s", ngl.state.location))

# Save to Jasper's repo for hosting (also stripped of middleauth layers).
ngl.state.location <- "/Users/GD/LMBD/Papers/banc/the-BANC-fly-connectome/neuroglancer_states/2026a"
if (dir.exists(ngl.state.location)) {
dir.create(ngl.state.location, showWarnings = FALSE, recursive = TRUE)
for (i in seq_len(nrow(ngl.df))) {
  nam <- gsub(" |_", "-", ngl.df$entry[i])
  link <- ngl.df$ngl_link[i]
  if (is.na(link)) next
  json_str <- tryCatch(
    fafbseg::ngl_decode_scene(link, return.json = TRUE),
    error = function(e) NULL
  )
  if (!is.null(json_str)) {
    json_str <- .strip_middleauth_in_json(json_str)
    writeLines(json_str, file.path(ngl.state.location, paste0(nam, ".json")))
  }
}
message(sprintf("Exported NGL state JSONs to %s", ngl.state.location))

}


