#' Supplementary data table generation
#'
#' Builds the per-table CSV files deposited as Supplementary Data
#' alongside the paper. Numbering matches the 2026-05-03 reorganisation
#' agreed with the editorial team:
#'   Supp 1  — annotation-scheme definitions (delegated to collaborator)
#'   Supp 2  — BANC metadata
#'   Supp 3  — FAFB metadata
#'   Supp 4  — MANC metadata
#'   Supp 5  — maleCNS metadata
#'   Supp 6  — AN/DN UMAP + cluster assignments
#'   Supp 7  — Effector UMAP + cluster assignments
#'   Supp 8  — CNS network UMAP
#'   Supp 9  — Literature review of AN/DN behavioural functions
#'   Supp 10 — Dataset issues (bounding boxes for known artefacts)
#'
#' @section Reads:
#'   banc.meta, franken.meta, paper.cols
#'   data/banc_annotations/v888/banc_neck_functional_classes.csv                                    (AN/DN UMAP + clusters)
#'   data/banc_eff_umap_clusters.csv                                           (EFF UMAP + clusters)
#'   .banc_spectral_csv                                                         (CNS network UMAP)
#'
#' @section Writes:
#'   manuscript/print/supplemental_data/supp_NN_*.csv                          (one per table)
#'
#' @section Paper:
#'   Supplementary Data 1–10 (see numbering above).
#'   Methods §"Cell type matching and annotation" + §"Naming AN/DN clusters" +
#'   §"Naming effector groups" + §"Naming CNS networks" describe how each
#'   table was generated.
#'
#' @section Used by:
#'   Harvard Dataverse upload (manuscript/print/dataverse/).
#'
#' @section Reproduce:
#'   Rscript R/text/supplemental_data.R
source("R/startup/banc-startup.R")
source("R/startup/gcs-helpers.R")
source("R/startup/banc-meta.R")
# franken.meta is needed for FAFB / MANC supps (3, 4) below
source("R/startup/franken-meta.R")

supp_path <- "manuscript/print/supplemental_data"
dir.create(supp_path, showWarnings = FALSE, recursive = TRUE)

# Columns common to all four dataset-metadata supps. `any_of()` tolerates
# datasets that don't yet have a column (warning + NA at the user's choice).
.dataset_common_cols <- c(
  "dataset", "flow", "super_class", "cell_class", "cell_sub_class", "cell_type",
  "region", "side", "cell_function", "cell_function_detailed",
  "peripheral_target_type", "body_part_sensory", "body_part_effector",
  "nerve", "hemilineage", "sexually_dimorphic",
  "neurotransmitter_verified", "neuropeptide_verified", "neurotransmitter_predicted",
  "other_names"
)

# Helper: warn about missing common columns in a dataset
.warn_missing <- function(df, expected, label) {
  missing <- setdiff(expected, colnames(df))
  if (length(missing)) {
    message("  [", label, "] missing ", length(missing),
            " column(s): ", paste(missing, collapse = ", "))
  }
}

# Helper: coalesce neurotransmitter_predicted from {predicted, top_nt}
.coalesce_nt_predicted <- function(df) {
  has_p <- "neurotransmitter_predicted" %in% colnames(df)
  has_t <- "top_nt"                     %in% colnames(df)
  if (has_p && has_t) {
    df %>% dplyr::mutate(neurotransmitter_predicted =
                           dplyr::coalesce(neurotransmitter_predicted, top_nt))
  } else if (!has_p && has_t) {
    df %>% dplyr::mutate(neurotransmitter_predicted = top_nt)
  } else {
    df
  }
}

############################
### Supp 1 — Annotation categories and terms
############################
# Wide-form per-column enumeration of every term used in each
# annotation category in BANC v888. Hierarchical or single-valued
# categories list every distinct level alphabetically; multi-value
# categories (neurotransmitter_verified, neuropeptide_verified) are
# split on commas to give the atomic vocabulary. Non-enumerable
# columns (cell_type, other_names, the cross-dataset cell_type and
# match_id columns) carry a single placeholder string so the column
# is documented even though its values are too numerous to enumerate
# in this table — see Supps 2-5 for the full per-neuron values.
.supp1_col_spec <- list(
  list(name = "flow",                       kind = "enum"),
  list(name = "super_class",                kind = "enum"),
  list(name = "cell_class",                 kind = "enum"),
  list(name = "cell_sub_class",             kind = "enum"),
  list(name = "cell_type",                  kind = "placeholder", value = "[string]"),
  list(name = "region",                     kind = "enum"),
  list(name = "side",                       kind = "enum"),
  list(name = "cell_function",              kind = "enum"),
  list(name = "cell_function_detailed",     kind = "enum"),
  list(name = "peripheral_target_type",     kind = "enum"),
  list(name = "body_part_sensory",          kind = "enum"),
  list(name = "body_part_effector",         kind = "enum"),
  list(name = "nerve",                      kind = "enum"),
  list(name = "hemilineage",                kind = "enum"),
  list(name = "sexually_dimorphic",         kind = "enum"),
  list(name = "neurotransmitter_verified",  kind = "enum_multi"),
  list(name = "neuropeptide_verified",      kind = "enum_multi"),
  list(name = "neurotransmitter_predicted", kind = "enum"),
  list(name = "other_names",                kind = "placeholder", value = "[string]"),
  list(name = "alignment_cell_type",        kind = "placeholder", value = "[string]"),
  list(name = "fafb_783_cell_type",         kind = "placeholder", value = "[string]"),
  list(name = "manc_121_cell_type",         kind = "placeholder", value = "[string]"),
  list(name = "fanc_1116_cell_type",        kind = "placeholder", value = "[string]"),
  list(name = "hemibrain_121_cell_type",    kind = "placeholder", value = "[string]"),
  list(name = "malecns_09_cell_type",    kind = "placeholder", value = "[string]"),
  list(name = "fafb_783_match_id",          kind = "placeholder", value = "[integer64]"),
  list(name = "manc_121_match_id",          kind = "placeholder", value = "[integer]"),
  list(name = "fanc_1116_match_id",         kind = "placeholder", value = "[integer]"),
  list(name = "hemibrain_121_match_id",     kind = "placeholder", value = "[integer]"),
  list(name = "malecns_09_match_id",     kind = "placeholder", value = "[integer]")
)

.supp1_values <- function(spec, df) {
  if (spec$kind == "placeholder") return(spec$value)
  if (!spec$name %in% colnames(df)) {
    message("  [Supp 1] missing column: ", spec$name)
    return(character(0))
  }
  vals <- df[[spec$name]]
  vals <- vals[!is.na(vals) & vals != ""]
  if (spec$kind == "enum_multi") {
    vals <- unlist(strsplit(as.character(vals), ","), use.names = FALSE)
    vals <- trimws(vals)
    vals <- vals[vals != ""]
  }
  sort(unique(as.character(vals)))
}

.supp1_cols <- lapply(.supp1_col_spec, .supp1_values, df = banc.meta)
names(.supp1_cols) <- vapply(.supp1_col_spec, function(s) s$name, character(1))
.supp1_nrow <- max(c(vapply(.supp1_cols, length, integer(1)), 1L))
.supp1_padded <- lapply(.supp1_cols, function(v) {
  if (length(v) < .supp1_nrow) c(v, rep("", .supp1_nrow - length(v))) else v
})
.supp1_df <- as.data.frame(.supp1_padded, stringsAsFactors = FALSE,
                            check.names = FALSE)
readr::write_csv(.supp1_df,
                 file = file.path(supp_path, "supplemental_data_1.txt"),
                 na = "")
message("Wrote supplemental_data_1.txt: ", .supp1_nrow, " rows × ",
        ncol(.supp1_df), " annotation categories")
rm(.supp1_col_spec, .supp1_values, .supp1_cols, .supp1_nrow, .supp1_padded, .supp1_df)

############################
### Supp 2 — BANC metadata
############################
# Restrict to neurons with proofread == TRUE OR roughly_proofread == TRUE
# (added 2026-05-06) — these are the cells we actually report on; excluding
# unproofread fragments shrinks the file by ~50% to clear Nature's 30 MB
# per-supp limit. Both flags are kept as output columns so users can tell
# the two pools apart.
banc_meta_export <- banc.meta %>%
  dplyr::filter(!grepl("DEBRIS", status),
                !super_class %in% c("not_a_neuron", "debris"),
                as.logical(proofread)         %in% TRUE |
                  as.logical(roughly_proofread) %in% TRUE) %>%
  dplyr::select(root_id,
                # `dataset` column dropped 2026-05-18 to keep Supp 2 under
                # Nature's 30 MB threshold; it was constant "BANC" — only
                # useful when concatenating Supps 2-5. Note absence in the
                # legend (all rows here are BANC by definition).
                dplyr::any_of(setdiff(.dataset_common_cols, "dataset")),
                # BANC-only: cell type from the NBLAST-to-connectivity
                # annealing algorithm (described in Supp 2 column legend,
                # not present in FAFB/MANC/maleCNS so not in common cols).
                dplyr::any_of("fafb_alignment_cell_type"),
                dplyr::any_of(c("proofread", "roughly_proofread")),
                dplyr::any_of(c("fafb_match", "manc_match", "hemibrain_match",
                                "fanc_match", "malecns_match")))
.warn_missing(banc_meta_export, .dataset_common_cols, "BANC")
readr::write_csv(banc_meta_export,
                 file = file.path(supp_path, "supplemental_data_2.txt"))
message("Wrote supplemental_data_2.txt: BANC metadata (",
        nrow(banc_meta_export), " neurons; proofread or roughly_proofread)")

############################
### Supp 3 — FAFB metadata
############################
fafb_meta_export <- franken.meta %>%
  dplyr::filter(!is.na(fafb_id)) %>%
  .coalesce_nt_predicted() %>%
  dplyr::mutate(dataset = "FAFB") %>%
  dplyr::select(root_783 = fafb_id, dplyr::any_of(.dataset_common_cols))
.warn_missing(fafb_meta_export, .dataset_common_cols, "FAFB")
readr::write_csv(fafb_meta_export,
                 file = file.path(supp_path, "supplemental_data_3.txt"))
message("Wrote supplemental_data_3.txt: FAFB metadata (",
        nrow(fafb_meta_export), " neurons)")

############################
### Supp 4 — MANC metadata
############################
manc_meta_export <- franken.meta %>%
  dplyr::filter(!is.na(manc_id)) %>%
  .coalesce_nt_predicted() %>%
  dplyr::mutate(dataset = "MANC") %>%
  dplyr::select(bodyid = manc_id, dplyr::any_of(.dataset_common_cols))
.warn_missing(manc_meta_export, .dataset_common_cols, "MANC")
readr::write_csv(manc_meta_export,
                 file = file.path(supp_path, "supplemental_data_4.txt"))
message("Wrote supplemental_data_4.txt: MANC metadata (",
        nrow(manc_meta_export), " neurons)")

############################
### Supp 5 — maleCNS metadata
############################
.malecns_gcs <- "gs://lee-lab_brain-and-nerve-cord-fly-connectome/compiled_data/malecns_09"
malecns_meta_export <- tryCatch({
  read_feather_gcs(file.path(.malecns_gcs, "malecns_09_meta.feather")) %>%
    .coalesce_nt_predicted() %>%
    dplyr::mutate(dataset = "maleCNS") %>%
    dplyr::select(malecns_09_id,
                  dplyr::any_of(.dataset_common_cols))
}, error = function(e) {
  warning("Could not load maleCNS metadata from ", .malecns_gcs, ": ",
          conditionMessage(e))
  tibble::tibble()
})
.warn_missing(malecns_meta_export, .dataset_common_cols, "maleCNS")
readr::write_csv(malecns_meta_export,
                 file = file.path(supp_path, "supplemental_data_5.txt"))
message("Wrote supplemental_data_5.txt: maleCNS metadata (",
        nrow(malecns_meta_export), " neurons)")

############################
### Supp 6 — AN/DN UMAP
############################
classes.nn.df <- readr::read_csv(file = "data/banc_annotations/v888/banc_neck_functional_classes.csv",
                                 col_types = banc.col.types) %>%
  dplyr::select(id, UMAP1, UMAP2, dplyr::any_of(c("PCA_UMAP1", "PCA_UMAP2")))
classes.nn.df <- classes.nn.df %>%
  dplyr::left_join(banc.meta %>%
                     dplyr::select(root_id, supervoxel_id, position,
                                   side, region, super_class, hemilineage,
                                   cell_function, nerve, cell_type,
                                   fafb_cell_type, manc_cell_type,
                                   super_cluster, cns_network),
                   by = c("id" = "root_id"))
readr::write_csv(classes.nn.df,
                 file = file.path(supp_path, "supplemental_data_6.txt"))
message("Wrote supplemental_data_6.txt: AN/DN UMAP (",
        nrow(classes.nn.df), " neurons)")

############################
### Supp 7 — EFF UMAP
############################
classes.eff.df <- readr::read_csv(file = "data/banc_annotations/v888/banc_efferent_functional_classes.csv",
                                  col_types = banc.col.types) %>%
  dplyr::select(id, UMAP1, UMAP2)
classes.eff.df <- classes.eff.df %>%
  dplyr::left_join(banc.meta %>%
                     dplyr::select(root_id, supervoxel_id, position,
                                   side, region, flow, super_class, hemilineage,
                                   cell_function, nerve, cell_type,
                                   fafb_cell_type, manc_cell_type,
                                   cluster, super_cluster),
                   by = c("id" = "root_id"))
readr::write_csv(classes.eff.df,
                 file = file.path(supp_path, "supplemental_data_7.txt"))
message("Wrote supplemental_data_7.txt: EFF UMAP (",
        nrow(classes.eff.df), " neurons)")

############################
### Supp 8 — CNS network UMAP
############################
cns_network_file <- .banc_spectral_csv
if (!file.exists(cns_network_file)) {
  warning(sprintf("v%d CNS network file not found: %s",
                  .version_num, cns_network_file))
}
cns.network.umap <- readr::read_csv(cns_network_file,
                                    col_types = banc.col.types) %>%
  dplyr::distinct(root_id, UMAP1 = umap_x, UMAP2 = umap_y)
cns.network.umap <- cns.network.umap %>%
  dplyr::left_join(banc.meta %>%
                     dplyr::select(root_id, supervoxel_id, position,
                                   side, region, super_class, hemilineage,
                                   cell_function, cell_type,
                                   fafb_cell_type, manc_cell_type,
                                   super_cluster, cns_network),
                   by = "root_id")
readr::write_csv(cns.network.umap,
                 file = file.path(supp_path, "supplemental_data_8.txt"))
message("Wrote supplemental_data_8.txt: CNS network UMAP (",
        nrow(cns.network.umap), " neurons)")

############################
### Supp 9 — Literature review
############################
lit.review <- cns.functions %>%
  dplyr::filter(super_class %in% c("ascending", "descending", "visual_projection"),
                !is.na(modality)) %>%
  dplyr::mutate(cell_function = dplyr::case_when(
    super_class == "visual_projection" ~ response,
    TRUE ~ modality
  )) %>%
  dplyr::filter(!is.na(cell_function) & cell_function != "" & !is.na(citations)) %>%
  # `doi` is populated on the functions table itself (2026-05-18); carry it
  # through instead of setting NA.
  dplyr::distinct(cell_type, other_names, super_class, cell_function, citations, doi)
readr::write_csv(lit.review,
                 file = file.path(supp_path, "supplemental_data_9.txt"))
message("Wrote supplemental_data_9.txt: Literature review (",
        nrow(lit.review), " cell types)")

############################
### Supp 10 — Dataset issues (bounding boxes)
############################
dataset_issues_bboxes <- tibble::tibble(
  issue = c(
    rep("tunnel of death", 8),
    "T2 blowout",
    "T1 soup",
    rep("champagne patch", 5),
    "left VLP blowout",
    rep("dorsal CB wavy patch", 2),
    rep("dorsal esophageal crush", 3),
    "butt wiggle"
  ),
  min_x = c(86240, 85969, 89471, 113941, 126446, 139380, 146357, 149234,
             99778, 146730, 116523, 114396, 115172, 111512, 116261,
             156321, 134418, 127114, 117293, 117339, 117413, 88387),
  min_y = c(24188, 34825, 35922, 38078, 35586, 35978, 35253, 34279,
             176438, 194587, 204075, 201794, 202544, 200317, 205698,
             24155, 32160, 36447, 25283, 23010, 16334, 236835),
  min_z = c(1504, 2230, 2935, 3195, 3145, 3007, 2605, 2019,
             3251, 4441, 5478, 5620, 5885, 5975, 6385,
             3251, 4852, 4536, 2884, 3169, 3351, 6076),
  max_x = c(100628, 100310, 113945, 126443, 139404, 150973, 153882, 153801,
             104086, 148357, 119830, 123629, 122619, 124825, 122632,
             163691, 142747, 142748, 128158, 128157, 128158, 111269),
  max_y = c(41809, 44999, 46013, 48442, 45760, 42310, 41070, 39355,
             181975, 198226, 208313, 209989, 211033, 215415, 213127,
             29468, 44743, 47412, 33914, 28054, 25926, 255615),
  max_z = c(2229, 2959, 3691, 3691, 3691, 3506, 3005, 2603,
             3687, 4616, 5620, 5882, 5975, 6385, 6495,
             3454, 5109, 4852, 3170, 3352, 3738, 6226)
)
readr::write_csv(dataset_issues_bboxes,
                 file = file.path(supp_path, "supplemental_data_10.txt"))
message("Wrote supplemental_data_10.txt: Dataset issues bounding boxes (",
        nrow(dataset_issues_bboxes), " regions)")

message("\nAll supplemental data written to: ", supp_path)
message("Files: ", paste(list.files(supp_path, pattern = "\\.csv$"), collapse = ", "))


##############################################################
## Push spectral clustering + betweenness results to GCS    ##
## with clean per-file names under compiled_data/banc_888/. ##
##############################################################
# The bancpipeline outputs encode every clustering parameter in the
# filename, which is informative for the build pipeline but unwieldy for
# users. Stage clean copies (banc_888_cns_network_spectral_clustering_v2.csv,
# banc_888_betweenness_all_to_all_v2.csv, etc.) to the public GCS bucket
# so they can be referenced by simple URLs in the paper and bundled into
# the Dataverse deposit.
.gcs_compiled <- "gs://lee-lab_brain-and-nerve-cord-fly-connectome/compiled_data/banc_888"

.gcs_uploads <- list(
  list(local  = "data/cns_network/spectral_clustering_min_connection_strength_1_banc_version_888_cluster_count_13_cluster_seed_10_embedding_seed_3_v2.csv",
       remote = "banc_888_cns_network_spectral_clustering_v2.csv"),
  list(local  = "data/cns_network/spectral_clustering_min_connection_strength_1_banc_version_888_cluster_count_13_cluster_seed_10_embedding_seed_3_v3.csv",
       remote = "banc_888_cns_network_spectral_clustering_v3.csv"),
  list(local  = "data/betweenness/888/betweenness_all_to_all_banc_888_v2.csv",
       remote = "banc_888_betweenness_all_to_all_v2.csv"),
  list(local  = "data/betweenness/888/betweenness_all_to_all_banc_888_v3.csv",
       remote = "banc_888_betweenness_all_to_all_v3.csv"),
  list(local  = "data/betweenness/888/betweenness_afferent_to_efferent_banc_888_v2.csv",
       remote = "banc_888_betweenness_afferent_to_efferent_v2.csv"),
  list(local  = "data/betweenness/888/betweenness_afferent_to_efferent_banc_888_v3.csv",
       remote = "banc_888_betweenness_afferent_to_efferent_v3.csv")
)

message("\nPushing spectral clustering + betweenness CSVs to GCS ...")
for (.u in .gcs_uploads) {
  if (!file.exists(.u$local)) {
    message("  [skip] missing local file: ", .u$local)
    next
  }
  .remote_url <- file.path(.gcs_compiled, .u$remote)
  .cmd <- sprintf("gsutil cp %s %s",
                  shQuote(.u$local), shQuote(.remote_url))
  message("  ", basename(.u$local), " -> ", .u$remote)
  .rc <- system(.cmd, intern = FALSE)
  if (.rc != 0) message("    [WARN] gsutil cp returned exit ", .rc)
}
message("GCS push complete.")
