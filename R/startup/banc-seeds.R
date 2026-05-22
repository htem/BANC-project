###########################################################
### Assign seed columns to banc.meta for influence runs
###
### Seeds define groups of neurons for computing influence
### scores. Each seed level captures a different granularity:
###   seed_00: individual cell_type (sensory, CX/MB output, VPN, AN/DN)
###   seed_01: super_class + cell_function
###   seed_02: sensory cell_sub_class
###   seed_03: detailed sensory/VPN function
###   seed_04: sensory cell_sub_class + side
###   seed_05: CX output sub_class / AN-DN cluster / MB output class / VPN abbreviated
###   seed_06: seed_05 + side
###   seed_07: cell_sub_type for AN/DN, cell_type for CX/MB/VPN
###   seed_08: detailed sensory (proprio/tactile) by body part + nerve + side
###   seed_09: peripheral_target_type
###   seed_10: sensory body_part + peripheral_target_type
###   seed_11: cluster (AN/DN UMAP cluster)
###   seed_12: individual neuron (cell_type + id)
###   seed_13: individual neuron id (for all-to-all)
###   seed_14: cns_network assignment
###
### Ported from bancpipeline/banc/franken/franken-seeds.R
### Must be sourced AFTER banc-meta.R has fully processed banc.meta
###########################################################

# Helper: extract first 2-3 letters of a cell type name (for VPN abbreviation)
extract_three_letters <- function(text) {
  sapply(text, function(t) {
    three_letters <- stringr::str_extract(t, "^[A-Za-z]{3}")
    if (!is.na(three_letters)) return(three_letters)
    two_letters <- stringr::str_extract(t, "^[A-Za-z]{2}")
    if (!is.na(two_letters)) return(two_letters)
    one_letter <- stringr::str_extract(t, "^[A-Za-z]{1}")
    return(one_letter)
  })
}

#' Assign seed columns (seed_00 through seed_12) to a BANC metadata table
#'
#' @param meta data.frame with banc.meta columns (must have cell_type,
#'   super_class, cell_class, cell_sub_class, side, cluster, id, etc.)
#' @return meta with seed_00 through seed_14 added
assign_banc_seeds <- function(meta) {

  message("Assigning seed columns to banc.meta...")

  # Ensure required columns exist (NA if missing)
  required_cols <- c("cell_function", "cell_function_detailed", "body_part_sensory",
                     "peripheral_target_type", "nerve", "flow", "cluster",
                     "neuromere", "cell_sub_type", "cell_type",
                     "super_class", "cell_class", "cell_sub_class", "side", "id")
  for (col in required_cols) {
    if (!col %in% colnames(meta)) {
      meta[[col]] <- NA_character_
      message(sprintf("  Note: '%s' column missing from banc.meta, seeds using it will be NA", col))
    }
  }

  # Compute temporary derived columns for seed-specific transformations
  # (these transform values differently from what's in banc.meta for seed purposes)
  meta <- meta %>%
    dplyr::mutate(
      # Cell function: fill unknown for seed grouping
      .s_cf = dplyr::case_when(
        is.na(cell_function) | cell_function == "" ~ "unknown",
        TRUE ~ cell_function
      ),
      # Body part sensory: group head sub-regions for seed grouping
      .s_bp = dplyr::case_when(
        grepl("head|^frontal|^frontoorbital|^orbital|^interocellar|^vibrissa|^interommatidial|^occipital_dorsal|^occipital_ventral|^postorbital_dorsal|^postorbital_ventral|^vertical|^postocellar|^supracervical", body_part_sensory) ~ "head",
        is.na(body_part_sensory) | body_part_sensory == "" ~ "unknown",
        TRUE ~ body_part_sensory
      ),
      # Cell function detailed: combine function + detail for finer grouping
      .s_cfd = dplyr::case_when(
        !is.na(cell_function_detailed) & !is.na(cell_function) & cell_function != "" ~
          paste0(cell_function, "_", cell_function_detailed),
        !is.na(cell_function) & cell_function != "" ~ cell_function,
        is.na(cell_function) | cell_function == "" ~ "unknown",
        TRUE ~ cell_function_detailed
      ),
      # Sensory flag: includes non-sensory super_class neurons with body_part_sensory (e.g. lamina L1-L4)
      .is_sensory = grepl("sensory", super_class) | (!is.na(body_part_sensory) & body_part_sensory != ""),
      # Cell function + nerve: prefer detailed, fall back to nerve
      .s_cfn = dplyr::case_when(
        !is.na(cell_function_detailed) ~ cell_function_detailed,
        !is.na(nerve) & nerve != "" ~ gsub("_r$|_l$|_left$|_right$|_R$|_L$|^right_|^left_", "", nerve),
        TRUE ~ cell_function
      )
    )

  # Assign seed columns
  meta <- meta %>%
    dplyr::mutate(
      seed_00 = dplyr::case_when(
        is.na(side) | !side %in% c("left", "right") ~ NA,
        .is_sensory ~ cell_type,
        grepl("central_complex_output|mushroom_body_output_neuron", cell_class) ~ cell_type,
        grepl("visual_projection", super_class) ~ cell_type,
        grepl("ascending|descending", super_class) ~ cell_type,
        TRUE ~ NA
      ),
      seed_01 = dplyr::case_when(
        grepl("efferent", flow) ~ NA,
        is.na(.s_cf) | .s_cf == "unknown" ~ NA,
        TRUE ~ paste0(super_class, "_", .s_cf)
      ),
      seed_02 = dplyr::case_when(
        is.na(side) | !side %in% c("left", "right") | is.na(cell_sub_class) ~ NA,
        .is_sensory ~ cell_sub_class,
        TRUE ~ NA
      ),
      seed_03 = dplyr::case_when(
        !(.is_sensory | grepl("visual_projection", super_class)) | is.na(.s_cfd) | .s_cfd == "unknown" ~ NA,
        grepl("visual_projection", super_class) ~ paste0("visual_projection_", .s_cfd),
        .is_sensory ~ paste0(.s_bp, "_", .s_cfd),
        TRUE ~ NA
      ),
      seed_04 = dplyr::case_when(
        is.na(side) | !side %in% c("left", "right") | is.na(cell_sub_class) ~ NA,
        .is_sensory ~ paste0(cell_sub_class, "_", side),
        TRUE ~ NA
      ),
      seed_05 = dplyr::case_when(
        grepl("central_complex_output", cell_class) & !is.na(cell_sub_class) & cell_sub_class != "" ~ cell_sub_class,
        grepl("ascending|descending", super_class) & !is.na(cluster) & cluster != "" ~ cluster,
        grepl("mushroom_body_output_neuron", cell_class) & !is.na(cell_class) & cell_class != "" ~ cell_class,
        grepl("visual_projection", super_class) & !is.na(cell_type) & cell_type != "" ~ extract_three_letters(cell_type),
        TRUE ~ NA
      ),
      seed_06 = dplyr::case_when(
        is.na(side) | !side %in% c("left", "right", "midline", "center") ~ NA,
        grepl("central_complex_output", cell_class) & !is.na(cell_sub_class) & cell_sub_class != "" ~ paste0(cell_sub_class, "_", side),
        grepl("ascending|descending", super_class) & !is.na(cluster) & cluster != "" ~ paste0(cluster, "_", side),
        grepl("mushroom_body_output_neuron", cell_class) & !is.na(cell_class) & cell_class != "" ~ paste0(cell_class, "_", side),
        grepl("visual_projection", super_class) & !is.na(cell_type) & cell_type != "" ~ paste0(extract_three_letters(cell_type), "_", side),
        TRUE ~ NA
      ),
      seed_07 = dplyr::case_when(
        grepl("central_complex_output|mushroom_body_output_neuron", cell_class) ~ cell_type,
        grepl("^EPG|^EL", cell_type) ~ cell_type,
        grepl("sensory_ascending", super_class) & grepl("SA", cell_sub_type) ~ cell_sub_type,
        grepl("ascending|descending", cell_class) & !is.na(cell_sub_type) & cell_sub_type != "" ~ cell_sub_type,
        grepl("visual_projection", super_class) ~ cell_type,
        TRUE ~ NA
      ),
      seed_08 = dplyr::case_when(
        !.is_sensory ~ NA,
        grepl("proprio|tactile|contract|vib", .s_cf) ~ paste0(.s_cf, "_", .s_bp, "_", .s_cfn, "_", side),
        TRUE ~ NA
      ),
      seed_09 = dplyr::case_when(
        grepl("efferent", flow) ~ NA,
        is.na(peripheral_target_type) | peripheral_target_type == "" ~ NA,
        TRUE ~ peripheral_target_type
      ),
      seed_10 = dplyr::case_when(
        !.is_sensory | is.na(peripheral_target_type) | peripheral_target_type == "" ~ NA,
        TRUE ~ paste0(.s_bp, "_", peripheral_target_type)
      ),
      seed_11 = dplyr::case_when(
        !is.na(cluster) & cluster != "" ~ cluster,
        TRUE ~ NA
      ),
      seed_12 = dplyr::case_when(
        grepl("ascending|descending|visual_projection", super_class) ~ paste0(cell_type, "_", id),
        TRUE ~ NA
      ),
      seed_13 = id,
      seed_14 = dplyr::case_when(
        !is.na(cns_network) & cns_network != "" ~ cns_network,
        TRUE ~ NA
      )
    )

  # Clean semicolons from all seed columns
  meta <- meta %>%
    dplyr::mutate(dplyr::across(starts_with("seed_"), ~ gsub(";", "_", .)))

  # Remove temporary columns
  meta <- meta %>%
    dplyr::select(-dplyr::any_of(c(".s_cf", ".s_bp", ".s_cfd", ".s_cfn", ".is_sensory")))

  n_seeds <- sum(grepl("^seed_\\d+$", colnames(meta)))
  n_assigned <- sum(!is.na(meta$seed_07))
  n_cns <- sum(!is.na(meta$seed_14))
  message(sprintf("  Assigned %d seed columns (%d neurons have seed_07, %d have seed_14)", n_seeds, n_assigned, n_cns))

  meta
}

## ── Push seed columns to SeaTable ──────────────────────────────
## Uncomment and run interactively to write seed_00–seed_12
## back to the banc_meta SeaTable.  Requires banctable_update_rows().
##
# push_seeds_to_seatable <- function(meta) {
#   seed_cols <- grep("^seed_\\d+$", colnames(meta), value = TRUE)
#   seed_update <- meta %>%
#     dplyr::select(`_id`, dplyr::all_of(seed_cols)) %>%
#     dplyr::mutate(dplyr::across(dplyr::all_of(seed_cols), ~ dplyr::if_else(is.na(.), "", .))) %>%
#     base::as.data.frame()
#   message(sprintf("Pushing %d seed columns for %d rows to SeaTable...", length(seed_cols), nrow(seed_update)))
#   banctable_update_rows(base = 'banc_meta',
#                         table = 'banc_meta',
#                         df = seed_update,
#                         append_allowed = FALSE,
#                         chunksize = 1000)
#   message("Done.")
# }
