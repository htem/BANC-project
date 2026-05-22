###############################################################################
# franken-meta-live.R
#
# Live loader for the cross-dataset matching layer. Guarded: the live
# SeaTable fetch runs only when `franken.meta` is not yet defined or
# BANC_LIVE=1 forces a refresh. The dispatcher at R/startup/franken-meta.R
# loads a committed parquet snapshot first and skips the live block in
# that case. Derived objects (franken.vispn.meta, franken.eff.meta, etc.)
# run unconditionally at the tail.
###############################################################################

.live_needed <- !exists("franken.meta") || identical(Sys.getenv("BANC_LIVE"), "1")
if (.live_needed) {

# # Check
# con <- DBI::dbConnect(RSQLite::SQLite(),
#                       file.path(banc.dropbox.connectivity.save.path,"frankenbrain_v.1.5_data.sqlite"))
# franken.meta <- dplyr::tbl(con, "meta") %>%
#   dplyr::collect()
# dbDisconnect(con)
.franken_cache <- file.path("data", "cache", "franken_meta.feather")
franken.orig <- tryCatch(
  {
    .fm <- bancr::franken_meta()
    # Cache the live result so subsequent SeaTable hiccups have a fallback.
    tryCatch({
      if (!dir.exists(dirname(.franken_cache))) dir.create(dirname(.franken_cache), recursive = TRUE)
      arrow::write_feather(.fm, .franken_cache)
      message("Cached franken_meta to ", .franken_cache, " (", nrow(.fm), " rows)")
    }, error = function(e) message("Could not write franken_meta cache: ", conditionMessage(e)))
    .fm
  },
  error = function(e) {
    message("Could not load franken_meta from SeaTable: ", conditionMessage(e))
    if (file.exists(.franken_cache)) {
      message("Loading franken_meta from local cache: ", .franken_cache)
      arrow::read_feather(.franken_cache)
    } else {
      stop("franken_meta unavailable (SeaTable unreachable and no cache at ", .franken_cache, ")")
    }
  }
)
rm(.franken_cache)

# Process
# The franken_meta SeaTable schema dropped the unified `neuron_id` column
# in May 2026 and now exposes `fafb_id` and `manc_id` separately (one or
# both may be set per row — FAFB-only neurons have only fafb_id; matched
# pairs have both). Reconstruct a unified `neuron_id` by appending the two
# (NA-tolerant) so the rest of the pipeline (id, dedup, joins) is
# unchanged.
.append_ids <- function(a, b) {
  a <- ifelse(is.na(a) | a == "", "", as.character(a))
  b <- ifelse(is.na(b) | b == "", "", as.character(b))
  out <- ifelse(a != "" & b != "", paste(a, b, sep = "_"),
                ifelse(a != "", a, ifelse(b != "", b, NA_character_)))
  out
}
franken.meta.simple <- franken.meta <- franken.orig %>%
  dplyr::select(-starts_with("_")) %>%
  dplyr::mutate(neuron_id = .append_ids(fafb_id, manc_id)) %>%
  dplyr::mutate(id = neuron_id) %>%
  # fix some errors
  dplyr::mutate(region = gsub(",","",region)) %>%
  dplyr::mutate(super_class = gsub(",","",super_class)) %>%
  dplyr::mutate(cell_function = gsub(",","",cell_function)) %>%
  dplyr::mutate(cell_function = gsub(",","",cell_function)) %>%
  dplyr::rename(manc_cell_type = MANC_type,
                fafb_cell_type = FAFB_cell_type) %>%
  dplyr::select(-starts_with("MANC_",ignore.case = FALSE)) %>%
  dplyr::select(-starts_with("FAFB_",ignore.case = FALSE)) %>%
  # filter out rows with NA values in neuron_id (no fafb_id and no manc_id)
  dplyr::filter(!is.na(neuron_id))

# Identify duplicated neuron_ids
duplicated_ids <- franken.meta$neuron_id[duplicated(franken.meta$neuron_id)]

# Split the dataframe into duplicates and non-duplicates
franken.meta_duplicates <- franken.meta %>%
  dplyr::filter(neuron_id %in% duplicated_ids | grepl("neck",region)) %>%
  dplyr::arrange(cell_type, fafb_cell_type, manc_cell_type)
franken.meta_unique <- franken.meta %>%
  dplyr::filter(!neuron_id %in% duplicated_ids & !grepl("neck",region))

# Process only the duplicates
franken.meta_duplicates <- franken.meta_duplicates %>%
  dplyr::group_by(neuron_id) %>%
  dplyr::reframe(across(everything(), ~ {
    if (is.character(.)) {
      paste(unique(na.omit(.)), collapse = ";")
    } else if (is.numeric(.)) {
      sum(., na.rm = TRUE)
    } else if (is.logical(.)) {
      any(., na.rm = TRUE)
    } else {
      first(na.omit(.))
    }
  }))
non_empty_ids <- franken.meta_duplicates$banc_id != ""
#franken.meta_duplicates$banc_id[non_empty_ids] <- bancr::banc_updateids(franken.meta_duplicates$banc_id[non_empty_ids])
franken.meta_duplicates <- franken.meta_duplicates %>%
  dplyr::mutate(id = ifelse(is.na(banc_id),id,banc_id))

# Combine the processed duplicates with the non-duplicates
franken.meta <- dplyr::bind_rows(franken.meta_unique,franken.meta_duplicates) %>%
  dplyr::mutate(neuromere = gsub(";.*","",neuromere),
                manc_cell_type = gsub(";.*","",manc_cell_type),
                fafb_cell_type = gsub(";.*","",fafb_cell_type)) %>%
  dplyr::mutate(cell_type = dplyr::case_when(
    is.na(cell_type)|cell_type=="" ~ cell_type,
    (grepl("ascending",super_class))&!is.na(manc_cell_type)&manc_cell_type!="" ~ gsub("\\;.*","",manc_cell_type),
    (grepl("descending",super_class))&!is.na(fafb_cell_type)&fafb_cell_type!="" ~ gsub("\\;.*","",fafb_cell_type),
    TRUE ~ gsub("\\;.*","",cell_type)
  )) %>%
  dplyr::mutate(cell_sub_type = dplyr::case_when(
    is.na(cell_type)|cell_type=="" ~ cell_type,
    !is.na(fafb_cell_type)|fafb_cell_type=="" ~ cell_type,
    !is.na(manc_cell_type)|manc_cell_type=="" ~ cell_type,
    # (!is.na(fafb_cell_type)&!is.na(manc_cell_type)) & (fafb_cell_type==manc_cell_type) ~ fafb_cell_type,
    # (!is.na(fafb_cell_type)&!is.na(manc_cell_type)) & (grepl("ascending",super_class)) ~ paste0(manc_cell_type,"_",fafb_cell_type),
    # (!is.na(fafb_cell_type)&!is.na(manc_cell_type)) & (grepl("descending",super_class)) ~ paste0(fafb_cell_type,"_",manc_cell_type),
    # (!is.na(fafb_cell_type)&!is.na(manc_cell_type)) ~ paste0(fafb_cell_type,"_",manc_cell_type),
    grepl("ascending",super_class)&!is.na(manc_cell_type) ~ manc_cell_type,
    grepl("descending",super_class)&!is.na(fafb_cell_type) ~ fafb_cell_type,
    TRUE ~ cell_type
  )) %>%
  # make composite cell type
  dplyr::ungroup() %>%
  dplyr::group_by(cell_sub_type) %>%
  dplyr::mutate(multi_neuromere = length(unique(neuromere))>1) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(cell_sub_type = dplyr::case_when(
    is.na(neuromere)|neuromere=="" ~ cell_sub_type,
    multi_neuromere ~ paste0(cell_sub_type,"_",neuromere),
    TRUE ~ cell_sub_type
  )) %>%
  dplyr::ungroup()

# # join
# franken.meta <- dplyr::left_join(franken.meta,
#                           classes.dn.df %>%
#                             dplyr::select(id, dn_cluster = cluster) %>%
#                             dplyr::distinct(id, .keep_all = TRUE),
#                           by = c('id')) %>%
#   dplyr::left_join(franken.meta,
#                           classes.an.df %>%
#                             dplyr::select(id, an_cluster = cluster) %>%
#                             dplyr::distinct(id, .keep_all = TRUE),
#                           by = c('id'))  %>%
#   dplyr::left_join(franken.meta,
#                    classes.eff.df %>%
#                      dplyr::select(id, eff_cluster = cluster) %>%
#                      dplyr::distinct(id, .keep_all = TRUE),
#                    by = c('id'))  %>%
#   dplyr::mutate(cluster = dplyr::case_when(
#     !is.na(an_cluster)&grepl("ascending",super_class) ~ an_cluster,
#     !is.na(dn_cluster)&grepl("descending",super_class) ~ dn_cluster,
#     !is.na(dn_cluster)&grepl("efferent",super_class) ~ eff_cluster,
#     TRUE ~ NA
#   )) %>%
#   dplyr::select(-an_cluster,-dn_cluster, -eff_cluster)

# ----- End of live block: write the dated snapshot ----------------------
# Atomic write: write to <name>.tmp.<pid> first, then rename. Without this,
# a script that crashes mid-write — or two scripts that race to refresh the
# snapshot in the same session — leave behind a half-written .parquet that
# pyarrow / arrow can't read ("Unexpected end of stream"). The rename is
# atomic at the filesystem level on the same volume, so the final path
# either has the complete snapshot or its previous version.
tryCatch({
  .snap_out <- file.path("data", "meta",
                         sprintf("franken_meta_%s.parquet",
                                 format(Sys.Date(), "%Y%m%d")))
  .snap_tmp <- paste0(.snap_out, ".tmp.", Sys.getpid())
  arrow::write_parquet(franken.meta, .snap_tmp, compression = "snappy")
  file.rename(.snap_tmp, .snap_out)
  message(sprintf("Wrote franken.meta snapshot %s (%.1f MB)",
                  .snap_out, file.info(.snap_out)$size / 1024^2))
  rm(.snap_out, .snap_tmp)
}, error = function(e) message("franken snapshot write failed: ", conditionMessage(e)))

}  # end if(.live_needed)
rm(.live_needed)

# ============================================================================
# Derived setup — runs unconditionally on both snapshot and live paths.
# ============================================================================

# Join franken.meta data for other labels
franken.meta.pre <- franken.meta
colnames(franken.meta.pre) <- paste0("pre_",colnames(franken.meta.pre))
franken.meta.post <- franken.meta
colnames(franken.meta.post) <- paste0("post_",colnames(franken.meta.post))

# Useful franken groups
franken.vispn.meta <- franken.meta %>%
  dplyr::filter(grepl("visual_projection",super_class))
franken.vispn.ids <- unique(franken.vispn.meta$id)
franken.vispn.cts <- unique(franken.vispn.meta$cell_type)
franken.vispn.ccs <- extract_three_letters(unique(franken.vispn.meta$cell_type))

franken.viscent.meta <- franken.meta %>%
  dplyr::filter(grepl("visual_centrifugal",super_class))
franken.viscent.ids <- unique(franken.viscent.meta$id)
franken.viscent.cts <- unique(franken.viscent.meta$cell_type)
franken.viscent.ccs <- extract_three_letters(unique(franken.viscent.meta$cell_type))

franken.eff.meta <- franken.meta %>%
  dplyr::filter(grepl("motor|efferent|endocrine",super_class))
franken.efferent.ids <- unique(franken.eff.meta$id)

##################
### NECK TABLE ###
##################

# # Wrangle
# andn.cns.meta <- franken.meta %>%
#   dplyr::filter(region=="neck_connective") %>%
#   dplyr::distinct(cell_type, cell_sub_type, fafb_cell_type, manc_cell_type, 
#                   cluster, hemilineage, nerve, super_class, cell_class, cell_sub_class, neuromere, top_nt, neurotransmitter_verified, neurotransmitter_source) %>%
#   dplyr::arrange(super_class,cell_type,cell_sub_type) %>%
#   as.data.frame() 
# 
# # Upload
# banctable_append_rows(base='banc_meta', 
#                       table = 'neck_connective', 
#                       df = andn.cns.meta, 
#                       append_allowed = TRUE, 
#                       chunksize = 1000)  



