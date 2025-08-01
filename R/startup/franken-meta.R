# # Check
# con <- DBI::dbConnect(RSQLite::SQLite(),
#                       file.path(banc.dropbox.connectivity.save.path,"frankenbrain_v.1.5_data.sqlite"))
# franken.meta <- dplyr::tbl(con, "meta") %>%
#   dplyr::collect() 
# dbDisconnect(con)
franken.orig <- bancr::franken_meta()

# Process
franken.meta.simple <- franken.meta <- franken.orig %>%
  dplyr::select(-starts_with("_")) %>%
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
  # filter out rows with NA values in neuron_id
  dplyr::filter(!is.na(neuron_id))

# Identify duplicated neuron_ids
duplicated_ids <- franken.meta$neuron_id[duplicated(franken.meta$neuron_id)]

# Split the dataframe into duplicates and non-duplicates
franken.meta_duplicates <- franken.meta %>% 
  dplyr::filter(neuron_id %in% duplicated_ids | grepl("neck",region)) %>%
  dplyr::arrange(cell_type, fafb_cell_type, manc_cell_type)
franken.meta_unique <- franken.meta %>% 
  dplyr::filter(!neuron_id %in% duplicated_ids|!grepl("neck",region))

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
  dplyr::mutate(composite_cell_type = dplyr::case_when(
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
  dplyr::group_by(composite_cell_type) %>%
  dplyr::mutate(multi_neuromere = length(unique(neuromere))>1) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(composite_cell_type = dplyr::case_when(
    is.na(neuromere)|neuromere=="" ~ composite_cell_type,
    multi_neuromere ~ paste0(composite_cell_type,"_",neuromere),
    TRUE ~ composite_cell_type
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
#   dplyr::distinct(cell_type, composite_cell_type, fafb_cell_type, manc_cell_type, 
#                   cluster, hemilineage, nerve, super_class, cell_class, cell_sub_class, neuromere, top_nt, neurotransmitter_verified, neurotransmitter_source) %>%
#   dplyr::arrange(super_class,cell_type,composite_cell_type) %>%
#   as.data.frame() 
# 
# # Upload
# banctable_append_rows(base='banc_meta', 
#                       table = 'neck_connective', 
#                       df = andn.cns.meta, 
#                       append_allowed = TRUE, 
#                       chunksize = 1000)  



