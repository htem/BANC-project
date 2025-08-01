####################
### compile data ###
####################

# Get meta
franken.chosen.meta <- franken.meta %>%
  dplyr::filter(grepl("CX_output|CX_input|MBON|DAN",cell_class)|
                grepl("motor|efferent|endocrine|visual_projection|visual_centrifugal|ascending|descending",super_class))
franken.chosen.ids <- unique(franken.chosen.meta$id)

# Connect to .sql file
con <- DBI::dbConnect(RSQLite::SQLite(),
                      file.path(banc.dropbox.influence.save.path,influence.sqlite))
influence.neck.meta <- dplyr::tbl(con, influence.table) %>%
  dplyr::filter(!is_seed,
                level %in% c("seed_1","seed_3", "seed_4", "seed_5", "seed_7"),
                id %in% !!franken.chosen.ids) %>%
  dplyr::collect() %>%
  dplyr::mutate(resolution = dplyr::case_when(
    level=="seed_1" ~ "forward_coarse",
    level=="seed_3" ~ "forward_fine",
    level=="seed_4" ~ "forward_sided",
    level=="seed_5" ~ "reverse_coarse",
    level=="seed_7" ~ "reverse_fine",
    TRUE ~ NA
  ))
DBI::dbDisconnect(con)

# Format
influence.neck.meta  <- influence.neck.meta %>%
  dplyr::left_join(franken.meta.post %>%
                     dplyr::select(post_id, post_side, post_cluster, post_body_part_sensory, post_body_part_effector,
                                   post_region, post_super_class, post_hemilineage, post_sez_class,
                                   post_cell_function, post_nerve, post_cell_class, post_cell_sub_class, 
                                   post_cell_type, post_composite_cell_type, post_top_nt) %>%
                     dplyr::distinct(post_id, .keep_all = TRUE),
                   by = c("id"="post_id")) %>%
  dplyr::left_join(franken.meta.pre %>%
                     dplyr::mutate(seed = pre_cell_type) %>%
                     dplyr::select(pre_side, pre_cluster, pre_body_part_sensory, pre_body_part_effector,
                                   pre_region, pre_super_class, pre_hemilineage, 
                                   pre_cell_function, pre_nerve, pre_cell_class, pre_cell_sub_class, 
                                   pre_cell_type, pre_composite_cell_type, pre_top_nt, seed) %>%
                     dplyr::distinct(pre_cell_type, .keep_all = TRUE),
                   by = c("seed")) %>%
  dplyr::left_join(cns.functions %>%
                     dplyr::select(pre_cell_type = cell_type, 
                                   pre_cell_function = response) %>%
                     dplyr::distinct(pre_cell_type, .keep_all = TRUE),
                   by = c("pre_cell_type")) %>%
  dplyr::left_join(cns.functions %>%
                     dplyr::select(post_cell_type = cell_type, 
                                   post_cell_function = response) %>%
                     dplyr::distinct(post_cell_type, .keep_all = TRUE),
                   by = c("post_cell_type")) %>%
  dplyr::filter(!grepl("afferent|sensory|glia",post_super_class)) %>%
  dplyr::mutate(super_class = case_when(
    post_super_class == "brain_central_other" & !is.na(post_sez_class) ~ "sez",
    TRUE ~ post_super_class
  )) %>%
  dplyr::mutate(post_cell_type = ifelse(grepl("KCg-s",post_cell_type),"KCg-s",post_cell_type)) %>%
  dplyr::mutate(post_cell_sub_class = ifelse(is.na(post_cell_sub_class),gsub("_.*","",post_cell_type),post_cell_sub_class)) %>%
  dplyr::group_by(id, level) %>%
  dplyr::mutate(influence_mad2 = as.vector((influence - median(influence, na.rm = TRUE))/stats::mad(influence, na.rm = TRUE)),
                influence_zscore2 = as.vector(scale(influence, center = TRUE, scale = TRUE))
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(influence = signif(influence,6),
                influence_log = signif(influence_log,6),
                influence_norm = signif(influence_norm,6),
                influence_norm_log = signif(influence_norm_log,6),
                influence_mad = signif(influence_mad,6),
                influence_mad2 = signif(influence_mad2,6),
                influence_mad3 = signif(influence_mad3,6),
                influence_zscore = signif(influence_zscore,6),
                influence_zscore2 = signif(influence_zscore2,6),
                influence_zscore3 = signif(influence_zscore3,6)
  )












