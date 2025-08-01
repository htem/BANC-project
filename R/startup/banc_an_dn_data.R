#########################################################
### READ DATA RELEVANT TO AN/DN ANALYSIS NEUROANATOMY ###
#########################################################

# Get AN/DN functions
if(exists("cns.functions")){
  andn.functions <- cns.functions %>%
    dplyr::filter(grepl("ascending|descending",super_class)) 
}

# Get AN-DN franken meta
if(exists("franken.meta")){
  
  # choose neck neurons
  franken.neck.meta <- franken.meta %>%
    dplyr::filter(grepl("descending|ascending",super_class))
  
  # join
  franken.neck.meta <- dplyr::left_join(franken.neck.meta,
                                 umap.dn.df %>%
                                   dplyr::filter(!is.na(cell_type)) %>%
                                   dplyr::select(cell_type, side, dn_cluster = cluster) %>%
                                   dplyr::distinct(cell_type, side, .keep_all = TRUE),
                                 by = c('cell_type',"side"))
  # # join
  # franken.neck.meta <- dplyr::left_join(franken.neck.meta,
  #                                umap.an.df %>%
  #                                  dplyr::select(cell_type, side, an_cluster = cluster) %>%
  #                                  dplyr::distinct(cell_type, .keep_all = TRUE),
  #                                by = c('cell_type','side'))  %>%
  #   dplyr::select(-an_cluster,-dn_cluster)
  
}

# BANC neck meta
if(exists("banc.meta")){
  banc.neck.meta <- banc.meta %>%
    dplyr::filter(region=="neck_connective") %>%
    dplyr::filter(!is.na(cell_type)) %>%
    dplyr::arrange(cell_type, fafb_cell_type, manc_cell_type,
                   top_nt,
                   side) %>%
    dplyr::distinct(root_id, .keep_all = TRUE) %>%
    dplyr::mutate(top_nt = dplyr::case_when(
      is.na(top_nt) ~ "unclear",
      top_nt%in%c("NA","na","unknown","Unclear","Unknown","none","0") ~ "unclear",
      TRUE ~ top_nt
    )) %>%
    dplyr::mutate(side = dplyr::case_when(
      is.na(side) ~ "midline",
      !side%in%c("right","left","midline") ~ "midline",
      TRUE ~ side
    )) %>%
    dplyr::mutate(cell_sub_class = dplyr::case_when(
      super_class=="sensory_ascending" ~ cell_type,
      TRUE ~ cell_sub_class
    )) %>%
    dplyr::distinct(root_id, root_position_nm, side, super_class, cell_class, 
                    pd_width, input_connections, output_connections, input_side_index, output_side_index,
                    cell_type, composite_cell_type, fafb_cell_type, manc_cell_type, cell_sub_class, 
                    top_nt, neurotransmitter_verified, cluster,
                    fafb_match, manc_match)
  
  # Pre and post, for joining
  banc.neck.meta.pre <- banc.neck.meta
  colnames(banc.neck.meta.pre) <- paste0("pre_",colnames(banc.neck.meta))
  banc.neck.meta.post <- banc.neck.meta
  colnames(banc.neck.meta.post) <- paste0("post_",colnames(banc.neck.meta))
}

# Subset connectivity
if(exists("banc.edgelist.simple")){
  
  # Neck edgelist
  banc.neck.el <- banc.edgelist.simple %>%
    # dplyr::mutate(pre = as.character(pre_pt_root_id),
    #               post = as.character(post_pt_root_id))  %>%
    dplyr::filter(!is.na(post_cell_type), 
                  !is.na(pre_cell_type)) %>%
    dplyr::filter(grepl("ascending|descending",pre_super_class),
                  grepl("ascending|descending",post_super_class))
  
  # Make DN edgelist
  dn.elist.pre <- banc.edgelist.simple %>%
    dplyr::filter(pre_super_class %in% c("descending")) %>%
    dplyr::left_join(franken.meta.post %>% 
                       dplyr::select(post_composite_cell_type,
                                     post_body_part_effector,
                                     post_body_part_sensory,
                                     post_origin) %>%
                       dplyr::distinct(post_composite_cell_type, .keep_all = TRUE),
                     by = c("post_composite_cell_type"))   %>%
    dplyr::mutate(targeting = dplyr::case_when(
      pre_side=="right"&post_side=="left" ~ "contra",
      pre_side=="left"&post_side=="right" ~ "ipsi",
      TRUE ~ "ipsi"
    ))
  dn.elist.post <- banc.edgelist.simple %>%
    dplyr::filter(post_super_class %in% c("descending","efferent_descending")) %>%
    dplyr::left_join(franken.meta.pre %>% 
                       dplyr::select(pre_composite_cell_type,
                                     pre_body_part_effector,
                                     pre_body_part_sensory,
                                     pre_origin) %>%
                       dplyr::distinct(pre_composite_cell_type, .keep_all = TRUE),
                     by = c("pre_composite_cell_type"))   %>%
    dplyr::mutate(targeting = dplyr::case_when(
      pre_side=="right"&post_side=="left" ~ "contra",
      pre_side=="left"&post_side=="right" ~ "ipsi",
      TRUE ~ "ipsi"
    ))
  
  # Make AN edgelist
  an.elist.pre <- banc.edgelist.simple %>%
    dplyr::filter(pre_super_class %in% c("ascending")) %>%
    dplyr::left_join(franken.meta.post %>% 
                       dplyr::select(post_composite_cell_type,
                                     post_body_part_effector,
                                     post_body_part_sensory,
                                     post_origin) %>%
                       dplyr::distinct(post_composite_cell_type, .keep_all = TRUE),
                     by = c("post_composite_cell_type")) %>%
    dplyr::mutate(targeting = dplyr::case_when(
      pre_side=="right"&post_side=="left" ~ "contra",
      pre_side=="left"&post_side=="right" ~ "ipsi",
      TRUE ~ "ipsi"
    ))
  an.elist.post <- banc.edgelist.simple %>%
    dplyr::filter(post_super_class %in% c("ascending","efferent_ascending")) %>%
    dplyr::left_join(franken.meta.pre %>% 
                       dplyr::select(pre_composite_cell_type,
                                     pre_body_part_effector,
                                     pre_body_part_sensory,
                                     pre_origin) %>%
                       dplyr::distinct(pre_composite_cell_type, .keep_all = TRUE),
                     by = c("pre_composite_cell_type"))  %>%
    dplyr::mutate(targeting = dplyr::case_when(
      pre_side=="right"&post_side=="left" ~ "contra",
      pre_side=="left"&post_side=="right" ~ "ipsi",
      TRUE ~ "ipsi"
    ))
  
  # Create a matrix of undirected connections
  dn.elist.cat <- rbind(dn.elist.pre %>%
                          dplyr::mutate(pre_composite_cell_type = dplyr::case_when(
                            !is.na(pre_composite_cell_type) ~ pre_composite_cell_type,
                            TRUE ~ pre, 
                          )) %>%
                          dplyr::mutate(post_composite_cell_type = dplyr::case_when(
                            !is.na(post_composite_cell_type) ~ post_composite_cell_type,
                            TRUE ~ post, 
                          )) %>%
                          dplyr::group_by(pre, post_composite_cell_type) %>%
                          dplyr::mutate(count = sum(count, na.rm = TRUE),
                                        norm = mean(norm, na.rm = TRUE)) %>%
                          dplyr::ungroup() %>%
                          #dplyr::mutate(id = pre_composite_cell_type, partner_id = paste0("post_",post)) %>%
                          dplyr::mutate(id = pre, partner_id = paste0("post_",post_composite_cell_type)) %>%
                          dplyr::distinct(id, partner_id, count, norm),
                        dn.elist.post %>%
                          dplyr::mutate(post_composite_cell_type = dplyr::case_when(
                            !is.na(post_composite_cell_type) ~ post_composite_cell_type,
                            TRUE ~ post, 
                          )) %>%
                          dplyr::mutate(pre_composite_cell_type = dplyr::case_when(
                            !is.na(pre_composite_cell_type) ~ pre_composite_cell_type,
                            TRUE ~ pre, 
                          )) %>%
                          dplyr::group_by(post, pre_composite_cell_type) %>%
                          dplyr::mutate(count = sum(count, na.rm = TRUE),
                                        norm = mean(norm, na.rm = TRUE)) %>%
                          dplyr::ungroup() %>%
                          #dplyr::mutate(id = post_composite_cell_type, partner_id = paste0("pre_",pre)) %>%
                          dplyr::mutate(id = post, partner_id = paste0("pre_",pre_composite_cell_type)) %>%
                          dplyr::distinct(id, partner_id, count, norm),
                        an.elist.pre %>%
                          dplyr::mutate(pre_composite_cell_type = dplyr::case_when(
                            !is.na(pre_composite_cell_type) ~ pre_composite_cell_type,
                            TRUE ~ pre, 
                          )) %>%
                          dplyr::mutate(post_composite_cell_type = dplyr::case_when(
                            !is.na(post_composite_cell_type) ~ post_composite_cell_type,
                            TRUE ~ post, 
                          )) %>%
                          dplyr::group_by(pre, post_composite_cell_type) %>%
                          dplyr::mutate(count = sum(count, na.rm = TRUE),
                                        norm = mean(norm, na.rm = TRUE)) %>%
                          dplyr::ungroup() %>%
                          #dplyr::mutate(id = pre_composite_cell_type, partner_id = paste0("post_",post)) %>%
                          dplyr::mutate(id = pre, partner_id = paste0("post_",post_composite_cell_type)) %>%
                          dplyr::distinct(id, partner_id, count, norm),
                        an.elist.post %>%
                          dplyr::mutate(post_composite_cell_type = dplyr::case_when(
                            !is.na(post_composite_cell_type) ~ post_composite_cell_type,
                            TRUE ~ post, 
                          )) %>%
                          dplyr::mutate(pre_composite_cell_type = dplyr::case_when(
                            !is.na(pre_composite_cell_type) ~ pre_composite_cell_type,
                            TRUE ~ pre, 
                          )) %>%
                          dplyr::group_by(post, pre_composite_cell_type) %>%
                          dplyr::mutate(count = sum(count, na.rm = TRUE),
                                        norm = mean(norm, na.rm = TRUE)) %>%
                          dplyr::ungroup() %>%
                          #dplyr::mutate(id = post_composite_cell_type, partner_id = paste0("pre_",pre)) %>%
                          dplyr::mutate(id = post, partner_id = paste0("pre_",pre_composite_cell_type)) %>%
                          dplyr::distinct(id, partner_id, count, norm)) 
  
  
  # Create a matrix of undirected connections
  an.elist.cat <- rbind(an.elist.pre %>%
                          dplyr::mutate(pre_composite_cell_type = dplyr::case_when(
                            !is.na(pre_composite_cell_type) ~ pre_composite_cell_type,
                            TRUE ~ pre, 
                          )) %>%
                          dplyr::group_by(pre_composite_cell_type, post) %>%
                          dplyr::mutate(count = sum(count, na.rm = TRUE),
                                        norm = mean(norm, na.rm = TRUE)) %>%
                          dplyr::ungroup() %>%
                          dplyr::mutate(id = pre_composite_cell_type, partner_id = paste0("post_",post)) %>%
                          dplyr::distinct(id, partner_id, count, norm),
                        an.elist.post %>%
                          dplyr::mutate(post_composite_cell_type = dplyr::case_when(
                            !is.na(post_composite_cell_type) ~ post_composite_cell_type,
                            TRUE ~ post, 
                          )) %>%
                          dplyr::group_by(post_composite_cell_type, pre) %>%
                          dplyr::mutate(count = sum(count, na.rm = TRUE),
                                        norm = mean(norm, na.rm = TRUE)) %>%
                          dplyr::ungroup() %>%
                          dplyr::mutate(id = post_composite_cell_type, partner_id = paste0("pre_",pre)) %>%
                          dplyr::distinct(id, partner_id, count, norm)) %>%
    dplyr::distinct(id, partner_id, count, norm)
}




