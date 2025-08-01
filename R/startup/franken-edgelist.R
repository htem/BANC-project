# Check
con <- DBI::dbConnect(RSQLite::SQLite(),
                      file.path(banc.dropbox.connectivity.save.path,"frankenbrain_v.1.6_data.sqlite"))
franken.edgelist.simple <- dplyr::tbl(con, "edgelist_simple") %>%
  dplyr::collect()
DBI::dbDisconnect(con)

# Add meta data to list
franken.edgelist.simple <- franken.edgelist.simple %>%
  dplyr::left_join(franken.meta.post %>% 
                     dplyr::select(post_id, post_top_nt, post_cluster, post_sez_class,
                                   post_side, post_region, post_super_class, 
                                   post_hemilineage, post_cell_function, post_nerve, 
                                   post_cell_class, post_cell_sub_class, post_cell_type) %>%
                     dplyr::distinct(post_id, .keep_all = TRUE),
                   by = c("post"="post_id")) %>%
  dplyr::left_join(franken.meta.pre %>% 
                     dplyr::select(pre_id, pre_top_nt, pre_cluster,pre_sez_class,
                                   pre_side, pre_region, pre_super_class, 
                                   pre_hemilineage, pre_cell_function, pre_nerve, 
                                   pre_cell_class, pre_cell_sub_class, pre_cell_type) %>%
                     dplyr::distinct(pre_id, .keep_all = TRUE),
                   by = c("pre"="pre_id"))
