# Get connectivity
if(is.null(banc.version)){
  banc.el <- banc_edgelist()
  banc.edgelist.simple <- banc.el %>%
    dplyr::mutate(pre = as.character(pre_pt_root_id),
                  post = as.character(post_pt_root_id)) %>%
    dplyr::rename(count = n) %>%
    dplyr::group_by(pre_pt_root_id) %>%
    dplyr::mutate(pre_count = sum(count, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(post_pt_root_id) %>%
    dplyr::mutate(post_count = sum(count, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(norm = round(count/post_count,6)) %>%
    dplyr::filter(pre_pt_root_id!=post_pt_root_id) %>%
    dplyr::arrange(dplyr::desc(count)) %>%
    dplyr::distinct()
}else{
  con <- DBI::dbConnect(RSQLite::SQLite(),
                        file.path(banc.dropbox.connectivity.save.path,"banc_572_data.sqlite"))
  banc.edgelist.simple <- dplyr::tbl(con, "edgelist_simple") %>%
    dplyr::collect()
  dbDisconnect(con)
}

# Add meta data to list
banc.edgelist.simple <- banc.edgelist.simple %>%
  dplyr::left_join(banc.meta.post %>% 
                     dplyr::select(post_id, post_top_nt, post_cluster,
                                   post_side, post_region, post_super_class, 
                                   post_hemilineage, post_cell_function, post_nerve, 
                                   post_cell_class, post_cell_sub_class, post_cell_type, post_composite_cell_type) %>%
                     dplyr::distinct(post_id, .keep_all = TRUE),
                   by = c("post"="post_id")) %>%
  dplyr::left_join(banc.meta.pre %>% 
                       dplyr::select(pre_id, pre_top_nt, pre_cluster,
                                     pre_side, pre_region, pre_super_class, 
                                     pre_hemilineage, pre_cell_function, pre_nerve, 
                                     pre_cell_class, pre_cell_sub_class, pre_cell_type, pre_composite_cell_type) %>%
                       dplyr::distinct(pre_id, .keep_all = TRUE),
                     by = c("pre"="pre_id"))

# Write and save
if(Sys.info()['effective_user']=="ab714"){
  banc.connectivity.save.path="/n/data1/hms/neurobio/wilson/banc/connectivity"
  readr::write_csv(banc.edgelist.simple, file = file.path(banc.connectivity.save.path,"banc_edgelist_simple.csv"))
  remote_name <- "hms"
  remote_path <- "neuroanat/connectomes/"  # Replace with your desired path on Dropbox
  system(paste("rclone copy", file.path(banc.connectivity.save.path,"banc_edgelist_simple.csv"), paste0(remote_name, ":", remote_path)))
  cat("File transfer complete.\n")    
}



