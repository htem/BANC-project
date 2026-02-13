##################################
## SUPPLEMENTAL DATA GENERATION ##
##################################
# Generate CSV files for paper supplementary data tables
# Exports neuron metadata for FAFB, MANC, and BANC datasets
source("R/startup/banc-startup.R")
banc.meta <- banctable_query()

# Supplementary data table 1: annotations list
banc_meta_in_626 <- banc.meta %>%
  dplyr::filter(!grepl("DEBRIS",status),
                !super_class %in% c("not_a_neuron","debris")) %>%
  dplyr::filter(!is.na(root_626) & !is.na(cell_ids_id_626)) %>%
  dplyr::select(root_id, root_626, supervoxel_id, position, nucleus_id, nucleus_position, 
                proofread, roughly_proofread,
                flow, super_class, cell_class, cell_sub_class, region, side, 
                cell_function, cell_function_detailed, peripheral_target_type, 
                body_part_sensory, 
                body_part_effector, 
         nerve, 
         hemilineage, 
         sexually_dimorphic,
         neurotransmitter_verified, 
         neuropeptide_verified, 
         neurotransmitter_predicted,
         fafb_match,
         hemibrain_match,
         manc_match,
         fanc_match)
banc_meta_in_626$root_id <- banc_rootid(banc_meta_in_626$supervoxel_id, version=821)
readr::write_csv(banc_meta_in_626,
                 file = "manuscript/resubmission_2/exports/banc_meta_v821.csv")

# Function to split comma-separated values and get unique terms
get_unique_split_terms <- function(column) {
  # Remove NA and empty strings first
  clean_values <- column[!is.na(column) & column != ""]
  
  # Split by comma and flatten
  split_values <- unlist(str_split(clean_values, ","))
  
  # Trim whitespace and remove empty strings again
  trimmed_values <- str_trim(split_values)
  final_values <- trimmed_values[trimmed_values != ""]
  
  # Return sorted unique values
  sort(unique(final_values))
}

# Apply the function to each column
unique_terms_list <- banc_meta_in_626 %>%
  dplyr::select(flow, super_class, cell_class, cell_sub_class, region, side, 
                cell_function, cell_function_detailed, peripheral_target_type, 
                body_part_sensory, body_part_effector, nerve, hemilineage, 
                neurotransmitter_verified, neuropeptide_verified, neurotransmitter_predicted) %>%
  map(get_unique_split_terms)

# Find the maximum length to pad shorter columns
max_length <- max(map_int(unique_terms_list, length))

# Pad shorter columns with NA to make them all the same length
annotation_terms <- unique_terms_list %>%
  map(~ c(.x, rep(NA, max_length - length(.x)))) %>%
  as_tibble()

# # Write to CSV file
# annotation_terms_filepath <- "/Users/hyang/HMS Dropbox/Helen Yang/BANC-project/manuscript/resubmission_2/supplemental_data/supplemental_data_1.csv"
# readr::write_csv(annotation_terms, annotation_terms_filepath, na = "")

# Export FAFB (brain) neuron metadata
fw.meta <- franken.meta %>%
  dplyr::filter(!is.na(fafb_id)) %>%
  dplyr::select(root_783 = fafb_id,
                nerve, 
                hemilineage,
                region,
                neurotransmitter_predicted = top_nt,
                neurotransmitter_verified,
                super_class,
                cell_class,
                cell_sub_class,
                cell_type,
                cell_function,
                cell_function_detailed,
                sexually_dimorphic)
readr::write_csv(fw.meta,
                 file = "manuscript/resubmission_2/exports/redrafted_fafb_meta_data.csv")
readr::write_csv(fw.meta,
                 file = "manuscript/resubmission_2/supplemental_data/supplemental_data_2.csv")
# MANC data
mc.meta <- franken.meta %>%
  dplyr::filter(!is.na(manc_id))  %>%
  dplyr::select(bodyid = manc_id,
                nerve, 
                hemilineage,
                region,
                neurotransmitter_predicted = top_nt,
                neurotransmitter_verified,
                super_class,
                cell_class,
                cell_sub_class,
                cell_type,
                cell_function,
                cell_function_detailed,
                sexually_dimorphic)
readr::write_csv(mc.meta,
                 file = "manuscript/resubmission_2/exports/redrafted_manc_meta_data.csv")
readr::write_csv(mc.meta,
                 file = "manuscript/resubmission_2/supplemental_data/supplemental_data_3.csv")

# # Hemibrain 
# hb.meta <- franken_meta(sql = "SELECT * FROM hemibrain")  %>%
#   dplyr::select(bodyid = hemibrain_121_id,
#                 nerve, 
#                 hemilineage,
#                 region,
#                 neurotransmitter_predicted,
#                 neurotransmitter_verified,
#                 super_class,
#                 cell_class,
#                 cell_sub_class,
#                 cell_type,
#                 cell_function,
#                 cell_function_detailed)
# readr::write_csv(hb.meta,
#                  file = "manuscript/resubmission_2/exports/redrafted_hemibrain_meta_data.csv")
# readr::write_csv(hb.meta,
#                  file = "manuscript/resubmission_2/supplemental_data/supplemental_data_4.csv")

# # maleCNS
# malecns.meta <- franken_meta(sql = "SELECT * FROM malecns")  %>%
#   dplyr::select(bodyid = malecns_09_id,
#                 nerve, 
#                 hemilineage,
#                 region,
#                 neurotransmitter_predicted,
#                 neurotransmitter_verified,
#                 super_class,
#                 cell_class,
#                 cell_sub_class,
#                 cell_type,
#                 cell_function,
#                 cell_function_detailed)
# readr::write_csv(malecns.meta,
#                  file = "manuscript/resubmission_2/exports/redrafted_hemibrain_meta_data.csv")
# readr::write_csv(malecns.meta,
#                  file = "manuscript/resubmission_2/supplemental_data/supplemental_data_5.csv")

# UMAP neck
classes.nn.df <- read_csv(file = "data/banc_annotations/banc_neck_functional_classes.csv", col_types = banc.col.types)
classes.nn.df$root_id <- banc_latestid(classes.nn.df$id)
classes.nn.df <- classes.nn.df %>%
  dplyr::select(root_id,
                UMAP1,
                UMAP2) %>%
  dplyr::left_join(banc.meta %>%
                     dplyr::select(root_id,
                                   root_626,
                                   supervoxel_id,
                                   position,
                                   side,
                                   region,
                                   super_class,
                                   hemilineage,
                                   cell_function,
                                   nerve,
                                   cell_type,
                                   fafb_cell_type,
                                   manc_cell_type,
                                   super_cluster,
                                   cns_network),
                   by = "root_id")
readr::write_csv(classes.nn.df,
                 file = "manuscript/resubmission_2/exports/neck_neuron_direct_connectivity_cosine_umap.csv")
readr::write_csv(classes.nn.df,
                 file = "manuscript/resubmission_2/supplemental_data/supplemental_data_4.csv")

# Efferent UMAp
classes.eff.df <- read_csv(file = "data/banc_annotations/banc_efferent_functional_classes.csv", col_types = banc.col.types)
classes.eff.df$root_id <- banc_latestid(classes.eff.df$id)
classes.eff.df <- classes.eff.df %>%
  dplyr::select(root_id,
                UMAP1,
                UMAP2) %>%
  dplyr::left_join(banc.meta %>%
                     dplyr::select(root_id,
                                   root_626,
                                   supervoxel_id,
                                   position,
                                  side,
                                  region,
                                  flow,
                                  super_class,
                                  hemilineage,
                                  cell_function,
                                  nerve,
                                  cell_type,
                                  fafb_cell_type,
                                  manc_cell_type,
                                  cluster,
                                  super_cluster),
                   by = "root_id")
readr::write_csv(classes.eff.df,
                 file = "manuscript/resubmission_2/exports/efferent_from_neck_influence_clusters_cosine_umap.csv")
readr::write_csv(classes.eff.df,
                 file = "manuscript/resubmission_2/supplemental_data/supplemental_data_5.csv")

# CNS network
cns.network.umap <- readr::read_csv("data/cns_network/spectral_clustering_min_connection_strength_1_banc_version_626_cluster_count_13_cluster_seed_10_embedding_seed_3.csv", 
                                    col_types = banc.col.types)
cns.network.umap <-cns.network.umap %>%
  dplyr::mutate(cns_network = paste0("CNS_",str_pad(spectral_cluster,width = 2,pad =0))) %>%
  dplyr::distinct(root_id, UMAP1=umap_x,UMAP2=umap_y)
cns.network.umap$root_id <- banc_latestid(cns.network.umap$root_id)
cns.network.umap <- cns.network.umap %>%
  dplyr::select(root_id,
                UMAP1,
                UMAP2) %>%
  dplyr::left_join(banc.meta %>%
                     dplyr::select(root_id,
                                   root_626,
                                   supervoxel_id,
                                   position,
                                   side,
                                   region,
                                   super_class,
                                   hemilineage,
                                   cell_function,
                                   nerve,
                                   cell_type,
                                   fafb_cell_type,
                                   manc_cell_type,
                                   cluster,
                                   super_cluster,
                                   cns_network),
                   by = "root_id")
readr::write_csv(cns.network.umap,
                 file = "manuscript/resubmission_2/exports/cns_network_spectral_clusters_umap.csv")
readr::write_csv(cns.network.umap,
                 file = "manuscript/resubmission_2/supplemental_data/supplemental_data_6.csv")

# Literature review
lit.review <- cns.functions %>%
  dplyr::filter(super_class %in% c("ascending","descending","visual_projection"),
                !is.na(modality)) %>%
  dplyr::mutate(cell_function = dplyr::case_when(
    super_class == "visual_projection" ~ response,
    TRUE ~ modality
  )) %>%
  dplyr::filter(!is.na(cell_function)&cell_function!=""&!is.na(citations)) %>%
  dplyr::distinct(cell_type, other_names, super_class, cell_function, citations) %>%
  dplyr::mutate(doi = NA)
readr::write_csv(lit.review,
                 file = "manuscript/resubmission_2/supplemental_data/supplemental_data_7.csv")

# Supplementary data 8: Known dataset issues — bounding boxes
# Bounding boxes delineating regions with known data-quality issues in the
# BANC dataset. Coordinates are in BANC raw-voxel space (1 voxel = 4 × 4 × 45 nm).
# Columns:
#   issue        — short label for the issue type
#   min_x, min_y, min_z — lower corner of the bounding box (voxels)
#   max_x, max_y, max_z — upper corner of the bounding box (voxels)
#
# To convert to nanometres: multiply x,y by 4 and z by 45.
# To convert to micrometres: multiply x,y by 0.004 and z by 0.045.

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
                 file = "manuscript/resubmission_2/supplemental_data/supplemental_data_8.csv")

# Send to google bucket
system("gsutil -m rsync -r manuscript/resubmission_2/exports/ gs://brain-and-nerve-cord_exports/brain_and_nerve_cord/v626/annotations")
system("gsutil -m rsync -r manuscript/resubmission_2/supplemental_data/ gs://brain-and-nerve-cord_exports/brain_and_nerve_cord/v626/annotations")




