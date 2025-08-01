##################################
## SUPPLEMENTAL DATA GENERATION ##
##################################
# Generate CSV files for paper supplementary data tables
# Exports neuron metadata for FAFB, MANC, and BANC datasets
source("R/startup/banc-startup.R")
banc.meta <- banctable_query()

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
                cell_type)
readr::write_csv(fw.meta,
                 file = "submission/exports/redrafted_manc_meta_data.csv")
readr::write_csv(fw.meta,
                 file = "submission/supplemental_data/supplemental_data_2.csv")
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
                cell_type)
readr::write_csv(mc.meta,
                 file = "submission/exports/redrafted_manc_meta_data.csv")
readr::write_csv(mc.meta,
                 file = "submission/supplemental_data/supplemental_data_3.csv")

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
                 file = "submission/exports/neck_neuron_direct_connectivity_cosine_umap.csv")
readr::write_csv(classes.nn.df,
                 file = "submission/supplemental_data/supplemental_data_4.csv")

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
                 file = "submission/exports/efferent_from_neck_influence_clusters_cosine_umap.csv")
readr::write_csv(classes.eff.df,
                 file = "submission/supplemental_data/supplemental_data_5.csv")

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
                 file = "submission/exports/cns_network_spectral_clusters_umap.csv")
readr::write_csv(cns.network.umap,
                 file = "submission/supplemental_data/supplemental_data_6.csv")

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
                 file = "submission/supplemental_data/supplemental_data_7.csv")

# Send to google bucket
system("gsutil -m rsync -r submission/exports/ gs://brain-and-nerve-cord_exports/brain_and_nerve_cord/v626/annotations")
system("gsutil -m rsync -r submission/supplemental_data/ gs://brain-and-nerve-cord_exports/brain_and_nerve_cord/v626/annotations")




