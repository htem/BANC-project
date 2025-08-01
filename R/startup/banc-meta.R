##########################
## BANC METADATA LOADER ##
##########################
# Load and process BANC neuron metadata from SQLite database
# Cleans automatic annotations and creates composite cell type assignments

# Query main BANC metadata table
bc.orig <- banctable_query()
if(is.null(banc.version)){
   banc.meta <- bc.orig %>%
    dplyr::mutate(cell_type = ifelse(grepl("auto", cell_type),NA,cell_type),
                  fafb_cell_type = ifelse(grepl("auto",fafb_cell_type),NA,fafb_cell_type),
                  manc_cell_type = ifelse(grepl("auto",manc_cell_type),NA,manc_cell_type),
                  id=root_id) %>%
    dplyr::mutate(cell_type = dplyr::case_when(
      grepl("neck",region)&grepl("auto",cell_type) ~ NA,
      TRUE ~ cell_type
    )) %>%
    dplyr::mutate(
      cell_class = gsub("auto:","",cell_class),
      cell_sub_class = gsub("auto:","",cell_sub_class),
      cell_type = gsub("auto:","",cell_type),
      super_class = gsub("auto:","",super_class),
      nerve = gsub("auto:","",nerve),
      hemilineage = gsub("auto:","",hemilineage),
      top_nt = gsub("auto:","",top_nt),
      cell_function = gsub("auto:","",cell_function)
    ) %>%
    dplyr::arrange(cell_type, fafb_cell_type, manc_cell_type) %>%
    dplyr::distinct(id, .keep_all = TRUE) %>%
    dplyr::distinct(supervoxel_id, .keep_all = TRUE) %>%
    dplyr::mutate(composite_cell_type = dplyr::case_when(
      is.na(cell_type) ~ id,
      grepl("ascending",super_class)&!is.na(manc_cell_type) ~ manc_cell_type,
      grepl("descending",super_class)&!is.na(fafb_cell_type) ~ fafb_cell_type,
      TRUE ~ cell_type
    )) %>%
    dplyr::ungroup()
   banc.meta <- banc.meta %>%
     dplyr::select(id, root_id, supervoxel_id, contains("seed")) %>%
     dplyr::left_join(bc.orig %>%
                        dplyr::select(-root_id,-contains("seed")) %>%
                        dplyr::distinct(root_626, cell_type, neurotransmitter_predicted, neurotransmitter_score,
                                        status, proofread, position, root_region,
                                        fafb_match, manc_match, hemibrain_match, fanc_match,
                                        fafb_nblast_match, manc_nblast_match, hemibrain_nblast_match, fanc_nblast_match,
                                        side, region, nerve, cluster, manual_cluster, super_cluster, cns_network,
                                        hemilineage, flow, super_class, cell_class, cell_sub_class,
                                        supervoxel_id, fafb_cell_type, manc_cell_type, root_position_nm, 
                                        segregation_index, pd_width, input_connections, output_connections, input_side_index, output_side_index,
                                        fafb_match, manc_match) %>%
                        dplyr::distinct(supervoxel_id, .keep_all = TRUE),
                      by = "supervoxel_id") %>%
     dplyr::mutate(cell_type = gsub("auto:","",cell_type))
   banc.meta <- banc.meta %>%
     dplyr::select(-hemilineage, -super_class, -cell_class, -cell_sub_class) %>%
     dplyr::left_join(franken.meta.simple[,c("manc_id", "neuromere")] %>%
                        dplyr::distinct(manc_id, .keep_all = TRUE),
                      by = c("manc_match"="manc_id")) %>%
     dplyr::left_join(franken.meta.simple[,c("cell_type", 
                                      "super_class",
                                      "cell_class", 
                                      "cell_sub_class", 
                                      "hemilineage", 
                                      "sez_class", 
                                      "top_nt",
                                      "cell_function",
                                      "cell_function_detailed",
                                      "neurotransmitter_verified",
                                      "neuropeptide_verified")] %>%
                        dplyr::filter(!is.na(cell_type), cell_type!="") %>%
                        dplyr::distinct(cell_type, .keep_all = TRUE),
                      by = c("cell_type"="cell_type")) %>%
     dplyr::group_by(cell_type) %>%
     dplyr::mutate(multi_neuromere = length(unique(neuromere))>1) %>%
     dplyr::ungroup() %>%
     dplyr::mutate(composite_cell_type = dplyr::case_when(
       is.na(neuromere)|neuromere=="" ~ cell_type,
       multi_neuromere ~ paste0(cell_type,"_",neuromere),
       TRUE ~ cell_type
     ))
}else{
  con <- DBI::dbConnect(RSQLite::SQLite(),
                        file.path(banc.dropbox.connectivity.save.path,banc.connectivity.version))
  banc.meta <- dplyr::tbl(con, "meta") %>%
    dplyr::collect()
  banc.meta$id <- banc.meta$root_id
  dbDisconnect(con)
  banc.meta <- banc.meta %>%
    dplyr::select(id, root_id, supervoxel_id, contains("seed")) %>%
    dplyr::left_join(bc.orig %>%
                       dplyr::select(-root_id,-contains("seed")) %>%
                       dplyr::distinct(root_626, cell_type, neurotransmitter_predicted, neurotransmitter_score,
                                       side, region, nerve, cluster, manual_cluster, super_cluster, cns_network,
                                       hemilineage, flow, super_class, cell_class, cell_sub_class,
                                       supervoxel_id, fafb_cell_type, manc_cell_type, root_position_nm, 
                                       segregation_index, pd_width, input_connections, output_connections, input_side_index, output_side_index,
                                       fafb_match, manc_match) %>%
                       dplyr::distinct(supervoxel_id, .keep_all = TRUE),
                     by = "supervoxel_id") %>%
    dplyr::mutate(cell_type = gsub("auto:","",cell_type))
  banc.meta <- banc.meta %>%
    dplyr::select(-hemilineage, -super_class, -cell_class, -cell_sub_class) %>%
    dplyr::left_join(franken.meta.simple[,c("manc_id", "neuromere")] %>%
                       dplyr::distinct(manc_id, .keep_all = TRUE),
                     by = c("manc_match"="manc_id")) %>%
    dplyr::left_join(franken.meta.simple[,c("cell_type", 
                                     "super_class",
                                     "cell_class", 
                                     "cell_sub_class", 
                                     "hemilineage", 
                                     "sez_class", 
                                     "top_nt",
                                     "cell_function",
                                     "cell_function_detailed",
                                     "neurotransmitter_verified",
                                     "neuropeptide_verified")] %>%
                       dplyr::filter(!is.na(cell_type), cell_type!="") %>%
                       dplyr::distinct(cell_type, .keep_all = TRUE),
                     by = c("cell_type"="cell_type")) %>%
    dplyr::group_by(cell_type) %>%
    dplyr::mutate(multi_neuromere = length(unique(neuromere))>1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(composite_cell_type = dplyr::case_when(
      is.na(neuromere)|neuromere=="" ~ cell_type,
      multi_neuromere ~ paste0(cell_type,"_",neuromere),
      TRUE ~ cell_type
    ))
}

# Add seeds
if(!"seed_07"%in%colnames(banc.meta)){
  con <- DBI::dbConnect(RSQLite::SQLite(),
                        file.path(banc.dropbox.influence.save.path,influence.sqlite))
  inf.meta <- dplyr::tbl(con, "meta") %>%
    dplyr::collect()
  dbDisconnect(con)
  banc.meta <- banc.meta %>%
    dplyr::left_join(inf.meta %>%
                       dplyr::select(root_626, contains("seed")),
                     by = "root_626")
}

# Get singular functions
cns.functions.singular <- cns.functions %>%
  dplyr::mutate(cell_function = dplyr::case_when(
    !is.na(modality)&modality!="" ~ modality,
    !is.na(behaviour)&behaviour!="" ~ behaviour,
    !is.na(response)&response!="" ~ response,
    !is.na(valence)&valence!="" ~ valence,
    TRUE ~ NA
  )) %>%
  dplyr::distinct(cell_type,.keep_all = TRUE) %>%
  dplyr::distinct(cell_type, cell_function)

# Add body part sensory
if(!"body_part_sensory"%in%colnames(banc.meta)){
  banc.meta <- banc.meta %>%
    dplyr::left_join(bc.orig %>%
                       dplyr::distinct(supervoxel_id,
                                       body_part_sensory) %>%
                       dplyr::distinct(supervoxel_id, .keep_all = TRUE),
                     by = "supervoxel_id")
}
banc.meta <- banc.meta %>%
  dplyr::left_join(franken.meta.simple[,c("manc_id", "body_part_sensory")] %>%
                     dplyr::filter(!is.na(body_part_sensory),
                                   !is.na(manc_id),
                                   body_part_sensory!="") %>%
                     dplyr::distinct(manc_id, .keep_all = TRUE),
                   by = c("manc_match"="manc_id")) %>%
  dplyr::left_join(franken.meta.simple[,c("fafb_id", "body_part_sensory")] %>%
                     dplyr::filter(!is.na(body_part_sensory), 
                                   !is.na(fafb_id),
                                   body_part_sensory!="") %>%
                     dplyr::distinct(fafb_id, .keep_all = TRUE),
                   by = c("fafb_match"="fafb_id")) %>%
  dplyr::mutate(body_part_sensory = dplyr::coalesce(body_part_sensory.y,body_part_sensory,body_part_sensory.x)) %>%
  dplyr::select(-body_part_sensory.x,-body_part_sensory.y) %>%
  dplyr::mutate(body_part_sensory=gsub(".*\\;","",body_part_sensory),
                body_part_sensory=ifelse(body_part_sensory=="",NA,body_part_sensory),
                body_part_sensory=ifelse(is.na(body_part_sensory)&grepl("sensory",super_class),'unknown',body_part_sensory))

# Add cell functions
banc.meta <- banc.meta %>%
  dplyr::left_join(cns.functions.singular %>%
                   dplyr::select(cell_type, cell_function) %>%
                   dplyr::distinct(cell_type, .keep_all = TRUE),
                 by = "cell_type") %>%
  dplyr::left_join(cns.functions.singular %>%
                     dplyr::select(cell_type, cell_function) %>%
                     dplyr::distinct(cell_type, .keep_all = TRUE),
                   by = c("fafb_cell_type"="cell_type")) %>%
  dplyr::left_join(cns.functions.singular %>%
                     dplyr::select(cell_type, cell_function) %>%
                     dplyr::distinct(cell_type, .keep_all = TRUE),
                   by = c("manc_cell_type"="cell_type")) %>%
  dplyr::mutate(cell_function.x = ifelse(cell_function.x=="",NA,cell_function.x),
                cell_function.y = ifelse(cell_function.y=="",NA,cell_function.y),
                cell_function.y.y = ifelse(cell_function.y.y=="",NA,cell_function.y.y),
                cell_function.x.x = ifelse(cell_function.x.x=="",NA,cell_function.x.x)) %>%
  dplyr::mutate(
    cell_function = coalesce(cell_function.x,cell_function.y,cell_function.x.x,cell_function.y.y)) %>%
  dplyr::mutate(cell_function = gsub("\\,.*","",cell_function),
                cell_function = gsub(" ","_",cell_function),
                cell_function = ifelse(cell_function=="",NA,cell_function)) %>%
  dplyr::select(-cell_function.x,-cell_function.y,-cell_function.x.x,-cell_function.y.y) 

# Add back in the most important meta data
if(!"body_part_effector"%in%colnames(banc.meta)){
  banc.meta <- banc.meta %>%
    dplyr::left_join(bc.orig %>%
                       dplyr::distinct(supervoxel_id,
                                       body_part_effector) %>%
                       dplyr::distinct(supervoxel_id, .keep_all = TRUE),
                     by = "supervoxel_id")
}
banc.meta <- banc.meta %>%
  dplyr::left_join(bc.orig %>%
                     dplyr::distinct(supervoxel_id,
                                     banc_super_class = super_class,
                                     banc_cell_class = cell_class,
                                     banc_cell_sub_class = cell_sub_class,
                                     banc_body_part_effector = body_part_effector,
                                     banc_cell_function = cell_function,
                                     banc_cell_function_detailed = cell_function_detailed) %>%
                     dplyr::distinct(supervoxel_id, .keep_all = TRUE),
                   by = "supervoxel_id") %>%
  dplyr::mutate(super_class = dplyr::case_when(
    grepl("auto",banc_super_class)|is.na(banc_super_class) ~ super_class,
    TRUE ~ banc_super_class
  )) %>%
  dplyr::mutate(super_class = dplyr::case_when(
    grepl("visceral|endocrine|efferent_non_motor",super_class)|grepl("visceral|endocrine|efferent_non_motor",cell_class) ~ "visceral_circulatory",
    grepl("motor|efferent",super_class)|grepl("motor|efferent",cell_class) ~ "motor",
    TRUE ~ super_class
  )) %>%
  dplyr::mutate(body_part_effector = dplyr::case_when(
    !grepl("motor|efferent|endocrine|visceral", banc_super_class) ~ NA,
    !is.na(banc_body_part_effector) ~ banc_body_part_effector,
    TRUE ~ body_part_effector
  )) %>%
  dplyr::mutate(cell_class = dplyr::case_when(
    !is.na(banc_cell_class) ~ banc_cell_class,
    TRUE ~ cell_class
  )) %>%
  dplyr::mutate(cell_sub_class = dplyr::case_when(
    !is.na(banc_cell_sub_class) ~ banc_cell_sub_class,
    TRUE ~ cell_sub_class
  )) %>%
  dplyr::mutate(cell_function = dplyr::case_when(
    !is.na(banc_cell_function) ~ banc_cell_function,
    TRUE ~ cell_function
  )) %>%
  dplyr::mutate(cell_function_detailed = dplyr::case_when(
    !is.na(banc_cell_function_detailed) ~ banc_cell_function_detailed,
    TRUE ~ cell_function_detailed
  )) %>%
  dplyr::select(-banc_cell_sub_class,
                -banc_body_part_effector,
                -banc_cell_function,
                -banc_cell_class,
                -banc_super_class,
                -banc_cell_function_detailed) %>%
  dplyr::mutate(cell_class = dplyr::case_when(
    grepl("^motor_neuron$|^motor$",cell_class) ~ "motor",
    grepl("endocrine|efferent|visceral",cell_class) ~ "visceral_circulatory",
    TRUE ~ cell_class
  ) ) %>%
  dplyr::mutate(body_part_effector = gsub("putative_|unknown_","",body_part_effector),
                cell_sub_class = gsub("putative_","",cell_sub_class),
                cell_function = gsub("putative_","",cell_function),
                super_class = gsub("\\;.*","",super_class)) %>%
  dplyr::mutate(cell_sub_class = ifelse(is.na(cell_sub_class)|cell_sub_class=="",cell_class,cell_sub_class),
                cell_class = ifelse(is.na(cell_class)|cell_class=="",cell_sub_class,cell_class)) %>%
  dplyr::mutate(cell_function = dplyr::case_when(
    super_class %in% c("ascending","descending") & cell_function %in% c("proprioception","chemosensory","gustatory-tactile") ~ NA,
    cell_function == "" ~ NA,
    TRUE ~ cell_function
  ))

# Correct neurotransmitter data
banc.meta <- banc.meta %>%
  dplyr::mutate(other_nt = top_nt,
                top_nt = dplyr::case_when(
                  !is.na(neuropeptide_verified)&neuropeptide_verified!=""&neurotransmitter_predicted%in%c("sertonin","histamine","octopamine","dopamine") ~ "peptide",
                  !is.na(neurotransmitter_predicted) ~ neurotransmitter_predicted,
                  top_nt=="unclear" ~ "unknown",
                  !is.na(top_nt) ~ gsub(";.*","",top_nt),
                  TRUE ~ "unknown"
                ))

# Cluster is the manually improved one
banc.meta$cluster <- banc.meta$manual_cluster
banc.meta$body_part_effector <- gsub(",.*| .*","",banc.meta$body_part_effector)
banc.meta$body_part_sensory <- gsub(",.*| .*","",banc.meta$body_part_sensory)

# CNS clusters switch
cns.network.umap <- readr::read_csv("data/cns_network/spectral_clustering_min_connection_strength_1_banc_version_626_cluster_count_13_cluster_seed_10_embedding_seed_3.csv", 
                             col_types = banc.col.types)
cns.cluster.mapping <-cns.network.umap %>%
  dplyr::mutate(cns_network = paste0("CNS_",str_pad(spectral_cluster,width = 2,pad =0))) %>%
  dplyr::distinct(unofficial_cluster_name,cns_network)
cns.cluster.names <- cns.cluster.mapping$unofficial_cluster_name
names(cns.cluster.names) <- cns.cluster.mapping$cns_network
banc.meta$cns_network <- cns.cluster.names[as.character(banc.meta$cns_network)]

# Join banc.meta data
banc.meta.pre <- banc.meta
colnames(banc.meta.pre) <- paste0("pre_",colnames(banc.meta.pre))
banc.meta.post <- banc.meta
colnames(banc.meta.post) <- paste0("post_",colnames(banc.meta.post))

# Meta data summaries
banc.meta$id <- banc.meta$root_id
banc.neck.meta <- banc.meta %>%
  dplyr::filter(grepl("descending|ascending",super_class),
                !grepl("effector",super_class))
banc.dn.meta <- subset(banc.meta, grepl("descending",super_class))
banc.an.meta <- subset(banc.meta, grepl("ascending",super_class))
banc.eff.meta <- subset(banc.meta, grepl("efferent|motor|endocrine|visceral",super_class)) 
banc.vpn.meta <- subset(banc.meta, grepl("visual_projection",super_class))
banc.sens.meta <- subset(banc.meta, grepl("sensory",super_class))
banc.sez.meta <- subset(banc.meta, !is.na(sez_class)&sez_class!="")
an.ids <- unique(banc.an.meta$id)
dn.ids <- unique(banc.dn.meta$id)

# Read DN data output
# umap.sez.df  <- read_csv(file = "data/banc_sez_functional_classes.csv", col_types = banc.col.types) %>%
#   dplyr::mutate(clusterno = gsub(".*_","",cluster)) %>%
#   dplyr::mutate(id = ifelse(is.na(id),cell_type,id))
classes.dn.df <- read_csv(file = "data/banc_annotations/banc_neck_functional_classes.csv", col_types = banc.col.types)
classes.eff.df <- read_csv(file = "data/banc_annotations/banc_efferent_functional_classes.csv", col_types = banc.col.types)

# Use manually improved clusters
classes.dn.df <- banc.neck.meta %>%
  dplyr::left_join(classes.dn.df %>%
                     dplyr::select(id, UMAP1, UMAP2),
                   by = "id")  %>%
  dplyr::mutate(clusterno = gsub(".*_","",cluster))
classes.eff.df <- banc.eff.meta %>%
  dplyr::left_join(classes.eff.df %>%
                     dplyr::select(id, UMAP1, UMAP2),
                   by = "id")  %>%
  dplyr::mutate(clusterno = gsub(".*_","",cluster))

# Neck key UMAP
neck.inclusion <- readr::read_csv(file="data/meta/banc_neck_inclusion.csv", 
                                  col_types = banc.col.types)
banc.in <- subset(neck.inclusion,in_group)$root_id
banc.out <- subset(neck.inclusion,!in_group)$root_id
umap.dn.df <- classes.dn.df %>%
  dplyr::mutate(cell_function = ifelse(grepl("unknown",cell_function),NA,cell_function),
                label = ifelse(!is.na(cell_function),cell_type,NA)) %>%
  dplyr::mutate(
    neck_group = dplyr::case_when(
      id %in% banc.in ~ "IN",
      TRUE ~ "OUT"
    ),
    group = paste0(super_class,"_", neck_group),
    label = ifelse(!is.na(cell_function),cell_type,NA)) %>%
  dplyr::mutate(cluster = ifelse(is.na(cluster),"0",cluster))  %>%
  dplyr::filter(!is.na(UMAP1))

# Efferent key umap
umap.eff.df <- classes.eff.df %>%
  dplyr::mutate(cell_function = ifelse(grepl("unknown",cell_function),NA,cell_function),
                label = ifelse(!is.na(cell_function),cell_type,NA)) %>%
  dplyr::mutate(cluster = ifelse(is.na(cluster),"0",cluster))  %>%
  dplyr::filter(!is.na(UMAP1))

# Ensure we have enough colors for all clusters
n_clusters <- length(unique(umap.dn.df$cluster))
cluster_colors <- cerise_limon_palette(n_clusters)
names(cluster_colors) <- sort(unique(umap.dn.df$cluster))
umap.dn.df$colours <- cluster_colors[umap.dn.df$cluster]

n_clusters <- length(unique(umap.eff.df$cluster))
cluster_colors <- cerise_limon_palette(n_clusters)
names(cluster_colors) <- sort(unique(umap.eff.df$cluster))
umap.eff.df$colours <- cluster_colors[umap.eff.df$cluster]

# Constant to add to log score
threshold.inf.value <- 17.18
threshold.sens.inf.value <- 12.14715

################
### MAPPINGD ###
################

# choose metrics
inf.metric <- "influence_norm_log"

# seed
efferent.target.map <- c(abdomen_neurosecretory_cell = "abdomen neurosecretory", 
                         corpus_allatum_neurosecretory_cell = "corpus allatum neurosecretory", 
                         digestive_tract_neurosecretory_cell = "digestive tract neurosecretory", 
                         proboscis_motor_neuron = "proboscis motor",
                         salivary_motor_neuron = "salivary gland motor",
                         pharynx_motor_neuron = "pharynx motor",
                         crop_motor_neuron = "pharynx motor",
                         antenna_motor_neuron = "antenna motor",
                         eye_motor_neuron = "eye motor",
                         haltere_motor_neuron = "haltere motor",
                         uterus_motor_neuron = "uterus motor",
                         abdomen_motor_neuron = "abdomen motor",
                         thoracic_abdominal_segmental_motor_neuron = "thoracic segmental motor",
                         subesophageal_zone_neurosecretory_cell = "sez neurosecretory",
                         ventral_nerve_cord_neurosecretory_cell = "vnc neurosecretory",
                         front_leg_motor_neuron = "front leg motor", 
                         front_leg_neurosecretory_cell = "front leg modulatory", 
                         haltere_power_neuron = "haltere power", 
                         haltere_steering_neuron = "haltere steering", 
                         hind_leg_motor_neuron = "hind leg motor", 
                         hind_leg_neurosecretory_cell = "hind leg modulatory", 
                         middle_leg_motor_neuron = "middle leg motor", 
                         middle_leg_neurosecretory_cell = "middle leg modulatory", 
                         neck_neurosecretory_cell = "neck modulatory", 
                         neck_pitch_motor_neuron = "neck pitch", 
                         neck_roll_motor_neuron = "neck roll", 
                         neck_yaw_motor_neuron = "neck yaw", 
                         neurohemal_complex_neurosecretory_cell = "neurohemal complex", 
                         reproductive_tract_neurosecretory_cell = "reproductive tract", 
                         ovaries = "ovaries",
                         retrocerebral_complex_neurosecretory_cell = "retrocerebral complex", 
                         ureter_neurosecretory_cell = "ureter neurosecretory", 
                         wing_neurosecretory_cell = "wing modulatory", 
                         wing_peripheral_intrinsic_neuron = "wing modulatory", 
                         wing_power_motor_neuron = "wing power", 
                         wing_steering_motor_neuron = "wing steering", 
                         wing_tension_motor_neuron = "wing tension")

# Get sensory seed map
sensory.seed.map.detailed <- c(#abdomen_endocrine_left = "abdomen_endocrine", 
  #abdomen_endocrine_right = "abdomen_endocrine", 
  abdomen_multidendritic_neuron = "abdomen multidendritic", 
  abdomen_orphan_neuron = "abdomen orphan", 
  #abdomen_strand_neuron, 
  abdominal_wall_multidendritic_neuron = "abdomen bitter", 
  antenna_bristle_neuron = "antenna bristle",
  antenna_campaniform_sensillum_neuron = "antenna campaniform", 
  antenna_hygrosensory_receptor_neuron = "antenna hygrosensory receptor", 
  #antenna_olfactory_receptor_neuron = "antenna olfactory receptor", 
  #antenna_orphan_neuron = "antenna orphan", 
  antenna_thermosensory_receptor_neuron = "antenna thermosensory receptor", 
  aorta_sensory_neuron = "aorta",
  cibarium_multidendritic_neuron = "cibarium", 
  crop_internal_taste_sensillum_neuron = "crop internal taste", 
  #endocrine_left = "vnc endocrine", 
  #endocrine_right = "vnc endocrine", 
  eye_bristle_neuron = "eye bristle", 
  front_leg_bristle_neuron = "front leg bristle", 
  front_leg_chordotonal_organ_neuron = "front leg chordotonal", 
  front_leg_claw_chordotonal_organ_neuron = "front leg chordotonal", 
  front_leg_club_chordotonal_organ_neuron = "front leg chordotonal", 
  front_leg_campaniform_sensillum_neuron = "front leg campaniform", 
  front_leg_hair_plate_neuron = "leg hair plate", 
  front_leg_hook_chordotonal = "front leg chordotonal", 
  front_leg_multidendritic_neuron = "multidendritic", 
  front_leg_orphan_neuron = "front leg orphan",  
  front_leg_taste_peg_neuron = "front leg taste peg", 
  frontal_bristle_neuron = "head bristle",   
  haustellum_bristle_neuron  = "head bristle",  
  interocellar_bristle_neuron  = "head bristle",   
  interommatidial_bristle_neuron  = "head bristle",   
  occipital_bristle_neuron  = "head bristle",   
  occipital_dorsal_bristle_neuron  = "head bristle",  
  postocellar_bristle_neuron  = "head bristle",   
  postorbital_dorsal_bristle_neuron  = "head bristle",   
  postorbital_ventral_bristle_neuron  = "head bristle",   
  vibrissa_bristle_neuron  = "head bristle",  
  maxillary_palp_bristle_neuron  = "head bristle",  
  haltere_bristle_neuron = "haltere bristle", 
  haltere_campaniform_sensillum_neuron = "haltere campaniform", 
  haltere_orphan_neuron = "haltere orphan", 
  hemolymph_sensory_neuron = "hemolymph", 
  hind_leg_bristle_neuron = "hind leg bristle", 
  hind_leg_campaniform_sensillum_neuron = "hind leg campaniform", 
  hind_leg_chordotonal_organ_neuron = "hind leg chordotonal", 
  hind_leg_claw_chordotonal_organ_neuron = "hind leg chordotonal", 
  hind_leg_club_chordotonal_organ_neuron  = "hind leg chordotonal",  
  hind_leg_hair_plate_neuron  = "leg hair plate",  
  hind_leg_hook_chordotonal  = "hind leg chordotonal",  
  hind_leg_multidendritic_neuron = "multidendritic",  
  hind_leg_orphan_neuron = "hind leg orphan",  
  hind_leg_taste_peg_neuron = "hind leg taste peg",   
  internal_thermosensory_receptor_neuron = "antenna thermosensory receptor", 
  johnstons_organ_A_neuron = "johnstons organ A", 
  johnstons_organ_B_neuron = "johnstons organ B", 
  johnstons_organ_C_neuron = "johnstons organ C", 
  johnstons_organ_D_neuron = "johnstons organ D", 
  johnstons_organ_E_neuron = "johnstons organ E", 
  johnstons_organ_F_neuron = "johnstons organ F", 
  johnstons_organ_other_neuron = "johnstons organ other", 
  labellum_bristle_neuron = "labellum bristle", 
  labellum_external_taste_sensillum_neuron = "labellum external taste", 
  #labellum_orphan_neuron = "labellum orphan", 
  labellum_taste_peg_neuron = "labellum taste peg", 
  #leg_taste_peg_neuron = "leg_taste_peg", 
  maxillary_palp_olfactory_receptor_neuron = "maxillary palp olfactory receptor", 
  metathoracic_chordotonal_organ_neuron = "metathoracic chordotonal",
  middle_leg_bristle_neuron = "middle leg bristle", 
  middle_leg_campaniform_sensillum_neuron = "middle leg campaniform", 
  middle_leg_chordotonal_organ_neuron = "middle leg chordotonal", 
  middle_leg_claw_chordotonal_organ_neuron = "middle leg chordotonal", 
  middle_leg_club_chordotonal_organ_neuron = "middle leg chordotonal", 
  middle_leg_hair_plate_neuron  = "leg hair plate",  
  middle_leg_hook_chordotonal = "middle leg chordotonal", 
  middle_leg_multidendritic_neuron = "leg multidendritic", 
  #middle_leg_orphan_neuron = "middle leg orphan", 
  middle_leg_taste_peg_neuron = "middle leg taste peg",
  #pars_intercerebralis_endocrine_enteric_left = "pars_intercerebralis_enteric", 
  #pars_intercerebralis_endocrine_enteric_right = "pars_intercerebralis_enteric", 
  #pars_lateralis_endocrine_corpus_allatum_left = "pars_lateralis_endocrine_retrocerebral_complex", 
  #pars_lateralis_endocrine_corpus_allatum_right = "pars_lateralis_endocrine_retrocerebral_complex", 
  #pars_lateralis_endocrine_retrocerebral_complex_left = "pars_lateralis_endocrine_retrocerebral_complex", 
  #pars_lateralis_endocrine_retrocerebral_complex_right = "pars_lateralis_endocrine_retrocerebral_complex", 
  pharynx_internal_taste_sensillum_neuron = "pharynx internal taste", 
  #pharynx_orphan_neuron = "pharynx orphan", 
  prosternal_hair_plate_neuron = "prosternal hair plate", 
  prothoracic_chordotonal_organ_neuron = "prothoracic chordotonal", 
  #retina_photoreceptor_neuron = "retina_photoreceptor", 
  #subesophageal_zone_endocrine_left = "subesophageal zone endocrine", 
  #subesophageal_zone_endocrine_right = "subesophageal zone endocrine", 
  thorax_bristle_neuron = "thorax bristle", 
  thorax_campaniform_sensillum_neuron = "thorax campaniform", 
  thorax_multidendritic_neuron = "thorax multidendritic",
  abdominal_terminalia_bristle = "abdomen terminalia bristle",
  #thorax_orphan_neuron = "thorax orphan", 
  wheelers_chordotonal_organ_neuron = "wheelers organ chordotonal", 
  wing_base_campaniform_sensillum_neuron = "wing base campaniform", 
  wing_base_chordotonal_organ_neuron = "wing base chordotonal",
  #wing_base_orphan_neuron = "wing base orphan", 
  #wing_campaniform_sensillum_neuron = "wing campaniform", 
  #wing_endocrine_left = "wing_non_motor", 
  #wing_endocrine_right = "wing_non_motor", 
  wing_margin_bristle_neuron = "wing margin bristle",
  wing_margin_taste_peg_neuron = "wing margin taste", 
  wing_multidendritic_neuron = "multidendritic", 
  wing_tegula_campaniform_sensillum_neuron = "wing tegula campaniform", 
  wing_tegula_chordotonal_organ_neuron = "wing tegula chordotonal", 
  wing_tegula_hair_plate_neuron = "wing tegula hair plate",
  #wing_tegula_orphan_neuron = wing tegula orphan,
  #visual_front_leg_feedback = "visual leg feedback", 
  visual_horizontal_widefieldmotion = "visual horizontal widefield motion", 
  `visual_large_objects,visual_thin_vertical_bar` = "visual thin vertical bar", 
  visual_loom = "visual loom", 
  `visual_object,visual_loom` = "visual loom",  
  visual_polarized_light = "polarized light", 
  visual_small_object = "visual small object", 
  `visual_small_object,visual_loom` = "visual loom",  
  visual_thin_vertical_bar = "visual thin vertical bar", 
  visual_vertical_widefieldmotion = "visual vertical widefield motion",
  visual_ocellar = "visual ocellar"
)

# Get sensory seed map
sensory.seed.map <- c(#abdomen_endocrine_left = "abdomen_endocrine", 
  #abdomen_endocrine_right = "abdomen_endocrine", 
  abdomen_multidendritic_neuron = "abdomen multidendritic", 
  abdomen_orphan_neuron = "abdomen orphan", 
  #abdomen_strand_neuron, 
  abdominal_wall_multidendritic_neuron = "abdominal wall multidendritic", 
  #antenna_bristle_neuron = "antenna bristle",
  #antenna_campaniform_sensillum_neuron = "antenna campaniform", 
  #antenna_hygrosensory_receptor_neuron = "antenna hygrosensory receptor", 
  antenna_olfactory_receptor_neuron = "antenna olfactory receptor", 
  #antenna_orphan_neuron = "antenna orphan", 
  antenna_thermosensory_receptor_neuron = "antenna thermosensory receptor", 
  aorta_sensory_neuron = "aorta",
  cibarium_multidendritic_neuron = "cibarium multidendritic", 
  crop_internal_taste_sensillum_neuron = "crop internal taste", 
  #endocrine_left = "vnc endocrine", 
  #endocrine_right = "vnc endocrine", 
  eye_bristle_neuron = "eye bristle", 
  front_leg_bristle_neuron = "leg bristle", 
  front_leg_chordotonal_organ_neuron = "leg chordotonal", 
  front_leg_claw_chordotonal_organ_neuron = "leg chordotonal", 
  front_leg_club_chordotonal_organ_neuron = "leg chordotonal", 
  front_leg_campaniform_sensillum_neuron = "leg campaniform", 
  front_leg_hair_plate_neuron = "leg hair plate", 
  front_leg_hook_chordotonal = "leg chordotonal", 
  front_leg_multidendritic_neuron = "leg multidendritic", 
  #front_leg_orphan_neuron = "leg orphan",  
  front_leg_taste_peg_neuron = "leg taste peg", 
  frontal_bristle_neuron = "head bristle",   
  haustellum_bristle_neuron  = "head bristle",  
  interocellar_bristle_neuron  = "head bristle",   
  interommatidial_bristle_neuron  = "head bristle",   
  occipital_bristle_neuron  = "head bristle",   
  occipital_dorsal_bristle_neuron  = "head bristle",  
  postocellar_bristle_neuron  = "head bristle",   
  postorbital_dorsal_bristle_neuron  = "head bristle",   
  postorbital_ventral_bristle_neuron  = "head bristle",   
  vibrissa_bristle_neuron  = "head bristle",  
  maxillary_palp_bristle_neuron  = "head bristle",  
  #haltere_bristle_neuron = "haltere bristle", 
  haltere_campaniform_sensillum_neuron = "haltere campaniform", 
  #haltere_orphan_neuron = "haltere orphan", 
  hemolymph_sensory_neuron = "hemolymph", 
  hind_leg_bristle_neuron = "leg bristle", 
  hind_leg_campaniform_sensillum_neuron = "leg campaniform", 
  hind_leg_chordotonal_organ_neuron = "leg chordotonal", 
  hind_leg_claw_chordotonal_organ_neuron = "leg chordotonal", 
  hind_leg_club_chordotonal_organ_neuron  = "leg chordotonal",  
  hind_leg_hair_plate_neuron  = "leg hair plate",  
  hind_leg_hook_chordotonal  = "leg chordotonal",  
  hind_leg_multidendritic_neuron = "leg multidendritic",  
  #hind_leg_orphan_neuron = "leg orphan",  
  hind_leg_taste_peg_neuron = "leg taste peg",   
  internal_thermosensory_receptor_neuron = "internal thermosensory receptor", 
  johnstons_organ_A_neuron = "johnstons organ A", 
  johnstons_organ_B_neuron = "johnstons organ B", 
  johnstons_organ_C_neuron = "johnstons organ C", 
  #johnstons_organ_D_neuron = "johnstons organ D", 
  johnstons_organ_E_neuron = "johnstons organ E", 
  johnstons_organ_F_neuron = "johnstons organ F", 
  #johnstons_organ_other_neuron = "johnstons organ other", 
  labellum_bristle_neuron = "labellum bristle", 
  labellum_external_taste_sensillum_neuron = "labellum external taste", 
  #labellum_orphan_neuron = "labellum orphan", 
  labellum_taste_peg_neuron = "labellum taste peg", 
  #leg_taste_peg_neuron = "leg_taste_peg", 
  #maxillary_palp_olfactory_receptor_neuron = "maxillary palp olfactory receptor", 
  metathoracic_chordotonal_organ_neuron = "metathoracic chordotonal",
  middle_leg_bristle_neuron = "leg bristle", 
  middle_leg_campaniform_sensillum_neuron = "leg campaniform", 
  middle_leg_chordotonal_organ_neuron = "leg chordotonal", 
  middle_leg_claw_chordotonal_organ_neuron = "leg chordotonal", 
  middle_leg_club_chordotonal_organ_neuron = "leg chordotonal", 
  middle_leg_hair_plate_neuron  = "leg hair plate",  
  middle_leg_hook_chordotonal = "leg chordotonal", 
  middle_leg_multidendritic_neuron = "leg multidendritic", 
  #middle_leg_orphan_neuron = "leg orphan", 
  middle_leg_taste_peg_neuron = "leg taste peg",
  #pars_intercerebralis_endocrine_enteric_left = "pars_intercerebralis_enteric", 
  #pars_intercerebralis_endocrine_enteric_right = "pars_intercerebralis_enteric", 
  #pars_lateralis_endocrine_corpus_allatum_left = "pars_lateralis_endocrine_retrocerebral_complex", 
  #pars_lateralis_endocrine_corpus_allatum_right = "pars_lateralis_endocrine_retrocerebral_complex", 
  #pars_lateralis_endocrine_retrocerebral_complex_left = "pars_lateralis_endocrine_retrocerebral_complex", 
  #pars_lateralis_endocrine_retrocerebral_complex_right = "pars_lateralis_endocrine_retrocerebral_complex", 
  pharynx_internal_taste_sensillum_neuron = "pharynx internal", 
  pharynx_orphan_neuron = "pharynx internal", 
  prosternal_hair_plate_neuron = "prosternal hair plate", 
  prothoracic_chordotonal_organ_neuron = "prothoracic chordotonal", 
  #retina_photoreceptor_neuron = "retina_photoreceptor", 
  #subesophageal_zone_endocrine_left = "subesophageal zone endocrine", 
  #subesophageal_zone_endocrine_right = "subesophageal zone endocrine", 
  thorax_bristle_neuron = "thorax bristle", 
  thorax_campaniform_sensillum_neuron = "thorax campaniform", 
  thorax_multidendritic_neuron = "thorax multidendritic",
  thorax_orphan_neuron = "thorax orphan", 
  wheelers_chordotonal_organ_neuron = "wheelers organ chordotonal", 
  wing_base_campaniform_sensillum_neuron = "wing campaniform", 
  wing_base_chordotonal_organ_neuron = "wing chordotonal",
  #wing_base_orphan_neuron = "wing base orphan", 
  wing_campaniform_sensillum_neuron = "wing campaniform", 
  #wing_endocrine_left = "wing_non_motor", 
  #wing_endocrine_right = "wing_non_motor", 
  wing_margin_bristle_neuron = "thorax bristle",
  abdominal_terminalia_bristle = "terminalia bristle",
  wing_margin_taste_peg_neuron = "wing taste", 
  wing_multidendritic_neuron = "wing multidendritic", 
  wing_tegula_campaniform_sensillum_neuron = "wing campaniform", 
  wing_tegula_chordotonal_organ_neuron = "wing chordotonal", 
  wing_tegula_hair_plate_neuron = "wing hair plate", 
  wing_tegula_orphan_neuron = "wing orphan",
  #visual_front_leg_feedback = "visual leg feedback", 
  visual_horizontal_widefieldmotion = "visual horizontal widefield motion", 
  `visual_large_objects,visual_thin_vertical_bar` = "visual thin vertical bar", 
  visual_loom = "visual loom", 
  `visual_object,visual_loom` = "visual loom",  
  visual_polarized_light = "polarized light", 
  visual_small_object = "visual small object", 
  `visual_small_object,visual_loom` = "visual loom",  
  visual_thin_vertical_bar = "visual thin vertical bar", 
  visual_vertical_widefieldmotion = "visual vertical widefield motion",
  visual_ocellar = "visual ocellar"
)

# super cluster ordering
super.clust.order <- c("flight steering 1",
                       "flight steering 2",
                       "flight power",
                       "head and eye orienting",
                       "grooming",
                       "probing",
                       "feeding",
                       "reproduction",
                       "tactile perception",
                       "proprioceptive perception",
                       "threat response",
                       "landing",
                       "walking",
                       "walking steering",
                       "visceral control")
cns.network.order = c("abdominal VNC",
  "ventral VNC",
  "dorsal VNC",
  "lateral brain",
  "inferior brain",
  "posterior brain",
  "left visual",
  "right visual",
  "flange median bundle",
  "superior brain",
  "left olfactory",
  "right olfactory",
  "central complex")
eff.super.order <- rev(c("abdomen-uterus", "abdomen-ureter", "energy homeostasis", "ingestion-digestion", 
  "abdomen-ovaries", "abdomen motor 1", "middle-hind leg", "proboscis-antenna", 
  "flight-energy-power", "thoracic-abdominal", "front leg", "abdomen motor 2", 
  "head-eye-antenna", "antenna", "feeding-endocrine"))







