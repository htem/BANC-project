######################################################
### PLOT UMAPS BASED ON COSINE DIRECT CONNECTIVITY ###
######################################################

###############
### STARTUP ###
###############

# load
source("R/startup/banc-startup.R")
source("R/startup/franken-meta.R")
source("R/startup/banc-meta.R")
source("R/startup/banc-functions.R")

# new meta
banc.an.dn.meta <- banc.meta %>%
  dplyr::filter(super_class %in% c("ascending","descending")) %>%
  dplyr::filter(!grepl("^SA|^SN|^AN_4|AN_5|^IN",cell_type))
banc.targets <- banc.meta %>%
  dplyr::filter(grepl("mushroom_body_output|mushroom_body_dopaminergic_neuron|central_complex_input",cell_class)|
                  grepl("visual_centrifugal",super_class)) %>%
  rbind(banc.eff.meta)
banc.sources <- banc.meta %>%
  dplyr::filter(grepl("mushroom_body_output|central_complex_output",cell_class)|
                  grepl("visual_projection",super_class))

########################
### INFLUENCE SCORES ###
########################

# Connect to .sql file
chosen.seeds <- na.omit(unique(banc.an.dn.meta$seed_12))
chosen.seeds <- na.omit(c(chosen.seeds,na.omit(unique(banc.sources$seed_07))))
chosen.targets <- na.omit(unique(c(banc.targets$id,banc.an.dn.meta$id)))
con <- DBI::dbConnect(RSQLite::SQLite(),
                      file.path(banc.dropbox.influence.save.path,influence.sqlite))
influence.dn.df <- dplyr::tbl(con, influence.table) %>%
  dplyr::filter(!is_seed,
                level %in% c("seed_12","seed_07"),
                seed %in% !!chosen.seeds,
                id %in% !!chosen.targets) %>%
  dplyr::select(seed, level, id, influence_original, influence_norm_original, influence_syn_norm) %>%
  dplyr::collect()
dbDisconnect(con)

# Format
influence.dn.df <- influence.dn.df %>%
  dplyr::left_join(banc.meta %>%
                     dplyr::distinct(root_id, super_class, cell_class, cluster, super_cluster),
                   by = c("id"="root_id"))

# Calculate
inf.metrics <- c(
  "influence_log",
  "influence_norm_log",
  "influence_norm_log_minmax",
  "influence_log_minmax",
  "influence_log_minmax_seed",
  "influence_syn_norm_log"
)
inf.metrics <- "influence_norm_log"
for(inf.metric in inf.metrics){
  
  # Plot
  nn.cluster.out.nn.cluster.key.plot <- banc_plot_key_features(
    influence.meta = influence.dn.df %>%
      dplyr::mutate(seed_original = seed) %>%
      dplyr::mutate(super_cluster = dplyr::case_when(
        !is.na(super_cluster) ~ super_cluster,
        TRUE ~ NA
      )) %>%
      dplyr::left_join(banc.meta %>%
                         dplyr::mutate(super_cluster = dplyr::case_when(
                           !is.na(super_cluster) ~ super_cluster,
                           TRUE ~ NA
                         )) %>%
                         dplyr::mutate(seed = dplyr::case_when(
                           super_class %in% c("ascending","descending") ~ seed_12,
                           TRUE ~ seed_07
                         )) %>%
                         dplyr::filter(!is.na(seed)) %>%
                         dplyr::distinct(seed, .keep_all = TRUE) %>%
                         dplyr::distinct(seed,  seed_super_cluster = super_cluster),
                       by=c("seed")) %>%
      dplyr::mutate(seed = seed_super_cluster,
                    target = super_cluster) %>%
      dplyr::filter(!is.na(seed), 
                    !is.na(target)),
    ###
    inf.metric = inf.metric,
    target.map = NULL,
    width = 6,
    height = 6,
    recalculate = TRUE,
    row.annotation = NULL,
    show.annotation = FALSE,
    influence.level = NULL,
    save.path = banc.fig5.path,
    seed.map  = FALSE,
    chosen.seeds = NULL,
    chosen.targets = NULL, 
    row.cols = NULL,
    super.class = NULL,
    plot.name = sprintf("managed_neck_super_clusters_to_neck_super_clusters_%s.pdf",inf.metric),
    rev = FALSE,
    row.dend = NULL,
    col.dend = NULL,
    method = "euclidean",
    symmetric = TRUE,
    diagonal = FALSE
  )
  
  #  by cluster
  nn.cluster.out.nn.cluster.key.plot <- banc_plot_key_features(
    influence.meta = influence.dn.df %>%
      dplyr::filter(id %in% banc.an.dn.meta$id) %>%
      dplyr::mutate(target = cluster) %>%
      dplyr::left_join(banc.an.dn.meta %>%
                         dplyr::distinct(seed_12, .keep_all = TRUE) %>%
                         dplyr::distinct(seed_12, umap_cluster = cluster),
                       by=c("seed"="seed_12")) %>%
      dplyr::mutate(seed = umap_cluster) %>%
      dplyr::filter(!is.na(seed), 
                    !is.na(target)),
    ###
    inf.metric = inf.metric,
    target.map = NULL,
    width = 14,
    height = 14,
    recalculate = TRUE,
    row.annotation = NULL,
    show.annotation = FALSE,
    influence.level = "seed_12",
    save.path = banc.fig5.supp.path,
    seed.map  = FALSE,
    chosen.seeds = NULL,
    chosen.targets = NULL, 
    row.cols = NULL,
    super.class = NULL,
    plot.name = sprintf("managed_neck_clusters_to_neck_clusters_%s.pdf",inf.metric),
    rev = FALSE,
    row.dend = NULL,
    col.dend = NULL,
    method = "euclidean",
    symmetric = TRUE,
    diagonal = FALSE
  )
  
  # Plot
  nn.cluster.in.mb.cx.key.plot <- banc_plot_key_features(
    influence.meta = influence.dn.df %>%
      dplyr::filter(id %in% banc.an.dn.meta$id) %>%
      dplyr::mutate(target = dplyr::case_when(
        !is.na(super_cluster) ~ super_cluster,
        TRUE ~ NA
      )) %>%
      dplyr::left_join(banc.meta %>%
                         dplyr::left_join(cns.functions %>%
                                            dplyr::select(cell_type, response) %>%
                                            dplyr::distinct(cell_type, .keep_all = TRUE), 
                                          by = "cell_type") %>%
                         dplyr::mutate(response = dplyr::case_when(
                           grepl("central_complex|mushroom_body",cell_class) ~ seed_07,
                           grepl("visual",super_class) ~ response,
                           TRUE ~ NA
                         )) %>%
                         dplyr::mutate(seed = dplyr::case_when(
                           grepl("central_complex|mushroom_body",cell_class) ~ seed_07,
                           grepl("visual",super_class)&!is.na(response) ~ seed_07,
                           TRUE ~ NA
                         )) %>%
                         dplyr::mutate(seed_cell_class = dplyr::case_when(
                           grepl("central_complex",cell_class) ~ "central_complex",
                           grepl("mushroom",cell_class) ~ "mushroom_body",
                           grepl("visual",super_class) ~ "visual_projection",
                           TRUE ~ NA
                         )) %>%
                         dplyr::filter(!is.na(seed),
                                       !is.na(response),
                                       response!="") %>%
                         dplyr::distinct(seed, 
                                         .keep_all = TRUE) %>%
                         dplyr::distinct(seed, 
                                         response,
                                         seed_cell_class),
                       by=c("seed")) %>%
      dplyr::mutate(seed = gsub("_|,.*"," ",response),
                    seed = gsub(" $","",seed)) %>%
      dplyr::filter(!is.na(seed), 
                    !is.na(seed_cell_class),
                    !is.na(target)),
    ###
    inf.metric = inf.metric,
    target.map = NULL,
    width = 12,
    height = 6,
    recalculate = TRUE,
    col.annotation = "seed_cell_class",
    col.order = TRUE,
    row.annotation = NULL,
    show.annotation = FALSE,
    influence.level = NULL,
    save.path = banc.fig6.path,
    seed.map  = FALSE,
    chosen.seeds = NULL,
    chosen.targets = NULL, 
    row.cols = NULL,
    super.class = NULL,
    plot.name = sprintf("managed_neck_super_clusters_and_mb_cx_%s.pdf",inf.metric),
    rev = FALSE,
    row.dend = NULL,
    col.dend = NULL,
    method = "euclidean",
    symmetric = FALSE,
    diagonal = TRUE
  )
  
  # All by cluster
  nn.cluster.out.nn.cluster.key.plot <- banc_plot_key_features(
    influence.meta = influence.dn.df %>%
      dplyr::filter(id %in% banc.eff.meta$id) %>%
      dplyr::mutate(target = cluster) %>%
      dplyr::left_join(banc.an.dn.meta %>%
                         dplyr::distinct(seed_12, .keep_all = TRUE) %>%
                         dplyr::distinct(seed_12, umap_cluster = cluster),
                       by=c("seed"="seed_12")) %>%
      dplyr::mutate(seed = umap_cluster) %>%
      dplyr::filter(!is.na(seed), 
                    !is.na(target)),
    ###
    inf.metric = inf.metric,
    target.map = NULL,
    width = 12,
    height = 8,
    recalculate = TRUE,
    row.annotation = NULL,
    show.annotation = FALSE,
    influence.level = "seed_12",
    save.path = banc.fig5.supp.path,
    seed.map  = FALSE,
    chosen.seeds = NULL,
    chosen.targets = NULL, 
    row.cols = NULL,
    super.class = NULL,
    plot.name = sprintf("managed_neck_clusters_to_efferent_clusters_%s.pdf",inf.metric),
    rev = FALSE,
    row.dend = NULL,
    col.dend = NULL,
    method = "euclidean",
    symmetric = FALSE,
    diagonal = TRUE
  )
  
  # All by cluster
  nn.cluster.out.nn.cluster.key.plot <- banc_plot_key_features(
    influence.meta = influence.dn.df %>%
      dplyr::filter(id %in% banc.eff.meta$id) %>%
      dplyr::mutate(target = super_cluster) %>%
      dplyr::left_join(banc.an.dn.meta %>%
                         dplyr::distinct(seed_12, .keep_all = TRUE) %>%
                         dplyr::distinct(seed_12, seed_super_cluster = super_cluster),
                       by=c("seed"="seed_12")) %>%
      dplyr::mutate(seed = seed_super_cluster) %>%
      dplyr::filter(!is.na(seed), 
                    !is.na(target)),
    ###
    inf.metric = inf.metric,
    target.map = NULL,
    width = 12,
    height = 8,
    recalculate = TRUE,
    row.annotation = NULL,
    show.annotation = FALSE,
    influence.level = "seed_12",
    save.path = banc.fig5.path,
    seed.map  = FALSE,
    chosen.seeds = NULL,
    chosen.targets = NULL, 
    row.cols = NULL,
    super.class = NULL,
    plot.name = sprintf("managed_neck_super_clusters_to_efferent_super_clusters_%s.pdf",inf.metric),
    rev = TRUE,
    row.dend = NULL,
    col.dend = NULL,
    method = "euclidean",
    symmetric = FALSE,
    diagonal = TRUE
  )
}

####################
### Correlations ###
####################

# Get sensory seed map
sensory.seed.map <- c(#abdomen_endocrine_left = "abdomen_endocrine", 
  #abdomen_endocrine_right = "abdomen_endocrine", 
  abdomen_multidendritic_neuron = "abdomen multidendritic", 
  abdomen_orphan_neuron = "abdomen orphan", 
  #abdomen_strand_neuron, 
  abdominal_wall_multidendritic_neuron = "abdominal wall multidendritic", 
  antenna_bristle_neuron = "antenna bristle",
  antenna_campaniform_sensillum_neuron = "antenna campaniform", 
  antenna_hygrosensory_receptor_neuron = "antenna hygrosensory receptor", 
  antenna_olfactory_receptor_neuron = "antenna olfactory receptor", 
  #antenna_orphan_neuron = "antenna orphan", 
  antenna_thermosensory_receptor_neuron = "antenna thermosensory receptor", 
  #aorta_orphan_neuron = "aorta",
  #APDN3, 
  cibarium_multidendritic_neuron = "cibarium multidendritic", 
  crop_internal_taste_sensillum_neuron = "crop internal taste", 
  #endocrine_left = "vnc endocrine", 
  #endocrine_right = "vnc endocrine", 
  eye_bristle_neuron = "eye bristle", 
  front_leg_bristle_neuron = "leg bristle", 
  front_leg_chordotonal_organ_neuron = "leg chordotonal", 
  front_leg_claw_chordotonal_organ_neuron = "leg chordotonal", 
  front_leg_club_chordotonal_organ_neuron = "leg chordotonal", 
  front_leg_hair_plate_neuron = "leg hair plate", 
  front_leg_hook_chordotonal = "leg chordotonal", 
  front_leg_multidendritic_neuron = "leg multidendritic", 
  front_leg_campaniform_sensillum_neuron = "leg campaniform", 
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
  haltere_bristle_neuron = "haltere bristle", 
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
  wing_margin_bristle_neuron = "wing bristle",
  wing_margin_taste_peg_neuron = "wing taste", 
  wing_multidendritic_neuron = "wing multidendritic", 
  wing_tegula_campaniform_sensillum_neuron = "wing campaniform", 
  wing_tegula_chordotonal_organ_neuron = "wing chordotonal", 
  wing_tegula_hair_plate_neuron = "wing hair plate", 
  wing_tegula_orphan_neuron = "wing orphan",
  visual_front_leg_feedback = "visual leg feedback", 
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
neck.seeds <- na.omit(unique(banc.an.dn.meta$cluster))
neck.super.seeds <- na.omit(unique(banc.an.dn.meta$super_cluster))
names(neck.seeds) <- neck.seeds
sensor.seed.map <- c(sensory.seed.map,neck.seeds,neck.super.seeds)

# Get alternative dataset for validation (seed_02)
chosen.seeds <- unique(c(na.omit(unique(banc.an.dn.meta$seed_07)),
                  na.omit(banc.vpn.meta %>%
                            dplyr::left_join(cns.functions %>%
                                               dplyr::select(cell_type, response) %>%
                                               dplyr::distinct(cell_type, .keep_all = TRUE), 
                                             by = "cell_type") %>%
                            dplyr::filter(!is.na(response), 
                                          response!="") %>%
                            dplyr::pull(seed_07)),
                  na.omit(unique(banc.sens.meta$seed_02))))
con <- DBI::dbConnect(RSQLite::SQLite(),
                      file.path(banc.dropbox.influence.save.path,influence.sqlite))
chosen.ids <- banc.meta %>%
  dplyr::filter(!is.na(super_cluster)) %>%
  dplyr::pull(id)
influence.neck.and.sens.df <- dplyr::tbl(con, influence.table) %>%
  dplyr::filter(!is_seed,
                level %in% c("seed_02","seed_07"),
                seed %in% chosen.seeds,
                id %in% chosen.ids) %>%
  dplyr::collect() %>%
  dplyr::select(seed, level, id, influence_original, influence_norm_original, influence_syn_norm) %>%
  dplyr::filter(!grepl("unknown",seed))
dbDisconnect(con)

# Correlation, all by cluster
corr.nn.sens.cluster.sim.key.plot <- banc_plot_key_features(
  influence.meta = influence.neck.and.sens.df %>%
    dplyr::left_join(banc.meta %>%
                       dplyr::select(id, cell_type),
                     by = "id") %>%
    dplyr::mutate(target = cell_type) %>%
    dplyr::left_join(banc.an.dn.meta %>%
                       dplyr::distinct(seed_07, .keep_all = TRUE) %>%
                       dplyr::distinct(seed_07, umap_cluster = cluster),
                     by=c("seed"="seed_07")) %>%
    dplyr::left_join(cns.functions %>%
                       dplyr::select(cell_type, response) %>%
                       dplyr::distinct(cell_type, .keep_all = TRUE), 
                     by = c("seed"="cell_type")) %>%
    dplyr::mutate(
      seed = dplyr::case_when(
      !is.na(response) ~ response,
      !is.na(umap_cluster) ~ umap_cluster,
      grepl("^AN|^DN|7",seed) ~ NA,
      TRUE ~ seed
    )) %>%
    dplyr::filter(!is.na(target),
                  !is.na(seed),
                  target!="0",
                  target!="",
                  seed!="0",
                  seed!=""),
  ###
  color.min = 0.00,
  color.max = 0.5,
  row.select = unname(sensory.seed.map),
  col.select = na.omit(unique(banc.an.dn.meta$cluster)),
  inf.metric = "influence",
  target.map = NULL,
  width = 12,
  height = 12,
  recalculate = TRUE,
  row.annotation = NULL,
  show.annotation = FALSE,
  influence.level = NULL,
  save.path = banc.fig3.path,
  seed.map  = sensor.seed.map,
  chosen.seeds = NULL,
  chosen.targets = NULL, 
  row.cols = NULL,
  super.class = NULL,
  plot.name = sprintf("correlation_neck_clusters_and_sensors_%s.pdf","influence"),
  rev = TRUE,
  row.thresh = 0.95,
  autocorrelation = TRUE,
  symmetric = FALSE,
  diagonal = TRUE
)

# Correlation, all by super cluster
corr.nn.sens.super.cluster.sim.key.plot <- banc_plot_key_features(
  influence.meta = influence.neck.and.sens.df %>%
    dplyr::left_join(banc.meta %>%
                       dplyr::select(id, cell_type),
                     by = "id") %>%
    dplyr::mutate(target = cell_type) %>%
    dplyr::left_join(banc.an.dn.meta %>%
                       dplyr::distinct(seed_07, .keep_all = TRUE) %>%
                       dplyr::distinct(seed_07, seed_super_cluster = super_cluster),
                     by=c("seed"="seed_07")) %>%
    dplyr::left_join(cns.functions %>%
                       dplyr::select(cell_type, response) %>%
                       dplyr::distinct(cell_type, .keep_all = TRUE), 
                     by = c("seed"="cell_type")) %>%
    dplyr::mutate(
      seed = dplyr::case_when(
        !is.na(response) ~ response,
        !is.na(seed_super_cluster) ~ seed_super_cluster,
        grepl("^AN|^DN|7",seed) ~ NA,
        TRUE ~ seed
      )) %>%
    dplyr::filter(!is.na(target),
                  !is.na(seed),
                  target!="0",
                  target!="",
                  seed!="0",
                  seed!=""),
  ###
  color.min = 0.00,
  color.max = 0.5,
  row.select = unname(sensory.seed.map),
  col.select = na.omit(unique(banc.an.dn.meta$super_cluster)),
  inf.metric = "influence",
  target.map = NULL,
  cellheight = 8,
  cellwidth = 8,
  width = 8,
  height = 8,
  recalculate = TRUE,
  row.annotation = NULL,
  show.annotation = FALSE,
  col.order = super.clust.order,
  influence.level = NULL,
  save.path = banc.fig3.path,
  seed.map  = sensor.seed.map,
  chosen.seeds = NULL,
  chosen.targets = NULL, 
  row.cols = NULL,
  super.class = NULL,
  plot.name = sprintf("correlation_neck_super_clusters_and_sensors_%s.pdf","influence"),
  rev = TRUE,
  row.thresh = 0.95,
  autocorrelation = TRUE,
  symmetric = FALSE,
  diagonal = TRUE
)

