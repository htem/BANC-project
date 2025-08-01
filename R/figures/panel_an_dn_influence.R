##################################
## FIGURE 3: AN/DN INFLUENCE   ##
##################################
# Analyses neural influence propagation between ascending/descending neurons
# and downstream motor targets. Shows how sensory input streams flow through
# neck connective clusters to control specific motor outputs and body parts.
# Output: figures/figure3/links/neck_*_influence_*.pdf

####################
## STARTUP        ##
####################

source("R/startup/banc-startup.R")
source("R/startup/franken-meta.R")
source("R/startup/banc-meta.R")
source("R/startup/banc-functions.R")
source("R/startup/banc_an_dn_data.R")

####################
## METADATA PREP  ##
####################

# Define control neuron types for comparative analysis
control.types <- na.omit(unique(franken.meta$cell_type[grepl("mushroom_body_input|mushroom_body_output|central_complex_input|dopa|kenyon_",franken.meta$cell_class)]))
banc.eff2.meta <- banc.eff.meta %>%
  dplyr::mutate(body_part_effector = dplyr::case_when(
    grepl("power|steering|tension",cell_function) ~ cell_function,
    grepl("pitch|yaw|roll",cell_function_detailed) ~ cell_function_detailed,
    TRUE ~ body_part_effector
  ))
banc.an.dn.meta <- banc.meta %>%
  dplyr::filter(super_class %in% c("ascending","descending")) %>%
  dplyr::filter(!grepl("^SA|^SN|^AN_4|AN_5",cell_type))
banc.targets <- banc.meta %>%
  dplyr::filter(super_class %in% c("descending","ascending","visual_centrifugal")|#'sensory","sensory_ascending","sensory_descending"
                  cell_type %in% control.types|#grepl("KC",cell_type)|
                  root_id%in%!!banc.eff2.meta$root_id)

####################
## INFLUENCE DATA ##
####################

# Extract influence scores from database for different seed types
con <- DBI::dbConnect(RSQLite::SQLite(),
                      file.path(banc.dropbox.influence.save.path,influence.sqlite))
chosen.cts <- banc.vpn.meta %>%
  dplyr::filter(!is.na(cell_function)) %>%
  dplyr::distinct(cell_type) %>%
  dplyr::pull(cell_type)
influence.vpn.df <- dplyr::tbl(con, influence.table) %>%
  dplyr::filter(!is_seed,
                level %in% c("seed_07"),
                seed %in% !!chosen.cts) %>%
  dplyr::select(seed, level, id, influence_original, influence_norm_original, influence_syn_norm) %>%
  dplyr::collect()
dbDisconnect(con)

# Extract sensory influence data for validation analysis
con <- DBI::dbConnect(RSQLite::SQLite(),
                      file.path(banc.dropbox.influence.save.path,influence.sqlite))
influence.sens.df <- dplyr::tbl(con, influence.table) %>%
  dplyr::filter(!is_seed,
                level %in% c("seed_02")) %>%
  dplyr::select(seed, level, id, influence_original, influence_norm_original, influence_syn_norm) %>%
  dplyr::collect() %>%
  dplyr::filter(!grepl("unknown",seed))
dbDisconnect(con)

# Extract central complex and mushroom body influence data
con <- DBI::dbConnect(RSQLite::SQLite(),
                      file.path(banc.dropbox.influence.save.path,influence.sqlite))
chosen.cts <- unique(franken.meta$cell_type[grepl("central_complex_output|mushroom_body_output",franken.meta$cell_class)])
influence.functions.df <- dplyr::tbl(con, influence.table) %>%
  dplyr::filter(!is_seed,
                level %in% c("seed_07"),
                seed %in% !!chosen.cts) %>%
  dplyr::select(seed, level, id, influence_original, influence_norm_original, influence_syn_norm) %>%
  dplyr::collect() 
dbDisconnect(con)

# Extract descending neuron influence data for neck analysis
con <- DBI::dbConnect(RSQLite::SQLite(),
                      file.path(banc.dropbox.influence.save.path,influence.sqlite))
chosen.seeds <- na.omit(unique(banc.an.dn.meta$seed_12))
influence.dn.df <- dplyr::tbl(con, influence.table) %>%
  dplyr::filter(!is_seed,
                level %in% c("seed_12"),
                seed %in% !!chosen.seeds,
                id %in% !!banc.targets$root_id) %>%
  dplyr::select(seed, level, id, influence_original, influence_norm_original, influence_syn_norm) %>%
  dplyr::collect()
dbDisconnect(con)

####################
## DATA FORMATTING ##
####################

# Add metadata and normalise influence scores for visualisation
influence.dn.df <- influence.dn.df %>%
  dplyr::left_join(banc.meta %>%
                     dplyr::distinct(root_id, cell_type, cell_sub_class, cell_class, cluster, cns_network, super_cluster),
                   by = c("id"="root_id")) %>%
  dplyr::ungroup() %>%
  calculate_influence_norms()

influence.vpn.df <- influence.vpn.df %>%
  dplyr::left_join(banc.meta %>%
                     dplyr::distinct(id, cell_type, cell_sub_class, cell_class, cluster, cns_network, super_cluster),
                   by = c("id")) %>%
  dplyr::filter(!is.na(level)) %>%
  dplyr::ungroup() %>%
  calculate_influence_norms()

influence.functions.df <- influence.functions.df %>%
  dplyr::left_join(banc.meta %>%
                     dplyr::distinct(id, cell_type, cell_sub_class, cell_class,  cluster, cns_network, super_cluster),
                   by = c("id")) %>%
  dplyr::filter(!is.na(level)) %>%
  calculate_influence_norms()

influence.sens.df <- influence.sens.df %>%
  dplyr::left_join(banc.meta %>%
                     dplyr::distinct(id, cell_type, cell_sub_class, cell_class,  cluster, cns_network, super_cluster),
                   by = c("id")) %>%
  calculate_influence_norms()

####################
## SENSORY TO NECK ##
####################

# Analyse sensory influence on neck super clusters
nn.super.cluster.in.sens.key.plot <- banc_plot_key_features(
  influence.meta = influence.sens.df %>%
    dplyr::filter(!is.na(seed))  %>%
    dplyr::left_join(umap.dn.df %>%
                       dplyr::distinct(id, .keep_all = TRUE) %>%
                       dplyr::distinct(id, target_cluster = super_cluster),
                     by=c("id")) %>%
    dplyr::left_join(banc.meta %>%
                       dplyr::distinct(seed_02, .keep_all = TRUE) %>%
                       dplyr::select(seed = seed_02, seed_class = body_part_sensory),
                     by = "seed") %>%
    dplyr::mutate(target = target_cluster) %>%
    dplyr::filter(!is.na(target),
                  !is.na(seed),
                  seed!="0",
                  target!="0") %>%
    rbind.fill(influence.vpn.df %>%
                   dplyr::filter(!is.na(seed))  %>%
                   dplyr::left_join(umap.dn.df %>%
                                      dplyr::distinct(id, .keep_all = TRUE) %>%
                                      dplyr::distinct(id, target_cluster = super_cluster),
                                    by=c("id")) %>%
                   dplyr::left_join(cns.functions %>%
                                      dplyr::select(seed = cell_type, vpn_function = response) %>%
                                      dplyr::distinct(seed, .keep_all = TRUE),
                                    by = "seed") %>%
                   dplyr::mutate(seed = vpn_function) %>%
                   dplyr::mutate(target = target_cluster) %>%
                   dplyr::filter(!is.na(target),
                                 !is.na(seed),
                                 seed!="0",
                                 target!="0",
                                 seed!="",
                                 !grepl("polarized",seed))),
  influence.level = NULL,
  seed.map = sensory.seed.map,
  inf.metric = inf.metric,
  save.path = banc.fig3.path,
  target.map = NULL,
  recalculate = TRUE,
  row.annotation = NULL,
  col.annotation = NULL,
  show.annotation = FALSE,
  col.thresh = 0.1,
  row.order = super.clust.order,
  super.class = NULL,
  width = 16,
  height = 8,
  cellheight = 7,
  cellwidth = 7,
  plot.name = sprintf("neck_super_clusters_from_all_sensors_%s.pdf",inf.metric),
  rev = FALSE, 
  method = "euclidean"
)

# Detailed cluster-level analysis of sensory influence on neck neurons
nn.cluster.in.sens.key.plot <- banc_plot_key_features(
  influence.meta = influence.sens.df %>%
    dplyr::filter(!is.na(seed))  %>%
    dplyr::left_join(umap.dn.df %>%
                       dplyr::distinct(id, .keep_all = TRUE) %>%
                       dplyr::distinct(id, target_cluster = cluster),
                     by=c("id")) %>%
    dplyr::left_join(banc.meta %>%
                       dplyr::distinct(seed_02, .keep_all = TRUE) %>%
                       dplyr::select(seed = seed_02, seed_class = body_part_sensory),
                     by = "seed") %>%
    dplyr::mutate(target = target_cluster) %>%
    dplyr::filter(!is.na(target),
                  !is.na(seed),
                  seed!="0",
                  target!="0"),
  influence.level = c("seed_02"),
  seed.map = sensory.seed.map,
  inf.metric = inf.metric,
  save.path = banc.fig3.path,
  target.map = NULL,
  recalculate = TRUE,
  row.annotation = NULL,
  col.annotation = NULL,
  show.annotation = FALSE,
  col.thresh = 0.1,
  super.class = NULL,
  width = 14,
  height = 14,
  plot.name = sprintf("neck_clusters_from_all_sensors_%s.pdf",inf.metric),
  rev = TRUE, 
  method = "euclidean"
)

######################
## NECK TO EFFECTORS ##
######################

# Analyse neck super cluster influence on specific effector cell types
nn.super.cluster.out.efferent.key.plot <- banc_plot_key_features(
  influence.meta = influence.dn.df %>%
    dplyr::mutate(target = cell_sub_class) %>%
    dplyr::filter(id %in% banc.eff2.meta$id,
                  !is.na(target)) %>%
    dplyr::left_join(banc.eff.meta %>%
                       dplyr::distinct(id, .keep_all = TRUE) %>%
                       dplyr::select(id, super_class),
                     by ="id") %>%
    dplyr::left_join(umap.dn.df %>%
                       dplyr::distinct(seed_12, .keep_all = TRUE) %>%
                       dplyr::distinct(seed_12, seed_super_cluster = super_cluster),
                     by=c("seed"="seed_12")) %>%
    dplyr::mutate(seed = seed_super_cluster) %>%
    dplyr::filter(!is.na(target),
                  !is.na(seed),
                  seed!="0",
                  target!="0"),
  ###
  inf.metric = inf.metric,
  target.map = efferent.target.map,
  width = 14,
  height = 8,
  recalculate = TRUE,
  col.annotation = NULL,
  row.annotation = "super_class",
  show.annotation = FALSE,
  influence.level = "seed_12",
  save.path = banc.fig3.path,
  seed.map  = FALSE,
  chosen.seeds = NULL,
  chosen.targets = efferent.target.map, 
  row.cols = NULL,
  row.order = TRUE,
  col.order = super.clust.order,
  super.class = NULL,
  cellheight = 7,
  cellwidth = 7,
  plot.name = sprintf("neck_super_clusters_to_effector_cell_sub_class_%s.pdf",inf.metric),
  rev = TRUE,
  method = "euclidean"
)


# Analyse influence on effector super clusters
nn.super.cluster.out.efferent.key.plot <- banc_plot_key_features(
  influence.meta = influence.dn.df %>%
    dplyr::mutate(target = super_cluster) %>%
    dplyr::filter(id %in% banc.eff2.meta$id,
                  !is.na(target)) %>%
    dplyr::left_join(umap.dn.df %>%
                       dplyr::distinct(seed_12, .keep_all = TRUE) %>%
                       dplyr::distinct(seed_12, seed_super_cluster = super_cluster),
                     by=c("seed"="seed_12")) %>%
    dplyr::mutate(seed = seed_super_cluster) %>%
    dplyr::filter(!is.na(target),
                  !is.na(seed),
                  seed!="0",
                  target!="0"),
  ###
  inf.metric = inf.metric,
  width = 12,
  height = 8,
  recalculate = TRUE,
  col.annotation = NULL,
  show.annotation = FALSE,
  influence.level = "seed_12",
  save.path = banc.fig3.path,
  seed.map  = FALSE,
  chosen.seeds = NULL,
  row.cols = NULL,
  row.order = TRUE,
  col.order = super.clust.order,
  super.class = NULL,
  cellheight = 7,
  cellwidth = 7,
  plot.name = sprintf("neck_super_clusters_to_effector_super_cluster_%s.pdf",inf.metric),
  rev = TRUE,
  method = "euclidean"
)

# Detailed cluster-level analysis of neck to effector influence
nn.cluster.out.efferent.key.plot <- banc_plot_key_features(
  influence.meta = influence.dn.df %>%
    dplyr::mutate(target = cell_sub_class) %>%
    dplyr::filter(id %in% banc.eff2.meta$id,
                  !is.na(target)) %>%
    dplyr::left_join(umap.dn.df %>%
                       dplyr::distinct(seed_12, .keep_all = TRUE) %>%
                       dplyr::distinct(seed_12, umap_cluster = cluster),
                     by=c("seed"="seed_12")) %>%
    dplyr::mutate(seed = umap_cluster) %>%
    dplyr::filter(!is.na(target),
                  !is.na(seed),
                  seed!="0",
                  target!="0"),
  ###
  inf.metric = inf.metric,
  target.map = efferent.target.map,
  width = 14,
  height = 8,
  recalculate = TRUE,
  row.annotation = NULL,
  show.annotation = FALSE,
  influence.level = "seed_12",
  save.path = banc.fig3.supp.path,
  seed.map  = FALSE,
  chosen.seeds = NULL,
  chosen.targets = efferent.target.map, 
  row.cols = NULL,
  super.class = NULL,
  plot.name = sprintf("neck_clusters_to_effector_cell_sub_class_%s.pdf",inf.metric),
  rev = FALSE,
  method = "euclidean"
)

######################
## VPN FUNCTION TO NECK ##
######################

# Analyse visual projection neuron functional influence on neck super clusters
nn.cluster.vpn.function.in.key.plot <- banc_plot_key_features(
  influence.meta = influence.vpn.df %>%
    dplyr::filter(!is.na(seed))  %>%
    dplyr::left_join(umap.dn.df %>%
                       dplyr::distinct(cell_type, .keep_all = TRUE) %>%
                       dplyr::distinct(cell_type, target_cluster = super_cluster),
                     by=c("cell_type")) %>%
    dplyr::left_join(cns.functions %>%
                       dplyr::select(seed = cell_type, vpn_function = response) %>%
                       dplyr::distinct(seed, .keep_all = TRUE),
                     by = "seed") %>%
    dplyr::mutate(seed = vpn_function) %>%
    dplyr::mutate(target = target_cluster) %>%
    dplyr::filter(!is.na(target),
                  !is.na(seed),
                  seed!="0",
                  target!="0",
                  seed!="",
                  !grepl("polarized",seed)),
  influence.level = c("seed_07"),
  seed.map = NULL,
  inf.metric = "influence_log",
  save.path = banc.fig3.path,
  target.map = NULL,
  recalculate = TRUE,
  row.annotation = NULL,
  col.annotation = NULL,
  show.annotation = FALSE,
  col.thresh = 0.1,
  super.class = NULL,
  width = 6,
  height = 6,
  plot.name = sprintf("neck_super_clusters_from_visual_projection_functions_%s.pdf","influence_log"),
  rev = FALSE, 
  method = "euclidean"
)

# Detailed cluster-level analysis of VPN functional influence
nn.cluster.vpn.function.in.key.plot <- banc_plot_key_features(
  influence.meta = influence.vpn.df %>%
    dplyr::filter(!is.na(seed))  %>%
    dplyr::left_join(umap.dn.df %>%
                       dplyr::distinct(cell_type, .keep_all = TRUE) %>%
                       dplyr::distinct(cell_type, target_cluster = cluster),
                     by=c("cell_type")) %>%
    dplyr::left_join(cns.functions %>%
                       dplyr::select(seed = cell_type, vpn_function = response) %>%
                       dplyr::distinct(seed, .keep_all = TRUE),
                     by = "seed") %>%
    dplyr::mutate(seed = vpn_function) %>%
    dplyr::mutate(target = target_cluster) %>%
    dplyr::filter(!is.na(target),
                  !is.na(seed),
                  seed!="0",
                  target!="0",
                  seed!="",
                  !grepl("polarized",seed)),
  influence.level = c("seed_07"),
  seed.map = NULL,
  inf.metric = inf.metric,
  save.path = banc.fig3.supp.path,
  target.map = NULL,
  recalculate = TRUE,
  row.annotation = NULL,
  col.annotation = NULL,
  show.annotation = FALSE,
  super.class = NULL,
  width = 14,
  height = 4,
  plot.name = sprintf("neck_clusters_from_visual_projection_functions_%s.pdf",inf.metric),
  rev = TRUE, 
  method = "euclidean"
)

##########################
## VPN CELL TYPES TO NECK ##
##########################

# Analyse individual VPN cell type influence on neck clusters
nn.cluster.vpn.cts.in.key.plot <- banc_plot_key_features(
  influence.meta = influence.vpn.df %>%
    dplyr::filter(!is.na(seed))  %>%
    dplyr::left_join(umap.dn.df %>%
                       dplyr::distinct(cell_type, .keep_all = TRUE) %>%
                       dplyr::distinct(cell_type, target_cluster = cluster),
                     by=c("cell_type")) %>%
    dplyr::mutate(target = target_cluster) %>%
    dplyr::filter(!is.na(target),
                  !is.na(seed),
                  seed!="0",
                  seed!="",
                  target!="0"),
  influence.level = c("seed_07"),
  seed.map = NULL,
  inf.metric = inf.metric,
  save.path = banc.fig3.supp.path,
  target.map = NULL,
  recalculate = TRUE,
  row.annotation = NULL,
  col.annotation = NULL,
  show.annotation = FALSE,
  col.thresh = 0.9,
  row.thresh = 0.9,
  super.class = NULL,
  width = 60,
  height = 12,
  plot.name = sprintf("neck_clusters_from_visual_projection_cell_types_%s.pdf",inf.metric),
  rev = FALSE, 
  method = "euclidean"
)

############################
## MB/CX CELL TYPES TO NECK ##
############################

# Analyse mushroom body and central complex influence on neck super clusters
nn.super.cluster.mb.cx.in.key.plot <- banc_plot_key_features(
  influence.meta = influence.functions.df %>%
    dplyr::filter(!is.na(seed),
                  id %in% banc.an.dn.meta$id) %>%
    dplyr::left_join(umap.dn.df %>%
                       dplyr::distinct(cell_type, .keep_all = TRUE) %>%
                       dplyr::distinct(cell_type, target_cluster = super_cluster),
                     by=c("cell_type")) %>%
    dplyr::left_join(banc.meta %>%
                       dplyr::distinct(seed_07, .keep_all = TRUE) %>%
                       dplyr::distinct(seed_07, seed_class = cell_class),
                     by=c("seed"="seed_07")) %>%
    dplyr::mutate(target = target_cluster,
                  seed = gsub("_neuron|_"," ",seed)) %>%
    dplyr::filter(!is.na(target),
                  !is.na(seed),
                  seed!="0",
                  seed!="",
                  target!="0"),
  influence.level = c("seed_07"),
  seed.map = NULL,
  inf.metric = "influence_norm_log",
  save.path = banc.fig6.path,
  target.map = NULL,
  recalculate = TRUE,
  row.annotation = NULL,
  col.annotation = "seed_class",
  show.annotation = FALSE,
  col.thresh = 0.5,
  super.class = NULL,
  width = 14,
  height = 16,
  plot.name = sprintf("neck_super_clusters_from_mb_and_cx_%s.pdf","influence_norm_log"),
  rev = FALSE, 
  method = "euclidean"
)

# Detailed cluster-level analysis of MB/CX influence
nn.cluster.mb.cx.in.key.plot <- banc_plot_key_features(
  influence.meta = influence.functions.df %>%
    dplyr::filter(!is.na(seed)) %>%
    dplyr::left_join(umap.dn.df %>%
                       dplyr::distinct(cell_type, .keep_all = TRUE) %>%
                       dplyr::distinct(cell_type, target_cluster = cluster),
                     by=c("cell_type")) %>%
    dplyr::left_join(banc.meta %>%
                       dplyr::distinct(seed_07, .keep_all = TRUE) %>%
                       dplyr::distinct(seed_07, seed_class = cell_class),
                     by=c("seed"="seed_07")) %>%
    dplyr::mutate(target = target_cluster,
                  seed = gsub("_neuron|_"," ",seed)) %>%
    dplyr::filter(!is.na(target),
                  !is.na(seed),
                  seed!="0",
                  seed!="",
                  target!="0"),
  influence.level = c("seed_07"),
  seed.map = NULL,
  inf.metric = "influence_norm_log",
  save.path = banc.fig6.supp.path,
  target.map = NULL,
  recalculate = TRUE,
  row.annotation = NULL,
  col.annotation = "seed_class",
  show.annotation = FALSE,
  col.thresh = 0.5,
  super.class = NULL,
  width = 14,
  height =16,
  plot.name = sprintf("neck_clusters_from_mb_and_cx_%s.pdf","influence_norm_log"),
  rev = TRUE, 
  method = "euclidean"
)

############################
## NECK TO MB/CX CELL TYPES ##
############################

# Analyse neck super cluster influence on mushroom body and central complex
chosen.cts <- unique(franken.meta$cell_type[grepl("central_complex_input|mushroom_body|mushroom_body_dopamin|dopa",franken.meta$cell_class)])
nn.super.cluster.out.mb.cx.key.plot <- banc_plot_key_features(
  influence.meta = influence.dn.df %>%
    dplyr::filter(cell_type %in% chosen.cts) %>%
    dplyr::mutate(target = dplyr::case_when(
      !is.na(cell_sub_class) ~ cell_sub_class,
      TRUE ~ cell_class
    )) %>%
    dplyr::filter(!is.na(target)) %>%
    dplyr::left_join(umap.dn.df %>%
                       dplyr::distinct(seed_12, .keep_all = TRUE) %>%
                       dplyr::distinct(seed_12, umap_cluster = super_cluster),
                     by=c("seed"="seed_12")) %>%
    dplyr::mutate(seed = umap_cluster,
                  target = gsub("_neuron|_"," ",target)) %>%
    dplyr::filter(!is.na(target),
                  !is.na(seed),
                  seed!="0",
                  seed!="",
                  target!="0"),
  ###
  inf.metric = "influence_norm_log",
  target.map = NULL,
  width = 6,
  height = 6,
  recalculate = TRUE,
  row.annotation = NULL,
  show.annotation = FALSE,
  influence.level = "seed_12",
  save.path = banc.fig6.path,
  seed.map  = FALSE,
  chosen.seeds = NULL,
  chosen.targets = NULL, 
  row.cols = NULL,
  super.class = NULL,
  col.order = super.clust.order,
  plot.name = sprintf("neck_super_clusters_to_mb_cx_%s.pdf","influence_norm_log"),
  rev = FALSE,
  method = "euclidean"
)

# Detailed cluster-level analysis of neck to MB/CX influence
nn.cluster.out.mb.cx.key.plot <- banc_plot_key_features(
  influence.meta = influence.dn.df %>%
    dplyr::filter(cell_type %in% chosen.cts) %>%
    dplyr::mutate(target = cell_type) %>%
    dplyr::filter(!is.na(target)) %>%
    dplyr::left_join(umap.dn.df %>%
                       dplyr::distinct(seed_12, .keep_all = TRUE) %>%
                       dplyr::distinct(seed_12, umap_cluster = cluster),
                     by=c("seed"="seed_12")) %>%
    dplyr::mutate(seed = umap_cluster,
                  target = gsub("_neuron|_"," ",target)) %>%
    dplyr::filter(!is.na(target),
                  !is.na(seed),
                  seed!="0",
                  seed!="",
                  target!="0"),
  ###
  inf.metric = "influence_norm_log",
  target.map = NULL,
  width = 14,
  height = 24,
  recalculate = TRUE,
  row.annotation = NULL,
  show.annotation = FALSE,
  influence.level = "seed_12",
  save.path = banc.fig6.supp.path,
  seed.map  = FALSE,
  chosen.seeds = NULL,
  chosen.targets = NULL, 
  row.thresh = 0.95,
  row.cols = NULL,
  super.class = NULL,
  plot.name = sprintf("neck_clusters_to_mb_cx_%s.pdf","influence_norm_log"),
  rev = FALSE,
  method = "euclidean"
)

####################
## NECK TO VCN    ##
####################

# Analyse neck cluster influence on visual centrifugal neurons
nn.cluster.out.vcn.key.plot <- banc_plot_key_features(
  influence.meta = influence.dn.df %>%
    dplyr::left_join(banc.meta %>%
                       dplyr::select(id, super_class),
                     by = "id") %>%
    dplyr::filter(super_class=="visual_centrifugal") %>%
    dplyr::mutate(target = cell_type) %>%
    dplyr::filter(!is.na(target)) %>%
    dplyr::left_join(umap.dn.df %>%
                       dplyr::distinct(seed_12, .keep_all = TRUE) %>%
                       dplyr::distinct(seed_12, umap_cluster = cluster),
                     by=c("seed"="seed_12")) %>%
    dplyr::mutate(seed = umap_cluster) %>%
    dplyr::filter(!is.na(target),
                  !is.na(seed),
                  seed!="0",
                  seed!="",
                  target!="0"),
  ###
  inf.metric = "influence_norm_log",
  target.map = NULL,
  width = 14,
  height = 24,
  recalculate = TRUE,
  row.annotation = NULL,
  show.annotation = FALSE,
  influence.level = "seed_12",
  save.path = banc.fig4.supp.path,
  seed.map  = FALSE,
  chosen.seeds = NULL,
  chosen.targets = NULL, 
  row.thresh = 0.25,
  row.cols = NULL,
  super.class = NULL,
  plot.name = sprintf("neck_clusters_to_visual_centrifugal_%s.pdf","influence_norm_log"),
  rev = FALSE,
  method = "euclidean"
)

#############################
## NECK FROM CONTROLLERS   ##
#############################

# Analyse how controller neurons (MB/CX/VPN) influence neck super clusters
nn.super.cluster.in.mb.cx.key.plot <- banc_plot_key_features(
  influence.meta = influence.functions.df %>%
    dplyr::filter(id %in% banc.an.dn.meta$id, 
                  level == "seed_07") %>%
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
                  !is.na(target),
                  seed != "0",
                  target != "0"),
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
  plot.name = sprintf("neck_super_clusters_and_mb_cx_%s.pdf",inf.metric),
  rev = FALSE,
  row.dend = NULL,
  col.dend = NULL,
  method = "euclidean",
  symmetric = FALSE,
  diagonal = TRUE
)

######################
## EFFERENT CLUSTERS ##
######################

# Analyse neck super cluster influence on efferent super clusters
nn.super.cluster.out.nn.cluster.key.plot <- banc_plot_key_features(
  influence.meta = influence.dn.df %>%
    dplyr::filter(id %in% banc.eff.meta$id) %>%
    dplyr::mutate(target = super_cluster) %>%
    dplyr::left_join(banc.an.dn.meta %>%
                       dplyr::distinct(seed_12, .keep_all = TRUE) %>%
                       dplyr::distinct(seed_12, seed_super_cluster = super_cluster),
                     by=c("seed"="seed_12")) %>%
    dplyr::mutate(seed = seed_super_cluster) %>%
    dplyr::filter(!is.na(target),
                  !is.na(seed),
                  seed!="0",
                  seed!="",
                  target!="0"),
  ###
  inf.metric = inf.metric,
  target.map = NULL,
  width = 12,
  height = 8,
  recalculate = TRUE,
  row.annotation = NULL,
  show.annotation = FALSE,
  influence.level = "seed_12",
  save.path = banc.fig3.path,
  seed.map  = FALSE,
  chosen.seeds = NULL,
  chosen.targets = NULL, 
  row.cols = NULL,
  super.class = NULL,
  plot.name = sprintf("neck_super_clusters_to_efferent_super_clusters_%s.pdf",inf.metric),
  rev = FALSE,
  row.dend = NULL,
  col.dend = NULL,
  method = "euclidean",
  symmetric = FALSE,
  diagonal = TRUE
)

# Detailed cluster-level analysis of neck to efferent connections
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
  save.path = banc.fig3.supp.path,
  seed.map  = FALSE,
  chosen.seeds = NULL,
  chosen.targets = NULL, 
  row.cols = NULL,
  super.class = NULL,
  plot.name = sprintf("neck_clusters_to_efferent_clusters_%s.pdf",inf.metric),
  rev = FALSE,
  row.dend = NULL,
  col.dend = NULL,
  method = "euclidean",
  symmetric = FALSE,
  diagonal = TRUE
)

# Analyse sensory modality influence on neck super clusters
nn.cluster.out.nn.cluster.key.plot <- banc_plot_key_features(
  influence.meta = influence.sens.df %>%
    dplyr::filter(id %in% banc.an.dn.meta$id) %>%
    dplyr::mutate(target = super_cluster) %>%
    dplyr::left_join(banc.sens.meta %>%
                       dplyr::distinct(seed_02, .keep_all = TRUE) %>%
                       dplyr::distinct(seed_02, seed_super_cluster = body_part_sensory),
                     by=c("seed"="seed_02")) %>%
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
  influence.level = NULL,
  save.path = banc.fig5.path,
  seed.map  = FALSE,
  chosen.seeds = NULL,
  chosen.targets = NULL, 
  row.cols = NULL,
  super.class = NULL,
  col.thresh = 0.15,
  plot.name = sprintf("sensory_modalities_to_neck_super_clusters_%s.pdf",inf.metric),
  rev = FALSE,
  row.dend = NULL,
  col.dend = NULL,
  method = "euclidean",
  symmetric = FALSE,
  diagonal = TRUE
)

####################
## NECK TO NECK   ##
####################

# Analyse inter-neck super cluster influence patterns
nn.super.cluster.out.nn.cluster.key.plot <- banc_plot_key_features(
  influence.meta = influence.dn.df %>%
    dplyr::filter(id %in% banc.an.dn.meta$id) %>%
    dplyr::mutate(target = super_cluster) %>%
    dplyr::left_join(banc.an.dn.meta %>%
                       dplyr::distinct(seed_12, .keep_all = TRUE) %>%
                       dplyr::distinct(seed_12, seed_super_cluster = super_cluster),
                     by=c("seed"="seed_12")) %>%
    dplyr::mutate(seed = seed_super_cluster) %>%
    dplyr::filter(!is.na(target),
                  !is.na(seed),
                  seed!="0",
                  seed!="",
                  target!="0"),
  ###
  inf.metric = inf.metric,
  target.map = NULL,
  width = 6,
  height = 6,
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
  plot.name = sprintf("neck_super_clusters_to_neck_super_clusters_%s.pdf",inf.metric),
  rev = FALSE,
  row.dend = NULL,
  col.dend = NULL,
  method = "euclidean",
  symmetric = TRUE,
  diagonal = FALSE
)

# # All by cluster
# nn.cluster.out.kcs.key.plot <- banc_plot_key_features(
#   influence.meta = influence.dn.df %>%
#     dplyr::filter(grepl("^KC",cell_type)|cell_class=="kenyon_cell") %>%
#     dplyr::mutate(target = id) %>%
#     dplyr::filter(!is.na(target)) %>%
#     dplyr::left_join(umap.dn.df %>%
#                        # dplyr::left_join(banc.an.dn.meta %>%
#                        #                    dplyr::select(root_id, seed_12),
#                        #                  by = c("id"="root_id")) %>%
#                        dplyr::distinct(seed_12, .keep_all = TRUE) %>%
#                        dplyr::distinct(seed_12, umap_cluster = cluster),
#                      by=c("seed"="seed_12")) %>%
#     dplyr::mutate(seed = umap_cluster) %>%
#     dplyr::filter(!is.na(umap_cluster)),
#   ###
#   inf.metric = "influence_log",
#   target.map = NULL,
#   width = 14,
#   height = 14,
#   cellheight = 0.2,
#   cellwidth= 12,
#   recalculate = TRUE,
#   row.annotation = NULL,
#   show.annotation = FALSE,
#   influence.level = "seed_12",
#   save.path = banc.fig4.extra.path,
#   seed.map  = FALSE,
#   chosen.seeds = NULL,
#   chosen.targets = NULL, 
#   col.thresh = 0.25,
#   row.cols = NULL,
#   super.class = NULL,
#   show.rownames = FALSE,
#   plot.name = sprintf("neck_clusters_to_kcs_%s.pdf","influence_log"),
#   #col.dend = nn.dn.cluster.out.efferent.key.plot$col.dend,
#   rev = FALSE
# )

# # All by cluster
# nn.cluster.out.sens.key.plot <- banc_plot_key_features(
#   influence.meta = influence.dn.df %>%
#     dplyr::filter(grepl("sensory",super_class)) %>%
#     dplyr::mutate(target = cell_sub_class) %>%
#     dplyr::filter(!is.na(target)) %>%
#     dplyr::left_join(umap.dn.df %>%
#                        # dplyr::left_join(banc.an.dn.meta %>%
#                        #                    dplyr::select(root_id, seed_12),
#                        #                  by = c("id"="root_id")) %>%
#                        dplyr::distinct(seed_12, .keep_all = TRUE) %>%
#                        dplyr::distinct(seed_12, umap_cluster = cluster),
#                      by=c("seed"="seed_12")) %>%
#     dplyr::mutate(seed = umap_cluster) %>%
#     dplyr::filter(!is.na(umap_cluster)),
#   ###
#   inf.metric = "influence_log",
#   target.map = NULL,
#   width = 14,
#   height = 24,
#   recalculate = TRUE,
#   row.annotation = NULL,
#   show.annotation = FALSE,
#   influence.level = "seed_12",
#   save.path = banc.fig4.extra.path,
#   seed.map  = FALSE,
#   chosen.seeds = NULL,
#   chosen.targets = NULL, 
#   #row.thresh = 0.25,
#   row.cols = NULL,
#   super.class = NULL,
#   plot.name = sprintf("neck_clusters_to_sensory_cell_sub_class_%s.pdf","influence_log"),
#   #col.dend = nn.dn.cluster.out.efferent.key.plot$col.dend,
#   rev = FALSE
# )


########################
## NETWORK ANALYSIS   ##
########################

# Create directed network graph of super cluster interactions
m4 <- nn.super.cluster.out.nn.cluster.key.plot$influence.matrix
m4[is.na(m4)] <- 0

# Edge list: super cluster -> CNS cluster (from columns in m1)
edges <- as.data.frame(t(as.table(m4)))
colnames(edges) <- c("to", "from", "weight")
edges <- edges[edges$weight > 0, ]

# 1. Compute 50th percentile threshold of edge weights
thresh <- quantile(edges$weight, 0.85, na.rm = TRUE)

# 2. Filter edges above threshold
edges <- edges[edges$weight > thresh, ]

# 3. Calculate log-weight for plotting (add a small constant if needed to avoid log(0))
edges$logweight <- log(edges$weight)

# Node dataframe & type/col
nodes <- data.frame(
  name = unique(c(edges$from, edges$to))
)

# Create the directed graph
g <- igraph::graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)

# Now you can plot with ggraph as well (ggraph supports igraph objects)
set.seed(42)
g.sp.sp <- ggraph(g, layout = "fr") +
  ggraph::geom_edge_bend(
    aes(width = logweight),
    alpha = 1,
    color = "grey40",
    show.legend = FALSE,
    arrow = grid::arrow(type = "closed", length = unit(3, "mm")),
    end_cap = ggraph::circle(7, "mm")
  ) +
  ggraph::geom_node_point(size = 7, color = "grey30") +
  ggraph::geom_node_text(aes(label = name), repel = TRUE, size = 3.5) +
  theme_void() +
  labs(title = "super cluster ↔ super cluster (edges > 85th percentile)") +
  scale_edge_width(range = c(0.05, 2))

print(g.sp.sp)
ggsave(plot = g.sp.sp,
       filename = file.path(banc.fig5.path, sprintf("%s_neck_super_cluster_network_plot.pdf",inf.metric)),
       width = 8, 
       height = 8, 
       dpi = 300, 
       bg = "transparent")

####################
## TANGLEGRAM     ##
####################

# Compare sensory input and effector output clustering patterns
corr.nn.cluster.in.sens.key.plot <- banc_plot_key_features(
  influence.meta = influence.sens.df %>%
    dplyr::filter(!is.na(seed))  %>%
    dplyr::left_join(umap.dn.df %>%
                       dplyr::distinct(cell_type, .keep_all = TRUE) %>%
                       dplyr::distinct(cell_type, target_cluster = cluster),
                     by=c("cell_type")) %>%
    dplyr::left_join(banc.meta %>%
                       dplyr::distinct(seed_02, .keep_all = TRUE) %>%
                       dplyr::select(seed = seed_02, seed_class = body_part_sensory),
                     by = "seed") %>%
    dplyr::mutate(target = target_cluster) %>%
    dplyr::filter(!is.na(target),
                  !is.na(seed),
                  seed!="0",
                  target!="0"),
  influence.level = c("seed_02"),
  seed.map = sensory.seed.map,
  inf.metric = "influence_norm_log",
  save.path = banc.fig3.supp.path,
  target.map = NULL,
  recalculate = TRUE,
  row.annotation = NULL,
  col.annotation = NULL,
  show.annotation = FALSE,
  #col.thresh = 0.1,
  super.class = NULL,
  width = 14,
  height = 14,
  plot.name = sprintf("correlation_neck_clusters_from_all_sensors_%s.pdf","influence_norm_log"),
  rev = FALSE, 
  autocorrelation = TRUE,
  method = "euclidean",
  symmetric = TRUE,
  diagonal = FALSE
)

# Generate correlation matrix for neck cluster to effector patterns
corr.nn.dn.cluster.out.efferent.key.plot <- banc_plot_key_features(
  influence.meta = influence.dn.df %>%
    dplyr::mutate(target = cell_sub_class) %>%
    dplyr::filter(!is.na(target)) %>%
    dplyr::left_join(umap.dn.df %>%
                       # dplyr::left_join(banc.an.dn.meta %>%
                       #                    dplyr::select(root_id, seed_12),
                       #                  by = c("id"="root_id")) %>%
                       dplyr::distinct(seed_12, .keep_all = TRUE) %>%
                       dplyr::distinct(seed_12, umap_cluster = cluster),
                     by=c("seed"="seed_12")) %>%
    dplyr::mutate(seed = umap_cluster) %>%
    dplyr::filter(!is.na(target),
                  !is.na(seed),
                  seed!="0",
                  target!="0"),
  ###
  inf.metric = "influence_norm_log",
  target.map = efferent.target.map,
  width = 14,
  height = 14,
  recalculate = TRUE,
  row.annotation = NULL,
  show.annotation = FALSE,
  influence.level = "seed_12",
  save.path = banc.fig3.supp.path,
  seed.map  = FALSE,
  chosen.seeds = NULL,
  chosen.targets = efferent.target.map, 
  #row.thresh = 0.1,
  row.cols = NULL,
  super.class = NULL,
  plot.name = sprintf("correlation_neck_clusters_to_effector_cell_sub_class_%s.pdf","influence_norm_log"),
  rev = TRUE,
  autocorrelation = TRUE,
  method = "euclidean",
  symmetric = TRUE,
  diagonal = FALSE
)

# Convert hclust to dendrogram and then to phylo ----
super.clusters <- banc.meta %>%
  dplyr::distinct(cluster, super_cluster) %>%
  dplyr:::filter(!is.na(cluster),
                 !is.na(super_cluster)) %>%
  dplyr::arrange(super_cluster, cluster) %>%
  dplyr::distinct(cluster, .keep_all = TRUE)

# Organise dendrogram
dend1 <- as.dendrogram(corr.nn.cluster.in.sens.key.plot$row.dend)
dend2 <- as.dendrogram(corr.nn.dn.cluster.out.efferent.key.plot$row.dend)
labels1 <- labels(dend1)
labels2 <- labels(dend2)
common_labels <- intersect(labels1, labels2)
#common_labels <- common_labels[grepl("DN",common_labels)]
dend1_pruned <- dendextend::prune(dend1, setdiff(labels1, common_labels))
dend2_pruned <- dendextend::prune(dend2, setdiff(labels2, common_labels))

# Attempt to minimize line crossings
dends_aligned <- dendextend::untangle(dend1_pruned, dend2_pruned, method = "step1side")

# The result is a list of 2 dendrograms, already aligned:
dend1_aligned <- dends_aligned[[1]]
dend2_aligned <- dends_aligned[[2]]

# 1. Build a cluster->color mapping for all clusters present in the dendrogram
leaf_clusters <- super.clusters$cluster
leaf_supercluster <- super.clusters$super_cluster
cluster2super <- setNames(leaf_supercluster, leaf_clusters)
super2col <- paper.cols  # already named by your super_cluster

# 2. For each dend, generate a color vector for the leaves in the correct order
get_leaf_colors <- function(dend, cluster2super, super2col) {
  leaves <- labels(dend)
  supers <- cluster2super[leaves]
  cols <- super2col[as.character(supers)]
  cols[is.na(cols)] <- "#000000"
  return(cols)
}

# 3. Assign leaf colors (labels and/or points) to each dendrogram
dend1_colored <- dendextend::set(dend1_aligned, "labels_col", get_leaf_colors(dend1_aligned, cluster2super, super2col))
dend2_colored <- dendextend::set(dend2_aligned, "labels_col", get_leaf_colors(dend2_aligned, cluster2super, super2col))

# (Optional: also color points at leaves. If you want colored points/circles:)
dend1_colored <- dendextend::set(dend1_colored, "leaves_pch", 19)
dend2_colored <- dendextend::set(dend2_colored, "leaves_pch", 19)
dend1_colored <- dendextend::set(dend1_colored, "leaves_col", get_leaf_colors(dend1_colored, cluster2super, super2col))
dend2_colored <- dendextend::set(dend2_colored, "leaves_col", get_leaf_colors(dend2_colored, cluster2super, super2col))

# After you have your aligned/colored dendrograms:
dend1_bold <- dendextend::set(dend1_colored, "labels_font", 2)
dend2_bold <- dendextend::set(dend2_colored, "labels_font", 2)

# 4. Tanglegram as before but using the colored dendrograms
dl <- dendextend::dendlist(dend1_colored, dend2_colored)

pdf(file.path(banc.fig3.supp.path, "neck_cluster_tangelgram.pdf"), 
    width = 10, 
    height = 8) 
d.plot <- dendextend::tanglegram(
  dl,
  sort = FALSE,
  fast = FALSE,
  main_left = "sensory input clustering",
  main_right = "effector output clustering",
  lab.cex = 0.8,
  margin_inner = 3,
  margin_outer = 6,
  lty = 1,
  common_subtrees_color_lines = FALSE, 
  highlight_distinct_edges  = FALSE, 
  highlight_branches_lwd = FALSE,
  lwd = 4
)
dev.off()

####################
## UMAP OVERLAYS  ##
####################

# Generate UMAP visualisations with influence score overlays for different body parts and modalities
cluster_centroids <- umap.dn.df %>%
  mutate(cluster = gsub("AN_|DN_|EFF_","",cluster)) %>%
  group_by(cluster) %>%
  summarise(UMAP1 = mean(UMAP1),
            UMAP2 = mean(UMAP2))

# Calculate concave hulls for each cluster
hulls <- umap.dn.df %>%
  dplyr::filter(cluster!="0",
                !is.na(UMAP1),
                !is.na(UMAP2)) %>%
  group_by(cluster) %>%
  do({
    cluster_id <- unique(.$cluster)
    hull_data <- concaveman::concaveman(as.matrix(.[, c("UMAP1", "UMAP2")]),
                                        concavity = 2, length_threshold = 0.5)
    as.data.frame(hull_data) %>%
      mutate(cluster = cluster_id)
  }) %>%
  ungroup()

# Prepare influence data for UMAP overlay visualisation
influence.df <- influence.dn.df %>%
  dplyr::ungroup() %>%
  dplyr::mutate(target = cell_sub_class) %>%
  calculate_influence_norms() %>%
  dplyr::select(-id)

# metrics
inf.metrics <- c(
  "influence_norm_log",
  "influence_norm_log_minmax")
body.parts.modalities <- na.omit(unique(banc.eff.meta$cell_sub_class))
for(inf.metric in inf.metrics){
  message("working on: ", inf.metric)
  
  # Generate UMAP plots showing influence on specific body part effectors
  for(bp in body.parts.modalities){
    # Map influence scores to UMAP coordinates
    if(!inf.metric%in%colnames(influence.dn.df)){
      next
    }
    message("plotting:", bp)
    umap_dn_df.bp <- umap.dn.df %>%
      # dplyr::left_join(banc.an.dn.meta %>%
      #                    dplyr::select(root_id, seed_12),
      #                  by = c("id"="root_id")) %>%
      dplyr::left_join(influence.df %>%
                         dplyr::filter(target==bp) %>%
                         dplyr::distinct(seed,.keep_all = TRUE),
                       by = c("seed_12"="seed")) %>%
      dplyr::distinct(id, .keep_all = TRUE) 
    umap_dn_df.bp$influence_score <- umap_dn_df.bp[[inf.metric]]
    umap_dn_df.bp <- umap_dn_df.bp %>%
      dplyr::arrange(influence_score)
    
    # Apply colour scaling thresholds for visualisation
    thresh.high <- quantile(influence.df[[inf.metric]],0.99, na.rm=TRUE)
    thresh.low <- quantile(influence.df[[inf.metric]],0.5, na.rm=TRUE)
    scaled_heatmap_breaks <- seq(thresh.low, thresh.high, length.out = n_breaks)
    scaled_heatmap_palette <- colorRampPalette(c("#1f4e79", "#4a90a4", "#7ba7bc", "#a67c8a", "#c4967d", "#b22222"))(n_breaks - 1)
    umap_dn_df.bp$influence_score[umap_dn_df.bp$influence_score>thresh.high] <- thresh.high
    umap_dn_df.bp$influence_score[umap_dn_df.bp$influence_score<thresh.low] <- thresh.low
    
    # Generate UMAP plot with influence-based colour scaling
    p_hulls.bp <-  ggplot(data = umap_dn_df.bp, 
                          aes(x = UMAP1, y = UMAP2)) +
      geom_polygon(data = hulls, 
                   aes(x = V1, y = V2, group = factor(cluster)), 
                   alpha = 0.2, 
                   fill = "grey90", 
                   color = "black", 
                   linetype = "dotted") +
      geom_point(data = subset(umap_dn_df.bp, is.na(influence_score)), alpha = 1, size = 2, col = "grey30") +
      geom_point(data = subset(umap_dn_df.bp, !is.na(influence_score)), aes(color=influence_score), alpha = 1, size = 2) +
      scale_color_gradientn(colours = scaled_heatmap_palette,
                            values = scales::rescale(scaled_heatmap_breaks),
                            limits = c(thresh.low, thresh.high),
                            na.value = "grey30") +
      theme_void() +
      labs(title = "",
           x = "UMAP1",
           y = "UMAP2") +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 6), 
        legend.title = element_text(size = 8), 
        legend.key.size = unit(0.5, "cm")
      ) +
      # geom_text(data = cluster_centroids,
      #           aes(label = cluster),
      #           colour = "black",
      #           size = 4,
      #           fontface = "bold") +
      labs(color = paste0(bp,": ",inf.metric)) +
      ggplot2::coord_fixed()
    
    # Export body part-specific influence UMAP
    fp <- file.path(banc.fig3.extra.path, "banc_dn_connectivity_umaps",inf.metric,"efferent_cell_function")
    dir.create(fp, showWarnings = FALSE, recursive = TRUE)
    ggsave(plot = p_hulls.bp,
           filename = file.path(fp, sprintf("dn_influence_umap_by_%s_%s.pdf",bp,inf.metric)),
           width = 8, height = 8, dpi = 300, bg = "transparent")
  }
  
  # Generate UMAP plots showing sensory modality influence patterns
  mods <- unique(influence.sens.df$seed)
  for(mod in mods){
    message("plotting:", mod)
    
    # Map sensory influence scores to UMAP coordinates
    if(!inf.metric%in%colnames(influence.sens.df)){
      next
    }
    umap_dn_df.mod <- umap.dn.df %>%
      dplyr::left_join(influence.sens.df %>%
                         dplyr::filter(seed==mod) %>%
                         dplyr::distinct(id, seed, .keep_all = TRUE),
                       by = c("id"))
    umap_dn_df.mod$influence_score <- umap_dn_df.mod[[inf.metric]]
    umap_dn_df.mod <- umap_dn_df.mod %>%
      dplyr::arrange(influence_score)
    scores <- na.omit(umap_dn_df.mod$influence_score)
    scores[is.infinite(scores)] <- min(scores[!is.infinite(scores)])
    thresh.high <- quantile(influence.sens.df[[inf.metric]],0.99, na.rm=TRUE)
    thresh.low <- quantile(influence.sens.df[[inf.metric]],0.5, na.rm=TRUE)
    scaled_heatmap_breaks <- seq(thresh.low, thresh.high, length.out = n_breaks)
    scaled_heatmap_palette <- colorRampPalette(c("#1f4e79", "#4a90a4", "#7ba7bc", "#a67c8a", "#c4967d", "#b22222"))(n_breaks - 1)
    umap_dn_df.mod$influence_score[umap_dn_df.mod$influence_score>thresh.high] <- thresh.high
    umap_dn_df.mod$influence_score[umap_dn_df.mod$influence_score<thresh.low] <- thresh.low
    
    # Generate sensory modality influence UMAP
    p_hulls.mod <-  ggplot(data = umap_dn_df.mod, 
                           aes(x = UMAP1, y = UMAP2)) +
      geom_polygon(data = hulls, 
                   aes(x = V1, y = V2, group = factor(cluster)), 
                   alpha = 0.2, 
                   fill = "grey90", 
                   color = "black", 
                   linetype = "dotted") +
      geom_point(data = subset(umap_dn_df.mod, is.na(influence_score)), alpha = 0.8, size = 1, col = "grey30") +
      geom_point(data = subset(umap_dn_df.mod, !is.na(influence_score)), aes(color=influence_score), alpha = 0.8, size = 2) +
      scale_color_gradientn(colours = scaled_heatmap_palette,
                            values = scales::rescale(scaled_heatmap_breaks),
                            limits = c(thresh.low, thresh.high),
                            na.value = "grey30") +
      theme_void() +
      labs(title = "",
           x = "UMAP1",
           y = "UMAP2") +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 6), 
        legend.title = element_text(size = 8),
        legend.key.size = unit(0.5, "cm") 
      ) +
      # geom_text(data = cluster_centroids,
      #           aes(label = cluster),
      #           colour = "black",
      #           size = 4,
      #           fontface = "bold") +
      labs(color = paste0(mod,": ",inf.metric)) +
      ggplot2::coord_fixed()
    
    # Export sensory modality influence UMAP
    fp <- file.path(banc.fig3.extra.path, "banc_dn_connectivity_umaps", inf.metric,"body_part_sensory_cell_function")
    dir.create(fp, showWarnings = FALSE)
    ggsave(plot = p_hulls.mod,
           filename = file.path(fp, sprintf("dn_influence_cosine_umap_by_%s_%s.pdf",mod, inf.metric)),
           width = 8, height = 8, dpi = 300, bg = "transparent")
  }
  
  # Generate UMAP plots showing visual projection neuron influence
  vpns <- unique(influence.vpn.df$seed)
  for(vpn in vpns){
    message("plotting:", vpn)
    
    # Map VPN influence scores to UMAP coordinates
    if(!inf.metric%in%colnames(influence.vpn.df)){
      next
    }
    umap_dn_df.vpn <- umap.dn.df %>%
      dplyr::left_join(influence.vpn.df %>%
                         dplyr::filter(seed==vpn) %>%
                         dplyr::distinct(id, seed, .keep_all = TRUE),
                       by = c("id"))
    umap_dn_df.vpn$influence_score <- umap_dn_df.vpn[[inf.metric]]
    umap_dn_df.vpn <- umap_dn_df.vpn %>%
      dplyr::arrange(influence_score)
    scores <- na.omit(umap_dn_df.vpn$influence_score)
    scores[is.infinite(scores)] <- min(scores[!is.infinite(scores)])
    thresh.high <- quantile(influence.vpn.df[[inf.metric]],0.99, na.rm=TRUE)
    thresh.low <- quantile(influence.vpn.df[[inf.metric]],0.5, na.rm=TRUE)
    scaled_heatmap_breaks <- seq(thresh.low, thresh.high, length.out = n_breaks)
    scaled_heatmap_palette <- colorRampPalette(c("#1f4e79", "#4a90a4", "#7ba7bc", "#a67c8a", "#c4967d", "#b22222"))(n_breaks - 1)
    umap_dn_df.vpn$influence_score[umap_dn_df.vpn$influence_score>thresh.high] <- thresh.high
    umap_dn_df.vpn$influence_score[umap_dn_df.vpn$influence_score<thresh.low] <- thresh.low
    
    # Generate VPN influence UMAP with appropriate scaling
    p_hulls.vpn <-  ggplot(data = umap_dn_df.vpn, 
                           aes(x = UMAP1, y = UMAP2)) +
      geom_polygon(data = hulls, 
                   aes(x = V1, y = V2, group = factor(cluster)), 
                   alpha = 0.2, 
                   fill = "grey90", 
                   color = "black", 
                   linetype = "dotted") +
      geom_point(data = subset(umap_dn_df.vpn, is.na(influence_score)), alpha = 0.8, size = 2, col = "grey30") +
      geom_point(data = subset(umap_dn_df.vpn, !is.na(influence_score)), aes(color=influence_score), alpha = 0.8, size = 2) +
      scale_color_gradientn(colours = scaled_heatmap_palette,
                            values = scales::rescale(scaled_heatmap_breaks),
                            limits = c(thresh.low, thresh.high),
                            na.value = "grey30") +
      theme_void() +
      labs(title = "",
           x = "UMAP1",
           y = "UMAP2") +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 8), 
        legend.key.size = unit(0.5, "cm")
      ) +
      # geom_text(data = cluster_centroids,
      #           aes(label = cluster),
      #           colour = "black",
      #           size = 4,
      #           fontface = "bold") +
      labs(color = paste0(vpn,": ", inf.metric)) +
      ggplot2::coord_fixed()
    
    # Export VPN influence UMAP
    fp <- file.path(banc.fig3.extra.path, "banc_dn_connectivity_umaps", inf.metric,"visual_projection")
    dir.create(fp, showWarnings = FALSE)
    ggsave(plot = p_hulls.vpn,
           filename = file.path(fp, sprintf("dn_influence_cosine_umap_by_%s_%s.pdf",vpn,inf.metric)),
           width = 8, height = 8, dpi = 300, bg = "transparent")
  }
  
  # Generate UMAP plots showing influence from MB/CX cell functions
  cell.functions <- na.omit(unique(influence.functions.df$seed))
  for(cf in cell.functions){
    message("plotting:", cf)
    
    # Map cell function influence scores to UMAP coordinates
    if(!inf.metric%in%colnames(influence.functions.df)){
      next
    }
    umap_dn_df.cf <- umap.dn.df %>%
      dplyr::left_join(influence.functions.df %>%
                         dplyr::filter(seed==cf) %>%
                         dplyr::distinct(id, seed, .keep_all = TRUE),
                       by = c("id"))
    umap_dn_df.cf$influence_score <- umap_dn_df.cf[[inf.metric]]
    umap_dn_df.cf <- umap_dn_df.cf %>%
      dplyr::arrange(influence_score)
    scores <- na.omit(umap_dn_df.cf$influence_score)
    scores[is.infinite(scores)] <- min(scores[!is.infinite(scores)])
    thresh.high <- quantile(influence.functions.df[[inf.metric]],0.99, na.rm=TRUE)
    thresh.low <- quantile(influence.functions.df[[inf.metric]],0.5, na.rm=TRUE)
    scaled_heatmap_breaks <- seq(thresh.low, thresh.high, length.out = n_breaks)
    scaled_heatmap_palette <- colorRampPalette(c("#1f4e79", "#4a90a4", "#7ba7bc", "#a67c8a", "#c4967d", "#b22222"))(n_breaks - 1)
    umap_dn_df.cf$influence_score[umap_dn_df.cf$influence_score>thresh.high] <- thresh.high
    umap_dn_df.cf$influence_score[umap_dn_df.cf$influence_score<thresh.low] <- thresh.low
    
    # Generate cell function influence UMAP
    p_hulls.cf <-  ggplot(data = umap_dn_df.cf, 
                          aes(x = UMAP1, y = UMAP2)) +
      geom_polygon(data = hulls, 
                   aes(x = V1, y = V2, group = factor(cluster)), 
                   alpha = 0.2, 
                   fill = "grey90", 
                   color = "black", 
                   linetype = "dotted") +
      geom_point(data = subset(umap_dn_df.cf, is.na(influence_score)), alpha = 0.8, size = 2, col = "grey30") +
      geom_point(data = subset(umap_dn_df.cf, !is.na(influence_score)), aes(color=influence_score), alpha = 0.8, size = 2) +
      scale_color_gradientn(colours = scaled_heatmap_palette,
                            values = scales::rescale(scaled_heatmap_breaks),
                            limits = c(thresh.low, thresh.high),
                            na.value = "grey30") +
      theme_void() +
      labs(title = "",
           x = "UMAP1",
           y = "UMAP2") +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 8), 
        legend.key.size = unit(0.5, "cm")
      ) +
      # geom_text(data = cluster_centroids,
      #           aes(label = cluster),
      #           colour = "black",
      #           size = 4,
      #           fontface = "bold") +
      labs(color = paste0(cf,": ",inf.metric)) +
      ggplot2::coord_fixed()
    
    # Export cell function influence UMAP
    fp <- file.path(banc.fig3.extra.path, "banc_dn_connectivity_umaps", inf.metric,"cell_functions")
    dir.create(fp, showWarnings = FALSE)
    ggsave(plot = p_hulls.cf,
           filename = file.path(fp, sprintf("dn_influence_cosine_umap_by_%s_%s.pdf",cf,inf.metric)),
           width = 8, height = 8, dpi = 300, bg = "transparent")
    
  }
}
