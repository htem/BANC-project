##############################
## FIGURE 6: ALL-TO-ALL FLOW ##
##############################
# Analyse network-wide influence patterns between major brain regions
# and functional clusters. Shows how information flows through the CNS.
# Output: figures/figure_6/links/cns_network_*.pdf

###################
## LOAD PACKAGES ##
###################

# Load required packages and connectivity data
source("R/startup/banc-startup.R")
source("R/startup/franken-meta.R")
source("R/startup/banc-meta.R")
source("R/startup/banc-functions.R")
source("R/startup/banc-edgelist.R")

##################################
## CNS NETWORK OUTPUT ANALYSIS  ##
##################################
# Analyse connectivity patterns between CNS networks and clusters

# Extract neuron connectivity data between CNS networks
cns.cluster.outputs <- banc.edgelist.simple %>%
  dplyr::left_join(banc.meta.pre %>%
                     dplyr::select(pre = pre_id,
                                   pre_cns_network),
                   by ="pre") %>%
  dplyr::left_join(banc.meta.post %>%
                     dplyr::select(post = post_id,
                                   post_cns_network),
                   by ="post") %>%
  dplyr::mutate(post_cns_network = dplyr::case_when(
    is.na(post_cns_network) ~ post_cluster,
    TRUE ~ post_cns_network
  )) %>%
  dplyr::filter(!is.na(pre_cns_network),
                !is.na(post_cns_network)) %>%
  dplyr::group_by(pre) %>%
  dplyr::filter(count > 5) %>%
  dplyr::mutate(home = pre_cns_network==post_cns_network,
                away = pre_cns_network!=post_cns_network,
                total_output = sum(count,na.rm = TRUE),
                home_norm = sum(count[home],na.rm=TRUE)/total_output,
                away_norm = sum(count[away],na.rm=TRUE)/total_output) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(pre,
                  pre_cns_network,
                  total_output,
                  home_norm,
                  away_norm) %>%
  dplyr::arrange(pre)

# Inputs neurons
cns.cluster.inputs <- banc.edgelist.simple %>%
  dplyr::left_join(banc.meta.post %>%
                     dplyr::select(post = post_id,
                                   post_cns_network),
                   by ="post") %>%
  dplyr::left_join(banc.meta.pre %>%
                     dplyr::select(pre = pre_id,
                                   pre_cns_network),
                   by ="pre") %>%
  dplyr::mutate(pre_cns_network = dplyr::case_when(
    is.na(pre_cns_network) ~ pre_cluster,
    TRUE ~ pre_cns_network
  )) %>%
  dplyr::filter(!is.na(post_cns_network),
                !is.na(pre_cns_network)) %>%
  dplyr::group_by(post) %>%
  dplyr::filter(count > 5) %>%
  dplyr::mutate(home = post_cns_network==pre_cns_network,
                away = post_cns_network!=pre_cns_network,
                total_output = sum(count,na.rm = TRUE),
                home_norm = sum(count[home],na.rm=TRUE)/total_output,
                away_norm = sum(count[away],na.rm=TRUE)/total_output) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(post,
                  post_cns_network,
                  total_output,
                  home_norm,
                  away_norm) %>%
  dplyr::arrange(post)

# Plot
g.cns.outputs <- ggplot(cns.cluster.outputs, 
                        aes(x = away_norm)) +
  geom_histogram(binwidth=0.1,
                 fill="lightgrey") +
  labs(
    x = "proportion 'away'",
    y = "frequency",
    title = ""
  ) +
  theme_minimal(base_size = 14) 

# Save
print(g.cns.outputs)
ggsave(plot = g.cns.outputs,
       filename = file.path(banc.fig6.supp.path, "cns_network_output_proportions.pdf"),
       width = 12, 
       height = 6, 
       dpi = 300, 
       bg = "transparent")

# Select
cns.cluster.selected.outputs <- cns.cluster.outputs %>%
  dplyr::filter(away_norm>0.5)

#######################
### PLOT KET POINTS ###
#######################

# Wrangle
cns.cluster.selected.outputs.ct <- cns.cluster.outputs %>%
  dplyr::left_join(banc.meta.pre %>%
                     dplyr::distinct(pre_id, .keep_all = TRUE) %>%
                     dplyr::select(-pre_cns_network), 
                   by = c("pre"="pre_id")) %>%
  dplyr::filter(grepl("mushroom|central",pre_cell_class)|grepl("mushroom|central",pre_cell_sub_class))
cns.cluster.selected.inputs.ct <- cns.cluster.inputs %>%
  dplyr::left_join(banc.meta.post %>%
                     dplyr::distinct(post_id, .keep_all = TRUE) %>%
                     dplyr::select(-post_cns_network), 
                   by = c("post"="post_id")) %>%
  dplyr::filter(grepl("mushroom|central",post_cell_class)|grepl("mushroom|central",post_cell_sub_class))
cns.cluster.both.ct <- cns.cluster.selected.outputs.ct %>%
  dplyr::select(id = pre, cell_type  = pre_cell_type, cell_sub_class = pre_cell_sub_class, cell_class = pre_cell_class, away_norm) %>%
  rbind(cns.cluster.selected.inputs.ct %>%
                 dplyr::select(id = post, cell_type  = post_cell_type, cell_sub_class = post_cell_sub_class, cell_class = post_cell_class, away_norm)) %>%
  dplyr::group_by(id, cell_type, cell_sub_class, cell_class) %>%
  dplyr::summarise(away_norm = max(away_norm))


# normalise
dens_df <- cns.cluster.selected.outputs.ct %>%
  group_by(pre_cell_class, pre_cell_sub_class) %>%
  do({
    vals <- .$away_norm
    vals <- vals[!is.na(vals)]
    d <- density(vals, from = 0, to = 1)
    data.frame(x = d$x, density = d$y / max(d$y, na.rm = TRUE))
  })

# Plot
g.props <- ggplot(dens_df, aes(x = x, y = density, color = pre_cell_sub_class)) +
  geom_line(size = 1.2) +
  facet_wrap(~pre_cell_class, scales = "free") +
  labs(
    x = "away proportion",
    y = "normalized density (peak = 1)",
    color = "cell sub class"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(size = 14),
    legend.position = "bottom"
  )

# Plot
print(g.props)
ggsave(plot = g.props,
       filename = file.path(banc.fig6.extra.path, "mb_cx_cluster_away_proportions.pdf"),
       width = 18, 
       height = 12, 
       dpi = 300, 
       bg = "transparent")

# normalise
dens_df2 <- cns.cluster.selected.outputs.ct %>%
  dplyr::group_by(pre_cell_class, pre_cell_sub_class) %>%
  dplyr::filter(sum(!is.na(away_norm)) >= 2) %>%
  dplyr::do({
    vals <- .$away_norm
    vals <- vals[!is.na(vals)]
    d <- density(vals, from = 0, to = 1)
    data.frame(x = d$x, density = d$y / max(d$y, na.rm = TRUE))
  }) %>%
  dplyr::ungroup()

# Plot
g.props.simple <- ggplot(dens_df2, aes(x = x, y = density, color = pre_cell_sub_class)) +
  geom_line(size = 1.2) +
  facet_wrap(~pre_cell_sub_class, scales = "free_y") +
  labs(
    x = "away proportion",
    y = "normalized density (peak = 1)",
    color = "cell sub class"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 14)
  )

# Plot
print(g.props.simple)
ggsave(plot = g.props.simple,
       filename = file.path(banc.fig6.extra.path, "mb_cx_many_facets_cluster_away_proportions.pdf"),
       width = 18, 
       height = 12, 
       dpi = 300, 
       bg = "transparent")

# normalise
dens_df3 <- cns.cluster.selected.inputs.ct %>%
  dplyr::group_by(post_cell_class, post_cell_sub_class) %>%
  dplyr::filter(sum(!is.na(away_norm)) >= 2) %>%
  dplyr::do({
    vals <- .$away_norm
    vals <- vals[!is.na(vals)]
    d <- density(vals, from = 0, to = 1)
    data.frame(x = d$x, density = d$y / max(d$y, na.rm = TRUE))
  }) %>%
  dplyr::ungroup()

# Plot
g.props.simple.inputs <- ggplot(dens_df3, aes(x = x, y = density, color = post_cell_sub_class)) +
  geom_line(size = 1.2) +
  facet_wrap(~post_cell_sub_class, scales = "free_y") +
  labs(
    x = "away proportion",
    y = "normalized density (peak = 1)",
    color = "cell sub class"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 14)
  )

# Plot
print(g.props.simple.inputs)
ggsave(plot = g.props.simple.inputs,
       filename = file.path(banc.fig6.extra.path, "mb_cx_many_facets_cluster_input_away_proportions.pdf"),
       width = 18, 
       height = 12, 
       dpi = 300, 
       bg = "transparent")

# normalise
dens_df4 <- cns.cluster.both.ct %>%
  dplyr::group_by(cell_class, cell_sub_class) %>%
  dplyr::filter(sum(!is.na(away_norm)) >= 2) %>%
  dplyr::do({
    vals <- .$away_norm
    vals <- vals[!is.na(vals)]
    d <- density(vals, from = 0, to = 1)
    data.frame(x = d$x, density = d$y / max(d$y, na.rm = TRUE))
  }) %>%
  dplyr::ungroup()

# Plot
g.props.simple.both <- ggplot(dens_df4, aes(x = x, y = density, color = cell_sub_class)) +
  geom_line(size = 1.2) +
  facet_wrap(~cell_sub_class, scales = "free_y") +
  labs(
    x = "away proportion",
    y = "normalized density (peak = 1)",
    color = "cell sub class"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 14)
  )

# Plot
print(g.props.simple.both)
ggsave(plot = g.props.simple.both,
       filename = file.path(banc.fig6.extra.path, "mb_cx_many_facets_cluster_both_away_proportions.pdf"),
       width = 18, 
       height = 12, 
       dpi = 300, 
       bg = "transparent")

############################
### GET INFLUENCE SCORES ###
############################

# Get influence meta
con <- DBI::dbConnect(RSQLite::SQLite(),
                      file.path(banc.dropbox.influence.save.path,"influence_banc_626.sqlite"))
inf.banc.meta <- dplyr::tbl(con, "meta") %>%
  dplyr::collect()
inf.banc.meta$id <- inf.banc.meta$root_id
dbDisconnect(con)

# Use updated banc.meta
inf.banc.meta <- inf.banc.meta %>%
  dplyr::distinct(id, supervoxel_id) %>%
  dplyr::left_join(banc.meta %>%
                     dplyr::select(-id) %>%
                     dplyr::distinct(supervoxel_id, 
                                     .keep_all = TRUE),
                   by ="supervoxel_id") %>%
  dplyr::mutate(cns_cluster_network = dplyr::case_when(
    !is.na(cluster) ~ cluster,
    TRUE ~ cns_network
  )) 

# Cluster CNS network mapping
cluster_network_map <- inf.banc.meta %>%
  dplyr::filter(!is.na(cns_cluster_network), !is.na(cns_network)) %>%
  dplyr::count(cns_cluster_network, cns_network, sort = TRUE) %>%
  dplyr::group_by(cns_cluster_network) %>%
  dplyr::slice_max(n, n = 1, with_ties = FALSE) %>% 
  dplyr::ungroup() %>%
  dplyr::select(cns_cluster_network, cns_network)

# Get selected IDs
pre.ids <- unique(cns.cluster.selected.outputs$pre)
target.ids <- inf.banc.meta %>%
  dplyr::filter(!is.na(cns_network)) %>%
  dplyr::pull(id)

# SQL column types
inf.col.types <- readr::cols(
  id = readr::col_character(),
  is_seed = readr::col_logical(),
  .default = readr::col_number())

# For each, get influence
influence.seed13 <- "/Volumes/T7 Shield/influence/seed_13_unsilenced_7_23_2025/seed_13/"
progressr::with_progress({
  p <- progressr::progressor(steps = length(pre.ids))
  inf.data <- foreach(pre.id = pre.ids, 
                      #.combine = list(), 
                      .packages = c("dplyr", "readr"), 
                      .errorhandling = 'pass') %do% {
                        
                        # Read all data
                        csv <- file.path(influence.seed13,paste0(pre.id,"_influence.csv"))
                        if(!file.exists(csv)){
                          message("no results for: ", pre.id)
                          return(NULL)
                        }
                        data <- readr::read_csv(csv,
                                                show_col_types = FALSE, 
                                                progress = FALSE,
                                                col_types = inf.col.types)
                        data <- data[,-1]
                        colnames(data) <- c("id","is_seed","influence")
                        n.seeds <- sum(data$is_seed, na.rm = TRUE)
                        id.seeds <- subset(data, is_seed)$id
                        syns.seeds <- subset(inf.banc.meta, root_id %in% id.seeds | 
                                               supervoxel_id %in% subset(inf.banc.meta, root_id %in% id.seeds)$supervoxel_id)$output_connections
                        n.syns.seed <- sum(syns.seeds, na.rm = TRUE)
                        if(n.syns.seed==0){
                          n.syns.seed <- 1
                        }
                        
                        # Skip if no influence
                        if(all(data$influence=="0")){
                          message("No values for: ", csv)
                          return(NULL)
                        }
                        
                        # Apply seed label
                        data <- data %>%
                          dplyr::filter(id %in% target.ids) %>%
                          dplyr::left_join(inf.banc.meta %>%
                                             dplyr::distinct(id, .keep_all = TRUE) %>%
                                             dplyr::select(id, target = cns_network),
                                           by = "id") %>%
                          dplyr::mutate(seed = gsub(".*from_|\\.csv*|_influence.*","",basename(csv)),
                                        level = "seed_13") %>%
                          dplyr::left_join(inf.banc.meta %>%
                                             dplyr::distinct(id, .keep_all = TRUE) %>%
                                             dplyr::select(seed = id, seed_cns_network = cns_cluster_network),
                                           by = "seed") %>%
                          dplyr::filter(seed_cns_network!=target)
                        
                        # Perform scaling operations
                        data$influence_original <- as.numeric(data$influence)
                        data$influence_norm_original <- as.numeric(data$influence)/n.seeds
                        data$influence_syn_norm_original <- as.numeric(data$influence)/n.syns.seed
                        data <- calculate_influence_norms(data) %>%
                          dplyr::select(seed, target,
                                        influence_original, influence_norm_original, influence_syn_norm_original,
                                        influence,influence_norm_log,influence_log)
                        p()
                        data
                      } 
})

# combine
cns.inf.data <- do.call(rbind,inf.data)

#################
### PLOT DATA ###
#################

# Primary CNS cluster outputs
cns.cluster.main.out.to.super.eff.key.plot <- banc_plot_key_features(
  influence.meta = cns.inf.data %>%
    dplyr::filter(grepl("EFF",target)) %>%
    dplyr::left_join(inf.banc.meta %>%
                       dplyr::distinct(id, .keep_all = TRUE) %>%
                       dplyr::select(seed = id, seed_cns_network = cns_network),
                     by = "seed") %>%
    dplyr::left_join(inf.banc.meta %>%
                       dplyr::distinct(cluster, .keep_all = TRUE) %>%
                       dplyr::select(target = cluster, target_super_cluster = super_cluster),
                     by = "target") %>%
    dplyr::mutate(seed = seed_cns_network, id = target, target = target_super_cluster) %>%
    dplyr::filter(!is.na(target),
                  !is.na(seed),
                  target!="0",
                  target!="",
                  seed!="0",
                  seed!=""),
  ###
  inf.metric = "influence_log",
  target.map = NULL,
  cellheight = 12,
  cellwidth = 12,
  width = 5,
  height = 5,
  col.order = cns.network.order,
  row.order = eff.super.order,
  recalculate = TRUE,
  show.annotation = FALSE,
  influence.level = NULL,
  save.path = banc.fig6.path,
  seed.map  = NULL,
  chosen.seeds = NULL,
  chosen.targets = NULL, 
  row.cols = NULL,
  super.class = NULL,
  plot.name = sprintf("major_cns_network_output_to_efferent_super_clusters_%s.pdf","influence_log"),
  rev = FALSE,
  symmetric = FALSE,
  diagonal = TRUE
)

# Order
#cns.network.order <- names(sort(colSums(cns.cluster.main.out.to.super.eff.key.plot$influence.matrix)))

# Primary CNS cluster outputs
cns.cluster.main.out.key.plot <- banc_plot_key_features(
  influence.meta = cns.inf.data %>%
    dplyr::filter(!is.na(target),
                  !is.na(seed),
                  target!="0",
                  target!="",
                  seed!="0",
                  seed!="") %>%
    dplyr::left_join(inf.banc.meta %>%
                       dplyr::distinct(id, .keep_all = TRUE) %>%
                       dplyr::select(seed = id, seed_cns_network = cns_network),
                     by = "seed") %>%
    dplyr::left_join(cluster_network_map,
                     by = c("target"="cns_cluster_network")) %>%
    dplyr::mutate(seed = seed_cns_network, target = cns_network, id = cns_network) %>%
    dplyr::filter(!grepl("^AN|^DN|^EFF",target)),
  ###
  inf.metric = "influence",
  target.map = NULL,
  cellheight = 12,
  cellwidth = 12,
  width = 6,
  height = 6,
  col.order = cns.network.order,
  row.order = cns.network.order,
  recalculate = TRUE,
  show.annotation = FALSE,
  influence.level = NULL,
  save.path = banc.fig6.path,
  seed.map  = NULL,
  chosen.seeds = NULL,
  chosen.targets = NULL, 
  row.cols = NULL,
  super.class = NULL,
  plot.name = sprintf("major_cns_network_output_%s.pdf","influence"),
  rev = FALSE,
  symmetric = TRUE,
  diagonal = FALSE
)

# Primary CNS cluster outputs
cns.cluster.main.out.key.plot <- banc_plot_key_features(
  influence.meta = cns.inf.data %>%
    dplyr::filter(!is.na(target),
                  !is.na(seed),
                  target!="0",
                  target!="",
                  seed!="0",
                  seed!="") %>%
    dplyr::left_join(inf.banc.meta %>%
                       dplyr::distinct(id, .keep_all = TRUE) %>%
                       dplyr::select(seed = id, seed_cns_network = cns_network),
                     by = "seed") %>%
    dplyr::left_join(cluster_network_map,
                     by = c("target"="cns_cluster_network")) %>%
    dplyr::mutate(seed = seed_cns_network, target = cns_network, id = cns_network) %>%
    dplyr::filter(!grepl("^AN|^DN|^EFF",target)),
  ###
  inf.metric = "influence_log",
  target.map = NULL,
  cellheight = 12,
  cellwidth = 12,
  width = 5,
  height = 5,
  col.order = cns.network.order,
  row.order = cns.network.order,
  recalculate = TRUE,
  show.annotation = FALSE,
  influence.level = NULL,
  save.path = banc.fig6.path,
  seed.map  = NULL,
  chosen.seeds = NULL,
  chosen.targets = NULL, 
  row.cols = NULL,
  super.class = NULL,
  plot.name = sprintf("major_cns_network_output_%s.pdf","influence_log"),
  rev = FALSE,
  symmetric = TRUE,
  diagonal = FALSE
)

# Primary CNS cluster outputs
cns.cluster.main.out.to.nn.key.plot <- banc_plot_key_features(
  influence.meta = cns.inf.data %>%
    dplyr::filter(grepl("^AN|^DN",target)) %>%
    dplyr::left_join(inf.banc.meta %>%
                       dplyr::distinct(id, .keep_all = TRUE) %>%
                       dplyr::select(seed = id, seed_cns_network = cns_network),
                     by = "seed") %>%
    dplyr::mutate(seed = seed_cns_network, target, id = target) %>%
    dplyr::filter(!is.na(target),
                  !is.na(seed),
                  target!="0",
                  target!="",
                  seed!="0",
                  seed!=""),
  ###
  inf.metric = "influence_log",
  target.map = NULL,
  cellheight = 12,
  cellwidth = 12,
  width = 5,
  height = 12,
  col.order = cns.network.order,
  recalculate = TRUE,
  show.annotation = FALSE,
  influence.level = NULL,
  save.path = banc.fig6.path,
  seed.map  = NULL,
  chosen.seeds = NULL,
  chosen.targets = NULL, 
  row.cols = NULL,
  super.class = NULL,
  plot.name = sprintf("major_cns_network_output_to_neck_clusters_%s.pdf","influence_log"),
  rev = FALSE,
  symmetric = FALSE,
  diagonal = TRUE
)

# Primary CNS cluster outputs
cns.cluster.main.out.to.super.nn.key.plot <- banc_plot_key_features(
  influence.meta = cns.inf.data %>%
    dplyr::filter(grepl("^AN|^DN",target)) %>%
    dplyr::left_join(inf.banc.meta %>%
                       dplyr::distinct(id, .keep_all = TRUE) %>%
                       dplyr::select(seed = id, seed_cns_network = cns_network),
                     by = "seed") %>%
    dplyr::left_join(inf.banc.meta %>%
                       dplyr::distinct(cluster, .keep_all = TRUE) %>%
                       dplyr::select(target = cluster, target_super_cluster = super_cluster),
                     by = "target") %>%
    dplyr::mutate(seed = seed_cns_network, 
                  id = target, 
                  target = target_super_cluster) %>%
    dplyr::filter(!is.na(target),
                  !is.na(seed),
                  target!="0",
                  target!="",
                  seed!="0",
                  seed!=""),
  ###
  inf.metric = "influence_log",
  target.map = NULL,
  cellheight = 12,
  cellwidth = 12,
  width = 5,
  height = 5,
  col.order = cns.network.order,
  row.order = super.clust.order,
  recalculate = TRUE,
  show.annotation = FALSE,
  influence.level = NULL,
  save.path = banc.fig6.path,
  seed.map  = NULL,
  chosen.seeds = NULL,
  chosen.targets = NULL, 
  row.cols = NULL,
  super.class = NULL,
  plot.name = sprintf("major_cns_network_output_to_neck_super_clusters_%s.pdf","influence_log"),
  rev = FALSE,
  symmetric = FALSE,
  diagonal = TRUE
)

# ##########################
# ### ALL OF CNS NETWORK ###
# ##########################
# 
# # CNS cluster to neck super cluster
# cns.cluster.to.eff.sub.class.key.plot <- banc_plot_key_features(
#   influence.meta = influence.cluster.df %>%
#     dplyr::filter(super_class %in% c("motor", "visceral_circulatory"),
#                   !grepl("AN|DN|EFF",seed)) %>%
#     dplyr::left_join(banc.eff.meta %>%
#                        dplyr::distinct(id, .keep_all = TRUE) %>%
#                        dplyr::select(id, cell_sub_class),
#                      by="id") %>%
#     dplyr::mutate(target = cell_sub_class) %>%
#     dplyr::filter(!is.na(seed), 
#                   !is.na(target)),
#   ###
#   influence.level = "seed_14",
#   inf.metric = inf.metric,
#   target.map = efferent.target.map,
#   cellwidth = 12,
#   cellheight = 12,
#   width = 14, 
#   height = 6,
#   recalculate = TRUE,
#   row.annotation = NULL,
#   show.annotation = FALSE,
#   save.path = banc.fig6.supp.path,
#   seed.map  = FALSE,
#   chosen.seeds = NULL,
#   chosen.targets = efferent.target.map, 
#   row.cols = NULL,
#   super.class = NULL,
#   plot.name = sprintf("cns_network_to_efferent_sub_class_%s.pdf",inf.metric),
#   rev = TRUE,
#   row.dend = NULL,
#   col.dend = NULL,
#   method = "euclidean",
#   symmetric = FALSE,
#   diagonal = TRUE
# )
# 
# # CNS cluster to CNS cluster
# cns.cluster.cns.cluster.key.plot <- banc_plot_key_features(
#   influence.meta = influence.cluster.df %>%
#     dplyr::mutate(target = cns_network) %>%
#     dplyr::filter(level == "seed_14",
#                   !is.na(seed), 
#                   !is.na(target),
#                   seed!="0",
#                   target!="0"),
#   ###
#   influence.level = "seed_14",
#   inf.metric = inf.metric,
#   target.map = NULL,
#   cellwidth = 12,
#   cellheight = 12,
#   width = 6, 
#   height = 6,
#   recalculate = TRUE,
#   row.annotation = NULL,
#   show.annotation = FALSE,
#   save.path = banc.fig6.path,
#   seed.map  = FALSE,
#   chosen.seeds = NULL,
#   chosen.targets = NULL, 
#   row.cols = NULL,
#   super.class = NULL,
#   plot.name = sprintf("cns_network_to_cns_network_%s.pdf",inf.metric),
#   rev = FALSE,
#   row.dend = NULL,
#   col.dend = NULL,
#   method = "euclidean",
#   symmetric = TRUE,
#   diagonal = FALSE
# )
# 
# # CNS cluster to neck super cluster
# cns.cluster.to.nn.super.cluster.key.plot <- banc_plot_key_features(
#   influence.meta = influence.cluster.df %>%
#     dplyr::filter(super_class %in% c("ascending", "descending"),
#                   !grepl("AN|DN|EFF",seed)) %>%
#     dplyr::mutate(target = super_cluster) %>%
#     dplyr::filter(!is.na(seed), 
#                   !is.na(target)),
#   ###
#   influence.level = "seed_11",
#   inf.metric = inf.metric,
#   target.map = NULL,
#   cellwidth = 12,
#   cellheight = 12,
#   width = 6, 
#   height = 6,
#   recalculate = TRUE,
#   row.annotation = NULL,
#   show.annotation = FALSE,
#   save.path = banc.fig6.path,
#   seed.map  = FALSE,
#   chosen.seeds = NULL,
#   chosen.targets = NULL, 
#   row.cols = NULL,
#   super.class = NULL,
#   plot.name = sprintf("cns_network_to_neck_super_cluster_%s.pdf",inf.metric),
#   rev = FALSE,
#   row.dend = NULL,
#   col.dend = NULL,
#   method = "euclidean",
#   symmetric = FALSE,
#   diagonal = TRUE
# )
# 
# # CNS cluster to neck super cluster
# cns.cluster.to.eff.super.cluster.key.plot <- banc_plot_key_features(
#   influence.meta = influence.cluster.df %>%
#     dplyr::filter(super_class %in% c("motor", "visceral_circulatory"),
#                   !grepl("AN|DN|EFF",seed)) %>%
#     dplyr::mutate(target = super_cluster) %>%
#     dplyr::filter(!is.na(seed), 
#                   !is.na(target)),
#   ###
#   influence.level = "seed_14",
#   inf.metric = inf.metric,
#   target.map = NULL,
#   cellwidth = 12,
#   cellheight = 12,
#   width = 6, 
#   height = 6,
#   recalculate = TRUE,
#   row.annotation = NULL,
#   show.annotation = FALSE,
#   save.path = banc.fig6.path,
#   seed.map  = FALSE,
#   chosen.seeds = NULL,
#   chosen.targets = NULL, 
#   row.cols = NULL,
#   super.class = NULL,
#   plot.name = sprintf("cns_network_to_efferent_super_cluster_%s.pdf",inf.metric),
#   rev = TRUE,
#   row.dend = NULL,
#   col.dend = NULL,
#   method = "euclidean",
#   symmetric = FALSE,
#   diagonal = TRUE
# )
# 
# # Now order
# cns.cluster.to.eff.super.cluster.key.plot <- banc_plot_key_features(
#   influence.meta = influence.cluster.df %>%
#     dplyr::filter(super_class %in% c("motor", "visceral_circulatory"),
#                   !grepl("AN|DN|EFF",seed)) %>%
#     dplyr::mutate(target = super_cluster) %>%
#     dplyr::filter(!is.na(seed), 
#                   !is.na(target)),
#   ###
#   influence.level = "seed_14",
#   inf.metric = inf.metric,
#   target.map = NULL,
#   cellwidth = 12,
#   cellheight = 12,
#   width = 6, 
#   height = 6,
#   recalculate = TRUE,
#   row.annotation = NULL,
#   show.annotation = FALSE,
#   save.path = banc.fig6.path,
#   seed.map  = FALSE,
#   chosen.seeds = NULL,
#   chosen.targets = NULL, 
#   row.order = rev(names(sort(rowSums(cns.cluster.to.eff.super.cluster.key.plot$influence.matrix)))),
#   col.order = rev(names(sort(colSums(cns.cluster.to.eff.super.cluster.key.plot$influence.matrix)))),
#   row.cols = NULL,
#   super.class = NULL,
#   plot.name = sprintf("cns_network_to_efferent_super_cluster_%s.pdf",inf.metric),
#   rev = TRUE,
#   row.dend = NULL,
#   col.dend = NULL,
#   method = "euclidean",
#   symmetric = FALSE,
#   diagonal = TRUE
# )
# 
# # CNS cluster to CNS cluster
# cns.cluster.cns.cluster.key.plot <- banc_plot_key_features(
#   influence.meta = influence.cluster.df %>%
#     dplyr::mutate(target = cns_network) %>%
#     dplyr::filter(!is.na(seed), 
#                   !is.na(target),
#                   seed!="0",
#                   target!="0"),
#   ###
#   quantile = 0.99,
#   influence.level = "seed_14",
#   inf.metric = "influence_quantile",
#   target.map = NULL,
#   cellwidth = 12,
#   cellheight = 12,
#   width = 6, 
#   height = 6,
#   recalculate = TRUE,
#   row.annotation = NULL,
#   show.annotation = FALSE,
#   save.path = banc.fig6.supp.path,
#   seed.map  = FALSE,
#   chosen.seeds = NULL,
#   chosen.targets = NULL, 
#   row.cols = NULL,
#   super.class = NULL,
#   plot.name = sprintf("cns_network_to_cns_network_%s.pdf","influence_quantile_99"),
#   rev = FALSE,
#   row.dend = NULL,
#   col.dend = NULL,
#   method = "euclidean",
#   symmetric = TRUE,
#   diagonal = FALSE
# )








