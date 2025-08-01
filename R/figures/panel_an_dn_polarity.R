############################
############################
### NECK NEURON POLARITY ###
############################
############################

###############
### STARTUP ###
###############

# load
source("R/startup/banc-startup.R")
source("R/startup/franken-meta.R")
banc.version <- NULL
source("R/startup/banc-meta.R")

######################
### Neuron calibre ###
######################

### overview
pd.width.plot <- banc.meta %>%
  dplyr::filter(super_class != "sensory", 
                !is.na(pd_width)) %>%
  dplyr::mutate(group = dplyr::case_when(
    grepl("descending",super_class) ~ "descending",
    grepl("ascending",super_class) ~ "ascending",
    grepl("brain",region) ~ "brain",
    grepl("brain",region) ~ "vnc",
    grepl("optic",region) ~ "optic"
  )) %>%
  dplyr::distinct(root_id, group, pd_width)

### DNs

# Wrangled
dn.pd.width.plot <- banc.meta %>%
  dplyr::filter(grepl("descending",super_class),
                !is.na(pd_width),
                !is.na(cluster),
                grepl("DN",cluster)) %>%
  dplyr::distinct(root_id, cluster, pd_width)

# Calculate median pd_width for each cluster and order clusters
cluster_order <- dn.pd.width.plot %>%
  dplyr::group_by(cluster) %>%
  dplyr::summarise(median_pd_width = median(pd_width, na.rm = TRUE)) %>%
  dplyr::arrange(median_pd_width) %>%
  dplyr::pull(cluster)

# Reorder the cluster factor based on median pd_width
dn.pd.width.plot <- dn.pd.width.plot %>%
  mutate(cluster = factor(cluster, levels = cluster_order))

# Create the histogram with ordered clusters
g.dn.pd.width <- ggplot(dn.pd.width.plot, aes(x = pd_width, fill = cluster)) +
  geom_histogram(binwidth = 0.1, position = "identity", alpha = 0.7) +
  facet_wrap(~ cluster, scales = "free_y", ncol = 4) +
  labs(title = "",
       subtitle = "",
       x = "",
       y = "") +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.background = element_blank(),
    strip.text = element_text(face = "bold",size = 20),
    axis.title = element_blank(),
    axis.text.y = element_text(angle = 0, hjust = 0.5, size = 16, color = "black"),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 24, color = "black"),
    panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.margin = margin(t = 20, r = 20, b = 20, unit = "pt")
  ) +
  scale_fill_viridis_d() 

# Print
print(g.dn.pd.width)

# Save
ggsave(plot = g.dn.pd.width,
       filename = file.path(banc.fig3.extra.path, "descending_primary_dendrite_width_by_cluster.pdf"), 
       width = 20, height = 8, dpi = 300) 

### ANs

# Wrangle
dn.pd.width.plot <- banc.meta %>%
  dplyr::filter(grepl("ascending",super_class),
                !is.na(pd_width),
                !is.na(cluster),
                grepl("AN",cluster)) %>%
  dplyr::distinct(root_id, cluster, pd_width)

# Calculate median pd_width for each cluster and order clusters
cluster_order <- dn.pd.width.plot %>%
  dplyr::group_by(cluster) %>%
  dplyr::summarise(median_pd_width = median(pd_width, na.rm = TRUE)) %>%
  dplyr::arrange(median_pd_width) %>%
  dplyr::pull(cluster)

# Reorder the cluster factor based on median pd_width
dn.pd.width.plot <- dn.pd.width.plot %>%
  mutate(cluster = factor(cluster, levels = cluster_order))

# Create the histogram with ordered clusters
g.dn.pd.width <- ggplot(dn.pd.width.plot, aes(x = pd_width, fill = cluster)) +
  geom_histogram(binwidth = 0.1, position = "identity", alpha = 0.7) +
  facet_wrap(~ cluster, scales = "free_y", ncol = 4) +
  labs(title = "",
       subtitle = "",
       x = "",
       y = "") +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.background = element_blank(),
    strip.text = element_text(face = "bold",size = 20),
    axis.title = element_blank(),
    axis.text.y = element_text(angle = 0, hjust = 0.5, size = 16, color = "black"),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 24, color = "black"),
    panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.margin = margin(t = 20, r = 20, b = 20, unit = "pt")
  ) +
  scale_fill_viridis_d() 

# Print
print(g.dn.pd.width)

# Save
ggsave(plot = g.dn.pd.width,
       filename = file.path(banc.fig3.path, "ascending_primary_dendrite_width_by_cluster.pdf"), 
       width = 20, height = 8, dpi = 300) 

##################
### SIDE INDEX ###
##################

# Show histogram of fow centrality
banc.plot.meta <- bc.orig %>%
  dplyr::filter(super_class %in% c("ascending", "descending"),
                !is.na(super_cluster),
                !is.na(output_side_index),
                output_side_index!=0&input_side_index!=0) %>%
  dplyr::mutate(
    contralaterality = 1-abs(input_side_index+output_side_index)
  )

# Plot segregation index
g.bilat <- ggplot(banc.plot.meta, aes(x = contralaterality, color = super_class)) +
  geom_density(size = 1.2, position = "identity", na.rm = TRUE) +
  facet_wrap(~super_cluster, scales = "free_y") +
  scale_color_manual(values = paper.cols)  +
  labs(
    x = "(-1) fully ipsilateral to fully contralateral (1)",
    y = "density",
    color = "super class"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    plot.title = element_text(hjust = 0.5, size = 14),
    strip.text = element_text(size = 18)
  ) +
  theme(legend.position = "none",
        legend.title = element_blank())

# Show
print(g.bilat)

# Save
ggsave(g.bilat, 
       filename = file.path(banc.fig3.supp.path, "neck_neuron_contralaterality_index_super_cluster.pdf"), 
       width = 14, height = 7, dpi = 300)

#########################
### SEGREGATION INDEX ###
#########################

# Show histogram of flow centrality
banc.plot.meta <- banc.meta %>%
  dplyr::filter(super_class %in% c("ascending", "descending"),
                !is.na(segregation_index),
                segregation_index !=0) %>%
  dplyr::group_by(cell_type, super_class) %>%
  dplyr::summarise(segregation_index = mean(segregation_index, na.rm = TRUE))

# Plot segregation index
g.si <- ggplot(banc.plot.meta, aes(x = segregation_index, 
                           color = super_class)) +
  geom_density(size = 1.2, na.rm = TRUE) +
  scale_color_manual(values = paper.cols) +
  labs(
    x = "segregation index",
    y = "density",
    color = "super class"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    plot.title = element_text(hjust = 0.5, size = 14)
  ) +
  theme(legend.position = "none")

# Show
print(g.si)

# Save
ggsave(g.si, 
       filename = file.path(banc.fig2.supp.path, "neck_neuron_segregation_index_super_class.pdf"), 
       width = 6, height = 3, dpi = 300)

#####################
### SYNAPSE COUNT ###
#####################

banc.an.dn <- banc.neck.meta %>%
  dplyr::filter(super_class %in% c("ascending",
                                   "descending"))
neck.synapses.list <- pbapply::pblapply(unique(banc.an.dn$root_id), function(x)
  bancr::banc_partners(x,
                       partners = "output"))
neck.synapses.list.select <- pbapply::pblapply(neck.synapses.list, function(x){
  try({points <- nat::xyzmatrix(x$pre_pt_position)
      x$pre_pt_root_id <- as.character(x$pre_pt_root_id)
      cbind(x,points) %>%
        dplyr::select(pre_pt_root_id,X,Y,Z)
  })
}
)
neck.synapses <- do.call(rbind, neck.synapses.list.select)

# Get inclusion
y_cut <- 450000
points <- nat::xyzmatrix(neck.synapses)
in_brain <- points[,2]<y_cut
neck.synapses$region <- ifelse(in_brain,"brain","ventral_nerve_cord")

# Get per cell_type/count totals
ct_totals <- neck.synapses %>%
  dplyr::select(pre_pt_root_id, region) %>%
  dplyr::mutate(pre_pt_root_id = as.character(pre_pt_root_id)) %>%
  dplyr::left_join(
    banc.an.dn %>%
      dplyr::select(root_id, super_class, cell_type, top_nt),
    by = c("pre_pt_root_id" = "root_id")
  ) %>%
  dplyr::group_by(super_class, cell_type) %>%
  dplyr::summarise(total_count = dplyr::n(), .groups = "drop")

# Get per cell_type, region counts
region_counts <- neck.synapses %>%
  dplyr::select(pre_pt_root_id, region) %>%
  dplyr::mutate(pre_pt_root_id = as.character(pre_pt_root_id)) %>%
  dplyr::left_join(
    banc.an.dn %>%
      dplyr::select(root_id, super_class, cell_type, top_nt),
    by = c("pre_pt_root_id" = "root_id")
  ) %>%
  dplyr::group_by(super_class, cell_type, region) %>%
  dplyr::summarise(region_count = dplyr::n(), .groups = "drop")

# Join and compute normalized column
neck.synapses.plot <- region_counts %>%
  dplyr::left_join(
    ct_totals,
    by = c("super_class", "cell_type")
  ) %>%
  dplyr::filter(!is.na(super_class)) %>%
  dplyr::mutate(norm = region_count / total_count) %>%
  dplyr::arrange(cell_type, region)
  
# Calculate median and IQR per region and super_class
summary_df <- neck.synapses.plot %>%
  dplyr::group_by(super_class, region) %>%
  dplyr::summarise(
    med = median(region_count, na.rm = TRUE),
    q1 = quantile(region_count, 0.25, na.rm = TRUE),
    q3 = quantile(region_count, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

g.syn.regions <- ggplot(neck.synapses.plot, 
                        aes(x = region, y = region_count)) +
  # Paired lines
  geom_line(aes(group = cell_type), color = "darkgrey", size = 0.7) +
  # Violin
  geom_violin(aes(fill = region), color = NA, alpha = 0.75, width = 0.8, trim = FALSE) +
  # Jittered points
  #geom_jitter(aes(color = region), width = 0.1, size = 1) +
  # IQR bars
  geom_errorbar(data = summary_df,
                aes(x = region, ymin = q1, ymax = q3),
                width = 0.3, color = "black", inherit.aes = FALSE) +
  # Median points
  geom_point(data = summary_df,
             aes(x = region, y = med),
             color = "black", size = 3, shape = 95, inherit.aes = FALSE) +
  facet_wrap(~super_class) +
  labs(
    x = "",
    y = "presynaptic count",
    color = "region",
    fill = "region"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 8),
    plot.title = element_text(hjust = 0.5, size = 14)
  ) +
  scale_fill_manual(values = paper.cols) +
  scale_color_manual(values = paper.cols) +
  scale_y_continuous(trans = "log10") +
  theme(legend.position = "none")

# Show
print(g.syn.regions)

# Save
ggsave(g.syn.regions, 
       filename = file.path(banc.fig2.extra.path, "neck_neuron_presynapes_by_super_class.pdf"), 
       width = , height = 4, dpi = 300)


# Plot segregation index
g.syn.dens <- ggplot(neck.synapses.plot %>% 
                 dplyr::filter(region=="ventral_nerve_cord"), 
               aes(x = norm, 
                   color = super_class)) +
  geom_density(size = 1.2, na.rm = TRUE) +
  scale_color_manual(values = paper.cols) +
  labs(
    x = "proportion of presynapses in the ventral nerve cord",
    y = "density",
    color = "super_class"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    plot.title = element_text(hjust = 0.5, size = 14)
  ) +
  theme(legend.position = "none")

# Show
print(g.syn.dens)

# Save
ggsave(g.syn.dens, 
       filename = file.path(banc.fig2.supp.path, "neck_neuron_presynapse_proportion_in_vnc.pdf"), 
       width = 6, height = 3, dpi = 300, bg = "transparent")

#####################################
### INFLUENCE TO EFFECTOR SCATTER ###
#####################################
# new meta
banc.eff2.meta <- banc.eff.meta %>%
  dplyr::mutate(body_part_effector = dplyr::case_when(
    grepl("power|steering|tension",cell_function) ~ cell_function,
    grepl("pitch|yaw|roll",cell_function_detailed) ~ cell_function_detailed,
    TRUE ~ body_part_effector
  ))

# Get alternative dataset for validation (seed_02)
banc.an.dn.meta <- banc.meta %>%
  dplyr::filter(super_class %in% c("ascending","descending")) %>%
  dplyr::filter(!grepl("^SA|^SN|^AN_4|^AN_5",cell_type))
chosen.seeds <- unique(banc.an.dn.meta$seed_07)
con <- DBI::dbConnect(RSQLite::SQLite(),
                      file.path(banc.dropbox.influence.save.path,influence.sqlite))
influence.nn.eff.db <- dplyr::tbl(con, influence.table) %>%
  dplyr::filter(!is_seed,
                level %in% c("seed_07")) %>%
  dplyr::filter(
    seed %in% !!chosen.seeds,
    id %in% !!banc.eff.meta$id
  ) %>%
  dplyr::collect() 
dbDisconnect(con)

# Organise
influence.plot.df <- influence.nn.eff.db %>%
  dplyr::left_join(banc.an.dn.meta %>%
                     dplyr::select(seed_07, seed_super_class = super_class) %>%
                     dplyr::distinct(seed_07, .keep_all = TRUE),
                   by = c("seed"="seed_07")) %>%
  dplyr::left_join(banc.eff2.meta %>%
                     dplyr::select(id, body_part_effector) %>%
                     dplyr::distinct(id, .keep_all = TRUE),
                   by = c("id")) %>%
  dplyr::mutate(target = id,
                seed = seed_super_class) %>%
  calculate_influence_norms()

# Pivot
influence.plot.wide <- influence.plot.df %>% 
  dplyr::filter(seed %in% c("ascending", "descending")) %>%
  dplyr::select(id, 
                seed, 
                influence_norm_log, 
                body_part_effector) %>%
  tidyr::pivot_wider(names_from = seed, values_from = influence_norm_log,
              names_prefix = "influence_norm_log_") %>%
  dplyr::filter(influence_norm_log_ascending>threshold.inf.value,
                influence_norm_log_descending>threshold.inf.value) %>%
  dplyr::arrange(influence_norm_log_ascending)

# Shapes and colors

# Coordinate coloursand shapes
body.part.shapes <- c("retrocerebral complex" = 21, 
                      "corpus allatum" = 24,
                      "enteric complex" = 23, 
                      "digestive tract" = 21, 
                      "crop" = 21, 
                      "salivary gland" = 24, 
                      "pharynx" = 23, 
                      "proboscis" = 21, 
                      "antenna" = 21, 
                      "eye" = 24, 
                      "neck" = 23, 
                      "haltere" = 21, 
                      "wing" = 21, 
                      "front leg" = 24,
                      "middle leg" = 23, 
                      "hind leg" = 21,
                      "ureter" = 21, 
                      "abdomen" = 24, 
                      "reproductive tract" = 21, 
                      "uterus" = 21, 
                      "neurohemal complex" = 21,
                      "haltere power" = 3,
                      "haltere steering" = 4,
                      "wing power" = 3,
                      "wing steering"= 4,
                      "wing tension" = 8,
                      "neck yaw" = 3,
                      "neck pitch" = 4,
                      "neck roll" = 8,
                      "thoracic abdominal segmental" = 21
)
body.parts <- names(body.part.shapes)
paper.cols <- c(paper.cols,
                `haltere power` = paper.cols[["haltere"]],
                `haltere steering` = paper.cols[["haltere"]],
                `wing power` = paper.cols[["wing"]],
                `wing steering`= paper.cols[["wing"]],
                `wing tension` = paper.cols[["wing"]],
                `neck yaw` = paper.cols[["neck"]],
                `neck pitch` = paper.cols[["neck"]],
                `neck roll` = paper.cols[["neck"]])
paper.cols <- paper.cols[!duplicated(names(paper.cols))]
influence.plot.wide$body_part_effector <- gsub("_"," ",influence.plot.wide$body_part_effector)
influence.plot.wide$body_part_effector <- factor(influence.plot.wide$body_part_effector, levels = body.parts)

# Plot
g.inf.an.dn.corr <- ggplot(influence.plot.wide, 
       aes(x = influence_norm_log_ascending, 
           y = influence_norm_log_descending,
           #color = body_part_effector,
           #shape = body_part_effector,
           group = 1)) +
  geom_point(color = "lightgrey") +
  # geom_smooth(aes(color = NULL, group = 1),
  #             formula = 'y ~ x', 
  #             method = "lm", 
  #             se = FALSE, 
  #             color = "black", 
  #             linetype = "dashed") +
  # stat_poly_eq(aes(color = NULL, 
  #                  label = paste(after_stat(eq.label), 
  #                                after_stat(rr.label), sep = "~~~")),
  #              formula = y ~ x, 
  #              parse = TRUE, 
  #              label.x = 0.2, 
  #              label.y = 0.1, 
  #              fontface = "bold",
  #              size =5) +
  geom_abline(slope = 1, 
              intercept = 0, 
              color = "black", 
              linetype = "solid",
              linewidth = 1) +
  theme_minimal() +
  labs(
    x = "influence_log_norm (ascending)",
    y = "influence_log_norm (descending)",
    title = ""
  ) +
  #scale_color_manual(values = paper.cols) +
  #scale_shape_manual(values = body.part.shapes) +
  theme_minimal() + 
  coord_equal() +
  theme(legend.position = "none")  +
  ggplot2::coord_fixed() + theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  ylim(c(17.5,25)) +
  xlim(c(17.5,25))

# Show
plot(g.inf.an.dn.corr)

# Save
ggsave(g.inf.an.dn.corr, 
       filename = file.path(banc.fig2.path, "influence_log_norm_ascending_descending_to_efferent_scatter.pdf"), 
       width = 3, height = 3, dpi = 300, bg = "transparent")


