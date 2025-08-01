##################################
## FIGURE 2: SENSORY-MOTOR     ##
##################################
# Analyses direct sensory input to motor output connections
# showing how peripheral sensors influence specific body part
# effectors. Creates heatmap visualisation of sensory-motor pathways.
# Output: figures/figure2/links/*_sensors_to_effectors_heatmap.pdf

####################
## STARTUP        ##
####################

source("R/startup/banc-startup.R")
source("R/startup/franken-meta.R")
source("R/startup/banc-meta.R")

# Define output paths for different figure types
banc.fig2.path <- "figures/figure_2/links/"
banc.fig2.supp.path <- "figures/figure_2/links/supplement"
banc.fig2.anat.path <- "figures/figure_2/links/neuroanatomy"
banc.fig2.extra.path <- "figures/figure_2/links/extra"

####################
## INFLUENCE DATA ##
####################

# Extract sensory-to-effector influence data from database
con <- DBI::dbConnect(RSQLite::SQLite(),
                      file.path(banc.dropbox.influence.save.path,influence.sqlite))
influence.sensors.db.orig <- dplyr::tbl(con, "influence") %>%
  dplyr::filter(!is_seed,
                level %in% c("seed_02","seed_07")) %>%
  dplyr::filter(
    level=="seed_07"&seed%in%!!banc.vpn.meta$cell_type|level=="seed_02",
    id %in% !!banc.eff.meta$root_id
  ) %>%
  dplyr::collect() 
dbDisconnect(con)

####################
## LABEL MAPPING  ##
####################

# Define simplified sensory modality groupings for visualisation
seed.map <- c(
             #leg_taste_peg_neuron, 
             #unknown_orphan_neuron, 
             #retina_photoreceptor_neuron = "retina", 
             #internal_thermosensory_receptor_neuron = "antennal lobe", 
             #antenna_hygrosensory_receptor_neuron = "antennal lobe",  
             #antenna_olfactory_receptor_neuron = "antennal lobe", 
             #antenna_thermosensory_receptor_neuron = "antennal lobe", 
             #maxillary_palp_olfactory_receptor_neuron = "antennal lobe", 
             
             hemolymph_sensory_neuron = "hemolymph", 
             aorta_sensory_neuron = "aorta", 
             #abdomen_oxygenation_neuron = "oxygenation",
             cibarium_multidendritic_neuron = "cibarium", 
             
             crop_internal_taste_sensillum_neuron = "crop", 
             
             pharynx_internal_taste_sensillum_neuron = "pharynx",
             pharynx_orphan_neuron = "pharynx",
             postocellar_bristle_neuron = "pharynx", 
             
             labellum_taste_peg_neuron = "labellar taste", 
             labellum_external_taste_sensillum_neuron = "labellar taste", 
             #labellum_orphan_neuron = "labellum", 
             labellum_bristle_neuron = "probosics",
             
             haustellum_bristle_neuron = "probosics", 
       
             antenna_bristle_neuron = "antenna", 
             antenna_campaniform_sensillum_neuron = "antenna", 
             #antenna_orphan_neuron = "antenna", 
            
             johnstons_organ_A_neuron  = "antenna", 
             johnstons_organ_B_neuron = "antenna", 
             johnstons_organ_C_neuron = "antenna", 
             johnstons_organ_D_neuron = "antenna", 
             johnstons_organ_E_neuron = "antenna", 
             johnstons_organ_F_neuron = "antenna", 
             johnstons_organ_other_neuron = "antenna", 
             
             frontal_bristle_neuron = "head", 
             interocellar_bristle_neuron = "head", 
             interommatidial_bristle_neuron  = "head", 
             maxillary_palp_bristle_neuron = "head", 
             occipital_bristle_neuron = "head",
             occipital_dorsal_bristle_neuron = "head",
             postorbital_dorsal_bristle_neuron = "head", 
             postorbital_ventral_bristle_neuron = "head", 
             vibrissa_bristle_neuron = "head", 
             
             eye_bristle_neuron = "eye", 
             
             prosternal_hair_plate_neuron = "prosternal organ", 
             
             metathoracic_chordotonal_organ_neuron = "body chordotonal", 
             prothoracic_chordotonal_organ_neuron = "body chordotonal", 
             wheelers_chordotonal_organ_neuron = "body chordotonal", 
             
             
             wing_base_campaniform_sensillum_neuron = "wing", 
             wing_base_chordotonal_organ_neuron  = "wing", 
             wing_base_orphan_neuron  = "wing", 
             wing_campaniform_sensillum_neuron  = "wing", 
             wing_margin_bristle_neuron  = "wing", 
             wing_margin_taste_peg_neuron  = "wing", 
             wing_multidendritic_neuron  = "wing", 
             wing_tegula_campaniform_sensillum_neuron  = "wing", 
             wing_tegula_chordotonal_organ_neuron  = "wing", 
             wing_tegula_hair_plate_neuron  = "wing", 
             wing_tegula_orphan_neuron  = "wing",
            
            haltere_bristle_neuron = "haltere", 
            haltere_campaniform_sensillum_neuron = "haltere", 
            haltere_orphan_neuron = "haltere", 
            
             thorax_bristle_neuron = "thorax", 
             thorax_campaniform_sensillum_neuron  = "thorax", 
             
             front_leg_bristle_neuron = "front leg", 
             front_leg_chordotonal_organ_neuron = "front leg", 
             front_leg_claw_chordotonal_organ_neuron = "front leg", 
             front_leg_club_chordotonal_organ_neuron = "front leg", 
             front_leg_hair_plate_neuron = "front leg", 
             front_leg_hook_chordotonal = "front leg", 
             front_leg_multidendritic_neuron = "front leg", 
             front_leg_orphan_neuron = "front leg", 
             front_leg_taste_peg_neuron = "front leg", 
             front_leg_campaniform_sensillum_neuron = "front leg", 
            
             middle_leg_bristle_neuron = "middle leg",
             middle_leg_campaniform_sensillum_neuron = "middle leg",
             middle_leg_chordotonal_organ_neuron = "middle leg",
             middle_leg_claw_chordotonal_organ_neuron = "middle leg",
             middle_leg_club_chordotonal_organ_neuron = "middle leg",
             middle_leg_hair_plate_neuron = "middle leg",
             middle_leg_hook_chordotonal_neuron = "middle leg",
             middle_leg_multidendritic_neuron = "middle leg",
             middle_leg_orphan_neuron = "middle leg",
             middle_leg_taste_peg_neuron = "middle leg",
             
             hind_leg_bristle_neuron = "hind leg", 
             hind_leg_campaniform_sensillum_neuron = "hind leg", 
             hind_leg_chordotonal_organ_neuron = "hind leg", 
             hind_leg_claw_chordotonal_organ_neuron = "hind leg", 
             hind_leg_club_chordotonal_organ_neuron = "hind leg", 
             hind_leg_hair_plate_neuron = "hind leg", 
             hind_leg_hook_chordotonal = "hind leg", 
             hind_leg_multidendritic_neuron = "hind leg", 
             hind_leg_orphan_neuron = "hind leg", 
             hind_leg_taste_peg_neuron = "hind leg", 

             abdomen_orphan_neuron = "abdomen", 
             abdomen_strand_neuron = "abdomen", 
             abdominal_wall_multidendritic_neuron = "abdomen",
             thorax_multidendritic_neuron = "thorax", 
             abdomen_multidendritic_neuron = "abdomen",
            abdominal_terminalia_bristle = "abdomen",
            thorax_orphan_neuron  = "thorax")

# Define simplified effector target groupings
target.map <- c(retrocerebral_complex = "retrocerebral",
                corpus_allatum = "retrocerebral",
                enteric_complex = "digestive tract",
                enteric_complex = "digestive tract",
                digestive_tract = "digestive tract",
                crop = "crop", 
                salivary_gland = "salivary gland", 
                pharynx = "pharynx", 
                proboscis = "proboscis",
                antenna= "antenna", 
                eye = "eye", 
                neck = "neck", 
                neck = "neck", 
                neck = "neck",
                wing = "wing",
                haltere = "haltere", 
                front_leg = "front leg", 
                middle_leg = "middle leg", 
                hind_leg = "hind leg", 
                thoracic_abdominal_segmental = "thoracic-abdominal",
                neurohemal_complex = "neurohemal",
                ureter = "ureter",
                abdomen = "abdomen",
                reproductive_tract = "reproductive tract",
                ovaries = "ovaries",
                uterus = "uterus"
                )

# Define heatmap axis ordering for biological interpretation
col.order <- unname(unique(seed.map))
row.order <- unname(unique(target.map))

####################
## DATA PROCESSING ##
####################

# Process influence data and apply biological annotations
influence.sensors.db <- influence.sensors.db.orig %>%
  dplyr::left_join(banc.meta %>%
                     dplyr::distinct(id, .keep_all = TRUE),
                   by = c("id")) %>%
  dplyr::mutate(
    cell_class = dplyr::case_when(
      grepl("motor",cell_class) ~ "motor",
      grepl("visceral_circulatory|endocrine",cell_class) ~ "visceral_circulatory",
      TRUE ~ "motor"
    )
  ) %>%
  dplyr::mutate(
    region = dplyr::case_when(
      !grepl("neck",cell_sub_class) ~ region,
      !is.na(manc_match) ~ "vnc", 
      !is.na(fafb_match) ~ "midbrain", 
      TRUE ~ "midbrain"
    )
  ) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(seed = seed,
                target = body_part_effector) %>%
  dplyr::ungroup()

# Apply sensory modality grouping labels
if(!is.null(names(seed.map))){
  influence.sensors.db <- influence.sensors.db %>%
    dplyr::mutate(seed = case_when(
      seed %in% names(seed.map) ~ seed.map[seed],
      TRUE ~ seed
    )) %>%
    dplyr::filter(seed %in% col.order)
}
if(!is.null(names(target.map))){
  influence.sensors.db <- influence.sensors.db %>%
    dplyr::mutate(target = case_when(
      target %in% names(target.map) ~ target.map[target],
      TRUE ~ target
    )) %>%
    dplyr::filter(target %in% row.order)
}

# Normalise influence scores for heatmap visualisation
influence.sensors.db <- influence.sensors.db %>%
  calculate_influence_norms()

####################
## HEATMAP ANALYSIS ##
####################

# Define primary influence metric for main figure
inf.primary.metric <- "influence_log_minmax"

# Additional influence metrics for supplementary analyses
inf.metrics <- c(
  "influence_log",
  "influence_syn_norm_log",
  "influence_syn_norm_log_minmax",
  "influence_norm_log",
  "influence_norm_log_minmax",
  "influence_log_minmax",
  "influence_log_minmax_seed")

# Generate heatmaps for each influence metric
inf.metrics <- inf.primary.metric
for(inf.metric in inf.metrics){
  
  # Create influence matrix for heatmap visualisation
  heatmap_matrix <- reshape2::acast(
    data = influence.sensors.db,
    formula = target ~ seed,
    value.var = inf.metric,
    fun.aggregate = function(x) mean(x, na.rm = TRUE)
  )
  heatmap_matrix[is.na(heatmap_matrix)] <- 0
  heatmap_matrix[is.infinite(heatmap_matrix)] <- 0
  
  # Define colour scale for influence values 
  scaled_heatmap_breaks <- seq(quantile(heatmap_matrix,0.01, na.rm=TRUE), quantile(heatmap_matrix,0.999, na.rm=TRUE), length.out = n_breaks)
  scaled_heatmap_palette <- colorRampPalette(c("#1f4e79", "#4a90a4", "#7ba7bc", "#a67c8a", "#c4967d", "#b22222"))(n_breaks - 1)

  # Generate row annotations for biological context
  cell_type_annotation <- influence.sensors.db %>%
    dplyr::distinct(target, cell_class, region) %>%
    dplyr::arrange(region, cell_class, target) %>%
    dplyr::distinct(target, .keep_all = TRUE) %>%
    dplyr::filter(!is.na(target), 
                  target %in% rownames(heatmap_matrix)) %>%
    column_to_rownames("target")
  
  # Align matrix and annotations for consistent ordering
  heatmap_matrix <- heatmap_matrix[rownames(heatmap_matrix) %in% rownames(cell_type_annotation),]
  cell_type_annotation <- cell_type_annotation[rownames(cell_type_annotation)%in%rownames(heatmap_matrix),]
  
  # Group effector targets by anatomical region
  groups <- split(rownames(cell_type_annotation), cell_type_annotation$region)
  
  # Apply minimum group size threshold for clustering
  groups <- lapply(groups, function(g) if(length(g) >= 2) g else NULL)
  groups <- groups[!sapply(groups, is.null)]
  
  # Perform region-aware hierarchical clustering
  clustering_result <- hclust_semisupervised(data = heatmap_matrix,
                                             groups = groups,
                                             dist_method = "euclidean",
                                             hclust_method = "ward.D2")
  heatmap_matrix_normalized <- clustering_result$data
  cell_type_annotation <- cell_type_annotation[rownames(heatmap_matrix_normalized), , drop = FALSE]
  
  # Define annotation colour scheme
  annotation_colors <- list(
    region = paper.cols[names(paper.cols) %in% unique(cell_type_annotation$region)]
  )
  
  # Calculate column distances for clustering
  col_dist <- dist(t(heatmap_matrix_normalized), method = "euclidean")
  euclidean_dist_matrix_cols <- hclust(col_dist, method = "ward.D2")
  
  # Optimise row and column ordering using seriation
  order_rows <- seriation::seriate(dist(heatmap_matrix_normalized))
  order_cols <- seriation::seriate(dist(t(heatmap_matrix_normalized)))
  
  # Apply seriation-based matrix reordering
  reordered_matrix <- heatmap_matrix_normalized[seriation::get_order(order_rows), seriation::get_order(order_cols)]
  
  # Select output directory based on analysis type
  if(inf.metric == inf.primary.metric){
    banc.fig2.path.heatmap <- banc.fig2.path
  }else{
    banc.fig2.path.heatmap <- banc.fig2.extra.path
  }
  
  # Generate and export sensory-motor heatmap
  col.order.hm <- intersect(col.order, colnames(reordered_matrix))
  row.order.hm <- intersect(row.order, rownames(reordered_matrix))
  pheatmap( 
    reordered_matrix[row.order.hm,col.order.hm],
    color = scaled_heatmap_palette,
    breaks = scaled_heatmap_breaks,
    #annotation_row = cell_type_annotation,
    annotation_colors = annotation_colors,
    clustering_method = "ward.D2",
    cluster_rows = FALSE, 
    cluster_cols = FALSE,
    treeheight_row = 0,
    treeheight_col = 0,
    show_rownames = TRUE,
    show_colnames = TRUE,
    fontsize_row = 12,
    fontsize_col = 12,
    cellwidth = 18,
    cellheight = 12,
    main = inf.metric,
    annotation_names_col = FALSE,
    annotation_names_row = FALSE,
    filename = file.path(banc.fig2.path.heatmap, sprintf("%s_sensors_to_effectors_heatmap.pdf",inf.metric))
  )
}

