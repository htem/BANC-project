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
source("R/startup/banc-edgelist.R")
banc.version <- NULL
source("R/startup/banc-meta.R")
banc.meta$root_id <- banc.meta$root_626

# Define output paths for different figure types
banc.fig2.path <- "figures/figure_2/links/"
banc.fig2.supp.path <- "figures/figure_2/links/supplement"
banc.fig2.anat.path <- "figures/figure_2/links/neuroanatomy"
banc.fig2.extra.path <- "figures/figure_2/links/extra"

####################
## INFLUENCE DATA ##
####################

# Define AN/DN metadata
banc.an.dn.meta <- banc.meta %>%
  dplyr::filter(super_class %in% c("ascending","descending")) %>%
  dplyr::filter(!grepl("^SA|^SN|^AN_4|AN_5|^IN",cell_type))

# Set up for influence calculation
banc.meta$root_id <- banc.meta$root_626
ic_banc <- influence_calculator_py(edgelist_simple = banc.edgelist.simple %>%
                                     dplyr::filter(count > 5),
                                   meta = banc.meta)
banc.sens.csc  <- banc.sens.meta %>%
  dplyr::distinct(seed_02) %>%
  dplyr::pull(seed_02)
cts <- na.omit(banc.sens.csc)
influence.sensors.db.orig <- data.frame()
for(ct in cts){
  banc.ct.meta <- subset(banc.meta,seed_02==ct)
  banc.ct.ids <- unique(na.omit(banc.ct.meta$root_id))
  try({
    control_influence.id <- calculate_influence_py(ic_banc, banc.ct.ids) %>%
      dplyr::filter(id %in% banc.eff.meta$id) %>%
      dplyr::left_join(banc.eff.meta %>%
                         dplyr::distinct(id=root_id,
                                         target=body_part_effector,
                                         target_super_class=super_class), by = "id")
    control_influence.id$seed <- ct
    control_influence.id$seed_class <- "sensory"
    control_influence.id$influence_norm_original <- control_influence.id$`Influence_score_(unsigned)`/length(banc.ct.ids)
    influence.sensors.db.orig <- rbind(influence.sensors.db.orig,
                                    control_influence.id)
  })
}
influence.sensors.db.orig <- influence.sensors.db.orig %>%
  dplyr::mutate(influence_original = `Influence_score_(unsigned)`) 

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
             visual_achromatic_lamina_projection = "retina",
             visual_chromatic_lamina_projection = "retina",
             lamina_monopolar = "retina",
             
             hemolymph_sensory_neuron = "hemolymph", 
             aorta_sensory_neuron = "hemolymph", 

             cibarium_multidendritic_neuron = "enteric", 
             crop_internal_taste_sensillum_neuron = "enteric",
             anterior_digestive_tract_internal_taste_sensillum_neuron = "enteric",
             anterior_digestive_tract_multidendritic_neuron = "enteric",
             
             pharynx_internal_taste_sensillum_neuron = "pharynx",
             # pharynx_orphan_neuron = "pharynx",  # Removed: orphan neurons excluded from analysis
             postocellar_bristle_neuron = "pharynx", 
             pharynx_fishtrap_bristle_neuron = "pharynx", 
             #labellum_orphan_neuron = pharynx", 
             
             labellum_bristle_neuron = "proboscis",
             haustellum_bristle_neuron = "proboscis", 
             labellum_taste_peg_neuron = "proboscis", 
             labellum_external_taste_sensillum_neuron = "proboscis", 
       
             antenna_bristle_neuron = "antenna", 
             antenna_campaniform_sensillum_neuron = "antenna", 
             johnstons_organ_A_neuron  = "antenna", 
             johnstons_organ_B_neuron = "antenna", 
             johnstons_organ_C_neuron = "antenna", 
             johnstons_organ_D_neuron = "antenna", 
             johnstons_organ_E_neuron = "antenna", 
             johnstons_organ_F_neuron = "antenna", 
             johnstons_organ_other_neuron = "antenna", 
             #antenna_orphan_neuron = "antenna",
             
             frontal_bristle_neuron = "head", 
             interocellar_bristle_neuron = "head", 
             interommatidial_bristle_neuron  = "head", 
             maxillary_palp_bristle_neuron = "head", 
             occipital_bristle_neuron = "head",
             occipital_dorsal_bristle_neuron = "head",
             postorbital_dorsal_bristle_neuron = "head", 
             postorbital_ventral_bristle_neuron = "head", 
             vibrissa_bristle_neuron = "head", 
             frontoorbital_bristle_neuron = "head",
             ocellar_bristle_neuron = "head",
             orbital_bristle_neuron = "head",
             
             eye_bristle_neuron = "eye", 
             
             prosternal_hair_plate_neuron = "neck", 
             neck_chordotonal_organ_neuron = "neck",
             
             metathoracic_chordotonal_organ_neuron = "body chordotonal", 
             prothoracic_chordotonal_organ_neuron = "body chordotonal", 
             wheelers_chordotonal_organ_neuron = "body chordotonal", 
             
             
             wing_base_campaniform_sensillum_neuron = "wing", 
             wing_base_chordotonal_organ_neuron  = "wing", 
             # wing_base_orphan_neuron  = "wing",  # Removed: orphan neurons excluded from analysis 
             wing_campaniform_sensillum_neuron  = "wing", 
             wing_margin_bristle_neuron  = "wing", 
             wing_margin_taste_peg_neuron  = "wing", 
             wing_multidendritic_neuron  = "wing", 
             wing_tegula_campaniform_sensillum_neuron  = "wing", 
             wing_tegula_chordotonal_organ_neuron  = "wing", 
             wing_tegula_hair_plate_neuron  = "wing", 
             # wing_tegula_orphan_neuron  = "wing",  # Removed: orphan neurons excluded from analysis
            
            haltere_bristle_neuron = "haltere", 
            haltere_campaniform_sensillum_neuron = "haltere", 
            # haltere_orphan_neuron = "haltere",  # Removed: orphan neurons excluded from analysis 
            haltere_chordotonal_organ_neuron = "haltere",
            haltere_thoracic_abdominal_segmental_sensory_neuron = "haltere",
            
             thorax_bristle_neuron = "thorax", 
             thorax_campaniform_sensillum_neuron  = "thorax", 
             
             front_leg_bristle_neuron = "front leg", 
             front_leg_chordotonal_organ_neuron = "front leg", 
             front_leg_claw_chordotonal_organ_neuron = "front leg", 
             front_leg_club_chordotonal_organ_neuron = "front leg", 
             front_leg_hair_plate_neuron = "front leg", 
             front_leg_hook_chordotonal = "front leg", 
             front_leg_multidendritic_neuron = "front leg", 
             # front_leg_orphan_neuron = "front leg",  # Removed: orphan neurons excluded from analysis 
             front_leg_taste_peg_neuron = "front leg", 
             front_leg_taste_bristle_neuron = "front leg", 
             front_leg_campaniform_sensillum_neuron = "front leg", 
             front_leg_hook_chordotonal_organ_neuron = "front leg",
             front_leg_chordotonal_organ_neuorn = "front leg",
             front_leg_bilateral_campaniform_sensillum_neuron = "front leg",
            
             middle_leg_bristle_neuron = "middle leg",
             middle_leg_campaniform_sensillum_neuron = "middle leg",
             middle_leg_chordotonal_organ_neuron = "middle leg",
             middle_leg_claw_chordotonal_organ_neuron = "middle leg",
             middle_leg_club_chordotonal_organ_neuron = "middle leg",
             middle_leg_hair_plate_neuron = "middle leg",
             middle_leg_hook_chordotonal_neuron = "middle leg",
             middle_leg_multidendritic_neuron = "middle leg",
             # middle_leg_orphan_neuron = "middle leg",  # Removed: orphan neurons excluded from analysis
             middle_leg_taste_peg_neuron = "middle leg",
             middle_leg_taste_bristle_neuron = "middle leg",
             middle_leg_hook_chordotonal_organ_neuron = "middle leg",
             middle_leg_bilateral_campaniform_sensillum_neuron = "middle leg",
             
             hind_leg_bristle_neuron = "hind leg", 
             hind_leg_campaniform_sensillum_neuron = "hind leg", 
             hind_leg_chordotonal_organ_neuron = "hind leg", 
             hind_leg_claw_chordotonal_organ_neuron = "hind leg", 
             hind_leg_club_chordotonal_organ_neuron = "hind leg", 
             hind_leg_hair_plate_neuron = "hind leg", 
             hind_leg_hook_chordotonal = "hind leg", 
             hind_leg_multidendritic_neuron = "hind leg", 
             # hind_leg_orphan_neuron = "hind leg",  # Removed: orphan neurons excluded from analysis 
             hind_leg_taste_peg_neuron = "hind leg", 
             hind_leg_taste_bristle_neuron = "hind leg", 
             hind_leg_hook_chordotonal_organ_neuron = "hind leg",
             hind_leg_bilateral_campaniform_sensillum_neuron = "hind leg",
            
             thorax_thoracic_abdominal_segmental_sensory_neuron = "thoracic-abdominal",
             thoracic_abdominal_segmental = "thoracic-abdominal",
             thoracic_segmental = "thoracic-abdominal",
             `thoracic-segmental` = "thoracic-abdominal",
             abdomen_oxygenation_neuron = "thoracic-abdominal",
            
             posterior_uterine_sensory_neuron = "reproductive",
            
             sex_peptide_sensory_neuron = "reproductive",
             abdominal_terminalia_bristle = "reproductive",

             # abdomen_orphan_neuron = "abdomen",  # Removed: orphan neurons excluded from analysis 
             abdomen_strand_neuron = "abdomen", 
             abdominal_wall_multidendritic_neuron = "abdomen",
             abdomen_multidendritic_neuron = "abdomen",
             abdominal_ppk_neuron = "abdomen",

            thorax_multidendritic_neuron = "thorax"
            # thorax_orphan_neuron  = "thorax"  # Removed: orphan neurons excluded from analysis
            )

# Define simplified effector target groupings
target.map <- c(retrocerebral_complex = "hemolymph",
                corpus_allatum = "hemolymph",
                enteric_complex = "enteric",
                digestive_tract = "enteric",
                anterior_digestive_tract = "enteric",
                crop = "enteric", 
                salivary_gland = "pharynx", 
                labellum = "pharynx",
                pharynx = "pharynx", 
                proboscis = "proboscis",
                antenna = "antenna", 
                eye = "eye", 
                neck = "neck", 
                wing = "wing",
                haltere = "haltere", 
                neurohemal_complex = "hemolymph",
                front_leg = "front leg", 
                middle_leg = "middle leg", 
                hind_leg = "hind leg", 
                thoracic_abdominal = "thoracic-abdominal",
                thoracic_abdominal_segmental = "thoracic-abdominal",
                thoracic_segmental = "thoracic-abdominal",
                `thoracic-segmental` = "thoracic-abdominal",
                ureter = "ureter",
                abdomen = "abdomen",
                reproductive_tract = "reproductive",
                ovaries = "ovaries",
                uterus = "reproductive"
                )

# Define heatmap axis ordering for biological interpretation
col.order <- unname(unique(seed.map))
row.order <- unname(unique(target.map))

# paied and unpaird
paired.bps <- unique(target.map[target.map%in%seed.map])
paired.bps <- setdiff(paired.bps,"thoracic-abdominal")
unpaired.out.bps <- c(unique(target.map[!target.map%in%seed.map]),"thoracic-abdominal")
unpaired.in.bps <- c(unique(seed.map[!seed.map%in%target.map]),"thoracic-abdominal")

#####################
## DATA PROCESSING ##
#####################

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
      !is.na(manc_match) ~ "ventral_nerve_cord", 
      !is.na(fafb_match) ~ "central_brain", 
      TRUE ~ "central_brain"
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

######################
## HEATMAP ANALYSIS ##
######################

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
  
  # Paired
  col.order.hm <- intersect(paired.bps, colnames(reordered_matrix))
  row.order.hm <- unique(c(intersect(paired.bps, rownames(reordered_matrix)),row.order.hm))
  pheatmap( 
    reordered_matrix[paired.bps,paired.bps],
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
    filename = file.path(banc.fig2.path.heatmap, sprintf("%s_paired_sensors_to_effectors_heatmap.pdf",inf.metric))
  )

  # Body-part relatedness analysis using cosine similarity
  # Test claim: "Each local loop is influenced by a select group of more distant sensors
  # in functionally related body parts"
  # Cosine similarity between effector body parts defines functional relatedness
  # Extract the paired body parts matrix
  paired_matrix <- reordered_matrix[paired.bps, paired.bps]

  # Get body part names in the order they appear in the matrix
  bp_names <- rownames(paired_matrix)
  n_bp <- length(bp_names)

  cat(sprintf("Analyzing %d body parts in paired matrix\n", n_bp))

  # Calculate cosine similarity between all pairs of effector body parts
  # Each row represents an effector body part's sensory input profile
  cosine_similarity_matrix <- matrix(NA, nrow = n_bp, ncol = n_bp,
                                     dimnames = list(bp_names, bp_names))

  for (i in 1:n_bp) {
    for (j in 1:n_bp) {
      vec_i <- paired_matrix[i, ]
      vec_j <- paired_matrix[j, ]
      # Cosine similarity = dot product / (magnitude_i * magnitude_j)
      dot_prod <- sum(vec_i * vec_j, na.rm = TRUE)
      mag_i <- sqrt(sum(vec_i^2, na.rm = TRUE))
      mag_j <- sqrt(sum(vec_j^2, na.rm = TRUE))
      if (mag_i > 0 && mag_j > 0) {
        cosine_similarity_matrix[i, j] <- dot_prod / (mag_i * mag_j)
      } else {
        cosine_similarity_matrix[i, j] <- 0
      }
    }
  }

  cat("Calculated cosine similarity matrix for effector body parts\n")
  cat(sprintf("Similarity range: %.3f to %.3f\n",
              min(cosine_similarity_matrix[lower.tri(cosine_similarity_matrix)]),
              max(cosine_similarity_matrix[lower.tri(cosine_similarity_matrix)])))

  # For each sensory→effector connection, get the cosine similarity
  # between that effector and the sensory body part (when it acts as an effector)
  # Matrix element [i,j]: influence from sensory body part j to effector body part i
  # X-axis: cosine similarity between effector i and effector j (both as effectors)
  # Y-axis: influence from j to i
  # Include ALL values (diagonal + off-diagonal)
  # When i == j (same body part), cosine similarity = 1.0
  similarity_influence_data <- data.frame(
    source_bp = character(),
    target_bp = character(),
    cosine_similarity = numeric(),
    influence = numeric(),
    is_same_bp = logical(),
    stringsAsFactors = FALSE
  )

  for (i in 1:n_bp) {
    for (j in 1:n_bp) {
      # Include ALL values, including diagonal (same body part)
      similarity_influence_data <- rbind(similarity_influence_data, data.frame(
        source_bp = bp_names[j],  # Sensory body part
        target_bp = bp_names[i],  # Effector body part
        cosine_similarity = cosine_similarity_matrix[i, j],  # Similarity between i and j as effectors
        influence = paired_matrix[i, j],  # Influence from j to i
        is_same_bp = (i == j),  # TRUE when same body part (diagonal)
        stringsAsFactors = FALSE
      ))
    }
  }

  cat(sprintf("Total values: %d (including %d same body part)\n",
              nrow(similarity_influence_data),
              sum(similarity_influence_data$is_same_bp)))
  cat(sprintf("Cosine similarity range: %.3f to %.3f\n",
              min(similarity_influence_data$cosine_similarity),
              max(similarity_influence_data$cosine_similarity)))
  cat(sprintf("Same body part similarities: %.3f to %.3f (should all be 1.0)\n",
              min(similarity_influence_data$cosine_similarity[similarity_influence_data$is_same_bp]),
              max(similarity_influence_data$cosine_similarity[similarity_influence_data$is_same_bp])))
  cat(sprintf("Influence range: %.2f to %.2f\n",
              min(similarity_influence_data$influence, na.rm = TRUE),
              max(similarity_influence_data$influence, na.rm = TRUE)))

  # Filter out matched body parts (diagonal, similarity = 1.0)
  similarity_influence_offdiag <- similarity_influence_data %>%
    dplyr::filter(!is_same_bp)

  # Test for linear correlation (off-diagonal only, matching plotted data)
  cor_test <- cor.test(similarity_influence_offdiag$cosine_similarity,
                       similarity_influence_offdiag$influence,
                       method = "pearson")

  # Calculate R²
  r_squared <- cor_test$estimate^2
  cat("\nLinear correlation test (Pearson, excluding matched body parts):\n")
  cat(sprintf("  Correlation coefficient (r): %.4f\n", cor_test$estimate))
  cat(sprintf("  R²: %.4f\n", r_squared))
  cat(sprintf("  p-value: %.3e\n", cor_test$p.value))
  cat(sprintf("  95%% CI: [%.4f, %.4f]\n", cor_test$conf.int[1], cor_test$conf.int[2]))

  # Create scatterplot with R² in subtitle
  similarity_plot <- ggplot2::ggplot(similarity_influence_offdiag,
                                     ggplot2::aes(x = cosine_similarity, y = influence)) +
    ggplot2::geom_point(alpha = 1, size = 2) +
    ggplot2::geom_smooth(method = "lm", color = paper.cols[["highlight"]], fill = "grey50",
                        alpha = 0.2, linewidth = 1) +
    ggplot2::labs(
      x = "Body part similarity\n(cosine similarity of sensory input profiles)",
      y = sprintf("%s\n(sensory → effector influence)", inf.metric),
      title = "Influence vs effector body part similarity",
      subtitle = sprintf("r = %.3f, R² = %.3f, p = %.3e",
                        cor_test$estimate, r_squared, cor_test$p.value)
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(color = "grey30")
    )

  # Save plot
  ggplot2::ggsave(
    file.path(banc.fig2.path.heatmap,
              sprintf("%s_body_part_similarity_scatterplot.pdf", inf.metric)),
    similarity_plot,
    width = 8,
    height = 6,
    dpi = 300
  )

  # Save data
  write.csv(
    similarity_influence_data,
    file.path(banc.fig2.path.heatmap,
              sprintf("%s_body_part_similarity_data.csv", inf.metric)),
    row.names = FALSE
  )

  # Save cosine similarity matrix
  write.csv(
    cosine_similarity_matrix,
    file.path(banc.fig2.path.heatmap,
              sprintf("%s_effector_cosine_similarity_matrix.csv", inf.metric)),
    row.names = TRUE
  )

  # Save test results
  writeLines(
    c("Body-part relatedness analysis: Influence vs effector body part similarity",
      "",
      sprintf("Metric: %s", inf.metric),
      sprintf("Number of body parts: %d", n_bp),
      sprintf("Total values analyzed: %d", nrow(similarity_influence_data)),
      sprintf("  Same body part (diagonal): %d", sum(similarity_influence_data$is_same_bp)),
      sprintf("  Different body parts (off-diagonal): %d", sum(!similarity_influence_data$is_same_bp)),
      "",
      sprintf("Pearson correlation test:"),
      sprintf("  Correlation coefficient (r): %.4f", cor_test$estimate),
      sprintf("  R²: %.4f", r_squared),
      sprintf("  p-value: %.3e", cor_test$p.value),
      sprintf("  95%% CI: [%.4f, %.4f]", cor_test$conf.int[1], cor_test$conf.int[2])
    ),
    file.path(banc.fig2.path.heatmap,
              sprintf("%s_body_part_similarity_test.txt", inf.metric))
  )

  # Same vs Different Body Part Analysis
  # Test claim: "Most groups of effector neurons receive their strongest influence
  # from sensors in the same body part"
  # Extract on-diagonal values (same body part)
  diagonal_values <- numeric(n_bp)
  diagonal_names <- character(n_bp)
  for (i in 1:n_bp) {
    diagonal_values[i] <- paired_matrix[i, i]
    diagonal_names[i] <- bp_names[i]
  }

  # Off-diagonal values from similarity_influence_data (different body parts only)
  off_diagonal_values <- similarity_influence_data$influence[!similarity_influence_data$is_same_bp]

  cat(sprintf("On-diagonal values (same body part): %d\n", length(diagonal_values)))
  cat(sprintf("  Mean: %.2f, Median: %.2f, SD: %.2f\n",
              mean(diagonal_values, na.rm = TRUE),
              median(diagonal_values, na.rm = TRUE),
              sd(diagonal_values, na.rm = TRUE)))
  cat(sprintf("  Range: %.2f to %.2f\n",
              min(diagonal_values, na.rm = TRUE),
              max(diagonal_values, na.rm = TRUE)))

  cat(sprintf("\nOff-diagonal values (different body parts): %d\n", length(off_diagonal_values)))
  cat(sprintf("  Mean: %.2f, Median: %.2f, SD: %.2f\n",
              mean(off_diagonal_values, na.rm = TRUE),
              median(off_diagonal_values, na.rm = TRUE),
              sd(off_diagonal_values, na.rm = TRUE)))
  cat(sprintf("  Range: %.2f to %.2f\n",
              min(off_diagonal_values, na.rm = TRUE),
              max(off_diagonal_values, na.rm = TRUE)))

  # Statistical test: Wilcoxon rank-sum test (non-parametric, appropriate for small sample sizes)
  diagonal_test <- wilcox.test(diagonal_values, off_diagonal_values,
                                alternative = "greater")  # Test if diagonal > off-diagonal
  cat("\nWilcoxon rank-sum test (one-sided: on-diagonal > off-diagonal):\n")
  cat(sprintf("  W statistic: %.1f\n", diagonal_test$statistic))
  cat(sprintf("  p-value: %.3e\n", diagonal_test$p.value))

  # Calculate effect size (rank-biserial correlation)
  # For Wilcoxon test: r = 1 - (2*U)/(n1*n2), where U = W
  n1 <- length(diagonal_values)
  n2 <- length(off_diagonal_values)
  r_rb <- 1 - (2 * diagonal_test$statistic) / (n1 * n2)
  cat(sprintf("  Effect size (rank-biserial r): %.3f\n", r_rb))

  # Create comparison boxplot
  comparison_data <- data.frame(
    group = c(rep("Same body part\n(on-diagonal)", n1),
              rep("Different body parts\n(off-diagonal)", n2)),
    influence = c(diagonal_values, off_diagonal_values),
    stringsAsFactors = FALSE
  )
  comparison_data$group <- factor(comparison_data$group,
                                  levels = c("Same body part\n(on-diagonal)",
                                           "Different body parts\n(off-diagonal)"))

  comparison_plot <- ggplot2::ggplot(comparison_data,
                                     ggplot2::aes(x = group, y = influence, fill = group)) +
    ggplot2::geom_boxplot(outlier.size = 1, outlier.alpha = 0.5, alpha = 0.7) +
    ggplot2::geom_jitter(width = 0.1, alpha = 0.3, size = 1.5) +
    ggplot2::scale_fill_manual(values = c("Same body part\n(on-diagonal)" = "#E31A1C",
                                         "Different body parts\n(off-diagonal)" = "grey60"),
                              guide = "none") +
    ggplot2::labs(
      x = NULL,
      y = sprintf("%s\n(sensory → effector influence)", inf.metric),
      title = "Same vs Different Body Part Influence",
      subtitle = sprintf("Wilcoxon test: p = %s, r = %.3f", fmt_p_value(diagonal_test$p.value), r_rb)
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = 10),
      plot.title = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(color = "grey30")
    )

  # Save plot
  ggplot2::ggsave(
    file.path(banc.fig2.path.heatmap,
              sprintf("%s_same_vs_different_body_part_boxplot.pdf", inf.metric)),
    comparison_plot,
    width = 7,
    height = 6,
    dpi = 300
  )

  # Save data
  comparison_summary <- data.frame(
    body_part = bp_names,
    diagonal_influence = diagonal_values,
    stringsAsFactors = FALSE
  )
  write.csv(
    comparison_summary,
    file.path(banc.fig2.path.heatmap,
              sprintf("%s_same_vs_different_body_part_data.csv", inf.metric)),
    row.names = FALSE
  )

  # Effect size interpretation
  effect_mag <- dplyr::case_when(
    abs(r_rb) < 0.1 ~ "negligible",
    abs(r_rb) < 0.3 ~ "small",
    abs(r_rb) < 0.5 ~ "medium",
    TRUE             ~ "large"
  )

  # Legend-style statement
  legend_stmt <- sprintf(
    paste0("A one-sided Wilcoxon rank-sum test showed that same body part (on-diagonal) ",
           "sensory-to-effector influence was significantly greater than different body part ",
           "(off-diagonal) influence (W=%.1f, p=%s, rank-biserial r=%.3f, %s effect). ",
           "On-diagonal median=%.2f (n=%d); off-diagonal median=%.2f (n=%d)."),
    diagonal_test$statistic, fmt_p_value(diagonal_test$p.value), r_rb, effect_mag,
    median(diagonal_values, na.rm = TRUE), n1,
    median(off_diagonal_values, na.rm = TRUE), n2
  )

  # Save test results
  test_out_path <- file.path(banc.fig2.path.heatmap,
                             sprintf("%s_same_vs_different_body_part_test.txt", inf.metric))
  writeLines(
    c("Same vs Different Body Part Analysis",
      strrep("=", 70),
      sprintf("Date: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
      sprintf("Metric: %s | Body parts: %d", inf.metric, n_bp),
      sprintf("Test: Wilcoxon rank-sum (one-sided, on-diagonal > off-diagonal)"),
      sprintf("Note: single test, no multiple comparisons correction needed"),
      "",
      "Group summaries",
      strrep("-", 70),
      sprintf("On-diagonal (same body part):      n=%d, median=%.2f, IQR=%.2f, range=[%.2f, %.2f]",
              n1, median(diagonal_values, na.rm=TRUE), IQR(diagonal_values, na.rm=TRUE),
              min(diagonal_values, na.rm=TRUE), max(diagonal_values, na.rm=TRUE)),
      sprintf("Off-diagonal (different body part): n=%d, median=%.2f, IQR=%.2f, range=[%.2f, %.2f]",
              n2, median(off_diagonal_values, na.rm=TRUE), IQR(off_diagonal_values, na.rm=TRUE),
              min(off_diagonal_values, na.rm=TRUE), max(off_diagonal_values, na.rm=TRUE)),
      "",
      "Test results",
      strrep("-", 70),
      sprintf("W = %.1f", diagonal_test$statistic),
      sprintf("p = %s", fmt_p_value(diagonal_test$p.value)),
      sprintf("Rank-biserial r = %.3f (%s effect)", r_rb, effect_mag),
      "",
      strrep("=", 70),
      "FIGURE LEGEND (copy-paste ready)",
      strrep("=", 70),
      legend_stmt,
      strrep("=", 70)
    ),
    test_out_path
  )

  # Transpose paired_matrix so body parts are rows (for UMAP)
  # Each row = one body part, columns = sensory inputs from other body parts
  umap_input <- t(paired_matrix)

  # Run UMAP
  set.seed(42)
  umap_result <- uwot::umap(
    umap_input,
    n_neighbors = min(5, nrow(umap_input) - 1),  # Adjust for small dataset
    min_dist = 0.3,
    metric = "euclidean",
    n_components = 2,
    verbose = FALSE
  )

  # Create data frame for plotting
  umap_df <- data.frame(
    body_part = rownames(umap_input),
    UMAP1 = umap_result[, 1],
    UMAP2 = umap_result[, 2],
    stringsAsFactors = FALSE
  )

  # Add body part colors from paper.cols
  umap_df$color <- sapply(umap_df$body_part, function(bp) {
    if (bp %in% names(paper.cols)) {
      return(paper.cols[[bp]])
    } else {
      return("grey50")  # Default color if not found
    }
  })

  # Create UMAP plot
  umap_plot <- ggplot2::ggplot(umap_df,
                                ggplot2::aes(x = UMAP1, y = UMAP2,
                                            color = I(color), label = body_part)) +
    ggplot2::geom_point(size = 10, alpha = 0.8) +
    ggrepel::geom_text_repel(
      size = 8,
      max.overlaps = 20,
      box.padding = 0.5,
      point.padding = 0.5,
      segment.color = "grey50",
      segment.size = 0.3
    ) +
    ggplot2::labs(
      x = "UMAP 1",
      y = "UMAP 2",
      title = "UMAP of effector body parts",
      subtitle = "Based on sensory input profiles"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),  # Remove all grid lines
      plot.title = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(color = "grey30"),
      legend.position = "none"
    )

  # Save UMAP plot
  ggplot2::ggsave(
    file.path(banc.fig2.path.heatmap,
              sprintf("%s_effector_body_parts_umap.pdf", inf.metric)),
    umap_plot,
    width = 8,
    height = 7,
    dpi = 300
  )

  # Save UMAP coordinates
  write.csv(
    umap_df,
    file.path(banc.fig2.path.heatmap,
              sprintf("%s_effector_body_parts_umap_coords.csv", inf.metric)),
    row.names = FALSE
  )

  # Statistical test
  df_raw <- influence.sensors.db.orig %>%
    dplyr::select(source=seed, target, value = influence_original)
  write_anova_summary(df_raw,file.path(banc.fig2.path.heatmap,"sensors_to_effector_stats.txt"))
  
  # unpaired
  col.order.hm <- intersect(unpaired.in.bps, colnames(reordered_matrix))
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
    filename = file.path(banc.fig2.path.heatmap, sprintf("%s_unpaired_sensors_to_effectors_heatmap.pdf",inf.metric))
  )
}

###########################
### Short ranged effects ###
###########################

# Params
inf.metric <- "influence"
eps_L     <- 1e-12
eps_share <- 1e-6
eff_id_col <- "id"

# ============================================================
# Combined locality and unmatchable routing (final, clean)
# ============================================================
eps_L     <- if (exists("eps_L")) eps_L else 1e-12
eps_share <- if (exists("eps_share")) eps_share else 1e-6

# Use your highlight color from paper.cols
highlight.col <- if ("highlight" %in% names(paper.cols)) {
  paper.cols[["highlight"]]
} else "#b22222"

# ------------------------------
# Build sensor→effector matrix
# ------------------------------
dat_long <- influence.sensors.db %>%
  mutate(
    seed_group = case_when(seed %in% names(seed.map) ~ seed.map[seed],
                           TRUE ~ seed),
    tgt_group  = case_when(target %in% names(target.map) ~ target.map[target],
                           TRUE ~ target)
  ) %>%
  filter(!is.na(seed_group), !is.na(tgt_group)) %>%
  filter(seed_group %in% col.order, tgt_group %in% row.order)

if (!inf.metric %in% names(dat_long)) {
  stop(sprintf("Column '%s' not found in influence.sensors.db.", inf.metric))
}

hm <- reshape2::acast(
  data = dat_long,
  formula = tgt_group ~ seed_group,
  value.var = inf.metric,
  fun.aggregate = function(x) mean(x, na.rm = TRUE)
)
hm[is.na(hm)] <- 0
hm[is.infinite(hm)] <- 0

hm <- hm[intersect(row.order, rownames(hm)), intersect(col.order, colnames(hm)), drop = FALSE]

# Row share (sensor share per effector)
row_sums <- pmax(rowSums(hm), eps_share)
row_share <- hm / row_sums

# Column share (effector share per sensor)
col_sums <- pmax(colSums(hm), eps_share)
col_share <- sweep(hm, 2, col_sums, "/")

# ------------------------------
# Matched effector locality (L)
# ------------------------------
rows_matched <- intersect(rownames(hm), colnames(hm))

matched_df <- lapply(rows_matched, function(rn) {
  shares <- row_share[rn, ]
  same_share <- if (rn %in% names(shares)) shares[[rn]] else NA_real_
  other_shares <- shares[setdiff(names(shares), rn)]
  best_other <- if (length(other_shares)) max(other_shares, na.rm = TRUE) else NA_real_
  
  ord <- sort(shares, decreasing = TRUE)
  top1_name <- names(ord)[1]; top1_share <- as.numeric(ord[1])
  top2_name <- if (length(ord) >= 2) names(ord)[2] else NA_character_
  top2_share <- if (length(ord) >= 2) as.numeric(ord[2]) else NA_real_
  
  tibble(
    facet       = "Matched effector locality",
    category    = rn,
    score       = log2((same_share + eps_L) / (best_other + eps_L)),
    #score       = (same_share) / (best_other),
    top1_name   = top1_name,  top1_share = top1_share,
    top2_name   = top2_name,  top2_share = top2_share,
    top1_is_matched = rn == top1_name,
    top2_is_matched = rn == top2_name
  )
}) %>% bind_rows()

# ------------------------------
# Unmatchable sensor routing (D)
# ------------------------------
cols_unmatchable <- setdiff(colnames(hm), rownames(hm))
unmatch_df <- lapply(cols_unmatchable, function(cn) {
  shares <- col_share[, cn]
  shares <- sort(shares, decreasing = TRUE, na.last = NA)
  top1_name <- names(shares)[1]; top1_share <- as.numeric(shares[1])
  top2_name <- if (length(shares) >= 2) names(shares)[2] else NA_character_
  top2_share <- if (length(shares) >= 2) as.numeric(shares[2]) else NA_real_
  
  tibble(
    facet       = "Unmatchable sensor routing",
    category    = cn,
    score       = log2((top1_share + eps_L) / (top2_share + eps_L)),
    #score       = (top1_share) / (top2_share),
    top1_name   = top1_name,  top1_share = top1_share,
    top2_name   = top2_name,  top2_share = top2_share,
    top1_is_matched = FALSE,
    top2_is_matched = FALSE
  )
}) %>% bind_rows()

# ------------------------------
# Permutation ANOVA (row_share ~ local)
# ------------------------------
if (length(rows_matched) > 1) {
  hm_loc <- hm[rows_matched, , drop = FALSE]
  sh_loc <- sweep(hm_loc, 1, pmax(rowSums(hm_loc), eps_share), "/")
  local_mat <- outer(rownames(hm_loc), colnames(hm_loc), "==")
  df_pairs <- tibble(
    share = as.numeric(sh_loc),
    local = factor(as.numeric(local_mat), levels = c(0, 1), labels = c("other", "local"))
  )
  
  aov_obs <- aov(share ~ local, data = df_pairs)
  F_obs <- as.numeric(summary(aov_obs)[[1]][["F value"]][1])
  set.seed(1)
  B_perm <- 999
  F_null <- replicate(B_perm, {
    perm_cols <- sample(colnames(hm_loc))
    local_perm <- factor(as.numeric(outer(rownames(hm_loc), perm_cols, "==")),
                         levels = c(0, 1), labels = c("other", "local"))
    dfp <- tibble(share = as.numeric(sh_loc), local = local_perm)
    as.numeric(summary(aov(share ~ local, data = dfp))[[1]][["F value"]][1])
  })
  p_perm <- (sum(F_null >= F_obs) + 1) / (B_perm + 1)
  matched_title <- glue("Matched effector locality (ANOVA: F={format(F_obs, digits=3)}, p_perm={format(p_perm, digits=3)})")
} else {
  matched_title <- "Matched effector locality"
}

# ------------------------------
# Combine data for plotting
# ------------------------------
fmt_lab2 <- function(nm, sh) {
  #ifelse(is.na(nm) | is.na(sh), "", sprintf("%s %s", nm, scales::percent(sh, accuracy = 0.01)))
  ifelse(is.na(nm) | is.na(sh), "", nm)
}

plot_df <- bind_rows(matched_df, unmatch_df) %>%
  mutate(
    facet = recode(facet,
                   "Matched effector locality" = matched_title,
                   .default = facet),
    category_lab = gsub("_", " ", category),
    
    # Order matched label first, highlight if paired
    is_matched_facet = grepl("^Matched effector locality", facet),
    top1_is_pair = is_matched_facet & top1_is_matched,
    top2_is_pair = is_matched_facet & top2_is_matched,
    
    # Select which label goes first (paired first)
    line1_name = case_when(top1_is_pair ~ top1_name,
                           top2_is_pair ~ top2_name,
                           TRUE ~ top1_name),
    line1_share = case_when(top1_is_pair ~ top1_share,
                            top2_is_pair ~ top2_share,
                            TRUE ~ top1_share),
    line2_name = case_when(top1_is_pair ~ top2_name,
                           top2_is_pair ~ top1_name,
                           TRUE ~ top2_name),
    line2_share = case_when(top1_is_pair ~ top2_share,
                            top2_is_pair ~ top1_share,
                            TRUE ~ top2_share),
    
    line1_col = case_when(top1_is_pair | top2_is_pair ~ paper.cols[["highlight"]],
                          TRUE ~ "grey40"),
    line2_col = "grey60",
    label_line1 = fmt_lab2(line1_name, line1_share),
    label_line2 = fmt_lab2(line2_name, line2_share),
    
    pad = 0.5,
    y_line1 = score + ifelse(score >= 0,  pad*1, -pad*1),
    y_line2 = score + ifelse(score >= 0,  pad*2, -pad*2)
  )

# Order by score within each facet
levs_by_facet <- plot_df %>%
  group_by(facet) %>%
  arrange(desc(score), .by_group = TRUE) %>%
  summarise(levs = list(unique(category_lab)), .groups = "drop")
levels_all <- unlist(levs_by_facet$levs, use.names = FALSE)
plot_df <- plot_df %>% mutate(category_lab = factor(category_lab, levels = levels_all))

# ------------------------------
# Plot
# ------------------------------
y_min <- min(plot_df$score, na.rm = TRUE)
y_max <- max(plot_df$score, na.rm = TRUE)
ylim  <- c(y_min - 0.05*(y_max - y_min), y_max + 0.05*(y_max - y_min))

p_facets <- ggplot(plot_df, aes(x = category_lab, y = score)) +
  geom_segment(aes(xend = category_lab, y = 0, yend = score),
               linewidth = 0.5, lineend = "round", colour = "black") +
  geom_point(size = 2.2, colour = "black") +
  geom_text(aes(y = y_line1, 
                label = label_line1, 
                colour = line1_col),
            size = 4, 
            show.legend = FALSE) +
  geom_text(aes(y = y_line2, 
                label = label_line2),
            colour = "grey60", 
            size = 4, 
            lineheight = 0.95, 
            show.legend = FALSE) +
  scale_colour_identity() +
  geom_hline(yintercept = 0, linetype = 1, linewidth = 1, color = "black") +
  facet_grid(. ~ facet, scales = "free_x", space = "free_x") +
  labs(x = NULL, y = "locality / routing score") +
  coord_cartesian(ylim = ylim, clip = "off") +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x        = element_text(angle = 90, vjust = 0.5, hjust = 1)
  ) 

# Plot and save
print(p_facets)
ggplot2::ggsave(
  file.path(banc.fig2.supp.path, sprintf("%s_locality_and_unmatchable_faceted.pdf", inf.metric)),
  p_facets, width = 16, height = 4, dpi = 300
)

###########################
### Long ranged effects ###
###########################

# --- helpers ---------------------------------------------------------------
to_ganglion <- function(x){
  x <- as.character(x)
  dplyr::case_when(
    is.na(x) ~ NA_character_,
    grepl("ventral_nerve_cord|\\bvnc\\b", x) ~ "ventral_nerve_cord",
    grepl("central_brain|brain|midbrain|optic_lobe", x) ~ "central_brain",
    TRUE ~ x
  )
}

# --- seed profiles (JOIN BY ORIGINAL seed ID) ------------------------------
seed_region_df <- banc.sens.meta %>%
  dplyr::distinct(cell_type, region, nerve, seed_02, .keep_all = TRUE) %>%
  dplyr::transmute(
    seed        = seed_02,
    seed_region = to_ganglion(region),
    seed_nerve  = nerve
  ) %>%
  dplyr::filter(!is.na(seed), !is.na(seed_region), !is.na(seed_nerve))

# --- base table with effector annotations ----------------------------------
base_tbl <- influence.sensors.db.orig %>%
  dplyr::left_join(banc.meta %>% dplyr::distinct(id, .keep_all = TRUE), by = "id") %>%
  dplyr::filter(!is.na(region)) %>%
  dplyr::mutate(
    # normalise brain labels
    region     = dplyr::case_when(grepl("brain", region) ~ "central_brain", TRUE ~ region),
    eff_region = to_ganglion(region),
    eff_nerve  = nerve,
    seed       = seed,      # original seed id (seed_02/seed_07 upstream)
    target     = id,
    # effector body-part from banc.meta, optionally simplified via target.map
    body_part_target = dplyr::case_when(
      !is.na(body_part_effector) & body_part_effector %in% names(target.map) ~ target.map[body_part_effector],
      !is.na(body_part_effector) ~ body_part_effector,
      TRUE ~ NA_character_
    ),
    # sensor "body-part" tag used for the short-range rule
    seed_body = dplyr::case_when(seed %in% names(seed.map) ~ seed.map[seed], TRUE ~ seed)
  ) %>%
  dplyr::filter(!is.na(seed), !is.na(target), !is.na(body_part_target), body_part_target != "unknown") %>%
  dplyr::left_join(seed_region_df, by = "seed")

# --- classify range per original sensor link -------------------------------
wrk_fine <- base_tbl %>%
  dplyr::filter(!is.na(eff_region), !is.na(seed_region)) %>%
  dplyr::mutate(
    short_flag = (!is.na(seed_body) & seed_body == body_part_target) |
      (!is.na(eff_nerve)  & !is.na(seed_nerve) & eff_nerve == seed_nerve),
    range_class = dplyr::case_when(
      short_flag ~ "short",
      seed_region == eff_region ~ "medium",
      seed_region != eff_region ~ "long",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::filter(!is.na(range_class)) %>%
  dplyr::mutate(range_class = factor(range_class, levels = c("short","medium","long")))

# Choose a linear influence column (pooled sums should use linear)
lin_col <- if ("influence_original" %in% names(wrk_fine)) {
  "influence_original"
} else if ("influence" %in% names(wrk_fine)) {
  "influence"
} else {
  stop("No linear influence column found in wrk_fine (need 'influence_original' or 'influence').")
}

# Ensure expected factor order for range_class
wrk_fine <- wrk_fine %>%
  mutate(range_class = factor(range_class, levels = c("short","medium","long")))

# Pooled totals per body part × range (micro-average across effectors)
bp_range_totals <- wrk_fine %>%
  filter(!is.na(body_part_target), !is.na(range_class)) %>%
  group_by(body_part_target, range_class) %>%
  summarise(total_lin = sum(.data[[lin_col]], na.rm = TRUE), .groups = "drop") %>%
  # make sure every body part has all 3 range levels (fill missing with 0)
  complete(body_part_target, range_class, fill = list(total_lin = 0))

# Convert to proportions within each body part
bp_range_props <- bp_range_totals %>%
  group_by(body_part_target) %>%
  mutate(total_all = sum(total_lin, na.rm = TRUE),
         prop = ifelse(total_all > 0, total_lin / total_all, NA_real_)) %>%
  ungroup()

# Exclude body parts with sparse or ambiguous data
exclude_bps <- c("thoracic-abdominal", "ureter")
bp_range_props <- bp_range_props %>%
  dplyr::filter(!body_part_target %in% exclude_bps)

# Order body parts by long-range share (descending)
long_order <- bp_range_props %>%
  filter(range_class == "long") %>%
  transmute(body_part_target, long_share = prop) %>%
  arrange(desc(replace_na(long_share, 0))) %>%
  pull(body_part_target)

bp_range_props <- bp_range_props %>%
  mutate(body_part_target = factor(body_part_target, levels = unique(long_order)))

# Colors from your palette
range_cols <- paper.cols[c("short","medium","long")]

# Plot (horizontal stacked bars)
p_stack <- ggplot(bp_range_props, aes(x = body_part_target, y = prop, fill = range_class)) +
  geom_col(width = 0.85, color = "white", linewidth = 0.2) +
  scale_fill_manual(values = range_cols, name = "range") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(x = "effector body part", y = "share of total influence") +
  theme_minimal(base_size = 11) +
  theme(     axis.text.x  = element_text(angle = 90, vjust = 0.5, hjust = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3),
    legend.position = "right"
  )

# Save
print(p_stack)
ggsave(
  filename = file.path(banc.fig2.supp.path, sprintf("%s_range_share_stacked_by_bodypart.pdf", inf.metric)),
  plot = p_stack,
  height = 4, width = 7, dpi = 300
)

##############################################################
### Sensory sub-class to effector cell type by body part   ###
##############################################################

# Prepare data: sensory cell types × effector cell types with body part AND region annotations
influence_celltype_to_celltype <- influence.sensors.db.orig %>%
  # Join sensory metadata for seed body parts AND region
  dplyr::left_join(banc.sens.meta %>%
                     dplyr::distinct(seed_02, body_part_sensory, region) %>%
                     dplyr::rename(seed_region = region),
                   by = c("seed" = "seed_02")) %>%
  # Join effector metadata for target cell types, body parts AND region
  dplyr::left_join(banc.meta %>%
                     dplyr::distinct(root_id, cell_type, body_part_effector, region) %>%
                     dplyr::rename(target_cell_type = cell_type,
                                   target_region = region),
                   by = c("id" = "root_id")) %>%
  # Apply body part mappings
  dplyr::mutate(
    seed_body_part = dplyr::case_when(
      seed %in% names(seed.map) ~ seed.map[seed],
      TRUE ~ seed
    ),
    target_body_part = dplyr::case_when(
      body_part_effector %in% names(target.map) ~ target.map[body_part_effector],
      TRUE ~ body_part_effector
    )
  ) %>%
  dplyr::filter(!is.na(seed_body_part), !is.na(target_body_part), !is.na(target_cell_type)) %>%
  dplyr::filter(seed_body_part %in% col.order, target_body_part %in% row.order) %>%
  # Create labels (include region since cell types can span regions)
  # Clean sensory names: replace underscore with space
  dplyr::mutate(
    seed_label_clean = gsub("_", " ", seed),
    seed_label = paste0(seed_label_clean, " [", seed_region, "]"),
    target_label = paste0(target_cell_type, " [", target_body_part, ", ", target_region, "]")
  )

# Aggregate: median influence per sensory cell type × effector cell type pair
# Include region information in grouping
influence_seed_to_target_ct <- influence_celltype_to_celltype %>%
  dplyr::group_by(seed, seed_body_part, seed_region, seed_label,
                  target_cell_type, target_body_part, target_region, target_label) %>%
  dplyr::summarise(
    median_influence = median(influence_original, na.rm = TRUE),
    n_obs = dplyr::n(),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    influence_log_adj = log(median_influence) + 24,
    influence_log_adj = ifelse(is.infinite(influence_log_adj) | is.nan(influence_log_adj),
                               0, influence_log_adj)
  ) %>%
  dplyr::group_by(target_body_part) %>%
  dplyr::mutate(
    influence_log_minmax = (influence_log_adj - min(influence_log_adj, na.rm = TRUE)) /
      (max(influence_log_adj, na.rm = TRUE) - min(influence_log_adj, na.rm = TRUE))
  ) %>%
  dplyr::ungroup()

cat("  Sensory to effector cell type aggregation complete\n")
cat("  Unique source cell types:", n_distinct(influence_seed_to_target_ct$seed), "\n")
cat("  Unique target cell types:", n_distinct(influence_seed_to_target_ct$target_cell_type), "\n")
cat("  Unique source-target pairs:", nrow(influence_seed_to_target_ct), "\n\n")

# Create matrix for heatmap (rows = sensory cell types, columns = effector cell types)
heatmap_matrix_ct_to_ct <- reshape2::acast(
  data = influence_seed_to_target_ct,
  formula = seed_label ~ target_label,
  value.var = "influence_log_adj",
  fun.aggregate = function(x) mean(x, na.rm = TRUE)
)
heatmap_matrix_ct_to_ct[is.na(heatmap_matrix_ct_to_ct)] <- 0
heatmap_matrix_ct_to_ct[is.infinite(heatmap_matrix_ct_to_ct)] <- 0

# Create row annotations (sensory body parts AND region)
row_annotation_sens <- influence_seed_to_target_ct %>%
  dplyr::distinct(seed_label, seed_body_part, seed_region) %>%
  dplyr::filter(seed_label %in% rownames(heatmap_matrix_ct_to_ct)) %>%
  tibble::column_to_rownames("seed_label") %>%
  dplyr::rename(body_part = seed_body_part, region = seed_region)

# Create column annotations (effector body parts AND region)
col_annotation_eff <- influence_seed_to_target_ct %>%
  dplyr::distinct(target_label, target_body_part, target_region) %>%
  dplyr::filter(target_label %in% colnames(heatmap_matrix_ct_to_ct)) %>%
  tibble::column_to_rownames("target_label") %>%
  dplyr::rename(body_part = target_body_part, region = target_region)

# Filter matrix to only include labeled rows/cols
heatmap_matrix_ct_to_ct <- heatmap_matrix_ct_to_ct[
  rownames(heatmap_matrix_ct_to_ct) %in% rownames(row_annotation_sens),
  colnames(heatmap_matrix_ct_to_ct) %in% rownames(col_annotation_eff),
  drop = FALSE
]

# Order rows and columns by REGION first, then body part, then cluster within body part
# Define region order
region_order <- c("central_brain", "optic_lobe", "ventral_nerve_cord")

# Rows (sensory): order by region → body part → cluster within body part
row_order_by_region <- lapply(region_order, function(reg) {
  # For this region, get all body parts
  bps_in_region <- unique(row_annotation_sens$body_part[row_annotation_sens$region == reg])
  bps_in_region <- bps_in_region[bps_in_region %in% col.order]
  bps_in_region <- bps_in_region[order(match(bps_in_region, col.order))]

  # For each body part in this region, cluster within
  lapply(bps_in_region, function(bp) {
    rows_in_bp_reg <- rownames(row_annotation_sens)[
      row_annotation_sens$body_part == bp & row_annotation_sens$region == reg
    ]
    if (length(rows_in_bp_reg) > 1) {
      bp_data <- heatmap_matrix_ct_to_ct[rows_in_bp_reg, , drop = FALSE]
      if (nrow(bp_data) > 1 && sum(!is.na(bp_data)) > 0) {
        hc <- hclust(dist(bp_data), method = "ward.D2")
        return(rows_in_bp_reg[hc$order])
      }
    }
    return(rows_in_bp_reg)
  }) %>% unlist()
}) %>% unlist()

row_order_by_region <- row_order_by_region[row_order_by_region %in% rownames(heatmap_matrix_ct_to_ct)]

# Columns (effector): order by region → body part → cluster within body part
col_order_by_region <- lapply(region_order, function(reg) {
  # For this region, get all body parts
  bps_in_region <- unique(col_annotation_eff$body_part[col_annotation_eff$region == reg])
  bps_in_region <- bps_in_region[bps_in_region %in% row.order]
  bps_in_region <- bps_in_region[order(match(bps_in_region, row.order))]

  # For each body part in this region, cluster within
  lapply(bps_in_region, function(bp) {
    cols_in_bp_reg <- rownames(col_annotation_eff)[
      col_annotation_eff$body_part == bp & col_annotation_eff$region == reg
    ]
    if (length(cols_in_bp_reg) > 1) {
      bp_data <- heatmap_matrix_ct_to_ct[, cols_in_bp_reg, drop = FALSE]
      if (ncol(bp_data) > 1 && sum(!is.na(bp_data)) > 0) {
        hc <- hclust(dist(t(bp_data)), method = "ward.D2")
        return(cols_in_bp_reg[hc$order])
      }
    }
    return(cols_in_bp_reg)
  }) %>% unlist()
}) %>% unlist()

col_order_by_region <- col_order_by_region[col_order_by_region %in% colnames(heatmap_matrix_ct_to_ct)]

# Reorder matrix by region-based ordering
heatmap_matrix_ct_to_ct <- heatmap_matrix_ct_to_ct[row_order_by_region, col_order_by_region, drop = FALSE]
row_annotation_sens <- row_annotation_sens[row_order_by_region, , drop = FALSE]
col_annotation_eff <- col_annotation_eff[col_order_by_region, , drop = FALSE]

# Define colors
scaled_breaks_ct <- seq(
  quantile(heatmap_matrix_ct_to_ct, 0.01, na.rm = TRUE),
  quantile(heatmap_matrix_ct_to_ct, 0.999, na.rm = TRUE),
  length.out = n_breaks
)
scaled_palette_ct <- colorRampPalette(c("#1f4e79", "#4a90a4", "#7ba7bc", "#a67c8a", "#c4967d", "#b22222"))(n_breaks - 1)

# Create annotation colors for both axes (body part AND region)
all_body_parts_both <- unique(c(row_annotation_sens$body_part, col_annotation_eff$body_part))
existing_colors <- paper.cols[names(paper.cols) %in% all_body_parts_both]
missing_body_parts <- setdiff(all_body_parts_both, names(existing_colors))

if (length(missing_body_parts) > 0) {
  n_missing <- length(missing_body_parts)
  new_colors <- colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(n_missing)
  names(new_colors) <- missing_body_parts
  all_colors_bp <- c(existing_colors, new_colors)
} else {
  all_colors_bp <- existing_colors
}

# Define region colors from paper.cols
all_regions <- unique(c(row_annotation_sens$region, col_annotation_eff$region))
region_colors <- paper.cols[names(paper.cols) %in% all_regions]

annotation_colors_both <- list(
  body_part = all_colors_bp,
  region = region_colors
)

# Calculate gap positions to separate regions
# Rows (sensory): find where region changes
row_regions <- row_annotation_sens$region
row_region_changes <- which(diff(as.numeric(factor(row_regions, levels = region_order))) != 0)

# Columns (effector): find where region changes
col_regions <- col_annotation_eff$region
col_region_changes <- which(diff(as.numeric(factor(col_regions, levels = region_order))) != 0)

# Generate heatmap (tall to show sensory names on rows)
pheatmap::pheatmap(
  heatmap_matrix_ct_to_ct,
  color = scaled_palette_ct,
  breaks = scaled_breaks_ct,
  annotation_row = row_annotation_sens,
  annotation_col = col_annotation_eff,
  annotation_colors = annotation_colors_both,
  gaps_row = row_region_changes,  # Add gaps between regions
  gaps_col = col_region_changes,  # Add gaps between regions
  clustering_method = "ward.D2",
  cluster_rows = FALSE,  # Already ordered by region/body part with internal clustering
  cluster_cols = FALSE,  # Already ordered by region/body part with internal clustering
  treeheight_row = 0,
  treeheight_col = 0,
  show_rownames = TRUE,   # Show sensory names
  show_colnames = FALSE,  # Too many effector names
  fontsize_row = 5,       # Readable sensory names
  fontsize_col = 4,
  cellwidth = 3,
  cellheight = 6,  # 2x larger for better visibility
  main = "Sensory sub-class (rows) to effector cell types (columns)",
  annotation_names_col = FALSE,
  annotation_names_row = FALSE,
  filename = file.path(banc.fig2.supp.path, "sensory_sub_class_to_effector_by_body_part.pdf"),
  width = 24,
  height = 80  # Tall to make sensory names legible (2x for 2x cell height)
)

# For each effector body part, test if influence varies by sensory body part
# Using cell_type level data
stats_results_optionA <- list()
for (eff_bp in row.order) {
  # Get data for this effector body part
  eff_data <- influence_seed_to_target_ct %>%
    dplyr::filter(target_body_part == eff_bp) %>%
    dplyr::select(seed, seed_body_part, target_body_part, influence_log_adj)

  if (nrow(eff_data) < 2) next

  # Get unique sensory body parts
  sens_bps <- unique(eff_data$seed_body_part)

  if (length(sens_bps) < 2) next

  # Kruskal-Wallis test
  kw_test <- tryCatch({
    kruskal.test(influence_log_adj ~ seed_body_part, data = eff_data)
  }, error = function(e) NULL)

  if (is.null(kw_test)) next

  # Pairwise Wilcoxon if KW is significant
  pw_test <- NULL
  if (kw_test$p.value < 0.05) {
    pw_test <- tryCatch({
      pairwise.wilcox.test(eff_data$influence_log_adj,
                          eff_data$seed_body_part,
                          p.adjust.method = "holm",
                          exact = FALSE)
    }, error = function(e) NULL)
  }

  stats_results_optionA[[eff_bp]] <- list(
    effector_body_part = eff_bp,
    n_observations = nrow(eff_data),
    n_sensory_body_parts = length(sens_bps),
    sensory_body_parts = sens_bps,
    kw_statistic = kw_test$statistic,
    kw_p_value = kw_test$p.value,
    pairwise_test = pw_test
  )
}

# Save statistical results
stats_summary_optionA <- lapply(names(stats_results_optionA), function(bp) {
  res <- stats_results_optionA[[bp]]
  data.frame(
    effector_body_part = res$effector_body_part,
    n_observations = res$n_observations,
    n_sensory_body_parts = res$n_sensory_body_parts,
    kw_chi_squared = as.numeric(res$kw_statistic),
    kw_p_value = res$kw_p_value,
    kw_significant = res$kw_p_value < 0.05,
    stringsAsFactors = FALSE
  )
}) %>% dplyr::bind_rows()
write.csv(stats_summary_optionA,
          file.path(banc.fig2.supp.path, "sensory_to_effector_celltype_level_stats.csv"),
          row.names = FALSE)

####################################
### Body part level with stats   ###
####################################

# Use existing body part aggregation for visualization
# But for statistics, work with individual neuron-level influence values

# Prepare data: for each effector neuron, get influence from each sensory body part
influence_neuron_level <- influence.sensors.db.orig %>%
  dplyr::left_join(banc.sens.meta %>%
                     dplyr::distinct(seed_02, body_part_sensory),
                   by = c("seed" = "seed_02")) %>%
  dplyr::left_join(banc.eff.meta %>%
                     dplyr::distinct(root_id, body_part_effector),
                   by = c("id" = "root_id")) %>%
  dplyr::mutate(
    # Apply body part mappings
    seed_body_part = dplyr::case_when(
      seed %in% names(seed.map) ~ seed.map[seed],
      TRUE ~ seed
    ),
    target_body_part = dplyr::case_when(
      body_part_effector %in% names(target.map) ~ target.map[body_part_effector],
      TRUE ~ body_part_effector
    ),
    influence_log = log(influence_original + 1)
  ) %>%
  dplyr::filter(!is.na(seed_body_part), !is.na(target_body_part)) %>%
  dplyr::filter(seed_body_part %in% col.order, target_body_part %in% row.order)

stats_results_optionB <- list()
for (eff_bp in row.order) {
  # Get data for this effector body part
  eff_data <- influence_neuron_level %>%
    dplyr::filter(target_body_part == eff_bp) %>%
    dplyr::select(id, seed_body_part, target_body_part, influence_log)

  if (nrow(eff_data) < 2) next

  # Get unique sensory body parts
  sens_bps <- unique(eff_data$seed_body_part)

  if (length(sens_bps) < 2) next

  # Calculate median influence per sensory body part
  medians_per_bp <- eff_data %>%
    dplyr::group_by(seed_body_part) %>%
    dplyr::summarise(
      median_influence = median(influence_log, na.rm = TRUE),
      n = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(median_influence))

  # Kruskal-Wallis test
  kw_test <- tryCatch({
    kruskal.test(influence_log ~ seed_body_part, data = eff_data)
  }, error = function(e) NULL)

  if (is.null(kw_test)) next

  # Pairwise Wilcoxon if KW is significant
  pw_test <- NULL
  pw_matrix <- NULL
  if (kw_test$p.value < 0.05) {
    pw_test <- tryCatch({
      pairwise.wilcox.test(eff_data$influence_log,
                          eff_data$seed_body_part,
                          p.adjust.method = "holm",
                          exact = FALSE)
    }, error = function(e) NULL)

    if (!is.null(pw_test)) {
      pw_matrix <- pw_test$p.value
    }
  }

  stats_results_optionB[[eff_bp]] <- list(
    effector_body_part = eff_bp,
    n_observations = nrow(eff_data),
    n_sensory_body_parts = length(sens_bps),
    sensory_body_parts = sens_bps,
    medians = medians_per_bp,
    kw_statistic = kw_test$statistic,
    kw_p_value = kw_test$p.value,
    pairwise_test = pw_test,
    pairwise_matrix = pw_matrix
  )
}

# Save statistical results with more detail
stats_summary_optionB <- lapply(names(stats_results_optionB), function(bp) {
  res <- stats_results_optionB[[bp]]

  # Get top sensory body part
  top_bp <- res$medians$seed_body_part[1]
  top_median <- res$medians$median_influence[1]

  # Check if top is matched
  is_matched <- (bp == top_bp) && (bp %in% paired.bps)

  data.frame(
    effector_body_part = res$effector_body_part,
    n_observations = res$n_observations,
    n_sensory_body_parts = res$n_sensory_body_parts,
    top_sensory_body_part = top_bp,
    top_median_influence = top_median,
    is_matched = is_matched,
    kw_chi_squared = as.numeric(res$kw_statistic),
    kw_p_value = res$kw_p_value,
    kw_significant = res$kw_p_value < 0.05,
    stringsAsFactors = FALSE
  )
}) %>% dplyr::bind_rows()

write.csv(stats_summary_optionB,
          file.path(banc.fig2.supp.path, "sensory_to_effector_bodypart_level_stats.csv"),
          row.names = FALSE)

# Generate detailed pairwise comparison results for manuscript
pairwise_details <- lapply(names(stats_results_optionB), function(bp) {
  res <- stats_results_optionB[[bp]]

  if (is.null(res$pairwise_matrix)) return(NULL)

  # Convert matrix to long format
  pw_mat <- res$pairwise_matrix
  if (is.null(pw_mat)) return(NULL)

  pw_long <- as.data.frame(as.table(pw_mat), stringsAsFactors = FALSE)
  names(pw_long) <- c("sensory_bp_1", "sensory_bp_2", "p_adj")
  pw_long <- pw_long %>%
    dplyr::filter(!is.na(p_adj)) %>%
    dplyr::mutate(
      effector_body_part = bp,
      significant = p_adj < 0.05
    ) %>%
    dplyr::arrange(p_adj)

  return(pw_long)
}) %>% dplyr::bind_rows()

if (!is.null(pairwise_details) && nrow(pairwise_details) > 0) {
  write.csv(pairwise_details,
            file.path(banc.fig2.supp.path, "sensory_to_effector_bodypart_pairwise_comparisons.csv"),
            row.names = FALSE)
}

# Create summary text for each significant effector body part
manuscript_text_optionB <- lapply(names(stats_results_optionB), function(bp) {
  res <- stats_results_optionB[[bp]]

  if (res$kw_p_value >= 0.05) return(NULL)

  # Format p-value
  fmt_p <- function(p) {
    if (p < 0.001) sprintf("%.2e", p)
    else sprintf("%.3f", signif(p, 3))
  }

  # Get top 3 sensory body parts
  top3 <- head(res$medians, 3)
  top_list <- paste(top3$seed_body_part, collapse = ", ")

  # Get pairwise significant comparisons for top sensory body part vs others
  top_bp <- top3$seed_body_part[1]
  is_matched <- (bp == top_bp) && (bp %in% paired.bps)

  matched_text <- if (is_matched) " (matched)" else ""

  text <- sprintf(
    "%s effectors: Kruskal-Wallis χ²=%.2f, p=%s. Top sensory sources: %s. Highest influence from %s%s.",
    bp, res$kw_statistic, fmt_p(res$kw_p_value), top_list, top_bp, matched_text
  )

  return(text)
}) %>% unlist()

manuscript_text_optionB <- manuscript_text_optionB[!sapply(manuscript_text_optionB, is.null)]

writeLines(manuscript_text_optionB,
           file.path(banc.fig2.supp.path, "sensory_to_effector_bodypart_manuscript_summary.txt"))

##############################################################
### Statistical test: Matched vs Unmatched influence       ##
##############################################################

# Prepare data: label each sensory-effector connection as matched or unmatched
matched_test_data <- influence_seed_to_target_ct %>%
  dplyr::select(seed, seed_body_part, target_body_part, influence_log_adj) %>%
  dplyr::mutate(
    is_matched = (seed_body_part == target_body_part) & (target_body_part %in% paired.bps)
  ) %>%
  dplyr::filter(!is.na(is_matched))

# Test for each effector body part: are matched sensors stronger?
matched_test_results <- list()

for (eff_bp in paired.bps) {  # Only test paired body parts
  bp_data <- matched_test_data %>%
    dplyr::filter(target_body_part == eff_bp)

  if (nrow(bp_data) < 10) next

  matched_vals <- bp_data %>% dplyr::filter(is_matched) %>% dplyr::pull(influence_log_adj)
  unmatched_vals <- bp_data %>% dplyr::filter(!is_matched) %>% dplyr::pull(influence_log_adj)

  if (length(matched_vals) < 2 || length(unmatched_vals) < 2) next

  # Wilcoxon rank-sum test
  wtest <- tryCatch({
    wilcox.test(matched_vals, unmatched_vals, alternative = "greater")
  }, error = function(e) NULL)

  if (is.null(wtest)) next

  # Effect size: rank-biserial correlation
  n1 <- length(matched_vals)
  n2 <- length(unmatched_vals)
  U <- wtest$statistic
  r_rb <- 1 - (2*U) / (n1 * n2)  # Rank-biserial correlation

  matched_test_results[[eff_bp]] <- list(
    effector_bp = eff_bp,
    n_matched = n1,
    n_unmatched = n2,
    median_matched = median(matched_vals, na.rm = TRUE),
    median_unmatched = median(unmatched_vals, na.rm = TRUE),
    mean_matched = mean(matched_vals, na.rm = TRUE),
    mean_unmatched = mean(unmatched_vals, na.rm = TRUE),
    wilcox_statistic = as.numeric(wtest$statistic),
    wilcox_p = wtest$p.value,
    rank_biserial = r_rb
  )
}

# Combine results into dataframe
matched_test_summary <- dplyr::bind_rows(matched_test_results) %>%
  dplyr::mutate(
    wilcox_p_adj = p.adjust(wilcox_p, method = "holm"),
    significant = wilcox_p_adj < 0.05
  ) %>%
  dplyr::arrange(wilcox_p)

# Save results
write.csv(matched_test_summary,
          file.path(banc.fig2.supp.path, "paired_unpaired_influence_test.csv"),
          row.names = FALSE)

# Print summary
cat(sprintf("  Tested %d paired body parts\n", nrow(matched_test_summary)))
cat(sprintf("  Significant (p_adj < 0.05): %d / %d (%.1f%%)\n",
            sum(matched_test_summary$significant),
            nrow(matched_test_summary),
            100 * sum(matched_test_summary$significant) / nrow(matched_test_summary)))
cat(sprintf("  Median effect size (rank-biserial): %.3f\n",
            median(matched_test_summary$rank_biserial, na.rm = TRUE)))

##############################################################
### Matched vs Unmatched influence analysis                ##
##############################################################

# Classify connections as matched (same body part) or unmatched (different body part)
influence_matched_unmatched <- influence_seed_to_target_ct %>%
  dplyr::mutate(
    match_class = dplyr::case_when(
      seed_body_part == target_body_part ~ "matched",
      seed_body_part != target_body_part ~ "unmatched",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::filter(!is.na(match_class)) %>%
  dplyr::filter(!target_body_part %in% exclude_bps) %>%
  dplyr::mutate(match_class = factor(match_class, levels = c("matched", "unmatched")))

# Calculate median and IQR for each effector body part × match class
match_summary <- influence_matched_unmatched %>%
  dplyr::filter(!is.na(target_body_part)) %>%
  dplyr::group_by(target_body_part, match_class) %>%
  dplyr::summarise(
    n = dplyr::n(),
    median_influence = median(influence_log_minmax, na.rm = TRUE),
    q25 = quantile(influence_log_minmax, 0.25, na.rm = TRUE),
    q75 = quantile(influence_log_minmax, 0.75, na.rm = TRUE),
    iqr = IQR(influence_log_minmax, na.rm = TRUE),
    .groups = "drop"
  )

# Statistical tests: Compare matched vs unmatched for each effector body part
match_test_results <- list()
match_test_text <- character()

for (eff_bp in unique(influence_matched_unmatched$target_body_part)) {
  bp_data <- influence_matched_unmatched %>%
    dplyr::filter(target_body_part == eff_bp)

  matched_vals <- bp_data %>% dplyr::filter(match_class == "matched") %>% dplyr::pull(influence_log_minmax)
  unmatched_vals <- bp_data %>% dplyr::filter(match_class == "unmatched") %>% dplyr::pull(influence_log_minmax)

  if (length(matched_vals) < 2 || length(unmatched_vals) < 2) next

  # Wilcoxon test: matched vs unmatched
  wtest <- tryCatch({
    wilcox.test(matched_vals, unmatched_vals, alternative = "greater")
  }, error = function(e) NULL)

  if (!is.null(wtest)) {
    # Calculate effect size (rank-biserial correlation)
    n1 <- length(matched_vals)
    n2 <- length(unmatched_vals)
    U <- wtest$statistic
    r_rb <- 1 - (2*U) / (n1 * n2)

    match_test_results[[eff_bp]] <- list(
      effector_bp = eff_bp,
      n_matched = n1,
      n_unmatched = n2,
      median_matched = median(matched_vals, na.rm = TRUE),
      median_unmatched = median(unmatched_vals, na.rm = TRUE),
      wilcox_p = wtest$p.value,
      rank_biserial = r_rb
    )

  }
}

match_test_summary <- dplyr::bind_rows(match_test_results) %>%
  dplyr::mutate(
    wilcox_p_adj = p.adjust(wilcox_p, method = "holm"),
    significant = wilcox_p_adj < 0.05
  ) %>%
  dplyr::arrange(wilcox_p)

# Build text output using Holm-adjusted p-values (consistent with plot)
match_test_text <- match_test_summary %>%
  dplyr::mutate(
    sig_label = dplyr::case_when(
      wilcox_p_adj < 0.001 ~ "***",
      wilcox_p_adj < 0.01  ~ "**",
      wilcox_p_adj < 0.05  ~ "*",
      TRUE                 ~ "ns"
    ),
    p_formatted = fmt_p_value(wilcox_p_adj),
    line = sprintf("%s: matched (n=%d, median=%.2f) vs unmatched (n=%d, median=%.2f), p_adj=%s %s, r=%.3f",
                   effector_bp, n_matched, median_matched, n_unmatched, median_unmatched,
                   p_formatted, sig_label, rank_biserial)
  ) %>%
  dplyr::pull(line)

# Save results
write.csv(match_summary,
          file.path(banc.fig2.supp.path, "paired_unpaired_influence_summary.csv"),
          row.names = FALSE)
write.csv(match_test_summary,
          file.path(banc.fig2.supp.path, "paired_unpaired_influence_tests.csv"),
          row.names = FALSE)
writeLines(c("Matched vs Unmatched Influence Tests (Wilcoxon rank-sum test, one-sided: matched > unmatched, Holm-adjusted)",
             "",
             match_test_text),
           file.path(banc.fig2.supp.path, "paired_unpaired_influence_by_effector_body_part.txt"))

# Create box plot with dodged positions and significance tests
# Order body parts by matched median (descending)
# Only include body parts that have matched connections (i.e., have sensors)
bp_order_match <- match_summary %>%
  dplyr::filter(match_class == "matched") %>%
  dplyr::filter(!is.na(target_body_part)) %>%
  dplyr::arrange(desc(median_influence)) %>%
  dplyr::pull(target_body_part)

# Prepare raw data for box plot with ordered body parts
plot_data_match_raw <- influence_matched_unmatched %>%
  dplyr::filter(!is.na(target_body_part)) %>%
  dplyr::filter(target_body_part %in% bp_order_match) %>%
  dplyr::mutate(
    target_body_part = factor(target_body_part, levels = bp_order_match)
  )

# Prepare summary data for significance annotations
plot_data_match <- match_summary %>%
  dplyr::filter(!is.na(target_body_part)) %>%
  dplyr::filter(target_body_part %in% bp_order_match) %>%
  dplyr::mutate(
    target_body_part = factor(target_body_part, levels = bp_order_match)
  )

# Prepare outlier data (points outside IQR) - COMMENTED OUT per user request
# Keep seed_body_part for coloring
# outlier_data_match <- influence_matched_unmatched %>%
#   dplyr::filter(!is.na(target_body_part)) %>%
#   dplyr::filter(target_body_part %in% bp_order_match) %>%
#   dplyr::left_join(
#     match_summary %>% dplyr::select(target_body_part, match_class, q25, q75),
#     by = c("target_body_part", "match_class")
#   ) %>%
#   dplyr::filter(influence_log_adj < q25 | influence_log_adj > q75) %>%
#   dplyr::mutate(
#     target_body_part = factor(target_body_part, levels = bp_order_match)
#   ) %>%
#   dplyr::select(target_body_part, match_class, influence_log_adj, seed_body_part)

# Add significance annotations from test results
# y_pos is set after y_lower/y_max are computed below
sig_annotations_match <- match_test_summary %>%
  dplyr::filter(effector_bp %in% bp_order_match) %>%
  dplyr::mutate(
    target_body_part = factor(effector_bp, levels = bp_order_match),
    sig_label = dplyr::case_when(
      wilcox_p_adj < 0.001 ~ "***",
      wilcox_p_adj < 0.01 ~ "**",
      wilcox_p_adj < 0.05 ~ "*",
      TRUE ~ ""
    )
  ) %>%
  dplyr::filter(sig_label != "")

# Create alternating background data
n_bp_match <- length(bp_order_match)
bg_data_match <- data.frame(
  target_body_part = factor(bp_order_match, levels = bp_order_match),
  xmin = seq(0.5, n_bp_match - 0.5, by = 1),
  xmax = seq(1.5, n_bp_match + 0.5, by = 1)
) %>%
  dplyr::mutate(
    fill_col = ifelse(row_number() %% 2 == 1, "white", "grey90")
  )

# Colors for matched vs unmatched
match_cols <- c("matched" = "#E31A1C", "unmatched" = "grey50")

# Get body part colors for outliers (sensory body parts)
outlier_bp_colors <- all_colors_bp

# Dodge amount for separating matched/unmatched groups
dodge_amount <- 0.6

# Calculate y-axis lower limit: just below the lowest Q25 across all groups
y_lower <- plot_data_match_raw %>%
  dplyr::group_by(target_body_part, match_class) %>%
  dplyr::summarise(q25 = quantile(influence_log_minmax, 0.25, na.rm = TRUE), .groups = "drop") %>%
  dplyr::pull(q25) %>%
  min(na.rm = TRUE)
y_lower <- y_lower - abs(y_lower) * 0.05

y_max <- min(1.0, max(plot_data_match_raw$influence_log_minmax, na.rm = TRUE))

# Add y_pos for significance annotations (just below the 1.0 cap)
sig_annotations_match$y_pos <- 0.97

# Create box plot (vertical) with matched vs unmatched
match_plot <- ggplot() +
  # Alternating background rectangles
  geom_rect(data = bg_data_match,
            aes(xmin = xmin, xmax = xmax,
                ymin = -Inf, ymax = Inf, fill = fill_col),
            inherit.aes = FALSE) +
  scale_fill_identity() +
  ggnewscale::new_scale_fill() +
  # Box plots for matched vs unmatched distributions (thinner boxes, no outliers)
  geom_boxplot(data = plot_data_match_raw,
               aes(x = target_body_part, y = influence_log_minmax, fill = match_class),
               position = position_dodge(width = dodge_amount),
               width = 0.4,  # Make boxes thinner
               outlier.shape = NA,  # Hide outliers
               linewidth = 0.5, alpha = 0.7) +
  scale_fill_manual(values = match_cols, name = "Match\nStatus",
                    labels = c("matched" = "Matched", "unmatched" = "Unmatched")) +
  # Add significance annotations (asterisks)
  geom_text(data = sig_annotations_match,
            aes(x = target_body_part, y = y_pos, label = sig_label),
            size = 5, vjust = 0, hjust = 0.5, inherit.aes = FALSE) +
  coord_cartesian(ylim = c(y_lower, 1.05)) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10),
    panel.background = element_rect(fill = "white", color = NA)
  ) +
  labs(
    x = "Effector body part",
    y = "Influence (log min-max normalised)"
  )

ggsave(
  filename = file.path(banc.fig2.supp.path, "paired_unpaired_influence_by_effector_body_part.pdf"),
  plot = match_plot,
  width = 14,
  height = 4,
  units = "in",
  device = cairo_pdf
)

##############################################################
### EXTENDED DATA FIGURE 4f                                ##
### Top 2 sensory body parts per effector body part        ##
##############################################################

# Calculate median influence for each sensory body part → effector body part
sens_to_eff_summary <- influence_seed_to_target_ct %>%
  dplyr::filter(!is.na(seed_body_part), !is.na(target_body_part)) %>%
  dplyr::group_by(seed_body_part, target_body_part) %>%
  dplyr::summarise(
    n = dplyr::n(),
    median_influence = median(influence_log_adj, na.rm = TRUE),
    q25 = quantile(influence_log_adj, 0.25, na.rm = TRUE),
    q75 = quantile(influence_log_adj, 0.75, na.rm = TRUE),
    iqr = IQR(influence_log_adj, na.rm = TRUE),
    .groups = "drop"
  )

# Get top 2 sensory body parts for each effector body part
# Strategy: For each paired effector body part, show:
#   1) The matched sensory input FIRST (if it exists in the data)
#   2) The top non-matched sensory input SECOND
# This ensures matched pairs are always visible when they exist
top2_sens_per_eff <- sens_to_eff_summary %>%
  dplyr::filter(target_body_part %in% paired.bps) %>%  # Only keep paired body parts
  dplyr::mutate(
    is_matched = (seed_body_part == target_body_part) & (target_body_part %in% paired.bps)
  ) %>%
  dplyr::group_by(target_body_part) %>%
  dplyr::arrange(desc(is_matched), desc(median_influence)) %>%  # Matched first, then by influence
  dplyr::slice_head(n = 2) %>%
  dplyr::mutate(rank = 1:dplyr::n()) %>%
  dplyr::ungroup()

# Diagnostic: Report which effector body parts have matched sensory in top 2
matched_in_top2 <- top2_sens_per_eff %>%
  dplyr::filter(is_matched) %>%
  dplyr::pull(target_body_part) %>%
  unique()

cat(sprintf("Top 2 analysis: %d effector body parts\n", length(unique(top2_sens_per_eff$target_body_part))))
cat(sprintf("  Matched sensory in top 2: %d/%d effector body parts\n",
            length(matched_in_top2), length(paired.bps)))
if (length(setdiff(paired.bps, matched_in_top2)) > 0) {
  cat(sprintf("  Missing matched connections for: %s\n",
              paste(setdiff(paired.bps, matched_in_top2), collapse=", ")))
}

# Statistical tests: compare top 2 for each effector
top2_test_results <- list()
top2_test_text <- character()

for (eff_bp in unique(top2_sens_per_eff$target_body_part)) {
  bp_data <- top2_sens_per_eff %>% dplyr::filter(target_body_part == eff_bp)

  if (nrow(bp_data) == 2) {
    sens1 <- bp_data$seed_body_part[1]
    sens2 <- bp_data$seed_body_part[2]

    # Get raw data for comparison
    vals1 <- influence_seed_to_target_ct %>%
      dplyr::filter(seed_body_part == sens1, target_body_part == eff_bp) %>%
      dplyr::pull(influence_log_adj)

    vals2 <- influence_seed_to_target_ct %>%
      dplyr::filter(seed_body_part == sens2, target_body_part == eff_bp) %>%
      dplyr::pull(influence_log_adj)

    if (length(vals1) >= 2 && length(vals2) >= 2) {
      wtest <- tryCatch({
        wilcox.test(vals1, vals2)
      }, error = function(e) NULL)

      if (!is.null(wtest)) {
        top2_test_results[[eff_bp]] <- list(
          effector_bp = eff_bp,
          sensory1 = sens1,
          sensory2 = sens2,
          n1 = length(vals1),
          n2 = length(vals2),
          median1 = median(vals1),
          median2 = median(vals2),
          wilcox_p = wtest$p.value
        )

      }
    }
  }
}

top2_test_summary <- dplyr::bind_rows(top2_test_results) %>%
  dplyr::mutate(
    wilcox_p_adj = p.adjust(wilcox_p, method = "holm"),
    significant = wilcox_p_adj < 0.05
  ) %>%
  dplyr::arrange(wilcox_p)

# Build text output using Holm-adjusted p-values (consistent with plot)
top2_test_text <- top2_test_summary %>%
  dplyr::mutate(
    sig_label = dplyr::case_when(
      wilcox_p_adj < 0.001 ~ "***",
      wilcox_p_adj < 0.01  ~ "**",
      wilcox_p_adj < 0.05  ~ "*",
      TRUE                 ~ "ns"
    ),
    p_formatted = fmt_p_value(wilcox_p_adj),
    line = sprintf("%s: %s (median=%.2f) vs %s (median=%.2f), p_adj=%s %s",
                   effector_bp, sensory1, median1, sensory2, median2,
                   p_formatted, sig_label)
  ) %>%
  dplyr::pull(line)

# Save test results
write.csv(top2_test_summary,
          file.path(banc.fig2.supp.path, "top_2_sensory_body_parts_by_effector_body_parts_tests.csv"),
          row.names = FALSE)
writeLines(c("Top 2 Sensory Body Parts: Statistical Tests (Wilcoxon rank-sum test, two-sided, Holm-adjusted)",
             "",
             top2_test_text),
           file.path(banc.fig2.supp.path, "top_2_sensory_body_parts_by_effector_body_parts.txt"))

# Add sig labels to plot data
top2_sig_labels <- top2_test_summary %>%
  dplyr::mutate(
    sig_label = dplyr::case_when(
      wilcox_p_adj < 0.001 ~ "***",
      wilcox_p_adj < 0.01 ~ "**",
      wilcox_p_adj < 0.05 ~ "*",
      TRUE ~ ""
    )
  )

# Order effector body parts by their matched sensor median (if exists) or top sensor median
eff_bp_order_top2 <- top2_sens_per_eff %>%
  dplyr::group_by(target_body_part) %>%
  dplyr::summarise(
    max_matched = max(ifelse(is_matched, median_influence, -Inf), na.rm = TRUE),
    max_overall = max(median_influence, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    sort_val = ifelse(is.finite(max_matched), max_matched, max_overall)
  ) %>%
  dplyr::arrange(desc(sort_val)) %>%
  dplyr::pull(target_body_part)

# Prepare summary data for annotations
plot_data_top2 <- top2_sens_per_eff %>%
  dplyr::mutate(
    target_body_part = factor(target_body_part, levels = eff_bp_order_top2),
    match_color = ifelse(is_matched, "matched", "unmatched")
  )

# Prepare raw data for box plots (only top 2 sensory-effector pairs)
plot_data_top2_raw <- influence_seed_to_target_ct %>%
  dplyr::inner_join(
    top2_sens_per_eff %>% dplyr::select(seed_body_part, target_body_part, rank, is_matched),
    by = c("seed_body_part", "target_body_part")
  ) %>%
  dplyr::mutate(
    target_body_part = factor(target_body_part, levels = eff_bp_order_top2),
    match_color = ifelse(is_matched, "matched", "unmatched"),
    sensory_rank = paste0(seed_body_part, " (#", rank, ")")  # Label with rank
  )

# Prepare outlier data for top 2
# outlier_data_top2 <- influence_seed_to_target_ct %>%
#   dplyr::inner_join(
#     top2_sens_per_eff %>% dplyr::select(seed_body_part, target_body_part, q25, q75),
#     by = c("seed_body_part", "target_body_part")
#   ) %>%
#   dplyr::filter(influence_log_adj < q25 | influence_log_adj > q75) %>%
#   dplyr::mutate(
#     target_body_part = factor(target_body_part, levels = eff_bp_order_top2)
#   )

# Create alternating background
n_eff_bp_top2 <- length(eff_bp_order_top2)
bg_data_top2 <- data.frame(
  target_body_part = factor(eff_bp_order_top2, levels = eff_bp_order_top2),
  ymin = seq(0.5, n_eff_bp_top2 - 0.5, by = 1),
  ymax = seq(1.5, n_eff_bp_top2 + 0.5, by = 1)
) %>%
  dplyr::mutate(
    fill_col = ifelse(row_number() %% 2 == 1, "white", "grey90")
  )

# Colors: matched = red, unmatched = grey
match_colors <- c("matched" = "#E31A1C", "unmatched" = "grey50")

# Dodge amount
dodge_top2 <- 0.6

# Calculate x-axis limits for label positioning
x_min_top2 <- min(plot_data_top2_raw$influence_log_adj, na.rm = TRUE)
x_max_top2 <- max(plot_data_top2_raw$influence_log_adj, na.rm = TRUE)
x_range_top2 <- x_max_top2 - x_min_top2

# Add position column for sensory body part labels on far left
plot_data_top2_labels <- plot_data_top2 %>%
  dplyr::mutate(
    x_label_pos = x_min_top2 - x_range_top2 * 0.05
  )

# Prepare p-value annotations with all p-values (not just significant ones)
pval_annot_top2 <- top2_test_summary %>%
  dplyr::mutate(
    target_body_part = factor(effector_bp, levels = eff_bp_order_top2),
    p_label = sprintf("%.3g", wilcox_p),
    # Position at right of plot
    x_pos = x_max_top2 + x_range_top2 * 0.05
  )

# Create top 2 sensory body parts plot with box plots
top2_plot <- ggplot() +
  # Alternating backgrounds
  geom_rect(data = bg_data_top2,
            aes(xmin = -Inf, xmax = Inf,
                ymin = ymin, ymax = ymax, fill = fill_col),
            inherit.aes = FALSE) +
  scale_fill_identity() +
  # Box plots for top 2 sensory body parts per effector (thinner boxes)
  ggnewscale::new_scale_fill() +
  geom_boxplot(data = plot_data_top2_raw,
               aes(x = influence_log_adj, y = target_body_part,
                   fill = match_color, group = interaction(target_body_part, sensory_rank)),
               position = position_dodge(width = dodge_top2),
               width = 0.4,  # Make boxes thinner
               outlier.size = 0.5, outlier.alpha = 0.3,
               linewidth = 0.5, alpha = 0.7) +
  scale_fill_manual(values = match_colors, name = "Match Status",
                    labels = c("matched" = "Matched", "unmatched" = "Unmatched")) +
  # Add sensory body part labels on far left
  ggnewscale::new_scale_color() +
  geom_text(data = plot_data_top2_labels,
            aes(x = x_label_pos, y = target_body_part,
                label = seed_body_part,
                color = match_color),
            position = position_dodge(width = dodge_top2),
            hjust = 1, size = 2.8, fontface = "italic") +
  scale_color_manual(values = match_colors, guide = "none") +
  # Add p-value annotations on right
  geom_text(data = pval_annot_top2,
            aes(x = x_pos, y = target_body_part, label = p_label),
            size = 3, vjust = 0.5, hjust = 0, inherit.aes = FALSE) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    panel.background = element_rect(fill = "white", color = NA)
  ) +
  labs(
    x = "Influence (log-adjusted)",
    y = "Effector body part",
    title = "Top 2 sensory body parts per effector body part"
  ) +
  coord_cartesian(xlim = c(x_min_top2 - x_range_top2 * 0.15, x_max_top2 + x_range_top2 * 0.15),
                  clip = "off")

ggsave(
  filename = file.path(banc.fig2.supp.path, "top_2_sensory_body_parts_by_effector_body_parts.pdf"),
  plot = top2_plot,
  width = 12,
  height = 10,
  units = "in",
  device = cairo_pdf
)

###################################################
### EXTENDED DATA FIGURE 4f                      ##
### Option C: Cell type level level with stats   ##
###################################################

# Calculate influence at cell_type × body_part level
# Keep both sensory and effector at cell_type resolution
bps  <- banc.sens.meta %>%
  dplyr::filter(!is.na(body_part_sensory),body_part_sensory!="unknown") %>%
  dplyr::distinct(body_part_sensory) %>%
  dplyr::pull(body_part_sensory)
influence.sensors.detailed.db.orig <- data.frame()
for(bp in bps){
  cts  <- banc.sens.meta %>%
    dplyr::filter(!is.na(body_part_sensory),
                  body_part_sensory!="unknown",
                  body_part_sensory==bp) %>%
    dplyr::distinct(cell_type) %>%
    dplyr::pull(cell_type)
  for(ct in cts){
    banc.ct.meta <- subset(banc.meta,seed_02==ct)
    banc.ct.ids <- unique(na.omit(banc.ct.meta$root_id))
    try({
      control_influence.id <- calculate_influence_py(ic_banc, banc.ct.ids) %>%
        dplyr::filter(id %in% banc.eff.meta$id) %>%
        dplyr::left_join(banc.eff.meta %>%
                           dplyr::distinct(id=root_id,
                                           target = cell_type,
                                           target_body_part=body_part_effector,
                                           target_super_class=super_class), by = "id")
      control_influence.id$seed <- ct
      control_influence.id$seed_body_part <- bp
      control_influence.id$seed_class <- "sensory"
      control_influence.id$influence_norm_original <- control_influence.id$`Influence_score_(unsigned)`/length(banc.ct.ids)
      influence.sensors.detailed.db.orig <- rbind(influence.sensors.detailed.db.orig,
                                                  control_influence.id)
    })
  }
}
# Create cell_type labels with body part
influence_celltype_detailed <- influence.sensors.detailed.db.orig %>%
  dplyr::mutate(influence_original = `Influence_score_(unsigned)`) %>%
  dplyr::mutate(
    seed_label = paste0(seed, " [", seed_body_part, "]"),
    target_label = paste0(target, " [", target_body_part, "]")
  )



