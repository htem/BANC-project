#' Pre-effector and pre-pre-effector adjusted-influence layers (Fig. 2g, ED Fig. 4e)
#'
#' Identifies pre-effector neurons (directly presynaptic to an effector at
#' ≥ 10 synapses) and pre-pre-effector neurons (presynaptic to a pre-
#' effector, ≥ 10 synapses), then runs influence calculations layer by
#' layer onto the effector populations. Renders the source×target heatmaps
#' used in Fig. 2g and the matched-body-part influence-fraction comparison
#' used in ED Fig. 4e.
#'
#' Note: this script uses a `count >= 10` synapse threshold for hop-tracing
#' (intentional, more conservative than the count_thresh = 5 used in the
#' influence model itself) so that the pre/pre-pre neuron rosters are
#' robust to single-synapse noise. The influencer call still uses
#' `count_thresh = 5` for the activity-propagation graph.
#'
#' @section Reads:
#'   banc.meta, banc.edgelist.simple
#'   data/influence/.../<pre_or_prepre_seed>_influence.csv
#'
#' @section Writes:
#'   figures/figure_2/links/*_pre_effector_*.pdf                          (Fig. 2g left)
#'   figures/figure_2/links/*_pre_pre_effector_*.pdf                      (Fig. 2g right)
#'   figures/figure_2/links/supplement/*_pre_pre_proportion_*.pdf         (ED Fig. 4e)
#'   figures/figure_2/links/*.txt                                          (two-way ANOVA + paired Wilcoxon)
#'
#' @section Paper:
#'   Fig. 2g — pre-effector and pre-pre-effector influence onto effectors,
#'             grouped by body part; Type-III two-way ANOVA, n_effector = 1,031,
#'             n_pre = 8,824, n_pre-pre = 16,838.
#'   ED Fig. 4e — proportion of effector influence from matched body part,
#'                paired Wilcoxon (P = 9.54 × 10⁻⁷, n = 21 body parts).
#'   Methods §"Influence" Eqs. 1–10.
#'
#' @section Schema:
#'   `count >= 10` synapse threshold is the hop-tracing threshold; do NOT
#'   change without revisiting the body-part roster sizes cited above.
#'
#' @section Used by:
#'   R/text/numbers.R (effector_neuron_count, pre_effector_count,
#'                     pre_pre_effector_count).
#'
#' @section Reproduce:
#'   BANC_NCORES=1 Rscript R/figures/panels_pre_effector_influence.R

###################
## LOAD PACKAGES ##
###################

# Load required packages and data for influence validation
source("R/startup/banc-startup.R")
source("R/startup/banc-meta.R")
source("R/startup/banc-edgelist.R")
library(influencer)

# Define output paths for different figure types
banc.fig2.path <- "figures/figure_2/links/"
banc.fig2.extra.path <- "figures/figure_2/links/supplement"
banc.fig2.anat.path <- "figures/figure_2/links/neuroanatomy"
banc.fig2.extra.path <- "figures/figure_2/links/extra"

####################
## LABEL MAPPING  ##
####################

# Define simplified effector target groupings
target.map <- c(retrocerebral_complex = "retrocerebral",
                corpus_allatum = "retrocerebral",
                enteric_complex = "digestive tract",
                digestive_tract = "digestive tract",
                crop = "crop", 
                salivary_gland = "salivary gland", 
                pharynx = "pharynx", 
                proboscis = "proboscis",
                antenna = "antenna", 
                eye = "eye", 
                neck = "neck", 
                wing = "wing",
                haltere = "haltere", 
                neurohemal_complex = "neurohemal",
                front_leg = "front leg", 
                middle_leg = "middle leg", 
                hind_leg = "hind leg", 
                thoracic_abdominal_segmental = "thoracic-abdominal",
                thoracic_abdominal = "thoracic-abdominal",
                `thoracic-abdominal` = "thoracic-abdominal",
                unknown_thoracic_abdominal_segmental = "thoracic-abdominal",
                unknown_thoracic_abdominal = "thoracic-abdominal",
                `unknown-thoracic-abdominal` = "thoracic-abdominal",
                ureter = "ureter",
                abdomen = "abdomen",
                reproductive_tract = "reproductive tract",
                # ovaries = "ovaries",  # subsumed into reproductive tract
                uterus = "uterus"
)

##################
## connectivity ##
##################

# Use BANC edgelist (GCS or already loaded).
# Global `count >= 3` filter removed 2026-04-09 — see banc-edgelist.R. The
# `count >= 10` filters further down are a *different* thing (intentional
# hop-tracing threshold for the pre-effector pyramid analysis) and remain.
bc.elist <- banc.edgelist.simple
bc.meta <- banc.meta

# Set up for influence calculation
ic_banc <- influence_calculator_py(edgelist_simple = bc.elist,
                                   meta = bc.meta,
                                   count_thresh = 5)

########################################
## Influence between efferent neurons ##
########################################

# Totl upstream
banc.eff.ids <- na.omit(unique(banc.eff.meta$root_id))
banc.pre.eff.ids <- bc.elist %>%
  dplyr::filter(post %in% banc.eff.ids,
                !pre %in% banc.eff.ids,
                count >= 10) %>%
  dplyr::pull(pre)
banc.pre.pre.eff.ids <- bc.elist %>%
  dplyr::filter(post %in% banc.pre.eff.ids,
                !pre %in% c(banc.pre.eff.ids,banc.eff.ids),
                count >= 10) %>%
  dplyr::pull(pre)
banc.pre.pre.pre.eff.ids <- bc.elist %>%
  dplyr::filter(post %in% banc.pre.pre.eff.ids,
                !pre %in% c(banc.pre.pre.eff.ids, banc.pre.eff.ids, banc.eff.ids),
                count >= 10) %>%
  dplyr::pull(pre)
.level_counts <- sprintf(
  paste0("Pre-effector neuron counts (count >= 10 hop threshold)\n",
         "  Effector neurons:           %d\n",
         "  Pre-effector neurons:       %d\n",
         "  Pre-pre-effector neurons:   %d\n",
         "  Pre-pre-pre-effector neurons: %d\n"),
  length(banc.eff.ids), length(banc.pre.eff.ids),
  length(banc.pre.pre.eff.ids), length(banc.pre.pre.pre.eff.ids))
message(.level_counts)
writeLines(.level_counts,
           file.path(banc.fig2.extra.path, "pre_effector_neuron_counts.txt"))

# Get influence results for BANC.
# Rbindlist refactor (2026-04-09): was rbind-in-loop accumulating to
# data.frame() — replaced with pre-allocated list + data.table::rbindlist.
# Also pre-indexed banc.eff.meta by body_part_effector via split() so each
# iteration is an O(1) lookup instead of a full subset() on the eff meta.
# The four seed sets per body_part (bp, pre, pre_pre, pre_pre_pre) are
# genuinely different — cannot be collapsed into one calculate_influence_py
# call because each represents a different upstream hop level.
banc.eff.meta <- banc.meta %>%
  dplyr::filter(grepl("motor",super_class)|grepl("visceral_circulatory",super_class))
banc.eff.ids <- unique(banc.eff.meta$root_id)
.eff_target_lookup <- banc.eff.meta %>%
  dplyr::distinct(id = root_id, target = body_part_effector)
.eff_ids_by_bp <- split(banc.eff.meta$root_id, banc.eff.meta$body_part_effector)
body.parts <- na.omit(unique(banc.meta$body_part_effector))
banc.efferent_influence_list <- vector("list", length(body.parts) * 4L)
.slot <- 0L
for(bp in body.parts){
  banc.bp.ids <- unique(.eff_ids_by_bp[[bp]])
  banc.pre.eff.ids <- bc.elist %>%
    dplyr::filter(post %in% banc.bp.ids,
                  !pre %in% banc.eff.ids,
                  count >= 10) %>%
    dplyr::pull(pre)
  try({
    efferent_influence.id <- calculate_influence_py(ic_banc, banc.bp.ids) %>%
      dplyr::filter(id %in% banc.eff.ids) %>%
      dplyr::left_join(.eff_target_lookup, by = "id")
    efferent_influence.id$seed <- bp
    efferent_influence.id$influence_norm_original <- efferent_influence.id$`Influence_score_(unsigned)`/length(banc.bp.ids)
    .slot <- .slot + 1L
    banc.efferent_influence_list[[.slot]] <- efferent_influence.id
  })
  try({
    efferent_influence.id <- calculate_influence_py(ic_banc, banc.pre.eff.ids) %>%
      dplyr::filter(id %in% banc.eff.ids) %>%
      dplyr::left_join(.eff_target_lookup, by = "id")
    efferent_influence.id$seed <- paste0("pre_",bp)
    efferent_influence.id$influence_norm_original <- efferent_influence.id$`Influence_score_(unsigned)`/length(banc.pre.eff.ids)
    .slot <- .slot + 1L
    banc.efferent_influence_list[[.slot]] <- efferent_influence.id
  })
  try({
    banc.pre.pre.eff.ids <- bc.elist %>%
      dplyr::filter(post %in% banc.pre.eff.ids,
                    !pre %in% c(banc.eff.ids,banc.pre.eff.ids),
                    count >= 10) %>%
      dplyr::pull(pre)
    efferent_influence.id <- calculate_influence_py(ic_banc, banc.pre.pre.eff.ids) %>%
      dplyr::filter(id %in% banc.eff.ids) %>%
      dplyr::left_join(.eff_target_lookup, by = "id")
    efferent_influence.id$seed <- paste0("pre_pre_",bp)
    efferent_influence.id$influence_norm_original <- efferent_influence.id$`Influence_score_(unsigned)`/length(banc.pre.pre.eff.ids)
    .slot <- .slot + 1L
    banc.efferent_influence_list[[.slot]] <- efferent_influence.id
  })
  try({
    banc.pre.pre.pre.eff.ids <- bc.elist %>%
      dplyr::filter(post %in% banc.pre.pre.eff.ids,
                    !pre %in% c(banc.eff.ids,banc.pre.eff.ids,banc.pre.pre.eff.ids),
                    count >= 10) %>%
      dplyr::pull(pre)
    efferent_influence.id <- calculate_influence_py(ic_banc, banc.pre.pre.pre.eff.ids) %>%
      dplyr::filter(id %in% banc.eff.ids) %>%
      dplyr::left_join(.eff_target_lookup, by = "id")
    efferent_influence.id$seed <- paste0("pre_pre_pre_",bp)
    efferent_influence.id$influence_norm_original <- efferent_influence.id$`Influence_score_(unsigned)`/length(banc.pre.pre.pre.eff.ids)
    .slot <- .slot + 1L
    banc.efferent_influence_list[[.slot]] <- efferent_influence.id
  })
}
banc.efferent_influence <- as.data.frame(
  data.table::rbindlist(banc.efferent_influence_list, fill = TRUE)
) %>%
  dplyr::mutate(influence_original = `Influence_score_(unsigned)`)
efferent_influence.signed <- data.frame()

################
## BANC plots ##
################

# Plot influence of efferent neurons onto efferent neurons
inf.metric <- "influence_log_minmax"
eff.out.key.plot <- banc_plot_key_features(
  influence.meta = banc.efferent_influence %>%
    dplyr::filter(id %in% banc.eff.ids,
                  !grepl("pre",seed)) %>%
    dplyr::mutate(target = case_when(
      target %in% names(target.map) ~ target.map[target],
      TRUE ~ target
    )) %>%
    dplyr::mutate(seed = case_when(
      seed %in% names(target.map) ~ target.map[seed],
      TRUE ~ seed
    )) %>%
    dplyr::filter(!is.na(seed), 
                  !is.na(target)),
  ###
  inf.metric = inf.metric,
  target.map = names(target.map),
  width = 14,
  height = 14,
  recalculate = TRUE,
  row.annotation = NULL,
  show.annotation = FALSE,
  save.path = banc.fig2.extra.path,
  seed.map  = FALSE,
  chosen.seeds = NULL,
  chosen.targets = NULL,
  row.cols = NULL,
  super.class = NULL,
  plot.name = sprintf("efferent_neuron_influence_by_body_part_%s.pdf",inf.metric),
  rev = FALSE,
  row.dend = NULL,
  col.dend = NULL,
  method = "euclidean",
  symmetric = FALSE,
  diagonal = TRUE,
  col.order = unique(target.map),
  row.order = unique(target.map),
  color.max = if (grepl("minmax", inf.metric)) 1 else 30
)
write_anova_summary(banc.efferent_influence %>%
                      dplyr::filter(id %in% banc.eff.ids,
                                    !grepl("pre",seed)) %>%
                      dplyr::mutate(target = case_when(
                        target %in% names(target.map) ~ target.map[target],
                        TRUE ~ target
                      )) %>%
                      dplyr::mutate(seed = case_when(
                        seed %in% names(target.map) ~ target.map[seed],
                        TRUE ~ seed
                      )) %>%
                      dplyr::filter(!is.na(seed), 
                                    !is.na(target)) %>%
                      dplyr::select(source=seed, target, value = influence_original),
                    file.path(banc.fig2.extra.path,"efferent_neuron_influence_by_body_part.txt"))

# Plot influence of pre efferent neurons onto efferent neurons
inf.metric <- "influence_log_minmax"
eff.out.key.plot <- banc_plot_key_features(
  influence.meta = banc.efferent_influence %>%
    dplyr::filter(id %in% banc.eff.ids,
                  grepl("pre_",seed),
                  !grepl("pre_pre",seed)) %>%
    dplyr::mutate(target = case_when(
      target %in% names(target.map) ~ target.map[target],
      TRUE ~ target
    )) %>%
    dplyr::mutate(seed = gsub("pre_","",seed)) %>%
    dplyr::mutate(seed = case_when(
      seed %in% names(target.map) ~ target.map[seed],
      TRUE ~ seed
    )) %>%
    dplyr::mutate(seed = paste0("pre ",seed)) %>%
    dplyr::filter(!is.na(seed), 
                  !is.na(target)),
  ###
  inf.metric = inf.metric,
  target.map = names(target.map),
  width = 14,
  height = 14,
  recalculate = TRUE,
  row.annotation = NULL,
  show.annotation = FALSE,
  save.path = banc.fig2.path,
  seed.map  = FALSE,
  chosen.seeds = NULL,
  chosen.targets = NULL, 
  row.cols = NULL,
  super.class = NULL,
  plot.name = sprintf("pre_efferent_neuron_influence_by_body_part_%s.pdf",inf.metric),
  rev = FALSE,
  row.dend = NULL,
  col.dend = NULL,
  method = "euclidean",
  symmetric = FALSE,
  diagonal = TRUE,
  col.order = paste0("pre ",unique(target.map)),
  row.order = unique(target.map),
  color.max = if (grepl("minmax", inf.metric)) 1 else 30
)
write_anova_summary(banc.efferent_influence %>%
                      dplyr::filter(id %in% banc.eff.ids,
                                    grepl("pre_",seed),
                                    !grepl("pre_pre",seed)) %>%
                      dplyr::mutate(target = case_when(
                        target %in% names(target.map) ~ target.map[target],
                        TRUE ~ target
                      )) %>%
                      dplyr::mutate(seed = gsub("pre_","",seed)) %>%
                      dplyr::mutate(seed = case_when(
                        seed %in% names(target.map) ~ target.map[seed],
                        TRUE ~ seed
                      )) %>%
                      dplyr::mutate(seed = paste0("pre ",seed)) %>%
                      dplyr::filter(!is.na(seed), 
                                    !is.na(target)) %>%
                      dplyr::select(source=seed, target, value = influence_original),
                    file.path(banc.fig2.extra.path,"pre_efferent_neuron_influence_by_body_part.txt"))

# Plot influence of pre efferent neurons onto efferent neurons
inf.metric <- "influence_log_minmax"
eff.out.key.plot <- banc_plot_key_features(
  influence.meta = banc.efferent_influence %>%
    dplyr::filter(id %in% banc.eff.ids,
                  grepl("pre_pre_",seed),
                  !grepl("pre_pre_pre_",seed)) %>%
    dplyr::mutate(target = case_when(
      target %in% names(target.map) ~ target.map[target],
      TRUE ~ target
    )) %>%
    dplyr::mutate(seed = gsub("pre_pre_","",seed)) %>%
    dplyr::mutate(seed = case_when(
      seed %in% names(target.map) ~ target.map[seed],
      TRUE ~ seed
    )) %>%
    dplyr::mutate(seed = paste0("pre pre ",seed)) %>%
    dplyr::filter(!is.na(seed), 
                  !is.na(target)),
  ###
  inf.metric = inf.metric,
  target.map = names(target.map),
  width = 14,
  height = 14,
  recalculate = TRUE,
  row.annotation = NULL,
  show.annotation = FALSE,
  save.path = banc.fig2.path,
  seed.map  = FALSE,
  chosen.seeds = NULL,
  chosen.targets = NULL, 
  row.cols = NULL,
  super.class = NULL,
  plot.name = sprintf("pre_pre_efferent_neuron_influence_by_body_part_%s.pdf",inf.metric),
  rev = FALSE,
  row.dend = NULL,
  col.dend = NULL,
  method = "euclidean",
  symmetric = FALSE,
  diagonal = TRUE,
  col.order = paste0("pre pre ",unique(target.map)),
  row.order = unique(target.map),
  color.max = if (grepl("minmax", inf.metric)) 1 else 30
)
write_anova_summary(banc.efferent_influence %>%
                      dplyr::filter(id %in% banc.eff.ids,
                                    grepl("pre_pre_",seed),
                                    !grepl("pre_pre_pre_",seed)) %>%
                      dplyr::mutate(target = case_when(
                        target %in% names(target.map) ~ target.map[target],
                        TRUE ~ target
                      )) %>%
                      dplyr::mutate(seed = gsub("pre_pre_","",seed)) %>%
                      dplyr::mutate(seed = case_when(
                        seed %in% names(target.map) ~ target.map[seed],
                        TRUE ~ seed
                      )) %>%
                      dplyr::mutate(seed = paste0("pre pre ",seed)) %>%
                      dplyr::filter(!is.na(seed), 
                                    !is.na(target)) %>%
                      dplyr::select(source=seed, target, value = influence_original),
                    file.path(banc.fig2.extra.path,"pre_pre_efferent_neuron_influence_by_body_part.txt"))

# Plot influence of pre efferent neurons onto efferent neurons
inf.metric <- "influence_log_minmax"
eff.out.key.plot <- banc_plot_key_features(
  influence.meta = banc.efferent_influence %>%
    dplyr::filter(id %in% banc.eff.ids,
                  grepl("pre_pre_pre_",seed)) %>%
    dplyr::mutate(target = case_when(
      target %in% names(target.map) ~ target.map[target],
      TRUE ~ target
    )) %>%
    dplyr::mutate(seed = gsub("pre_pre_pre_","",seed)) %>%
    dplyr::mutate(seed = case_when(
      seed %in% names(target.map) ~ target.map[seed],
      TRUE ~ seed
    )) %>%
    dplyr::mutate(seed = paste0("pre pre pre ",seed)) %>%
    dplyr::filter(!is.na(seed), 
                  !is.na(target)),
  ###
  inf.metric = inf.metric,
  target.map = names(target.map),
  width = 14,
  height = 14,
  recalculate = TRUE,
  row.annotation = NULL,
  show.annotation = FALSE,
  save.path = banc.fig2.extra.path,
  seed.map  = FALSE,
  chosen.seeds = NULL,
  chosen.targets = NULL,
  row.cols = NULL,
  super.class = NULL,
  plot.name = sprintf("pre_pre_pre_efferent_neuron_influence_by_body_part_%s.pdf",inf.metric),
  rev = FALSE,
  row.dend = NULL,
  col.dend = NULL,
  method = "euclidean",
  symmetric = FALSE,
  diagonal = TRUE,
  col.order = paste0("pre pre pre ",unique(target.map)),
  row.order = unique(target.map),
  color.max = if (grepl("minmax", inf.metric)) 1 else 30
)
write_anova_summary(banc.efferent_influence %>%
                      dplyr::filter(id %in% banc.eff.ids,
                                    grepl("pre_pre_pre_",seed)) %>%
                      dplyr::mutate(target = case_when(
                        target %in% names(target.map) ~ target.map[target],
                        TRUE ~ target
                      )) %>%
                      dplyr::mutate(seed = gsub("pre_pre_pre_","",seed)) %>%
                      dplyr::mutate(seed = case_when(
                        seed %in% names(target.map) ~ target.map[seed],
                        TRUE ~ seed
                      )) %>%
                      dplyr::mutate(seed = paste0("pre pre pre ",seed)) %>%
                      dplyr::filter(!is.na(seed),
                                    !is.na(target)) %>%
                      dplyr::select(source=seed, target, value = influence_original),
                    file.path(banc.fig2.extra.path,"pre_pre_pre_efferent_neuron_influence_by_body_part.txt"))

##############################################################################
## STATISTICAL ANALYSIS: Diagonal vs Off-Diagonal across network levels    ##
##############################################################################

# Define the 3 levels to analyze (effector level removed)
analysis_levels <- list(
  list(name = "pre_effector", pattern = "^pre_", pattern_exclude = "pre_pre", display = "Pre-effector → Effector"),
  list(name = "pre_pre_effector", pattern = "^pre_pre_", pattern_exclude = "pre_pre_pre", display = "Pre-pre-effector → Effector"),
  list(name = "pre_pre_pre_effector", pattern = "^pre_pre_pre_", pattern_exclude = NULL, display = "Pre-pre-pre-effector → Effector")
)

# Initialize storage for results
all_level_stats <- list()
summary_metrics <- data.frame()

# Function to calculate Shannon entropy (reusable)
# `shannon_entropy()` (formerly `calculate_entropy()`) lives in
# R/startup/banc-functions.R (hoisted + renamed 2026-05-21).

# Process each level
for (level_info in analysis_levels) {

  cat(sprintf("\n--- %s ---\n", level_info$display))

  # Filter and prepare data for this level
  level_data <- banc.efferent_influence %>%
    dplyr::filter(id %in% banc.eff.ids) %>%
    dplyr::mutate(
      target = case_when(
        target %in% names(target.map) ~ target.map[target],
        TRUE ~ target
      )
    )

  # Apply level-specific filtering
  if (level_info$name == "effector") {
    level_data <- level_data %>% dplyr::filter(!grepl("pre", seed))
    level_data$seed_clean <- level_data$seed
  } else if (level_info$name == "pre_effector") {
    level_data <- level_data %>%
      dplyr::filter(grepl("^pre_", seed), !grepl("pre_pre", seed)) %>%
      dplyr::mutate(seed_clean = gsub("pre_", "", seed))
  } else if (level_info$name == "pre_pre_effector") {
    level_data <- level_data %>%
      dplyr::filter(grepl("^pre_pre_", seed), !grepl("pre_pre_pre", seed)) %>%
      dplyr::mutate(seed_clean = gsub("pre_pre_", "", seed))
  } else if (level_info$name == "pre_pre_pre_effector") {
    level_data <- level_data %>%
      dplyr::filter(grepl("^pre_pre_pre_", seed)) %>%
      dplyr::mutate(seed_clean = gsub("pre_pre_pre_", "", seed))
  }

  # Apply target map to seed_clean
  level_data <- level_data %>%
    dplyr::mutate(
      seed_clean = case_when(
        seed_clean %in% names(target.map) ~ target.map[seed_clean],
        TRUE ~ seed_clean
      )
    ) %>%
    dplyr::filter(!is.na(seed_clean), !is.na(target))

  # Label diagonal vs off-diagonal
  level_data <- level_data %>%
    dplyr::mutate(is_diagonal = (seed_clean == target))

  # === ANALYSIS 1: Diagonal vs Off-Diagonal Test ===
  diagonal_vals <- level_data %>% dplyr::filter(is_diagonal) %>% dplyr::pull(influence_original)
  off_diagonal_vals <- level_data %>% dplyr::filter(!is_diagonal) %>% dplyr::pull(influence_original)

  if (length(diagonal_vals) >= 2 && length(off_diagonal_vals) >= 2) {
    wtest <- wilcox.test(diagonal_vals, off_diagonal_vals, alternative = "greater")

    # Calculate effect size (rank-biserial)
    # Formula: r = (2*U)/(n1*n2) - 1
    # Ranges from -1 to 1, positive when group 1 > group 2
    n1 <- length(diagonal_vals)
    n2 <- length(off_diagonal_vals)
    U <- wtest$statistic
    r_rb <- (2*U) / (n1 * n2) - 1

    cat(sprintf("  Diagonal (n=%d): median=%.3f, IQR=[%.3f, %.3f]\n",
                n1,
                median(diagonal_vals, na.rm = TRUE),
                quantile(diagonal_vals, 0.25, na.rm = TRUE),
                quantile(diagonal_vals, 0.75, na.rm = TRUE)))
    cat(sprintf("  Off-diagonal (n=%d): median=%.3f, IQR=[%.3f, %.3f]\n",
                n2,
                median(off_diagonal_vals, na.rm = TRUE),
                quantile(off_diagonal_vals, 0.25, na.rm = TRUE),
                quantile(off_diagonal_vals, 0.75, na.rm = TRUE)))
    cat(sprintf("  Wilcoxon test: p=%.3e, rank-biserial r=%.3f\n", wtest$p.value, r_rb))

    # Store results
    all_level_stats[[level_info$name]] <- list(
      level = level_info$display,
      n_diagonal = n1,
      n_off_diagonal = n2,
      median_diagonal = median(diagonal_vals, na.rm = TRUE),
      median_off_diagonal = median(off_diagonal_vals, na.rm = TRUE),
      wilcox_p = wtest$p.value,
      rank_biserial = r_rb
    )
  }

  # === ANALYSIS 2: Influence Concentration ===
  # For each target body part, calculate:
  # - Proportion from matched source
  # - Shannon entropy (diversity of sources)

  concentration_results <- level_data %>%
    dplyr::group_by(target) %>%
    dplyr::summarise(
      total_influence = sum(influence_original, na.rm = TRUE),
      matched_influence = sum(influence_original[is_diagonal], na.rm = TRUE),
      matched_proportion = matched_influence / total_influence,
      n_sources = dplyr::n_distinct(seed_clean),
      entropy = shannon_entropy(influence_original),
      .groups = "drop"
    )

  cat(sprintf("\n  Concentration metrics:\n"))
  cat(sprintf("    Median matched proportion: %.3f (IQR: %.3f - %.3f)\n",
              median(concentration_results$matched_proportion, na.rm = TRUE),
              quantile(concentration_results$matched_proportion, 0.25, na.rm = TRUE),
              quantile(concentration_results$matched_proportion, 0.75, na.rm = TRUE)))
  cat(sprintf("    Median Shannon entropy: %.3f (IQR: %.3f - %.3f)\n",
              median(concentration_results$entropy, na.rm = TRUE),
              quantile(concentration_results$entropy, 0.25, na.rm = TRUE),
              quantile(concentration_results$entropy, 0.75, na.rm = TRUE)))

  # Store summary metrics for plotting
  summary_metrics <- rbind(summary_metrics, data.frame(
    level = level_info$name,
    level_display = level_info$display,
    level_order = which(sapply(analysis_levels, function(x) x$name) == level_info$name),
    matched_proportion_median = median(concentration_results$matched_proportion, na.rm = TRUE),
    matched_proportion_q25 = quantile(concentration_results$matched_proportion, 0.25, na.rm = TRUE),
    matched_proportion_q75 = quantile(concentration_results$matched_proportion, 0.75, na.rm = TRUE),
    entropy_median = median(concentration_results$entropy, na.rm = TRUE),
    entropy_q25 = quantile(concentration_results$entropy, 0.25, na.rm = TRUE),
    entropy_q75 = quantile(concentration_results$entropy, 0.75, na.rm = TRUE),
    rank_biserial = if(!is.null(all_level_stats[[level_info$name]])) all_level_stats[[level_info$name]]$rank_biserial else NA,
    wilcox_p = if(!is.null(all_level_stats[[level_info$name]])) all_level_stats[[level_info$name]]$wilcox_p else NA,
    stringsAsFactors = FALSE
  ))

  # Store raw concentration data for pairwise testing
  concentration_results$level <- level_info$name
  concentration_results$level_order <- which(sapply(analysis_levels, function(x) x$name) == level_info$name)
  if (!exists("all_concentration_data")) {
    all_concentration_data <- concentration_results
  } else {
    all_concentration_data <- rbind(all_concentration_data, concentration_results)
  }
}

##############################################################################
## HELPER FUNCTIONS                                                         ##
##############################################################################

# Helper function for p-value formatting
fmt_p <- function(p) {
  if (p < 0.001) return(sprintf("%.2e", p))
  else if (p < 0.01) return(sprintf("%.4f", p))
  else return(sprintf("%.3f", p))
}

# Helper function for significance symbols
sig_symbol <- function(p) {
  if (p < 0.001) return("***")
  else if (p < 0.01) return("**")
  else if (p < 0.05) return("*")
  else return("ns")
}

##############################################################################
## PAIRWISE TESTS: Between adjacent network levels                         ##
##############################################################################

# Test matched proportion between adjacent levels
pairwise_prop_tests <- data.frame()
for (i in 1:2) {
  level1_data <- all_concentration_data %>% dplyr::filter(level_order == i) %>% dplyr::pull(matched_proportion)
  level2_data <- all_concentration_data %>% dplyr::filter(level_order == i+1) %>% dplyr::pull(matched_proportion)

  wtest <- wilcox.test(level1_data, level2_data, paired = FALSE)

  pairwise_prop_tests <- rbind(pairwise_prop_tests, data.frame(
    comparison = sprintf("%d vs %d", i, i+1),
    level1 = i,
    level2 = i+1,
    x_pos = i + 0.5,
    p_value = wtest$p.value,
    sig_label = sig_symbol(wtest$p.value),
    stringsAsFactors = FALSE
  ))
}

# Test entropy between adjacent levels
pairwise_entropy_tests <- data.frame()
for (i in 1:2) {
  level1_data <- all_concentration_data %>% dplyr::filter(level_order == i) %>% dplyr::pull(entropy)
  level2_data <- all_concentration_data %>% dplyr::filter(level_order == i+1) %>% dplyr::pull(entropy)

  wtest <- wilcox.test(level1_data, level2_data, paired = FALSE)

  pairwise_entropy_tests <- rbind(pairwise_entropy_tests, data.frame(
    comparison = sprintf("%d vs %d", i, i+1),
    level1 = i,
    level2 = i+1,
    x_pos = i + 0.5,
    p_value = wtest$p.value,
    sig_label = sig_symbol(wtest$p.value),
    stringsAsFactors = FALSE
  ))
}
# Apply Holm correction across pairwise tests
pairwise_prop_tests$p_adj <- p.adjust(pairwise_prop_tests$p_value, method = "holm")
pairwise_prop_tests$sig_label <- sapply(pairwise_prop_tests$p_adj, sig_symbol)
pairwise_entropy_tests$p_adj <- p.adjust(pairwise_entropy_tests$p_value, method = "holm")
pairwise_entropy_tests$sig_label <- sapply(pairwise_entropy_tests$p_adj, sig_symbol)

for (i in 1:nrow(pairwise_prop_tests)) {
  cat(sprintf("  %s: p=%.3e %s\n", pairwise_prop_tests$comparison[i],
              pairwise_prop_tests$p_adj[i], pairwise_prop_tests$sig_label[i]))
}
for (i in 1:nrow(pairwise_entropy_tests)) {
  cat(sprintf("  %s: p=%.3e %s\n", pairwise_entropy_tests$comparison[i],
              pairwise_entropy_tests$p_adj[i], pairwise_entropy_tests$sig_label[i]))
}

cat("\n")

##############################################################################
## VISUALIZATION: Summary plots across network levels                      ##
##############################################################################

# Add significance labels to summary_metrics for effect size plot (Holm-adjusted)
summary_metrics$wilcox_p_adj <- p.adjust(summary_metrics$wilcox_p, method = "holm")
summary_metrics$sig_label <- sapply(summary_metrics$wilcox_p_adj, sig_symbol)

# Calculate y positions for pairwise test labels
prop_y_max <- max(summary_metrics$matched_proportion_q75, na.rm = TRUE)
prop_y_range <- max(summary_metrics$matched_proportion_median, na.rm = TRUE) - min(summary_metrics$matched_proportion_median, na.rm = TRUE)
pairwise_prop_tests$y_pos <- prop_y_max + 0.05 * prop_y_range

entropy_y_max <- max(summary_metrics$entropy_q75, na.rm = TRUE)
entropy_y_range <- max(summary_metrics$entropy_median, na.rm = TRUE) - min(summary_metrics$entropy_median, na.rm = TRUE)
pairwise_entropy_tests$y_pos <- entropy_y_max + 0.05 * entropy_y_range

# Plot 1: Paired dotplot of matched proportion — pre vs pre-pre
paired_data <- all_concentration_data %>%
  dplyr::filter(level_order %in% c(1, 2)) %>%
  dplyr::mutate(level_label = factor(
    ifelse(level_order == 1, "Pre", "Pre-pre"),
    levels = c("Pre", "Pre-pre")
  )) %>%
  dplyr::group_by(target) %>%
  dplyr::filter(dplyr::n() == 2) %>%
  dplyr::ungroup()

p_matched_prop <- ggplot2::ggplot(paired_data,
                                   ggplot2::aes(x = level_label, y = matched_proportion)) +
  ggplot2::geom_line(ggplot2::aes(group = target), alpha = 0.3, color = "grey50") +
  ggplot2::geom_point(ggplot2::aes(color = level_label), size = 2.5) +
  # ggpubr::stat_compare_means(method = "wilcox.test", 
  #                            paired = TRUE,
  #                            labe.x = 2,
  #                            label = "p.signif", 
  #                            size = 6) +
  ggplot2::scale_color_manual(values = c("Pre" = "lightgrey",
                                          "Pre-pre" = "darkgrey")) +
  ggplot2::labs(
    x = "",
    y = "Proportion of influence\nfrom matched body part"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    legend.position = "none",
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_blank()
  )

ggplot2::ggsave(
  file.path(banc.fig2.extra.path, "pre_effector_matched_proportion_by_level.pdf"),
  p_matched_prop,
  width = 4,
  height = 3,
  dpi = 300
)

# Write accompanying stats for the paired dotplot
pre_vals <- paired_data %>% dplyr::filter(level_order == 1) %>% dplyr::arrange(target) %>% dplyr::pull(matched_proportion)
prepre_vals <- paired_data %>% dplyr::filter(level_order == 2) %>% dplyr::arrange(target) %>% dplyr::pull(matched_proportion)
pre_vals <- pre_vals[!is.na(pre_vals)]
prepre_vals <- prepre_vals[!is.na(prepre_vals)]
n_pairs <- min(length(pre_vals), length(prepre_vals))
if (n_pairs < 2) {
  message(sprintf("Skipping paired Wilcoxon: only %d valid pairs", n_pairs))
  wt <- list(statistic = NA_real_, p.value = NA_real_)
} else {
  wt <- wilcox.test(pre_vals[seq_len(n_pairs)], prepre_vals[seq_len(n_pairs)], paired = TRUE)
}
n_pairs <- length(pre_vals)
n_increase <- sum(prepre_vals > pre_vals)
n_decrease <- sum(prepre_vals < pre_vals)
n_tie <- sum(prepre_vals == pre_vals)
r_rb <- 1 - (2 * wt$statistic) / (n_pairs * (n_pairs + 1) / 2)

# Effect size interpretation
effect_mag <- dplyr::case_when(
  abs(r_rb) < 0.1 ~ "negligible",
  abs(r_rb) < 0.3 ~ "small",
  abs(r_rb) < 0.5 ~ "medium",
  TRUE             ~ "large"
)

legend_stmt <- sprintf(
  paste0("A paired Wilcoxon signed-rank test showed that the proportion of influence ",
         "from the matched body part was significantly higher for pre-effector neurons ",
         "than pre-pre-effector neurons (V=%.1f, p=%s, rank-biserial r=%.3f, %s effect; ",
         "n=%d body part pairs; %d/%d pairs showed a decrease from pre to pre-pre)."),
  wt$statistic, fmt_p_value(wt$p.value), r_rb, effect_mag,
  n_pairs, n_decrease, n_pairs
)

stats_path <- file.path(banc.fig2.extra.path, "pre_effector_matched_proportion_by_level.txt")
writeLines(c(
  "Paired Wilcoxon signed-rank test: Pre vs Pre-pre matched proportion",
  strrep("=", 70),
  sprintf("Date: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  sprintf("N pairs (body parts): %d", n_pairs),
  sprintf("Test: paired Wilcoxon signed-rank (single test, no correction needed)"),
  "",
  "Group summaries",
  strrep("-", 70),
  sprintf("Pre      — median: %.4f, IQR: %.4f, range: [%.4f, %.4f]",
          median(pre_vals), IQR(pre_vals),
          min(pre_vals), max(pre_vals)),
  sprintf("Pre-pre  — median: %.4f, IQR: %.4f, range: [%.4f, %.4f]",
          median(prepre_vals), IQR(prepre_vals),
          min(prepre_vals), max(prepre_vals)),
  "",
  "Test results",
  strrep("-", 70),
  sprintf("V = %.1f", wt$statistic),
  sprintf("p = %s", fmt_p_value(wt$p.value)),
  sprintf("Rank-biserial r = %.3f (%s effect)", r_rb, effect_mag),
  "",
  sprintf("Pairs where Pre > Pre-pre: %d / %d", n_decrease, n_pairs),
  sprintf("Pairs where Pre-pre > Pre: %d / %d", n_increase, n_pairs),
  sprintf("Ties: %d / %d", n_tie, n_pairs),
  "",
  strrep("=", 70),
  "FIGURE LEGEND (copy-paste ready)",
  strrep("=", 70),
  legend_stmt,
  strrep("=", 70)
), stats_path)
cat("Saved paired dotplot stats to:", stats_path, "\n")

# Plot 1b: Paired dotplot of UNmatched proportion — pre vs pre-pre
paired_data_um <- paired_data %>%
  dplyr::mutate(unmatched_proportion = 1 - matched_proportion)

p_unmatched_prop <- ggplot2::ggplot(paired_data_um,
                                     ggplot2::aes(x = level_label, y = unmatched_proportion)) +
  ggplot2::geom_line(ggplot2::aes(group = target), alpha = 0.3, color = "grey50") +
  ggplot2::geom_point(ggplot2::aes(color = level_label), size = 2.5) +
  ggpubr::stat_compare_means(method = "wilcox.test",
                              paired = TRUE,
                              label = "p.format",
                              size = 5) +
  ggplot2::scale_color_manual(values = c("Pre" = "lightgrey",
                                          "Pre-pre" = "darkgrey")) +
  ggplot2::labs(
    x = "",
    y = "Proportion of influence\nfrom unmatched body parts"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    legend.position = "none",
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_blank()
  )

ggplot2::ggsave(
  file.path(banc.fig2.extra.path, "pre_effector_unmatched_proportion_by_level.pdf"),
  p_unmatched_prop,
  width = 5,
  height = 5,
  dpi = 300
)

# Stats for unmatched dotplot
pre_um <- 1 - pre_vals
prepre_um <- 1 - prepre_vals
wt_um <- wilcox.test(pre_um, prepre_um, paired = TRUE)
r_rb_um <- 1 - (2 * wt_um$statistic) / (n_pairs * (n_pairs + 1) / 2)

writeLines(c(
  "Paired Wilcoxon signed-rank test: Pre vs Pre-pre unmatched proportion",
  sprintf("  N pairs (body parts): %d", n_pairs),
  sprintf("  Pre    — median: %.4f, IQR: [%.4f, %.4f]",
          median(pre_um), quantile(pre_um, 0.25), quantile(pre_um, 0.75)),
  sprintf("  Pre-pre — median: %.4f, IQR: [%.4f, %.4f]",
          median(prepre_um), quantile(prepre_um, 0.25), quantile(prepre_um, 0.75)),
  "",
  sprintf("  V statistic: %.1f", wt_um$statistic),
  sprintf("  p-value: %s", format.pval(wt_um$p.value, digits = 3)),
  sprintf("  Effect size (rank-biserial r): %.3f", r_rb_um),
  "",
  sprintf("  Pairs where Pre < Pre-pre: %d / %d", sum(prepre_um > pre_um), n_pairs),
  sprintf("  Pairs where Pre > Pre-pre: %d / %d", sum(prepre_um < pre_um), n_pairs),
  sprintf("  Ties: %d / %d", sum(prepre_um == pre_um), n_pairs)
), file.path(banc.fig2.extra.path, "pre_effector_unmatched_proportion_by_level_stats.txt"))
cat("Saved unmatched dotplot stats to:", file.path(banc.fig2.extra.path, "pre_effector_unmatched_proportion_by_level_stats.txt"), "\n")

# Plot 2: Shannon entropy across levels
p_entropy <- ggplot2::ggplot(summary_metrics,
                               ggplot2::aes(x = level_order, y = entropy_median)) +
  ggplot2::geom_line(size = 1, color = paper.cols[["highlight"]]) +
  ggplot2::geom_point(size = 3, color = paper.cols[["highlight"]]) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = entropy_q25,
                                       ymax = entropy_q75),
                         width = 0.1, color = paper.cols[["highlight"]]) +
  ggplot2::scale_x_continuous(breaks = 1:3,
                               labels = c("Pre", "Pre-pre", "Pre-pre-pre")) +
  # Add pairwise test labels between adjacent levels
  ggplot2::geom_text(data = pairwise_entropy_tests,
                     ggplot2::aes(x = x_pos, y = y_pos, label = sig_label),
                     size = 4, inherit.aes = FALSE) +
  ggplot2::labs(
    x = "Network level",
    y = "Shannon entropy (bits)",
    title = "Influence diversity increases with network distance",
    subtitle = "Median (IQR); Holm-adjusted pairwise tests between adjacent levels"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    plot.title = ggplot2::element_text(face = "bold")
  )

ggplot2::ggsave(
  file.path(banc.fig2.extra.path, "pre_effector_entropy_by_level.pdf"),
  p_entropy,
  width = 7,
  height = 5,
  dpi = 300
)

# Plot 3: Effect size (rank-biserial) across levels with significance labels
p_effect_size <- ggplot2::ggplot(summary_metrics,
                                  ggplot2::aes(x = level_order, y = rank_biserial)) +
  ggplot2::geom_line(size = 1, color = paper.cols[["highlight"]]) +
  ggplot2::geom_point(size = 3, color = paper.cols[["highlight"]]) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  # Add significance labels next to each point
  ggplot2::geom_text(ggplot2::aes(label = sig_label),
                     hjust = -0.5, size = 4, color = "black") +
  ggplot2::scale_x_continuous(breaks = 1:3,
                               labels = c("Pre", "Pre-pre", "Pre-pre-pre")) +
  ggplot2::labs(
    x = "Network level",
    y = "Effect size (rank-biserial correlation)",
    title = "Matched body part advantage diminishes with network distance",
    subtitle = "Wilcoxon test: diagonal > off-diagonal (Holm-adjusted)"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    plot.title = ggplot2::element_text(face = "bold")
  )

ggplot2::ggsave(
  file.path(banc.fig2.extra.path, "pre_effector_effect_size_by_level.pdf"),
  p_effect_size,
  width = 7,
  height = 5,
  dpi = 300
)

##############################################################################
## MANUSCRIPT-READY TEXT SUMMARY                                           ##
##############################################################################

# Generate text summary
summary_lines <- c(
  "Influence Spreading Analysis: Pre-effector levels",
  sprintf("Date: %s\n", format(Sys.time(), "%Y-%m-%d")),
  "ANALYSIS 1: Diagonal vs Off-diagonal (Wilcoxon)",
  ""
)

for (level_name in names(all_level_stats)) {
  stats <- all_level_stats[[level_name]]
  summary_lines <- c(summary_lines,
    sprintf("%s: diag n=%d median=%.3f | off-diag n=%d median=%.3f | p=%s %s | r=%.3f",
            stats$level, stats$n_diagonal, stats$median_diagonal,
            stats$n_off_diagonal, stats$median_off_diagonal,
            fmt_p(stats$wilcox_p), sig_symbol(stats$wilcox_p), stats$rank_biserial)
  )
}

summary_lines <- c(summary_lines, "",
  "ANALYSIS 2: Influence concentration", "")

for (i in 1:nrow(summary_metrics)) {
  row <- summary_metrics[i, ]
  summary_lines <- c(summary_lines,
    sprintf("%s: matched=%.3f (IQR %.3f-%.3f) | entropy=%.3f (IQR %.3f-%.3f) bits",
            row$level_display, row$matched_proportion_median,
            row$matched_proportion_q25, row$matched_proportion_q75,
            row$entropy_median, row$entropy_q25, row$entropy_q75)
  )
}

summary_lines <- c(summary_lines, "",
  sprintf("Effect size: %.3f→%.3f | Matched prop: %.1f%%→%.1f%% | Entropy: %.2f→%.2f bits",
          summary_metrics$rank_biserial[1], summary_metrics$rank_biserial[nrow(summary_metrics)],
          summary_metrics$matched_proportion_median[1]*100,
          summary_metrics$matched_proportion_median[nrow(summary_metrics)]*100,
          summary_metrics$entropy_median[1], summary_metrics$entropy_median[nrow(summary_metrics)])
)

# Write summary to file
writeLines(summary_lines,
           file.path(banc.fig2.extra.path, "pre_effector_influence_spreading_analysis.txt"))

##############################################################################
## NEW: Body part similarity vs % total influence by pre-level             ##
##############################################################################

# Calculate cosine similarity between effector body parts based on their
# influence profiles (recalculated in-script from banc.efferent_influence)

# Get the effector-to-effector influence matrix (no pre- prefix)
eff_base <- banc.efferent_influence %>%
  dplyr::filter(id %in% banc.eff.ids, !grepl("pre", seed)) %>%
  dplyr::mutate(
    target = dplyr::case_when(target %in% names(target.map) ~ target.map[target], TRUE ~ target),
    seed   = dplyr::case_when(seed   %in% names(target.map) ~ target.map[seed],   TRUE ~ seed)
  ) %>%
  dplyr::filter(!is.na(seed), !is.na(target)) %>%
  dplyr::group_by(seed, target) %>%
  dplyr::summarise(val = sum(influence_original, na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = target, values_from = val, values_fill = 0)

eff_mat <- as.matrix(eff_base[, -1])
rownames(eff_mat) <- eff_base$seed
bp_names_eff <- rownames(eff_mat)
n_bp_eff <- length(bp_names_eff)

# Cosine similarity between effector body parts
cos_sim_mat <- matrix(NA, nrow = n_bp_eff, ncol = n_bp_eff,
                      dimnames = list(bp_names_eff, bp_names_eff))
for (i in seq_len(n_bp_eff)) {
  for (j in seq_len(n_bp_eff)) {
    vi <- eff_mat[i, ]
    vj <- eff_mat[j, ]
    dp <- sum(vi * vj, na.rm = TRUE)
    mi <- sqrt(sum(vi^2, na.rm = TRUE))
    mj <- sqrt(sum(vj^2, na.rm = TRUE))
    cos_sim_mat[i, j] <- if (mi > 0 && mj > 0) dp / (mi * mj) else 0
  }
}

# For each pre-level, build data: body part similarity vs % of total influence
# Each dot = one cell from the influence matrix (seed_clean x target)
sim_vs_pct_data <- data.frame()
pre_level_names <- c("pre_effector", "pre_pre_effector", "pre_pre_pre_effector")
pre_level_labels <- c("Pre", "Pre-pre", "Pre-pre-pre")
pre_level_prefixes <- c("pre_", "pre_pre_", "pre_pre_pre_")
pre_level_excludes <- c("pre_pre", "pre_pre_pre", NA)

for (li in seq_along(pre_level_names)) {
  level_label <- pre_level_labels[li]
  pfx <- pre_level_prefixes[li]
  exc <- pre_level_excludes[li]

  # Get per-neuron influence for this level
  lev_dat <- banc.efferent_influence %>%
    dplyr::filter(id %in% banc.eff.ids) %>%
    dplyr::mutate(
      target = dplyr::case_when(target %in% names(target.map) ~ target.map[target], TRUE ~ target)
    ) %>%
    dplyr::filter(grepl(paste0("^", pfx), seed))
  if (!is.na(exc)) lev_dat <- lev_dat %>% dplyr::filter(!grepl(exc, seed))
  lev_dat <- lev_dat %>%
    dplyr::mutate(seed_clean = gsub(paste0("^", pfx), "", seed)) %>%
    dplyr::mutate(
      seed_clean = dplyr::case_when(
        seed_clean %in% names(target.map) ~ target.map[seed_clean], TRUE ~ seed_clean)
    ) %>%
    dplyr::filter(!is.na(seed_clean), !is.na(target))

  # Aggregate: total influence per (seed_clean, target) cell
  cell_vals <- lev_dat %>%
    dplyr::group_by(seed_clean, target) %>%
    dplyr::summarise(influence = sum(influence_original, na.rm = TRUE), .groups = "drop")

  # Normalise per seed_clean: % of that group's total influence going to each target
  cell_vals <- cell_vals %>%
    dplyr::group_by(seed_clean) %>%
    dplyr::mutate(pct_influence = 100 * influence / sum(influence, na.rm = TRUE)) %>%
    dplyr::ungroup()

  # Look up cosine similarity between seed body part and target body part
  for (ri in seq_len(nrow(cell_vals))) {
    bp_seed <- cell_vals$seed_clean[ri]
    bp_tgt  <- cell_vals$target[ri]
    if (bp_seed %in% bp_names_eff && bp_tgt %in% bp_names_eff) {
      sim_vs_pct_data <- rbind(sim_vs_pct_data, data.frame(
        level = level_label,
        seed_body_part = bp_seed,
        target_body_part = bp_tgt,
        cosine_similarity = cos_sim_mat[bp_seed, bp_tgt],
        pct_influence = cell_vals$pct_influence[ri],
        stringsAsFactors = FALSE
      ))
    }
  }
}

sim_vs_pct_data$level <- factor(sim_vs_pct_data$level, levels = pre_level_labels)

# Plot
p_sim_pct <- ggplot2::ggplot(sim_vs_pct_data,
                              ggplot2::aes(x = cosine_similarity, y = pct_influence,
                                           color = level)) +
  ggplot2::geom_smooth(method = "lm", se = TRUE, alpha = 0.2, linewidth = 1) +
  ggplot2::geom_point(alpha = 0.3, size = 1.5) +
  ggplot2::scale_color_manual(values = c(
    "Pre" = paper.cols[["highlight"]],
    "Pre-pre" = paper.cols[["ascending"]],
    "Pre-pre-pre" = paper.cols[["descending"]]
  )) +
  ggplot2::labs(
    x = "Body part similarity\n(cosine similarity of effector influence profiles)",
    y = "% of total influence to body part effector",
    color = "Network level"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    legend.position = "bottom"
  )

ggplot2::ggsave(
  file.path(banc.fig2.extra.path, "pre_effector_similarity_vs_influence_by_level.pdf"),
  p_sim_pct,
  width = 8,
  height = 6,
  dpi = 300
)

# ---- Statistical test: do the three curves differ in shape? ----
# ANCOVA with interaction tests whether slopes differ across levels.
# Main effect of level tests whether intercepts differ.
sim_stats_lines <- character()
sim_stats_lines <- c(sim_stats_lines,
  "Statistical comparison of regression lines: similarity vs % influence by level",
  paste0("Date: ", Sys.Date()),
  paste0("N observations: ", nrow(sim_vs_pct_data)),
  ""
)

# Fit full model with interaction
fit_full <- stats::lm(pct_influence ~ cosine_similarity * level, data = sim_vs_pct_data)
fit_no_interaction <- stats::lm(pct_influence ~ cosine_similarity + level, data = sim_vs_pct_data)

# ANOVA: does the interaction term improve the fit (i.e., slopes differ)?
interaction_test <- stats::anova(fit_no_interaction, fit_full)
sim_stats_lines <- c(sim_stats_lines,
  "=== ANCOVA: do regression slopes differ by level? ===",
  "(Tests whether the cosine_similarity x level interaction is significant)",
  ""
)
sim_stats_lines <- c(sim_stats_lines, utils::capture.output(print(interaction_test)), "")

# Type III ANOVA on the full model
anova_3 <- car::Anova(fit_full, type = "III")
sim_stats_lines <- c(sim_stats_lines,
  "=== Type-III ANOVA table (full model) ===",
  ""
)
sim_stats_lines <- c(sim_stats_lines, utils::capture.output(print(anova_3)), "")

# Per-level regression summaries
sim_stats_lines <- c(sim_stats_lines, "=== Per-level linear regression summaries ===", "")
for (lv in pre_level_labels) {
  sub_dat <- sim_vs_pct_data[sim_vs_pct_data$level == lv, ]
  fit_lv <- stats::lm(pct_influence ~ cosine_similarity, data = sub_dat)
  s <- summary(fit_lv)
  sim_stats_lines <- c(sim_stats_lines,
    paste0("--- Level: ", lv, " (n = ", nrow(sub_dat), ") ---"),
    paste0("  Slope     = ", round(stats::coef(fit_lv)[["cosine_similarity"]], 4)),
    paste0("  Intercept = ", round(stats::coef(fit_lv)[["(Intercept)"]], 4)),
    paste0("  R-squared = ", round(s$r.squared, 4)),
    paste0("  Adj R-sq  = ", round(s$adj.r.squared, 4)),
    paste0("  F-stat    = ", round(s$fstatistic[1], 4),
           ", p = ", format.pval(stats::pf(s$fstatistic[1], s$fstatistic[2],
                                            s$fstatistic[3], lower.tail = FALSE), digits = 4)),
    ""
  )
}

# Pairwise slope comparisons via emtrends
if (requireNamespace("emmeans", quietly = TRUE)) {
  emt <- emmeans::emtrends(fit_full, pairwise ~ level, var = "cosine_similarity")
  sim_stats_lines <- c(sim_stats_lines,
    "=== Pairwise slope comparisons (emmeans::emtrends) ===",
    ""
  )
  sim_stats_lines <- c(sim_stats_lines,
    "Estimated slopes per level:",
    utils::capture.output(print(emt$emtrends)),
    "",
    "Pairwise contrasts (slope differences):",
    utils::capture.output(print(emt$contrasts)),
    ""
  )
} else {
  sim_stats_lines <- c(sim_stats_lines,
    "Note: emmeans package not available; pairwise slope comparisons skipped.",
    ""
  )
}

sim_stats_path <- file.path(banc.fig2.extra.path,
                             "pre_effector_similarity_vs_influence_by_level_stats.txt")
writeLines(sim_stats_lines, sim_stats_path)
cat("Saved similarity vs influence stats to:", sim_stats_path, "\n")


##############################################
## Effector-to-effector influence           ##
## (upstream seeds = effectors with CNS output) ##
##############################################

# Identify effector cell types with significant CNS output:
# at least one neuron of that cell_type has a unitary connection (count >= 10)
# to any downstream cell type within the CNS.
cat("\n=== Effector CNS output analysis ===\n")
eff_output_ct <- bc.elist %>%
  dplyr::filter(pre %in% banc.eff.ids) %>%
  dplyr::left_join(banc.eff.meta %>%
                     dplyr::distinct(root_id, body_part_effector),
                   by = c("pre" = "root_id")) %>%
  dplyr::filter(!is.na(post_cell_type), post_cell_type != "") %>%
  dplyr::rename(cell_type = pre_cell_type,
                cell_class = pre_cell_class,
                cell_sub_class = pre_cell_sub_class) %>%
  dplyr::group_by(pre, cell_type, cell_class, cell_sub_class, body_part_effector,
                  post_cell_type) %>%
  dplyr::summarise(total_count = sum(count, na.rm = TRUE), .groups = "drop") %>%
  dplyr::filter(total_count >= 10)

# Qualifying cell types: cell_type must have (a) at least one member with a
# unitary connection >= 10, AND (b) at least one member with >= 100 total
# CNS output synapses.
.eff_neuron_output <- bc.elist %>%
  dplyr::filter(pre %in% banc.eff.ids) %>%
  dplyr::rename(cell_type = pre_cell_type) %>%
  dplyr::group_by(pre, cell_type) %>%
  dplyr::summarise(neuron_cns_output = sum(count, na.rm = TRUE), .groups = "drop") %>%
  dplyr::filter(neuron_cns_output >= 100) %>%
  dplyr::distinct(cell_type)
cns_output_cell_types <- intersect(
  unique(eff_output_ct$cell_type),
  .eff_neuron_output$cell_type
)

# Root IDs for qualifying cell types (all members, not just those with output)
cns_output_eff_ids <- banc.eff.meta %>%
  dplyr::filter(cell_type %in% cns_output_cell_types) %>%
  dplyr::pull(root_id) %>%
  unique()

cat(sprintf("Effector cell types with CNS output (unitary >= 10 + total >= 100): %d / %d\n",
            length(cns_output_cell_types),
            dplyr::n_distinct(banc.eff.meta$cell_type)))
cat(sprintf("Effector neurons in qualifying types: %d / %d\n",
            length(cns_output_eff_ids), length(banc.eff.ids)))

# Build sidecar: qualifying cell types ranked by total synaptic output + cell class
cns_output_summary <- eff_output_ct %>%
  dplyr::group_by(cell_type, cell_class, cell_sub_class, body_part_effector) %>%
  dplyr::summarise(
    n_neurons_with_output = dplyr::n_distinct(pre),
    n_downstream_cell_types = dplyr::n_distinct(post_cell_type),
    total_synaptic_output = sum(total_count, na.rm = TRUE),
    max_unitary_connection = max(total_count, na.rm = TRUE),
    top_targets = paste(head(sort(unique(post_cell_type)), 5), collapse = "; "),
    .groups = "drop"
  ) %>%
  dplyr::left_join(
    banc.eff.meta %>% dplyr::count(cell_type, name = "n_total_neurons"),
    by = "cell_type"
  ) %>%
  dplyr::arrange(cell_class, desc(total_synaptic_output))

sidecar_lines <- c(
  sprintf("Effector cell types with CNS output (member with unitary connection >= 10 + member with total output >= 100)"),
  sprintf("Date: %s", format(Sys.time(), "%Y-%m-%d %H:%M")),
  sprintf("Qualifying cell types: %d", length(cns_output_cell_types)),
  sprintf("Qualifying neurons: %d / %d total effectors", length(cns_output_eff_ids), length(banc.eff.ids)),
  "",
  format_table_txt(cns_output_summary)
)
writeLines(sidecar_lines,
           file.path(banc.fig2.extra.path, "effector_cns_output_cell_types.txt"))
cat("Wrote sidecar: effector_cns_output_cell_types.txt\n")

# Compute influence from qualifying effectors to all effectors
# Group qualifying effectors by body_part_effector for seed construction
cns_output_eff_by_bp <- banc.eff.meta %>%
  dplyr::filter(cell_type %in% cns_output_cell_types) %>%
  split(.$body_part_effector)
cns_output_bps <- names(cns_output_eff_by_bp)

cat(sprintf("Computing effector→effector influence for %d body parts...\n",
            length(cns_output_bps)))
eff_eff_influence_list <- vector("list", length(cns_output_bps))
for (i in seq_along(cns_output_bps)) {
  bp <- cns_output_bps[i]
  seed_ids <- unique(cns_output_eff_by_bp[[bp]]$root_id)
  try({
    inf_df <- calculate_influence_py(ic_banc, seed_ids) %>%
      dplyr::filter(id %in% banc.eff.ids) %>%
      dplyr::left_join(.eff_target_lookup, by = "id")
    inf_df$seed <- bp
    inf_df$influence_norm_original <- inf_df$`Influence_score_(unsigned)` / length(seed_ids)
    inf_df$influence_original <- inf_df$`Influence_score_(unsigned)`
    eff_eff_influence_list[[i]] <- inf_df
  })
}
eff_eff_influence <- as.data.frame(
  data.table::rbindlist(eff_eff_influence_list, fill = TRUE)
)

# Plot: effector (with CNS output) → effector influence heatmap
# Matches dimensions and ordering of pre-effector/pre-pre-effector panels,
# with non-qualifying body parts greyed out (NA).
# Iterate over both influence metrics.
all_bps <- unique(target.map)

# Remap seed/target labels and normalise
eff_eff_normed <- eff_eff_influence %>%
  dplyr::mutate(
    target = dplyr::case_when(target %in% names(target.map) ~ target.map[target], TRUE ~ target),
    seed = dplyr::case_when(seed %in% names(target.map) ~ target.map[seed], TRUE ~ seed)
  ) %>%
  dplyr::filter(!is.na(seed), !is.na(target)) %>%
  calculate_influence_norms()

# Map qualifying body parts (those with seed data)
qualifying_bps <- unique(eff_eff_normed$seed)
non_qualifying_bps <- setdiff(all_bps, qualifying_bps)
cat(sprintf("Qualifying seed body parts: %s\n", paste(qualifying_bps, collapse = ", ")))
cat(sprintf("Greyed-out body parts: %s\n", paste(non_qualifying_bps, collapse = ", ")))

for (inf.metric in c("influence_log", "influence_log_minmax")) {

  # Build matrix: rows = target body parts, cols = seed body parts
  eff_eff_matrix <- reshape2::acast(
    eff_eff_normed, target ~ seed, value.var = inf.metric,
    fun.aggregate = function(x) mean(x, na.rm = TRUE)
  )

  # Expand to full dimensions matching pre-effector layout
  full_matrix <- matrix(NA, nrow = length(all_bps), ncol = length(all_bps),
                        dimnames = list(all_bps, all_bps))
  shared_rows <- intersect(all_bps, rownames(eff_eff_matrix))
  shared_cols <- intersect(all_bps, colnames(eff_eff_matrix))
  full_matrix[shared_rows, shared_cols] <- eff_eff_matrix[shared_rows, shared_cols]

  # Ensure non-qualifying COLUMNS are NA (greyed out)
  if (length(non_qualifying_bps) > 0) {
    full_matrix[, intersect(non_qualifying_bps, colnames(full_matrix))] <- NA
  }

  # For minmax metrics: row-wise re-normalise on non-NA values so each
  # target row reaches 1.0 within the qualifying seeds
  if (grepl("minmax", inf.metric)) {
    for (r in seq_len(nrow(full_matrix))) {
      vals <- full_matrix[r, ]
      finite_vals <- vals[is.finite(vals) & !is.na(vals)]
      if (length(finite_vals) >= 2) {
        mn <- min(finite_vals); mx <- max(finite_vals)
        if (mx > mn) {
          full_matrix[r, ] <- ifelse(!is.na(vals), (vals - mn) / (mx - mn), NA)
        }
      }
    }
  }

  # Colour scale
  n_breaks <- 100
  if (grepl("minmax", inf.metric)) {
    scaled_heatmap_breaks <- seq(0, 1, length.out = n_breaks)
  } else {
    vals_finite <- full_matrix[is.finite(full_matrix) & !is.na(full_matrix)]
    scaled_heatmap_breaks <- seq(
      quantile(vals_finite, 0.01), quantile(vals_finite, 0.99),
      length.out = n_breaks)
  }
  scaled_heatmap_palette <- colorRampPalette(
    c("#1f4e79", "#4a90a4", "#7ba7bc", "#a67c8a", "#c4967d", "#b22222")
  )(n_breaks - 1)

  pheatmap(
    full_matrix,
    color = scaled_heatmap_palette,
    breaks = scaled_heatmap_breaks,
    na_col = "grey85",
    cluster_rows = FALSE,
    cluster_cols = FALSE,
    show_rownames = TRUE,
    show_colnames = TRUE,
    fontsize_row = 12,
    fontsize_col = 12,
    cellwidth = 12,
    cellheight = 12,
    main = sprintf("effector (CNS output) → effector: %s", inf.metric),
    filename = file.path(banc.fig2.extra.path,
      sprintf("effector_cns_output_to_effector_influence_%s.pdf", inf.metric))
  )
  cat(sprintf("Saved: effector_cns_output_to_effector_influence_%s.pdf\n", inf.metric))
}

write_anova_summary(eff_eff_normed %>%
                      dplyr::filter(!is.na(seed), !is.na(target)) %>%
                      dplyr::select(source = seed, target, value = influence_original),
                    file.path(banc.fig2.extra.path,
                              "effector_cns_output_to_effector_influence.txt"))
cat("Done: effector CNS output → effector influence heatmaps\n")

