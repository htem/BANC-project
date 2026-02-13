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
banc.meta <- banctable_query()

# paths
banc.fig1.path <- "figures/figure_1/links/"
banc.fig1.supp.path <- "figures/figure_1/links/supplement"
banc.fig1.anat.path <- "figures/figure_1/links/neuroanatomy"
banc.fig1.extra.path <- "figures/figure_1/links/extra"

###################
#### PROOFREAD ####
###################
regions <- c("optic_lobe","central_brain","ventral_nerve_cord","neck_connective")
lamina.types <- c("R1-6", "Lai")

# Helper: harmonize franken region labels to match BANC conventions
harmonize_franken_region <- function(region, super_class) {
  dplyr::case_when(
    grepl("ascending|descending", super_class) ~ "neck_connective",
    super_class %in% c("visual_centrifugal") ~ "central_brain",
    region %in% c("midbrain","brain","central_brain","sez") ~ "central_brain",
    region %in% c("vnc","ventral_nerve_cord") ~ "ventral_nerve_cord",
    grepl("optic", region) ~ "optic_lobe",
    TRUE ~ region
  )
}

# Get what the counts should be (Franken totals per region)
franken.count <- franken.meta %>%
  dplyr::filter(!grepl("glia|astrocyte|not_a_neuron",super_class)) %>%
  dplyr::arrange(dataset) %>%
  dplyr::distinct(fafb_id, manc_id, .keep_all = TRUE) %>%
  dplyr::mutate(region = harmonize_franken_region(region, super_class)) %>%
  dplyr::group_by(region) %>%
  dplyr::summarise(franken_count = dplyr::n(), .groups = "drop")

franken.count$franken_count[franken.count$region=="neck_connective"] <- 3693

# --- Compute lamina (unrecoverable) counts per region: Franken − BANC (>= 0) ----
# Harmonize regions for both datasets
franken_regions_norm <- franken.meta %>%
  dplyr::mutate(region = harmonize_franken_region(region, super_class))

banc_regions_norm <- banc.meta %>%
  dplyr::filter(!grepl("DEBRIS|MERGE|TADPOLE|GLIA|TRACHEA|NOT_A_NEURON",status),
                !super_class %in% c("glia","trachea","not_a_neuron","debris")) %>%
  dplyr::mutate(region = dplyr::case_when(
    super_class %in% c("visual_centrifugal") ~ "central_brain",
    region %in% c("midbrain","brain","central_brain") ~ "central_brain",
    region %in% c("vnc","ventral_nerve_cord") ~ "ventral_nerve_cord",
    region %in% c("optic") ~ "optic_lobe",
    grepl('neck_connective', region) ~ "neck_connective",
    grepl('CAN|FLA|GNG|AMMC|SAD|PRW',root_region) | region=="sez" ~ "central_brain",
    grepl('_midbrain_',root_region) ~ "central_brain",
    grepl('optic', root_region) ~ "optic_lobe",
    grepl('optic', region) ~ "optic_lobe",
    TRUE ~ region
  ))

franken_lamina_by_region <- franken_regions_norm %>%
  dplyr::filter(cell_type %in% lamina.types) %>%
  dplyr::count(region, name = "n_franken_lamina")

banc_lamina_by_region <- banc_regions_norm %>%
  dplyr::filter(cell_type %in% lamina.types) %>%
  dplyr::count(region, name = "n_banc_lamina")

lamina_missing_by_region <- dplyr::full_join(franken_lamina_by_region,
                                             banc_lamina_by_region,
                                             by = "region") %>%
  dplyr::mutate(
    n_franken_lamina = dplyr::coalesce(n_franken_lamina, 0L),
    n_banc_lamina    = dplyr::coalesce(n_banc_lamina,    0L),
    missing_count_lamina = pmax(n_franken_lamina - n_banc_lamina, 0L)
  ) %>%
  dplyr::select(region, missing_count_lamina)

# Process the data
processed_data <- banc.meta %>%
  dplyr::filter(!grepl("DEBRIS|MERGE|TADPOLE|GLIA|TRACHEA|NOT_A_NEURON",status),
                !super_class %in% c("glia","trachea","not_a_neuron","debris")) %>%
  dplyr::distinct(root_id, .keep_all = TRUE) %>%
  dplyr::mutate(region = dplyr::case_when(
    super_class %in% c("visual_centrifugal") ~ "central_brain",
    region %in% c("midbrain","brain","central_brain") ~ "central_brain",
    region %in% c("vnc","ventral_nerve_cord") ~ "ventral_nerve_cord",
    region %in% c("optic") ~ "optic_lobe",
    grepl('neck_connective', region) ~ "neck_connective",
    grepl('CAN|FLA|GNG|AMMC|SAD|PRW',root_region)|region=="sez" ~ "central_brain",
    grepl('_midbrain_',root_region) ~ "central_brain",
    grepl('optic', root_region) ~ "optic_lobe",
    grepl('optic', region) ~ "optic_lobe",
    TRUE ~ region
  )) %>%
  dplyr::mutate(region = ifelse(is.na(region),"undetermined",region),
                side = ifelse(is.na(side),"undetermined",side),
                proofread = ifelse(is.na(proofread),"FALSE",proofread),
                roughly_proofread = if ("roughly_proofread" %in% names(.))
                  ifelse(is.na(roughly_proofread),"FALSE",roughly_proofread)
                else "FALSE"
  ) %>%
  # Assign proofread status: proofread > roughly_proofread > FALSE
  dplyr::mutate(proof_status = dplyr::case_when(
    proofread == "TRUE" ~ "TRUE",
    roughly_proofread == "TRUE" & (grepl("sensory",super_class)|!is.na(nucleus_id)) ~ "ROUGHLY_PROOFREAD",
    TRUE ~ "FALSE"
  )) %>%
  dplyr::filter(region %in% regions) %>%
  dplyr::group_by(region) %>%
  dplyr::mutate(banc_proofread = sum(proof_status == "TRUE"),
                banc_roughly_proofread = sum(proof_status == "ROUGHLY_PROOFREAD"),
                banc_not_proofread = sum(proof_status == "FALSE")) %>%
  dplyr::distinct(region, banc_proofread, banc_roughly_proofread, banc_not_proofread) %>%
  dplyr::left_join(franken.count, by = "region") %>%
  dplyr::left_join(lamina_missing_by_region, by = "region") %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    missing_count_lamina = dplyr::coalesce(missing_count_lamina, 0L),
    # Expected total from Franken (partial datasets)
    expected_total = franken_count,
    # LAMINA (dark grey) = lamina neurons in Franken absent from BANC — unrecoverable
    lamina_count = pmin(missing_count_lamina,
                        pmax(expected_total - banc_proofread - banc_roughly_proofread, 0L)),
    # MISSING (light grey) = remaining gap to expected, after proofread + roughly + lamina
    missing_count = pmax(expected_total - banc_proofread - banc_roughly_proofread - lamina_count, 0L)
  ) %>%
  dplyr::distinct(region,
                  `TRUE`    = banc_proofread,
                  `ROUGHLY_PROOFREAD` = banc_roughly_proofread,
                  `MISSING` = missing_count,
                  `LAMINA`  = lamina_count,
                  expected_total) %>%
  reshape2::melt(id = c("region", "expected_total"),
                 value.name = "count",
                 variable.name = "proofread") %>%
  dplyr::group_by(region) %>%
  dplyr::mutate(
    proportion = count / expected_total,
    label = ifelse(count > 0, as.character(count), ""),
    ypos = cumsum(proportion) - proportion / 2
  ) %>%
  dplyr::mutate(label = ifelse(proportion < 0.1, NA, label)) %>%
  dplyr::ungroup()

# Plot
processed_data$region <- factor(processed_data$region, levels = rev(regions))
processed_data$proofread <- factor(processed_data$proofread, levels = c("LAMINA","MISSING","ROUGHLY_PROOFREAD","TRUE"))

# Compute max proportion for dynamic axis breaks and save width
max_prop <- max(tapply(processed_data$proportion, processed_data$region, sum))
proof_save_width <- 10 * max(max_prop, 1)

proof_stacked_bar <- ggplot(processed_data, aes(x = region, y = proportion, fill = proofread)) +
  geom_bar(stat = "identity", width = 0.7, color = "white") +
  geom_hline(yintercept = 1, color = "black", linewidth = 0.8) +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 8) +
  scale_y_continuous(breaks = seq(0, ceiling(max_prop * 4) / 4, by = 0.25),
                     labels = scales::percent_format()) +
  scale_fill_manual(values = paper.cols) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 1, size = 20),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20, unit = "pt")
  ) +
  labs(title = "", y = "proportion", fill = "") +
  coord_flip()

# Save
print(proof_stacked_bar)
ggsave(plot = proof_stacked_bar,
       filename = file.path(banc.fig1.path, "banc_proofread_overview.pdf"),
       width = proof_save_width, height = 4, dpi = 300, limitsize = FALSE)
ggsave(plot = convert_to_dark_mode(proof_stacked_bar),
       filename = file.path(banc.fig1.path, "dark_mode_banc_proofread_overview.pdf"),
       width = proof_save_width, height = 4, dpi = 300, limitsize = FALSE)

################################
#### PROOFREAD BY SUPER_CLASS ##
################################

# Franken expected counts by super_class and region
# For non-neck regions: standard dedup by id then fafb_id/manc_id
franken.sc.count.non_neck <- franken.meta %>%
  dplyr::filter(!grepl("glia|astrocyte|not_a_neuron",super_class),
                !grepl("ascending|descending",super_class)) %>%
  dplyr::arrange(dataset) %>%
  dplyr::distinct(id, .keep_all = TRUE) %>%
  dplyr::distinct(fafb_id, manc_id, .keep_all = TRUE) %>%
  dplyr::mutate(region = harmonize_franken_region(region, super_class)) %>%
  dplyr::filter(region %in% regions) %>%
  dplyr::group_by(region, super_class) %>%
  dplyr::summarise(franken_count = dplyr::n(), .groups = "drop")

# For neck connective: count only MANC entries to avoid double-counting
franken.sc.count.neck <- franken.meta %>%
  dplyr::filter(!grepl("glia|astrocyte|not_a_neuron",super_class),
                grepl("ascending|descending",super_class)) %>%
  dplyr::distinct(manc_id, .keep_all = TRUE) %>%
  dplyr::filter(!is.na(manc_id)) %>%
  dplyr::mutate(region = "neck_connective") %>%
  dplyr::group_by(region, super_class) %>%
  dplyr::summarise(franken_count = dplyr::n(), .groups = "drop")

franken.sc.count <- dplyr::bind_rows(franken.sc.count.non_neck, franken.sc.count.neck)

# Lamina counts by super_class and region
franken_lamina_sc <- franken_regions_norm %>%
  dplyr::filter(cell_type %in% lamina.types, region %in% regions) %>%
  dplyr::count(region, super_class, name = "n_franken_lamina")

banc_lamina_sc <- banc_regions_norm %>%
  dplyr::filter(cell_type %in% lamina.types, region %in% regions) %>%
  dplyr::count(region, super_class, name = "n_banc_lamina")

lamina_missing_sc <- dplyr::full_join(franken_lamina_sc, banc_lamina_sc,
                                       by = c("region","super_class")) %>%
  dplyr::mutate(
    n_franken_lamina = dplyr::coalesce(n_franken_lamina, 0L),
    n_banc_lamina    = dplyr::coalesce(n_banc_lamina, 0L),
    missing_count_lamina = pmax(n_franken_lamina - n_banc_lamina, 0L)
  ) %>%
  dplyr::select(region, super_class, missing_count_lamina)

# Process BANC data by super_class
sc_data <- banc.meta %>%
  dplyr::filter(!super_class %in% c("glia","trachea","not_a_neuron","debris")) %>%
  dplyr::distinct(root_id, .keep_all = TRUE) %>%
  dplyr::mutate(region = dplyr::case_when(
    super_class %in% c("visual_centrifugal") ~ "central_brain",
    region %in% c("midbrain","brain","central_brain") ~ "central_brain",
    region %in% c("vnc","ventral_nerve_cord") ~ "ventral_nerve_cord",
    region %in% c("optic") ~ "optic_lobe",
    grepl('neck_connective', region) ~ "neck_connective",
    grepl('CAN|FLA|GNG|AMMC|SAD|PRW',root_region)|region=="sez" ~ "central_brain",
    grepl('_midbrain_',root_region) ~ "central_brain",
    grepl('optic', root_region) ~ "optic_lobe",
    grepl('optic', region) ~ "optic_lobe",
    TRUE ~ region
  )) %>%
  dplyr::mutate(region = ifelse(is.na(region),"undetermined",region),
                proofread = ifelse(is.na(proofread),"FALSE",proofread),
                roughly_proofread = if ("roughly_proofread" %in% names(.))
                  ifelse(is.na(roughly_proofread),"FALSE",roughly_proofread)
                else "FALSE"
  ) %>%
  dplyr::mutate(proof_status = dplyr::case_when(
    proofread == "TRUE" ~ "TRUE",
    roughly_proofread == "TRUE" & (grepl("sensory",super_class)|!is.na(nucleus_id)) ~ "ROUGHLY_PROOFREAD",
    TRUE ~ "FALSE"
  )) %>%
  dplyr::filter(region %in% regions) %>%
  dplyr::group_by(region, super_class) %>%
  dplyr::summarise(banc_proofread = sum(proof_status == "TRUE"),
                   banc_roughly_proofread = sum(proof_status == "ROUGHLY_PROOFREAD"),
                   .groups = "drop") %>%
  dplyr::inner_join(franken.sc.count, by = c("region","super_class")) %>%
  dplyr::left_join(lamina_missing_sc, by = c("region","super_class")) %>%
  dplyr::mutate(
    missing_count_lamina = dplyr::coalesce(missing_count_lamina, 0L),
    expected_total = franken_count,
    lamina_count = pmin(missing_count_lamina,
                        pmax(expected_total - banc_proofread - banc_roughly_proofread, 0L)),
    missing_count = pmax(expected_total - banc_proofread - banc_roughly_proofread - lamina_count, 0L)
  )

# Reshape for stacking
sc_plot_data <- sc_data %>%
  dplyr::select(region, super_class,
                `TRUE` = banc_proofread,
                `ROUGHLY_PROOFREAD` = banc_roughly_proofread,
                `MISSING` = missing_count,
                `LAMINA` = lamina_count,
                expected_total) %>%
  reshape2::melt(id = c("region","super_class","expected_total"),
                 value.name = "count", variable.name = "proofread") %>%
  dplyr::mutate(
    proportion = count / expected_total,
    super_class = gsub("_"," ",super_class)
  )

sc_plot_data$proofread <- factor(sc_plot_data$proofread,
                                  levels = c("LAMINA","MISSING","ROUGHLY_PROOFREAD","TRUE"))
sc_plot_data$region <- factor(sc_plot_data$region, levels = regions)

proof_sc_bar <- ggplot(sc_plot_data, aes(x = reorder(super_class, proportion, FUN = sum),
                                          y = proportion, fill = proofread)) +
  geom_bar(stat = "identity", width = 0.7, color = "white") +
  geom_hline(yintercept = 1, color = "black", linewidth = 0.5) +
  scale_y_continuous(breaks = seq(0, ceiling(max(tapply(sc_plot_data$proportion,
                     paste(sc_plot_data$region, sc_plot_data$super_class), sum)) * 4) / 4, by = 0.25),
                     labels = scales::percent_format()) +
  scale_fill_manual(values = paper.cols) +
  facet_wrap(~ region, scales = "free_y") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 12, face = "bold")
  ) +
  labs(fill = "") +
  coord_flip()

print(proof_sc_bar)
ggsave(plot = proof_sc_bar,
       filename = file.path(banc.fig1.path, "banc_proofread_by_super_class.pdf"),
       width = 14, height = 10, dpi = 300)
ggsave(plot = convert_to_dark_mode(proof_sc_bar),
       filename = file.path(banc.fig1.path, "dark_mode_banc_proofread_by_super_class.pdf"),
       width = 14, height = 10, dpi = 300)

#################
#### MATCHED ####
#################

# Process the data (include proofread and roughly_proofread neurons)
processed_data <- banc.meta %>%
  dplyr::mutate(region = ifelse(is.na(region),"undetermined",region),
                side = ifelse(is.na(side),"undetermined",side),
                proofread = ifelse(is.na(proofread),"FALSE",proofread),
                roughly_proofread = ifelse(is.na(roughly_proofread),"FALSE",roughly_proofread),
                matched = dplyr::case_when(
                  !is.na(fafb_match) ~"MATCHED",
                  !is.na(manc_match) ~"MATCHED",
                  !is.na(hemibrain_match) ~"MATCHED",
                  !is.na(fanc_match) ~"MATCHED",
                  !is.na(cell_type)&cell_type!="" ~"MATCHED",
                  !is.na(fafb_nblast_match) ~"NBLAST",
                  !is.na(manc_nblast_match) ~"NBLAST",
                  !is.na(hemibrain_nblast_match) ~"NBLAST",
                  !is.na(fanc_nblast_match) ~"NBLAST",
                  TRUE ~ "UNMATCHED"
                )) %>%
  dplyr::filter(proofread == "TRUE" | roughly_proofread == "TRUE",
                region %in% regions,
                region != "undetermined") %>%
  dplyr::mutate(matched = ifelse(is.na(matched),"undetermined",matched),
                region = ifelse(is.na(region),"undetermined",region)) %>%
  dplyr::group_by(matched, region) %>%
  dplyr::summarise(count = dplyr::n()) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(region) %>%
  dplyr::mutate(
    proportion = count/sum(count),
    label = count,
    ypos = cumsum(proportion) - proportion/2
  ) %>%
  dplyr::mutate(label = ifelse(proportion<0.05,NA,label))

#  Define custom colors
match_colors <- c(
  "MATCHED" =paper.cols[["MATCHED"]],
  "UNMATCHED" = 'grey70',
  "NBLAST" = paper.cols[["NBLAST"]]
)

# Create the stacked bar plot
processed_data$region <- factor(processed_data$region, levels = rev(regions))
processed_data$matched <- factor(processed_data$matched, levels = rev(c("MATCHED","NBLAST","UNMATCHED")))
stacked_bar <- ggplot(processed_data, aes(x = region, y = proportion, fill = matched)) +
  geom_bar(stat = "identity", width = 0.7, color = "white") +
  geom_text(aes(label = label), 
            position = position_stack(vjust = 0.5),
            size = 8) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = match_colors) +
  theme_minimal() +
  theme(
    legend.position = "none", 
    axis.title = element_blank(),
    axis.text.y = element_blank(), #element_text(angle = 0, hjust = 1, size = 20),
    axis.text.x = element_text(angle = 0, hjust = 1, size = 20),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20, unit = "pt")
  ) +
  labs(title = "",
       y = "proportion",
       fill = "") +
  coord_flip()

# Display the plot
print(stacked_bar)

# Save
ggsave(plot = stacked_bar,
       filename = file.path(banc.fig1.path, "banc_proofread_match_overview.pdf"), 
       width = 10, height = 4, dpi = 300, limitsize = FALSE)
ggsave(plot = convert_to_dark_mode(stacked_bar),
       filename = file.path(banc.fig1.path, "dark_mode_banc_proofread_match_overview.pdf"), 
       width = 10, height = 4, dpi = 300, limitsize = FALSE)

########################
#### NBLAST RESULTS ####
########################

# FAFB validated
bc.fafb.matches.df.valid <- banc.meta %>%
  dplyr::distinct(pt_root_id = root_id,
                  pt_supervoxel_id = supervoxel_id,
                  pt_position = position,
                  query_id =  root_id,
                  match_id = fafb_match,
                  match_cell_type = fafb_cell_type) %>%
  dplyr::filter(!is.na(pt_root_id),!is.na(match_id)) %>%
  dplyr::mutate(valid = 't', score = 1)
fafb.matches.df.valid <- readr::read_csv(file=file.path(banc.meta.save.path,"banc_fafb_reviewed_matches.csv"),
                                         col_types = banc.col.types, 
                                         show_col_types = FALSE) %>%
  dplyr::mutate(score=0.9)
fafb.matches.df.valid <- rbind(bc.fafb.matches.df.valid,fafb.matches.df.valid) %>%
  dplyr::distinct(pt_root_id,
                  match_id,
                  .keep_all=TRUE)

# FAFB
fafb.matches.df.valid <- readr::read_csv(file=file.path(banc.meta.save.path,"banc_fafb_reviewed_matches.csv"),
                                         col_types = banc.col.types, 
                                         show_col_types = FALSE) %>%
  dplyr::mutate(score=0.9)
fafb.matches.df.valid <- rbind(bc.fafb.matches.df.valid,fafb.matches.df.valid) %>%
  dplyr::distinct(pt_root_id,
                  match_id,
                  .keep_all=TRUE)
banc.meta.fafb.nb <- suppressWarnings(readr::read_csv(file = file.path(banc.meta.save.path,"banc_fafb_783_nblast.csv"), 
                                                      col_types = banc.col.types,
                                                      show_col_types = FALSE)) %>%
  #rbind(fafb.matches.df.valid) %>%
  dplyr::arrange(dplyr::desc(score)) %>%
  dplyr::filter(!grepl("\\_",match_id), score > 0) %>%
  dplyr::mutate(dataset = "FAFB")

# Update ids
banc.meta.fafb.nb <- banc_updateids(banc.meta.fafb.nb,
                                    root.column = "pt_root_id",
                                    supervoxel.column = "pt_supervoxel_id",
                                    position.column = "pt_position")

# MANC
bc.manc.matches.df.valid <- banc.meta %>%
  dplyr::distinct(pt_root_id = root_id,
                  pt_supervoxel_id = supervoxel_id,
                  pt_position = position,
                  query_id =  root_id,
                  match_id = manc_match,
                  match_cell_type = manc_cell_type) %>%
  dplyr::filter(!is.na(pt_root_id),!is.na(match_id)) %>%
  dplyr::mutate(valid = 't', score = 1)
manc.matches.df.valid <- readr::read_csv(file=file.path(banc.meta.save.path,"banc_manc_reviewed_matches.csv"),
                                         col_types = banc.col.types, 
                                         show_col_types = FALSE) %>%
  dplyr::mutate(score=0.9)
manc.matches.df.valid <- rbind(bc.manc.matches.df.valid,manc.matches.df.valid) %>%
  dplyr::distinct(pt_root_id,
                  match_id,
                  .keep_all=TRUE)

# MANC
banc.meta.manc.nb <- suppressWarnings(readr::read_csv(file = file.path(banc.meta.save.path,"banc_manc_v1.2.1_nblast.csv"), 
                                                      col_types = banc.col.types,
                                                      show_col_types = FALSE)) %>%
  #rbind(manc.matches.df.valid) %>%
  dplyr::arrange(dplyr::desc(score)) %>%
  dplyr::filter(!grepl("\\_",match_id), 
                !is.na(pt_supervoxel_id), 
                !is.na(pt_root_id), 
                score > 0) %>%
  dplyr::mutate(dataset = "MANC")
banc.meta.manc.nb <- banc_updateids(banc.meta.manc.nb,
                                    root.column = "pt_root_id",
                                    supervoxel.column = "pt_supervoxel_id",
                                    position.column = "pt_position")

# # hemibrain
# bc.hemibrain.matches.df.valid <- banc.meta %>%
#   dplyr::distinct(pt_root_id = root_id,
#                   pt_supervoxel_id = supervoxel_id,
#                   pt_position = position,
#                   query_id =  root_id,
#                   match_id = hemibrain_match,
#                   match_cell_type = hemibrain_cell_type) %>%
#   dplyr::filter(!is.na(pt_root_id),!is.na(match_id)) %>%
#   dplyr::mutate(valid = 't', score = 1)
# hemibrain.matches.df.valid <- readr::read_csv(file=file.path(banc.meta.save.path,"banc_hemibrain_reviewed_matches.csv"),
#                                          col_types = banc.col.types, 
#                                          show_col_types = FALSE) %>%
#   dplyr::mutate(score=0.9)
# hemibrain.matches.df.valid <- rbind(bc.hemibrain.matches.df.valid,hemibrain.matches.df.valid) %>%
#   dplyr::distinct(pt_root_id,
#                   match_id,
#                   .keep_all=TRUE)

# hemibrain
banc.meta.hemibrain.nb <- suppressWarnings(readr::read_csv(file = file.path(banc.meta.save.path,"banc_hemibrain_v1.2.1_nblast.csv"), 
                                                           col_types = banc.col.types,
                                                           show_col_types = FALSE)) %>%
  dplyr::arrange(dplyr::desc(score)) %>%
  dplyr::filter(!grepl("\\_",match_id), 
                !is.na(pt_supervoxel_id), 
                !is.na(pt_root_id), 
                score > 0) %>%
  dplyr::mutate(dataset = "hemibrain")
banc.meta.hemibrain.nb <- banc_updateids(banc.meta.hemibrain.nb,
                                         root.column = "pt_root_id",
                                         supervoxel.column = "pt_supervoxel_id",
                                         position.column = "pt_position")

# FANC
banc.meta.fanc.nb <- suppressWarnings(readr::read_csv(file = file.path(banc.meta.save.path,"banc_fanc_1116_nblast.csv"), 
                                                      col_types = banc.col.types,
                                                      show_col_types = FALSE)) %>%
  dplyr::arrange(dplyr::desc(score)) %>%
  dplyr::filter(!grepl("\\_",match_id), 
                !is.na(pt_supervoxel_id), 
                !is.na(pt_root_id), 
                score > 0) %>%
  dplyr::mutate(dataset = "fanc")
banc.meta.fanc.nb <- banc_updateids(banc.meta.fanc.nb,
                                    root.column = "pt_root_id",
                                    supervoxel.column = "pt_supervoxel_id",
                                    position.column = "pt_position")

# maleCNS
banc.meta.malecns.nb <- suppressWarnings(readr::read_csv(file = file.path(banc.meta.save.path,"banc_malecns_v0.9_nblast.csv"),
                                                          col_types = banc.col.types,
                                                          show_col_types = FALSE)) %>%
  dplyr::arrange(dplyr::desc(score)) %>%
  dplyr::filter(!grepl("\\_",match_id),
                !is.na(pt_supervoxel_id),
                !is.na(pt_root_id),
                score > 0) %>%
  dplyr::mutate(dataset = "maleCNS")
banc.meta.malecns.nb <- banc_updateids(banc.meta.malecns.nb,
                                       root.column = "pt_root_id",
                                       supervoxel.column = "pt_supervoxel_id",
                                       position.column = "pt_position")


##############################
#### NBLAST DISTRIBUTIONS ####
##############################

# Get our calls
nblast.threshold.borderline <- 0.2
nblast.threshold <- 0.3
nblast.threshold.very_good <- 0.45

# Wrangle data
nblast.scores <- banc.meta.fafb.nb %>%
  rbind(banc.meta.manc.nb) %>%
  rbind(banc.meta.hemibrain.nb) %>%
  rbind(banc.meta.fanc.nb) %>%
  rbind(banc.meta.malecns.nb) %>%
  dplyr::arrange(dplyr::desc(score)) %>%
  dplyr::group_by(dataset, match_id) %>%
  dplyr::slice_head(n = 1) %>%
  dplyr::ungroup()

# Calculate normalized densities for each dataset
normalized_densities <- nblast.scores %>%
  group_by(dataset) %>%
  do({
    dens <- density(.$score)
    data.frame(
      x = dens$x,
      y = dens$y / max(dens$y)  # Normalize by dividing by the max density
    )
  }) %>%
  ungroup()

# Create the plot with normalized densities
g.ndense <- ggplot(normalized_densities, aes(x = x, y = y, color = dataset)) +
  geom_line(linewidth = 1.5) +
  theme_minimal() +
  labs(
    title = "",
    x = "normalised NBLAST score",
    y = "normalised density",
    color = "dataset"
  ) +
  scale_color_manual(values = paper.cols) +
  scale_x_continuous(breaks = seq(0, max(nblast.scores$score, na.rm = TRUE), by = 0.1)) +
  geom_vline(xintercept = 0, color = "grey80", linetype = "dashed", size = 1) +
  geom_vline(xintercept = nblast.threshold.borderline, color = "grey70", linetype = "dashed", size = 1) +
  geom_vline(xintercept = nblast.threshold, color = "grey40", linetype = "dashed", size = 1) +
  geom_vline(xintercept = nblast.threshold.very_good, color = "grey10", linetype = "dashed", size = 1) +
  annotate("text", x = nblast.threshold.borderline, y = Inf, label = "borderline", 
           vjust = 10, hjust = 1.1, size = 5, color = "grey70",fontface = "bold") +
  annotate("text", x = nblast.threshold, y = Inf, label = "good", 
           vjust = 5, hjust = 1.1, size = 5, color = "grey40", fontface = "bold") +
  annotate("text", x = nblast.threshold.very_good, y = Inf, label = "confident", 
           vjust = 1, hjust = 1.1, size = 5, color = "grey10", fontface = "bold") +
  theme(legend.position = "none",
        axis.title = element_text(size = 18, color = "black"),
        axis.text = element_text(size = 16, color = "black"))

# save
print(g.ndense)
ggsave(plot = g.ndense,
       filename = file.path(banc.fig1.supp.path,"nblast_score_distributions_by_dataset.pdf"), 
       width = 12, height = 4, dpi = 300)

##########################
#### FAFB NBLAST PLOT ####
##########################

# Get our calls
nblast.threshold.borderline <- 0.2
nblast.threshold <- 0.3
nblast.threshold.very_good <- 0.5

# Remove 'auto' prefix and empty entries
bc_cleaned <- banc.meta.fafb.nb %>%
  dplyr::arrange(dplyr::desc(score)) %>%
  dplyr::group_by(pt_root_id) %>%
  dplyr::slice_head(n = 1) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    assignments_match = dplyr::case_when(
      is.na(score) ~ "no_nblast",
      score < nblast.threshold.borderline ~ "bad",
      score >= nblast.threshold.very_good ~ "confident",
      score >= nblast.threshold ~ "good",
      score >= nblast.threshold.borderline ~ "borderline",
      TRUE ~ "bad"
    ),
  ) %>%
  dplyr::mutate(assignments_match=factor(assignments_match, 
                                         levels = c("no_nblast","bad","borderline","good","confident")))

# Define colours
col.values = c("no_nblast" = "lightgrey",
               "bad" = hemibrainr:::hemibrain_bright_colors[["cerise"]],
               "borderline"= hemibrainr:::hemibrain_bright_colors[["orange"]],
               "good" = hemibrainr:::hemibrain_bright_colors[["yellow"]],
               "confident" = hemibrainr:::hemibrain_bright_colors[["green"]])

# Calculate the percentage of matching and non-matching assignments
match_summary <- bc_cleaned %>%
  dplyr::group_by(assignments_match) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::mutate(percentage = count / sum(count) * 100)

# Plot 1: Stacked bar plot of matching and non-matching assignments
plot1 <- ggplot(match_summary, aes(x = "", y = percentage, fill = assignments_match)) +
  geom_bar(stat = "identity", width = 0.5) +
  coord_flip() +
  scale_fill_manual(values = col.values) +
  theme_minimal() +
  theme(legend.position = "none",
        strip.text = element_text(size = 10),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8)) +
  labs(
    title = "",
    x = "",
    y = "percentage",
    fill = "assignments match"
  ) +
  geom_text(aes(label = sprintf("%.1f%%\n(n=%s)", percentage, scales::comma(count))), 
            position = position_stack(vjust = 0.5), size = 3)

# Plot 2: Normalized density plot of fafb_top NBLAST scores
plot2 <- ggplot(bc_cleaned, aes(x = score, 
                                y= (..count..)/nrow(bc_cleaned))) +
  geom_density(alpha = 0.5, fill = hemibrainr:::hemibrain_bright_colors[["marine"]]) +
  labs(
    title = "",
    x = "BANC-FAFB NBLAST score",
    y = "density",
    fill = "assignments match"
  ) +
  theme_minimal() +
  theme(legend.position = "none",
        strip.text = element_text(size = 10),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8)) +
  scale_x_continuous(breaks = seq(0, max(bc_cleaned$score, na.rm = TRUE), by = 0.1)) +
  ggplot2::geom_vline(xintercept = 0, color = hemibrainr:::hemibrain_bright_colors[["cerise"]], linetype = "dashed", linewidth = 1) +
  ggplot2::geom_vline(xintercept = nblast.threshold.borderline, color = hemibrainr:::hemibrain_bright_colors[["orange"]], linetype = "dashed", linewidth = 1) +
  ggplot2::geom_vline(xintercept = nblast.threshold, color = hemibrainr:::hemibrain_bright_colors[["yellow"]], linetype = "dashed", linewidth = 1) +
  ggplot2::geom_vline(xintercept = nblast.threshold.very_good, color = hemibrainr:::hemibrain_bright_colors[["green"]], linetype = "dashed", linewidth = 1) +
  ggplot2::annotate("text", x = nblast.threshold.borderline, y = Inf, label = sprintf("borderline: >%s",nblast.threshold.borderline), 
                    size = 5, vjust = 2.2, hjust = 1.1, color = hemibrainr:::hemibrain_bright_colors[["orange"]]) +
  ggplot2::annotate("text", x = nblast.threshold, y = Inf, label = sprintf("good: >%s",nblast.threshold), 
                    size = 5, vjust = 2.2, hjust = 1.1, color = hemibrainr:::hemibrain_bright_colors[["yellow"]]) +
  ggplot2::annotate("text", x = nblast.threshold.very_good, y = Inf, label = sprintf("confident: >%s",nblast.threshold.very_good), 
                    size = 5, vjust = 2.2, hjust = 1.1, color = hemibrainr:::hemibrain_bright_colors[["green"]])

# Combine the plots into a single image
combined_plot <- gridExtra::grid.arrange(
  plot1,
  plot2,
  ncol = 1,
  heights = c(1, 2),
  top = ""
)
dark_mode_combined_plot <- gridExtra::grid.arrange(
  convert_to_dark_mode(plot1),
  convert_to_dark_mode(plot2),
  ncol = 1,
  heights = c(1, 2),
  top = ""
)

# Display the combined plot
print(combined_plot)

# Save the combined plot
ggsave(
  plot = combined_plot,
  filename = file.path(banc.fig1.extra.path, "fafb_nblast_match_banc.pdf"),
  width = 12, height = 4, dpi = 300
)
ggsave(
  plot = dark_mode_combined_plot,
  filename = file.path(banc.fig1.extra.path, "dark_mode_fafb_nblast_match_banc.pdf"),
  width = 12, height = 4, dpi = 300, bg = "transparent"
)

##########################
#### MANC NBLAST PLOT ####
##########################

# Get our calls
nblast.threshold.borderline <- 0.2
nblast.threshold <- 0.3
nblast.threshold.very_good <- 0.5

# Remove 'auto' prefix and empty entries
bc_cleaned <- banc.meta.manc.nb %>%
  dplyr::arrange(dplyr::desc(score)) %>%
  dplyr::group_by(pt_root_id) %>%
  dplyr::slice_head(n = 1) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    assignments_match = dplyr::case_when(
      is.na(score) ~ "no_nblast",
      score < nblast.threshold.borderline ~ "bad",
      score >= nblast.threshold.very_good ~ "confident",
      score >= nblast.threshold ~ "good",
      score >= nblast.threshold.borderline ~ "borderline",
      TRUE ~ "bad"
    ),
  ) %>%
  dplyr::mutate(assignments_match=factor(assignments_match, 
                                         levels = c("no_nblast","bad","borderline","good","confident")))

# Define colours
col.values = c("no_nblast" = "lightgrey",
               "bad" = hemibrainr:::hemibrain_bright_colors[["cerise"]],
               "borderline"= hemibrainr:::hemibrain_bright_colors[["orange"]],
               "good" = hemibrainr:::hemibrain_bright_colors[["yellow"]],
               "confident" = hemibrainr:::hemibrain_bright_colors[["green"]])

# Calculate the percentage of matching and non-matching assignments
match_summary <- bc_cleaned %>%
  dplyr::group_by(assignments_match) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::mutate(percentage = count / sum(count) * 100)

# Plot 1: Stacked bar plot of matching and non-matching assignments
plot1 <- ggplot(match_summary, aes(x = "", y = percentage, fill = assignments_match)) +
  geom_bar(stat = "identity", width = 0.5) +
  coord_flip() +
  scale_fill_manual(values = col.values) +
  theme_minimal() +
  theme(legend.position = "none",
        strip.text = element_text(size = 10),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8)) +
  labs(
    title = "",
    x = "",
    y = "percentage",
    fill = "assignments match"
  ) +
  geom_text(aes(label = sprintf("%.1f%%\n(n=%s)", percentage, scales::comma(count))), 
            position = position_stack(vjust = 0.5), size = 3)

# Plot 2: Normalized density plot of manc_top NBLAST scores
plot2 <- ggplot(bc_cleaned, aes(x = score, 
                                y= (..count..)/nrow(bc_cleaned))) +
  geom_density(alpha = 0.5, fill = hemibrainr:::hemibrain_bright_colors[["marine"]]) +
  labs(
    title = "",
    x = "BANC-MANC NBLAST score",
    y = "density",
    fill = "assignments match"
  ) +
  theme_minimal() +
  theme(legend.position = "none",
        strip.text = element_text(size = 10),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8)) +
  scale_x_continuous(breaks = seq(0, max(bc_cleaned$score, na.rm = TRUE), by = 0.1)) +
  ggplot2::geom_vline(xintercept = 0, color = hemibrainr:::hemibrain_bright_colors[["cerise"]], linetype = "dashed", linewidth = 1) +
  ggplot2::geom_vline(xintercept = nblast.threshold.borderline, color = hemibrainr:::hemibrain_bright_colors[["orange"]], linetype = "dashed", linewidth = 1) +
  ggplot2::geom_vline(xintercept = nblast.threshold, color = hemibrainr:::hemibrain_bright_colors[["yellow"]], linetype = "dashed", linewidth = 1) +
  ggplot2::geom_vline(xintercept = nblast.threshold.very_good, color = hemibrainr:::hemibrain_bright_colors[["green"]], linetype = "dashed", linewidth = 1) +
  ggplot2::annotate("text", x = nblast.threshold.borderline, y = Inf, label = sprintf("borderline: >%s",nblast.threshold.borderline), 
                    size = 5, vjust = 2.2, hjust = 1.1, color = hemibrainr:::hemibrain_bright_colors[["orange"]]) +
  ggplot2::annotate("text", x = nblast.threshold, y = Inf, label = sprintf("good: >%s",nblast.threshold), 
                    size = 5, vjust = 2.2, hjust = 1.1, color = hemibrainr:::hemibrain_bright_colors[["yellow"]]) +
  ggplot2::annotate("text", x = nblast.threshold.very_good, y = Inf, label = sprintf("confident: >%s",nblast.threshold.very_good), 
                    size = 5, vjust = 2.2, hjust = 1.1, color = hemibrainr:::hemibrain_bright_colors[["green"]])

# Combine the plots into a single image
combined_plot <- gridExtra::grid.arrange(
  plot1,
  plot2,
  ncol = 1,
  heights = c(1, 2),  # This sets the relative heights: 1/3 for plot1, 2/3 for plot2
  top = ""
)

# Display the combined plot
print(combined_plot)

# Save the combined plot
ggsave(
  plot = combined_plot,
  filename = file.path(banc.fig1.extra.path, "manc_nblast_match_banc.pdf"),
  width = 12, height = 4, dpi = 300
)
ggsave(
  plot = convert_to_dark_mode(combined_plot),
  filename = file.path(banc.fig1.extra.path, "dark_mode_manc_nblast_match_banc.pdf"),
  width = 12, height = 4, dpi = 300
)

###############################
#### hemibrain NBLAST PLOT ####
###############################

# Get our calls
nblast.threshold.borderline <- 0.2
nblast.threshold <- 0.3
nblast.threshold.very_good <- 0.5

# Remove 'auto' prefix and empty entries
bc_cleaned <- banc.meta.hemibrain.nb %>%
  dplyr::arrange(dplyr::desc(score)) %>%
  dplyr::group_by(pt_root_id) %>%
  dplyr::slice_head(n = 1) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    assignments_match = dplyr::case_when(
      is.na(score) ~ "no_nblast",
      score < nblast.threshold.borderline ~ "bad",
      score >= nblast.threshold.very_good ~ "confident",
      score >= nblast.threshold ~ "good",
      score >= nblast.threshold.borderline ~ "borderline",
      TRUE ~ "bad"
    ),
  ) %>%
  dplyr::mutate(assignments_match=factor(assignments_match, 
                                         levels = c("no_nblast","bad","borderline","good","confident")))

# Define colours
col.values = c("no_nblast" = "lightgrey",
               "bad" = hemibrainr:::hemibrain_bright_colors[["cerise"]],
               "borderline"= hemibrainr:::hemibrain_bright_colors[["orange"]],
               "good" = hemibrainr:::hemibrain_bright_colors[["yellow"]],
               "confident" = hemibrainr:::hemibrain_bright_colors[["green"]])

# Calculate the percentage of matching and non-matching assignments
match_summary <- bc_cleaned %>%
  dplyr::group_by(assignments_match) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::mutate(percentage = count / sum(count) * 100)

# Plot 1: Stacked bar plot of matching and non-matching assignments
plot1 <- ggplot(match_summary, aes(x = "", y = percentage, fill = assignments_match)) +
  geom_bar(stat = "identity", width = 0.5) +
  coord_flip() +
  scale_fill_manual(values = col.values) +
  theme_minimal() +
  theme(legend.position = "none",
        strip.text = element_text(size = 10),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8)) +
  labs(
    title = "",
    x = "",
    y = "percentage",
    fill = "assignments match"
  ) +
  geom_text(aes(label = sprintf("%.1f%%\n(n=%s)", percentage, scales::comma(count))), 
            position = position_stack(vjust = 0.5), size = 3)

# Plot 2: Normalized density plot of hemibrain_top NBLAST scores
plot2 <- ggplot(bc_cleaned, aes(x = score, 
                                y= (..count..)/nrow(bc_cleaned))) +
  geom_density(alpha = 0.5, fill = hemibrainr:::hemibrain_bright_colors[["marine"]]) +
  labs(
    title = "",
    x = "BANC-hemibrain NBLAST score",
    y = "density",
    fill = "assignments match"
  ) +
  theme_minimal() +
  theme(legend.position = "none",
        strip.text = element_text(size = 10),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8)) +
  scale_x_continuous(breaks = seq(0, max(bc_cleaned$score, na.rm = TRUE), by = 0.1)) +
  ggplot2::geom_vline(xintercept = 0, color = hemibrainr:::hemibrain_bright_colors[["cerise"]], linetype = "dashed", linewidth = 1) +
  ggplot2::geom_vline(xintercept = nblast.threshold.borderline, color = hemibrainr:::hemibrain_bright_colors[["orange"]], linetype = "dashed", linewidth = 1) +
  ggplot2::geom_vline(xintercept = nblast.threshold, color = hemibrainr:::hemibrain_bright_colors[["yellow"]], linetype = "dashed", linewidth = 1) +
  ggplot2::geom_vline(xintercept = nblast.threshold.very_good, color = hemibrainr:::hemibrain_bright_colors[["green"]], linetype = "dashed", linewidth = 1) +
  ggplot2::annotate("text", x = nblast.threshold.borderline, y = Inf, label = sprintf("borderline: >%s",nblast.threshold.borderline), 
                    size = 5, vjust = 2.2, hjust = 1.1, color = hemibrainr:::hemibrain_bright_colors[["orange"]]) +
  ggplot2::annotate("text", x = nblast.threshold, y = Inf, label = sprintf("good: >%s",nblast.threshold), 
                    size = 5, vjust = 2.2, hjust = 1.1, color = hemibrainr:::hemibrain_bright_colors[["yellow"]]) +
  ggplot2::annotate("text", x = nblast.threshold.very_good, y = Inf, label = sprintf("confident: >%s",nblast.threshold.very_good), 
                    size = 5, vjust = 2.2, hjust = 1.1, color = hemibrainr:::hemibrain_bright_colors[["green"]])

# Combine the plots into a single image
combined_plot <- gridExtra::grid.arrange(
  plot1,
  plot2,
  ncol = 1,
  heights = c(1, 2),  # This sets the relative heights: 1/3 for plot1, 2/3 for plot2
  top = ""
)

# Display the combined plot
print(combined_plot)

# Save the combined plot
ggsave(
  plot = combined_plot,
  filename = file.path(banc.fig1.extra.path, "hemibrain_nblast_match_banc.pdf"),
  width = 12, height = 4, dpi = 300
)

##########################
#### FANC NBLAST PLOT ####
##########################

# Get our calls
nblast.threshold.borderline <- 0.2
nblast.threshold <- 0.3
nblast.threshold.very_good <- 0.5

# Remove 'auto' prefix and empty entries
bc_cleaned <- banc.meta.fanc.nb %>%
  dplyr::arrange(dplyr::desc(score)) %>%
  dplyr::group_by(pt_root_id) %>%
  dplyr::slice_head(n = 1) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    assignments_match = dplyr::case_when(
      is.na(score) ~ "no_nblast",
      score < nblast.threshold.borderline ~ "bad",
      score >= nblast.threshold.very_good ~ "confident",
      score >= nblast.threshold ~ "good",
      score >= nblast.threshold.borderline ~ "borderline",
      TRUE ~ "bad"
    ),
  ) %>%
  dplyr::mutate(assignments_match=factor(assignments_match, 
                                         levels = c("no_nblast","bad","borderline","good","confident")))

# Define colours
col.values = c("no_nblast" = "lightgrey",
               "bad" = hemibrainr:::hemibrain_bright_colors[["cerise"]],
               "borderline"= hemibrainr:::hemibrain_bright_colors[["orange"]],
               "good" = hemibrainr:::hemibrain_bright_colors[["yellow"]],
               "confident" = hemibrainr:::hemibrain_bright_colors[["green"]])

# Calculate the percentage of matching and non-matching assignments
match_summary <- bc_cleaned %>%
  dplyr::group_by(assignments_match) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::mutate(percentage = count / sum(count) * 100)

# Plot 1: Stacked bar plot of matching and non-matching assignments
plot1 <- ggplot(match_summary, aes(x = "", y = percentage, fill = assignments_match)) +
  geom_bar(stat = "identity", width = 0.5) +
  coord_flip() +
  scale_fill_manual(values = col.values) +
  theme_minimal() +
  theme(legend.position = "none",
        strip.text = element_text(size = 10),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8)) +
  labs(
    title = "",
    x = "",
    y = "percentage",
    fill = "assignments match"
  ) +
  geom_text(aes(label = sprintf("%.1f%%\n(n=%s)", percentage, scales::comma(count))), 
            position = position_stack(vjust = 0.5), size = 3)

# Plot 2: Normalized density plot of fanc_top NBLAST scores
plot2 <- ggplot(bc_cleaned, aes(x = score, 
                                y= (..count..)/nrow(bc_cleaned))) +
  geom_density(alpha = 0.5, fill = hemibrainr:::hemibrain_bright_colors[["marine"]]) +
  labs(
    title = "",
    x = "BANC-FANC NBLAST score",
    y = "density",
    fill = "assignments match"
  ) +
  theme_minimal() +
  theme(legend.position = "none",
        strip.text = element_text(size = 10),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8)) +
  scale_x_continuous(breaks = seq(0, max(bc_cleaned$score, na.rm = TRUE), by = 0.1)) +
  ggplot2::geom_vline(xintercept = 0, color = hemibrainr:::hemibrain_bright_colors[["cerise"]], linetype = "dashed", linewidth = 1) +
  ggplot2::geom_vline(xintercept = nblast.threshold.borderline, color = hemibrainr:::hemibrain_bright_colors[["orange"]], linetype = "dashed", linewidth = 1) +
  ggplot2::geom_vline(xintercept = nblast.threshold, color = hemibrainr:::hemibrain_bright_colors[["yellow"]], linetype = "dashed", linewidth = 1) +
  ggplot2::geom_vline(xintercept = nblast.threshold.very_good, color = hemibrainr:::hemibrain_bright_colors[["green"]], linetype = "dashed", linewidth = 1) +
  ggplot2::annotate("text", x = nblast.threshold.borderline, y = Inf, label = sprintf("borderline: >%s",nblast.threshold.borderline), 
                    size = 5, vjust = 2.2, hjust = 1.1, color = hemibrainr:::hemibrain_bright_colors[["orange"]]) +
  ggplot2::annotate("text", x = nblast.threshold, y = Inf, label = sprintf("good: >%s",nblast.threshold), 
                    size = 5, vjust = 2.2, hjust = 1.1, color = hemibrainr:::hemibrain_bright_colors[["yellow"]]) +
  ggplot2::annotate("text", x = nblast.threshold.very_good, y = Inf, label = sprintf("confident: >%s",nblast.threshold.very_good), 
                    size = 5, vjust = 2.2, hjust = 1.1, color = hemibrainr:::hemibrain_bright_colors[["green"]])

# Combine the plots into a single image
combined_plot <- gridExtra::grid.arrange(
  plot1,
  plot2,
  ncol = 1,
  heights = c(1, 2),  
  top = ""
)

# Display the combined plot
print(combined_plot)

# Save the combined plot
ggsave(
  plot = combined_plot,
  filename = file.path(banc.fig1.extra.path, "fanc_nblast_match_banc.pdf"),
  width = 12, height = 4, dpi = 300
)
