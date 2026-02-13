###############
### NUMBERS ###
###############
source("R/startup/banc-startup.R")

# get metda data
franken.meta <- franken_meta()
banc.meta <- banctable_query()

# get neck_connective seed plane info
neck_connective_y92500 <- banc_cave_query("neck_connective_y92500", live = FALSE,
                                          version = 626)
neck_connective_y92500 <- neck_connective_y92500 %>%
  filter(valid == "t")

neck_connective_y121000 <- banc_cave_query("neck_connective_y121000", live = FALSE,
                                           version = 626)

neck_connective_y121000 <- neck_connective_y121000 %>%
  filter(valid == "t")

neck_connective_all <- bind_rows(neck_connective_y92500, neck_connective_y121000)

neck_connective_all <- neck_connective_all %>%
  distinct(pt_root_id, .keep_all = TRUE) %>%
  mutate(root_id = as.character(pt_root_id))

# prepare
df <- data.frame(identity = "influence_norm_thresh",
                 number = 17.18)
df <- rbind(df,
            data.frame(identity = "postsynaptic_detetions",
                       number = 218460852)
)
df <- rbind(df,
            data.frame(identity = "proportion_autapses",
                       number = 0.021)
)

# Proofread neurons
banc.proofread.count <- banc.meta %>% 
  dplyr::distinct(root_id, .keep_all = TRUE) %>%
  dplyr::filter(!super_class%in%c("glia","trachea","not_a_neuron"),
                !grepl("DEBRIS|NOT_A_NEURON|TRACHEA",status)) %>%
  dplyr::filter(proofread=="TRUE") %>%
  nrow()
df <- rbind(df,
            data.frame(identity = "proofread_neuron_count",
                       number = banc.proofread.count)
)

# Identified neurons
banc.identified.count <- banc.meta %>% 
  dplyr::distinct(root_id, .keep_all = TRUE) %>%
  dplyr::filter(!super_class%in%c("trachea","not_a_neuron"),
                !grepl("DEBRIS|NOT_A_NEURON|TRACHEA",status)) %>%
  dplyr::filter(proofread=="TRUE"|(roughly_proofread=="TRUE")&!is.na(nucleus_id)|(roughly_proofread=="TRUE")&!grepl("sensory",super_class)) %>%
  nrow()
df <- rbind(df,
            data.frame(identity = "identified_neuron_count",
                       number = banc.identified.count)
)


# Cell type count, total
banc.ct.count <- banc.meta %>% 
  dplyr::distinct(root_id, .keep_all = TRUE) %>%
  dplyr::filter(!super_class%in%c("glia","trachea","not_a_neuron")) %>%
  dplyr::filter(!is.na(cell_type)&cell_type!="") %>%
  nrow()
df <- rbind(df,
            data.frame(identity = "cell_typed_neuron_count",
                       number = banc.ct.count)
)

# Cell type count, central brain and VNC
banc.ct.cb.vnc.count <- banc.meta %>% 
  dplyr::distinct(root_id, .keep_all = TRUE) %>%
  dplyr::filter(!super_class%in%c("glia","trachea","not_a_neuron")) %>%
  dplyr::filter(!is.na(cell_type)&cell_type!="",
                region%in%c("central_brain","ventral_nerve_cord")) %>%
  nrow()
banc.ct.cb.vnc.prop <- banc.ct.cb.vnc.count/banc.meta %>% 
  dplyr::distinct(root_id, .keep_all = TRUE) %>%
  dplyr::filter(!super_class%in%c("glia","trachea","not_a_neuron")) %>%
  dplyr::filter(region%in%c("central_brain","ventral_nerve_cord")) %>%
  nrow()
df <- rbind(df,
            data.frame(identity = "cell_typed_neuron_proportion_cb_vnc",
                       number = banc.ct.cb.vnc.prop)
)

# proportion of cell typed neurons
banc.proofread.prop <- banc.ct.count/(166000-9390)
df <- rbind(df,
            data.frame(identity = "cell_typed_neuron_proportion",
                       number = banc.proofread.prop)
)

# removed
nt.removed.cts <- c('Tm9', 'Tm4', 'Tm2', 'Tm1', 'TmY20', 'Mi1', 'DVMn 1a-c', 
                    'TTMn', 'Fe reductor MN', 'b1 MN', 'MNwm36', 'Acc. ti flexor MN', 
                    'MNhl68','MNhm42', 'MNhm03', 'Tr flexor MN', 'Tr extensor MN', 'b3 MN',
                    'hg1 MN', 'hg3 MN', 'i1 MN', 'i2 MN', 'tp1 MN', 'tp2 MN', 'tpn MN',
                    'b2 MN', 'ltm2-femur MN', 'ltm1-tibia MN', 'ps2 MN',
                    'Ti flexor MN', 'Tergopleural/Pleural promotor MN', 'MNnm03',
                    'ps1 MN', 'MNhl65', 'MNhl73', 'MNml79')

# nt GT cell types in total
nt.ct.gt <- franken.meta %>% 
  dplyr::distinct(neuron_id, .keep_all = TRUE) %>%
  dplyr::filter(!is.na(neurotransmitter_verified),
                neurotransmitter_verified!="") %>%
  dplyr::distinct(cell_type, .keep_all = TRUE) %>%
  nrow()
df <- rbind(df,
            data.frame(identity = "nt_gt_cell_type_count",
                       number = nt.ct.gt-length(nt.removed.cts))
)

# nt GT neurons in total
banc.nt.n.gt <- banc.meta %>% 
  dplyr::distinct(root_id, .keep_all = TRUE) %>%
  dplyr::filter(!is.na(neurotransmitter_verified),
                neurotransmitter_verified!="",
                !cell_type %in% nt.removed.cts) %>%
  nrow()
df <- rbind(df,
            data.frame(identity = "banc_nt_neuron_count",
                       number = banc.nt.n.gt)
)

# nt GT cell types in total
banc.nt.ct.gt <- banc.meta %>% 
  dplyr::distinct(root_id, .keep_all = TRUE) %>%
  dplyr::filter(!is.na(neurotransmitter_verified),
                neurotransmitter_verified!="",
                !cell_type %in% nt.removed.cts) %>%
  dplyr::distinct(cell_type, .keep_all = TRUE) %>%
  nrow()
df <- rbind(df,
            data.frame(identity = "banc_nt_cell_type_count",
                       number = banc.nt.ct.gt)
)

# get numbers of ANs
an.count <- banc.meta %>% 
  dplyr::distinct(root_id, .keep_all = TRUE) %>%
  dplyr::filter(super_class=="ascending") %>%
  nrow()
df <- rbind(df,
            data.frame(identity = "an_neuron_count",
                       number = an.count)
)

# get numbers of unmatched ANs
an.not.count <- banc.meta %>% 
  dplyr::distinct(root_id, .keep_all = TRUE) %>%
  dplyr::filter(super_class=="ascending", is.na(manc_match)) %>%
  nrow()
df <- rbind(df,
            data.frame(identity = "an_neuron_not_matched_count",
                       number = an.not.count)
)

# get numbers of DNs
dn.count <- banc.meta %>% 
  dplyr::distinct(root_id, .keep_all = TRUE) %>%
  dplyr::filter(super_class=="descending") %>%
  nrow()
df <- rbind(df,
            data.frame(identity = "dn_neuron_count",
                       number = dn.count)
)

# get numbers of unmatched DNs
dn.not.count <- banc.meta %>% 
  dplyr::distinct(root_id, .keep_all = TRUE) %>%
  dplyr::filter(super_class=="descending", is.na(fafb_match)) %>%
  nrow()
df <- rbind(df,
            data.frame(identity = "dn_neuron_not_matched_count",
                       number = dn.not.count)
)


# get numbers of an cell types
an.ct.count <- banc.meta %>% 
  dplyr::distinct(root_id, .keep_all = TRUE) %>%
  dplyr::filter(super_class=="ascending") %>%
  dplyr::distinct(cell_type, .keep_all = TRUE) %>%
  nrow()
df <- rbind(df,
            data.frame(identity = "an_cell_type_count",
                       number = an.ct.count)
)

# get numbers of DN cell types
dn.ct.count <- banc.meta %>% 
  dplyr::distinct(root_id, .keep_all = TRUE) %>%
  dplyr::filter(super_class=="descending") %>%
  dplyr::distinct(cell_type, .keep_all = TRUE) %>%
  nrow()
df <- rbind(df,
            data.frame(identity = "dn_cell_type_count",
                       number = dn.ct.count)
)

# get numbers of sensory DNs
dn.sensory.count <- banc.meta %>% 
  dplyr::distinct(root_id, .keep_all = TRUE) %>%
  dplyr::filter(super_class=="sensory_descending") %>%
  nrow()
df <- rbind(df,
            data.frame(identity = "dn_sensory_count",
                       number = dn.sensory.count)
)

# get numbers of sensory DN cell types
dn.sensory.ct.count <- banc.meta %>% 
  dplyr::distinct(root_id, .keep_all = TRUE) %>%
  dplyr::filter(super_class=="sensory_descending") %>%
  dplyr::distinct(cell_type, .keep_all = TRUE) %>%
  nrow()
df <- rbind(df,
            data.frame(identity = "dn_sensory_cell_type_count",
                       number = dn.sensory.ct.count)
)

# get numbers of sensory ANs
an.sensory.count <- banc.meta %>% 
  dplyr::distinct(root_id, .keep_all = TRUE) %>%
  dplyr::filter(super_class=="sensory_ascending") %>%
  nrow()
df <- rbind(df,
            data.frame(identity = "an_sensory_count",
                       number = an.sensory.count)
)

# get numbers of sensory AN cell types
an.sensory.ct.count <- banc.meta %>% 
  dplyr::distinct(root_id, .keep_all = TRUE) %>%
  dplyr::filter(super_class=="sensory_ascending") %>%
  dplyr::distinct(cell_type, .keep_all = TRUE) %>%
  nrow()
df <- rbind(df,
            data.frame(identity = "an_sensory_cell_type_count",
                       number = an.sensory.ct.count)
)

# get numbers of ascending visceral circulatory cells
an.eff.count <- banc.meta %>% 
  dplyr::distinct(root_id, .keep_all = TRUE) %>%
  dplyr::filter(super_class=="ascending_visceral_circulatory") %>%
  nrow()
df <- rbind(df,
            data.frame(identity = "an_efferent_count",
                       number = an.eff.count)
)

# get numbers of ascending visceral circulatory cell types
an.eff.ct.count <- banc.meta %>% 
  dplyr::distinct(root_id, .keep_all = TRUE) %>%
  dplyr::filter(super_class=="ascending_visceral_circulatory") %>%
  dplyr::distinct(cell_type, .keep_all = TRUE) %>%
  nrow()
df <- rbind(df,
            data.frame(identity = "an_efferent_cell_type_count",
                       number = an.eff.ct.count)
)

# get numbers of neck motor neurons
neck.efferent.count <- banc.meta %>% 
  dplyr::distinct(root_id, .keep_all = TRUE) %>%
  dplyr::filter(super_class == "motor",
                grepl("neck",cell_class)|grepl("neck",cell_sub_class)) %>%
  nrow()
df <- rbind(df,
            data.frame(identity = "neck_efferent_count",
                       number = neck.efferent.count)
)

# get numbers of neck neuron segments
neck.count <- banc.meta %>% 
  dplyr::distinct(root_id, .keep_all = TRUE) %>%
  dplyr::filter(region=="neck_connective"|grepl("ascending",super_class)|grepl("descending",super_class)) %>%
  dplyr::filter(!super_class%in%c("glia","trachea","not_a_neuron")) %>%
  nrow()
cave_missing_bm <- neck_connective_all %>%
  filter(!(root_id %in% banc.meta$root_626)) %>%
  nrow()

df <- rbind(df,
            data.frame(identity = "neck_count_all",
                       number = neck.count + cave_missing_bm)
)

# get numbers of proofread neck neurons
neck.count <- banc.meta %>% 
  dplyr::distinct(root_id, .keep_all = TRUE) %>%
  dplyr::filter(region=="neck_connective"|grepl("ascending",super_class)|grepl("descending",super_class)) %>%
  dplyr::filter(!super_class%in%c("glia","trachea","not_a_neuron")) %>%
  dplyr::filter(proofread == TRUE) %>%
  nrow()
df <- rbind(df,
            data.frame(identity = "neck_proofread",
                       number = neck.count)
)

# get numbers of neurons
neuron.count <- banc.meta %>% 
  dplyr::distinct(root_id, .keep_all = TRUE) %>%
  dplyr::filter(!super_class%in%c("glia","trachea","not_a_neuron")) %>%
  nrow()
df <- rbind(df,
            data.frame(identity = "total_neuron_count",
                       number = neuron.count)
)

# get numbers of sensory neurons
sensory.count <- banc.meta %>% 
  dplyr::distinct(root_id, .keep_all = TRUE) %>%
  dplyr::filter(grepl("sensory",super_class)) %>%
  nrow()
df <- rbind(df,
            data.frame(identity = "total_sensory_count",
                       number = sensory.count)
)

# orphans
sensory.orphan.count <- banc.meta %>% 
  dplyr::distinct(root_id, .keep_all = TRUE) %>%
  dplyr::filter(grepl("sensory",super_class),
                grepl("orphan",cell_sub_class)|is.na(cell_sub_class)) %>%
  nrow()
df <- rbind(df,
            data.frame(identity = "orphan_sensory_count",
                       number = sensory.orphan.count)
)
df <- rbind(df,
            data.frame(identity = "sensory_used_count",
                       number = sensory.count-sensory.orphan.count)
)

# get numbers of efferent neurons
efferent.count <- banc.meta %>% 
  dplyr::distinct(root_id, .keep_all = TRUE) %>%
  dplyr::filter(grepl("motor|visceral",super_class)) %>%
  nrow()
df <- rbind(df,
            data.frame(identity = "total_efferent_count",
                       number = efferent.count)
)

# BANC KC outgoing connections
kc.out.count <- banc.meta %>% 
  dplyr::distinct(root_id, .keep_all = TRUE) %>%
  dplyr::filter(grepl("kenyon_cell",cell_class)) %>%
  dplyr::filter(output_connections!=0,
                !is.na(output_connections),
                output_connections!="")
kc.mean.count <- sum(kc.out.count$output_connections)/nrow(kc.out.count)
df <- rbind(df,
            data.frame(identity = "banc_mean_kc_output_connections",
                       number = kc.mean.count)
)

# # FAFB KC outgoing connections
# kc.out.count <- franken.meta %>% 
#   dplyr::distinct(neuron_id, .keep_all = TRUE) %>%
#   dplyr::filter(grepl("kenyon_cell",cell_class)) %>%
#   dplyr::filter(output_connections!=0,
#                 !is.na(output_connections),
#                 output_connections!="")
# kc.mean.count <- sum(kc.out.count$output_connections)/nrow(kc.out.count)
# df <- rbind(df,
#             data.frame(identity = "fafb_mean_kc_output_connections",
#                        number = kc.mean.count)
# )


# Synapse review
data <- read_csv(file.path(banc.path,"data","synapses","2024-09-20_aelysia_synapse_sample_complete.csv")) 
df <- rbind(df,
            data.frame(identity = "banc_reveiwed_postsynaptic_connections",
                       number = nrow(data))
)
df <- rbind(df,
            data.frame(identity = "banc_reveiwed_postsynaptic_neuropils",
                       number =length(unique(data$neuropil))/2)
)

# Nerves review
nerve.count <- banc.meta %>% 
  dplyr::distinct(root_id, .keep_all = TRUE) %>%
  dplyr::filter(!is.na(nerve), 
                nerve!="",
                grepl("nerve",nerve),
                grepl("_",nerve)) %>%
  dplyr::distinct(nerve) %>%
  nrow()
df <- rbind(df,
            data.frame(identity = "nerve_count",
                       number = nerve.count)
)

# Missing photoreceptors (R1-6 and ocellar)
photo.count <- franken.meta %>% 
  dplyr::distinct(neuron_id, .keep_all = TRUE) %>%
  dplyr::filter(grepl("photo|retin",cell_sub_class)) %>%
  dplyr::filter(!grepl("R7|R8", cell_type)) %>%
  nrow()
df <- rbind(df,
            data.frame(identity = "fafb_photo_count",
                       number = photo.count)
)


# Get UMAP coordinates
cns.umap <- readr::read_csv("data/cns_network/spectral_clustering_min_connection_strength_1_banc_version_626_cluster_count_13_cluster_seed_10_embedding_seed_3.csv", 
                            col_types = banc.col.types) %>%
  dplyr::select(root_id, 
                spectral_cluster, 
                UMAP1 = umap_x, 
                UMAP2 = umap_y) %>%
  dplyr::left_join(banc.meta, by = "root_id") %>%
  dplyr::mutate(cns_network = paste0("CNS_",str_pad(as.integer(spectral_cluster)+1,width=2,pad="0"))) %>%
  dplyr::filter(!is.na(cns_network)) %>%
  dplyr::filter(!super_class%in%c("glia","sensory", "sensory_ascending","motor",
                                  "visceral_circulatory","not_a_neuron","visual_centrifugal","visual_projection"),
                !is.na(super_class))
df <- rbind(df,
            data.frame(identity = "cns_network_neuron_count",
                       number = nrow(cns.umap))
)

# Proportion of neurons in cluster
cns.umap.prop.intrinsic <- nrow(cns.umap)/banc.meta %>% 
  dplyr::distinct(root_id, .keep_all = TRUE) %>%
  dplyr::filter(region%in%c("central_brain","ventral_nerve_cord"),
                flow=="intrinsic") %>%
  nrow()
df <- rbind(df,
            data.frame(identity = "cns_network_neuron_prop",
                       number = cns.umap.prop.intrinsic)
)

# AN/DN metadata
banc.an.dn.meta <- banc.meta %>%
  dplyr::filter(super_class %in% c("ascending","descending")) %>%
  dplyr::filter(!grepl("^SA|^SN|^AN_4|AN_5|^IN",cell_type))

# AN/Dn cell types of known function
neck.ct<-banc.an.dn.meta %>%
  dplyr::distinct(cell_type) %>%
  dplyr::pull(cell_type)
neck.ct.function <-banc.an.dn.meta %>%
  dplyr::filter(!is.na(cell_function))%>%
  dplyr::distinct(cell_type) %>%
  dplyr::pull(cell_type)
df <- rbind(df,
            data.frame(identity = "an_dn_function_prop",
                       number = round(length(neck.ct.function)/length(neck.ct),2))
)

# Missing types
fafb.cts <- franken.meta %>%
  dplyr::filter(!region%in%"neck_connective",
                flow!="efferent",dataset=="FAFB") %>%
  dplyr::distinct(cell_type) %>%
  dplyr::pull(cell_type)
manc.cts <- franken.meta %>%
  dplyr::filter(!region%in%"neck_connective",
                flow!="efferent",dataset=="MANC") %>%
  dplyr::distinct(cell_type) %>%
  dplyr::pull(cell_type)
banc.cts <- banc.meta %>%
  dplyr::filter(region!="neck_connective")
banc.cts <- unique(c(banc.cts$cell_type,banc.cts$fafb_cell_type,banc.cts$manc_cell_type))
fafb.cts.not.in.banc <- sum(!fafb.cts%in%banc.cts)
manc.cts.not.in.banc <- sum(!manc.cts%in%banc.cts)
fafb.ct.prop.missing <- fafb.cts.not.in.banc/length(fafb.cts)
manc.ct.prop.missing <- manc.cts.not.in.banc/length(manc.cts)
df <- rbind(df,
            data.frame(identity = "manc_ct_prop_missing",
                       number = manc.ct.prop.missing))
df <- rbind(df,
            data.frame(identity = "fafb_ct_prop_missing",
                       number = fafb.ct.prop.missing))
df <- rbind(df,
            data.frame(identity = "manc_ct_count_missing",
                       number = manc.cts.not.in.banc))
df <- rbind(df,
            data.frame(identity = "fafb_ct_count_missing",
                       number = fafb.cts.not.in.banc))


# Flow percentages by region
flow_by_region <- banc.meta %>%
  dplyr::distinct(root_id, .keep_all = TRUE) %>%
  dplyr::filter(!super_class %in% c("glia", "trachea", "not_a_neuron")) %>%
  dplyr::filter(!is.na(region), region != "", !is.na(flow), region != "brain") %>%
  dplyr::group_by(region) %>%
  dplyr::summarise(
    n_total = dplyr::n(),
    n_afferent = sum(grepl("afferent", flow), na.rm = TRUE),
    n_efferent = sum(grepl("efferent", flow), na.rm = TRUE),
    n_intrinsic = sum(grepl("intrinsic", flow), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    pct_afferent = n_afferent / n_total,
    pct_efferent = n_efferent / n_total,
    pct_intrinsic = n_intrinsic / n_total
  )

for (i in seq_len(nrow(flow_by_region))) {
  r <- flow_by_region$region[i]
  df <- rbind(df,
              data.frame(identity = paste0("flow_afferent_pct_", r),
                         number = flow_by_region$pct_afferent[i]))
  df <- rbind(df,
              data.frame(identity = paste0("flow_efferent_pct_", r),
                         number = flow_by_region$pct_efferent[i]))
  df <- rbind(df,
              data.frame(identity = paste0("flow_intrinsic_pct_", r),
                         number = flow_by_region$pct_intrinsic[i]))
}

# save
readr::write_csv(x = df %>%
                   dplyr::distinct(),
                 "manuscript/resubmission_2/numbers.csv")










