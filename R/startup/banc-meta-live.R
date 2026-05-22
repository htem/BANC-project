###############################################################################
# banc-meta-live.R
#
# Live BANC metadata pipeline: queries SeaTable for the most-recent manual
# curations, reads the GCS-resident segmentation-properties feather, joins
# them, and applies the column-coalescing priority documented in CLAUDE.md
# (SeaTable > GCS > franken for manual curations; GCS > SeaTable > franken
# for `proofread`, since that's a segmentation property).
#
# The live block at the top of this file is GUARDED: it runs only if
# `banc.meta` is not yet defined, or if BANC_LIVE=1 forces a refresh.
# The dispatcher at R/startup/banc-meta.R loads a committed parquet
# snapshot first and skips the live block in that case.
#
# When the live block does run, it writes a freshly-dated parquet
# snapshot at data/meta/banc_888_meta_<YYYYMMDD>.parquet at the end of
# the block (so the next source() can fast-path through the dispatcher).
#
# The derived setup at the tail of this file (per-class subsets,
# ordering vectors, seed/effector maps, classes.dn.df, umap.dn.df, etc.)
# runs UNCONDITIONALLY — both the snapshot fast-path and the live path
# rely on it.
###############################################################################

.live_needed <- !exists("banc.meta") || identical(Sys.getenv("BANC_LIVE"), "1")
if (.live_needed) {

# Query main BANC metadata table (used by all paths for merging latest annotations)
# Always re-query SeaTable on each source() to pick up the latest annotations.
# (Removed idempotency guard 2026-04-11 — the guard caused stale bc.orig to
# persist across re-sources, masking updated super_cluster labels.)
{
  # Quick connectivity check (5s timeout) to avoid hanging on unreachable SeaTable
  .seatable_ok <- tryCatch({
    h <- curl::new_handle(connecttimeout = 5, timeout = 10)
    curl::handle_setopt(h, http_version = 2L)  # force HTTP/1.1 — SeaTable's CSP header breaks HTTP/2 framing
    curl::curl_fetch_memory("https://cloud.seatable.io/api/v2.1/ping/", handle = h)
    TRUE
  }, error = function(e) FALSE)
  .bc_cache <- "data/meta/bc_orig_cache.feather"
  if (.seatable_ok) {
    bc.orig <- tryCatch(banctable_query(), error = function(e) {
      warning("SeaTable query failed: ", e$message, "\n  Continuing with GCS-only metadata")
      data.frame()
    })
    # Cache for offline use
    if (is.data.frame(bc.orig) && nrow(bc.orig) > 0) {
      tryCatch({
        if (!dir.exists(dirname(.bc_cache))) dir.create(dirname(.bc_cache), recursive = TRUE)
        arrow::write_feather(bc.orig, .bc_cache)
        message("Cached SeaTable data to ", .bc_cache, " (", nrow(bc.orig), " rows)")
      }, error = function(e) NULL)
    }
  } else {
    # Try loading from cache
    if (file.exists(.bc_cache)) {
      message("SeaTable unreachable — loading cached SeaTable data from ", .bc_cache)
      bc.orig <- tryCatch(arrow::read_feather(.bc_cache), error = function(e) data.frame())
      if (nrow(bc.orig) > 0) message("Loaded ", nrow(bc.orig), " rows from cache")
    } else {
      message("SeaTable unreachable and no cache found — using GCS-only metadata (no live annotation enrichment)")
      bc.orig <- data.frame()
    }
  }
  rm(.seatable_ok, .bc_cache)
}

banc.meta.gcs.ok <- FALSE
if (exists("banc.gcs.bucket") && !is.null(banc.gcs.bucket)) {
  # GCS feather (primary path for v746+)
  tryCatch({
  message("Loading BANC metadata from GCS...")
  if (!exists("gcs") || is.null(gcs)) gcs <- setup_gcs_filesystem()
  meta_path <- construct_path(banc.gcs.bucket, banc.gcs.dataset, "meta")
  banc.meta <- read_feather_smart(meta_path, gcs_filesystem = gcs)
  banc.meta.gcs.ok <- TRUE
  # GCS feather uses versioned ID column (e.g., banc_746_id) — normalize to root_id
  version_col <- grep("^banc_[0-9]+_id$", colnames(banc.meta), value = TRUE)
  if (length(version_col) == 1 && !"root_id" %in% colnames(banc.meta)) {
    banc.meta$root_id <- banc.meta[[version_col]]
  }
  # Root IDs must always be character (Arrow feather may read int64)
  banc.meta$root_id <- as.character(banc.meta$root_id)
  banc.meta$id <- banc.meta$root_id
  # Ensure supervoxel_id is character for joins with SeaTable data
  banc.meta$supervoxel_id <- as.character(banc.meta$supervoxel_id)
  if (nrow(bc.orig) > 0 && "supervoxel_id" %in% colnames(bc.orig)) {
    bc.orig$supervoxel_id <- as.character(bc.orig$supervoxel_id)
  }
  # Ensure columns needed for cell_sub_type exist (as NA) before enrichment
  for (.gc in c("fafb_cell_type", "manc_cell_type", "neuromere")) {
    if (!.gc %in% colnames(banc.meta)) banc.meta[[.gc]] <- NA_character_
  }
  rm(.gc)
  # GCS feather is a slim export — enrich from SeaTable (bc.orig has full annotations)
  gcs_cols <- setdiff(colnames(banc.meta), c("supervoxel_id"))
  st_cols <- setdiff(colnames(bc.orig), c(gcs_cols, "_id"))
  if (length(st_cols) > 0 && nrow(bc.orig) > 0 && "supervoxel_id" %in% colnames(bc.orig)) {
    banc.meta <- banc.meta %>%
      dplyr::left_join(bc.orig %>%
                         dplyr::select(supervoxel_id, dplyr::any_of(st_cols)) %>%
                         dplyr::distinct(supervoxel_id, .keep_all = TRUE),
                       by = "supervoxel_id")
  }
  # Ensure match columns are character for downstream joins with franken.meta
  for (.mc in c("manc_match", "fafb_match", "hemibrain_match", "fanc_match")) {
    if (.mc %in% colnames(banc.meta)) banc.meta[[.mc]] <- as.character(banc.meta[[.mc]])
  }
  # cell_sub_type: cell_type disambiguated by neuromere when members of the same
  # cell_type span multiple neuromeres (e.g. duplicated motor neurons in T1/T2/T3).
  # NA cell_types fall back to id; otherwise just cell_type.
  if ("neuromere" %in% colnames(banc.meta)) {
    banc.meta <- banc.meta %>%
      dplyr::group_by(cell_type) %>%
      dplyr::mutate(.multi_neuromere = dplyr::n_distinct(neuromere[!is.na(neuromere) & neuromere != ""]) > 1) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(cell_sub_type = dplyr::case_when(
        is.na(cell_type) ~ id,
        .multi_neuromere & !is.na(neuromere) & neuromere != "" ~ paste0(cell_type, "_", neuromere),
        TRUE ~ cell_type
      )) %>%
      dplyr::select(-.multi_neuromere)
  } else {
    banc.meta <- banc.meta %>%
      dplyr::mutate(cell_sub_type = dplyr::if_else(is.na(cell_type), id, cell_type))
  }
  # Merge latest SeaTable annotations for columns that may have been updated since GCS snapshot
  if (nrow(bc.orig) > 0) {
    banc.meta <- banc.meta %>%
      dplyr::left_join(bc.orig %>%
                         dplyr::select(supervoxel_id, st_cell_type = cell_type, st_status = status,
                                       st_proofread = proofread, st_cluster = cluster,
                                       st_manual_cluster = manual_cluster, st_super_cluster = super_cluster,
                                       st_side = side) %>%
                         dplyr::distinct(supervoxel_id, .keep_all = TRUE),
                       by = "supervoxel_id") %>%
      dplyr::mutate(
        cell_type = dplyr::coalesce(gsub("auto:", "", st_cell_type), cell_type),
        status = dplyr::coalesce(st_status, if ("status" %in% names(.)) status else NA_character_),
        # GCS proofread takes priority over potentially stale SeaTable cache
        proofread = dplyr::coalesce(if ("proofread" %in% names(.)) proofread else NA_character_, st_proofread),
        cluster = dplyr::coalesce(st_cluster, cluster),
        manual_cluster = dplyr::coalesce(st_manual_cluster, if ("manual_cluster" %in% names(.)) manual_cluster else NA_character_),
        # SeaTable super_cluster takes priority (2026-04-11 reverted) — SeaTable
        # has the current labels; GCS feather is stale. The .super_cluster_remap
        # at line ~406 catches any remaining old names from either source.
        super_cluster = dplyr::coalesce(st_super_cluster, if ("super_cluster" %in% names(.)) super_cluster else NA_character_),
        # SeaTable side is much more populated than GCS (which is mostly NA in v850)
        side = dplyr::coalesce(st_side, if ("side" %in% names(.)) side else NA_character_)
      ) %>%
      dplyr::select(-starts_with("st_"))

    # Fallback: if supervoxel_id join failed (stale cache from different materialization),
    # transfer proofread via cell_type mode
    n_proofread <- sum(banc.meta$proofread == TRUE | banc.meta$proofread == "TRUE", na.rm = TRUE)
    n_bc_proofread <- sum(bc.orig$proofread == TRUE, na.rm = TRUE)
    if (n_proofread < n_bc_proofread * 0.5) {
      message(sprintf("Supervoxel_id join recovered only %d/%d proofread — falling back to cell_type transfer",
                      n_proofread, n_bc_proofread))
      # Build per-cell_type modal proofread from bc.orig
      ct_proofread <- bc.orig %>%
        dplyr::filter(!is.na(cell_type), cell_type != "", !grepl("^auto:", cell_type)) %>%
        dplyr::mutate(cell_type = gsub("^auto:", "", cell_type)) %>%
        dplyr::group_by(cell_type) %>%
        dplyr::summarise(
          ct_proofread = any(proofread == TRUE, na.rm = TRUE),
          .groups = "drop"
        )
      banc.meta <- banc.meta %>%
        dplyr::left_join(ct_proofread, by = "cell_type") %>%
        dplyr::mutate(
          proofread = dplyr::case_when(
            !is.na(proofread) & (proofread == TRUE | proofread == "TRUE") ~ TRUE,
            !is.na(ct_proofread) ~ ct_proofread,
            TRUE ~ FALSE
          ),
          roughly_proofread = dplyr::if_else(
            !is.na(cell_type) & cell_type != "" & !proofread,
            TRUE, FALSE
          )
        ) %>%
        dplyr::select(-ct_proofread)
      message(sprintf("After cell_type transfer: %d proofread, %d roughly_proofread",
                      sum(banc.meta$proofread == TRUE, na.rm = TRUE),
                      sum(banc.meta$roughly_proofread == TRUE, na.rm = TRUE)))
    }
  }

  # Normalize proofread to logical (v850 has boolean, v746 had character)
  if ("proofread" %in% colnames(banc.meta)) {
    banc.meta$proofread <- as.logical(banc.meta$proofread)
  }

  }, error = function(e) {
    message("GCS loading failed: ", e$message)
    message("Falling back to SeaTable/SQLite path...")
  })
}

if (!banc.meta.gcs.ok) {
  stop("GCS metadata loading failed. Cannot load banc.meta.\n",
       "Ensure gcs-helpers.R is sourced (source('R/startup/gcs-helpers.R')) ",
       "and that the GCS bucket is reachable.")
}

# Ensure all expected downstream columns exist (some come from frankenmeta which GCS path skips)
for (.col in c("neuropeptide_verified", "neurotransmitter_verified",
               "neurotransmitter", "neuromere",
               "fafb_cell_type", "manc_cell_type", "hemibrain_cell_type",
               "fanc_cell_type", "malecns_cell_type",
               "proofread", "roughly_proofread", "cell_sub_type",
               "fafb_match", "manc_match", "hemibrain_match", "fanc_match",
               "malecns_match",
               "fafb_nblast_match", "manc_nblast_match", "hemibrain_nblast_match", "fanc_nblast_match",
               "malecns_nblast_match",
               "manual_cluster", "super_cluster",
               "root_626", "root_850", "nucleus_id", "cell_function_detailed",
               "root_region", "root_position_nm", "status",
               "sexually_dimorphic")) {
  if (!.col %in% colnames(banc.meta)) {
    banc.meta[[.col]] <- NA_character_
  }
}
rm(.col)

# Normalize proofread to logical (may be character from SeaTable/SQLite path)
if ("proofread" %in% colnames(banc.meta) && !is.logical(banc.meta$proofread)) {
  banc.meta$proofread <- as.logical(banc.meta$proofread)
}
# Derive roughly_proofread if missing or all NA
if (all(is.na(banc.meta$roughly_proofread)) ||
    (is.character(banc.meta$roughly_proofread) && all(banc.meta$roughly_proofread %in% c(NA_character_, "")))) {
  banc.meta$roughly_proofread <- !banc.meta$proofread &
    !is.na(banc.meta$cell_type) & banc.meta$cell_type != ""
  message(sprintf("Derived roughly_proofread: %d neurons", sum(banc.meta$roughly_proofread, na.rm = TRUE)))
}
if (is.character(banc.meta$roughly_proofread)) {
  banc.meta$roughly_proofread <- as.logical(banc.meta$roughly_proofread)
}

# Seeds are computed after all enrichments (see assign_banc_seeds below)

# Get singular functions
cns.functions.singular <- cns.functions %>%
  dplyr::mutate(cell_function = dplyr::case_when(
    !is.na(modality)&modality!="" ~ modality,
    !is.na(behaviour)&behaviour!="" ~ behaviour,
    !is.na(response)&response!="" ~ response,
    !is.na(valence)&valence!="" ~ valence,
    TRUE ~ NA_character_
  )) %>%
  dplyr::distinct(cell_type,.keep_all = TRUE) %>%
  dplyr::distinct(cell_type, cell_function)

# Add body part sensory
if(!"body_part_sensory"%in%colnames(banc.meta) && nrow(bc.orig) > 0){
  banc.meta <- banc.meta %>%
    dplyr::left_join(bc.orig %>%
                       dplyr::distinct(supervoxel_id,
                                       body_part_sensory) %>%
                       dplyr::distinct(supervoxel_id, .keep_all = TRUE),
                     by = "supervoxel_id")
}
banc.meta <- banc.meta %>%
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
if(!"body_part_effector"%in%colnames(banc.meta) && nrow(bc.orig) > 0){
  banc.meta <- banc.meta %>%
    dplyr::left_join(bc.orig %>%
                       dplyr::distinct(supervoxel_id,
                                       body_part_effector) %>%
                       dplyr::distinct(supervoxel_id, .keep_all = TRUE),
                     by = "supervoxel_id")
}
if (nrow(bc.orig) > 0) {
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
} # end if (nrow(bc.orig) > 0) enrichment block

# Assign visual cell functions for retina/lamina photoreceptors
banc.meta <- banc.meta %>%
  dplyr::mutate(
    cell_function = dplyr::case_when(
      cell_type %in% c("R7", "R8") ~ "visual_chromatic",
      cell_type %in% c("L1", "L2", "L3") ~ "visual_achromatic",
      TRUE ~ cell_function
    ),
    body_part_sensory = dplyr::case_when(
      cell_type %in% c("R7", "R8") ~ "retina",
      cell_type %in% c("L1", "L2", "L3") ~ "lamina",
      TRUE ~ body_part_sensory
    )
  )

# Resolve final neurotransmitter:
#   1. neurotransmitter_verified (highest confidence)
#   2. neuropeptide_verified (peptide override)
#   3. neurotransmitter_predicted (CNN prediction)
banc.meta <- banc.meta %>%
  dplyr::mutate(neurotransmitter = dplyr::case_when(
    !is.na(neurotransmitter_verified) & neurotransmitter_verified != "" ~ neurotransmitter_verified,
    !is.na(neuropeptide_verified)     & neuropeptide_verified     != "" ~ neuropeptide_verified,
    !is.na(neurotransmitter_predicted) & neurotransmitter_predicted != "" ~ neurotransmitter_predicted,
    TRUE ~ NA_character_
  ))

# Cluster: v850 has it in meta directly; override with manual_cluster if available.
# (For AN/DN we further overwrite below — the AN/DN cluster→super_cluster
# mapping is now 1:1, so we make `cluster` self-describing as
# "<super_cluster> (AN|DN)" instead of carrying the manual_cluster id.)
if ("manual_cluster" %in% colnames(banc.meta) &&
    sum(!is.na(banc.meta$manual_cluster) & banc.meta$manual_cluster != "") > 100) {
  banc.meta$cluster <- banc.meta$manual_cluster
}

# AN/DN cluster relabel (added 2026-04-09).
# Since AN/DN clusters now map 1:1 onto super_clusters, replace the old
# cluster id with "<super_cluster> (AN)" / "<super_cluster> (DN)" so:
#   1. The label is self-describing in plots.
#   2. AN and DN within the same super_cluster remain DISTINCT groups for
#      anything that groups by `cluster`.
# Non-AN/DN neurons are left alone (still carry the manual_cluster value).
# AN/DN neurons with NA super_cluster are also left alone (no clean rename).
.is_ascending      <- !is.na(banc.meta$super_class) & banc.meta$super_class == "ascending"
.is_descending     <- !is.na(banc.meta$super_class) & banc.meta$super_class == "descending"
.has_super_cluster <- !is.na(banc.meta$super_cluster) & banc.meta$super_cluster != ""
banc.meta$cluster[.is_ascending  & .has_super_cluster] <- paste0(
  banc.meta$super_cluster[.is_ascending  & .has_super_cluster], " (AN)"
)
banc.meta$cluster[.is_descending & .has_super_cluster] <- paste0(
  banc.meta$super_cluster[.is_descending & .has_super_cluster], " (DN)"
)
rm(.is_ascending, .is_descending, .has_super_cluster)

# Translate old super_cluster names to the SeaTable-canonical labels.
# Kept narrow: only labels that may still appear in the GCS feather but
# have been merged or renamed in SeaTable. Removed dormant entries
# (2026-05-01) — SeaTable is the source of truth for super_cluster.
.super_cluster_remap <- c(
  "wing-leg-tactile"       = "tactile",
  "head-leg-tactile"       = "tactile",
  "tactile perception"     = "tactile",
  "tactile_perception"     = "tactile",
  "takeoff_landing"        = "takeoff-landing"
)
.needs_remap <- !is.na(banc.meta$super_cluster) & banc.meta$super_cluster %in% names(.super_cluster_remap)
if (sum(.needs_remap) > 0) {
  message(sprintf("Remapping %d stale super_cluster labels (%s)",
                  sum(.needs_remap),
                  paste(unique(banc.meta$super_cluster[.needs_remap]), collapse = ", ")))
  banc.meta$super_cluster[.needs_remap] <- .super_cluster_remap[banc.meta$super_cluster[.needs_remap]]
}
banc.meta$body_part_effector <- gsub(",.*| .*","",banc.meta$body_part_effector)
banc.meta$body_part_sensory <- gsub(",.*| .*","",banc.meta$body_part_sensory)

# Ensure columns needed for seeds exist (enrich from seatable if missing)
for (.col in c("flow", "nerve", "peripheral_target_type")) {
  if (!.col %in% colnames(banc.meta)) {
    if (nrow(bc.orig) > 0 && .col %in% colnames(bc.orig)) {
      banc.meta <- banc.meta %>%
        dplyr::left_join(bc.orig %>%
                           dplyr::distinct(supervoxel_id, !!rlang::sym(.col)) %>%
                           dplyr::distinct(supervoxel_id, .keep_all = TRUE),
                         by = "supervoxel_id")
    }
  }
}
rm(.col)

# Assign seed columns for influence runs
source("R/startup/banc-seeds.R")
# v850+ has seeds pre-computed in meta (seed_00 through seed_14)
existing_seeds <- grep("^seed_\\d+$", colnames(banc.meta), value = TRUE)
if (length(existing_seeds) >= 13 && sum(!is.na(banc.meta[[existing_seeds[1]]])) > 100) {
  message(sprintf("Using %d pre-computed seed columns from GCS meta", length(existing_seeds)))
} else {
  banc.meta <- assign_banc_seeds(banc.meta)
}

# CNS network: load spectral clustering CSV for human-readable name mapping
cns.network.umap <- list()
i <- 1
callback <- function(x, pos) {
  cns.network.umap[[i <<- i + 1]] <<- x
}
cns_network_file <- .banc_spectral_csv
if (file.exists(cns_network_file)) {
  x <- readr::read_csv_chunked(
    cns_network_file,
    callback = readr::SideEffectChunkCallback$new(callback),
    chunk_size = 10000,
    col_types = banc.col.types
  )
  cns.network.umap <- dplyr::bind_rows(cns.network.umap)
  if ("unofficial_cluster_name" %in% colnames(cns.network.umap)) {
    # Legacy CSV format: build mapping from CSV column.
    cns.cluster.mapping <- cns.network.umap %>%
      dplyr::mutate(cns_network = paste0("CNS_",str_pad(spectral_cluster,width = 2,pad =0))) %>%
      dplyr::distinct(unofficial_cluster_name,cns_network)
    cns.cluster.names <- cns.cluster.mapping$unofficial_cluster_name
    names(cns.cluster.names) <- cns.cluster.mapping$cns_network
    if ("cns_network" %in% colnames(banc.meta) && !is.null(banc.meta$cns_network)) {
      # Map codes (CNS_01) to names; skip if already in name format (from SeaTable)
      raw_cn <- as.character(banc.meta$cns_network)
      is_code <- grepl("^CNS_", raw_cn) & !is.na(raw_cn)
      if (any(is_code)) {
        banc.meta$cns_network[is_code] <- cns.cluster.names[raw_cn[is_code]]
      }
    }
  } else {
    # New v850+ format (2026-04-07): the CSV no longer has unofficial_cluster_name.
    # banc.meta$cns_network already contains the descriptive names directly, so
    # build cns.cluster.names as a self-map for backward compat with downstream
    # code (panel_super_clusters.R lines 359, 1264, 1327) that expects a named
    # character vector keyed by cns_network value.
    if ("cns_network" %in% colnames(banc.meta) && !is.null(banc.meta$cns_network)) {
      .cns_names <- sort(unique(stats::na.omit(as.character(banc.meta$cns_network))))
      cns.cluster.names <- setNames(.cns_names, .cns_names)
      rm(.cns_names)
    } else {
      cns.cluster.names <- character(0)
    }
  }
} else {
  message("WARNING: No spectral clustering CSV found — cns_network will use raw codes (CNS_01..CNS_13)")
  cns.cluster.names <- setNames(unique(na.omit(banc.meta$cns_network)),
                                 unique(na.omit(banc.meta$cns_network)))
}

# ----- End of live block: write the dated snapshot ----------------------
tryCatch({
  .snap_out <- file.path("data", "meta",
                         sprintf("banc_888_meta_%s.parquet",
                                 format(Sys.Date(), "%Y%m%d")))
  arrow::write_parquet(banc.meta, .snap_out, compression = "snappy")
  message(sprintf("Wrote banc.meta snapshot %s (%.1f MB)",
                  .snap_out, file.info(.snap_out)$size / 1024^2))
  rm(.snap_out)
}, error = function(e) message("Snapshot write failed: ", conditionMessage(e)))

}  # end if(.live_needed)
rm(.live_needed)

# ============================================================================
# Derived setup — runs unconditionally on both snapshot and live paths.
# Uses banc.meta (set above) but not bc.orig. Builds per-class subsets,
# canonical ordering vectors, the seed/effector lookup maps, and the
# AN/DN cluster CSV joins.
# ============================================================================

# Join banc.meta data
banc.meta.pre <- banc.meta
colnames(banc.meta.pre) <- paste0("pre_",colnames(banc.meta.pre))
banc.meta.post <- banc.meta
colnames(banc.meta.post) <- paste0("post_",colnames(banc.meta.post))

# Meta data summaries
if(!is.null(banc.version)&&banc.version=="banc_626"){
  banc.meta$root_id <- banc.meta$root_626
}
banc.meta$id <- banc.meta$root_id
banc.neck.meta <- banc.meta %>%
  dplyr::filter(grepl("descending|ascending",super_class),
                !grepl("effector",super_class))
banc.dn.meta <- subset(banc.meta, grepl("descending",super_class))
banc.an.meta <- subset(banc.meta, grepl("ascending",super_class))
banc.eff.meta <- subset(banc.meta, grepl("efferent|motor|endocrine|visceral",super_class)) 
banc.vpn.meta <- subset(banc.meta, grepl("visual_projection",super_class))
banc.sens.meta <- subset(banc.meta, grepl("sensory",super_class) | (!is.na(body_part_sensory) & body_part_sensory != ""))
banc.sens.meta  <- banc.sens.meta %>%
  dplyr::mutate(body_part_sensory = dplyr::case_when(
    grepl("pharynx",body_part_sensory) ~ "pharynx",
    TRUE ~ body_part_sensory
  ))
an.ids <- unique(banc.an.meta$id)
dn.ids <- unique(banc.dn.meta$id)

# Read DN data output
# umap.sez.df  <- read_csv(file = "data/banc_sez_functional_classes.csv", col_types = banc.col.types) %>%
#   dplyr::mutate(clusterno = gsub(".*_","",cluster)) %>%
#   dplyr::mutate(id = ifelse(is.na(id),cell_type,id))
# Read CSV data — ONLY take UMAP coords + calculated_cluster from these.
# Explicitly drop super_cluster (and any other metadata columns) to prevent
# stale CSV values from shadowing the banc.meta values (2026-04-11).
# Read CSV data — ONLY UMAP coords. cluster/manual_cluster/super_cluster
# all come from banc.meta (SeaTable + GCS), never from these CSVs.
# PCA-UMAP is the primary projection. If the CSV still has the old layout
# (UMAP1/2 = cosine, PCA_UMAP1/2 = PCA), swap here so UMAP1/2 = PCA
# everywhere downstream. If cosine_UMAP1 already exists, the swap was
# already applied in-CSV by panel_an_dn_umap.R recalculate=TRUE.
classes.dn.df <- read_csv(file = "data/banc_annotations/v888/banc_neck_functional_classes.csv", col_types = banc.col.types)
if ("PCA_UMAP1" %in% colnames(classes.dn.df) && !"cosine_UMAP1" %in% colnames(classes.dn.df)) {
  message("PCA-UMAP swap: CSV has PCA_UMAP1/2 but no cosine_UMAP1/2 — swapping now.")
  classes.dn.df <- classes.dn.df %>%
    dplyr::rename(cosine_UMAP1 = UMAP1, cosine_UMAP2 = UMAP2,
                  UMAP1 = PCA_UMAP1, UMAP2 = PCA_UMAP2)
}
classes.dn.df <- classes.dn.df %>%
  dplyr::select(id, UMAP1, UMAP2,
                dplyr::any_of(c("cosine_UMAP1", "cosine_UMAP2")))
classes.eff.df <- read_csv(file = "data/banc_annotations/v888/banc_efferent_functional_classes.csv", col_types = banc.col.types) %>%
  dplyr::select(id, UMAP1, UMAP2)

# Join UMAP coords from CSV to banc.meta-derived subsets.
# CSV IDs are root_850 (regenerated by panel_an_dn_umap.R with v850 data).
classes.dn.df <- banc.neck.meta %>%
  dplyr::left_join(classes.dn.df %>%
                     dplyr::rename(csv_id = id) %>%
                     dplyr::mutate(csv_id = as.character(csv_id)),
                   by = c("root_id" = "csv_id"))  %>%
  dplyr::mutate(clusterno = gsub(".*_","",cluster))
# EFF CSV IDs are root_850; join via root_850 when available, else root_id
.eff_csv_join_col <- if ("root_850" %in% colnames(banc.eff.meta)) "root_850" else "root_id"
classes.eff.df <- banc.eff.meta %>%
  dplyr::left_join(classes.eff.df %>%
                     dplyr::rename(csv_id = id) %>%
                     dplyr::mutate(csv_id = as.character(csv_id)),
                   by = setNames("csv_id", .eff_csv_join_col))  %>%
  dplyr::mutate(clusterno = gsub(".*_","",cluster))

# Neck key UMAP
neck.inclusion <- readr::read_csv(file="data/meta/banc_neck_inclusion.csv",
                                  col_types = banc.col.types)
# neck_inclusion.csv root_id may be root_850; map to current root_id via root_850
.ni_ids <- as.character(neck.inclusion$root_id)
if ("root_850" %in% colnames(banc.meta) &&
    sum(.ni_ids %in% banc.meta$root_id) < sum(.ni_ids %in% as.character(banc.meta$root_850))) {
  .ni_bridge <- banc.meta %>% dplyr::distinct(root_id, root_850) %>%
    dplyr::mutate(root_850 = as.character(root_850))
  neck.inclusion <- neck.inclusion %>%
    dplyr::mutate(root_id = as.character(root_id)) %>%
    dplyr::left_join(.ni_bridge, by = c("root_id" = "root_850")) %>%
    dplyr::mutate(root_id = dplyr::coalesce(root_id.y, root_id)) %>%
    dplyr::select(-root_id.y)
  message("neck_inclusion.csv: remapped root_850 → root_888 via banc.meta bridge")
}
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

# NOTE: do NOT hardcode threshold.inf.value here — banc-startup.R already
# loads it from data/determined_thresholds/influence_norm_log_elbow_threshold.csv
# (written by panels_body_parts.R). The previous hardcode of 17.18 was clobbering
# that dynamically-loaded value because banc-meta.R is sourced after banc-startup.R.
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
                         neck_roll_pitch_motor_neuron = "neck",
                         neck_motor_neuron = "neck",
                         prothorax_neurosecretory_cell = "vnc neurosecretory",
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
  frontoorbital_bristle_neuron = "head bristle",
  ocellar_bristle_neuron = "head bristle",
  orbital_bristle_neuron = "head bristle",
  abdomen_oxygenation_neuron = "abdomen oxygenation",
  wing_base_orphan_neuron = "wing base orphan",
  wing_campaniform_sensillum_neuron = "wing campaniform",
  thorax_orphan_neuron = "thorax orphan",
  thorax_thoracic_abdominal_segmental_sensory_neuron = "thoracic-abdominal",
  haltere_thoracic_abdominal_segmental_sensory_neuron  = "thoracic-abdominal",
  # haltere_thoracic_abdominal_segmental_sensory_neuron
  hind_leg_hook_chordotonal_organ_neuron = "hind leg chordotonal",
  middle_leg_hook_chordotonal_organ_neuron = "middle leg chordotonal",
  front_leg_hook_chordotonal_organ_neuron = "front leg chordotonal",
  wing_tegula_orphan_neuron = "wing tegula orphan",
  pharynx_orphan_neuron = "pharynx orphan neuron",
  anterior_digestive_tract_internal_taste_sensillum_neuron = "enteric internal taste",
  anterior_digestive_tract_multidendritic_neuron = "enteric multidendritic",
  posterior_uterine_sensory_neuron = "uterine",
  pharynx_fishtrap_bristle_neuron = "pharynx fishtrap",
  middle_leg_orphan_neuron = "middle leg orphan",
  haltere_chordotonal_organ_neuron = "haltere chordotonal",
  putative_hind_leg_club_chordotonal_organ_neuron = "hind leg chordotonal",
  neck_chordotonal_organ_neuron = "neck chordotonal",
  putative_front_leg_claw_chordotonal_organ_neuron = "front leg chordotonal",
  putative_front_leg_hair_plate_neuron = "front leg hair plate",
  hind_leg_bilateral_campaniform_sensillum_neuron = "hind leg campaniform",
  middle_leg_bilateral_campaniform_sensillum_neuron = "middle leg campaniform",
  front_leg_chordotonal_organ_neuorn = "front leg chordotonal",
  sex_peptide_sensory_neuron = "sex peptide",
  abdominal_ppk_neuron = "abdominal ppk",
  front_leg_bilateral_campaniform_sensillum_neuron = "front leg campaniform",
  #abdomen_endocrine_right = "abdomen_endocrine", 
  abdomen_multidendritic_neuron = "abdomen multidendritic",
  abdomen_orphan_neuron = "abdomen orphan",
  #abdomen_strand_neuron,
  abdominal_wall_multidendritic_neuron = "abdomen bitter", 
  antenna_bristle_neuron = "antenna bristle",
  antenna_campaniform_sensillum_neuron = "antenna campaniform", 
  antenna_hygrosensory_receptor_neuron = "antenna hygrosensory receptor", 
  antenna_olfactory_receptor_neuron = "antenna olfactory receptor", 
  #antenna_orphan_neuron = "antenna orphan", 
  antenna_thermosensory_receptor_neuron = "antenna thermosensory receptor", 
  aorta_sensory_neuron = "aorta",
  cibarium_multidendritic_neuron = "cibarium", 
  crop_internal_taste_sensillum_neuron = "crop internal taste", 
  anterior_digestive_tract_internal_taste_sensillum_gustatory_neuron = "anterior digestive",
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
  front_leg_taste_peg_neuron = "front leg taste bristle",
  front_leg_taste_bristle_neuron = "front leg taste bristle", 
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
  hind_leg_taste_peg_neuron = "hind leg taste bristle",
  hind_leg_taste_bristle_neuron = "hind leg taste bristle",   
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
  middle_leg_bristle_neuron = "middle leg bristle", 
  middle_leg_campaniform_sensillum_neuron = "middle leg campaniform", 
  middle_leg_chordotonal_organ_neuron = "middle leg chordotonal", 
  middle_leg_claw_chordotonal_organ_neuron = "middle leg chordotonal", 
  middle_leg_club_chordotonal_organ_neuron = "middle leg chordotonal", 
  middle_leg_hair_plate_neuron  = "leg hair plate",  
  middle_leg_hook_chordotonal = "middle leg chordotonal", 
  middle_leg_multidendritic_neuron = "leg multidendritic", 
  #middle_leg_orphan_neuron = "middle leg orphan", 
  middle_leg_taste_peg_neuron = "middle leg taste bristle",
  middle_leg_taste_bristle_neuron = "middle leg taste bristle",
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
  retina_photoreceptor_neuron = "retina photoreceptor", 
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
  wing_margin_taste_peg_neuron = "wing margin taste bristle", 
  wing_margin_taste_bristle_neuron = "wing margin taste bristle", 
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
  visual_ocellar = "visual ocellar",
  visual_achromatic_lamina_projection = "visual achromatic lamina projection",
  visual_chromatic_lamina_projection = "visual chromatic lamina projection"
)

# Get sensory seed map
sensory.seed.map <- c(
  #abdomen_endocrine_left = "abdomen_endocrine", 
  frontoorbital_bristle_neuron = "head bristle",
  ocellar_bristle_neuron = "head bristle",
  orbital_bristle_neuron = "head bristle",
  abdomen_oxygenation_neuron = "abdomen oxygenation",
  #wing_base_orphan_neuron = "wing base orphan",
  wing_campaniform_sensillum_neuron = "wing campaniform",
  #thorax_orphan_neuron = "thorax orphan",
  thorax_thoracic_abdominal_segmental_sensory_neuron = "thoracic-abdominal",
  haltere_thoracic_abdominal_segmental_sensory_neuron  = "thoracic-abdominal",
  # haltere_thoracic_abdominal_segmental_sensory_neuron
  hind_leg_hook_chordotonal_organ_neuron = "leg chordotonal",
  middle_leg_hook_chordotonal_organ_neuron = "leg chordotonal",
  front_leg_hook_chordotonal_organ_neuron = "leg chordotonal",
  #wing_tegula_orphan_neuron = "wing tegula orphan",
  #pharynx_orphan_neuron = "pharynx orphan neuron",
  anterior_digestive_tract_internal_taste_sensillum_neuron = "enteric internal taste",
  anterior_digestive_tract_multidendritic_neuron = "enteric multidendritic",
  posterior_uterine_sensory_neuron = "uterine",
  pharynx_fishtrap_bristle_neuron = "pharynx fishtrap",
  #middle_leg_orphan_neuron = "middle leg orphan",
  haltere_chordotonal_organ_neuron = "haltere chordotonal",
  putative_hind_leg_club_chordotonal_organ_neuron = "hind leg chordotonal",
  neck_chordotonal_organ_neuron = "neck chordotonal",
  putative_front_leg_claw_chordotonal_organ_neuron = "leg chordotonal",
  putative_front_leg_hair_plate_neuron = "leg hair plate",
  hind_leg_bilateral_campaniform_sensillum_neuron = "leg campaniform",
  middle_leg_bilateral_campaniform_sensillum_neuron = "leg campaniform",
  front_leg_chordotonal_organ_neuron = "leg chordotonal",
  sex_peptide_sensory_neuron = "sex peptide",
  abdominal_ppk_neuron = "abdominal ppk",
  front_leg_bilateral_campaniform_sensillum_neuron = "leg campaniform",
  #abdomen_endocrine_right = "abdomen_endocrine", 
  abdomen_multidendritic_neuron = "abdomen multidendritic", 
  #abdomen_orphan_neuron = "abdomen orphan", 
  #abdomen_strand_neuron, 
  abdominal_wall_multidendritic_neuron = "abdominal wall multidendritic", 
  antenna_bristle_neuron = "antenna bristle",
  antenna_campaniform_sensillum_neuron = "antenna campaniform", 
  antenna_hygrosensory_receptor_neuron = "antenna hygrosensory receptor",
  antenna_olfactory_receptor_neuron = "antenna olfactory receptor",
  #antenna_orphan_neuron = "antenna orphan", 
  antenna_thermosensory_receptor_neuron = "antenna thermosensory receptor", 
  aorta_sensory_neuron = "aorta",
  cibarium_multidendritic_neuron = "cibarium multidendritic", 
  crop_internal_taste_sensillum_neuron = "crop internal taste", 
  anterior_digestive_tract_internal_taste_sensillum_gustatory_neuron = "anterior digestive",
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
  front_leg_taste_peg_neuron = "leg taste bristle", 
  front_leg_taste_bristle_neuron = "leg taste bristle", 
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
  hind_leg_taste_peg_neuron = "leg taste bristle",   
  hind_leg_taste_bristle_neuron = "leg taste bristle",   
  internal_thermosensory_receptor_neuron = "antenna thermosensory receptor", 
  johnstons_organ_A_neuron = "johnstons organ A", 
  johnstons_organ_B_neuron = "johnstons organ B", 
  johnstons_organ_C_neuron = "johnstons organ C", 
  johnstons_organ_D_neuron = "johnstons organ D", 
  johnstons_organ_E_neuron = "johnstons organ E", 
  johnstons_organ_F_neuron = "johnstons organ F", 
  #johnstons_organ_other_neuron = "johnstons organ other", 
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
  middle_leg_taste_peg_neuron = "leg taste bristle",
  middle_leg_taste_bristle_neuron = "leg taste bristle",
  #pars_intercerebralis_endocrine_enteric_left = "pars_intercerebralis_enteric", 
  #pars_intercerebralis_endocrine_enteric_right = "pars_intercerebralis_enteric", 
  #pars_lateralis_endocrine_corpus_allatum_left = "pars_lateralis_endocrine_retrocerebral_complex", 
  #pars_lateralis_endocrine_corpus_allatum_right = "pars_lateralis_endocrine_retrocerebral_complex", 
  #pars_lateralis_endocrine_retrocerebral_complex_left = "pars_lateralis_endocrine_retrocerebral_complex", 
  #pars_lateralis_endocrine_retrocerebral_complex_right = "pars_lateralis_endocrine_retrocerebral_complex", 
  pharynx_internal_taste_sensillum_neuron = "pharynx internal", 
  #pharynx_orphan_neuron = "pharynx internal",
  prosternal_hair_plate_neuron = "prosternal hair plate", 
  prothoracic_chordotonal_organ_neuron = "prothoracic chordotonal", 
  #retina_photoreceptor_neuron = "retina photoreceptor", 
  #subesophageal_zone_endocrine_left = "subesophageal zone endocrine", 
  #subesophageal_zone_endocrine_right = "subesophageal zone endocrine", 
  thorax_bristle_neuron = "thorax bristle", 
  thorax_campaniform_sensillum_neuron = "thorax campaniform", 
  thorax_multidendritic_neuron = "thorax multidendritic",
  #thorax_orphan_neuron = "thorax orphan", 
  wheelers_chordotonal_organ_neuron = "wheelers organ chordotonal", 
  wing_base_campaniform_sensillum_neuron = "wing campaniform", 
  wing_base_chordotonal_organ_neuron = "wing chordotonal",
  #wing_base_orphan_neuron = "wing base orphan", 
  wing_campaniform_sensillum_neuron = "wing campaniform", 
  wing_margin_bristle_neuron = "thorax bristle",
  abdominal_terminalia_bristle = "terminalia bristle",
  wing_margin_taste_peg_neuron = "wing taste bristle", 
  wing_margin_taste_bristle_neuron = "wing taste bristle", 
  wing_multidendritic_neuron = "wing multidendritic", 
  wing_tegula_campaniform_sensillum_neuron = "wing campaniform", 
  wing_tegula_chordotonal_organ_neuron = "wing chordotonal", 
  wing_tegula_hair_plate_neuron = "wing hair plate", 
  wing_tegula_orphan_neuron = "wing orphan",
  visual_front_leg_feedback = "visual thin vertical bar", 
  visual_horizontal_widefieldmotion = "visual horizontal widefield motion", 
  `visual_large_objects,visual_thin_vertical_bar` = "visual thin vertical bar", 
  visual_loom = "visual loom", 
  `visual_object,visual_loom` = "visual loom",  
  visual_polarized_light = "polarized light", 
  visual_small_object = "visual small object", 
  `visual_small_object,visual_loom` = "visual loom",  
  visual_thin_vertical_bar = "visual thin vertical bar", 
  visual_vertical_widefieldmotion = "visual vertical widefield motion",
  visual_chromatic = "visual chromatic",
  visual_achromatic = "visual achromatic",
  visual_ocellar = "visual ocellar",
  visual_achromatic_lamina_projection = "visual achromatic",
  visual_chromatic_lamina_projection = "visual chromatic"
)

# super cluster ordering
super.clust.order <- c("flight steering 1",
                       "flight steering 2",
                       "flight power",
                       "head orienting",
                       "grooming",
                       "probing",
                       "feeding",
                       "taste-touch",
                       "tactile",
                       "threat response",
                       "vibratory",
                       "proprioceptive",
                       "postural control",
                       "walking",
                       "walking steering",
                       "reproduction",
                       "visceral control")
cns.network.order = c("abdominal VNC",
  "leg VNC",
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
  "central complex related")
eff.super.order <- rev(c("abdomen-positioning", 
                         "abdomen-ureter", 
                         "energy homeostasis", 
                         "ingestion-digestion",
                         "abdomen-reproductive", 
                         "abdomen motor 1", 
                         "middle-hind leg", 
                         "front leg", 
                         "proboscis-antenna",
                         "head-eye-antenna", 
                         "flight-steering",
                         "flight-energy-power", 
                         "thoracic-abdominal", 
                         "abdomen motor 2", 
                         "feeding-endocrine"))




