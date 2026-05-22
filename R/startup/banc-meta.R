###############################################################################
# banc-meta.R
#
# Dispatcher for the BANC v888 metadata loader. Reads the committed parquet
# snapshot at data/meta/banc_888_meta_<YYYYMMDD>.parquet if one is present
# (the default after a clean clone) and falls back to the live SeaTable +
# GCS loader at R/startup/banc-meta-live.R when no snapshot exists or when
# the BANC_LIVE=1 environment variable forces a refresh.
#
# The derived per-class data frames (banc.an.meta, banc.dn.meta, banc.eff.meta,
# banc.sens.meta, banc.vpn.meta, banc.neck.meta), the canonical ordering
# vectors, and the seed/effector lookup maps all live at the tail of
# banc-meta-live.R and run in both paths (they only depend on banc.meta).
#
# Reproducing the snapshot:
#     BANC_LIVE=1 Rscript R/startup/banc-meta-live.R
# That writes a freshly-dated banc_888_meta_<YYYYMMDD>.parquet under
# data/meta/; this dispatcher picks the most-recent file on next source().
###############################################################################

.snaps <- sort(
  list.files(file.path("data", "meta"),
             pattern = "^banc_888_meta_[0-9]{8}\\.parquet$",
             full.names = TRUE),
  decreasing = TRUE
)
.use_snap <- length(.snaps) > 0 && !identical(Sys.getenv("BANC_LIVE"), "1")

if (.use_snap) {
  message("Loading banc.meta from snapshot: ", basename(.snaps[1]))
  banc.meta <- arrow::read_parquet(.snaps[1])
  if ("root_id" %in% colnames(banc.meta)) {
    banc.meta$root_id <- as.character(banc.meta$root_id)
  }
  if ("supervoxel_id" %in% colnames(banc.meta)) {
    banc.meta$supervoxel_id <- as.character(banc.meta$supervoxel_id)
  }
  if (!"id" %in% colnames(banc.meta) && "root_id" %in% colnames(banc.meta)) {
    banc.meta$id <- banc.meta$root_id
  }
  message(sprintf("  %d rows x %d cols", nrow(banc.meta), ncol(banc.meta)))
  # bc.orig is a SeaTable-only intermediate produced by the live loader; the
  # snapshot has its enrichment baked in. Provide an empty placeholder so any
  # downstream defensive check that tests `nrow(bc.orig) > 0` works.
  bc.orig <- data.frame()
}
rm(.snaps, .use_snap)

# banc-meta-live.R guards its live block on `exists("banc.meta")` so it only
# runs the SeaTable + GCS pipeline when the snapshot fast-path above hasn't
# already populated banc.meta. The derived setup (per-class subsets, ordering
# vectors, classes.dn.df, umap.dn.df, sensory.seed.map, etc.) at the tail of
# banc-meta-live.R always runs.
source("R/startup/banc-meta-live.R")
