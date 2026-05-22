###############################################################################
# franken-meta.R
#
# Dispatcher for the cross-dataset "franken" matching layer (the lookup that
# joins BANC, FAFB, MANC, hemibrain, and maleCNS cell types). Prefers a
# committed feather snapshot at data/meta/franken_meta_<YYYYMMDD>.feather;
# falls back to the live SeaTable + `bancr::franken_meta()` loader in
# R/startup/franken-meta-live.R when no snapshot exists or BANC_LIVE=1
# forces a refresh.
#
# Format note: the snapshot is feather (Arrow IPC), not parquet. arrow
# 16.1.0 wrote a deterministically-corrupt 5.5 MB parquet for this
# franken.meta schema — both R and Python arrow could read the file
# footer (62 cols x 163 833 rows) but the row group always failed
# "Unexpected end of stream". Feather sidesteps that. banc.meta still
# uses parquet (works fine for that schema).
#
# Reproducing the snapshot:
#     BANC_LIVE=1 Rscript R/startup/franken-meta-live.R
# That writes a freshly-dated franken_meta_<YYYYMMDD>.feather under
# data/meta/; this dispatcher picks the most-recent file on next source().
###############################################################################

.snaps <- sort(
  list.files(file.path("data", "meta"),
             pattern = "^franken_meta_[0-9]{8}\\.feather$",
             full.names = TRUE),
  decreasing = TRUE
)
.use_snap <- length(.snaps) > 0 && !identical(Sys.getenv("BANC_LIVE"), "1")

if (.use_snap) {
  message("Loading franken.meta from snapshot: ", basename(.snaps[1]))
  franken.meta <- arrow::read_feather(.snaps[1])
  message(sprintf("  %d rows x %d cols", nrow(franken.meta), ncol(franken.meta)))
}
rm(.snaps, .use_snap)

# franken-meta-live.R guards its live block on `exists("franken.meta")` so
# it only runs the SeaTable fetch when the snapshot fast-path above hasn't
# already populated franken.meta.
source("R/startup/franken-meta-live.R")
