#' Largest-cell-types neuroanatomy renders for Fig. 1a (right panel)
#'
#' Produces five neuroanatomy PNGs of the largest BANC cell types by
#' volume, plus a sensory + effector "all neurons" composite:
#'
#'   1. largest_neurons_neuroanatomy_100.png   — top 100 cell types
#'   2. largest_neurons_neuroanatomy_200.png   — top 200 cell types
#'   3. largest_neurons_neuroanatomy_300.png   — top 300 cell types
#'   4. largest_neurons_neuroanatomy_500.png   — top 500 cell types (Fig. 1a)
#'   5. largest_neurons_neuroanatomy_all_sensory_all_effector.png
#'
#' Strategy: render in segments of 100 cell types and composite with
#' `magick::image_composite()` to build the cumulative milestone images,
#' otherwise the full render OOMs. Each segment is rendered onto a
#' transparent background so segments can be layered.
#'
#' @section Reads:
#'   banc.meta, paper.cols
#'   neuron meshes via bancr::banc_read_neuron_meshes (GCS-backed)
#'
#' @section Writes:
#'   figures/figure_1/links/largest_neurons_neuroanatomy_*.png               (Fig. 1a right)
#'
#' @section Paper:
#'   Fig. 1a (right panel) — rendering of the largest 500 cell types by volume.
#'   Methods §"BANC neuropil mesh generation".
#'
#' @section Notes:
#'   Long-running; expect tens of minutes to hours depending on the cell-
#'   type count and mesh density. Renders best on a machine with ≥ 32 GB
#'   RAM; if OOM, reduce the segment size below 100.
#'
#' @section Reproduce:
#'   Rscript R/text/largest_neurons_neuroanatomy.R

###############
### STARTUP ###
###############

source("R/startup/banc-startup.R")
source("R/startup/banc-meta.R")
source("R/startup/banc-functions.R")
library(nat.ggplot)
library(magick)

################
### SETTINGS ###
################

simplify_percent <- 0.1  # retain 10% of mesh vertices
mesh_alpha <- 0.5
dpi <- 600
plot_width <- 10
plot_height <- 10
output_dir <- banc.fig1.path

###############
### HELPERS ###
###############

# Distinct proofread neurons
bm <- banc.meta %>%
  dplyr::distinct(root_id, .keep_all = TRUE) %>%
  dplyr::filter(!super_class %in% c("glia", "trachea", "not_a_neuron"))

# Base plot template
make_base_plot <- function() {
  g <- ggplot2::ggplot() +
    ggplot2::coord_fixed() +
    ggplot2::theme_void() +
    ggplot2::guides(fill = "none", color = "none") +
    ggplot2::theme(legend.position = "none",
                   plot.margin = ggplot2::margin(0, 0, 0, 0))
  g + geom_neuron(x = banc_neuropil.surf,
                  cols = c("grey60", "grey30"),
                  rotation_matrix = bancr:::banc_rotation_matrices[["main"]],
                  alpha = 0.1)
}

# Download meshes for a set of root_850 IDs, one at a time, simplify, return neuronlist
fetch_meshes <- function(ids) {
  ids <- unique(na.omit(ids))
  neurons <- list()
  for (id in ids) {
    tryCatch({
      n <- banc_read_neuron_meshes(id, OmitFailures = TRUE)
      if (length(n)) {
        n <- nlapply(n, Rvcg::vcgQEdecim, percent = simplify_percent)
        neurons <- c(neurons, n)
      }
    }, error = function(e) {
      message("    ID ", id, " failed: ", e$message)
    })
  }
  neurons
}

# Build a plot for a batch of cell types, save as transparent PNG.
# If total meshes exceed max_meshes_per_plot, splits into sub-segments
# and composites them together to avoid R expression depth overflow.
max_meshes_per_plot <- 500

render_segment <- function(ct_list, neuron_df, ct_cols, seg_file) {
  # Collect all (id, colour) pairs first, then batch-render
  all_jobs <- list()
  for (ct in ct_list) {
    ct_ids <- unique(na.omit(neuron_df$root_850[neuron_df$cell_type == ct]))
    if (!length(ct_ids)) next
    col <- ct_cols[[ct]]
    for (id in ct_ids) all_jobs[[length(all_jobs) + 1]] <- list(id = id, col = col)
  }
  if (!length(all_jobs)) {
    # Still save an empty base plot so compositing works
    g <- make_base_plot()
    ggsave(plot = g, filename = seg_file,
           width = plot_width, height = plot_height, dpi = dpi, bg = "transparent")
    rm(g); gc(verbose = FALSE)
    return(0L)
  }

  # Split jobs into batches of max_meshes_per_plot
  n_batches <- ceiling(length(all_jobs) / max_meshes_per_plot)
  sub_files <- character()
  seg_meshes <- 0L

  for (b in seq_len(n_batches)) {
    batch_start <- (b - 1) * max_meshes_per_plot + 1
    batch_end <- min(b * max_meshes_per_plot, length(all_jobs))
    batch_jobs <- all_jobs[batch_start:batch_end]

    g <- make_base_plot()
    batch_n <- 0L
    for (job in batch_jobs) {
      neurons <- fetch_meshes(job$id)
      if (!length(neurons)) next
      neurons <- as.neuronlist(neurons)
      g <- g +
        geom_neuron(x = neurons,
                    cols = c(adjust_color_brightness(job$col, 1.1),
                             adjust_color_brightness(job$col, 0.9)),
                    rotation_matrix = bancr:::banc_rotation_matrices[["main"]],
                    alpha = mesh_alpha)
      batch_n <- batch_n + length(neurons)
      rm(neurons); gc(verbose = FALSE)
    }

    if (n_batches == 1) {
      sub_file <- seg_file
    } else {
      sub_file <- sub("\\.png$", sprintf("_sub%02d.png", b), seg_file)
    }
    message(sprintf("  Saving sub-batch %d/%d: %s (%d meshes)", b, n_batches, basename(sub_file), batch_n))
    ggsave(plot = g, filename = sub_file,
           width = plot_width, height = plot_height, dpi = dpi, bg = "transparent")
    sub_files <- c(sub_files, sub_file)
    seg_meshes <- seg_meshes + batch_n
    rm(g); gc(verbose = FALSE)
  }

  # Composite sub-batches if there were multiple
  if (n_batches > 1) {
    message("  Compositing ", n_batches, " sub-batches -> ", basename(seg_file))
    composite_segments(sub_files, seg_file)
    unlink(sub_files)
  }

  message("  Segment total: ", seg_meshes, " meshes")
  seg_meshes
}

# Composite segment PNGs into a milestone image
composite_segments <- function(seg_files, out_file) {
  message("Compositing ", length(seg_files), " segments -> ", out_file)
  base <- magick::image_read(seg_files[1])
  if (length(seg_files) > 1) {
    for (f in seg_files[-1]) {
      overlay <- magick::image_read(f)
      base <- magick::image_composite(base, overlay, operator = "over")
      rm(overlay)
    }
  }
  magick::image_write(base, path = out_file)
  rm(base)
  gc(verbose = FALSE)
}

save_plot <- function(g, filename) {
  path <- file.path(output_dir, filename)
  message("Saving to ", path)
  ggsave(plot = g, filename = path,
         width = plot_width, height = plot_height, dpi = dpi)
}

###############
### COLOURS ###
###############

# Assign random colours from paper.cols (remove NAs, ignore names)
available_cols <- na.omit(unname(paper.cols))

##############################################
### PART 1: TOP 100 / 200 / 300 / 500 PNG ###
##############################################

# Compute top 500 cell types by max volume_nm3
top_500_cts <- bm %>%
  dplyr::filter(!is.na(cell_type), cell_type != "", !is.na(volume_nm3)) %>%
  dplyr::group_by(cell_type) %>%
  dplyr::summarise(max_vol = max(volume_nm3, na.rm = TRUE), .groups = "drop") %>%
  dplyr::arrange(dplyr::desc(max_vol)) %>%
  dplyr::slice_head(n = 500)

# Get all neurons for these cell types, with root_850 IDs
top_500_neurons <- bm %>%
  dplyr::filter(cell_type %in% top_500_cts$cell_type) %>%
  dplyr::left_join(top_500_cts, by = "cell_type") %>%
  dplyr::arrange(dplyr::desc(max_vol), cell_type)

# Ordered cell types (largest first)
ordered_cts <- top_500_cts$cell_type
n_cts <- length(ordered_cts)

# Assign colours
set.seed(42)
ct_cols <- sample(available_cols, n_cts,
                  replace = n_cts > length(available_cols))
names(ct_cols) <- ordered_cts

message(sprintf("=== TOP %d: %d cell types, %d neurons ===",
                n_cts, n_cts, nrow(top_500_neurons)))

# Render in segments of 100, composite at milestones
seg_size <- 100
milestones <- c(100, 200, 300, 500)
seg_dir <- tempdir()
seg_files <- character()
total_meshes <- 0L

n_segs <- ceiling(n_cts / seg_size)
for (s in seq_len(n_segs)) {
  start_i <- (s - 1) * seg_size + 1
  end_i <- min(s * seg_size, n_cts)
  seg_cts <- ordered_cts[start_i:end_i]
  seg_file <- file.path(seg_dir, sprintf("seg_%03d_%03d.png", start_i, end_i))

  message(sprintf("\n=== Segment %d: cell types %d–%d ===", s, start_i, end_i))
  for (ct in seg_cts) {
    message(sprintf("  [%d/%d] %s", match(ct, ordered_cts), n_cts, ct))
  }

  seg_meshes <- render_segment(seg_cts, top_500_neurons, ct_cols, seg_file)
  seg_files <- c(seg_files, seg_file)
  total_meshes <- total_meshes + seg_meshes
  message(sprintf("  Segment done: %d meshes (cumulative: %d)", seg_meshes, total_meshes))

  # Save at milestones
  if (end_i %in% milestones) {
    out_name <- sprintf("largest_neurons_neuroanatomy_%d.png", end_i)
    composite_segments(seg_files, file.path(output_dir, out_name))
  }
}

# Save final if we stopped short of a milestone
if (!n_cts %in% milestones) {
  # Find the largest milestone we passed
  passed <- milestones[milestones <= n_cts]
  if (length(passed)) {
    largest_passed <- max(passed)
    out_name <- sprintf("largest_neurons_neuroanatomy_%d.png", largest_passed)
    if (!file.exists(file.path(output_dir, out_name))) {
      # Composite up to the relevant segments
      n_segs_needed <- ceiling(largest_passed / seg_size)
      composite_segments(seg_files[seq_len(n_segs_needed)],
                         file.path(output_dir, out_name))
    }
  }
}

# Clean up segment temp files
unlink(seg_files)
rm(top_500_neurons, top_500_cts)
gc(verbose = FALSE)

message(sprintf("\n=== TOP %d DONE: %d meshes rendered ===", n_cts, total_meshes))

############################################
### PART 2: ALL SENSORY + ALL EFFECTOR   ###
############################################

message("\n=== ALL SENSORY + ALL EFFECTOR ===")

sens_eff <- bm %>%
  dplyr::filter(grepl("sensory|motor|visceral", super_class),
                !is.na(cell_type), cell_type != "") %>%
  dplyr::group_by(cell_type) %>%
  dplyr::mutate(max_vol = max(volume_nm3, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(dplyr::desc(max_vol), cell_type)

se_cts <- unique(sens_eff$cell_type)
n_se <- length(se_cts)
message(sprintf("  %d cell types, %d neurons", n_se, nrow(sens_eff)))

# Assign colours
set.seed(123)
se_cols <- sample(available_cols, n_se,
                  replace = n_se > length(available_cols))
names(se_cols) <- se_cts

# Render in segments
se_seg_files <- character()
se_meshes <- 0L
n_se_segs <- ceiling(n_se / seg_size)

for (s in seq_len(n_se_segs)) {
  start_i <- (s - 1) * seg_size + 1
  end_i <- min(s * seg_size, n_se)
  seg_cts <- se_cts[start_i:end_i]
  seg_file <- file.path(seg_dir, sprintf("se_seg_%03d_%03d.png", start_i, end_i))

  message(sprintf("\n=== Sensory/Effector segment %d: cell types %d–%d ===", s, start_i, end_i))

  seg_meshes <- render_segment(seg_cts, sens_eff, se_cols, seg_file)
  se_seg_files <- c(se_seg_files, seg_file)
  se_meshes <- se_meshes + seg_meshes
  message(sprintf("  Segment done: %d meshes (cumulative: %d)", seg_meshes, se_meshes))
}

composite_segments(se_seg_files,
                   file.path(output_dir, "largest_neurons_neuroanatomy_all_sensory_all_effector.png"))

unlink(se_seg_files)
message(sprintf("=== SENSORY+EFFECTOR DONE: %d meshes rendered ===", se_meshes))

########################################################
### PART 3: ALL SENSORY + ALL EFFECTOR (SUPER CLASS) ###
########################################################

message("\n=== ALL SENSORY + ALL EFFECTOR (coloured by super_class) ===")

sens_eff_sc <- bm %>%
  dplyr::filter(grepl("sensory|motor|visceral", super_class),
                !is.na(root_850), !is.na(super_class), super_class != "")

sc_groups <- unique(sens_eff_sc$super_class)
message(sprintf("  %d super_classes, %d neurons", length(sc_groups), nrow(sens_eff_sc)))

# Map each super_class to its paper.cols colour (fall back to grey)
sc_cols <- vapply(sc_groups, function(sc) {
  if (sc %in% names(paper.cols) && !is.na(paper.cols[[sc]])) paper.cols[[sc]]
  else "grey50"
}, character(1))
names(sc_cols) <- sc_groups
message("  Super class colours:")
for (sc in sc_groups) message("    ", sc, " -> ", sc_cols[[sc]])

# Render one segment per super_class (each has bounded neuron count)
sc_seg_files <- character()
sc_meshes <- 0L

for (i in seq_along(sc_groups)) {
  sc <- sc_groups[i]
  sc_ids <- unique(na.omit(sens_eff_sc$root_850[sens_eff_sc$super_class == sc]))
  message(sprintf("\n[%d/%d] super_class=%s: %d neurons", i, length(sc_groups), sc, length(sc_ids)))

  if (!length(sc_ids)) next

  # For large super_classes, batch into sub-segments of 500 neurons
  batch_size <- 500
  n_batches <- ceiling(length(sc_ids) / batch_size)

  for (b in seq_len(n_batches)) {
    batch_start <- (b - 1) * batch_size + 1
    batch_end <- min(b * batch_size, length(sc_ids))
    batch_ids <- sc_ids[batch_start:batch_end]

    seg_file <- file.path(seg_dir, sprintf("sc_seg_%02d_%02d.png", i, b))
    g_sc <- make_base_plot()

    neurons <- fetch_meshes(batch_ids)
    if (length(neurons)) {
      neurons <- as.neuronlist(neurons)
      col <- sc_cols[[sc]]
      g_sc <- g_sc +
        geom_neuron(x = neurons,
                    cols = c(adjust_color_brightness(col, 1.1),
                             adjust_color_brightness(col, 0.9)),
                    rotation_matrix = bancr:::banc_rotation_matrices[["main"]],
                    alpha = mesh_alpha)
      n_added <- length(neurons)
      rm(neurons)
      gc(verbose = FALSE)

      message(sprintf("  Saving batch %d/%d: %s (%d meshes)", b, n_batches, seg_file, n_added))
      ggsave(plot = g_sc, filename = seg_file,
             width = plot_width, height = plot_height, dpi = dpi, bg = "transparent")
      sc_seg_files <- c(sc_seg_files, seg_file)
      sc_meshes <- sc_meshes + n_added
    }
    rm(g_sc)
    gc(verbose = FALSE)
  }
}

composite_segments(sc_seg_files,
                   file.path(output_dir, "largest_neurons_neuroanatomy_all_sensory_all_effector_super_class.png"))

unlink(sc_seg_files)
message(sprintf("=== SENSORY+EFFECTOR (SUPER CLASS) DONE: %d meshes rendered ===", sc_meshes))

message("\n### largest_neurons_neuroanatomy.R complete ###")
