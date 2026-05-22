#' Rebuild interactive AN/DN + EFF cluster-reassignment HTML widgets
#'
#' Lightweight tool-only script (no SeaTable mutation by default) that
#' regenerates the two interactive HTML cluster-reassignment widgets used
#' for AN/DN super_cluster curation and effector-cluster curation:
#'
#'   1. super_cluster_reassignment_tool.html (figure 3, AN/DN)
#'   2. eff_umap_cluster_reassignment_tool.html (figure 2 / ED Fig. 4f, effectors)
#'
#' Each widget shows the UMAP + cluster labels and lets the user
#' (interactively, in a browser) propose reassignments. The actual
#' SeaTable write is gated by a commented block + a JS string baked into
#' the widget, so opening the HTML does not trigger any write on its own.
#'
#' @section Reads:
#'   SeaTable (or cache) for current cluster / super_cluster columns
#'   data/banc_annotations/v888/banc_neck_functional_classes.csv                                    (AN/DN UMAP + cluster)
#'   data/banc_eff_umap_clusters.csv                                           (EFF UMAP + cluster)
#'
#' @section Writes:
#'   figures/figure_3/links/extra/super_cluster_reassignment_tool.html
#'   figures/figure_2/links/extra/eff_umap_cluster_reassignment_tool.html
#'
#' Hard constraint: SeaTable write blocks are commented out by default;
#' do not enable a write path from a Claude-driven run (CLAUDE.md hard
#' constraint "No SeaTable writes from figure-script runs"). This script
#' is excluded from the figure-runner.
#'

source("R/startup/banc-startup.R")

#############################
## LOAD SEATABLE METADATA  ##
#############################

# Always pull fresh SeaTable data — never silently fall back to stale cache
.bc_cache <- "data/meta/bc_orig_cache.feather"
bc.orig <- tryCatch(banctable_query(), error = function(e) {
  message("SeaTable query failed: ", e$message)
  data.frame()
})
if (is.data.frame(bc.orig) && nrow(bc.orig) > 0) {
  arrow::write_feather(bc.orig, .bc_cache)
  message("Fresh SeaTable pull: ", nrow(bc.orig), " rows (cache updated)")
} else {
  stop("SeaTable query returned no data. This script requires a live SeaTable connection.\n",
       "Run from an R session where banctable_query() works.")
}

# Build a lightweight metadata lookup from SeaTable
# Key columns: supervoxel_id, root_id, cell_type, cluster, manual_cluster, super_cluster, etc.
st_meta <- bc.orig %>%
  dplyr::transmute(
    st_id = `_id`,  # SeaTable row ID for updates
    root_id = as.character(root_id),
    supervoxel_id = as.character(supervoxel_id),
    cell_type = cell_type,
    cell_sub_type = if ("cell_sub_type" %in% names(.)) cell_sub_type else cell_type,
    cell_function = if ("cell_function" %in% names(.)) cell_function else NA_character_,
    super_class = if ("super_class" %in% names(.)) super_class else NA_character_,
    side = if ("side" %in% names(.)) side else NA_character_,
    cluster = if ("cluster" %in% names(.)) cluster else NA_character_,
    manual_cluster = if ("manual_cluster" %in% names(.)) manual_cluster else NA_character_,
    super_cluster = if ("super_cluster" %in% names(.)) super_cluster else NA_character_,
    root_626 = if ("root_626" %in% names(.)) as.character(root_626) else NA_character_
  ) %>%
  dplyr::distinct(root_id, .keep_all = TRUE)

# Apply manual_cluster → cluster override (same as banc-meta.R line 503)
if (sum(!is.na(st_meta$manual_cluster) & st_meta$manual_cluster != "") > 100) {
  st_meta$cluster <- dplyr::coalesce(st_meta$manual_cluster, st_meta$cluster)
}

message(sprintf("SeaTable metadata: %d neurons, %d with super_cluster, %d with cluster",
                nrow(st_meta),
                sum(!is.na(st_meta$super_cluster) & st_meta$super_cluster != ""),
                sum(!is.na(st_meta$cluster) & st_meta$cluster != "")))

##############################
## LOAD UMAP COORDS FROM CSV ##
##############################

# AN/DN UMAP
# UMAP1/UMAP2 = PCA-UMAP (primary, since 2026-04-08); cosine_UMAP1/2 retained for reference.
dn_csv <- readr::read_csv("data/banc_annotations/v888/banc_neck_functional_classes.csv",
                           col_types = readr::cols(.default = "c",
                                                   UMAP1 = "d", UMAP2 = "d",
                                                   cosine_UMAP1 = "d", cosine_UMAP2 = "d"))
# Join: UMAP coords from CSV + fresh metadata from SeaTable
umap.dn.df <- dn_csv %>%
  dplyr::select(id, UMAP1, UMAP2,
                dplyr::any_of(c("cosine_UMAP1", "cosine_UMAP2")),
                calculated_cluster = cluster) %>%
  dplyr::filter(!is.na(UMAP1)) %>%
  dplyr::left_join(st_meta, by = c("id" = "root_id"))

# EFF UMAP
eff_csv <- readr::read_csv("data/banc_annotations/v888/banc_efferent_functional_classes.csv",
                            col_types = readr::cols(.default = "c", UMAP1 = "d", UMAP2 = "d"))
umap.eff.df <- eff_csv %>%
  dplyr::select(id, UMAP1, UMAP2, calculated_cluster = cluster) %>%
  dplyr::filter(!is.na(UMAP1)) %>%
  dplyr::left_join(st_meta, by = c("id" = "root_id"))

# Load prior super_cluster from v626 CSV (join via root_626)
v626_csv <- readr::read_csv("data/banc_annotations/v626/banc_neck_functional_classes.csv",
                             col_types = readr::cols(.default = "c"))
v626_sc_map <- v626_csv %>%
  dplyr::select(root_626 = id, prior_super_cluster = super_cluster) %>%
  dplyr::filter(!is.na(prior_super_cluster), prior_super_cluster != "") %>%
  dplyr::distinct(root_626, .keep_all = TRUE)

umap.dn.df <- umap.dn.df %>%
  dplyr::left_join(v626_sc_map, by = "root_626") %>%
  dplyr::mutate(prior_super_cluster = ifelse(is.na(prior_super_cluster) | prior_super_cluster == "",
                                              "unassigned", prior_super_cluster))

# Build cell_function_plot (modality-only, for icon/coloring)
modality_lookup <- cns.functions %>%
  dplyr::filter(!is.na(modality), modality != "") %>%
  dplyr::distinct(cell_type, modality)
umap.dn.df <- umap.dn.df %>%
  dplyr::left_join(modality_lookup, by = "cell_type") %>%
  dplyr::mutate(cell_function_plot = dplyr::if_else(
    is.na(modality) | tolower(modality) == "unknown", NA_character_, modality))

# Load cluster method assignments (if available)
cluster_csv <- file.path(banc.fig3.extra.path, "cluster_options", "cluster_assignments_all_methods.csv")
if (file.exists(cluster_csv)) {
  cluster_methods <- read.csv(cluster_csv, stringsAsFactors = FALSE, colClasses = c(id = "character"))
  umap.dn.df <- umap.dn.df %>%
    dplyr::left_join(cluster_methods %>% dplyr::mutate(id = as.character(id)), by = "id")
  message(sprintf("Loaded %d cluster methods for %d neurons",
                  ncol(cluster_methods) - 1, sum(umap.dn.df$id %in% cluster_methods$id)))
}

message(sprintf("AN/DN UMAP: %d neurons (%d with prior super_cluster, %d with cell_function) | EFF UMAP: %d neurons",
                nrow(umap.dn.df),
                sum(umap.dn.df$prior_super_cluster != "unassigned"),
                sum(!is.na(umap.dn.df$cell_function_plot)),
                nrow(umap.eff.df)))

strip_prefix <- function(x) gsub("^(AN_|DN_|EFF_)", "", x)

# --- Estimated cluster: cell-type-aware boundary optimization ---
# Phase 1: For each cell type, find the majority cluster; move minority members
#           to that cluster if spatially adjacent (k-NN gate).
# Phase 2: Iterative boundary swapping — for each boundary neuron, swap to the
#           neighboring cluster with more members of the same cell type, if
#           spatially supported. Never empties a cluster (preserves cluster count).
#           Some cell types will remain split if they span non-adjacent clusters.
estimate_clusters <- function(df, base_col = "manual_cluster", leiden_col = NULL,
                              k_nn = 15, max_passes = 8,
                              min_nn_frac_phase1 = 0.4, min_nn_frac_phase2 = 0.25) {
  base <- df[[base_col]]
  ct <- df$cell_sub_type
  umap_coords <- as.matrix(df[, c("UMAP1", "UMAP2")])
  estimated <- base
  n_initial_clusters <- length(unique(estimated[estimated != "unassigned"]))

  nn_result <- nabor::knn(umap_coords, k = min(k_nn, nrow(umap_coords) - 1))

  # ---- Phase 1: Cell-type unification (adjacency-only) ----
  # For each cell type, find majority cluster; move minority members if k-NN adjacent
  ct_table <- tapply(base, ct, function(x) {
    x <- x[x != "unassigned"]
    if (length(x) == 0) return("unassigned")
    names(sort(table(x), decreasing = TRUE))[1]
  })

  n_unified <- 0L
  for (pass in 1:3) {
    n_pass <- 0L
    for (i in seq_len(nrow(df))) {
      if (is.na(ct[i]) || ct[i] == "" || estimated[i] == "unassigned") next
      target_cluster <- ct_table[[ct[i]]]
      if (is.na(target_cluster) || target_cluster == "unassigned") next
      if (estimated[i] == target_cluster) next

      # Don't empty a cluster
      if (sum(estimated == estimated[i], na.rm = TRUE) <= 2) next

      nn_clusters <- estimated[nn_result$nn.idx[i, ]]
      nn_in_target <- sum(nn_clusters == target_cluster, na.rm = TRUE) / length(nn_clusters)
      if (nn_in_target >= min_nn_frac_phase1) {
        estimated[i] <- target_cluster
        n_pass <- n_pass + 1L
      }
    }
    n_unified <- n_unified + n_pass
    message(sprintf("  Phase 1, pass %d: %d neurons unified", pass, n_pass))
    if (n_pass == 0) break
  }
  message(sprintf("  Phase 1 total (cell-type unification): %d neurons reassigned", n_unified))

  # ---- Phase 2: Cell-type-aware boundary swapping ----
  # For each boundary neuron (k-NN spans multiple clusters), check if a neighboring
  # cluster has strictly more members of the same cell_type. If so, and there's
  # spatial support, swap. This resolves overlapping boundaries while keeping cell
  # types together. Cluster count is preserved (never empty a cluster).
  n_swapped_total <- 0L
  for (pass in seq_len(max_passes)) {
    # Precompute cell_type × cluster counts for this pass
    valid <- estimated != "unassigned" & !is.na(ct) & ct != ""
    ct_cluster <- table(ct[valid], estimated[valid])

    # Precompute cluster sizes for empty-check
    cluster_sizes <- table(estimated[estimated != "unassigned"])

    n_swapped <- 0L
    for (i in seq_len(nrow(df))) {
      if (estimated[i] == "unassigned" || is.na(ct[i]) || ct[i] == "") next

      # k-NN cluster composition
      nn_clusters <- estimated[nn_result$nn.idx[i, ]]
      nn_clusters <- nn_clusters[nn_clusters != "unassigned"]
      if (length(nn_clusters) == 0) next

      neighbor_set <- unique(nn_clusters)
      # Skip interior neurons (all k-NN in same cluster)
      if (length(neighbor_set) == 1 && neighbor_set[1] == estimated[i]) next

      current <- estimated[i]
      my_ct <- ct[i]

      # How many of my cell_type are in my current cluster?
      ct_current <- if (my_ct %in% rownames(ct_cluster) && current %in% colnames(ct_cluster))
                      ct_cluster[my_ct, current] else 0L

      # Find best neighboring cluster: one with strictly more of my cell_type
      best <- current
      best_ct <- ct_current
      best_nn <- sum(nn_clusters == current) / length(nn_clusters)

      for (nc in setdiff(neighbor_set, current)) {
        ct_nc <- if (my_ct %in% rownames(ct_cluster) && nc %in% colnames(ct_cluster))
                   ct_cluster[my_ct, nc] else 0L
        nn_frac <- sum(nn_clusters == nc) / length(nn_clusters)

        # Strictly more cell_type members in the target cluster
        # Use nn_frac as tiebreaker when counts are equal
        if (ct_nc > best_ct || (ct_nc == best_ct && nn_frac > best_nn)) {
          best <- nc
          best_ct <- ct_nc
          best_nn <- nn_frac
        }
      }

      # Only swap if: target has strictly more cell_type members AND spatial support
      if (best != current && best_ct > ct_current && best_nn >= min_nn_frac_phase2) {
        # Don't empty a cluster
        if (cluster_sizes[[current]] <= 2) next
        estimated[i] <- best
        cluster_sizes[[current]] <- cluster_sizes[[current]] - 1L
        cluster_sizes[[best]] <- cluster_sizes[[best]] + 1L
        n_swapped <- n_swapped + 1L
      }
    }
    n_swapped_total <- n_swapped_total + n_swapped
    n_current_clusters <- length(unique(estimated[estimated != "unassigned"]))
    message(sprintf("  Phase 2, pass %d: %d neurons swapped (%d clusters, started with %d)",
                    pass, n_swapped, n_current_clusters, n_initial_clusters))
    if (n_swapped == 0) break
  }
  message(sprintf("  Phase 2 total (boundary swapping): %d neurons swapped", n_swapped_total))

  n_final_clusters <- length(unique(estimated[estimated != "unassigned"]))
  message(sprintf("  Estimated cluster complete: %d total changes from %s (%d → %d clusters)",
                  sum(estimated != base), base_col, n_initial_clusters, n_final_clusters))
  estimated
}

# --- Helper: build hull traces for a grouping column ---
build_hull_traces <- function(df, group_col, visible_default, line_style = "solid") {
  # Guard: column may not exist (e.g. estimated_cluster on the EFF tool)
  if (!group_col %in% colnames(df)) return(list())
  grp_pts <- df %>% dplyr::filter(.data[[group_col]] != "unassigned",
                                  !is.na(.data[[group_col]]))
  grp_ids <- sort(unique(grp_pts[[group_col]]))
  # Guard: zero valid groups → no hull traces (scales::hue_pal errors on n=0)
  if (length(grp_ids) == 0) return(list())
  grp_cols <- if (line_style == "dash") {
    scales::hue_pal(h = c(180, 360))(length(grp_ids))
  } else if (line_style == "dot") {
    scales::hue_pal(h = c(60, 240))(length(grp_ids))
  } else {
    scales::hue_pal()(length(grp_ids))
  }
  names(grp_cols) <- grp_ids
  traces <- list()
  for (cl in grp_ids) {
    pts <- grp_pts %>% dplyr::filter(.data[[group_col]] == cl)
    if (nrow(pts) < 3) next
    hull_dat <- as.data.frame(concaveman::concaveman(
      as.matrix(pts[, c("UMAP1", "UMAP2")]), concavity = 2, length_threshold = 0.5))
    hull_dat <- rbind(hull_dat, hull_dat[1, , drop = FALSE])
    traces[[length(traces) + 1]] <- list(type = "hull", data = hull_dat,
      color = grp_cols[[cl]], name = cl, visible = visible_default, dash = line_style)
  }
  centroids <- grp_pts %>%
    dplyr::group_by(.data[[group_col]]) %>%
    dplyr::summarise(UMAP1 = median(UMAP1), UMAP2 = median(UMAP2), .groups = "drop")
  for (i in seq_len(nrow(centroids))) {
    traces[[length(traces) + 1]] <- list(type = "label",
      x = centroids$UMAP1[i], y = centroids$UMAP2[i],
      text = centroids[[group_col]][i], visible = visible_default)
  }
  traces
}

# --- Helper: add hull/label traces to a plotly object ---
add_hull_traces <- function(p, all_traces) {
  for (tr in all_traces) {
    vis <- if (tr$visible) TRUE else FALSE
    if (tr$type == "hull") {
      p <- plotly::add_trace(p, data = tr$data,
        x = ~V1, y = ~V2, type = "scatter", mode = "lines",
        fill = "toself", fillcolor = paste0(tr$color, "22"),
        line = list(color = tr$color, width = 1.5, dash = tr$dash),
        showlegend = FALSE, hoverinfo = "none", visible = vis, inherit = FALSE)
    } else {
      p <- plotly::add_text(p, x = tr$x, y = tr$y, text = tr$text,
        textposition = "middle center",
        textfont = list(color = "grey30", size = 9, family = "Helvetica Bold"),
        showlegend = FALSE, hoverinfo = "none", visible = vis, inherit = FALSE)
    }
  }
  p
}

# --- Helper: build the full interactive widget ---
build_interactive_tool <- function(df, ct_group, title, output_path) {
  shared <- crosstalk::SharedData$new(df, key = ~id, group = ct_group)

  ax_range <- range(c(df$UMAP1, df$UMAP2), na.rm = TRUE)
  ax_pad <- diff(ax_range) * 0.05
  ax_lim <- list(ax_range[1] - ax_pad, ax_range[2] + ax_pad)

  # --- Build color vectors for each scheme ---
  lookup_color <- function(label) {
    if (is.na(label) || label == "" || label == "unassigned") return("#CCCCCC")
    if (label %in% names(paper.cols)) return(paper.cols[[label]])
    spaced <- gsub("_", " ", label)
    if (spaced %in% names(paper.cols)) return(paper.cols[[spaced]])
    "#999999"
  }

  # --- Build all color schemes ---
  # Helper: build a categorical color vector from a column using cerise_limon palette
  make_cat_colors <- function(vals) {
    vals <- as.character(vals)
    levels <- sort(unique(vals[!is.na(vals) & vals != "" & vals != "unassigned"]))
    pal <- cerise_limon_palette(max(length(levels), 1))
    names(pal) <- levels
    colors <- sapply(vals, function(x) {
      if (is.na(x) || x == "" || x == "unassigned") "#CCCCCC"
      else if (x %in% names(pal)) pal[[x]]
      else "#999999"
    }, USE.NAMES = FALSE)
    list(colors = colors, palette = pal)
  }

  # Named list: scheme_id → list(colors, palette, label)
  color_schemes <- list()

  # Helper: build a named palette from a color vector + labels (deduplicated, sorted)
  make_palette <- function(colors, labels) {
    df_tmp <- data.frame(label = as.character(labels), color = as.character(colors),
                         stringsAsFactors = FALSE)
    df_tmp <- df_tmp[!is.na(df_tmp$label) & df_tmp$label != "" & df_tmp$label != "unassigned", ]
    df_tmp <- df_tmp[!duplicated(df_tmp$label), ]
    df_tmp <- df_tmp[order(df_tmp$label), ]
    setNames(df_tmp$color, df_tmp$label)
  }

  # 1. super_cluster (paper.cols)
  colors_super <- sapply(df$super_cluster, lookup_color, USE.NAMES = FALSE)
  color_schemes[["super_cluster"]] <- list(
    colors = colors_super, palette = make_palette(colors_super, df$super_cluster), label = "super_cluster")

  # 2. prior_super_cluster (paper.cols)
  if ("prior_super_cluster" %in% names(df)) {
    colors_prior <- sapply(df$prior_super_cluster, lookup_color, USE.NAMES = FALSE)
    color_schemes[["prior_super_cluster"]] <- list(
      colors = colors_prior, palette = make_palette(colors_prior, df$prior_super_cluster), label = "prior super_cluster")
  }

  # 3. cell_function_plot
  if ("cell_function_plot" %in% names(df)) {
    cfp <- make_cat_colors(df$cell_function_plot)
    color_schemes[["cell_function"]] <- list(colors = cfp$colors, palette = cfp$palette, label = "cell function")
  }

  # 4+. Cluster method columns (method1_*, method2_*, etc.)
  method_cols <- grep("^method[0-9]", names(df), value = TRUE)
  for (mc in method_cols) {
    mc_data <- make_cat_colors(df[[mc]])
    # Make human-readable label from column name
    mc_label <- gsub("_", " ", gsub("^method[0-9]+_", "", mc))
    color_schemes[[mc]] <- list(colors = mc_data$colors, palette = mc_data$palette, label = mc_label)
  }

  # Also add manual_cluster and estimated_cluster as color options
  if ("manual_cluster" %in% names(df)) {
    mc_data <- make_cat_colors(df$manual_cluster)
    color_schemes[["manual_cluster"]] <- list(colors = mc_data$colors, palette = mc_data$palette, label = "manual cluster")
  }
  if ("estimated_cluster" %in% names(df)) {
    ec_data <- make_cat_colors(df$estimated_cluster)
    color_schemes[["estimated_cluster"]] <- list(colors = ec_data$colors, palette = ec_data$palette, label = "estimated cluster")
  }

  # --- Build plotly with multi-trace (one trace per super_cluster level) ---
  # Multi-trace gives crosstalk a more reliable per-trace key binding for the
  # plotly→DT lasso linking; the older single-trace approach with an explicit
  # marker.color array left lasso→table broken in the AN/DN tool.
  has_prior <- "prior_super_cluster" %in% names(df)
  has_cfp <- "cell_function_plot" %in% names(df)

  hover_text <- paste0(df$cell_type,
    "\nfunction: ", df$cell_function,
    "\ncalculated: ", df$calculated_cluster,
    "\nmanual: ", df$manual_cluster,
    if ("estimated_cluster" %in% names(df)) paste0("\nestimated: ", df$estimated_cluster) else "",
    "\nsuper_cluster: ", df$super_cluster,
    if (has_prior) paste0("\nprior_super_cluster: ", df$prior_super_cluster) else "",
    if (has_cfp) paste0("\ncell_function: ", ifelse(is.na(df$cell_function_plot), "unknown", df$cell_function_plot)) else "",
    "\nid: ", df$id)

  # Build a discrete palette in the same order plotly will iterate the levels
  super_levels <- sort(unique(as.character(df$super_cluster)))
  super_palette <- setNames(sapply(super_levels, lookup_color), super_levels)

  # Attach the per-row id as `customdata` so the lasso → DT bridge can extract
  # ids directly from plotly_selected events. Without this, the only extraction
  # path is the text-fallback regex on hover text, which is brittle.
  p <- plotly::plot_ly(shared,
    x = ~UMAP1, y = ~UMAP2,
    type = "scatter", mode = "markers",
    color = ~super_cluster,
    colors = super_palette,
    customdata = ~id,
    height = 750,
    text = hover_text,
    hoverinfo = "text",
    showlegend = FALSE,
    marker = list(size = 7, line = list(width = 0.5, color = "white")))

  n_point_traces <- length(super_levels)  # one trace per super_cluster level

  # Pre-compute per-trace df row indices for color toggling. plotly groups rows
  # by the `color` variable in the order of `super_levels`, preserving within-
  # group row order. `trace_row_idx[[k]]` lists the df rows that end up in
  # plotly trace k (1-based, in the order plotly will draw them).
  trace_for_row <- match(as.character(df$super_cluster), super_levels)
  trace_row_idx <- split(seq_len(nrow(df)), trace_for_row)
  # Ensure list is ordered 1..n_point_traces (split() gives string-named list)
  trace_row_idx <- trace_row_idx[as.character(seq_len(n_point_traces))]

  # --- Hull traces ---
  manual_traces    <- build_hull_traces(df, "manual_cluster", TRUE, "solid")
  calc_traces      <- build_hull_traces(df, "calculated_cluster", FALSE, "dash")
  has_estimated    <- "estimated_cluster" %in% names(df)
  estimated_traces <- if (has_estimated) build_hull_traces(df, "estimated_cluster", FALSE, "solid") else list()
  super_traces     <- build_hull_traces(df, "super_cluster", FALSE, "dot")
  all_traces <- c(manual_traces, calc_traces, estimated_traces, super_traces)
  p <- add_hull_traces(p, all_traces)

  n_manual    <- length(manual_traces)
  n_calc      <- length(calc_traces)
  n_estimated <- length(estimated_traces)
  n_super     <- length(super_traces)
  make_vis <- function(show_m, show_c, show_e, show_s) {
    c(rep(TRUE, n_point_traces),
      rep(show_m, n_manual), rep(show_c, n_calc),
      rep(show_e, n_estimated), rep(show_s, n_super))
  }

  # --- Hull toggle buttons (row 1) ---
  hull_buttons <- list(
    list(label = "Manual cluster", method = "restyle",
         args = list("visible", make_vis(TRUE, FALSE, FALSE, FALSE))),
    list(label = "Calculated cluster", method = "restyle",
         args = list("visible", make_vis(FALSE, TRUE, FALSE, FALSE))))
  if (has_estimated) {
    hull_buttons <- c(hull_buttons, list(
      list(label = "Estimated cluster", method = "restyle",
           args = list("visible", make_vis(FALSE, FALSE, TRUE, FALSE)))))
  }
  hull_buttons <- c(hull_buttons, list(
    list(label = "Super cluster", method = "restyle",
         args = list("visible", make_vis(FALSE, FALSE, FALSE, TRUE))),
    list(label = "No hulls", method = "restyle",
         args = list("visible", make_vis(FALSE, FALSE, FALSE, FALSE)))))

  p <- plotly::layout(p,
    dragmode = "lasso",
    xaxis = list(title = "", zeroline = FALSE, showticklabels = FALSE,
                 showgrid = FALSE, range = ax_lim),
    yaxis = list(title = "", zeroline = FALSE, showticklabels = FALSE,
                 showgrid = FALSE, range = ax_lim),
    legend = list(orientation = "v", x = 1.02, y = 1, font = list(size = 9)),
    margin = list(r = 150, t = 60),
    updatemenus = list(
      list(type = "buttons", direction = "right",
           x = 0, y = 1.08, xanchor = "left", yanchor = "top",
           showactive = TRUE, buttons = hull_buttons)
    )
  ) %>% plotly::highlight(on = "plotly_selected", off = "plotly_deselect")

  # --- Build HTML color legends and buttons dynamically from color_schemes ---
  make_legend_html <- function(palette, id_suffix, visible = FALSE) {
    items <- paste0(
      '<span style="display:inline-block;margin:1px 6px 1px 0;">',
      '<span style="display:inline-block;width:10px;height:10px;background:',
      palette, ';border:1px solid #999;margin-right:2px;vertical-align:middle;"></span>',
      '<span style="font-size:10px;vertical-align:middle;">', names(palette), '</span></span>')
    htmltools::div(id = paste0("legend_", id_suffix),
                   style = if (!visible) "display:none;" else "",
                   class = "color-legend",
                   htmltools::HTML(paste(items, collapse = " ")))
  }

  scheme_ids <- names(color_schemes)
  legend_divs <- lapply(scheme_ids, function(sid) {
    make_legend_html(color_schemes[[sid]]$palette, sid, visible = (sid == "super_cluster"))
  })

  # Serialize per-trace color arrays for JS. With multi-trace point plots,
  # we need a (n_point_traces × variable) array per scheme so Plotly.restyle
  # can apply per-trace colors to all point traces at once.
  per_trace_color_parts <- sapply(scheme_ids, function(sid) {
    scheme_colors <- color_schemes[[sid]]$colors
    per_trace <- lapply(trace_row_idx, function(rows) scheme_colors[rows])
    sprintf('"%s": %s', sid, jsonlite::toJSON(per_trace, auto_unbox = FALSE))
  })
  color_data_js <- paste0('var perTraceColors = {',
                          paste(per_trace_color_parts, collapse = ', '), '};\n',
                          'var pointTraceIndices = [',
                          paste(seq_len(n_point_traces) - 1L, collapse = ', '),
                          '];')
  legend_ids_js <- paste0('var legendIds = [', paste0('"', scheme_ids, '"', collapse = ', '), '];')

  # Button bar
  btn_style <- "font-size: 10px; padding: 2px 6px; cursor: pointer; margin-right: 3px;"
  color_btn_html <- htmltools::div(
    style = "margin: 3px 0; font-size: 11px; flex-wrap: wrap; display: flex; align-items: center; gap: 2px;",
    htmltools::tags$b("Colors: ", style = "font-size: 11px; margin-right: 4px;"),
    lapply(scheme_ids, function(sid) {
      htmltools::tags$button(
        id = paste0("cbtn_", sid),
        color_schemes[[sid]]$label,
        class = if (sid == "super_cluster") "color-btn active-color-btn" else "color-btn",
        style = btn_style)
    })
  )

  color_toggle_js <- htmltools::tags$script(htmltools::HTML(sprintf('
document.addEventListener("DOMContentLoaded", function() {
  // Wait for plotly to fully render (it creates .js-plotly-plot after DOMContentLoaded)
  var attempts = 0;
  var initColors = setInterval(function() {
    attempts++;
    var plotEl = document.querySelector(".js-plotly-plot") || document.querySelector(".plotly");
    if (!plotEl && attempts < 20) return;
    clearInterval(initColors);
    if (!plotEl) { console.log("No plotly element found"); return; }

    %s
    %s

    document.querySelectorAll(".color-btn").forEach(function(btn) {
      btn.addEventListener("click", function() {
        var scheme = btn.id.replace("cbtn_", "");
        var pe = document.querySelector(".js-plotly-plot") || document.querySelector(".plotly");
        if (perTraceColors[scheme] && pe) {
          // Multi-trace restyle: one color array per point trace
          Plotly.restyle(pe, {"marker.color": perTraceColors[scheme]}, pointTraceIndices);
        }
        legendIds.forEach(function(lid) {
          var el = document.getElementById("legend_" + lid);
          if (el) el.style.display = (lid === scheme) ? "" : "none";
        });
        document.querySelectorAll(".color-btn").forEach(function(b) {
          b.classList.remove("active-color-btn");
        });
        btn.classList.add("active-color-btn");
      });
    });
  }, 500);
});', color_data_js, legend_ids_js)))

  # Hide st_id column (index 0) from display but keep in data for JS access
  has_st_id <- "st_id" %in% names(df)
  st_id_col_idx <- if (has_st_id) which(names(df) == "st_id") - 1L else NULL  # 0-based

  dt <- DT::datatable(shared,
    extensions = "Buttons",
    options = list(
      dom = "Bfrtip",
      buttons = list(
        list(extend = "copy", text = "Copy visible"),
        list(extend = "csv", text = "CSV visible", filename = "selected_neurons"),
        # Custom: copy ONLY the id column from currently filtered/visible
        # rows, comma-separated, to the clipboard. Looks up the id column
        # dynamically by header text so it survives column reordering.
        list(extend = "copy", text = "Copy IDs", action = DT::JS(
          "function(e, dt, node, config) {",
          "  var hdrs = dt.columns().header();",
          "  var idColIdx = -1;",
          "  for (var i = 0; i < hdrs.length; i++) {",
          "    var h = hdrs[i];",
          "    if (h && h.textContent && h.textContent.trim().toLowerCase() === 'id') {",
          "      idColIdx = i; break;",
          "    }",
          "  }",
          "  if (idColIdx < 0) { alert('Copy IDs: id column not found'); return; }",
          "  var ids = [];",
          "  dt.rows({search: 'applied'}).every(function() {",
          "    var v = this.data()[idColIdx];",
          "    if (v != null && v !== '') ids.push(String(v));",
          "  });",
          "  var csv = ids.join(',');",
          "  var btn = $(node);",
          "  var orig = btn.find('span').length ? btn.find('span').text() : btn.text();",
          "  function flash(msg) {",
          "    if (btn.find('span').length) { btn.find('span').text(msg); }",
          "    else { btn.text(msg); }",
          "    setTimeout(function() {",
          "      if (btn.find('span').length) { btn.find('span').text(orig); }",
          "      else { btn.text(orig); }",
          "    }, 1500);",
          "  }",
          "  function fallback() {",
          "    var ta = document.createElement('textarea');",
          "    ta.value = csv;",
          "    ta.style.position = 'fixed';",
          "    ta.style.left = '-9999px';",
          "    document.body.appendChild(ta);",
          "    ta.focus(); ta.select();",
          "    try { document.execCommand('copy'); } catch(err) {}",
          "    document.body.removeChild(ta);",
          "    flash('Copied ' + ids.length + ' IDs');",
          "  }",
          "  if (navigator.clipboard && navigator.clipboard.writeText) {",
          "    navigator.clipboard.writeText(csv).then(function() {",
          "      flash('Copied ' + ids.length + ' IDs');",
          "    }).catch(fallback);",
          "  } else { fallback(); }",
          "}"))),
      pageLength = 50, scrollX = TRUE, scrollY = "650px",
      columnDefs = if (has_st_id) list(list(visible = FALSE, targets = st_id_col_idx)) else list(),
      initComplete = DT::JS(
        "function(settings, json) {",
        "  $(this.api().table().container()).css({'font-size': '11px'});",
        "}")
    ),
    filter = "top", rownames = FALSE, selection = "none")

  # Column index for the 'id' key column (0-based); shifts if st_id is present
  id_col_idx <- which(names(df) == "id") - 1L

  # Direct-event bridge: rather than relying on crosstalk's plotly→DT auto-link
  # (which has been flaky for the AN/DN tool, particularly with many hull traces
  # and a custom column layout), wire plotly_selected straight into a DT column
  # search. Includes a visible debug status indicator at bottom-left so we can
  # see what the bridge is doing in the browser.
  bridge_js <- htmltools::tags$script(htmltools::HTML(sprintf('
(function() {
  function ensureStatus() {
    var s = document.getElementById("bridge-status");
    if (!s) {
      s = document.createElement("div");
      s.id = "bridge-status";
      s.style.cssText = "position:fixed; bottom:5px; left:5px; z-index:9999; " +
        "background:#fff; border:1px solid #999; padding:3px 6px; " +
        "font:11px monospace; color:#333; max-width:600px;";
      document.body.appendChild(s);
    }
    return s;
  }
  function setStatus(msg) {
    var s = ensureStatus();
    s.textContent = "[bridge] " + msg;
    console.log("[bridge]", msg);
  }
  function findIdCol(dtApi) {
    var idx = 0;
    var hdrs = dtApi.columns().header();
    for (var i = 0; i < hdrs.length; i++) {
      var h = hdrs[i];
      if (h && h.textContent && h.textContent.trim().toLowerCase() === "id") {
        return i;
      }
    }
    return idx;
  }
  function findPlotDiv() {
    var candidates = [".js-plotly-plot", ".plotly.html-widget .js-plotly-plot",
                      ".plotly.html-widget"];
    for (var i = 0; i < candidates.length; i++) {
      var els = document.querySelectorAll(candidates[i]);
      for (var j = 0; j < els.length; j++) {
        if (typeof els[j].on === "function") return els[j];
      }
    }
    return null;
  }
  var attempts = 0;
  function init() {
    attempts++;
    var tables = document.querySelectorAll("table.dataTable");
    if (tables.length === 0) {
      if (attempts < 60) { setTimeout(init, 300); return; }
      setStatus("FAIL: no DT table after " + attempts + " attempts"); return;
    }
    var dtApi = $(tables[0]).DataTable();
    if (!dtApi) {
      if (attempts < 60) { setTimeout(init, 300); return; }
      setStatus("FAIL: DT API unavailable"); return;
    }
    var pdiv = findPlotDiv();
    if (!pdiv) {
      if (attempts < 60) { setTimeout(init, 300); return; }
      setStatus("FAIL: no plotly div after " + attempts + " attempts"); return;
    }
    var idColIdx = findIdCol(dtApi);
    var allKeys = dtApi.rows().data().toArray().map(function(r) { return r[idColIdx]; });
    var selHandle = null;
    try { selHandle = new crosstalk.SelectionHandle("%s"); } catch(e) {}
    var isFiltering = false;
    var lassoActive = false;

    // Direction 1: table search/filter → highlight UMAP via crosstalk
    dtApi.on("draw.dt", function() {
      if (lassoActive || !selHandle) return;
      var visKeys = [];
      dtApi.rows({search: "applied"}).every(function() {
        visKeys.push(this.data()[idColIdx]);
      });
      if (visKeys.length < allKeys.length && visKeys.length > 0) {
        isFiltering = true;
        selHandle.set(visKeys);
      } else if (isFiltering) {
        isFiltering = false;
        selHandle.clear();
      }
    });

    // Direction 2: lasso on UMAP → DT column search
    function handleSelected(eventData) {
      if (!eventData || !eventData.points || eventData.points.length === 0) {
        setStatus("plotly_selected fired but no points"); return;
      }
      var seen = {};
      var nWithCustom = 0, nWithText = 0, nWithKey = 0;
      eventData.points.forEach(function(pt) {
        // Try plotly customdata, the trace key array, then hover text fallback
        if (pt.customdata != null && pt.customdata !== "") {
          seen[String(pt.customdata)] = true;
          nWithCustom++;
        } else if (pt.data && pt.data.key && pt.data.key[pt.pointNumber] != null) {
          seen[String(pt.data.key[pt.pointNumber])] = true;
          nWithKey++;
        } else if (pt.text) {
          var m = String(pt.text).match(/id:\\s*(\\d+)/);
          if (m) { seen[m[1]] = true; nWithText++; }
        }
      });
      var idList = Object.keys(seen);
      setStatus("lasso → " + idList.length + " ids (custom=" + nWithCustom +
                ", key=" + nWithKey + ", text=" + nWithText + ")");
      if (idList.length === 0) return;
      var pattern = "^(" + idList.join("|") + ")$";
      lassoActive = true;
      dtApi.column(idColIdx).search(pattern, true, false).draw();
      lassoActive = false;
    }
    function handleDeselect() {
      setStatus("lasso cleared");
      lassoActive = true;
      dtApi.column(idColIdx).search("").draw();
      lassoActive = false;
    }
    pdiv.on("plotly_selected", handleSelected);
    pdiv.on("plotly_deselect", handleDeselect);

    // Expose for browser console debugging
    window.__bridge = {
      pdiv: pdiv, dtApi: dtApi, idColIdx: idColIdx,
      handleSelected: handleSelected, handleDeselect: handleDeselect,
      testFilter: function() {
        var ids = allKeys.slice(0, 5);
        var pattern = "^(" + ids.join("|") + ")$";
        lassoActive = true;
        dtApi.column(idColIdx).search(pattern, true, false).draw();
        lassoActive = false;
        setStatus("test: filtered to 5 rows");
      }
    };
    setStatus("ready: " + allKeys.length + " rows, idCol=" + idColIdx +
              " — try lasso, or run window.__bridge.testFilter()");
  }
  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", function() { setTimeout(init, 500); });
  } else {
    setTimeout(init, 500);
  }
})();', ct_group)))

  # --- Code generation panel: lasso/filter → R code for SeaTable update ---
  codegen_ui <- htmltools::div(
    id = "codegen-panel",
    style = "background: #f8f8f8; border: 1px solid #ccc; padding: 8px; margin: 5px 0; font-size: 12px;",
    htmltools::div(style = "display: flex; gap: 8px; align-items: center;",
      htmltools::tags$button(id = "btn_generate", "Generate R code",
                             style = "font-size: 11px; padding: 3px 10px; cursor: pointer;"),
      htmltools::tags$button(id = "btn_copy", "Copy",
                             style = "font-size: 11px; padding: 3px 10px; cursor: pointer; display: none;"),
      htmltools::tags$span(id = "codegen_status", style = "font-size: 11px; color: #666;")
    ),
    htmltools::tags$textarea(id = "r_code_output", rows = "8", readonly = "readonly",
                             style = "width: 100%; font-family: monospace; font-size: 10px; display: none; margin-top: 5px;")
  )

  codegen_js <- htmltools::tags$script(htmltools::HTML(paste0('
document.addEventListener("DOMContentLoaded", function() {
  setTimeout(function() {
    var selHandle = new crosstalk.SelectionHandle("', ct_group, '");

    document.getElementById("btn_generate").addEventListener("click", function() {
      var keys = selHandle.value;
      var status = document.getElementById("codegen_status");
      var output = document.getElementById("r_code_output");
      var copyBtn = document.getElementById("btn_copy");

      if (!keys || keys.length === 0) {
        status.textContent = "No neurons selected \\u2014 use lasso on UMAP or filter the table";
        return;
      }

      var lines = [];
      lines.push("# " + keys.length + " neurons selected from UMAP tool");
      lines.push("selected_ids <- c(");
      for (var i = 0; i < keys.length; i++) {
        var comma = (i < keys.length - 1) ? "," : "";
        lines.push("  \\"" + keys[i] + "\\"" + comma);
      }
      lines.push(")");
      lines.push("");
      lines.push("# ---- Edit these before running ----");
      lines.push("new_manual_cluster <- \\"CHANGE_ME\\"");
      lines.push("new_super_cluster  <- \\"CHANGE_ME\\"");
      lines.push("");
      lines.push("push.df <- banc.meta %>%");
      lines.push("  filter(root_id %in% selected_ids) %>%");
      lines.push("  transmute(`_id` = st_id,");
      lines.push("            manual_cluster = new_manual_cluster,");
      lines.push("            super_cluster  = new_super_cluster)");
      lines.push("");
      lines.push("banctable_update_rows(base = \\"banc_meta\\", table = \\"banc_meta\\",");
      lines.push("  df = push.df, append_allowed = FALSE, chunksize = 1000)");

      output.value = lines.join("\\n");
      output.style.display = "block";
      copyBtn.style.display = "inline";
      status.textContent = keys.length + " neurons \\u2192 R code ready";
    });

    document.getElementById("btn_copy").addEventListener("click", function() {
      var output = document.getElementById("r_code_output");
      output.select();
      document.execCommand("copy");
      document.getElementById("codegen_status").textContent = "Copied!";
    });
  }, 1500);
});')))

  widget <- htmltools::browsable(htmltools::tagList(
    crosstalk::crosstalkLibs(),
    htmltools::tags$style("
      .tool-container { display: flex; gap: 10px; width: 100%; height: 90vh; }
      .tool-umap { flex: 2; min-width: 0; }
      .tool-table { flex: 1; min-width: 0; overflow-y: auto; }
      .tool-table .dataTables_wrapper { font-size: 11px; }
      h3 { font-size: 14px; margin: 5px 0; }
      .color-legend { margin: 2px 0; line-height: 1.6; }
      .active-color-btn { background: #4a90d9; color: white; }
      .color-btn { border: 1px solid #999; border-radius: 3px; background: #f0f0f0; }
    "),
    htmltools::h3(title),
    codegen_ui,
    color_btn_html,
    legend_divs,
    htmltools::div(class = "tool-container",
      htmltools::div(class = "tool-umap", p),
      htmltools::div(class = "tool-table", dt)),
    bridge_js,
    codegen_js,
    color_toggle_js
  ))

  htmltools::save_html(widget, file = output_path)
  message("Saved: ", output_path)
}

########################################
## 1. AN/DN CLUSTER REASSIGNMENT TOOL ##
########################################

message("=== Building AN/DN tool ===")
reassign.df <- umap.dn.df %>%
  dplyr::mutate(
    id = as.character(id),
    manual_cluster = dplyr::if_else(
      is.na(cluster) | cluster %in% c("", "0", "NA"),
      "unassigned", strip_prefix(as.character(cluster))),
    calculated_cluster = dplyr::if_else(
      is.na(calculated_cluster) | calculated_cluster %in% c("", "0", "NA"),
      "unassigned", strip_prefix(as.character(calculated_cluster))),
    super_cluster = dplyr::if_else(
      is.na(super_cluster) | super_cluster %in% c("", "0", "NA"),
      "unassigned", as.character(super_cluster)),
    cell_function = ifelse(is.na(cell_function), "", cell_function),
    cell_type = ifelse(is.na(cell_type), "", cell_type),
    cell_sub_type = ifelse(is.na(cell_sub_type), "", cell_sub_type),
    super_class = ifelse(is.na(super_class), "", super_class),
    side = ifelse(is.na(side), "", side)
  )

reassign.df$estimated_cluster <- estimate_clusters(reassign.df, base_col = "manual_cluster")

reassign.df <- reassign.df %>%
  dplyr::select(st_id, id, cell_type, cell_sub_type, cell_function,
                calculated_cluster, manual_cluster, estimated_cluster, super_cluster,
                prior_super_cluster, cell_function_plot,
                dplyr::matches("^method[0-9]"),
                super_class, side, UMAP1, UMAP2)

build_interactive_tool(
  df = reassign.df,
  ct_group = "andn_umap",
  title = "AN/DN cluster reassignment \u2014 Lasso UMAP or search/filter table. Buttons toggle hulls.",
  output_path = file.path(banc.fig3.extra.path, "super_cluster_reassignment_tool.html")
)

######################################
## 2. EFF CLUSTER REASSIGNMENT TOOL ##
######################################

message("=== Building EFF tool ===")
eff_interactive_df <- umap.eff.df %>%
  dplyr::transmute(
    st_id = st_id,
    id = as.character(id),
    UMAP1, UMAP2,
    calculated_cluster = ifelse(is.na(calculated_cluster) | calculated_cluster == "",
                                "unassigned", as.character(calculated_cluster)),
    manual_cluster = ifelse(is.na(cluster) | cluster == "",
                            "unassigned", as.character(cluster)),
    super_cluster = ifelse(is.na(super_cluster) | super_cluster == "",
                           "unassigned", as.character(super_cluster)),
    cell_type = ifelse(is.na(cell_type), "", cell_type),
    cell_sub_type = ifelse(is.na(cell_sub_type), "", cell_sub_type),
    cell_function = ifelse(is.na(cell_function), "", cell_function),
    super_class = ifelse(is.na(super_class), "", super_class),
    side = ifelse(is.na(side), "", side)
  )

build_interactive_tool(
  df = eff_interactive_df,
  ct_group = "eff_umap",
  title = "Efferent UMAP \u2014 Lasso UMAP or search/filter table to highlight. Buttons toggle hull boundaries.",
  output_path = file.path(banc.fig2.extra.path, "eff_umap_cluster_reassignment_tool.html")
)

message("=== Done ===")

##############################################################
## OPTIONAL: Push estimated_cluster to SeaTable as manual_cluster
## Uncomment and run interactively after reviewing the tool.
## Only updates AN/DN neurons where estimated_cluster differs from manual_cluster.
##############################################################

# Helper: mode for character vectors
mode_chr <- function(x) {
  x <- as.character(stats::na.omit(x[x != "" & x != "unassigned"]))
  if (length(x) == 0L) return(NA_character_)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Start from reassign.df (built above) — has id, super_class, manual_cluster, estimated_cluster, super_cluster
update.df <- reassign.df %>%
  dplyr::filter(estimated_cluster != "unassigned",
                manual_cluster != estimated_cluster) %>%
  dplyr::mutate(
    # Re-add AN_/DN_ prefix with zero-padded number
    new_cluster = dplyr::case_when(
      grepl("descending", super_class) ~ paste0("DN_", stringr::str_pad(estimated_cluster, width = 2, pad = "0")),
      grepl("ascending", super_class)  ~ paste0("AN_", stringr::str_pad(estimated_cluster, width = 2, pad = "0")),
      TRUE ~ estimated_cluster
    )
  )

# Compute new super_cluster: modal super_cluster within each estimated cluster
cluster_super <- reassign.df %>%
  dplyr::filter(estimated_cluster != "unassigned",
                super_cluster != "unassigned") %>%
  dplyr::group_by(estimated_cluster) %>%
  dplyr::summarise(new_super_cluster = mode_chr(super_cluster), .groups = "drop")

update.df <- update.df %>%
  dplyr::left_join(cluster_super, by = "estimated_cluster")

message(sprintf("Neurons to update: %d (cluster changed)", nrow(update.df)))

# Match to SeaTable _id via root_id
st_ids <- banctable_query("SELECT _id, root_id FROM banc_meta") %>%
  dplyr::mutate(root_id = as.character(root_id))

push.df <- update.df %>%
  dplyr::left_join(st_ids, by = c("id" = "root_id")) %>%
  dplyr::filter(!is.na(`_id`)) %>%
  dplyr::transmute(
    `_id`,
    manual_cluster = new_cluster,
    super_cluster = dplyr::if_else(!is.na(new_super_cluster), new_super_cluster, super_cluster)
  ) %>%
  base::as.data.frame()

message(sprintf("Pushing %d rows to SeaTable (manual_cluster + super_cluster)...", nrow(push.df)))
print(table(push.df$manual_cluster))

# banctable_update_rows(base = "banc_meta",
#                       table = "banc_meta",
#                       df = push.df,
#                       append_allowed = FALSE,
#                       chunksize = 1000)
# message("SeaTable updated.")
