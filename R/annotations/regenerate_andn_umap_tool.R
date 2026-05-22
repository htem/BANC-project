# Standalone script to regenerate the AN/DN UMAP cluster reassignment tool
# Reads UMAP coords from data/banc_annotations/v888/banc_neck_functional_classes.csv
# Joins manual_cluster + super_cluster from SeaTable cache (data/meta/bc_orig_cache.feather)
# To refresh: Rscript -e 'library(bancr); arrow::write_feather(banctable_query(), "data/meta/bc_orig_cache.feather")'

library(dplyr)
library(readr)
library(arrow)
library(bancr)
library(plotly)
library(crosstalk)
library(DT)
library(htmltools)
library(concaveman)
library(scales)

# Strip AN_/DN_/EFF_ prefix — show just numbers
strip_prefix <- function(x) gsub("^(AN_|DN_|EFF_)", "", x)

# Read UMAP data — force id to character for 18-digit root IDs
neck_df <- read_csv("data/banc_annotations/v888/banc_neck_functional_classes.csv", show_col_types = FALSE,
                    col_types = cols(id = col_character()))

# Pull fresh from SeaTable (fall back to cache if unreachable)
st_cache <- "data/meta/bc_orig_cache.feather"
st_raw <- tryCatch({
  message("Querying SeaTable...")
  bc <- banctable_query()
  arrow::write_feather(bc, st_cache)
  message("Fresh SeaTable pull: ", nrow(bc), " rows (cached to ", st_cache, ")")
  bc
}, error = function(e) {
  message("SeaTable unreachable: ", conditionMessage(e))
  if (file.exists(st_cache)) {
    message("Falling back to cached SeaTable: ", st_cache)
    arrow::read_feather(st_cache) %>% as.data.frame()
  } else {
    message("WARNING: No SeaTable cache — using CSV columns only")
    NULL
  }
})

if (!is.null(st_raw)) {
  st <- st_raw %>%
    as.data.frame() %>%
    mutate(root_850 = as.character(root_850)) %>%
    distinct(root_850, .keep_all = TRUE) %>%
    filter(root_850 %in% neck_df$id) %>%
    select(root_850, st_manual_cluster = manual_cluster,
           st_super_cluster = super_cluster)
  message("Matched ", nrow(st), " neck neurons")
  neck_df <- neck_df %>% left_join(st, by = c("id" = "root_850"))
} else {
  neck_df$st_manual_cluster <- NA_character_
  neck_df$st_super_cluster <- NA_character_
}

# Prep interactive data frame
reassign.df <- neck_df %>%
  filter(!is.na(UMAP1), !is.na(UMAP2)) %>%
  transmute(
    id = id,
    UMAP1, UMAP2,
    calculated_cluster = {
      x <- cluster
      ifelse(is.na(x) | x %in% c("", "0", "NA"), "unassigned", strip_prefix(as.character(x)))
    },
    manual_cluster = {
      x <- coalesce(st_manual_cluster,
                     if ("manual_cluster" %in% names(neck_df)) neck_df$manual_cluster[match(id, neck_df$id)] else NA_character_)
      ifelse(is.na(x) | x %in% c("", "0", "NA"), "unassigned", strip_prefix(as.character(x)))
    },
    super_cluster = {
      x <- coalesce(st_super_cluster, super_cluster)
      ifelse(is.na(x) | x %in% c("", "NA"), "unassigned", as.character(x))
    },
    cell_type = ifelse(is.na(cell_type), "", cell_type),
    cell_sub_type = ifelse(is.na(cell_sub_type), "", cell_sub_type),
    cell_function = ifelse(is.na(cell_function), "", cell_function),
    super_class = ifelse(is.na(super_class), "", super_class),
    side = ifelse(is.na(side), "", side)
  )

shared_data <- SharedData$new(reassign.df, key = ~id)

ax_range <- range(c(reassign.df$UMAP1, reassign.df$UMAP2))
ax_pad <- diff(ax_range) * 0.05
ax_lim <- list(ax_range[1] - ax_pad, ax_range[2] + ax_pad)

# Build plotly — points colored by super_cluster
# color= splits into one trace per super_cluster level
n_point_traces <- length(unique(reassign.df$super_cluster))

p <- plot_ly(
  shared_data,
  x = ~UMAP1, y = ~UMAP2,
  type = "scatter", mode = "markers",
  color = ~super_cluster,
  height = 750,
  text = ~paste0(cell_type, "\nfunction: ", cell_function,
                 "\ncalculated: ", calculated_cluster,
                 "\nmanual: ", manual_cluster,
                 "\nsuper_cluster: ", super_cluster,
                 "\nid: ", id),
  hoverinfo = "text",
  marker = list(size = 7, line = list(width = 0.5, color = "white"))
)

# --- Manual cluster hulls (solid lines) ---
manual_pts <- reassign.df %>% filter(manual_cluster != "unassigned")
manual_ids <- sort(unique(manual_pts$manual_cluster))
manual_cols <- hue_pal()(length(manual_ids))
names(manual_cols) <- manual_ids
n_manual_hull <- 0
for (cl in manual_ids) {
  pts <- manual_pts %>% filter(manual_cluster == cl)
  if (nrow(pts) < 3) next
  hull_dat <- as.data.frame(concaveman(
    as.matrix(pts[, c("UMAP1", "UMAP2")]), concavity = 2, length_threshold = 0.5))
  hull_dat <- rbind(hull_dat, hull_dat[1, , drop = FALSE])
  p <- add_trace(p, data = hull_dat,
    x = ~V1, y = ~V2, type = "scatter", mode = "lines",
    fill = "toself",
    fillcolor = paste0(manual_cols[[cl]], "22"),
    line = list(color = manual_cols[[cl]], width = 1.5),
    showlegend = FALSE, hoverinfo = "none", inherit = FALSE)
  n_manual_hull <- n_manual_hull + 1
}
# Manual centroid labels
manual_centroids <- manual_pts %>%
  group_by(manual_cluster) %>%
  summarise(UMAP1 = median(UMAP1), UMAP2 = median(UMAP2), .groups = "drop")
p <- add_text(p, data = manual_centroids,
  x = ~UMAP1, y = ~UMAP2, text = ~manual_cluster,
  textposition = "middle center",
  textfont = list(color = "grey30", size = 10, family = "Arial Black"),
  showlegend = FALSE, hoverinfo = "none", inherit = FALSE)
n_manual_traces <- n_manual_hull + 1  # hulls + 1 label trace

# --- Calculated cluster hulls (dashed lines) ---
calc_pts <- reassign.df %>% filter(calculated_cluster != "unassigned")
calc_ids <- sort(unique(calc_pts$calculated_cluster))
calc_cols <- hue_pal(h = c(180, 360))(length(calc_ids))
names(calc_cols) <- calc_ids
n_calc_hull <- 0
for (cl in calc_ids) {
  pts <- calc_pts %>% filter(calculated_cluster == cl)
  if (nrow(pts) < 3) next
  hull_dat <- as.data.frame(concaveman(
    as.matrix(pts[, c("UMAP1", "UMAP2")]), concavity = 2, length_threshold = 0.5))
  hull_dat <- rbind(hull_dat, hull_dat[1, , drop = FALSE])
  p <- add_trace(p, data = hull_dat,
    x = ~V1, y = ~V2, type = "scatter", mode = "lines",
    fill = "toself",
    fillcolor = paste0(calc_cols[[cl]], "15"),
    line = list(color = calc_cols[[cl]], width = 1.5, dash = "dash"),
    showlegend = FALSE, hoverinfo = "none", visible = FALSE, inherit = FALSE)
  n_calc_hull <- n_calc_hull + 1
}
# Calculated centroid labels
calc_centroids <- calc_pts %>%
  group_by(calculated_cluster) %>%
  summarise(UMAP1 = median(UMAP1), UMAP2 = median(UMAP2), .groups = "drop")
p <- add_text(p, data = calc_centroids,
  x = ~UMAP1, y = ~UMAP2, text = ~calculated_cluster,
  textposition = "middle center",
  textfont = list(color = "steelblue", size = 9, family = "Arial"),
  showlegend = FALSE, hoverinfo = "none", visible = FALSE, inherit = FALSE)
n_calc_traces <- n_calc_hull + 1  # hulls + 1 label trace

# Build visibility vectors for toggle buttons
# Trace order: [point traces] [manual hulls] [manual labels] [calc hulls] [calc labels]
n_total <- n_point_traces + n_manual_traces + n_calc_traces
vis_points <- rep(TRUE, n_point_traces)

vis_manual <- c(vis_points,
                rep(TRUE, n_manual_traces),
                rep(FALSE, n_calc_traces))
vis_calc   <- c(vis_points,
                rep(FALSE, n_manual_traces),
                rep(TRUE, n_calc_traces))
vis_both   <- rep(TRUE, n_total)

# Layout with toggle buttons
p <- layout(p,
  dragmode = "lasso",
  xaxis = list(title = "", zeroline = FALSE, showticklabels = FALSE,
               showgrid = FALSE, range = ax_lim),
  yaxis = list(title = "", zeroline = FALSE, showticklabels = FALSE,
               showgrid = FALSE, range = ax_lim),
  legend = list(orientation = "v", x = 1.02, y = 1,
                font = list(size = 9)),
  margin = list(r = 150, t = 60),
  updatemenus = list(
    list(
      type = "buttons",
      direction = "right",
      x = 0, y = 1.08, xanchor = "left",
      buttons = list(
        list(label = "Manual clusters",
             method = "restyle",
             args = list("visible", as.list(vis_manual))),
        list(label = "Calculated clusters",
             method = "restyle",
             args = list("visible", as.list(vis_calc))),
        list(label = "Both",
             method = "restyle",
             args = list("visible", as.list(vis_both)))
      )
    )
  )
) %>% highlight(on = "plotly_selected", off = "plotly_deselect")

# Linked table
dt <- datatable(shared_data,
  extensions = "Buttons",
  options = list(
    dom = "Bfrtip",
    buttons = list(
      list(extend = "copy", text = "Copy visible"),
      list(extend = "csv", text = "CSV visible",
           filename = "selected_neurons")
    ),
    pageLength = 50,
    scrollX = TRUE, scrollY = "650px",
    initComplete = JS(
      "function(settings, json) {",
      "  $(this.api().table().container()).css({'font-size': '11px'});",
      "}"
    )
  ),
  filter = "top",
  rownames = FALSE,
  selection = "none")

# Side-by-side: 2/3 UMAP, 1/3 table
out_path <- "figures/figure_3/links/extra/super_cluster_reassignment_tool.html"
widget <- browsable(
  tagList(
    tags$style("
      .tool-container { display: flex; gap: 10px; width: 100%; height: 90vh; }
      .tool-umap { flex: 2; min-width: 0; }
      .tool-table { flex: 1; min-width: 0; overflow-y: auto; }
      .tool-table .dataTables_wrapper { font-size: 11px; }
      h3 { font-size: 14px; margin: 5px 0; }
    "),
    h3("AN/DN cluster reassignment \u2014 lasso to filter table. Use buttons to toggle hull sets."),
    div(class = "tool-container",
      div(class = "tool-umap", p),
      div(class = "tool-table", dt)
    )
  )
)
save_html(widget, file = out_path)
message("Saved: ", out_path)
