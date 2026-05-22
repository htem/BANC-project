#' Automated CNS-network adjacency diagram (sanity check for Fig. 6c)
#'
#' Builds a ggraph layout of inter-CNS-network direct connectivity to
#' check the hand-drawn diagram in Fig. 6c. Edge weight is the number of
#' UNIQUE pre-neurons in source network A that have ≥ 1 above-threshold
#' synaptic connection (count ≥ 5) to ANY neuron in target network B;
#' edges are unidirectional and self-edges excluded.
#'
#' Left/right visual networks are merged into a single "visual" node, and
#' left/right olfactory networks into a single "olfactory" node, matching
#' how the Fig. 6c hand drawing groups them.
#'
#' Two variants of the diagram are produced: a threshold variant
#' (`min_partners = 500` unique source neurons per edge) and a top-K
#' variant (`top_k = 3` strongest outputs per source AND inputs per
#' target).
#'
#' @section Reads:
#'   banc.meta, banc.edgelist.simple
#'   .banc_spectral_csv via `banc.meta$cns_network` (SeaTable-backed —
#'   corrections made there are picked up at next session source).
#'
#' @section Writes:
#'   figures/figure_6/links/extra/panel_cns_network_diagram*.pdf
#'
#' @section Paper:
#'   Fig. 6c — strongest connections between CNS networks (hand drawing);
#'             this script is the automated sanity check, NOT the final
#'             figure asset.
#'   Methods §"Naming CNS networks".
#'
#' @section Schema:
#'   `count_thresh = 5` matches the paper-wide synapse threshold.
#'   `min_partners = 500` is the published-cut threshold for the diagram.
#'
#' @section Reproduce:
#'   BANC_NCORES=1 Rscript R/figures/panels_cns_network_diagram.R

source("R/startup/banc-startup.R")
source("R/startup/banc-meta.R")
source("R/startup/banc-edgelist.R")

library(igraph)
library(ggraph)
library(tidygraph)

# Thresholds
count_thresh   <- 5     # min synapses per pre→post pair to count the partner
min_partners   <- 500   # min unique source neurons per CNS→CNS edge (threshold variant)
top_k          <- 3     # top-K outputs per source AND top-K inputs per target (top-K variant)

# Helper: merge L/R visual and L/R olfactory networks (per user request,
# 2026-05-05). banc.meta$cns_network comes from SeaTable, so corrections
# made there are picked up automatically on the next session source.
.merge_lr_networks <- function(x) {
  dplyr::case_when(
    x %in% c("left visual", "right visual")     ~ "visual",
    x %in% c("left olfactory", "right olfactory") ~ "olfactory",
    TRUE ~ x
  )
}

##########################
## BUILD EDGE DATA      ##
##########################

# Direct connectivity between CNS networks: for each over-threshold synaptic
# edge (count >= 5), tag pre / post home network, merge L/R visual +
# olfactory, then count the number of UNIQUE PRE NEURONS in network A that
# have ≥1 above-threshold connection to ANY neuron in network B.
cns_edges <- banc.edgelist.simple %>%
  dplyr::select(pre, post, count) %>%
  dplyr::filter(count >= count_thresh) %>%
  dplyr::left_join(banc.meta %>%
                     dplyr::distinct(root_id, pre_cns = cns_network),
                   by = c("pre" = "root_id")) %>%
  dplyr::left_join(banc.meta %>%
                     dplyr::distinct(root_id, post_cns = cns_network),
                   by = c("post" = "root_id")) %>%
  dplyr::mutate(pre_cns  = .merge_lr_networks(pre_cns),
                post_cns = .merge_lr_networks(post_cns)) %>%
  dplyr::filter(!is.na(pre_cns), !is.na(post_cns),
                pre_cns != post_cns) %>%
  # Count unique source neurons per cross-network edge
  dplyr::distinct(pre, pre_cns, post_cns) %>%
  dplyr::count(pre_cns, post_cns, name = "n_partners") %>%
  dplyr::filter(n_partners >= min_partners) %>%
  dplyr::rename(from = pre_cns, to = post_cns)

message(sprintf("CNS network edges (count >= %d, n_partners >= %d): %d directed pairs, %d total source-neuron contributions",
                count_thresh, min_partners, nrow(cns_edges), sum(cns_edges$n_partners)))

##########################
## BUILD NODE DATA      ##
##########################

# Node size = number of neurons in each CNS network (after L/R merge)
cns_nodes <- banc.meta %>%
  dplyr::filter(!is.na(cns_network)) %>%
  dplyr::mutate(cns_network = .merge_lr_networks(cns_network)) %>%
  dplyr::count(cns_network, name = "n_neurons") %>%
  dplyr::rename(name = cns_network)

# Node colors from paper.cols
cns_nodes$color <- paper.cols[cns_nodes$name]
cns_nodes$color[is.na(cns_nodes$color)] <- "grey70"

# Short labels for readability — name on top, neuron count on bottom line.
cns_nodes$label <- paste0(gsub(" ", "\n", cns_nodes$name),
                            "\n(n=", scales::comma(cns_nodes$n_neurons), ")")

##########################
## BUILD GRAPH          ##
##########################

##########################
## PLOT HELPERS         ##
##########################

# Build a stress layout, then rotate the (x, y) coords 90° counter-clockwise
# so the diagram reads top-to-bottom rather than left-to-right
# (per user request 2026-05-06).
.rotated_layout <- function(graph, layout_method = "stress", seed = 42) {
  set.seed(seed)
  lay <- ggraph::create_layout(graph, layout = layout_method)
  .x <- lay$x; .y <- lay$y
  lay$x <- -.y
  lay$y <-  .x
  lay
}

# Build the diagram plot from a (nodes, edges) graph object. `subtitle_txt`
# describes which edge-selection rule produced the edges (for the title).
.build_diagram <- function(g_local, subtitle_txt, with_edge_labels = FALSE) {
  lay <- .rotated_layout(g_local)
  base <- ggraph::ggraph(lay) +
    ggraph::geom_edge_arc(
      mapping = if (with_edge_labels) {
        aes(width = n_partners, alpha = n_partners, label = n_partners)
      } else {
        aes(width = n_partners, alpha = n_partners)
      },
      arrow = arrow(length = unit(2.5, "mm"), type = "open"),
      end_cap   = ggraph::circle(12, "mm"),
      start_cap = ggraph::circle(12, "mm"),
      strength = 0.15,
      color = "grey40",
      label_size   = 2,
      label_colour = "black",
      label_dodge  = unit(3, "mm")
    ) +
    ggraph::scale_edge_width_continuous(
      range  = c(0.3, 4),
      name   = "source neurons",
      breaks = scales::pretty_breaks(4)
    ) +
    ggraph::scale_edge_alpha_continuous(range = c(0.2, 0.8), guide = "none") +
    ggraph::geom_node_point(
      aes(size = n_neurons, fill = color),
      shape = 21, color = "grey30", stroke = 0.5
    ) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_size_continuous(
      range  = c(8, 25),
      name   = "network\nneurons",
      breaks = scales::pretty_breaks(4)
    ) +
    ggraph::geom_node_text(aes(label = label), size = 2.5, fontface = "bold",
                           repel = FALSE) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "right",
      legend.text  = element_text(size = 8),
      legend.title = element_text(size = 9)
    ) +
    ggplot2::labs(title = paste("CNS network inter-connectivity —", subtitle_txt))
  base
}

##########################
## VARIANT 1 — THRESHOLD (≥ min_partners source neurons) ##
##########################

g_thresh <- tidygraph::tbl_graph(
  nodes = cns_nodes,
  edges = cns_edges,
  directed = TRUE
)
.thresh_subtitle <- sprintf("count >= %d, >= %d source neurons",
                            count_thresh, min_partners)

p_thresh <- .build_diagram(g_thresh, .thresh_subtitle, with_edge_labels = FALSE)
ggsave(
  plot = p_thresh,
  filename = file.path(banc.fig6.extra.path, "panel_cns_network_diagram.pdf"),
  width = 10, height = 14, dpi = 300   # taller than wide because rotated 90°
)
message("Saved: panel_cns_network_diagram.pdf")

p_thresh_lab <- .build_diagram(g_thresh, .thresh_subtitle, with_edge_labels = TRUE)
ggsave(
  plot = p_thresh_lab,
  filename = file.path(banc.fig6.extra.path, "panel_cns_network_diagram_labeled.pdf"),
  width = 10, height = 14, dpi = 300
)
message("Saved: panel_cns_network_diagram_labeled.pdf")

##########################
## VARIANT 2 — TOP-K (top 3 outputs per source ∪ top 3 inputs per target) ##
##########################

# Re-build edges WITHOUT the n_partners >= min_partners cut so the top-K rule
# can pick from the full set. Same upstream count_thresh and L/R merge.
cns_edges_full <- banc.edgelist.simple %>%
  dplyr::select(pre, post, count) %>%
  dplyr::filter(count >= count_thresh) %>%
  dplyr::left_join(banc.meta %>%
                     dplyr::distinct(root_id, pre_cns = cns_network),
                   by = c("pre" = "root_id")) %>%
  dplyr::left_join(banc.meta %>%
                     dplyr::distinct(root_id, post_cns = cns_network),
                   by = c("post" = "root_id")) %>%
  dplyr::mutate(pre_cns  = .merge_lr_networks(pre_cns),
                post_cns = .merge_lr_networks(post_cns)) %>%
  dplyr::filter(!is.na(pre_cns), !is.na(post_cns),
                pre_cns != post_cns) %>%
  dplyr::distinct(pre, pre_cns, post_cns) %>%
  dplyr::count(pre_cns, post_cns, name = "n_partners") %>%
  dplyr::rename(from = pre_cns, to = post_cns)

# Top-K with absolute floor: a node may show fewer than K outputs / inputs
# if its K-th candidate falls below `min_partners_top`.
min_partners_top <- 100
.top3_out <- cns_edges_full %>%
  dplyr::group_by(from) %>%
  dplyr::slice_max(n_partners, n = top_k, with_ties = FALSE) %>%
  dplyr::ungroup() %>%
  dplyr::filter(n_partners >= min_partners_top)
.top3_in <- cns_edges_full %>%
  dplyr::group_by(to) %>%
  dplyr::slice_max(n_partners, n = top_k, with_ties = FALSE) %>%
  dplyr::ungroup() %>%
  dplyr::filter(n_partners >= min_partners_top)
cns_edges_top <- dplyr::bind_rows(.top3_out, .top3_in) %>%
  dplyr::distinct(from, to, n_partners)

message(sprintf("Top-%d variant (>= %d): %d edges (%d source-top + %d target-top, deduped)",
                top_k, min_partners_top,
                nrow(cns_edges_top), nrow(.top3_out), nrow(.top3_in)))

g_top <- tidygraph::tbl_graph(
  nodes = cns_nodes,
  edges = cns_edges_top,
  directed = TRUE
)
.top_subtitle <- sprintf("top %d outputs per source ∪ top %d inputs per target",
                         top_k, top_k)

p_top <- .build_diagram(g_top, .top_subtitle, with_edge_labels = FALSE)
ggsave(
  plot = p_top,
  filename = file.path(banc.fig6.extra.path, "panel_cns_network_diagram_top3.pdf"),
  width = 10, height = 14, dpi = 300
)
message("Saved: panel_cns_network_diagram_top3.pdf")

p_top_lab <- .build_diagram(g_top, .top_subtitle, with_edge_labels = TRUE)
ggsave(
  plot = p_top_lab,
  filename = file.path(banc.fig6.extra.path, "panel_cns_network_diagram_top3_labeled.pdf"),
  width = 10, height = 14, dpi = 300
)
message("Saved: panel_cns_network_diagram_top3_labeled.pdf")
