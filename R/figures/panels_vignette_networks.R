#' Vignette network diagrams for paper exemplar circuits (Fig. 3c/g/h/i/j/k, 4f/g, 5c, 6h)
#'
#' Renders the manually-curated example circuit diagrams used throughout
#' the paper to illustrate cluster behaviour and cross-cluster motifs.
#' Each vignette is driven by one curated CSV at
#' `figures/vignette_neuron_lists/network_<vig>_neurons.csv`, one row per
#' neuron with pre-resolved `root_id`, `super_class`, `super_cluster`,
#' and side/neurotransmitter metadata.
#'
#' For each vignette the script (1) reads the curated CSV, (2) aggregates
#' edges from `banc.edgelist.simple` between the contained root_ids,
#' grouped by `display_name`, (3) lays out a Sugiyama network coloured by
#' `super_cluster` with edges coloured by the majority pre-neurotransmitter,
#' and (4) writes `network_<vig>.pdf`, `network_<vig>_edges.csv`, and a
#' copy of the curated neurons CSV (without the
#' `cell_representative_point_id` column) into `figures/vignettes/`.
#'
#' @section Reads:
#'   banc.meta, banc.edgelist.simple, paper.cols
#'   figures/vignette_neuron_lists/network_<vig>_neurons.csv               (curated)
#'
#' @section Writes:
#'   figures/vignettes/network_<vig>.pdf
#'   figures/vignettes/network_<vig>_edges.csv
#'   figures/vignettes/network_<vig>_neurons.csv                            (rendered copy)
#'
#' @section Paper:
#'   Fig. 3c, 3g, 3h, 3i, 3j, 3k — AN/DN cluster exemplars.
#'   Fig. 4f — head-orienting cluster circuit.
#'   Fig. 4g — postural control cluster circuit.
#'   Fig. 5c — cross-cluster (proprioceptive / threat-response / walking).
#'   Fig. 6h (mb) — mushroom-body → AN/DN example.
#'   Fig. 6h (cx) — central-complex → AN/DN example.
#'   Methods §"Naming AN/DN clusters" + Supplementary Data 9.
#'
#' @section Schema:
#'   The 11 vignette CSVs (fig3c/g/h/i/j/k, fig4f, fig4g, fig5c, fig6h_mb,
#'   fig6h_cx) are the canonical set; older names (`fig4e`, `fig4f` as
#'   used pre-2026-05-06) have been renamed and should not reappear.
#'
#' @section Used by:
#'   R/text/ngl_links.R reads from `figures/vignette_neuron_lists/` to
#'   build the Neuroglancer links that appear in the figure legends.
#'
#' @section Reproduce:
#'   BANC_NCORES=1 Rscript R/figures/panels_vignette_networks.R

source("R/startup/banc-startup.R")
source("R/startup/banc-meta.R")
source("R/startup/banc-edgelist.R")

vignette.input.path <- "figures/vignette_neuron_lists/"
vignette.save.path  <- "figures/vignettes/"
dir.create(vignette.save.path, showWarnings = FALSE, recursive = TRUE)

curated_csvs <- list.files(vignette.input.path,
                           pattern = "^network_.*_neurons\\.csv$",
                           full.names = TRUE)
if (length(curated_csvs) == 0) {
  stop("No curated vignette CSVs found in ", vignette.input.path)
}
message(sprintf("Found %d curated vignette files", length(curated_csvs)))

# `majority_vote()` lives in R/startup/banc-functions.R (hoisted 2026-05-21).

`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a

build_vignette_from_curated <- function(curated, vig_name, edgelist) {
  if (nrow(curated) == 0) return(NULL)
  curated <- curated %>%
    dplyr::filter(!is.na(root_id), !is.na(display_name)) %>%
    dplyr::mutate(root_id = as.character(root_id))

  id_map <- stats::setNames(as.character(curated$display_name),
                            as.character(curated$root_id))
  message(sprintf("  %d neurons -> %d display-name groups",
                  length(id_map), length(unique(unname(id_map)))))

  # Per-display-name aggregate metadata. Pull super_class / super_cluster
  # from the curated row directly (they're authoritative); for nodes that
  # collapse multiple cell_types, take majority vote.
  node_meta <- curated %>%
    dplyr::group_by(display_name) %>%
    dplyr::summarise(
      super_class   = majority_vote(super_class),
      super_cluster = majority_vote(super_cluster),
      n_neurons     = dplyr::n(),
      cell_types    = paste(unique(stats::na.omit(cell_type)), collapse = ", "),
      .groups = "drop"
    )

  # Edges between display-name groups. Self-loops dropped, count >= 3 floor.
  vig_edges <- edgelist %>%
    dplyr::filter(pre %in% names(id_map), post %in% names(id_map)) %>%
    dplyr::mutate(from_name = unname(id_map[as.character(pre)]),
                  to_name   = unname(id_map[as.character(post)])) %>%
    dplyr::filter(from_name != to_name) %>%
    dplyr::group_by(from_name, to_name) %>%
    dplyr::summarise(
      count       = sum(count, na.rm = TRUE),
      pre_top_nt  = majority_vote(pre_neurotransmitter),
      .groups = "drop"
    ) %>%
    dplyr::filter(count >= 3)
  message(sprintf("  %d aggregated edges (count >= 3)", nrow(vig_edges)))
  if (nrow(vig_edges) == 0) return(NULL)

  # Restrict node table to those that actually have an edge in the figure.
  active_nodes <- unique(c(vig_edges$from_name, vig_edges$to_name))
  nodes <- node_meta %>%
    dplyr::filter(display_name %in% active_nodes) %>%
    dplyr::transmute(
      name = display_name,
      super_class, super_cluster, n_neurons,
      label = ifelse(n_neurons > 1,
                     sprintf("%s (n=%d)", display_name, n_neurons),
                     display_name),
      node_colour = vapply(super_cluster, function(sc) {
        if (is.na(sc)) return("#B3B3B3")
        col <- paper.cols[sc]
        if (is.na(col)) return("#B3B3B3")
        col
      }, character(1))
    )

  edges_df <- vig_edges %>% dplyr::rename(from = from_name, to = to_name)

  nt_cols <- c(acetylcholine = "#EB5D25", glutamate = "#00A67D", gaba = "#2A68AE",
               serotonin = "#D5A848", dopamine = "#B87969", octopamine = "#725C98",
               tyramine = "#B56FCC", histamine = "#2a5f75")

  g <- tidygraph::tbl_graph(nodes = nodes,
                            edges = edges_df %>% dplyr::select(from, to, count, pre_top_nt),
                            directed = TRUE, node_key = "name")

  p <- ggraph::ggraph(g, layout = "sugiyama") +
    ggraph::geom_edge_bend(
      ggplot2::aes(width = log10(count), colour = pre_top_nt, label = count),
      alpha = 0.7,
      label_size = 2, label_dodge = grid::unit(2, "mm"),
      arrow = grid::arrow(length = grid::unit(2, "mm"), type = "closed"),
      start_cap = ggraph::circle(5, "mm"), end_cap = ggraph::circle(5, "mm"),
      lineend = "round", strength = 0.3) +
    ggraph::scale_edge_width(range = c(0.3, 2.5), name = "log10(synapses)") +
    ggraph::scale_edge_colour_manual(values = nt_cols, na.value = "#4D4D4D",
                                     name = "neurotransmitter") +
    ggraph::geom_node_point(ggplot2::aes(colour = node_colour), size = 5) +
    ggplot2::scale_colour_identity() +
    ggraph::geom_node_text(ggplot2::aes(label = label), repel = TRUE,
                           size = 2.2, max.overlaps = 30, lineheight = 0.85) +
    ggraph::theme_graph(base_family = "") +
    ggplot2::labs(title = vig_name)

  list(plot = p, edges = edges_df, nodes = nodes)
}

for (csv_file in curated_csvs) {
  vig_name <- gsub("^network_|_neurons\\.csv$", "", basename(csv_file))
  message(sprintf("\n=== Building vignette: %s ===", vig_name))

  curated <- readr::read_csv(csv_file,
                             col_types = readr::cols(
                               .default = readr::col_character()
                             ))

  result <- tryCatch(
    build_vignette_from_curated(curated, vig_name, banc.edgelist.simple),
    error = function(e) {
      message(sprintf("  ERROR: %s", conditionMessage(e))); NULL
    }
  )
  if (is.null(result)) next

  fname <- vig_name   # already snake-cased in the curated filename

  ggplot2::ggsave(plot = result$plot,
                  filename = file.path(vignette.save.path, paste0("network_", fname, ".pdf")),
                  width = 12, height = 10)
  readr::write_csv(result$edges,
                   file.path(vignette.save.path, paste0("network_", fname, "_edges.csv")))
  # Pass the curated neuron table through (drop the helper
  # cell_representative_point_id column for the published copy).
  out_neurons <- curated
  if ("cell_representative_point_id" %in% colnames(out_neurons)) {
    out_neurons <- out_neurons %>% dplyr::select(-cell_representative_point_id)
  }
  readr::write_csv(out_neurons,
                   file.path(vignette.save.path, paste0("network_", fname, "_neurons.csv")))
  message(sprintf("  Saved %d nodes, %d edges, %d neurons (passed through)",
                  nrow(result$nodes), nrow(result$edges), nrow(out_neurons)))
}
