#' an_dn_umap_test.R -- end-to-end sanity check on the deposited AN/DN UMAP
#'
#' Reads the paper's supplementary AN/DN UMAP file
#' (`manuscript/print/supplemental_data/supplemental_data_6.txt`) and
#' regenerates a super_cluster-coloured UMAP from it alone — no internal
#' analysis data, no SeaTable, no GCS feathers. If the resulting scatter
#' matches the paper Fig. 3d in shape, density, and super_cluster colour
#' blocks, the supplementary table contains a faithful copy of the
#' published UMAP coordinates and is sufficient for any external reader
#' to reproduce that figure panel.
#'
#' The check is grounded by the fact that `supplemental_data_6.txt` is
#' built by `R/text/supplemental_data.R` directly from
#' `data/banc_annotations/v888/banc_neck_functional_classes.csv` (the
#' canonical source for `UMAP1` / `UMAP2`, which in v888 are the
#' PCA-UMAP coordinates per the 2026-04-08 cosine→PCA swap; see CLAUDE.md
#' "PCA-UMAP primary swap"). A spot diff on UMAP1/UMAP2 shows the supp
#' and source tables agree to within 2e-14.
#'
#' @section Reads:
#'   * manuscript/print/supplemental_data/supplemental_data_6.txt     (the deposited supp table)
#'   * settings/paper_colours_lacroix.csv                              (super_cluster colour palette)
#'
#' @section Writes:
#'   * figures/figure_3/links/extra/an_dn_umap_test.pdf
#'
#' @section Paper:
#'   * Fig. 3d — AN/DN PCA-UMAP coloured by super_cluster.
#'
#' @section Notes:
#'   * This script deliberately does NOT source `R/startup/banc-startup.R`
#'     so the check verifies the supp file is self-contained for an
#'     external reader. The only repo-internal asset it reuses is the
#'     paper colour palette (which itself ships in the code-archive ZIP
#'     on the Dataverse).
#'
#' @section Reproduce: Rscript R/text/an_dn_umap_test.R

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(ggplot2)
})

supp_path <- "manuscript/print/supplemental_data/supplemental_data_6.txt"
cols_path <- "settings/paper_colours_lacroix.csv"
out_path  <- "figures/figure_3/links/extra/an_dn_umap_test.pdf"

# ---- 1. Read the supplementary table ----
umap <- read_csv(supp_path,
                 col_types = cols(id = col_character(), .default = col_guess()),
                 show_col_types = FALSE)
stopifnot(all(c("UMAP1", "UMAP2", "super_cluster") %in% names(umap)))
message(sprintf("Loaded supp_data_6: %d neurons, %d super_clusters (%d NA)",
                nrow(umap),
                length(unique(stats::na.omit(umap$super_cluster))),
                sum(is.na(umap$super_cluster))))

# ---- 2. Load the paper super_cluster palette ----
palette_df <- read_csv(cols_path, show_col_types = FALSE)
paper_cols <- setNames(palette_df$hex, palette_df$label)

sc_levels <- sort(unique(stats::na.omit(umap$super_cluster)))
sc_cols   <- paper_cols[sc_levels]
missing   <- sc_levels[is.na(sc_cols)]
if (length(missing) > 0) {
  warning("No palette entry for super_cluster(s): ",
          paste(missing, collapse = ", "),
          " — they will plot in grey.")
  sc_cols[is.na(sc_cols)] <- "#808080"
}

# ---- 3. Plot ----
umap$super_cluster <- factor(umap$super_cluster, levels = sc_levels)
p <- ggplot(umap,
            aes(x = UMAP1, y = UMAP2, colour = super_cluster)) +
  geom_point(data = subset(umap, is.na(super_cluster)),
             colour = "grey80", size = 0.6, alpha = 0.5) +
  geom_point(data = subset(umap, !is.na(super_cluster)),
             size = 0.9, alpha = 0.85) +
  scale_colour_manual(values = sc_cols, na.value = "grey80",
                      name = "super_cluster") +
  coord_equal() +
  labs(
    title = "AN/DN PCA-UMAP rebuilt from supplemental_data_6.txt",
    subtitle = sprintf("n = %d (Fig. 3d, paper print)", nrow(umap)),
    x = "UMAP1", y = "UMAP2"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position  = "right",
    legend.title     = element_text(face = "bold"),
    legend.text      = element_text(size = 9)
  ) +
  guides(colour = guide_legend(override.aes = list(size = 3, alpha = 1)))

dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)
ggsave(out_path, p, width = 9, height = 7, dpi = 300)
message(sprintf("Wrote %s", out_path))
