###################
###################
### BETWEENESS ###
###################
###################

###############
### STARTUP ###
###############

# load
source("R/startup/banc-startup.R")
source("R/startup/franken-meta.R")
source("R/startup/banc-meta.R")
source("R/startup/banc-edgelist.R")
banc.version <- NULL
source("R/startup/banc-meta.R")
banc.meta$root_id<-banc.meta$root_626

# paths
banc.fig2.path <- "figures/figure_2/links/"
banc.fig2.supp.path <- "figures/figure_2/links/supplement"
banc.fig2.anat.path <- "figures/figure_2/links/neuroanatomy"
banc.fig2.extra.path <- "figures/figure_2/links/extra"

# data
layer.data <- read_csv("data/feedforward/layers_banc_626.csv", col_types = banc.col.types)
betweeness.data <- read_csv("data/betweeness/betweenness_afferent_to_efferent_filtered.csv", col_types = banc.col.types)
betweeness.data2 <- read_csv("data/betweeness/betweenness_all_to_all_filtered.csv", col_types = banc.col.types)
betweeness.data <- banc.meta %>%
  dplyr::left_join(betweeness.data %>%
                     dplyr::mutate(betweenness=as.numeric(betweenness),
                                   betweenness = ifelse(betweenness>quantile(betweenness,0.99),NA,betweenness)) %>%
                     dplyr::distinct(root_626,betweenness), by = "root_626") %>%
  dplyr::left_join(layer.data %>%
                     dplyr::rename(root_626= `Root ID`,
                                   fwd_layer=`FWD layer`,
                                   bwd_layer=`BWD layer`) %>%
                     dplyr::mutate(fwd_layer=as.numeric(fwd_layer),
                                   bwd_layer=as.numeric(bwd_layer)),
                   by = "root_626") %>%
  dplyr::left_join(betweeness.data2 %>%
                     dplyr::mutate(betw_all=as.numeric(betweenness)) %>%
                     dplyr::distinct(root_626,betw_all),by="root_626")

bt <- betweeness.data %>%
  dplyr::mutate(class = dplyr::case_when(
    grepl("sensory",super_class) ~ super_class,
    grepl("motor",super_class) ~ super_class,
    grepl("visceral_circulatory",super_class) ~ super_class,
    #!is.na(cell_sub_class) ~ cell_sub_class,
    !is.na(cell_class) ~ cell_class,
    TRUE ~ super_class
  )) %>%
  dplyr::filter(!is.na(super_class),
                !is.na(betweenness),
                proofread == "TRUE",
                !super_class %in% c("glia","","trachea","not_a_neuron","motor","visceral_circulatory", "sensory",
                                    "ascending_visceral_circulatory","sensory_ascending","sensory_descending")) %>%
  dplyr::transmute(
    root_id     = dplyr::coalesce(.data$root_626, .data$root_id),
    super_class,
    cell_class,
    cell_sub_class,
    class,
    neurotransmitter_predicted,
    cell_type,
    cns_network,
    fwd_layer,
    betw_all,
    betw = as.numeric(betweenness)
  ) %>%
  dplyr::filter(!is.na(betw), !is.na(super_class)) %>%
  dplyr::ungroup()

#################################
#### BETWEENESS VIOLIN PLOTS ####
#################################
min_n <- 10
eps <- 0.1

bt1 <- bt %>%
  dplyr::filter(!is.na(class)) %>%
  dplyr::group_by(class) %>% dplyr::mutate(n = dplyr::n()) %>% dplyr::ungroup() %>%
  dplyr::filter(n >= min_n) %>%
  dplyr::mutate(
    class    = forcats::fct_reorder(class, betw, .fun = median, .desc = TRUE),
    betw_eps = pmax(betw, eps)
  )

# per-class totals
lab1 <- bt1 %>%
  dplyr::group_by(class) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop")
p_violin <- ggplot2::ggplot(bt1, ggplot2::aes(x = class, y = betw_eps, fill = super_class)) +
  ggplot2::geom_violin(scale = "width", colour = NA, alpha = 0.9) +
  ggplot2::geom_boxplot(width = 0.12, outlier.shape = NA, fill = "white", colour = "grey10") +
  ggplot2::scale_fill_manual(values = paper.cols, drop = FALSE) +
  ggplot2::scale_y_continuous(
    trans  = "log10",
    breaks = scales::log_breaks(10),
    labels = scales::label_number(),
    expand = ggplot2::expansion(mult = c(0, 0.10))   # headroom
  ) +
  ggplot2::scale_x_discrete(labels = ~ gsub("_", " ", .x)) +
  ggplot2::annotation_logticks(sides = "l") +
  ggplot2::geom_text(
    data = lab1,
    ggplot2::aes(x = class, y = 11000, label = scales::comma(n)),
    inherit.aes = FALSE, angle = 90, vjust = 1.1, size = 3.2
  ) +
  ggplot2::coord_cartesian(clip = "off") +
  ggplot2::labs(
    x = NULL,
    y = paste0("betweenness (log10, ε=", signif(eps, 2), " added to zeros)"),
    fill = "super_class"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 35, hjust = 1, vjust = 1),
    legend.position = "bottom"
  ) +
  ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, byrow = TRUE))

# Plot
plot(p_violin)
ggplot2::ggsave(file.path(banc.fig2.supp.path, "betweenness_violin_by_class.pdf"),
                p_violin, width = 20, height = 5, dpi = 300)

# ---- Pairwise Wilcoxon tests for class-level betweenness ----
# Pool ascending/descending neurons by super_class and compare against individual classes
bt1_test <- bt1 %>%
  dplyr::mutate(
    test_group = dplyr::case_when(
      super_class %in% c("ascending", "descending") ~ as.character(super_class),
      TRUE ~ as.character(class)
    )
  )

test_levels <- unique(bt1_test$test_group)
an_dn_groups <- intersect(c("ascending", "descending"), test_levels)
other_groups <- setdiff(test_levels, an_dn_groups)

# Build comparisons: each other class vs ascending, and each other class vs descending
comparisons_class <- unlist(lapply(an_dn_groups, function(ad) {
  lapply(other_groups, function(oc) c(ad, oc))
}), recursive = FALSE)

res_pairwise_class <- write_pairwise_wilcox(
  data = bt1_test,
  value_col = betw_eps,
  group_col = test_group,
  comparisons = comparisons_class,
  out_path = file.path(banc.fig2.supp.path, "betweenness_violin_by_class_pairwise_wilcox_summary.txt"),
  adjust_method = "holm",
  alpha = 0.05
)

super_order <- bt %>%
  dplyr::group_by(super_class) %>%
  dplyr::summarise(med = mean(betw, na.rm = TRUE), .groups = "drop") %>%
  dplyr::arrange(dplyr::desc(med)) %>% dplyr::pull(super_class)

bt2 <- bt %>%
  dplyr::mutate(
    super_class = forcats::fct_relevel(super_class, super_order),
    super_class = factor(super_class)
  ) %>%
  dplyr::mutate(betw = pmax(betw, eps))

lab2 <- bt2 %>%
  dplyr::group_by(super_class) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop")

# make a mostly-grey palette, highlight ascending/descending
grey_col <- "grey70"
fill_cols2 <- setNames(rep(grey_col, length(levels(bt2$super_class))), levels(bt2$super_class))
hl <- intersect(c("ascending", "descending"), names(paper.cols))
fill_cols2[hl] <- paper.cols[hl]

# Define comparisons for betweenness plot
comparisons_betw <- list(
  c("ascending", "descending"),
  c("ascending", "ventral_nerve_cord_intrinsic"),
  c("descending", "ventral_nerve_cord_intrinsic")
)

p_violin2 <- ggplot2::ggplot(bt2, ggplot2::aes(x = super_class, y = betw, fill = super_class)) +
  ggplot2::geom_violin(scale = "width", colour = NA, alpha = 0.9) +
  ggplot2::geom_boxplot(width = 0.12, outlier.shape = NA, fill = "white", colour = "grey10") +
  ggplot2::scale_fill_manual(values = fill_cols2, drop = FALSE) +
  ggplot2::scale_y_continuous(
    trans  = "log10",
    breaks = scales::log_breaks(10),
    labels = scales::label_number(),
    expand = ggplot2::expansion(mult = c(0.05, 0.15))
  ) +
  ggplot2::geom_text(
    data = lab2,
    ggplot2::aes(x = super_class, y = 11000, label = scales::comma(n)),
    inherit.aes = FALSE, angle = 0, vjust = 0.5, size = 4
  ) +
  ggpubr::stat_compare_means(
    comparisons = comparisons_betw,
    method = "wilcox.test",
    p.adjust.method = "holm",
    label = "p.signif",
    size = 5,
    tip.length = 0.02,
    bracket.size = 0.4
  ) +
  ggplot2::scale_x_discrete(labels = ~ gsub("_", " ", .x)) +
  ggplot2::annotation_logticks(sides = "l") +
  ggplot2::labs(
    x = NULL,
    y = paste0("betweenness (log10, ε=", signif(eps, 2), " added to zeros)"),
    fill = "super_class"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 35, hjust = 1, vjust = 1),
    legend.position = "none"
  ) +
  ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, byrow = TRUE))

plot(p_violin2)
ggplot2::ggsave(file.path(banc.fig2.path, "betweenness_violin_by_superclass.pdf"),
                p_violin2, width = 8, height = 4, dpi = 300)

# Generate detailed statistics file
res_pairwise_betw <- write_pairwise_wilcox(
  data = bt2,
  value_col = betw,
  group_col = super_class,
  comparisons = comparisons_betw,
  out_path = file.path(banc.fig2.path, "betweenness_pairwise_wilcox_summary.txt"),
  adjust_method = "holm",
  alpha = 0.05
)

# Append Dunn post-hoc tests to the same summary file
res_dunn_betw <- write_dunn_posthoc(
  data = bt2,
  value_col = betw,
  group_col = super_class,
  highlights = c("ascending", "descending", "ventral_nerve_cord_intrinsic"),
  group_labels = c(ascending = "ANs", descending = "DNs"),
  adjust_method = "holm",
  alpha = 0.05,
  append_to = file.path(banc.fig2.path, "betweenness_pairwise_wilcox_summary.txt")
)


# ------- VIOLIN (betw_all) -------
super_order <- bt %>%
  dplyr::group_by(super_class) %>%
  dplyr::summarise(med = mean(betw_all, na.rm = TRUE), .groups = "drop") %>%
  dplyr::arrange(dplyr::desc(med)) %>% dplyr::pull(super_class)

bt.all <- bt %>%
  dplyr::mutate(
    super_class = forcats::fct_relevel(super_class, super_order),
    super_class = factor(super_class)
  ) %>%
  dplyr::mutate(betw_all = pmax(betw_all, eps))

lab.all <- bt.all %>%
  dplyr::group_by(super_class) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop")

# palette for this plot too (mostly grey, highlight ascending/descending)
fill_cols_all <- setNames(rep(grey_col, length(levels(bt.all$super_class))), levels(bt.all$super_class))
hl_all <- intersect(c("ascending", "descending"), names(paper.cols))
fill_cols_all[hl_all] <- paper.cols[hl_all]

# Define comparisons for betw_all plot
comparisons_betw_all <- list(
  c("ascending", "descending"),
  c("ascending", "visual_centrifugal"),
  c("ascending", "visual_projection"),
  c("descending", "visual_centrifugal"),
  c("descending", "visual_projection")
)

p_violin_all <- ggplot2::ggplot(bt.all, ggplot2::aes(x = super_class, y = betw_all, fill = super_class)) +
  ggplot2::geom_violin(scale = "width", colour = NA, alpha = 0.9) +
  ggplot2::geom_boxplot(width = 0.12, outlier.shape = NA, fill = "white", colour = "grey10") +
  ggplot2::scale_fill_manual(values = fill_cols_all, drop = FALSE) +
  ggplot2::scale_y_continuous(
    trans  = "log10",
    breaks = scales::log_breaks(10),
    labels = scales::label_number(),
    expand = ggplot2::expansion(mult = c(0.05, 0.25))
  ) +
  # ggplot2::geom_text(
  #   data = lab.all,
  #   ggplot2::aes(x = super_class, y = 1100000000, label = scales::comma(n)),
  #   inherit.aes = FALSE, angle = 0, vjust = 0.5, size = 4
  # ) +
  ggpubr::stat_compare_means(
    comparisons = comparisons_betw_all,
    method = "wilcox.test",
    p.adjust.method = "holm",
    label = "p.signif",
    size = 5,
    tip.length = 0.02,
    bracket.size = 0.4
  ) +
  ggplot2::scale_x_discrete(labels = ~ gsub("_", " ", .x)) +
  ggplot2::annotation_logticks(sides = "l") +
  ggplot2::labs(
    x = NULL,
    y = paste0("betw_alleenness (log10, ε=", signif(eps, 2), " added to zeros)"),
    fill = "super_class"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 35, hjust = 1, vjust = 1),
    legend.position = "none"
  ) +
  ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, byrow = TRUE))

# Plot and save!
plot(p_violin_all)
ggplot2::ggsave(file.path(banc.fig2.supp.path, "betw_alleenness_violin_by_superclass.pdf"),
                p_violin_all, width = 5, height = 4, dpi = 300)

# Generate detailed statistics file
res_pairwise_betw_all <- write_pairwise_wilcox(
  data = bt.all,
  value_col = betw_all,
  group_col = super_class,
  comparisons = comparisons_betw_all,
  out_path = file.path(banc.fig2.supp.path, "betw_alleenness_pairwise_wilcox_summary.txt"),
  adjust_method = "holm",
  alpha = 0.05
)

# Append Dunn post-hoc tests for all-to-all betweenness
res_dunn_betw_all <- write_dunn_posthoc(
  data = bt.all,
  value_col = betw_all,
  group_col = super_class,
  highlights = c("ascending", "descending", "ventral_nerve_cord_intrinsic"),
  group_labels = c(ascending = "ANs", descending = "DNs"),
  adjust_method = "holm",
  alpha = 0.05,
  append_to = file.path(banc.fig2.supp.path, "betw_alleenness_pairwise_wilcox_summary.txt")
)

# Pairwise test results
cat("Betweenness (afferent→efferent):", res_pairwise_betw$legend, "\n")
cat("All-to-all betweenness:", res_pairwise_betw_all$legend, "\n")

# Some stats
# Pretty p-values
format_p <- function(p) {
  if (is.na(p)) return("NA")
  if (p < 1e-4) return(formatC(p, format = "e", digits = 2))
  scales::number(p, accuracy = 0.0001)
}

# Kruskal–Wallis + Dunn (Holm), focusing on AN/DN vs others
kw_dunn_summary <- function(data, value_col, group_col = super_class,
                            highlights = c("ascending","descending")) {
  
  gsym <- rlang::ensym(group_col)
  vsym <- rlang::ensym(value_col)
  
  df <- data %>%
    dplyr::select(!!gsym, !!vsym) %>%
    dplyr::filter(is.finite(!!vsym)) %>%
    dplyr::mutate(!!gsym := droplevels(as.factor(!!gsym)))
  
  # Kruskal–Wallis
  fml <- stats::as.formula(paste(rlang::as_string(vsym), "~", rlang::as_string(gsym)))
  kw  <- rstatix::kruskal_test(df, formula = fml)
  kw_p <- kw$p
  
  # Medians by group (to check that AN/DN are higher)
  meds <- df %>%
    dplyr::group_by(!!gsym) %>%
    dplyr::summarise(med = stats::median(!!vsym, na.rm = TRUE), .groups = "drop") %>%
    dplyr::rename(group = !!gsym)
  
  # Dunn pairwise with Holm
  dunn <- rstatix::dunn_test(df, formula = fml, p.adjust.method = "holm")
  
  # Keep pairs involving ascending/descending, and where the highlighted class has the higher median
  dunn_hl <- dunn %>%
    dplyr::filter(group1 %in% highlights | group2 %in% highlights) %>%
    dplyr::left_join(meds %>% dplyr::rename(group1 = group, med1 = med), by = "group1") %>%
    dplyr::left_join(meds %>% dplyr::rename(group2 = group, med2 = med), by = "group2") %>%
    dplyr::mutate(
      hl         = ifelse(group1 %in% highlights, group1, group2),
      hl_med     = ifelse(group1 %in% highlights, med1,  med2),
      other_med  = ifelse(group1 %in% highlights, med2,  med1),
      hl_higher  = hl_med > other_med
    ) %>%
    dplyr::filter(hl_higher)
  
  max_p <- if (nrow(dunn_hl)) max(dunn_hl$p.adj, na.rm = TRUE) else NA_real_

  # Also keep the full Dunn table for reference
  dunn_full <- dunn %>%
    dplyr::filter(group1 %in% highlights | group2 %in% highlights) %>%
    dplyr::left_join(meds %>% dplyr::rename(group1 = group, med1 = med), by = "group1") %>%
    dplyr::left_join(meds %>% dplyr::rename(group2 = group, med2 = med), by = "group2") %>%
    dplyr::mutate(
      hl    = ifelse(group1 %in% highlights, group1, group2),
      other = ifelse(group1 %in% highlights, group2, group1)
    )

  # Number of non-highlight groups that should appear
  all_groups <- levels(df[[rlang::as_string(gsym)]])
  other_groups <- setdiff(all_groups, highlights)
  n_expected <- length(highlights) * length(other_groups)

  list(kw_p = kw_p, max_pairwise_p = max_p,
       dunn_table = dunn_hl, dunn_full = dunn_full,
       n_expected = n_expected, n_significant = nrow(dunn_hl),
       other_groups = other_groups, meds = meds)
}

# ---- Run for the two panels (expects bt2 with betw; bt.all with betw_all) ----
res_betw     <- kw_dunn_summary(bt2,    betw,     super_class)
res_betw_all <- kw_dunn_summary(bt.all, betw_all, super_class)

# Helper: format a Dunn table into readable lines
format_dunn_table <- function(res) {
  lines <- character()
  for (i in seq_len(nrow(res$dunn_full))) {
    row <- res$dunn_full[i, ]
    sig <- ifelse(row$p.adj < 0.05, "*", "ns")
    lines <- c(lines, sprintf("  %s vs %s: p.adj = %s [%s], median %s = %.2f, median %s = %.2f",
      row$hl, row$other, format_p(row$p.adj), sig,
      row$group1, row$med1, row$group2, row$med2))
  }
  lines <- c(lines, "",
    sprintf("  Comparisons expected: %d, significant (highlighted > other): %d",
      res$n_expected, res$n_significant),
    sprintf("  Max adjusted p (among significant): %s", format_p(res$max_pairwise_p)))
  lines
}

# Ready-to-paste statements
stmt_betw <- glue(
  "Betweenness varies significantly with super-class (Kruskal\u2013Wallis test, p = {format_p(res_betw$kw_p)}). ",
  "Post-hoc Dunn tests with Holm correction showed that ascending neurons (ANs) and descending neurons (DNs) ",
  "had higher betweenness than all other super classes ",
  "(all adjusted p \u2264 {format_p(res_betw$max_pairwise_p)})."
)

stmt_betw_all <- glue(
  "All-to-all betweenness varies significantly with super-class (Kruskal\u2013Wallis test, p = {format_p(res_betw_all$kw_p)}). ",
  "Post-hoc Dunn tests with Holm correction showed that ANs and DNs ",
  "had higher all-to-all betweenness than all other super classes ",
  "(all adjusted p \u2264 {format_p(res_betw_all$max_pairwise_p)})."
)

cat(stmt_betw, "\n\n", stmt_betw_all, "\n")

# ---- VNC intrinsic vs remaining non-AN/DN super classes (Dunn post-hoc) ----
# Filter to exclude ascending and descending, keep VNC intrinsic and the 4 other classes
bt2_vnc <- bt2 %>%
  dplyr::filter(!super_class %in% c("ascending", "descending")) %>%
  dplyr::mutate(super_class = droplevels(super_class))
res_betw_vnc <- kw_dunn_summary(bt2_vnc, betw, super_class,
                                highlights = c("ventral_nerve_cord_intrinsic"))

stmt_betw_vnc <- glue(
  "Among non-AN/DN super-classes, betweenness also varies significantly (Kruskal\u2013Wallis test, p = {format_p(res_betw_vnc$kw_p)}). ",
  "Post-hoc Dunn tests with Holm correction showed that VNC intrinsic neurons ",
  "had higher betweenness than the remaining four super-classes ",
  "(all adjusted p \u2264 {format_p(res_betw_vnc$max_pairwise_p)})."
)
cat("\n", stmt_betw_vnc, "\n")

# ---- Write all Dunn post-hoc results to .txt ----
dunn_out <- c(
  "Betweenness Dunn post-hoc test summary",
  paste0("Date: ", Sys.Date()),
  "",
  "========================================",
  "1. AN/DN vs all other super classes (afferent-to-efferent betweenness)",
  "========================================",
  "",
  stmt_betw,
  "",
  "Pairwise comparisons (Holm-corrected):",
  format_dunn_table(res_betw),
  "",
  "========================================",
  "2. AN/DN vs all other super classes (all-to-all betweenness)",
  "========================================",
  "",
  stmt_betw_all,
  "",
  "Pairwise comparisons (Holm-corrected):",
  format_dunn_table(res_betw_all),
  "",
  "========================================",
  "3. VNC intrinsic vs remaining non-AN/DN super classes",
  "========================================",
  "",
  stmt_betw_vnc,
  "",
  "Pairwise comparisons (Holm-corrected):",
  format_dunn_table(res_betw_vnc)
)
writeLines(dunn_out, file.path(banc.fig2.path, "betweenness_violin_by_superclass_dunn_posthoc.txt"))
cat("Saved:", file.path(banc.fig2.path, "betweenness_violin_by_superclass_dunn_posthoc.txt"), "\n")

# ------- VIOLIN (fwd_layer) -------
super_order <- betweeness.data %>%
  dplyr::mutate(cns_network = dplyr::case_when(
    grepl("motor",super_class) ~ "motor",
    grepl("visceral_circulatory",super_class) ~ "visceral_circulatory",
    grepl("sensory",super_class) ~ "sensory",
    grepl("optic_lobe_intrinsic",super_class) ~ "optic_lobe_intrinsic",
    TRUE ~ cns_network
  )) %>%
  dplyr::group_by(cns_network) %>%
  dplyr::summarise(med = mean(fwd_layer, na.rm = TRUE), .groups = "drop") %>%
  dplyr::arrange(dplyr::desc(med)) %>% 
  dplyr::pull(cns_network)

bt.all <- betweeness.data %>%
  dplyr::mutate(cns_network = dplyr::case_when(
    grepl("motor",super_class) ~ "motor",
    grepl("visceral_circulatory",super_class) ~ "visceral_circulatory",
    grepl("sensory",super_class) ~ "sensory",
    grepl("optic_lobe_intrinsic",super_class) ~ "optic_lobe_intrinsic",
    TRUE ~ cns_network
  )) %>%
  dplyr::filter(!is.na(cns_network)) %>%
  dplyr::mutate(
    cns_network = forcats::fct_relevel(cns_network, rev(super_order)),
    cns_network = factor(cns_network)
  ) %>%
  dplyr::mutate(fwd_layer = pmax(fwd_layer, eps))

lab.all <- bt.all %>%
  dplyr::group_by(cns_network) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop")

# palette for this plot too (mostly grey, highlight ascending/descending)
fill_cols_all <- setNames(rep(grey_col, length(levels(bt.all$cns_network))), levels(bt.all$cns_network))
hl_all <- intersect(c("sensory", "visceral_circulatory", "motor"), names(paper.cols))
fill_cols_all[hl_all] <- paper.cols[hl_all]

p_violin_all <- ggplot2::ggplot(bt.all, ggplot2::aes(x = cns_network, y = fwd_layer, fill = cns_network)) +
  ggplot2::geom_violin(scale = "width", colour = NA, alpha = 0.9) +
  ggplot2::geom_boxplot(width = 0.12, outlier.shape = NA, fill = "white", colour = "grey10") +
  ggplot2::scale_fill_manual(values = fill_cols_all, drop = FALSE) +
  ggplot2::geom_text(
    data = lab.all,
    ggplot2::aes(x = cns_network, y = 2500, label = scales::comma(n)),
    inherit.aes = FALSE, angle = 0, vjust = 0.5, size = 4
  ) +
  ggplot2::scale_x_discrete(labels = ~ gsub("_", " ", .x)) +
  ggplot2::labs(
    x = NULL,
    y = "forward layering",
    fill = "cns_network"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 35, hjust = 1, vjust = 1),
    legend.position = "none"
  ) +
  ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, byrow = TRUE))

# Plot and save!
plot(p_violin_all)
ggplot2::ggsave(file.path(banc.fig2.supp.path, "fwd_layereenness_violin_by_cns_network.pdf"),
                p_violin_all, width = 8, height = 4, dpi = 300)

#################################
#### BETWEENESS VIOLIN PLOTS ####
#################################
class_order <- bt %>%
  dplyr::group_by(class) %>%
  dplyr::summarise(med = stats::median(fwd_layer, na.rm = TRUE), .groups = "drop") %>%
  dplyr::arrange(med) %>% dplyr::pull(class)

# ensure super_class is a factor so we can pull its levels for mapping
# Relevel class by median layering (descending)
bt1 <- bt %>%
  dplyr::mutate(
    class = forcats::fct_relevel(class, class_order),  # <- order x-axis
    super_class = factor(super_class)                  # for color mapping
  )

# Map colors for only the super_class levels present
p_violin3 <- ggplot2::ggplot(bt1, ggplot2::aes(x = class, y = fwd_layer, fill = super_class)) +
  ggplot2::geom_violin(scale = "width", colour = NA, alpha = 0.9) +
  ggplot2::geom_boxplot(width = 0.12, outlier.shape = NA, fill = "white", colour = "grey10") +
  ggplot2::scale_fill_manual(values = paper.cols, drop = FALSE) +
  ggplot2::labs(x = NULL, y = "layering", fill = "super_class") +
  ggplot2::theme_minimal() +
  ggplot2::scale_x_discrete(labels = ~ gsub("_", " ", .x)) +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 35, hjust = 1, vjust = 1),
    legend.position = "bottom"
  ) +
  ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, byrow = TRUE))

# Save
plot(p_violin3)
ggplot2::ggsave(file.path(banc.fig2.extra.path, "layering_violin_by_class.pdf"),
                p_violin, width = 16, height = 5, dpi = 300)

# Map colors for only the super_class levels present
super_order <- bt %>%
  dplyr::group_by(super_class) %>%
  dplyr::summarise(med = stats::median(fwd_layer, na.rm = TRUE), .groups = "drop") %>%
  dplyr::arrange(med) %>% dplyr::pull(super_class)
bt2 <- bt %>%
  dplyr::mutate(
    super_class = forcats::fct_relevel(super_class, super_order),  # <- order x-axis
    super_class = factor(super_class)                  # for color mapping
  )
p_violin4 <- ggplot2::ggplot(bt2, ggplot2::aes(x = super_class, y = fwd_layer, fill = super_class)) +
  ggplot2::geom_violin(scale = "width", colour = NA, alpha = 0.9) +
  ggplot2::geom_boxplot(width = 0.12, outlier.shape = NA, fill = "white", colour = "grey10") +
  ggplot2::scale_fill_manual(values = paper.cols, drop = FALSE) +
  ggplot2::labs(x = NULL, y = "layering", fill = "super_class") +
  ggplot2::scale_x_discrete(labels = ~ gsub("_", " ", .x)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 35, hjust = 1, vjust = 1),
    legend.position = "bottom"
  ) +
  ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, byrow = TRUE))

# Save
plot(p_violin4)
ggplot2::ggsave(file.path(banc.fig2.extra.path, "layering_violin_by_superclass.pdf"),
                p_violin4, width = 16, height = 5, dpi = 300)

############################
#### BETWEENESS HEATMAP ####
############################

# ---- Base (use only neurons with a defined network) ----
bt_hm_base <- bt %>%
  dplyr::filter(!is.na(cns_network), cns_network != "") %>%
  dplyr::mutate(betw_pct = 100 * dplyr::percent_rank(betw))  # 0..100 global percentile

# ---- Row & column orders by median RAW betweenness ----
row_order <- bt_hm_base %>%
  dplyr::group_by(class) %>%
  dplyr::summarise(med_betw = stats::median(betw, na.rm = TRUE), .groups = "drop") %>%
  dplyr::arrange(dplyr::desc(med_betw)) %>%
  dplyr::pull(class)

col_order <- bt_hm_base %>%
  dplyr::group_by(cns_network) %>%
  dplyr::summarise(med_betw = stats::median(betw, na.rm = TRUE), .groups = "drop") %>%
  dplyr::arrange(dplyr::desc(med_betw)) %>%
  dplyr::pull(cns_network)

# ---- Aggregate heatmap values: mean percentile per (class x network) ----
bt_hm <- bt_hm_base %>%
  dplyr::group_by(class, cns_network) %>%
  dplyr::summarise(mean_pct = mean(betw_pct, na.rm = TRUE), .groups = "drop") %>%
  tidyr::complete(class, cns_network, fill = list(mean_pct = NA_real_)) %>%
  dplyr::mutate(
    class       = factor(class,       levels = row_order),
    cns_network = factor(cns_network, levels = col_order)
  )

# ---- Plot ----
p_heat <- ggplot2::ggplot(bt_hm, ggplot2::aes(x = cns_network, y = class, fill = mean_pct)) +
  ggplot2::geom_tile(color = NA) +
  ggplot2::scale_fill_gradientn(
    colours = c("#1f4e79","#4a90a4","#7ba7bc","#a67c8a","#c4967d","#b22222"),
    limits = c(0, 100), oob = scales::squish, na.value = "grey90"
  ) +
  ggplot2::labs(
    x = "cns_network", y = NULL,
    fill = "mean betweenness\npercentile (0–100)",
    title = "Rows: class (ordered by median betweenness) • Columns: cns_network (ordered by median betweenness)"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1, vjust = 1))

print(p_heat)
ggplot2::ggsave(file.path(banc.fig2.extra.path, "betweenness_heatmap_superclass_by_network.pdf"),
                p_heat, width = 12, height = 12, dpi = 300)

############################
#### BETWEENESS HEATMAP ####
############################

# ---- Base (use only neurons with a defined network) ----
bt_hm_base <- bt %>%
  dplyr::filter(!is.na(cns_network), cns_network != "") %>%
  dplyr::mutate(fwd_layer_pct = 100 * dplyr::percent_rank(fwd_layer))  # 0..100 global percentile

# ---- Row & column orders by median RAW layering ----
row_order <- bt_hm_base %>%
  dplyr::group_by(class) %>%
  dplyr::summarise(med_fwd_layer = stats::median(fwd_layer, na.rm = TRUE), .groups = "drop") %>%
  dplyr::arrange(dplyr::desc(med_fwd_layer)) %>%
  dplyr::pull(class)

col_order <- bt_hm_base %>%
  dplyr::group_by(cns_network) %>%
  dplyr::summarise(med_fwd_layer = stats::median(fwd_layer, na.rm = TRUE), .groups = "drop") %>%
  dplyr::arrange(dplyr::desc(med_fwd_layer)) %>%
  dplyr::pull(cns_network)

# ---- Aggregate heatmap values: mean percentile per (class x network) ----
bt_hm <- bt_hm_base %>%
  dplyr::group_by(class, cns_network) %>%
  dplyr::summarise(mean_pct = mean(fwd_layer_pct, na.rm = TRUE), .groups = "drop") %>%
  tidyr::complete(class, cns_network, fill = list(mean_pct = NA_real_)) %>%
  dplyr::mutate(
    class       = factor(class,       levels = row_order),
    cns_network = factor(cns_network, levels = col_order)
  )

# ---- Plot ----
p_heat <- ggplot2::ggplot(bt_hm, ggplot2::aes(x = cns_network, y = class, fill = mean_pct)) +
  ggplot2::geom_tile(color = NA) +
  ggplot2::scale_fill_gradientn(
    colours = c("#1f4e79","#4a90a4","#7ba7bc","#a67c8a","#c4967d","#b22222"),
    limits = c(0, 100), oob = scales::squish, na.value = "grey90"
  ) +
  ggplot2::labs(
    x = "cns_network", y = NULL,
    fill = "mean layering\npercentile (0–100)",
    title = "Rows: class (ordered by median layering) • Columns: cns_network (ordered by median layering)"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1, vjust = 1))

print(p_heat)
ggplot2::ggsave(file.path(banc.fig2.extra.path, "layering_heatmap_superclass_by_network.pdf"),
                p_heat, width = 12, height = 12, dpi = 300)

###############
#### TOP K ####
###############

# ---- Composition of ALL vs TOP 5% (by super_class) ----
k <- 0.05

# Rank neurons by betweenness (no other subsetting)
bt_rank <- bt %>%
  dplyr::arrange(dplyr::desc(betw)) %>%
  dplyr::mutate(rank = dplyr::row_number(), frac = rank / dplyr::n()) %>%
  dplyr::filter(!is.na(super_class))

# All neurons
all_sc <- bt_rank %>%
  dplyr::count(super_class, name = "n") %>%
  dplyr::mutate(cohort = factor("All neurons", levels = c("All neurons","Top 5% betweenness")))

# Top 5% by betweenness
top5_sc <- bt_rank %>%
  dplyr::filter(frac <= k) %>%
  dplyr::count(super_class, name = "n") %>%
  dplyr::mutate(cohort = factor("Top 5% betweenness", levels = c("All neurons","Top 5% betweenness")))

comp <- dplyr::bind_rows(all_sc, top5_sc)

# Consistent stack order: use frequency in the FULL set
stack_order <- all_sc %>%
  dplyr::arrange(dplyr::desc(n)) %>%
  dplyr::pull(super_class)

comp <- comp %>%
  dplyr::mutate(super_class = forcats::fct_relevel(super_class, stack_order))

# Color mapping keyed to present super_class levels (uses your paper.cols)
fill_vals <- if (!is.null(names(paper.cols))) {
  vals <- paper.cols[levels(comp$super_class)]
  vals[is.na(vals)] <- "#999999"
  stats::setNames(vals, levels(comp$super_class))
} else {
  stats::setNames(paper.cols[seq_along(levels(comp$super_class))], levels(comp$super_class))
}

# Totals above each bar
bar_totals <- comp %>%
  dplyr::group_by(cohort) %>%
  dplyr::summarise(total = sum(n), .groups = "drop")

p_comp <- ggplot2::ggplot(comp, ggplot2::aes(x = cohort, y = n, fill = super_class)) +
  ggplot2::geom_col(position = "fill") +
  ggplot2::scale_y_continuous(labels = scales::percent, expand = ggplot2::expansion(mult = c(0, 0.08))) +
  ggplot2::scale_fill_manual(values = fill_vals, drop = FALSE) +
  ggplot2::labs(x = NULL, y = "composition (%)", fill = "super_class") +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.position = "bottom",
    axis.text.x = ggplot2::element_text(size = 11)
  ) +
  ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, byrow = TRUE)) +
  ggplot2::geom_text(
    data = bar_totals,
    ggplot2::aes(x = cohort, y = 1.02, label = scales::comma(total)),
    inherit.aes = FALSE, vjust = 0, size = 3.5
  ) +
  ggplot2::coord_cartesian(clip = "off")

print(p_comp)
ggplot2::ggsave(file.path(banc.fig2.extra.path, "betweenness_top_five_percent.pdf"),
                p_comp, width = 6, height = 6, dpi = 300)

################################
#### BETWEENESS +  LAYERING ####
################################

# classes to drop (include both "trache" and "trachea", and both spellings of ascending visceral)
drop_sc <- c("glia",
             "trachea",
             "not_a_neuron",
             "sensory_ascending",
             "sensory_descending",
             "ascending_visceral_circulatory")

bt_hex <- betweeness.data %>%
  dplyr::transmute(
    fwd_layer   = as.numeric(fwd_layer),
    betw        = as.numeric(betweenness),
    cluster = as.character(super_cluster),
    flow,
    super_cluster,
    cns_network,
    super_class
  ) %>%
  dplyr::mutate(cluster = dplyr::case_when(
    !is.na(super_cluster) ~ super_cluster,
    !is.na(cns_network) ~ cns_network,
    TRUE ~ flow
  )) %>%
  dplyr::filter(
    !is.na(fwd_layer), fwd_layer > 0,
    !is.na(betw),      betw > 0,
    !is.na(cluster),
    !tolower(super_class) %in% drop_sc
  )

# Order facets by median betweenness (descending)
sc_order <- bt_hex %>%
  dplyr::group_by(cluster) %>%
  dplyr::summarise(med = stats::median(betw), .groups = "drop") %>%
  dplyr::arrange(dplyr::desc(med)) %>%
  dplyr::pull(cluster)

bt_hex <- bt_hex %>%
  dplyr::mutate(cluster = factor(cluster, levels = sc_order))

# Faceted hexbin with per-facet normalized fill (count / max count in that facet)
p_hex_grid <- ggplot2::ggplot(bt_hex, ggplot2::aes(x = fwd_layer, y = betw)) +
  ggplot2::stat_binhex(
    bins = 40,
    ggplot2::aes(fill = ggplot2::after_stat(count / sum(count)))  # per-facet normalization
  ) +
  ggplot2::scale_fill_gradientn(
    colours = c("#f0f4ff", "#9ecae1", "#3182bd", "#08306b"),
    #limits  = c(0, 1),
    name    = "within-facet\nrelative count"
  ) +
  ggplot2::scale_y_continuous(trans = "log10") +
  ggplot2::labs(x = "feed-forward layer (FWD)", y = "betweenness (log10)") +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.position = "right",
    strip.text = ggplot2::element_text(face = "bold")
  ) +
  ggplot2::facet_wrap(~ cluster, ncol = 4)

print(p_hex_grid)

# Optional save
# ggplot2::ggsave(file.path(banc.fig2.extra.path, "betweenness_vs_fwd_hex_grid_perfacet_scale.pdf"),
#                 p_hex_grid, width = 12, height = 8, dpi = 300)


# Chunk log(10)b etweeness into chunks
# See influence between chunks, sensory, efferent
# low betweeness chunks influence higher chunks.

# One point + IQR error bars per cluster (faceted by flow)
# Uses median (point) and 25–75% quantiles (error bars) for robustness.

# Prep summary (reuses your bt_hex)
bt_pt <- bt_hex %>%
  dplyr::mutate(betw_log = log10(betw)) %>%
  dplyr::filter(is.finite(betw_log), !is.na(super_class), !is.na(cluster)) %>%
  dplyr::group_by(super_class, cluster) %>%
  dplyr::summarise(
    n    = dplyr::n(),
    x_med = stats::median(fwd_layer, na.rm = TRUE),
    x_lo  = stats::quantile(fwd_layer, 0.25, na.rm = TRUE),
    x_hi  = stats::quantile(fwd_layer, 0.75, na.rm = TRUE),
    y_med = stats::median(betw_log, na.rm = TRUE),
    y_lo  = stats::quantile(betw_log, 0.25, na.rm = TRUE),
    y_hi  = stats::quantile(betw_log, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::filter(is.finite(x_med), is.finite(y_med), n >= 10) %>%   # drop tiny/singular groups
  dplyr::mutate(cluster = factor(cluster))

# Colors keyed to present clusters
cl_lvls <- levels(bt_pt$cluster)
cl_cols <- stats::setNames(grDevices::hcl.colors(length(cl_lvls), "Dark 3"), cl_lvls)

p_pts <- ggplot2::ggplot(bt_pt, ggplot2::aes(x = x_med, y = y_med, colour = cluster)) +
  # vertical IQR
  ggplot2::geom_errorbar(ggplot2::aes(ymin = y_lo, ymax = y_hi), width = 0) +
  # horizontal IQR (use a segment for maximum compatibility)
  ggplot2::geom_segment(ggplot2::aes(x = x_lo, xend = x_hi, y = y_med, yend = y_med)) +
  # point
  ggplot2::geom_point(size = 2.2) +
  # labels
  ggrepel::geom_text_repel(
    ggplot2::aes(label = cluster),
    size = 3,
    max.overlaps = Inf,
    min.segment.length = 0,
    box.padding = 0.3,
    seed = 42,
    show.legend = FALSE
  ) +
  ggplot2::scale_color_manual(values = cl_cols, guide = "none") +
  ggplot2::labs(
    x = "feed-forward layer (median, IQR)",
    y = "log10(betweenness) (median, IQR)"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(strip.text = ggplot2::element_text(face = "bold")) +
  ggplot2::facet_wrap(~ super_class, ncol = 4, scales = "fixed")

print(p_pts)

# Optional save
# ggplot2::ggsave(file.path(banc.fig2.extra.path, "betweenness_fwd_point_IQR_by_flow.pdf"),
#                 p_pts, width = 12, height = 8, dpi = 300)

############################
## INFLUENCE to effectors ##
############################
ic_banc <- influence_calculator_py(edgelist_simple = banc.edgelist.simple, 
                                   meta = banc.meta)

# Get influence results for BANC
banc.eff.meta <- banc.meta %>%
  dplyr::filter(grepl("motor",super_class)|grepl("visceral_circulatory",super_class))
banc.eff.ids <- unique(banc.eff.meta$root_id)
banc.efferent_influence <- efferent_influence.signed <- data.frame()
body.parts <- na.omit(unique(betweeness.data$body_part_effector))
layers <- sort(unique(round_any(betweeness.data$fwd_layer,100)))
for(lay in layers){
  banc.ids <- betweeness.data %>%
    dplyr::mutate(fwd_layer_round = round_any(fwd_layer,100)) %>%
    dplyr::filter(!grepl("sensory",super_class),
                  fwd_layer_round==lay) %>%
    dplyr::pull(root_id)
  banc.chosen.meta <- betweeness.data %>%
    dplyr::filter(root_id %in% banc.ids)
  try({
    efferent_influence.id <- calculate_influence_py(ic_banc, banc.ids) %>%
      dplyr::filter(id %in% banc.eff.ids) %>%
      dplyr::left_join(banc.eff.meta %>%
                         dplyr::distinct(id=root_id,
                                         target=body_part_effector), by = "id")
    efferent_influence.id$seed <- lay
    efferent_influence.id$influence_norm_original <- efferent_influence.id$`Influence_score_(unsigned)`/length(banc.ids)
    banc.efferent_influence <- rbind(banc.efferent_influence,
                                     efferent_influence.id)
  })
}
banc.efferent_influence <- banc.efferent_influence %>%
  dplyr::mutate(influence_original = `Influence_score_(unsigned)`) 

# Plot influence of efferent neurons onto efferent neurons
inf.metric <- "influence_log"
eff.out.key.plot <- banc_plot_key_features(
  influence.meta = banc.efferent_influence %>%
    dplyr::filter(id %in% banc.eff.ids,
                  !grepl("pre",seed)) %>%
    dplyr::filter(!is.na(seed), 
                  !is.na(target)),
  ###
  inf.metric = inf.metric,
  target.map = names(efferent.target.map),
  width = 14,
  height = 14,
  recalculate = TRUE,
  row.annotation = NULL,
  show.annotation = FALSE,
  save.path = banc.fig2.path,
  seed.map  = FALSE,
  chosen.seeds = NULL,
  chosen.targets = NULL, 
  row.cols = NULL,
  super.class = NULL,
  plot.name = sprintf("fwd_layers_to_efferent_neuron_%s.pdf",inf.metric),
  rev = FALSE,
  row.dend = NULL,
  col.dend = NULL,
  method = "euclidean",
  symmetric = FALSE,
  diagonal = TRUE,
  col.order = as.character(layers)
)

# Get influence results for BANC
banc.eff.meta <- banc.meta %>%
  dplyr::filter(grepl("motor",super_class)|grepl("visceral_circulatory",super_class))
banc.eff.ids <- unique(banc.eff.meta$root_id)
banc.efferent_influence <- efferent_influence.signed <- data.frame()
body.parts <- na.omit(unique(betweeness.data$body_part_effector))
layers <- sort(unique(round_any(log10(1+betweeness.data$betw_all),0.5)))
for(lay in layers){
  banc.ids <- betweeness.data %>%
    dplyr::mutate(betweenness_round = round_any(log10(1+betweeness.data$betw_all),0.5)) %>%
    dplyr::filter(betweenness!=0,
                  !grepl("sensory",super_class),
                  betweenness_round==lay) %>%
    dplyr::pull(root_id)
  banc.chosen.meta <- betweeness.data %>%
    dplyr::filter(root_id %in% banc.ids)
  try({
    efferent_influence.id <- calculate_influence_py(ic_banc, banc.ids) %>%
      dplyr::filter(id %in% banc.eff.ids) %>%
      dplyr::left_join(banc.eff.meta %>%
                         dplyr::distinct(id=root_id,
                                         target=body_part_effector), by = "id")
    efferent_influence.id$seed <- lay
    efferent_influence.id$influence_norm_original <- efferent_influence.id$`Influence_score_(unsigned)`/length(banc.ids)
    banc.efferent_influence <- rbind(banc.efferent_influence,
                                     efferent_influence.id)
  })
}
banc.efferent_influence <- banc.efferent_influence %>%
  dplyr::mutate(influence_original = `Influence_score_(unsigned)`) 

# Plot influence of efferent neurons onto efferent neurons
inf.metric <- "influence_norm_log"
eff.out.key.plot <- banc_plot_key_features(
  influence.meta = banc.efferent_influence %>%
    dplyr::filter(id %in% banc.eff.ids,
                  !grepl("pre",seed)) %>%
    dplyr::filter(!is.na(seed), 
                  !is.na(target)),
  ###
  inf.metric = inf.metric,
  width = 14,
  height = 14,
  recalculate = TRUE,
  row.annotation = NULL,
  show.annotation = FALSE,
  save.path = banc.fig2.path,
  seed.map  = FALSE,
  chosen.seeds = NULL,
  chosen.targets = NULL, 
  row.cols = NULL,
  super.class = NULL,
  plot.name = sprintf("betweeness_to_efferent_neuron_%s.pdf",inf.metric),
  rev = FALSE,
  row.dend = NULL,
  col.dend = NULL,
  method = "euclidean",
  symmetric = FALSE,
  diagonal = TRUE,
  col.order = as.character(layers)
)

###################################
## INFLUENCE to layers to layers ##
###################################

# Get influence results for BANC
banc.banc_influence <- banc_influence.signed <- data.frame()
body.parts <- na.omit(unique(betweeness.data$body_part_effector))
layers <- sort(unique(round_any(betweeness.data$fwd_layer,100)))
for(lay in layers){
  banc.ids <- betweeness.data %>%
    dplyr::mutate(fwd_layer_round = round_any(fwd_layer,100)) %>%
    dplyr::filter(!grepl("sensory",super_class),
                  fwd_layer_round==lay) %>%
    dplyr::pull(root_id)
  banc.chosen.meta <- betweeness.data %>%
    dplyr::filter(root_id %in% banc.ids)
  try({
    banc_influence.id <- calculate_influence_py(ic_banc, banc.ids) %>%
      dplyr::left_join(betweeness.data %>%
                         dplyr::mutate(target=plyr::round_any(fwd_layer,100)) %>%
                         dplyr::distinct(id=root_id,target), 
                       by = "id")
    banc_influence.id$seed <- lay
    banc_influence.id$influence_norm_original <- banc_influence.id$`Influence_score_(unsigned)`/length(banc.ids)
    banc.banc_influence <- rbind(banc.banc_influence,
                                 banc_influence.id)
  })
}
banc.banc_influence <- banc.banc_influence %>%
  dplyr::mutate(influence_original = `Influence_score_(unsigned)`) 

# Plot influence of efferent neurons onto efferent neurons
inf.metric <- "influence_norm_log"
eff.out.key.plot <- banc_plot_key_features(
  influence.meta = banc.banc_influence %>%
    dplyr::filter(!is.na(seed), 
                  !is.na(target)),
  ###
  inf.metric = inf.metric,
  width = 14,
  height = 14,
  recalculate = TRUE,
  row.annotation = NULL,
  show.annotation = FALSE,
  save.path = banc.fig2.path,
  seed.map  = FALSE,
  chosen.seeds = NULL,
  chosen.targets = NULL, 
  row.cols = NULL,
  super.class = NULL,
  plot.name = sprintf("fwd_layers_to_layer_bins_%s.pdf",inf.metric),
  rev = FALSE,
  row.dend = NULL,
  col.dend = NULL,
  method = "euclidean",
  symmetric = FALSE,
  diagonal = FALSE,
  col.order = as.character(layers),
  row.order = as.character(layers)
)

# Plot influence of efferent neurons onto efferent neurons
inf.metric <- "norm"
eff.out.key.plot <- banc_plot_key_features(
  influence.meta = banc.edgelist.simple %>%
    dplyr::left_join(layer.data %>%
                       dplyr::distinct(pre = `Root ID`, 
                                       pre_fwd_layer = as.numeric(`FWD layer`)),
                     by = "pre") %>%
    dplyr::left_join(layer.data %>%
                       dplyr::distinct(post = `Root ID`, 
                                       post_fwd_layer = as.numeric(`FWD layer`)),
                     by = "post") %>%
    dplyr::mutate(target = round_any(post_fwd_layer,100),
                  seed = round_any(pre_fwd_layer,100)) %>%
    dplyr::filter(!is.na(seed), 
                  !is.na(target)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(target) %>%
    dplyr::mutate(total = sum(count)) %>%
    dplyr::group_by(target, seed) %>%
    dplyr::mutate(count = sum(count),
                  norm = count/total) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(seed,target,count,norm),
  ###
  inf.metric = inf.metric,
  width = 14,
  height = 14,
  recalculate = FALSE,
  row.annotation = NULL,
  show.annotation = FALSE,
  save.path = banc.fig2.path,
  seed.map  = FALSE,
  chosen.seeds = NULL,
  chosen.targets = NULL, 
  row.cols = NULL,
  super.class = NULL,
  plot.name = sprintf("direct_fwd_layers_to_layer_bins_%s.pdf",inf.metric),
  rev = FALSE,
  row.dend = NULL,
  col.dend = NULL,
  method = "euclidean",
  symmetric = FALSE,
  diagonal = FALSE,
  col.order = as.character(layers),
  row.order = as.character(layers)
)
