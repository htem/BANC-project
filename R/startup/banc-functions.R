#' BANC analysis helpers
#'
#' Roxygen2-documented helpers shared across figure / text / annotation
#' scripts. Categories:
#'   - Metadata filters (`filter_valid_neurons`, etc.).
#'   - Influence helpers (`query_influence`, `calculate_influence_norms`,
#'     `banc_influence_loop`).
#'   - Stat summaries (`write_kruskal_summary`, `write_dunn_posthoc`,
#'     `write_anova_summary`, `write_diversity_nonparam_summary`,
#'     `format_table_txt`, `fmt_p_value`, `humanise_group`).
#'   - Plot helpers (`banc_plot_violin_by_class`, `convert_to_dark_mode`,
#'     `cap_to_99th`).
#'   - I/O helpers (`banc_load_betweenness`).
#'
#' Naming convention: `<verb>_<object>` for general helpers
#' (`cap_to_99th`, `fmt_p_value`), `banc_<noun>` for things tied to BANC
#' data (`banc_load_betweenness`, `banc_plot_key_features`), leading `.`
#' for private-ish helpers used inside one file. Functions are hoisted
#' here once they have at least two callers or are non-trivial enough to
#' be worth documenting.
#'
#' Sourced unconditionally by `R/startup/banc-startup.R`.

#' Filter metadata to valid neurons
#'
#' Standardised filter for removing non-neuronal entries (glia, trachea, debris,
#' merges, etc.) from BANC metadata. Designed for use in dplyr chains:
#'   banc.meta %>% filter_valid_neurons()
#'
#' @param df data.frame with banc.meta columns (super_class, status, proofread, root_id)
#' @param only_proofread logical; if TRUE (default), keep only proofread or roughly_proofread neurons
#' @param deduplicate logical; if TRUE (default), keep one row per root_id,
#'   preferring the row with the most non-NA metadata columns
#' @return filtered (and optionally deduplicated) data.frame
filter_valid_neurons <- function(df,
                                  only_proofread = TRUE,
                                  deduplicate = TRUE) {
  # Exclude non-neuronal super_classes
  if ("super_class" %in% colnames(df)) {
    df <- df %>%
      dplyr::filter(!grepl("glia|trachea|not_a_neuron|debris",
                           super_class, ignore.case = TRUE))
  }
  # Exclude non-neuronal status values
  if ("status" %in% colnames(df)) {
    df <- df %>%
      dplyr::filter(!grepl("GLIA|TRACHEA|NOT_A_NEURON|DEBRIS|DELETE",
                           status))
  }
  # Proofread filter
  if (only_proofread) {
    if ("proofread" %in% colnames(df) && "roughly_proofread" %in% colnames(df)) {
      df <- df %>%
        dplyr::filter(as.logical(proofread) %in% TRUE |
                      as.logical(roughly_proofread) %in% TRUE)
    } else if ("proofread" %in% colnames(df)) {
      df <- df %>%
        dplyr::filter(as.logical(proofread) %in% TRUE)
    }
  }
  # Deduplicate by root_id, keeping the row with the most non-NA values
  if (deduplicate) {
    id_col <- if ("root_id" %in% colnames(df)) "root_id"
              else if ("id" %in% colnames(df)) "id"
              else NULL
    if (!is.null(id_col)) {
      df <- df %>%
        dplyr::mutate(.n_nonna = rowSums(!is.na(dplyr::pick(dplyr::everything())))) %>%
        dplyr::arrange(dplyr::desc(.n_nonna)) %>%
        dplyr::distinct(!!rlang::sym(id_col), .keep_all = TRUE) %>%
        dplyr::select(-.n_nonna)
    }
  }
  df
}

# ---------------------------------------------------------------------------
# banc_influence_loop — shared PSOCK-parallel influence computation helper
# ---------------------------------------------------------------------------
# Replaces the per-script for(ct in cts){calculate_influence_py(...)} pattern.
#
# Arguments:
#   cts          Character vector of seed identifiers (cell types, seed_02
#                values, or raw root_ids when seed_column = NULL).
#   seed_column  Column in meta_df to look up root_ids from cts values.
#                NULL means each element of cts IS a root_id (single-neuron mode,
#                e.g. panel_influence_validation.R).
#   level_name   Value for the `level` column in the output.
#   target_ids   Character vector of target root_ids to keep in results.
#   ic           Existing influence calculator for the sequential path.
#                Ignored in parallel mode (each worker builds its own).
#                If NULL in sequential mode, one is built and cached as ic_banc.
#   meta_df      Data frame with at least root_id + seed_column. Default:
#                banc.meta from the global env.
#   elist_df     Edgelist data frame (count > 0 pre-filtered). Default:
#                banc.edgelist.simple %>% filter(count > 0) from global env.
#   ncores       Integer. NULL = auto (min(4, detectCores()-1), but sequential
#                if < 50 tasks). 1L = force sequential. Respects BANC_NCORES
#                env var: BANC_NCORES=1 forces sequential everywhere.
#
# Returns: data.frame with id, seed, level, influence_original,
#          influence_norm_original, Influence_score_(unsigned), etc.
#
# Kill-switch: BANC_NCORES=1 in shell, or ncores=1L per-call.
# ---------------------------------------------------------------------------
banc_influence_loop <- function(cts, seed_column, level_name, target_ids,
                                ic = NULL,
                                meta_df = NULL,
                                elist_df = NULL,
                                ncores = NULL) {
  if (is.null(meta_df))  meta_df  <- as.data.frame(get("banc.meta", envir = .GlobalEnv))
  if (is.null(elist_df)) elist_df <- get("banc.edgelist.simple", envir = .GlobalEnv) %>%
                                       dplyr::filter(count > 0)
  n <- length(cts)
  if (n == 0) return(data.frame())

  # Resolve ncores — BANC_NCORES env var takes precedence
  env_nc <- Sys.getenv("BANC_NCORES", unset = NA)
  if (!is.na(env_nc) && nzchar(env_nc)) {
    ncores <- suppressWarnings(as.integer(env_nc))
  }
  if (is.null(ncores)) {
    ncores <- max(1L, min(4L, parallel::detectCores() - 1L))
    if (n < 50) ncores <- 1L
  }
  ncores <- as.integer(max(1L, ncores))

  # Helper: look up seed root_ids for one ct value
  .get_seed_ids <- function(ct, meta, seed_col) {
    if (is.null(seed_col)) return(as.character(ct))
    unique(meta$root_id[meta[[seed_col]] == ct & !is.na(meta[[seed_col]])])
  }

  # --- Sequential path ---
  if (ncores <= 1L) {
    message(sprintf("[%s] Computing influence sequentially (%d tasks)...",
                    level_name, n))
    if (is.null(ic)) {
      if (exists("ic_banc", envir = .GlobalEnv)) {
        ic <- get("ic_banc", envir = .GlobalEnv)
      } else {
        message("  Building influence calculator...")
        ic <- influence_calculator_py(edgelist_simple = elist_df,
                                      meta = meta_df, count_thresh = 5)
        assign("ic_banc", ic, envir = .GlobalEnv)
      }
    }
    out <- vector("list", n)
    pb <- progress::progress_bar$new(
      format = paste0(level_name, " [:bar] :current/:total (:percent) eta: :eta"),
      total = n, clear = FALSE, width = 70
    )
    for (i in seq_along(cts)) {
      ct <- cts[i]
      tryCatch({
        ids <- .get_seed_ids(ct, meta_df, seed_column)
        if (length(ids) == 0) { pb$tick(); next }
        res <- calculate_influence_py(ic, ids) %>%
          dplyr::filter(id %in% target_ids)
        res$seed <- ct
        res$level <- level_name
        res$influence_norm_original <-
          res$`Influence_score_(unsigned)` / length(ids)
        out[[i]] <- res
      }, error = function(e) {
        message(sprintf("  Warning: %s/%s failed: %s", level_name, ct, e$message))
      })
      pb$tick()
    }
    result <- as.data.frame(data.table::rbindlist(out, fill = TRUE))
    if (nrow(result) > 0) result$influence_original <- result$`Influence_score_(unsigned)`
    message(sprintf("[%s] Done — %d rows.", level_name, nrow(result)))
    return(result)
  }

  # --- Parallel path: PSOCK cluster ---
  message(sprintf("[%s] Computing influence in parallel (%d tasks, %d workers)...",
                  level_name, n, ncores))
  message("  Workers building PETSc calculators — expect a few minutes startup.")

  chunks <- parallel::splitIndices(n, ncores)
  cts_chunks <- lapply(chunks, function(idx) cts[idx])

  cl <- parallel::makeCluster(ncores)
  on.exit(parallel::stopCluster(cl), add = TRUE)

  results <- parallel::parLapply(cl, cts_chunks,
    function(chunk, seed_col, lvl, tgt_ids, meta_w, elist_w) {
      ic_w <- influencer::influence_calculator_py(
        edgelist_simple = elist_w, meta = meta_w, count_thresh = 5
      )
      out <- vector("list", length(chunk))
      for (j in seq_along(chunk)) {
        ct <- chunk[j]
        tryCatch({
          if (is.null(seed_col)) {
            ids <- as.character(ct)
          } else {
            ids <- unique(meta_w$root_id[meta_w[[seed_col]] == ct &
                                           !is.na(meta_w[[seed_col]])])
          }
          if (length(ids) == 0) next
          res <- influencer::calculate_influence_py(ic_w, ids)
          res <- res[res$id %in% tgt_ids, ]
          res$seed <- ct
          res$level <- lvl
          res$influence_norm_original <-
            res$`Influence_score_(unsigned)` / length(ids)
          out[[j]] <- res
        }, error = function(e) NULL)
      }
      do.call(rbind, out)
    },
    seed_col = seed_column, lvl = level_name, tgt_ids = target_ids,
    meta_w = meta_df, elist_w = elist_df
  )

  result <- as.data.frame(data.table::rbindlist(results, fill = TRUE))
  if (nrow(result) > 0) result$influence_original <- result$`Influence_score_(unsigned)`
  message(sprintf("[%s] Done — %d rows.", level_name, nrow(result)))
  result
}

calculate_influence_norms <- function(influence.df,
                                      const = -24,
                                      quantile = FALSE){
  # Use data.table for fast grouped operations on large datasets
  has_target <- "target" %in% colnames(influence.df)
  inf.threshold <- exp(const)

  dt <- data.table::as.data.table(influence.df)
  if (!has_target) dt[, target := id]
  if (!"influence_original" %in% names(dt)) dt[, influence_original := influence]
  if (!"influence_norm_original" %in% names(dt)) dt[, influence_norm_original := influence_norm]

  # Compute no_seeds from ratio of original to norm_original
  dt[, no_seeds := data.table::fifelse(is.na(influence_original / influence_norm_original),
                           1, influence_original / influence_norm_original)]

  # Aggregate per (target, seed): sum influence, then deduplicate
  dt[, `:=`(
    influence_per_seed_sum = sum(influence_original, na.rm = TRUE)
  ), by = .(target, seed)]
  dt <- unique(dt, by = c("target", "seed"), fromLast = FALSE)

  # Count unique IDs per target group AFTER deduplication
  # (when target==id, this is always 1 — matching old dplyr behavior)
  dt[, no_targets := data.table::uniqueN(id), by = target]

  # Compute normalized values
  dt[, `:=`(
    influence = pmax(influence_per_seed_sum, inf.threshold),
    influence_norm = pmax(influence_per_seed_sum / (no_seeds[1] * no_targets[1]),
                         inf.threshold)
  ), by = .(target, seed)]

  # Log transforms
  dt[, `:=`(
    influence_norm_log = log(influence_norm) - const,
    influence_log = log(influence / no_targets) - const
  )]
  dt[is.na(influence), influence_log := 0]
  dt[, influence_per_seed_sum := NULL]

  # Quantile (per-seed)
  if (!is.null(quantile)) {
    dt[, influence_quantile := signif(stats::quantile(influence_original, quantile, na.rm = TRUE), 4),
       by = seed]
  }

  # Min-max normalization by target and by seed in two passes
  dt[, `:=`(
    influence_norm_log_minmax = {
      mn <- min(influence_norm_log, na.rm = TRUE); mx <- max(influence_norm_log, na.rm = TRUE)
      if (mx == mn) rep(0, .N) else (influence_norm_log - mn) / (mx - mn)
    },
    influence_log_minmax = {
      mn <- min(influence_log, na.rm = TRUE); mx <- max(influence_log, na.rm = TRUE)
      if (mx == mn) rep(0, .N) else (influence_log - mn) / (mx - mn)
    }
  ), by = target]

  dt[, `:=`(
    influence_norm_log_minmax_seed = {
      mn <- min(influence_norm_log, na.rm = TRUE); mx <- max(influence_norm_log, na.rm = TRUE)
      if (mx == mn) rep(0, .N) else (influence_norm_log - mn) / (mx - mn)
    },
    influence_log_minmax_seed = {
      mn <- min(influence_log, na.rm = TRUE); mx <- max(influence_log, na.rm = TRUE)
      if (mx == mn) rep(0, .N) else (influence_log - mn) / (mx - mn)
    }
  ), by = seed]

  # Round to 4 significant figures
  for (col in c("influence", "influence_log", "influence_norm", "influence_norm_log",
                "influence_log_minmax", "influence_norm_log_minmax",
                "influence_log_minmax_seed", "influence_norm_log_minmax_seed")) {
    if (col %in% names(dt)) data.table::set(dt, j = col, value = signif(dt[[col]], 4))
  }

  # Clean up
  dt[, c("no_seeds", "no_targets") := NULL]
  if (!has_target) dt[, target := NULL]

  # Convert back to tibble
  tibble::as_tibble(dt)
}

############################################################
## FUNCTION: query_influence
## Purpose:
##   Compute influence scores on-the-fly using the influencer package.
##
##   Requires: banc.edgelist.simple and banc.meta in global env
##   Uses influencer::influence_calculator_py() + calculate_influence_py()
##
## Inputs:
##   - levels    : character vector of seed levels, e.g. c("seed_07")
##   - seeds     : character vector of seed cell types (NULL = all seeds in level)
##   - ids       : character vector of target neuron ids (NULL = all)
##   - include_seeds : logical, whether to include seed neurons (default FALSE)
##   - normalize : logical, whether to apply calculate_influence_norms (default TRUE)
##
## Returns:
##   data.frame with columns: id, is_seed, influence, influence_original,
##   influence_norm_original, seed, level
##   (plus normalized columns if normalize=TRUE)
############################################################
query_influence <- function(levels = NULL,
                            seeds = NULL,
                            ids = NULL,
                            include_seeds = FALSE,
                            normalize = TRUE,
                            ncores = NULL) {

  # Ensure edgelist and meta are available
  if (!exists("banc.edgelist.simple", envir = .GlobalEnv)) {
    tryCatch(source("R/startup/banc-edgelist.R"), error = function(e) {
      stop("banc.edgelist.simple not available and banc-edgelist.R failed: ", e$message)
    })
  }
  if (!exists("banc.meta", envir = .GlobalEnv)) {
    stop("banc.meta must be loaded before calling query_influence()")
  }

  elist <- get("banc.edgelist.simple", envir = .GlobalEnv)
  meta <- get("banc.meta", envir = .GlobalEnv)

  # Resolve ncores
  # BANC_NCORES env var takes precedence over the function arg, so a single
  # `Sys.setenv(BANC_NCORES=1)` (or shell export) forces sequential mode across
  # the whole pipeline without editing every figure script.
  env_ncores <- Sys.getenv("BANC_NCORES", unset = NA)
  if (!is.na(env_ncores) && nzchar(env_ncores)) {
    ncores <- suppressWarnings(as.integer(env_ncores))
  } else if (is.null(ncores)) {
    ncores <- max(1L, parallel::detectCores() - 1L)
  }
  ncores <- as.integer(ncores)
  if (is.na(ncores) || ncores < 1L) ncores <- 1L

  # Determine seed groups
  if (is.null(levels)) levels <- paste0("seed_", sprintf("%02d", 0:14))
  levels <- intersect(levels, colnames(meta))

  # Coerce ids to character once upfront (root IDs must always be character)
  if (!is.null(ids)) ids <- as.character(ids)

  # Build the full list of (level, seed_value) pairs to process
  all_tasks <- list()
  for (lvl in levels) {
    lvl_seeds <- unique(na.omit(meta[[lvl]]))
    lvl_seeds <- lvl_seeds[lvl_seeds != ""]
    if (!is.null(seeds)) lvl_seeds <- intersect(lvl_seeds, seeds)
    if (length(lvl_seeds) == 0) next
    for (sv in lvl_seeds) {
      all_tasks <- append(all_tasks, list(list(level = lvl, seed_value = sv)))
    }
  }

  if (length(all_tasks) == 0) {
    warning("No influence results computed. Check that seed levels/values exist in banc.meta.")
    return(data.frame(id = character(), is_seed = logical(), influence = numeric(),
                      seed = character(), level = character()))
  }

  # --- Helper: process a single (level, seed_value) task ---
  .process_one_task <- function(task, ic, meta_df, include_seeds, ids) {
    lvl <- task$level
    sv  <- task$seed_value
    seed_ids <- unique(meta_df$root_id[meta_df[[lvl]] == sv & !is.na(meta_df[[lvl]])])
    if (length(seed_ids) == 0) return(NULL)

    inf_raw <- calculate_influence_py(ic, seed_ids)
    inf_raw <- inf_raw %>%
      dplyr::mutate(
        seed = sv,
        level = lvl,
        is_seed = id %in% as.character(seed_ids),
        influence = `Influence_score_(unsigned)`,
        influence_original = `Influence_score_(unsigned)`,
        influence_norm_original = `Influence_score_(unsigned)` / length(seed_ids)
      ) %>%
      dplyr::select(id, is_seed, influence, influence_original,
                    influence_norm_original,
                    seed, level,
                    dplyr::any_of(c("n_input_synapses", "n_output_synapses")))

    if (!include_seeds) inf_raw <- inf_raw %>% dplyr::filter(!is_seed)
    if (!is.null(ids)) {
      inf_raw$id <- as.character(inf_raw$id)
      inf_raw <- inf_raw %>% dplyr::filter(id %in% ids)
    }
    inf_raw
  }

  # --- Sequential path (ncores == 1): original behaviour ---
  if (ncores == 1L) {
    message(sprintf("Computing influence sequentially (%d tasks)...", length(all_tasks)))

    # Create or reuse cached influence calculator
    if (!exists("banc.ic", envir = .GlobalEnv)) {
      message("Building influence calculator (first call, will be cached)...")
      banc.ic <- influence_calculator_py(
        edgelist_simple = elist %>% dplyr::filter(count > 0),
        meta = meta,
        count_thresh = 5
      )
      assign("banc.ic", banc.ic, envir = .GlobalEnv)
      message("Influence calculator ready.")
    }
    ic <- get("banc.ic", envir = .GlobalEnv)

    influence.list <- list()
    n_done <- 0L
    for (task in all_tasks) {
      tryCatch({
        res <- .process_one_task(task, ic, meta, include_seeds, ids)
        if (!is.null(res)) influence.list <- append(influence.list, list(res))
      }, error = function(e) {
        message(sprintf("  Warning: influence for seed '%s' in %s failed: %s",
                        task$seed_value, task$level, e$message))
      })
      n_done <- n_done + 1L
      if (n_done %% 20 == 0) message(sprintf("  %d / %d tasks done", n_done, length(all_tasks)))
    }

  } else {
    # --- Parallel path (ncores > 1): PSOCK cluster ---
    message(sprintf("Computing influence in parallel using %d cores (%d tasks)...",
                    ncores, length(all_tasks)))

    # Prepare serializable data for workers (no Python objects)
    elist_filtered <- elist %>% dplyr::filter(count > 0)
    meta_df <- as.data.frame(meta)

    # Split tasks into ncores chunks
    chunk_indices <- parallel::splitIndices(length(all_tasks), ncores)
    task_chunks <- lapply(chunk_indices, function(idx) all_tasks[idx])

    # Worker function: builds its own Python influence calculator, processes its chunk
    .worker_fn <- function(task_chunk, elist_df, meta_worker, include_seeds_w, ids_w) {
      # Each worker creates its own Python influence calculator
      ic_worker <- influencer::influence_calculator_py(
        edgelist_simple = elist_df,
        meta = meta_worker,
        count_thresh = 5
      )

      results <- list()
      for (task in task_chunk) {
        tryCatch({
          lvl <- task$level
          sv  <- task$seed_value
          seed_ids <- unique(meta_worker$root_id[meta_worker[[lvl]] == sv &
                                                   !is.na(meta_worker[[lvl]])])
          if (length(seed_ids) == 0) next

          inf_raw <- influencer::calculate_influence_py(ic_worker, seed_ids)
          inf_raw <- inf_raw %>%
            dplyr::mutate(
              seed = sv,
              level = lvl,
              is_seed = id %in% as.character(seed_ids),
              influence = `Influence_score_(unsigned)`,
              influence_original = `Influence_score_(unsigned)`,
              influence_norm_original = `Influence_score_(unsigned)` / length(seed_ids)
            ) %>%
            dplyr::select(id, is_seed, influence, influence_original,
                          influence_norm_original,
                          seed, level,
                          dplyr::any_of(c("n_input_synapses", "n_output_synapses")))

          if (!include_seeds_w) inf_raw <- inf_raw %>% dplyr::filter(!is_seed)
          if (!is.null(ids_w)) {
            inf_raw$id <- as.character(inf_raw$id)
            inf_raw <- inf_raw %>% dplyr::filter(id %in% ids_w)
          }
          results <- append(results, list(inf_raw))
        }, error = function(e) {
          # silently skip failed seeds in workers
        })
      }
      dplyr::bind_rows(results)
    }

    cl <- parallel::makeCluster(ncores, type = "PSOCK")
    on.exit(parallel::stopCluster(cl), add = TRUE)

    # Load required packages on each worker
    parallel::clusterEvalQ(cl, {
      library(influencer)
      library(dplyr)
      library(reticulate)
    })

    # Run workers
    influence.list <- parallel::parLapply(
      cl, task_chunks,
      fun = .worker_fn,
      elist_df = elist_filtered,
      meta_worker = meta_df,
      include_seeds_w = include_seeds,
      ids_w = ids
    )
  }

  if (length(influence.list) == 0) {
    warning("No influence results computed. Check that seed levels/values exist in banc.meta.")
    return(data.frame(id = character(), is_seed = logical(), influence = numeric(),
                      seed = character(), level = character()))
  }

  influence.df <- dplyr::bind_rows(influence.list)
  rm(influence.list); gc(verbose = FALSE)
  message(sprintf("Computed influence: %d rows (%d seeds across %d levels)",
                  nrow(influence.df), length(unique(influence.df$seed)), length(levels)))

  # Normalize
  if (normalize && nrow(influence.df) > 0) {
    influence.df <- calculate_influence_norms(influence.df)
  }

  influence.df
}


############################################################
## FUNCTION: write_anova_summary (UPDATED)
## Purpose:
##   Run a two-way ANOVA (source × target) on raw replicates
##   and write a concise, conditional figure-legend–style
##   summary + supporting stats to a .txt file.
##
## Inputs:
##   - df_raw  : data.frame/tibble with columns:
##               source (factor/character), target (factor/character), value (numeric)
##               Must contain REPLICATES per (source, target) for classical ANOVA.
##               If no replication, a permutation-based test is used instead.
##   - out_path: file path for the .txt output
##
## Optional:
##   - perms           : permutations for the no-replication test (default 5000)
##   - seed            : RNG seed for permutations (default 123)
##   - alpha           : significance threshold for language (default 0.05)
##   - use_sum_to_zero : if TRUE, set contrasts to c("contr.sum","contr.poly")
##   - force_treatment : if TRUE, set contrasts to c("contr.treatment","contr.poly")
##                        (takes precedence over use_sum_to_zero if both set)
##
## Requires: dplyr, car, effectsize
############################################################

#' Format a data frame / tibble as a plain-text table (no truncation)
#'
#' Uses knitr::kable with "pipe" format to produce a Markdown-style table
#' that shows ALL columns and ALL rows without tibble's truncation.
#'
#' @param x A data.frame or tibble
#' @param digits Number of significant digits for numeric columns (default 4)
#' @return Character vector of lines suitable for writeLines / cat
format_table_txt <- function(x, digits = 4) {
  if (inherits(x, "data.frame")) {
    # Rename p-value display columns to capital-P forms (Nature style).
    # `rename_with` only acts on columns that match the predicate, so tables
    # without these columns are unaffected.
    .rn <- function(nm) {
      # Whole-token replacement: `p` -> `P` only when `p` stands alone
      # (between non-letter boundaries). Handles `p_value`, `p_adj`,
      # `ks_p_adj`, `wilcox_p_adj_disp`, `p`, `p.adj`, `p.adj.signif`,
      # `kw_p_value` etc., but leaves words like `pre`, `post`, `proofread`,
      # `prop` alone.
      gsub("(^|[^A-Za-z])p($|[^A-Za-z])", "\\1P\\2", nm)
    }
    x <- dplyr::rename_with(x, .rn)
  }
  knitr::kable(x, format = "pipe", digits = digits)
}

#' Format a p-value for human-readable statistical prose
#'
#' Produces concise p-value strings: values >= 0.001 shown to 3 significant
#' figures; smaller values use coefficient × 10^exponent notation.
#'
#' @param p Numeric p-value
#' @param digits Significant figures (default 3)
#' @return Character string, e.g. "0.929", "3.83 \u00d7 10^\u221224",
#'   "< 2.2 \u00d7 10^\u221216" (Nature house style: spaces around \u00d7,
#'   Unicode minus \u2212 on negative exponents).
fmt_p_value <- function(p, digits = 3) {
  .minus <- "\u2212"   # Unicode minus (U+2212), per Nature style
  .times <- "\u00d7"   # Multiplication sign (U+00D7)
  vapply(p, function(x) {
    if (is.na(x)) return("NA")
    if (!is.finite(x) || x < 2.2e-16) return(sprintf("< 2.2 %s 10^%s16", .times, .minus))
    if (x >= 0.001) return(as.character(signif(x, digits)))
    expo <- floor(log10(x))
    coef <- signif(x / 10^expo, digits)
    expo_str <- if (expo < 0) sprintf("%s%d", .minus, abs(expo)) else as.character(expo)
    sprintf("%s %s 10^%s", coef, .times, expo_str)
  }, character(1))
}

#' Humanise a group name for statistical prose
#'
#' Replaces underscores with spaces for readability in figure legends.
#'
#' @param x Character vector of group names
#' @return Character vector with underscores replaced by spaces
humanise_group <- function(x) gsub("_", " ", x)

#' Format a single statistical result in the canonical inline form
#'
#' Produces the short "(p = ..., test type, n = ...)" string used across
#' figure legends and .txt sidecars. `extra` lets a caller append extra
#' descriptors (e.g. `df = 16`, `eta^2 = 0.23`) before the closing paren.
#'
#' @param p     Numeric p-value
#' @param test  Test type as it should appear in prose (e.g.
#'              "two-way ANOVA", "Kruskal-Wallis", "Dunn post-hoc")
#' @param n     Total sample size
#' @param extra Optional character vector of additional descriptors
#'              ("df = 16", "η² = 0.23", "Holm-adjusted") inserted between
#'              `test` and `n`
#' @return A single character string, e.g.
#'   "(p = 1.23×10^-5, two-way ANOVA, df = 16, n = 3120)"
fmt_stat_concise <- function(p, test, n, extra = character()) {
  p_str <- fmt_p_value(p)
  p_str <- if (startsWith(p_str, "<")) paste("P", p_str) else paste("P =", p_str)
  parts <- c(p_str, test, extra, paste0("n = ", n))
  paste0("(", paste(parts, collapse = ", "), ")")
}

write_anova_summary <- function(df_raw,
                                out_path,
                                perms = 5000,
                                seed = 123,
                                alpha = 0.05,
                                use_sum_to_zero = FALSE,
                                force_treatment = FALSE) {
  # ---- Package checks ----
  for (pkg in c("dplyr", "car", "effectsize")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Package '", pkg, "' is required but not installed.")
    }
  }
  
  # ---- Validate inputs ----
  if (!all(c("source","target","value") %in% base::names(df_raw))) {
    stop("df_raw must have columns: source, target, value")
  }
  
  # ---- Coerce & basic counts ----
  df_raw <- df_raw |>
    dplyr::mutate(
      source = base::as.factor(source),
      target = base::as.factor(target),
      value  = base::as.numeric(value)
    )
  
  n_obs <- base::nrow(df_raw)
  n_src <- base::nlevels(df_raw$source)
  n_tgt <- base::nlevels(df_raw$target)
  
  # ---- Replication check ----
  cell_counts <- df_raw |>
    dplyr::count(source, target, name = "n")
  has_replication <- base::any(cell_counts$n > 1)
  
  # ---- Helpers ----
  .fmt_p <- function(p) {
    val <- fmt_p_value(p)
    ifelse(startsWith(val, "<"), paste("P", val), paste("P =", val))
  }
  .sig_word <- function(p, alpha) if (base::is.finite(p) && p < alpha) "significant" else "not significant"
  .es_label <- function(eta) {
    if (is.na(eta)) return("—")
    if (eta < 0.01) "negligible"
    else if (eta < 0.06) "small"
    else if (eta < 0.14) "moderate"
    else "large"
  }
  add_section <- function(title, obj = NULL) {
    hdr <- paste0("\n", title, "\n", strrep("-", max(3, nchar(title))), "\n")
    out_lines <<- c(out_lines, hdr)
    if (!is.null(obj)) {
      tbl <- if (inherits(obj, "data.frame")) format_table_txt(obj)
             else utils::capture.output(obj)
      out_lines <<- c(out_lines, tbl)
    }
  }
  
  # ---- Collect output lines ----
  out_lines <- character()
  add_section("Two-way ANOVA (source × target) Summary")
  out_lines <- c(
    out_lines,
    paste0("Date: ", base::format(base::Sys.time(), "%Y-%m-%d %H:%M:%S")),
    paste0("Observations: ", n_obs),
    paste0("Unique sources: ", n_src, " | Unique targets: ", n_tgt),
    paste0("Replication present: ", has_replication)
  )
  
  if (has_replication) {
    # ---------- Large-design fast path ----------
    # car::Anova(type="III") refits the model multiple times. Even a single
    # stats::lm(value ~ source * target) with 525k obs × 7,935 cells can
    # take many hours and exhaust memory. For large designs we therefore
    # compute the ANOVA decomposition analytically via dplyr group-bys —
    # O(n) and trivially memory-bounded. Override via
    # getOption("banc.fast_anova_threshold", 1000L).
    .anova_threshold <- getOption("banc.fast_anova_threshold", 1000L)
    if ((as.numeric(n_src) * as.numeric(n_tgt)) > .anova_threshold) {
      message(sprintf(
        "[write_anova_summary] Large design (%d × %d = %d cells > %d) — analytic SS decomposition fast path",
        n_src, n_tgt, n_src * n_tgt, .anova_threshold))
      add_section(sprintf(
        "Large design (%d source × %d target = %d cells) — analytic SS decomposition fast path. Override via options(banc.fast_anova_threshold = ...).",
        n_src, n_tgt, n_src * n_tgt))

      gm <- mean(df_raw$value, na.rm = TRUE)
      ss_total <- sum((df_raw$value - gm)^2, na.rm = TRUE)
      ss_src <- df_raw |>
        dplyr::group_by(source) |>
        dplyr::summarise(.n = dplyr::n(), .mean = mean(value, na.rm = TRUE),
                         .groups = "drop") |>
        dplyr::summarise(s = sum(.n * (.mean - gm)^2, na.rm = TRUE)) |>
        dplyr::pull(s)
      ss_tgt <- df_raw |>
        dplyr::group_by(target) |>
        dplyr::summarise(.n = dplyr::n(), .mean = mean(value, na.rm = TRUE),
                         .groups = "drop") |>
        dplyr::summarise(s = sum(.n * (.mean - gm)^2, na.rm = TRUE)) |>
        dplyr::pull(s)
      ss_cell <- df_raw |>
        dplyr::group_by(source, target) |>
        dplyr::summarise(.n = dplyr::n(), .mean = mean(value, na.rm = TRUE),
                         .groups = "drop") |>
        dplyr::summarise(s = sum(.n * (.mean - gm)^2, na.rm = TRUE)) |>
        dplyr::pull(s)
      ss_int  <- ss_cell - ss_src - ss_tgt
      ss_res  <- ss_total - ss_cell

      df_src <- as.numeric(n_src) - 1
      df_tgt <- as.numeric(n_tgt) - 1
      df_int <- df_src * df_tgt
      df_res <- as.numeric(n_obs) - as.numeric(n_src) * as.numeric(n_tgt)
      df_res <- max(df_res, 1)
      ms_res <- ss_res / df_res
      f_src <- (ss_src / df_src) / ms_res
      f_tgt <- (ss_tgt / df_tgt) / ms_res
      f_int <- (ss_int / df_int) / ms_res
      p_src <- stats::pf(f_src, df_src, df_res, lower.tail = FALSE)
      p_tgt <- stats::pf(f_tgt, df_tgt, df_res, lower.tail = FALSE)
      p_int <- stats::pf(f_int, df_int, df_res, lower.tail = FALSE)
      eta_src <- ss_src / (ss_src + ss_res)
      eta_tgt <- ss_tgt / (ss_tgt + ss_res)
      eta_int <- ss_int / (ss_int + ss_res)
      anova_tbl <- data.frame(
        Term     = c("source", "target", "source:target", "Residuals"),
        SS       = c(ss_src, ss_tgt, ss_int, ss_res),
        Df       = c(df_src, df_tgt, df_int, df_res),
        F.value  = c(f_src, f_tgt, f_int, NA),
        Pr_F     = c(p_src, p_tgt, p_int, NA),
        eta2_p   = c(eta_src, eta_tgt, eta_int, NA),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      add_section("ANOVA table (analytic, balanced-cell unweighted SS)", anova_tbl)
      legend_text <- paste0(
        "Two-way ANOVA (", n_obs, " obs over ", n_src, " sources × ",
        n_tgt, " targets), source × target interaction ",
        .sig_word(p_int, alpha), " (",
        sprintf("F(%d, %d) = %.2f, ", df_int, df_res, f_int),
        .fmt_p(p_int), "; partial η² = ",
        sprintf("%.2f", eta_int), ")."
      )
      add_section("Concise figure-legend statement", legend_text)
      add_section(
        "Concise stat",
        fmt_stat_concise(
          p     = p_int,
          test  = "two-way ANOVA, source×target interaction (analytic SS)",
          n     = n_obs,
          extra = c(sprintf("F(%d, %d) = %.2f", df_int, df_res, f_int),
                    sprintf("partial η² = %.2f", eta_int))
        )
      )
      base::writeLines(out_lines, out_path)
      message(sprintf("Wrote %s (analytic fast path)", out_path))
      return(invisible(list(anova = anova_tbl, fast_path = TRUE,
                              p_int = p_int, F_int = f_int)))
    }
    # ---------- Classical Type-III ANOVA ----------
    old_contr <- base::options("contrasts")
    changed_contr <- FALSE

    # Choose contrast coding (treatment overrides sum-to-zero if both requested)
    if (isTRUE(force_treatment)) {
      base::options(contrasts = c("contr.treatment","contr.poly"))
      changed_contr <- TRUE
      contrast_label <- "treatment contrasts"
    } else if (isTRUE(use_sum_to_zero)) {
      base::options(contrasts = c("contr.sum","contr.poly"))
      changed_contr <- TRUE
      contrast_label <- "sum-to-zero contrasts"
    } else {
      contrast_label <- "session contrasts"
    }

    fit_lm  <- stats::lm(value ~ source * target, data = df_raw)
    anova_3 <- car::Anova(fit_lm, type = "III")
    es_tbl  <- effectsize::eta_squared(anova_3, partial = TRUE, ci = 0.95) |>
      base::as.data.frame()
    
    add_section("Type-III ANOVA table", anova_3)
    add_section("Partial eta^2 (95% CI)", es_tbl)
    
    # Extract rows
    pick_row <- function(tab, pattern) {
      r <- base::grep(pattern, base::rownames(tab), perl = TRUE)
      if (length(r) != 1L) stop("Could not uniquely find row matching: ", pattern)
      tab[r, , drop = FALSE]
    }
    pick_es <- function(param_regex, es_df) {
      r <- base::grep(param_regex, es_df$Parameter, perl = TRUE)
      if (length(r) != 1L) return(c(eta = NA_real_, lo = NA_real_, hi = NA_real_))
      c(
        eta = es_df[r, grep("Eta2",   names(es_df))][[1]],
        lo  = es_df[r, grep("CI_low", names(es_df))][[1]],
        hi  = es_df[r, grep("CI_high",names(es_df))][[1]]
      )
    }
    
    row_int <- pick_row(anova_3, "source:\\s*target")
    row_src <- pick_row(anova_3, "^source$")
    row_tgt <- pick_row(anova_3, "^target$")
    df_res  <- as.numeric(anova_3["Residuals","Df"])
    
    df1_int <- as.numeric(row_int[,"Df"]); F_int <- as.numeric(row_int[,"F value"]); p_int <- as.numeric(row_int[,"Pr(>F)"])
    df1_src <- as.numeric(row_src[,"Df"]); F_src <- as.numeric(row_src[,"F value"]); p_src <- as.numeric(row_src[,"Pr(>F)"])
    df1_tgt <- as.numeric(row_tgt[,"Df"]); F_tgt <- as.numeric(row_tgt[,"F value"]); p_tgt <- as.numeric(row_tgt[,"Pr(>F)"])
    
    es_int <- pick_es("source:\\s*target", es_tbl)
    es_src <- pick_es("^source$",         es_tbl)
    es_tgt <- pick_es("^target$",         es_tbl)
    
    # Dynamic phrasing
    phr_int_sig <- .sig_word(p_int, alpha)
    phr_src_sig <- .sig_word(p_src, alpha)
    phr_tgt_sig <- .sig_word(p_tgt, alpha)
    
    lab_int <- .es_label(es_int["eta"])
    lab_src <- .es_label(es_src["eta"])
    lab_tgt <- .es_label(es_tgt["eta"])
    
    # Optional note if interaction is largest effect
    largest_term <- c(interaction = es_int["eta"], source = es_src["eta"], target = es_tgt["eta"])
    dominant_note <- if (!any(is.na(largest_term)) && which.max(largest_term) == 1L)
      " The interaction carried the largest effect size among tested terms." else ""
    
    # Legend sentence (conditional, concise)
    legend_text <- sprintf(
      paste0("Two-way ANOVA (Type-III SS; %s) on %d observations ",
             "with %d sources and %d targets found a %s source×target interaction, ",
             "F(%d, %d) = %.2f, %s, partial η² = %.2f [%.2f–%.2f, %s]. ",
             "Source main effect was %s, F(%d, %d) = %.2f, %s, partial η² = %.2f (%s); ",
             "target main effect was %s, F(%d, %d) = %.2f, %s, partial η² = %.2f (%s).%s"),
      contrast_label, n_obs, n_src, n_tgt,
      phr_int_sig,
      df1_int, df_res, F_int, .fmt_p(p_int), es_int["eta"], es_int["lo"], es_int["hi"], lab_int,
      phr_src_sig, df1_src, df_res, F_src, .fmt_p(p_src), es_src["eta"], lab_src,
      phr_tgt_sig, df1_tgt, df_res, F_tgt, .fmt_p(p_tgt), es_tgt["eta"], lab_tgt,
      dominant_note
    )
    
    add_section("Concise figure-legend statement", legend_text)
    add_section(
      "Concise stat",
      fmt_stat_concise(
        p    = p_int,
        test = "two-way ANOVA Type-III, source×target interaction",
        n    = n_obs,
        extra = c(sprintf("F(%d, %d) = %.2f", df1_int, df_res, F_int),
                  sprintf("η² = %.2f", es_int["eta"]))
      )
    )

    # Add additive vs interaction comparison
    fit_add   <- stats::lm(value ~ source + target, data = df_raw)
    delta_tbl <- stats::anova(fit_add, fit_lm)
    add_section("Model comparison: additive vs interaction", delta_tbl)

    # Restore contrasts only if changed
    if (changed_contr) base::options(contrasts = old_contr$contrasts)
    
  } else {
    # ---------- No replication: permutation-based interaction test ----------
    add_section("No replication detected — running permutation-based interaction test")
    
    fit_add   <- stats::lm(value ~ source + target, data = df_raw)
    ss_obs    <- base::sum(stats::residuals(fit_add)^2)
    df_int    <- (base::nlevels(df_raw$source) - 1L) * (base::nlevels(df_raw$target) - 1L)
    
    base::set.seed(seed)
    perm_stats <- base::rep(NA_real_, perms)
    for (b in base::seq_len(perms)) {
      dfp <- df_raw |>
        dplyr::group_by(target) |>
        dplyr::mutate(value = base::sample(value)) |>
        dplyr::ungroup()
      perm_stats[b] <- base::sum(stats::residuals(stats::lm(value ~ source + target, data = dfp))^2)
    }
    p_perm <- (1 + base::sum(perm_stats >= ss_obs)) / (perms + 1)
    
    add_section("Permutation test for interaction (no replication)")
    out_lines <- c(
      out_lines,
      paste0("Observed SS_interaction_like = ", base::format(ss_obs, digits = 5)),
      paste0("Interaction df = ", df_int),
      paste0("Permutations = ", perms, ", p_perm = ", base::format(p_perm, digits = 3, scientific = TRUE))
    )
    
    # Concise statement
    phr_perm <- .sig_word(p_perm, alpha)
    legend_text <- paste0(
      "Two-way structure without replication: a permutation test of nonadditivity (",
      n_obs, " observations; ", n_src, " sources; ", n_tgt, " targets) was ",
      phr_perm, " (p_perm = ", base::format(p_perm, digits = 3, scientific = TRUE), ")."
    )
    add_section("Concise figure-legend statement", legend_text)
    add_section(
      "Concise stat",
      fmt_stat_concise(
        p    = p_perm,
        test = "permutation test of nonadditivity (no replication)",
        n    = n_obs,
        extra = sprintf("perms = %d", perms)
      )
    )
  }
  
  # ---- Write to file ----
  base::dir.create(base::dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  base::writeLines(out_lines, con = out_path)
  
  # ---- Return legend text invisibly ----
  invisible(tail(out_lines, n = 1))
}

############################################################
## FUNCTION: write_ks_summary
## Purpose:
##   Run two-sample Kolmogorov–Smirnov tests comparing each
##   group (type) vs a reference group (default "other"),
##   Holm-adjust p-values, and write a concise, conditional
##   figure-legend–style summary + supporting tables to .txt.
##
## Inputs:
##   - df         : data.frame/tibble with columns:
##                  type (factor/character), prop (numeric)
##   - out_path   : file path for the .txt output
##   - type_col   : name of the type column (default "type")
##   - value_col  : name of the numeric column (default "prop")
##   - ref_type   : name of the reference group (default "other")
##   - adjust_method : p-value adjustment (default "holm")
##   - alpha      : significance threshold for language (default 0.05)
##   - p_floor    : floor for p-value display to avoid zeros (default 1e-300)
##
## Requires: dplyr, forcats
############################################################
write_ks_summary <- function(df,
                             out_path,
                             type_col   = "type",
                             value_col  = "prop",
                             ref_type   = "other",
                             adjust_method = "holm",
                             alpha      = 0.05,
                             p_floor    = 1e-300) {
  # ---- checks ----
  for (pkg in c("dplyr","forcats")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Package '", pkg, "' is required but not installed.")
    }
  }
  if (!all(c(type_col, value_col) %in% names(df))) {
    stop("df must contain columns: '", type_col, "' and '", value_col, "'.")
  }
  
  # ---- prep ----
  df <- df |>
    dplyr::rename(type = !!type_col, value = !!value_col) |>
    dplyr::mutate(
      type  = base::as.character(type),
      value = base::as.numeric(value)
    ) |>
    dplyr::filter(!is.na(type), is.finite(value))
  
  n_total <- nrow(df)
  n_types <- dplyr::n_distinct(df$type)
  
  if (!ref_type %in% df$type) {
    stop("Reference group '", ref_type, "' not found in 'type' column.")
  }
  
  # Reorder types by median(value), ref_type last (purely cosmetic for tables)
  type_order <- df |>
    dplyr::filter(type != ref_type) |>
    dplyr::group_by(type) |>
    dplyr::summarise(median_value = stats::median(value, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(median_value)) |>
    dplyr::pull(type)
  type_order <- c(type_order, ref_type)
  df <- df |>
    dplyr::mutate(type = forcats::fct_relevel(type, type_order))
  
  # Reference vector
  ref_vec <- df |>
    dplyr::filter(type == ref_type) |>
    dplyr::pull(value)
  
  if (length(ref_vec) < 2) {
    stop("Reference group '", ref_type, "' has fewer than 2 observations, KS test not meaningful.")
  }
  
  # KS per type vs ref
  ks_results <- df |>
    dplyr::filter(type != ref_type) |>
    dplyr::group_by(type) |>
    dplyr::summarise(
      n_group     = dplyr::n(),
      median_val  = stats::median(value, na.rm = TRUE),
      ks_D        = suppressWarnings(stats::ks.test(value, ref_vec)$statistic[[1]]),
      p_value     = suppressWarnings(stats::ks.test(value, ref_vec)$p.value),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      p_adj   = stats::p.adjust(p_value, method = adjust_method),
      signif  = dplyr::case_when(
        p_adj <= 1e-4 ~ "****",
        p_adj <= 1e-3 ~ "***",
        p_adj <= 1e-2 ~ "**",
        p_adj <= 5e-2 ~ "*",
        TRUE          ~ "ns"
      ),
      # display helper to avoid zeros from underflow
      p_adj_disp = vapply(p_adj, fmt_p_value, character(1))
    ) |>
    dplyr::arrange(p_adj)
  
  # Summary medians (ordered)
  med_table <- df |>
    dplyr::group_by(type) |>
    dplyr::summarise(
      n = dplyr::n(),
      median_value = stats::median(value, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(median_value))
  
  # Helper: conditional prose
  sig_types <- ks_results |>
    dplyr::filter(p_adj < alpha) |>
    dplyr::pull(type) |> as.character()
  ns_types  <- ks_results |>
    dplyr::filter(p_adj >= alpha) |>
    dplyr::pull(type) |> as.character()
  
  fmt_list <- function(x) {
    if (length(x) == 0) return("none")
    if (length(x) == 1) return(x)
    paste0(paste(x[-length(x)], collapse = ", "), " and ", x[length(x)])
  }
  
  # Compact roll-up lines
  line_sig <- if (length(sig_types)) {
    paste0("Significant (Holm-adjusted α=", alpha, "): ", fmt_list(sig_types), ".")
  } else {
    paste0("No groups differed from '", ref_type, "' at Holm-adjusted α=", alpha, ".")
  }
  line_ns <- if (length(ns_types)) {
    paste0("Not significant: ", fmt_list(ns_types), ".")
  } else {
    NULL
  }
  
  # Compose figure-legend style statement
  # Example: "Two-sample KS tests (Holm-adjusted) comparing each type to 'other' on prop (N=..., K=...) ..."
  legend_text <- paste0(
    "Two-sample Kolmogorov–Smirnov tests (", adjust_method,
    "-adjusted) comparing each group to '", ref_type, "' (N=",
    n_total, " observations; K=", n_types, " groups) on '", value_col, "'. ",
    line_sig, if (!is.null(line_ns)) paste0(" ", line_ns) else ""
  )

  # Canonical concise stat — uses the smallest (most-significant) Holm-adjusted
  # p-value across the K-1 comparisons; "n" is total observations.
  .min_p <- suppressWarnings(min(ks_results$p_adj, na.rm = TRUE))
  if (!is.finite(.min_p)) .min_p <- NA_real_
  concise_stat <- fmt_stat_concise(
    p    = .min_p,
    test = paste0("Kolmogorov–Smirnov vs '", ref_type, "' (",
                  adjust_method, "-adjusted)"),
    n    = n_total,
    extra = c(sprintf("K = %d groups", n_types),
              "min adjusted p across comparisons")
  )
  
  # ---- write to file ----
  out_lines <- c(
    paste0("KS Summary (vs '", ref_type, "')"),
    strrep("-", 60),
    paste0("Date: ", base::format(base::Sys.time(), "%Y-%m-%d %H:%M:%S")),
    paste0("Observations: ", n_total),
    paste0("Groups (types): ", n_types),
    paste0("Reference group: '", ref_type, "'"),
    paste0("Adjustment: ", adjust_method, " | alpha = ", alpha),
    "",
    "Legend-style statement",
    "----------------------",
    legend_text,
    "",
    "Concise stat",
    "------------",
    concise_stat,
    "",
    "Medians by group (descending)",
    "-----------------------------",
    format_table_txt(med_table),
    "",
    "KS results vs reference (sorted by adjusted p-value)",
    "----------------------------------------------------",
    format_table_txt(
      ks_results |>
        dplyr::select(type, n_group, median_val, ks_D, p_value, p_adj, signif, p_adj_disp)
    )
  )

  base::dir.create(base::dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  base::writeLines(out_lines, con = out_path)
  
  invisible(list(
    legend = legend_text,
    medians = med_table,
    results = ks_results
  ))
}

############################################################
## FUNCTION: write_nonparam_summary
## Purpose:
##   Run BOTH Kolmogorov–Smirnov tests (distribution) AND
##   Wilcoxon rank-sum tests (median) comparing each group
##   vs a reference group, with Holm adjustment. Write a
##   concise figure-legend–style summary to .txt.
##
## Inputs:
##   - df         : data.frame/tibble with columns:
##                  type (factor/character), value (numeric)
##   - out_path   : file path for the .txt output
##   - type_col   : name of the type column (default "type")
##   - value_col  : name of the numeric column (default "prop")
##   - ref_type   : name of the reference group (default "other")
##   - adjust_method : p-value adjustment (default "holm")
##   - alpha      : significance threshold for language (default 0.05)
##   - p_floor    : floor for p-value display to avoid zeros (default 1e-300)
##
## Requires: dplyr, forcats
############################################################
write_nonparam_summary <- function(df,
                                   out_path,
                                   type_col   = "type",
                                   value_col  = "prop",
                                   ref_type   = "other",
                                   adjust_method = "holm",
                                   alpha      = 0.05,
                                   p_floor    = 1e-300,
                                   calculate_effect_size = TRUE) {
  # ---- checks ----
  for (pkg in c("dplyr","forcats")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Package '", pkg, "' is required but not installed.")
    }
  }
  if (!all(c(type_col, value_col) %in% names(df))) {
    stop("df must contain columns: '", type_col, "' and '", value_col, "'.")
  }

  # ---- prep ----
  df <- df |>
    dplyr::rename(type = !!type_col, value = !!value_col) |>
    dplyr::mutate(
      type  = base::as.character(type),
      value = base::as.numeric(value)
    ) |>
    dplyr::filter(!is.na(type), is.finite(value))

  n_total <- nrow(df)
  n_types <- dplyr::n_distinct(df$type)

  if (!ref_type %in% df$type) {
    stop("Reference group '", ref_type, "' not found in 'type' column.")
  }

  # Reorder types by median(value), ref_type last
  type_order <- df |>
    dplyr::filter(type != ref_type) |>
    dplyr::group_by(type) |>
    dplyr::summarise(median_value = stats::median(value, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(median_value)) |>
    dplyr::pull(type)
  type_order <- c(type_order, ref_type)
  df <- df |>
    dplyr::mutate(type = forcats::fct_relevel(type, type_order))

  # Reference vector
  ref_vec <- df |>
    dplyr::filter(type == ref_type) |>
    dplyr::pull(value)

  if (length(ref_vec) < 2) {
    stop("Reference group '", ref_type, "' has fewer than 2 observations, tests not meaningful.")
  }

  # Reference median for reporting
  ref_median <- stats::median(ref_vec, na.rm = TRUE)

  # Run both tests per type vs ref
  if (calculate_effect_size) {
    test_results <- df |>
      dplyr::filter(type != ref_type) |>
      dplyr::group_by(type) |>
      dplyr::summarise(
        n_group     = dplyr::n(),
        median_val  = stats::median(value, na.rm = TRUE),

        # KS test (distribution)
        ks_D        = suppressWarnings(stats::ks.test(value, ref_vec)$statistic[[1]]),
        ks_p        = suppressWarnings(stats::ks.test(value, ref_vec)$p.value),

        # Wilcoxon rank-sum test (median)
        wilcox_W    = suppressWarnings(stats::wilcox.test(value, ref_vec, exact = FALSE)$statistic[[1]]),
        wilcox_p    = suppressWarnings(stats::wilcox.test(value, ref_vec, exact = FALSE)$p.value),

        # Effect size (rank-biserial correlation)
        rank_biserial = {
          n1 <- dplyr::n()
          n2 <- length(ref_vec)
          U <- suppressWarnings(stats::wilcox.test(value, ref_vec, exact = FALSE)$statistic[[1]])
          (2 * U) / (n1 * n2) - 1
        },

        .groups = "drop"
      )
  } else {
    test_results <- df |>
      dplyr::filter(type != ref_type) |>
      dplyr::group_by(type) |>
      dplyr::summarise(
        n_group     = dplyr::n(),
        median_val  = stats::median(value, na.rm = TRUE),

        # KS test (distribution)
        ks_D        = suppressWarnings(stats::ks.test(value, ref_vec)$statistic[[1]]),
        ks_p        = suppressWarnings(stats::ks.test(value, ref_vec)$p.value),

        # Wilcoxon rank-sum test (median)
        wilcox_W    = suppressWarnings(stats::wilcox.test(value, ref_vec, exact = FALSE)$statistic[[1]]),
        wilcox_p    = suppressWarnings(stats::wilcox.test(value, ref_vec, exact = FALSE)$p.value),

        .groups = "drop"
      )
  }

  # Apply Holm correction to BOTH sets of p-values
  if (calculate_effect_size) {
    test_results <- test_results |>
      dplyr::mutate(
        ks_p_adj     = stats::p.adjust(ks_p, method = adjust_method),
        wilcox_p_adj = stats::p.adjust(wilcox_p, method = adjust_method),

        # Effect size magnitude
        effect_size = dplyr::case_when(
          abs(rank_biserial) < 0.1 ~ "negligible",
          abs(rank_biserial) < 0.3 ~ "small",
          abs(rank_biserial) < 0.5 ~ "medium",
          TRUE ~ "large"
        ),

        # Significance stars for KS test
        ks_signif = dplyr::case_when(
          ks_p_adj <= 1e-4 ~ "****",
          ks_p_adj <= 1e-3 ~ "***",
          ks_p_adj <= 1e-2 ~ "**",
          ks_p_adj <= 5e-2 ~ "*",
          TRUE             ~ "ns"
        ),

        # Significance stars for Wilcoxon test
        wilcox_signif = dplyr::case_when(
          wilcox_p_adj <= 1e-4 ~ "****",
          wilcox_p_adj <= 1e-3 ~ "***",
          wilcox_p_adj <= 1e-2 ~ "**",
          wilcox_p_adj <= 5e-2 ~ "*",
          TRUE                 ~ "ns"
        ),

        # Display helpers
        ks_p_adj_disp = vapply(ks_p_adj, fmt_p_value, character(1)),
        wilcox_p_adj_disp = vapply(wilcox_p_adj, fmt_p_value, character(1))
      ) |>
      dplyr::arrange(ks_p_adj)
  } else {
    test_results <- test_results |>
      dplyr::mutate(
        ks_p_adj     = stats::p.adjust(ks_p, method = adjust_method),
        wilcox_p_adj = stats::p.adjust(wilcox_p, method = adjust_method),

        # Significance stars for KS test
        ks_signif = dplyr::case_when(
          ks_p_adj <= 1e-4 ~ "****",
          ks_p_adj <= 1e-3 ~ "***",
          ks_p_adj <= 1e-2 ~ "**",
          ks_p_adj <= 5e-2 ~ "*",
          TRUE             ~ "ns"
        ),

        # Significance stars for Wilcoxon test
        wilcox_signif = dplyr::case_when(
          wilcox_p_adj <= 1e-4 ~ "****",
          wilcox_p_adj <= 1e-3 ~ "***",
          wilcox_p_adj <= 1e-2 ~ "**",
          wilcox_p_adj <= 5e-2 ~ "*",
          TRUE                 ~ "ns"
        ),

        # Display helpers
        ks_p_adj_disp = vapply(ks_p_adj, fmt_p_value, character(1)),
        wilcox_p_adj_disp = vapply(wilcox_p_adj, fmt_p_value, character(1))
      ) |>
      dplyr::arrange(ks_p_adj)
  }

  # Summary medians (ordered)
  med_table <- df |>
    dplyr::group_by(type) |>
    dplyr::summarise(
      n = dplyr::n(),
      median_value = stats::median(value, na.rm = TRUE),
      iqr = stats::IQR(value, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(median_value))

  # Helper: format p-value to 3 significant figures
  fmt_p <- function(p) {
    fmt_p_value(p)
  }

  # Identify significant types with details
  if (calculate_effect_size) {
    wilcox_sig_df <- test_results |>
      dplyr::filter(wilcox_p_adj < alpha) |>
      dplyr::select(type, wilcox_p_adj, rank_biserial, effect_size)

    # Helper: format list with p-values and effect sizes
    fmt_wilcox_list <- function(df) {
      if (nrow(df) == 0) return("none")
      items <- purrr::pmap_chr(df, function(type, wilcox_p_adj, rank_biserial, effect_size) {
        sprintf("%s (p=%s, r=%.3f, %s effect)",
                type, fmt_p(wilcox_p_adj), rank_biserial, effect_size)
      })
      if (length(items) == 1) return(items)
      paste0(paste(items[-length(items)], collapse = "; "), "; and ", items[length(items)])
    }

    # Compose concise legend-style statement
    legend_text <- paste0(
      "Non-parametric comparisons of each group to '", ref_type, "' ",
      "(N=", n_total, "; K=", n_types, " groups; ", adjust_method, "-adjusted α=", alpha, "): ",
      "Wilcoxon rank-sum tests showed significant differences for ",
      if (nrow(wilcox_sig_df) > 0) fmt_wilcox_list(wilcox_sig_df) else "none",
      ". Effect sizes reported as rank-biserial correlation (r)."
    )
    .min_p_np <- suppressWarnings(min(test_results$wilcox_p_adj, na.rm = TRUE))
  } else {
    wilcox_sig_df <- test_results |>
      dplyr::filter(wilcox_p_adj < alpha) |>
      dplyr::select(type, wilcox_p_adj)

    # Helper: format list with just p-values
    fmt_wilcox_list <- function(df) {
      if (nrow(df) == 0) return("none")
      items <- purrr::pmap_chr(df, function(type, wilcox_p_adj) {
        sprintf("%s (p=%s)", type, fmt_p(wilcox_p_adj))
      })
      if (length(items) == 1) return(items)
      paste0(paste(items[-length(items)], collapse = "; "), "; and ", items[length(items)])
    }

    # Compose concise legend-style statement
    legend_text <- paste0(
      "Non-parametric comparisons of each group to '", ref_type, "' ",
      "(N=", n_total, "; K=", n_types, " groups; ", adjust_method, "-adjusted α=", alpha, "): ",
      "Wilcoxon rank-sum tests showed significant differences for ",
      if (nrow(wilcox_sig_df) > 0) fmt_wilcox_list(wilcox_sig_df) else "none", "."
    )
    .min_p_np <- suppressWarnings(min(test_results$wilcox_p_adj, na.rm = TRUE))
  }
  if (!is.finite(.min_p_np)) .min_p_np <- NA_real_
  concise_stat <- fmt_stat_concise(
    p    = .min_p_np,
    test = paste0("Wilcoxon rank-sum vs '", ref_type, "' (",
                  adjust_method, "-adjusted)"),
    n    = n_total,
    extra = c(sprintf("K = %d groups", n_types),
              "min adjusted p across comparisons")
  )

  # ---- write to file ----
  if (calculate_effect_size) {
    wilcox_cols <- c("type", "n_group", "median_val", "wilcox_W", "wilcox_p", "wilcox_p_adj",
                     "wilcox_signif", "wilcox_p_adj_disp", "rank_biserial", "effect_size")
  } else {
    wilcox_cols <- c("type", "n_group", "median_val", "wilcox_W", "wilcox_p", "wilcox_p_adj",
                     "wilcox_signif", "wilcox_p_adj_disp")
  }

  out_lines <- c(
    paste0("Non-parametric Summary (vs '", ref_type, "')"),
    strrep("=", 70),
    paste0("Date: ", base::format(base::Sys.time(), "%Y-%m-%d %H:%M:%S")),
    paste0("Observations: ", n_total),
    paste0("Groups (types): ", n_types),
    paste0("Reference group: '", ref_type, "' (median = ", signif(ref_median, 4), ")"),
    paste0("Adjustment: ", adjust_method, " | alpha = ", alpha),
    "",
    "Medians by group (descending)",
    strrep("-", 70),
    format_table_txt(med_table),
    "",
    "Kolmogorov-Smirnov tests (distribution differences)",
    strrep("-", 70),
    format_table_txt(
      test_results |>
        dplyr::select(type, n_group, median_val, ks_D, ks_p, ks_p_adj, ks_signif, ks_p_adj_disp)
    ),
    "",
    "Wilcoxon rank-sum tests (median differences)",
    strrep("-", 70),
    format_table_txt(
      test_results |>
        dplyr::select(dplyr::all_of(wilcox_cols))
    ),
    "",
    strrep("=", 70),
    "FIGURE LEGEND (copy-paste ready)",
    strrep("=", 70),
    legend_text,
    strrep("=", 70),
    "",
    "Concise stat",
    strrep("-", 70),
    concise_stat
  )

  base::dir.create(base::dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  base::writeLines(out_lines, con = out_path)

  invisible(list(
    legend = legend_text,
    medians = med_table,
    results = test_results
  ))
}

#' Perform pairwise Wilcoxon rank-sum tests for specific group comparisons
#'
#' @param data Data frame containing the data
#' @param value_col Name of the column containing values (unquoted)
#' @param group_col Name of the column containing groups (unquoted, default = super_class)
#' @param comparisons List of character vectors, each with two group names to compare
#' @param out_path Path for output .txt file
#' @param adjust_method P-value adjustment method (default = "holm")
#' @param alpha Significance level (default = 0.05)
write_pairwise_wilcox <- function(data, value_col, group_col = super_class,
                                   comparisons, out_path,
                                   adjust_method = "holm", alpha = 0.05) {

  gsym <- rlang::ensym(group_col)
  vsym <- rlang::ensym(value_col)

  # Prepare data
  df <- data %>%
    dplyr::select(!!gsym, !!vsym) %>%
    dplyr::filter(is.finite(!!vsym)) %>%
    dplyr::mutate(!!gsym := droplevels(as.factor(!!gsym)))

  # Calculate medians for all groups
  meds <- df %>%
    dplyr::group_by(!!gsym) %>%
    dplyr::summarise(
      n = dplyr::n(),
      median_value = stats::median(!!vsym, na.rm = TRUE),
      iqr = stats::IQR(!!vsym, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::rename(group = !!gsym)

  # Perform Wilcoxon tests for each comparison
  test_results <- purrr::map_dfr(comparisons, function(pair) {
    group1_data <- df %>% dplyr::filter(!!gsym == pair[1]) %>% dplyr::pull(!!vsym)
    group2_data <- df %>% dplyr::filter(!!gsym == pair[2]) %>% dplyr::pull(!!vsym)

    wtest <- stats::wilcox.test(group1_data, group2_data, exact = FALSE)

    # Calculate effect size using rank-biserial correlation
    # The rank-biserial correlation is a standardized measure of effect size for Wilcoxon test
    # Formula: r = (2*U)/(n1*n2) - 1, where U is the Mann-Whitney U statistic
    # This gives: r = +1 when group1 > group2 always, r = -1 when group1 < group2 always
    n1 <- length(group1_data)
    n2 <- length(group2_data)

    U <- wtest$statistic[[1]]
    rank_biserial <- (2 * U) / (n1 * n2) - 1

    # Effect size interpretation
    effect_magnitude <- dplyr::case_when(
      abs(rank_biserial) < 0.1 ~ "negligible",
      abs(rank_biserial) < 0.3 ~ "small",
      abs(rank_biserial) < 0.5 ~ "medium",
      TRUE ~ "large"
    )

    tibble::tibble(
      group1 = pair[1],
      group2 = pair[2],
      n1 = n1,
      n2 = n2,
      median1 = stats::median(group1_data, na.rm = TRUE),
      median2 = stats::median(group2_data, na.rm = TRUE),
      W = wtest$statistic[[1]],
      p_value = wtest$p.value,
      rank_biserial = rank_biserial,
      effect_size = effect_magnitude
    )
  })

  # Apply Holm correction
  test_results <- test_results %>%
    dplyr::mutate(
      p_adj = stats::p.adjust(p_value, method = adjust_method),
      signif = dplyr::case_when(
        p_adj <= 0.0001 ~ "****",
        p_adj <= 0.001  ~ "***",
        p_adj <= 0.01   ~ "**",
        p_adj <= alpha  ~ "*",
        TRUE            ~ "ns"
      ),
      p_adj_disp = vapply(p_adj, fmt_p_value, character(1))
    )

  # Generate legend text
  sig_comparisons <- test_results %>% dplyr::filter(p_adj <= alpha)
  adj_label <- paste0(toupper(substring(adjust_method, 1, 1)),
                      substring(adjust_method, 2))

  if (nrow(sig_comparisons) > 0) {
    comp_items <- sig_comparisons %>%
      dplyr::mutate(
        comp_text = sprintf("%s vs %s (p=%s, r=%.3f, %s effect)",
                           humanise_group(group1), humanise_group(group2),
                           p_adj_disp, rank_biserial, effect_size)
      ) %>%
      dplyr::pull(comp_text)

    if (length(comp_items) == 1) {
      sig_text <- comp_items
    } else {
      sig_text <- paste0(paste(comp_items[-length(comp_items)], collapse = "; "),
                         " and ", comp_items[length(comp_items)])
    }

    legend <- sprintf(
      "Pairwise Wilcoxon rank-sum tests with %s correction for multiple comparisons (\u03b1=%.2f) showed significant differences between the medians for %s. Effect sizes reported as rank-biserial correlation (r).",
      adj_label, alpha, sig_text
    )
  } else {
    legend <- sprintf(
      "Pairwise Wilcoxon rank-sum tests with %s correction for multiple comparisons (\u03b1=%.2f) showed no significant differences between any group medians.",
      adj_label, alpha
    )
  }

  # Write output file
  cat(file = out_path,
      "Pairwise Wilcoxon Rank-Sum Tests\n",
      "======================================================================\n",
      sprintf("Date: %s\n", Sys.time()),
      sprintf("Total observations: %d\n", nrow(df)),
      sprintf("Number of groups: %d\n", length(unique(df[[rlang::as_string(gsym)]]))),
      sprintf("Number of comparisons: %d\n", length(comparisons)),
      sprintf("Adjustment method: %s | alpha = %.2f\n\n", adjust_method, alpha),

      "Medians by group\n",
      "----------------------------------------------------------------------\n",
      sep = ""
  )

  # Write medians table
  med_output <- format_table_txt(meds)
  cat(file = out_path, append = TRUE, paste(med_output, collapse = "\n"), "\n")

  cat(file = out_path, append = TRUE,
      "\n\nPairwise comparisons (Wilcoxon rank-sum tests)\n",
      "----------------------------------------------------------------------\n"
  )

  # Write test results
  test_output <- format_table_txt(test_results %>%
    dplyr::select(group1, group2, n1, n2, median1, median2, W, p_value, p_adj, signif, p_adj_disp, rank_biserial, effect_size))
  cat(file = out_path, append = TRUE, paste(test_output, collapse = "\n"), "\n")

  cat(file = out_path, append = TRUE,
      "\n\n======================================================================\n",
      "FIGURE LEGEND (copy-paste ready)\n",
      "======================================================================\n",
      legend, "\n",
      "======================================================================\n"
  )

  .min_p_pw <- suppressWarnings(min(test_results$p_adj, na.rm = TRUE))
  if (!is.finite(.min_p_pw)) .min_p_pw <- NA_real_
  pw_concise <- fmt_stat_concise(
    p    = .min_p_pw,
    test = paste0("pairwise Wilcoxon rank-sum (", adjust_method, "-adjusted)"),
    n    = sum(meds$n),
    extra = c(sprintf("%d comparisons", length(comparisons)),
              "min adjusted p across pairs")
  )
  cat(file = out_path, append = TRUE,
      "\nConcise stat\n----------------------------------------------------------------------\n",
      pw_concise, "\n"
  )

  list(
    test_results = test_results,
    medians = meds,
    legend = legend
  )
}

############################################################
## FUNCTION: write_dunn_posthoc
## Purpose:
##   Kruskal-Wallis omnibus test followed by Dunn pairwise
##   post-hoc tests (via rstatix::dunn_test). Reports results
##   for highlighted groups and optionally appends formatted
##   output to an existing .txt file.
##
## Inputs:
##   - data          : data.frame/tibble
##   - value_col     : name of numeric column (unquoted)
##   - group_col     : name of grouping column (unquoted, default super_class)
##   - highlights    : character vector of groups to highlight (default c("ascending","descending"))
##   - adjust_method : p.adjust method (default "holm")
##   - alpha         : significance threshold (default 0.05)
##   - append_to     : optional file path to append results to
##
## Requires: dplyr, rstatix, rlang
############################################################

write_dunn_posthoc <- function(data, value_col, group_col = super_class,
                               highlights = c("ascending", "descending"),
                               group_labels = NULL,
                               adjust_method = "holm", alpha = 0.05,
                               append_to = NULL) {

  gsym <- rlang::ensym(group_col)
  vsym <- rlang::ensym(value_col)

  # Prepare data
  df <- data %>%
    dplyr::select(!!gsym, !!vsym) %>%
    dplyr::filter(is.finite(!!vsym)) %>%
    dplyr::mutate(!!gsym := droplevels(as.factor(!!gsym)))

  # Kruskal-Wallis omnibus test
  fml <- stats::as.formula(paste(rlang::as_string(vsym), "~", rlang::as_string(gsym)))
  kw <- rstatix::kruskal_test(df, formula = fml)

  # Medians by group
  meds <- df %>%
    dplyr::group_by(!!gsym) %>%
    dplyr::summarise(
      n = dplyr::n(),
      median_value = stats::median(!!vsym, na.rm = TRUE),
      iqr = stats::IQR(!!vsym, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::rename(group = !!gsym)

  # Dunn pairwise post-hoc
  dunn <- rstatix::dunn_test(df, formula = fml, p.adjust.method = adjust_method)

  # Annotate with medians
  dunn_annotated <- dunn %>%
    dplyr::left_join(meds %>% dplyr::select(group, median_value) %>%
                       dplyr::rename(group1 = group, median1 = median_value), by = "group1") %>%
    dplyr::left_join(meds %>% dplyr::select(group, median_value) %>%
                       dplyr::rename(group2 = group, median2 = median_value), by = "group2")

  # Highlighted pairs (involving highlight groups)
  dunn_hl <- dunn_annotated %>%
    dplyr::filter(group1 %in% highlights | group2 %in% highlights) %>%
    dplyr::mutate(
      hl       = ifelse(group1 %in% highlights, group1, group2),
      hl_med   = ifelse(group1 %in% highlights, median1, median2),
      other_med = ifelse(group1 %in% highlights, median2, median1),
      hl_higher = hl_med > other_med,
      comparison_type = ifelse(group1 %in% highlights & group2 %in% highlights,
                               "within_highlights", "vs_other")
    )

  # --- Statement construction ---
  adj_label <- paste0(toupper(substring(adjust_method, 1, 1)),
                      substring(adjust_method, 2))

  # Helper: format group name with optional parenthetical label
  .label_group <- function(g) {
    nm <- humanise_group(g)
    if (!is.null(group_labels) && g %in% names(group_labels))
      paste0(nm, " (", group_labels[g], ")")
    else nm
  }
  .join_items <- function(items) {
    if (length(items) == 1) return(items)
    if (length(items) == 2) return(paste(items, collapse = " and "))
    paste0(paste(items[-length(items)], collapse = ", "), " and ", items[length(items)])
  }

  # Split into cross (highlighted vs non-highlighted) and within (both highlighted)
  cross_hl  <- dunn_hl %>% dplyr::filter(comparison_type == "vs_other")
  within_hl <- dunn_hl %>% dplyr::filter(comparison_type == "within_highlights")

  # Cross-comparisons: do ALL show highlighted group significantly higher?
  cross_sig <- cross_hl %>% dplyr::filter(hl_higher, p.adj <= alpha)
  all_cross_sig <- nrow(cross_sig) == nrow(cross_hl) & nrow(cross_hl) > 0

  hl_text <- .join_items(vapply(highlights, .label_group, character(1)))

  if (all_cross_sig) {
    max_p <- max(cross_sig$p.adj)
    p_fmt <- fmt_p_value(max_p)
    p_clause <- if (startsWith(p_fmt, "<")) sprintf("all P %s", p_fmt)
                else sprintf("all P \u2264 %s", p_fmt)
    cross_stmt <- sprintf(
      "%s had significantly higher medians than all other groups (%s)",
      hl_text, p_clause)
  } else if (nrow(cross_sig) > 0) {
    # Fallback: list significant cross-comparisons individually
    pair_items <- cross_sig %>%
      dplyr::mutate(
        other = ifelse(group1 %in% highlights, group2, group1),
        item  = sprintf("%s vs %s (P = %s)",
                        .label_group(hl), humanise_group(other),
                        fmt_p_value(p.adj))) %>%
      dplyr::pull(item)
    cross_stmt <- sprintf("%s had significantly higher medians: %s",
                          hl_text, .join_items(pair_items))
  } else {
    cross_stmt <- NULL
  }

  # Within-highlights summary
  within_stmt <- NULL
  if (nrow(within_hl) > 0) {
    ns_rows  <- within_hl %>% dplyr::filter(p.adj > alpha)
    sig_rows <- within_hl %>% dplyr::filter(p.adj <= alpha)
    parts <- c()
    if (nrow(ns_rows) > 0) {
      ns_items <- ns_rows %>%
        dplyr::mutate(item = sprintf("%s vs %s (P = %s)",
                                     humanise_group(group1), humanise_group(group2),
                                     fmt_p_value(p.adj))) %>%
        dplyr::pull(item)
      parts <- c(parts, sprintf("no significant difference between %s",
                                .join_items(ns_items)))
    }
    if (nrow(sig_rows) > 0) {
      sig_items <- sig_rows %>%
        dplyr::mutate(item = sprintf("%s vs %s (P = %s)",
                                     humanise_group(group1), humanise_group(group2),
                                     fmt_p_value(p.adj))) %>%
        dplyr::pull(item)
      parts <- c(parts, sprintf("significant differences between %s",
                                .join_items(sig_items)))
    }
    within_stmt <- paste("Among highlighted groups:", paste(parts, collapse = "; "))
  }

  # Combine
  kw_stmt <- sprintf(
    "Kruskal\u2013Wallis test showed significant variation across groups (H(%d) = %.2f, P = %s).",
    kw$df, kw$statistic, fmt_p_value(kw$p))

  statement <- paste(kw_stmt,
    sprintf("Post-hoc Dunn tests with %s correction for multiple comparisons (\u03b1=%.2g) showed %s.",
            adj_label, alpha,
            cross_stmt %||% "no significant differences among highlighted groups"))
  if (!is.null(within_stmt)) statement <- paste(statement, within_stmt)

  # Optionally append to existing file
  if (!is.null(append_to)) {
    dunn_display <- dunn_hl %>%
      dplyr::select(group1, group2, statistic, p, p.adj, p.adj.signif,
                    median1, median2, hl, hl_higher, comparison_type)

    section_lines <- c(
      "\n\nDunn Post-Hoc Tests (Kruskal-Wallis + pairwise)",
      "----------------------------------------------------------------------",
      sprintf("Omnibus: Kruskal\u2013Wallis H(%d) = %.4f, P = %s",
              kw$df, kw$statistic, fmt_p_value(kw$p)),
      sprintf("Post-hoc: Dunn test with %s correction for multiple comparisons", adj_label),
      sprintf("Highlighted groups: %s\n", paste(highlights, collapse = ", ")),
      "Dunn pairwise results (highlighted groups):",
      "----------------------------------------------------------------------",
      format_table_txt(dunn_display),
      "",
      "----------------------------------------------------------------------",
      "STATEMENT (copy-paste ready)",
      "----------------------------------------------------------------------",
      statement,
      "----------------------------------------------------------------------",
      "Concise stat",
      "----------------------------------------------------------------------",
      fmt_stat_concise(
        p     = kw$p,
        test  = "Kruskal-Wallis omnibus + Dunn post-hoc",
        n     = sum(meds$n),
        extra = c(sprintf("H(%d) = %.2f", kw$df, kw$statistic),
                  sprintf("Dunn %s-adjusted", adjust_method))
      ),
      "----------------------------------------------------------------------"
    )
    cat(file = append_to, append = TRUE, paste(section_lines, collapse = "\n"), "\n")
  }

  invisible(list(
    kw_result = kw,
    dunn_full = dunn_annotated,
    dunn_highlighted = dunn_hl,
    medians = meds,
    statement = statement
  ))
}

############################################################
## FUNCTION: write_diversity_nonparam_summary
## Purpose:
##   For a numeric variable by CATEGORY (and optional GROUP facet):
##   - Summarise n / median / IQR per (group, category)
##   - Within each GROUP: Kruskal–Wallis across CATEGORY
##       + pairwise Wilcoxon (Holm) with rank–biserial r
##   - Between GROUPS for each CATEGORY: pairwise Wilcoxon (Holm)
##       with rank–biserial r (works with 2+ groups)
##   - Write all results + concise statements to a .txt beside your plot
##
## Inputs:
##   df           : data.frame
##   group_col    : name of grouping column (e.g. "group") OR NULL for single-group data
##   category_col : name of category column (e.g. "category")
##   value_col    : name of numeric column (e.g. "cos_sim")
##   plot_path    : path to the saved figure (to derive .txt basename)
##   out_path     : optional explicit .txt path (overrides plot_path)
##   adjust_method: p.adjust method for pairwise tests (default "holm")
##   alpha        : significance threshold for legend prose (default 0.05)
##
## Requires: dplyr, effectsize
############################################################
write_diversity_nonparam_summary <- function(
    df,
    group_col    = "group",
    category_col = "category",
    value_col    = "cos_sim",
    plot_path    = NULL,
    out_path     = NULL,
    adjust_method = "holm",
    alpha         = 0.05,
    calculate_effect_size = FALSE,  # Default FALSE to avoid integer overflow with large datasets
    include_kw   = TRUE              # FALSE: skip KW section + prefix; always run pairwise Wilcoxon
) {
  for (pkg in c("dplyr","effectsize")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Package '", pkg, "' is required but not installed.")
    }
  }
  if (is.null(out_path)) {
    if (is.null(plot_path)) stop("Provide either plot_path (to derive .txt) or out_path.")
    out_path <- paste0(tools::file_path_sans_ext(plot_path), ".txt")
  }
  
  # ---- prep data ----
  has_group <- !is.null(group_col) && group_col %in% names(df)
  if (!all(c(category_col, value_col) %in% names(df))) {
    stop("df is missing required columns: '", category_col, "' and/or '", value_col, "'.")
  }
  df0 <- df |>
    dplyr::mutate(
      !!category_col := as.factor(.data[[category_col]]),
      !!value_col    := as.numeric(.data[[value_col]])
    ) |>
    dplyr::filter(!is.na(.data[[category_col]]), is.finite(.data[[value_col]]))
  
  if (has_group) {
    df0 <- df0 |>
      dplyr::mutate(!!group_col := as.factor(.data[[group_col]])) |>
      dplyr::filter(!is.na(.data[[group_col]]))
  }
  
  n_obs <- nrow(df0)
  k_cat <- base::nlevels(df0[[category_col]])
  k_grp <- if (has_group) base::nlevels(df0[[group_col]]) else 1L
  
  # ---- helpers ----
  .fmt_p <- function(p) {
    val <- fmt_p_value(p)
    ifelse(startsWith(val, "<"), paste("P", val), paste("P =", val))
  }
  .stars <- function(p) {
    if (!is.finite(p)) return("****")
    if (p <= 1e-4) "****" else if (p <= 1e-3) "***" else if (p <= 1e-2) "**"
    else if (p <= 5e-2) "*" else "ns"
  }
  add_sec <- function(lines, ttl, obj = NULL) {
    hdr <- c("", ttl, strrep("-", max(3, nchar(ttl))))
    if (is.null(obj)) {
      c(lines, hdr)
    } else if (inherits(obj, "data.frame")) {
      c(lines, hdr, format_table_txt(obj))
    } else {
      c(lines, hdr, utils::capture.output(obj))
    }
  }
  
  # ---- group/category summaries ----
  if (has_group) {
    med_tbl <- df0 |>
      dplyr::group_by(.data[[group_col]], .data[[category_col]]) |>
      dplyr::summarise(
        n = dplyr::n(),
        median = stats::median(.data[[value_col]]),
        iqr = stats::IQR(.data[[value_col]]),
        .groups = "drop"
      ) |>
      dplyr::arrange(dplyr::desc(median))
    names(med_tbl)[1:2] <- c("group","category")
  } else {
    med_tbl <- df0 |>
      dplyr::group_by(.data[[category_col]]) |>
      dplyr::summarise(
        n = dplyr::n(),
        median = stats::median(.data[[value_col]]),
        iqr = stats::IQR(.data[[value_col]]),
        .groups = "drop"
      ) |>
      dplyr::arrange(dplyr::desc(median))
    names(med_tbl)[1] <- "category"
  }
  
  out <- character()
  out <- add_sec(out, "Diversity: non-parametric summary")
  out <- c(out,
           paste0("Date: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
           paste0("Observations: ", n_obs),
           paste0("Categories (K): ", k_cat),
           paste0("Groups (G): ", k_grp),
           paste0("Category column: '", category_col, "' | Value column: '", value_col, "'"),
           if (has_group) paste0("Group column: '", group_col, "'") else "Group column: <none>",
           paste0("Adjustment: ", adjust_method, " | alpha = ", alpha))
  out <- add_sec(out, "Group/category medians (descending)", med_tbl)
  
  # ---- within-group: KW + pairwise Wilcoxon ----
  within_list <- list()
  within_text <- character()
  if (k_cat >= 2) {
    if (!has_group) {
      df_g <- df0
      kw <- stats::kruskal.test(df_g[[value_col]] ~ df_g[[category_col]])
      if (include_kw) out <- add_sec(out, "Kruskal–Wallis (overall)", kw)
      if (!include_kw || kw$p.value < alpha) {
        pw <- stats::pairwise.wilcox.test(df_g[[value_col]], df_g[[category_col]],
                                          p.adjust.method = adjust_method, exact = FALSE)
        pmat <- pw$p.value
        pw_long <- if (is.null(pmat)) {
          data.frame(category1=character(), category2=character(), p_adj=numeric())
        } else {
          as.data.frame(as.table(pmat), stringsAsFactors = FALSE)
        }
        if (nrow(pw_long)) {
          names(pw_long) <- c("category1","category2","p_adj")
          pw_long <- pw_long[!is.na(pw_long$p_adj), , drop = FALSE]

          # Calculate effect sizes if requested
          if (calculate_effect_size) {
            ef_rows <- lapply(seq_len(nrow(pw_long)), function(i) {
              c1 <- pw_long$category1[i]; c2 <- pw_long$category2[i]
              x <- df_g[[value_col]][df_g[[category_col]] == c1]
              y <- df_g[[value_col]][df_g[[category_col]] == c2]
              ef <- tryCatch(effectsize::rank_biserial(x, y, paired = FALSE), error = function(e) NULL)
              if (is.null(ef)) data.frame(category1=c1, category2=c2, r_rb=NA_real_, CI_low=NA_real_, CI_high=NA_real_)
              else data.frame(category1=c1, category2=c2, r_rb=ef$Rank_biserial, CI_low=ef$CI_low, CI_high=ef$CI_high)
            })
            pw_long <- merge(pw_long, do.call(rbind, ef_rows), by=c("category1","category2"), all.x=TRUE)
          }

          pw_long$stars <- sapply(pw_long$p_adj, .stars)
        }
        out <- add_sec(out, "Pairwise Wilcoxon across categories (adjusted p)", pw_long)
        sig_pairs <- pw_long[pw_long$p_adj < alpha, , drop = FALSE]
        .kw_prefix <- if (include_kw)
          sprintf("Kruskal–Wallis across categories: H(%d) = %.2f, %s. ",
                  kw$parameter, round(unname(kw$statistic), 2), .fmt_p(kw$p.value))
          else ""
        within_text <- c(within_text,
                         if (nrow(sig_pairs))
                           paste0(.kw_prefix, "Pairwise Wilcoxon (", adjust_method, "): ",
                                  paste0(sig_pairs$category1, "–", sig_pairs$category2,
                                         " (", .fmt_p(sig_pairs$p_adj), ")", collapse = "; "), ".")
                         else
                           paste0(.kw_prefix, "Pairwise Wilcoxon (", adjust_method,
                                  "): no significant pairs.")
        )
      } else {
        if (include_kw) {
          within_text <- c(within_text,
                           paste0("Kruskal–Wallis across categories: H(", kw$parameter, ") = ",
                                  round(unname(kw$statistic), 2), ", ", .fmt_p(kw$p.value),
                                  ". No evidence of category differences."))
        }
      }
    } else {
      for (g in levels(df0[[group_col]])) {
        df_g <- df0[df0[[group_col]] == g, , drop = FALSE]
        if (nrow(df_g) < 2 || nlevels(df_g[[category_col]]) < 2) next
        kw <- stats::kruskal.test(df_g[[value_col]] ~ df_g[[category_col]])
        if (include_kw) out <- add_sec(out, paste0("Kruskal–Wallis within group: ", g), kw)
        pw <- NULL; pw_long <- data.frame()
        if (!include_kw || kw$p.value < alpha) {
          pw <- stats::pairwise.wilcox.test(df_g[[value_col]], df_g[[category_col]],
                                            p.adjust.method = adjust_method, exact = FALSE)
          pmat <- pw$p.value
          pw_long <- if (is.null(pmat)) {
            data.frame(category1=character(), category2=character(), p_adj=numeric())
          } else {
            as.data.frame(as.table(pmat), stringsAsFactors = FALSE)
          }
          if (nrow(pw_long)) {
            names(pw_long) <- c("category1","category2","p_adj")
            pw_long <- pw_long[!is.na(pw_long$p_adj), , drop = FALSE]

            # Calculate effect sizes if requested
            if (calculate_effect_size) {
              ef_rows <- lapply(seq_len(nrow(pw_long)), function(i) {
                c1 <- pw_long$category1[i]; c2 <- pw_long$category2[i]
                x <- df_g[[value_col]][df_g[[category_col]] == c1]
                y <- df_g[[value_col]][df_g[[category_col]] == c2]
                ef <- tryCatch(effectsize::rank_biserial(x, y, paired = FALSE), error = function(e) NULL)
                if (is.null(ef)) data.frame(category1=c1, category2=c2, r_rb=NA_real_, CI_low=NA_real_, CI_high=NA_real_)
                else data.frame(category1=c1, category2=c2, r_rb=ef$Rank_biserial, CI_low=ef$CI_low, CI_high=ef$CI_high)
              })
              pw_long <- merge(pw_long, do.call(rbind, ef_rows), by=c("category1","category2"), all.x=TRUE)
            }

            pw_long$group <- g
            pw_long$stars <- sapply(pw_long$p_adj, .stars)
          }
        }
        within_list[[g]] <- pw_long
        sig_pairs <- pw_long[pw_long$p_adj < alpha, , drop = FALSE]
        .kw_prefix <- if (include_kw)
          sprintf("Kruskal–Wallis: H(%d) = %.2f, %s. ",
                  kw$parameter, round(unname(kw$statistic), 2), .fmt_p(kw$p.value))
          else ""
        if (nrow(pw_long)) {
          within_text <- c(within_text,
                           if (nrow(sig_pairs))
                             paste0("[", g, "] ", .kw_prefix,
                                    "Pairwise Wilcoxon (", adjust_method, "): ",
                                    paste0(sig_pairs$category1, "–", sig_pairs$category2,
                                           " (", .fmt_p(sig_pairs$p_adj), ")", collapse = "; "), ".")
                           else
                             paste0("[", g, "] ", .kw_prefix,
                                    "Pairwise Wilcoxon (", adjust_method,
                                    "): no significant pairs.")
          )
        } else if (include_kw) {
          within_text <- c(within_text,
                           paste0("[", g, "] Kruskal–Wallis: H(", kw$parameter, ") = ",
                                  round(unname(kw$statistic), 2), ", ", .fmt_p(kw$p.value),
                                  ". No evidence of category differences."))
        }
        if (nrow(pw_long)) {
          out <- add_sec(out, paste0("Pairwise Wilcoxon across categories (", g, ")"), pw_long)
        }
      }
    }
  }
  
  if (length(within_text)) {
    out <- add_sec(out, "Within-group legend statements", paste(within_text, collapse = "\n"))
  }
  
  # ---- between-groups: per category ----
  between_text <- character()
  if (has_group && k_grp >= 2) {
    cat_levels <- levels(df0[[category_col]])
    between_all <- data.frame()
    for (cname in cat_levels) {
      df_c <- df0[df0[[category_col]] == cname, , drop = FALSE]
      if (nrow(df_c) < 2 || nlevels(df_c[[group_col]]) < 2) next
      pwg <- stats::pairwise.wilcox.test(df_c[[value_col]], df_c[[group_col]],
                                         p.adjust.method = adjust_method, exact = FALSE)
      pmat <- pwg$p.value
      tmp <- if (is.null(pmat)) {
        data.frame(group1=character(), group2=character(), p_adj=numeric())
      } else {
        as.data.frame(as.table(pmat), stringsAsFactors = FALSE)
      }
      if (nrow(tmp)) {
        names(tmp) <- c("group1","group2","p_adj")
        tmp <- tmp[!is.na(tmp$p_adj), , drop = FALSE]

        # Calculate effect sizes if requested
        if (calculate_effect_size) {
          ef_rows <- lapply(seq_len(nrow(tmp)), function(i) {
            g1 <- tmp$group1[i]; g2 <- tmp$group2[i]
            x  <- df_c[[value_col]][df_c[[group_col]] == g1]
            y  <- df_c[[value_col]][df_c[[group_col]] == g2]
            ef <- tryCatch(effectsize::rank_biserial(x, y, paired = FALSE), error = function(e) NULL)
            if (is.null(ef)) data.frame(group1=g1, group2=g2, r_rb=NA_real_, CI_low=NA_real_, CI_high=NA_real_)
            else data.frame(group1=g1, group2=g2, r_rb=ef$Rank_biserial, CI_low=ef$CI_low, CI_high=ef$CI_high)
          })
          tmp <- merge(tmp, do.call(rbind, ef_rows), by=c("group1","group2"), all.x=TRUE)
        }

        tmp$category <- cname
        tmp$stars <- sapply(tmp$p_adj, .stars)
        between_all <- rbind(between_all, tmp)
      }
    }
    if (nrow(between_all)) {
      out <- add_sec(out, "Between-group Wilcoxon per category (adjusted p)", between_all)
      sig_bt <- between_all[between_all$p_adj < alpha, , drop = FALSE]
      if (nrow(sig_bt)) {
        between_text <- paste0(
          "Between groups (pairwise Wilcoxon, ", adjust_method, "): significant differences for ",
          paste0(sig_bt$category, " [", sig_bt$group1, "–", sig_bt$group2, ", ",
                 .fmt_p(sig_bt$p_adj), "]", collapse = "; "), "."
        )
      } else {
        between_text <- paste0("Between groups (pairwise Wilcoxon, ", adjust_method,
                               "): no significant differences across categories.")
      }
      out <- add_sec(out, "Between-group legend statement", between_text)
    }
  }
  
  # ---- write file ----
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  writeLines(out, con = out_path)
  
  invisible(list(
    medians = med_tbl,
    within_pairs = within_list,
    out_path = out_path
  ))
}
extract_three_letters <- function(text) {
  sapply(text, function(t) {
    three_letters <- stringr::str_extract(t, "^[A-Za-z]{3}")
    if (!is.na(three_letters)) {
      return(three_letters)
    }
    two_letters <- stringr::str_extract(t, "^[A-Za-z]{2}")
    if (!is.na(two_letters)) {
      return(two_letters)
    }
    one_letter <- stringr::str_extract(t, "^[A-Za-z]{1}")
    return(one_letter)
  })
}

# Function to calculate cosine similarity for sparse matrices
cosine_similarity_sparse <- function(mat) {
  # Calculate the norm of each column
  col_norms <- sqrt(colSums(mat^2))
  
  # Normalize the matrix
  mat_normalized <- mat %*% Diagonal(x = 1 / col_norms)
  
  # Calculate cosine similarity
  sim <- t(mat_normalized) %*% mat_normalized
  
  return(as.matrix(sim))
}

# A dorsal view of the BANC brain
dorsal <- structure(c(0.997957646846771, -0.0199870802462101, 
                      0.0606706738471985, 0, 0.055451937019825, 0.742548227310181, 
                      -0.667493462562561, 0, -0.0317096672952175, 0.66949450969696, 
                      0.742140114307404, 0, 0, 0, 0, 1), dim = c(4L, 4L))

# Help merge
.merge_hclust <- function(hclist) {
  #-- Merge
  d <- as.dendrogram(hclist[[1]])
  for (i in 2:length(hclist)) {
    d <- merge(d, as.dendrogram(hclist[[i]]))
  }
  as.hclust(d)
}

# Define the hclust_semisupervised and .merge_hclust functions
hclust_semisupervised <- function(data, groups, 
                                  dist_method = "cosine",
                                  dist_p = 2, 
                                  hclust_method = "ward.D2") {
  hclist <- lapply(groups, function (group) {
    if(dist_method=="cosine"){
      datag <- data[match(group,rownames(data)),]
      cosine_sim_matrix <- cosine_similarity_sparse(t(datag))
      colnames(cosine_sim_matrix) <- rownames(cosine_sim_matrix) <- rownames(datag)
      cosine_sim_matrix[is.na(cosine_sim_matrix)] <- 0
      hclust(as.dist(1 - cosine_sim_matrix), 
             method = hclust_method)
    }else{
      hclust(dist(data[match(group,rownames(data)),], 
                  method = dist_method, 
                  p = dist_p), 
             method = hclust_method) 
    }
  })
  hc <- .merge_hclust(hclist)
  data_reordered <- data[match(unlist(groups),rownames(data)),]
  return(list(data = data_reordered, hclust = hc))
}

adjust_color_brightness <- function(color, factor) {
  col <- col2rgb(color)
  col <- pmin(pmax(col * factor, 0), 255)  
  col <- rgb(t(col), maxColorValue = 255)
  return(col)
}

#' Convert a ggplot object to dark mode while preserving original formatting
#'
#' @param plot A ggplot2 object to convert to dark mode
#' @param bg_color Background color for the plot
#' @param text_color Text color for labels, titles, etc.
#' @param grid_color Color for grid lines
#' @param line_color Color for trend lines that were previously black
#' @param preserve_colors Logical; whether to preserve original point/line colors
#'        or adjust them for better visibility on dark background
#' @param brighten_factor Factor by which to brighten colors if preserve_colors=FALSE
#'
#' @return A ggplot2 object with dark mode theme applied while preserving original formatting
#'
#' Convert a ggplot object to dark mode with transparent background
#'
#' @param plot A ggplot2 object to convert to dark mode
#' @param text_color Text color for labels, titles, etc.
#' @param grid_color Color for grid lines
#' @param line_color Color for trend lines that were previously black
#' @param preserve_colors Logical; whether to preserve original point/line colors
#'        or adjust them for better visibility on dark background
#' @param brighten_factor Factor by which to brighten colors if preserve_colors=FALSE
#' @param panel_bg_alpha Alpha transparency for panel background (0=fully transparent)
#'
#' @return A ggplot2 object with dark mode theme applied with transparent background
#'
convert_to_dark_mode <- function(plot, 
                                 text_color = "#FFFFFF",
                                 grid_color = "#555555",
                                 line_color = "#FFFFFF",
                                 preserve_colors = TRUE,
                                 brighten_factor = 1.3,
                                 panel_bg_alpha = 0.5) {
  
  library(ggplot2)
  
  # Function to brighten colors if needed
  brighten_color <- function(color, factor = brighten_factor) {
    # Convert to RGB
    rgb_vals <- col2rgb(color) / 255
    
    # Brighten RGB values
    rgb_vals <- pmin(rgb_vals * factor, 1)
    
    # Convert back to hex
    hex <- rgb(rgb_vals[1], rgb_vals[2], rgb_vals[3])
    return(hex)
  }
  
  # Extract current colors if we need to adjust them
  if (!preserve_colors && !is.null(plot$scales$get_scales("colour"))) {
    # Try to get the scale
    color_scale <- plot$scales$get_scales("colour")
    
    # If it's a discrete scale with a palette
    if (inherits(color_scale, "ScaleDiscrete") && !is.null(color_scale$palette)) {
      # Get number of levels
      n_colors <- length(unique(ggplot2::ggplot_build(plot)$data[[1]]$colour))
      if (n_colors > 0) {
        # Get current colors
        current_colors <- color_scale$palette(n_colors)
        
        # Brighten existing colors
        brightened_colors <- sapply(current_colors, brighten_color)
        
        # Replace with brightened colors
        plot <- plot + scale_color_manual(values = brightened_colors)
      }
    }
  }
  
  # Find any black line color specifications in geom_smooth and change to white
  for (i in seq_along(plot$layers)) {
    if ("GeomSmooth" %in% class(plot$layers[[i]]$geom)) {
      if (!is.null(plot$layers[[i]]$aes_params$colour) && 
          plot$layers[[i]]$aes_params$colour == "black") {
        plot$layers[[i]]$aes_params$colour <- line_color
      }
    }
    
    # Also check for black points, lines or text
    if (any(c("GeomPoint", "GeomLine", "GeomText", "GeomLabel") %in% class(plot$layers[[i]]$geom))) {
      if (!is.null(plot$layers[[i]]$aes_params$colour) && 
          plot$layers[[i]]$aes_params$colour == "black") {
        plot$layers[[i]]$aes_params$colour <- line_color
      }
    }
  }
  
  # Create a semi-transparent panel background (dark with transparency)
  panel_bg_color <- rgb(0.1, 0.1, 0.1, panel_bg_alpha)  # Dark gray with transparency
  
  # Create a new theme with transparent backgrounds
  dark_theme <- theme(
    # Full transparency for plot background
    plot.background = element_rect(fill = "transparent", color = NA),
    
    # Semi-transparent or transparent panel background
    panel.background = element_rect(fill = panel_bg_color, color = NA),
    
    # Text colors only (preserve sizes, angles, etc.)
    axis.title = element_text(color = text_color),
    axis.text = element_text(color = text_color),
    plot.title = element_text(color = text_color),
    plot.subtitle = element_text(color = text_color),
    legend.text = element_text(color = text_color),
    legend.title = element_text(color = text_color),
    strip.text = element_text(color = text_color),
    
    # Grid lines
    panel.grid.major = element_line(color = grid_color),
    panel.grid.minor = element_line(color = grid_color),
    
    # Facet backgrounds - transparent
    strip.background = element_rect(fill = "transparent", color = NA),
    
    # Legend - transparent background
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.key = element_rect(fill = "transparent", color = NA),
    
    # Make sure plot area is transparent
    panel.border = element_rect(color = NA, fill = NA)
  )
  
  # Apply the dark theme
  dark_plot <- plot + dark_theme
  
  return(dark_plot)
}

banc_interpret_umaps <- function(
    umap.df,
    influence.df,
    elist.pre = NULL,
    elist.post = NULL,
    identifier,
    umaps = TRUE,
    neuroanatomy = TRUE,
    neuroanatomy.xyz = FALSE,
    anatomy.group = "cluster",
    banc.meta  = NULL,
    save.path = NULL,
    cluster.colors = NULL,
    inf.metric = "influence_log",
    cluster.centroids = FALSE,
    seed.map = NULL,
    target.map = NULL,
    recalculate = FALSE,
    width = 10,
    height = 10,
    dpi = 300,
    ncores = NULL,
    scaled_heatmap_palette = NULL,
    scaled_heatmap_breaks = NULL,
    # Optional fixed zoom box (UMAP-space). When provided, the function
    # uses these instead of the per-seed bbox computed from non-NA
    # influence points — so a "concise sample" of cell types can be
    # locked across panels (sensors vs effectors) and metrics. Set both
    # NULL to fall back to per-seed auto-zoom (legacy behaviour).
    xlim_fixed = NULL,
    ylim_fixed = NULL
){
  # Parallelism control (added 2026-04-09):
  # - ncores = NULL -> auto-detect (min(detectCores()-1, 6L)), but honour BANC_NCORES env var
  # - ncores = 1L (or BANC_NCORES=1) -> sequential (old behaviour)
  # - otherwise -> parallel::mclapply with fork-based workers (macOS/Linux)
  .env_ncores <- suppressWarnings(as.integer(Sys.getenv("BANC_NCORES", NA_character_)))
  if (is.null(ncores)) {
    ncores <- if (!is.na(.env_ncores)) .env_ncores
              else max(1L, min(parallel::detectCores() - 1L, 6L))
  }
  ncores <- max(1L, as.integer(ncores))
  .is_forkable <- .Platform$OS.type != "windows"
  
  # create save folder
  dir.create(file.path(save.path,identifier), recursive = TRUE, showWarnings = FALSE)
  
  # Calculate cluster centroids
  if(cluster.centroids){
    cluster_centroids <- umap.df %>%
      group_by(cluster) %>%
      summarise(UMAP1 = mean(UMAP1, na.rm = TRUE),
                UMAP2 = mean(UMAP2, na.rm = TRUE)) 
  }else{
    cluster_centroids <- data.frame()
  }
  
  # Create a function to generate n colors
  cerise_limon_base <- c("#EE5B32", "#F6B83C", "#4BA747", "#5BB6E4", "#7C378A")
  cerise_limon_palette <- grDevices::colorRampPalette(cerise_limon_base)
  
  ##############################
  ### WHAT ARE OUR CLUSTERS? ###
  ##############################
  if(umaps){
    
    # Iterate over influence
    # Rename seeds
    if(!is.null(names(seed.map))){
      influence.df <- influence.df %>%
        dplyr::mutate(seed = case_when(
          seed %in% names(seed.map) ~ seed.map[seed],
          TRUE ~ seed
        )) %>%
        dplyr::filter(seed %in% unname(seed.map))
    }
    if(!is.null(names(target.map))){
      influence.df <- influence.df %>%
        dplyr::mutate(target = case_when(
          target %in% names(target.map) ~ target.map[target],
          TRUE ~ target
        )) %>%
        dplyr::filter(target %in% unname(target.map))
    }
    
      # normalisations
      if(recalculate){
        influence.df <- calculate_influence_norms(influence.df)
      }
      entries <- na.omit(unique(influence.df$seed))
      if(length(entries) == 0 || !(inf.metric %in% colnames(influence.df)) ||
         all(is.na(influence.df[[inf.metric]]))){
        message("banc_interpret_umaps: no usable influence data for inf.metric=",
                inf.metric, " — skipping umaps block")
        entries <- character(0)
      }
      if (length(entries) > 0) {
        # Compute thresholds from NON-ZERO values only (2026-04-10) — zeros
        # are fill values from absent influence and drag the low quantile down,
        # compressing the color scale into saturation for meaningful values.
        # Use 10th/99th percentiles (widened 2026-04-10 from 5th/95th) to give
        # more dynamic range at the hot end — the old 95th percentile was too
        # aggressive, clipping most mid-high values to saturated red.
        .nz_vals <- na.omit(influence.df[[inf.metric]])
        .nz_vals <- .nz_vals[.nz_vals > 0]
        if (length(.nz_vals) > 0) {
          thresh.high <- quantile(.nz_vals, 0.99)
          thresh.low  <- quantile(.nz_vals, 0.10)
        } else {
          thresh.high <- 1
          thresh.low  <- 0
        }
      }
      # Render one UMAP overlay PNG for a single seed 'entry'. Extracted so we can
      # run it in parallel via mclapply (added 2026-04-09 to cut blowouts runtime).
      .render_seed_entry <- function(entry){
        message("Working on influence seed: ", entry)
        inf.entry <- influence.df %>%
          dplyr::filter(seed==entry)
        if(max(inf.entry[[inf.metric]],na.rm=TRUE)==0){
          message("no data for entry: ", entry)
          return(invisible(NULL))
        }
        if("id"%in%colnames(umap.df)){
          umap.df.entry <- dplyr::left_join(umap.df,
                                            inf.entry[,c("id",inf.metric)] %>%
                                              dplyr::distinct(),
                                            by = "id") 
        }else{
          umap.df.entry <- dplyr::left_join(umap.df,
                                            inf.entry[,c("cell_type",inf.metric)] %>%
                                              dplyr::distinct(),
                                            by = "cell_type")
        }
        umap.df.entry$norm <- umap.df.entry[[inf.metric]]
        if(all(is.na(umap.df.entry[[inf.metric]]))){
          return(invisible(NULL))
        }
        # Keep all rows (cluster + grey-context). Order: NA first (drawn at
        # the bottom by geom_point), then ascending norm so the highest
        # values render on top.
        umap.df.entry <- umap.df.entry %>%
          dplyr::arrange(!is.na(norm), norm)
        umap.df.entry$norm <- ifelse(umap.df.entry$norm>thresh.high,thresh.high,umap.df.entry$norm)
        umap.df.entry$norm <- ifelse(umap.df.entry$norm<thresh.low,thresh.low,umap.df.entry$norm)
        # Bounding box. If the caller supplied xlim_fixed/ylim_fixed, use
        # them (locks the zoom across panels/metrics for a fixed cell-type
        # sample). Otherwise fall back to per-seed auto-zoom: encompass
        # only the points with non-NA influence from THIS seed.
        if (!is.null(xlim_fixed) && !is.null(ylim_fixed)) {
          .xlim_clust <- xlim_fixed
          .ylim_clust <- ylim_fixed
        } else {
          .clust_pts <- umap.df.entry[!is.na(umap.df.entry$norm), c("UMAP1","UMAP2"), drop = FALSE]
          if (nrow(.clust_pts) >= 2) {
            .x_rng <- range(.clust_pts$UMAP1, na.rm = TRUE)
            .y_rng <- range(.clust_pts$UMAP2, na.rm = TRUE)
            .pad   <- 0.05  # 5% padding
            .x_pad <- diff(.x_rng) * .pad
            .y_pad <- diff(.y_rng) * .pad
            .xlim_clust <- c(.x_rng[1] - .x_pad, .x_rng[2] + .x_pad)
            .ylim_clust <- c(.y_rng[1] - .y_pad, .y_rng[2] + .y_pad)
          } else {
            .xlim_clust <- NULL
            .ylim_clust <- NULL
          }
        }
        
        # scale colors
        n_breaks <- 100
        if(is.null(scaled_heatmap_palette)){
          scaled_heatmap_palette <- grDevices::colorRampPalette(c("#1f4e79", "#4a90a4", "#7ba7bc", "#a67c8a", "#c4967d", "#b22222"))(n_breaks - 1)
        }
        if(is.null(scaled_heatmap_breaks)){
          scaled_heatmap_breaks <- seq(
            stats::quantile(influence.df[[inf.metric]], 0, na.rm = TRUE), 
            stats::quantile(influence.df[[inf.metric]], 1, na.rm = TRUE), 
            length.out = n_breaks)
          thresh.high <- max(scaled_heatmap_breaks,na.rm=TRUE)
          thresh.low <- min(scaled_heatmap_breaks,na.rm=TRUE)
          umap.df.entry$norm <- ifelse(umap.df.entry$norm>thresh.high,thresh.high,umap.df.entry$norm)
          umap.df.entry$norm <- ifelse(umap.df.entry$norm<thresh.low,thresh.low,umap.df.entry$norm)
        }
        
        # Make plot
        if(is.null(umap.df.entry$super_class)){
          p_entry <- ggplot(umap.df.entry, aes(x = UMAP1,
                                               y = UMAP2,
                                               color = norm)) +
            #geom_density_2d(aes(group = 1), col="grey70", alpha = 0.5) +
            geom_point(data = subset(umap.df.entry, is.na(norm)),
                       alpha = 0.6,
                       size = 1.5,
                       col = "grey70") +
            geom_point(data = subset(umap.df.entry, !is.na(norm)),
                       alpha = 0.9,
                       size = 3) +
            scale_color_gradientn(colours = scaled_heatmap_palette,
                                  values = scales::rescale(scaled_heatmap_breaks),
                                  limits = c(thresh.low, thresh.high),
                                  na.value = "grey70") +
            theme_void() +
            labs(title = "",
                 x = "UMAP1",
                 y = "UMAP2") +
            theme(
              legend.position = "bottom",
              legend.text = element_text(size = 6),
              legend.title = element_text(size = 8),
              legend.key.size = unit(0.5, "cm")
            ) +
            labs(color = paste0(entry,": norm")) +
            ggplot2::coord_fixed(xlim = .xlim_clust, ylim = .ylim_clust)
          
          if(nrow(cluster_centroids)){
            p_entry <- p_entry + 
              geom_text(data = cluster_centroids,
                        aes(label = cluster),
                        colour = "black",
                        size = 4,
                        fontface = "bold")
          }
        }else{
          # Uniform circles (shape = 19) for all points — no per-super_class
          # shape distinction (2026-05-12).
          p_entry <- ggplot(umap.df.entry, aes(x = UMAP1,
                                               y = UMAP2,
                                               color = norm)) +
            geom_point(data = subset(umap.df.entry, is.na(norm)),
                       alpha = 0.6,
                       size = 1.5,
                       shape = 19,
                       col = "grey70") +
            geom_point(data = subset(umap.df.entry, !is.na(norm)),
                       alpha = 0.9,
                       size = 3,
                       shape = 19) +
            scale_color_gradientn(colours = scaled_heatmap_palette,
                                  values = scales::rescale(scaled_heatmap_breaks),
                                  limits = c(thresh.low, thresh.high),
                                  na.value = "grey70",
                                  guide = guide_legend(title = "points:")) +
            theme_void() +
            labs(title = "",
                 x = "UMAP1",
                 y = "UMAP2") +
            theme(
              legend.position = "bottom",
              legend.text = element_text(size = 6),
              legend.title = element_text(size = 8),
              legend.key.size = unit(0.5, "cm")
            ) +
            labs(color = paste0(entry,": norm")) +
            ggplot2::coord_fixed(xlim = .xlim_clust, ylim = .ylim_clust)

          if(nrow(cluster_centroids)){
            p_entry <- p_entry +
              geom_text(data = cluster_centroids,
                        aes(label = cluster),
                        colour = "black",
                        size = 4,
                        fontface = "bold")
          }
        }
        
        # Save. Defaults to PNG, but a small allow-list of seed entries are
        # written as vector PDFs (for Illustrator placement in the paper).
        .pdf_seed_basenames <- c(
          "influence_log_minmax_visual vertical widefield motion_umap",
          "influence_log_minmax_prosternal hair plate_umap",
          "influence_log_minmax_proboscis motor_umap",
          "influence_log_minmax_neck roll_umap"
        )
        .base <- paste0(inf.metric, "_", gsub("_|m ", "", entry), "_umap")
        .ext  <- if (.base %in% .pdf_seed_basenames) ".pdf" else ".png"
        dir.create(file.path(save.path,identifier), recursive = TRUE, showWarnings = FALSE)
        ggsave(plot = p_entry,
               filename = file.path(save.path, identifier, paste0(.base, .ext)),
               width = width, height = height, dpi = dpi)
        invisible(NULL)
      }  # end .render_seed_entry

      # Dispatch the per-seed render. Fall back to sequential lapply when
      # ncores == 1 or the platform can't fork (Windows).
      if (length(entries) > 0) {
        if (ncores > 1L && .is_forkable) {
          message(sprintf("banc_interpret_umaps: rendering %d seed PNGs via mclapply on %d cores",
                          length(entries), ncores))
          tryCatch(
            parallel::mclapply(entries, .render_seed_entry,
                               mc.cores = ncores, mc.preschedule = FALSE),
            error = function(e) {
              message("mclapply failed, falling back to sequential: ", e$message)
              lapply(entries, .render_seed_entry)
            }
          )
        } else {
          lapply(entries, .render_seed_entry)
        }
      }

    # Iterate over connectivity features, looking at direct outputs to postsynaptic targets
    if(!is.null(elist.pre)){
      cols <- c("post_super_class","post_cell_function", "post_nerve",
                "post_cell_class", "post_cell_sub_class",
                "post_body_part_effector", "post_body_part_sensory", "post_origin")
      cols <- intersect(cols,colnames(elist.pre))
      for(col in cols){
        message("Working on outputs from: ", col)
        elist.pre.select <- elist.pre
        entries <- na.omit(unique(elist.pre.select[[col]]))
        entries <- entries[!grepl(">|/",entries)]
        if(col=="post_origin"){
          entries <- c(entries,"Leg|leg","Wing|wing", "neck|notum", "IntTct","ANm|abdomen")
        }
        for(entry in entries){
          if(col=="pre_origin"){
            dn.elist.entry <- elist.post[!is.na(elist.post[[col]]),]
            dn.elist.entry <- dn.elist.entry[grepl(entry,dn.elist.entry[[col]]),]
          }else{
            dn.elist.entry <- elist.pre[!is.na(elist.pre[[col]]),]
            dn.elist.entry <- dn.elist.entry[dn.elist.entry[[col]]==entry,] 
          }
          if(max(dn.elist.entry$norm,na.rm=TRUE)<0.005|nrow(dn.elist.entry)<25){
            next
          }
          umap.df.entry <- dplyr::left_join(umap.df,
                                            dn.elist.entry %>%
                                              dplyr::select(pre, post, norm) %>%
                                              dplyr::mutate(norm = ifelse(norm>0.02,0.02,norm)),
                                            by = c("id"="pre")) %>%
            dplyr::arrange(dplyr::desc(norm))
          
          # Make plot
          p_entry <- ggplot(umap.df.entry, aes(x = UMAP1, y = UMAP2, color = norm)) +
            #geom_density_2d(col="grey70", alpha = 0.5) +
            geom_point(data = subset(umap.df.entry, is.na(norm)), alpha = 0.9, size = 2, col = "grey30") +
            geom_point(data = subset(umap.df.entry, norm==0), alpha = 0.9, size = 2, col = "grey30") +
            geom_point(data = subset(umap.df.entry, !is.na(norm)&norm!=0), alpha = 0.9, size = 2) +
            scale_color_gradientn(colours = rev(cerise_limon_palette(100)),
                                  #values = scales::rescale(connection_heatmap_breaks),
                                  limits = c(0, 0.02),
                                  na.value = "grey30") +
            theme_void() +
            labs(title = "",
                 x = "UMAP1",
                 y = "UMAP2") +
            theme(
              legend.position = "bottom",
              legend.text = element_text(size = 5),  # Adjust this value to change label size
              legend.title = element_text(size = 8),  # Adjust this value to change title size
              legend.key.size = unit(0.5, "cm")  # Adjust this value to change the size of the color bar
            ) +
            geom_text(data = cluster_centroids,
                      aes(label = cluster),
                      colour = "grey70",
                      size = 4,
                      fontface = "bold") +
            ggplot2::coord_fixed()
          
          # Save
          dir.create(file.path(save.path,identifier,col), recursive = TRUE, showWarnings = FALSE)
          ggsave(plot = p_entry,
                 filename = file.path(save.path,identifier,col,paste0(col,"_",entry,"_umap.png")),
                 width = 10, height = 10, dpi = dpi)
        }
      }
      
    }
    
    # Iterate over connectivity features, looking at direct inputs from presynaptic targets
    if(!is.null(elist.post)){
      cols <- c("pre_super_class","pre_cell_function", "pre_nerve",
                "pre_cell_class", "pre_cell_sub_class",
                "pre_body_part_effector", "pre_body_part_sensory", "pre_origin")
      cols <- intersect(cols,colnames(elist.post))
      for(col in cols){
        message("Working on inputs from: ", col)
        elist.post.select <- elist.post
        entries <- na.omit(unique(elist.post.select[[col]]))
        entries <- entries[!grepl(">|/",entries)]
        if(col=="pre_origin"){
          entries <- c(entries,"Leg|leg","Wing|wing", "neck|notum", "IntTct","ANm|abdomen")
        }
        for(entry in entries){
          if(col=="pre_origin"){
            dn.elist.entry <- elist.post[!is.na(elist.post[[col]]),]
            dn.elist.entry <- dn.elist.entry[grepl(entry,dn.elist.entry[[col]]),]
          }else{
            dn.elist.entry <- elist.post[!is.na(elist.post[[col]]),]
            dn.elist.entry <- dn.elist.entry[dn.elist.entry[[col]]==entry,]
          }
          if(max(dn.elist.entry$norm,na.rm=TRUE)<0.005|nrow(dn.elist.entry)<25){
            next
          }
          umap.df.entry <- dplyr::left_join(umap.df,
                                            dn.elist.entry %>%
                                              dplyr::select(pre, post, norm) %>%
                                              dplyr::mutate(norm = ifelse(norm>0.02,0.02,norm)),
                                            by = c("id"="post")) %>%
            dplyr::arrange(dplyr::desc(norm))
          
          # Make plot
          p_entry <- ggplot(umap.df.entry, aes(x = UMAP1, y = UMAP2, color = norm)) +
            #geom_density_2d(col="grey70", alpha = 0.5) +
            geom_point(data = subset(umap.df.entry, is.na(norm)), alpha = 0.9, size = 2, col = "grey30") +
            geom_point(data = subset(umap.df.entry, norm==0), alpha = 0.9, size = 2, col = "grey30") +
            geom_point(data = subset(umap.df.entry, !is.na(norm)&norm!=0), alpha = 0.9, size = 2) +
            scale_color_gradientn(colours = rev(cerise_limon_palette(100)),
                                  #values = scales::rescale(connection_heatmap_breaks),
                                  limits = c(0, 0.02),
                                  na.value = "grey30") +
            theme_void() +
            labs(title = "",
                 x = "UMAP1",
                 y = "UMAP2") +
            theme(
              legend.position = "bottom",
              legend.text = element_text(size = 5),  # Adjust this value to change label size
              legend.title = element_text(size = 8),  # Adjust this value to change title size
              legend.key.size = unit(0.5, "cm")  # Adjust this value to change the size of the color bar
            ) +
            geom_text(data = cluster_centroids,
                      aes(label = cluster),
                      colour = "grey70",
                      size = 4,
                      fontface = "bold") +
            ggplot2::coord_fixed()
          
          # Save
          dir.create(file.path(save.path,identifier,col), recursive = TRUE, showWarnings = FALSE)
          ggsave(plot = p_entry,
                 filename = file.path(save.path,identifier,col, paste0(col,"_",entry,"_umap.png")),
                 width = 10, height = 10, dpi = dpi)
        }
      } 
    }
  }
  
  ###############################
  ### VISUALISE OUR CLUSTERS? ###
  ###############################
  
  if(neuroanatomy){
    # plot DN UMAP clusters neuropils
    g.anat <- ggplot2::ggplot() +
      ggplot2::coord_fixed() +
      ggplot2::theme_void() +
      ggplot2::guides(fill = "none", color = "none") +
      ggplot2::theme(legend.position = "none",
                     plot.title = ggplot2::element_text(hjust = 0, size = 8,
                                                        face = "bold",
                                                        colour = "black"),
                     axis.title.x = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_blank(),
                     axis.text.y = ggplot2::element_blank(),
                     axis.ticks.y = ggplot2::element_blank(),
                     axis.line = ggplot2::element_blank(),
                     panel.grid.major = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank(),
                     plot.margin = ggplot2::margin(0, 0, 0, 0),
                     panel.spacing = ggplot2::unit(0, "cm"),
                     panel.border = ggplot2::element_blank(),
                     panel.background = ggplot2::element_blank(),
                     plot.background = ggplot2::element_blank()) +
      ggplot2::labs(title = '')
    
    # Plot over clusters
    for(clust in sort(unique(umap.df[[anatomy.group]]))){
      try({
        message("Working on neuroanatomy for: ",clust)
        
        # Get neuron data
        neuron.meta <- umap.df[umap.df[[anatomy.group]]==clust,]
        neuron.meta <- neuron.meta %>%
          dplyr::arrange(cell_type)
        cts <- na.omit(unique(neuron.meta$cell_type))
        banc.meta.cluster <- banc.meta %>%
          dplyr::filter(cell_type %in% cts | fafb_cell_type %in% cts | manc_cell_type %in% cts) %>%
          dplyr::arrange(cell_type)
        if(!nrow(banc.meta)){
          next
        }
        neuron.ids <- unique(banc.meta.cluster$root_id)
        neuron.ids <- na.omit(neuron.ids)
        plot.neurons <- banc_read_l2skel(neuron.ids, OmitFailures = TRUE)
        if(!length(plot.neurons)){
          next
        }
        if(neuroanatomy.xyz){
          plot.neurons <- nat::xyzmatrix(plot.neurons)
        }else{
          plot.neurons <- plot.neurons[sample(1:length(plot.neurons),min(300,length(plot.neurons)))]
          plot.neurons <-bancr:::banc_reroot(plot.neurons, roots = banc.meta.cluster)
        }
        
        # MAIN
        g.dn.main <- g.anat +
          geom_neuron(x = banc_neuropil.surf,
                      cols = c("grey60", "grey30"),
                      rotation_matrix = bancr:::banc_rotation_matrices[["main"]],
                      alpha = 0.1) +
          geom_neuron(x = plot.neurons,
                      root = TRUE,
                      cols = cerise_limon_palette(length(plot.neurons)),
                      #cols = c(adjust_color_brightness(cluster.colors[[clust]], 0.3),adjust_color_brightness(cluster.colors[[clust]], 1.7)),
                      rotation_matrix = bancr:::banc_rotation_matrices[["main"]],
                      alpha = 0.3)
        dir.create(file.path(save.path,identifier,"neuroanatomy"), recursive = TRUE, showWarnings = FALSE)
        ggsave(plot = g.dn.main,
               filename = file.path(save.path, identifier,"neuroanatomy",paste0(clust,"_neuroanatomy_main.png")),
               width = 10, height = 10, dpi = 72)
        
        # VNC
        plot.neurons.vnc <- banc_decapitate(plot.neurons, invert = FALSE, OmitFailures = TRUE)
        if(length(plot.neurons.vnc)){
          plot.neurons.vnc <- plot.neurons.vnc[unlist(lapply(plot.neurons.vnc,function(x) !is.null(x)))]
          plot.neurons.vnc <-bancr:::banc_reroot(plot.neurons.vnc, roots = banc.meta.cluster, OmitFailures = TRUE)
          if(!length(plot.neurons.vnc)){
            next
          }
          g.dn.vnc <- g.anat +
            geom_neuron(x = banc_vnc_neuropil.surf,
                        cols = c("grey60", "grey30"),
                        rotation_matrix = bancr:::banc_rotation_matrices[["vnc"]],
                        alpha = 0.1) +
            geom_neuron(x = plot.neurons.vnc,
                        root = TRUE,
                        cols = cerise_limon_palette(length(plot.neurons.vnc)),
                        #cols = c(adjust_color_brightness(cluster.colors[[clust]], 0.3),adjust_color_brightness(cluster.colors[[clust]], 1.7)),
                        rotation_matrix = bancr:::banc_rotation_matrices[["vnc"]],
                        alpha = 0.3)
          g.dn.vnc.side <- g.anat +
            geom_neuron(x = banc_vnc_neuropil.surf,
                        root = TRUE,
                        cols = c("grey60", "grey30"),
                        rotation_matrix = bancr:::banc_rotation_matrices[["vnc_side"]],
                        alpha = 0.1) +
            geom_neuron(x = plot.neurons.vnc,
                        root = TRUE,
                        cols = cerise_limon_palette(length(plot.neurons.vnc)),
                        #cols = c(adjust_color_brightness(cluster.colors[[clust]], 0.3),adjust_color_brightness(cluster.colors[[clust]], 1.7)),
                        rotation_matrix = bancr:::banc_rotation_matrices[["vnc_side"]],
                        alpha = 0.3)
          # Save
          ggsave(plot = g.dn.vnc,
                 filename = file.path(save.path,identifier,"neuroanatomy",paste0(clust,"_neuroanatomy_vnc.png")),
                 width = 10, height = 10, dpi = 72)
          ggsave(plot = g.dn.vnc.side,
                 filename = file.path(save.path,identifier,"neuroanatomy",paste0(clust,"_neuroanatomy_vnc_side.png")),
                 width = 10, height = 10, dpi = 72)
        } 
        
        # BRAIN
        plot.neurons.brain <- banc_decapitate(plot.neurons, invert = TRUE, OmitFailures = TRUE)
        if(length(plot.neurons.brain)){
          plot.neurons.brain <- plot.neurons.brain[unlist(lapply(plot.neurons.brain,function(x) !is.null(x)))]
          plot.neurons.brain <- bancr:::banc_reroot(plot.neurons.brain, roots = banc.meta.cluster, OmitFailures = TRUE)
          if(!length(plot.neurons.brain)){
            next
          }
          g.dn.brain <- g.anat +
            geom_neuron(x = banc_brain_neuropil.surf,
                        cols = c("grey60", "grey30"),
                        rotation_matrix = bancr:::banc_rotation_matrices[["front"]],
                        alpha = 0.1) +
            geom_neuron(x = plot.neurons.brain,
                        root = TRUE,
                        cols = cerise_limon_palette(length(plot.neurons.brain)),
                        #cols = c(adjust_color_brightness(cluster.colors[[clust]], 0.3),adjust_color_brightness(cluster.colors[[clust]], 1.7)),
                        rotation_matrix = bancr:::banc_rotation_matrices[["front"]],
                        alpha = 0.3)
          g.dn.brain.side <- g.anat +
            geom_neuron(x = banc_brain_neuropil.surf,
                        cols = c("grey60", "grey30"),
                        rotation_matrix = bancr:::banc_rotation_matrices[["brain_side"]],
                        alpha = 0.1) +
            geom_neuron(x = plot.neurons.brain,
                        root = TRUE,
                        cols = cerise_limon_palette(length(plot.neurons.brain)),
                        #cols = c(adjust_color_brightness(cluster.colors[[clust]], 0.3),adjust_color_brightness(cluster.colors[[clust]], 1.7)),
                        rotation_matrix = bancr:::banc_rotation_matrices[["brain_side"]],
                        alpha = 0.3)
          g.dn.brain.dorsal <- g.anat +
            geom_neuron(x = banc_brain_neuropil.surf,
                        cols = c("grey60", "grey30"),
                        rotation_matrix = dorsal,
                        alpha = 0.1) +
            geom_neuron(x = plot.neurons.brain,
                        root = TRUE,
                        cols = cerise_limon_palette(length(plot.neurons.brain)),
                        #cols = c(adjust_color_brightness(cluster.colors[[clust]], 0.3),adjust_color_brightness(cluster.colors[[clust]], 1.7)),
                        rotation_matrix = dorsal,
                        alpha = 0.3)
          ggsave(plot = g.dn.brain,
                 filename = file.path(save.path,identifier,"neuroanatomy",paste0(clust,"_neuroanatomy_brain.png")),
                 width = 10, height = 10, dpi = 72)
          ggsave(plot = g.dn.brain.side,
                 filename = file.path(save.path,identifier,"neuroanatomy",paste0(clust,"_neuroanatomy_brain_side.png")),
                 width = 10, height = 10, dpi = 72)
          ggsave(plot = g.dn.brain.dorsal,
                 filename = file.path(save.path,identifier,"neuroanatomy",paste0(clust,"_neuroanatomy_brain_dorsal.png")),
                 width = 10, height = 10, dpi = 72)
        }
      })
    }
  }
  return(NULL)
}

banc_plot_key_features <- function(
    influence.meta,
    save.path,
    inf.metric = "influence_norm_log",
    numbers = FALSE,
    col.annotation = NULL,
    row.annotation = NULL,
    show.annotation = TRUE,
    row.thresh = NULL,
    col.thresh = NULL,
    col.order = FALSE,
    row.order = FALSE,
    row.select = NULL,
    col.select = NULL,
    recalculate = FALSE,
    seed.map = NULL,
    target.map = NULL,
    chosen.seeds = unique(seed.map),
    chosen.targets = unique(target.map),
    super.class = NULL,
    influence.level = NULL,
    row.dend = NULL,
    col.dend = NULL,
    row.cols = NULL,
    dend.cols = NULL,
    plot.name = NULL,
    rev = FALSE,
    width = 24, 
    height = 24,
    symmetric = FALSE,
    show.rownames = TRUE,
    show.colnames = TRUE,
    cellheight = 12,
    cellwidth = 12,
    color.min = NULL, 
    color.max = NULL,
    autocorrelation = FALSE,
    diagonal = TRUE,
    method = "cosine",
    quantile = NULL
){
  
  # Defensive guard: skip (not crash) when upstream filters left empty/unusable data.
  # Added 2026-04-09 — both panel_cluster_sensory_correlations.R and
  # panels_cell_type_blowouts.R call this with filter chains that can produce
  # empty inputs, and reshape2::dcast on all-NA values crashes with
  # "dim(ordered) <- ns : dims [product 1] do not match the length of object [0]".
  .skip_plot <- function(reason) {
    msg <- sprintf("banc_plot_key_features skipping %s: %s",
                   if (is.null(plot.name)) "<unnamed>" else plot.name, reason)
    message(msg)
    return(invisible(NULL))
  }
  if (is.null(influence.meta) || !inherits(influence.meta, "data.frame") ||
      nrow(influence.meta) == 0) {
    return(.skip_plot("influence.meta is empty"))
  }
  if (!all(c("seed", "target") %in% colnames(influence.meta))) {
    return(.skip_plot("influence.meta missing seed/target columns"))
  }

  # Reshape the data
  influence_df <- influence.meta %>%
    dplyr::filter(!is.na(seed),
                  !is.na(target))
  if (nrow(influence_df) == 0) {
    return(.skip_plot("no rows after !is.na(seed)/!is.na(target) filter"))
  }
  if(!is.null(influence.level)){
    influence_df <- influence_df %>%
      dplyr::filter(level %in% influence.level)
    if (nrow(influence_df) == 0) {
      return(.skip_plot(sprintf("no rows at level %s",
                                paste(influence.level, collapse = ","))))
    }
  }
  if(!is.null(super.class)){
    influence_df <- influence_df %>%
      dplyr::filter(grepl(super.class,super_class))
    if (nrow(influence_df) == 0) {
      return(.skip_plot(sprintf("no rows matching super.class=%s", super.class)))
    }
  }

  # Rename seeds
  if(!is.null(names(seed.map))){
    influence_df <- influence_df %>%
      dplyr::mutate(seed = case_when(
        seed %in% names(seed.map) ~ seed.map[seed],
        TRUE ~ seed
      ))
  }
  if(!is.null(names(target.map))){
    influence_df <- influence_df %>%
      dplyr::mutate(target = case_when(
        target %in% names(target.map) ~ target.map[target],
        TRUE ~ target
      ))
  }

  # normalisations
  if(recalculate){
    influence_df <- calculate_influence_norms(influence_df, quantile=quantile)
  }

  # Choose metric — guard against missing column and all-NA
  if (!(inf.metric %in% colnames(influence_df))) {
    return(.skip_plot(sprintf("inf.metric '%s' not in columns (have: %s)",
                              inf.metric,
                              paste(colnames(influence_df), collapse = ","))))
  }
  influence_df$influence_score <- influence_df[[inf.metric]]
  if (all(is.na(influence_df$influence_score)) ||
      all(!is.finite(influence_df$influence_score))) {
    return(.skip_plot(sprintf("all %s values NA or non-finite (nrow=%d)",
                              inf.metric, nrow(influence_df))))
  }
  
  # Filter seeds
  if(!is.null(chosen.seeds)){
    influence_df <- influence_df %>%
      dplyr::filter(seed %in% chosen.seeds)
  }
  if(!is.null(chosen.targets)){
    influence_df <- influence_df %>%
      dplyr::filter(target %in% chosen.targets)
  }

  # Cast
  influence_matrix <- influence_df  %>%
    dplyr::distinct(seed, target, .keep_all = TRUE) %>%
    reshape2::dcast(seed ~ target, 
                    fun.aggregate = mean, 
                    value.var = "influence_score", 
                    fill = 0)
  
  # Guard against empty cast (happens when influence_score is all-zero after fill)
  if (is.null(influence_matrix) || nrow(influence_matrix) == 0 ||
      ncol(influence_matrix) < 2) {
    .nr <- if (is.null(influence_matrix)) 0L else nrow(influence_matrix)
    .nc <- if (is.null(influence_matrix)) 0L else ncol(influence_matrix)
    return(.skip_plot(sprintf("dcast produced %dx%d matrix", .nr, .nc)))
  }

  # Set row names and remove the seed column
  rownames(influence_matrix) <- influence_matrix$seed
  influence_matrix$seed <- NULL
  nams <- dimnames(influence_matrix)

  # Convert to matrix
  influence_matrix <- as.matrix(influence_matrix)
  influence_matrix <- matrix(as.numeric(as.matrix(influence_matrix)),
                             nrow = nrow(influence_matrix),
                             ncol = ncol(influence_matrix))
  influence_matrix[is.na(influence_matrix)] <- 0
  influence_matrix[is.infinite(influence_matrix)] <- 0
  dimnames(influence_matrix) <- nams
  influence_matrix <- t(influence_matrix)

  # After transpose, still guard before any hclust/pheatmap call
  if (nrow(influence_matrix) < 2 || ncol(influence_matrix) < 2) {
    return(.skip_plot(sprintf("final matrix too small: %dx%d",
                              nrow(influence_matrix), ncol(influence_matrix))))
  }
  
  # Remove all-zero rows from the original matrix
  if(!diagonal){
    diag(influence_matrix) <- min(influence_matrix, na.rm = TRUE)
  }

  # Change to autocorrelation matrix:
  if(autocorrelation){
    if(rev){
      dims <- colnames(influence_matrix)
      sparse_matrix <- as(as.matrix(t(influence_matrix)), "dgCMatrix")
      influence_matrix <- cosine_similarity_sparse(t(sparse_matrix))
      rownames(influence_matrix) <- colnames(influence_matrix) <- dims
    }else{
      dims <- rownames(influence_matrix)
      sparse_matrix <- as(as.matrix(t(influence_matrix)), "dgCMatrix")
      influence_matrix <- cosine_similarity_sparse(sparse_matrix)
      rownames(influence_matrix) <- colnames(influence_matrix) <- dims
    }
    if(symmetric){
      col.annotation <- row.annotation
      col.order <- row.order
    }
  }
  if(symmetric){
    row.select <- col.select <- intersect(rownames(influence_matrix),colnames(influence_matrix))
  }
  if(!is.null(row.select)){
    influence_matrix <- influence_matrix[rownames(influence_matrix)%in%row.select,]
  }
  if(!is.null(col.select)){
    influence_matrix <- influence_matrix[,colnames(influence_matrix)%in%col.select]
  }
  
  # remove with thresh
  if(!is.null(row.thresh)){
    row.thresh.real <- quantile(influence_matrix, row.thresh, na.rm = TRUE)
  }
  if(!is.null(col.thresh)){
    col.thresh.real <- quantile(influence_matrix, col.thresh, na.rm = TRUE)
  }
  if(!is.null(row.thresh)){
    influence_matrix <- influence_matrix[apply(influence_matrix, 1, function(row) any(row > row.thresh.real)),]
  }
  if(!is.null(col.thresh)){
    influence_matrix <- influence_matrix[,apply(influence_matrix, 2, function(col) any(col > col.thresh.real))]
  }
  
  # Get col annotations
  annotation_colors <- list()
  if(!is.null(row.annotation)){
    row_annotation <- influence_df %>%
      dplyr::filter(!is.na(target)) %>%
      dplyr::select(eval(row.annotation), target) %>%
      dplyr::distinct(target, .keep_all = TRUE) %>%
      as.data.frame()
    row_annotation[[row.annotation]][is.na(row_annotation[[row.annotation]])] <- "other"
    rownames(row_annotation) <- row_annotation$target
    row_annotation <- row_annotation[rownames(row_annotation) %in% rownames(influence_matrix),]
    row_annotation$target <- NULL 
    entries <- na.omit(unique(row_annotation[[row.annotation]]))
    # Use paper.cols when available, fall back to rainbow (2026-04-10).
    # Ensures EFF super_cluster annotations etc. get their designated colors.
    if (exists("paper.cols") && is.character(paper.cols)) {
      cols <- ifelse(entries %in% names(paper.cols),
                     paper.cols[entries],
                     rainbow(length(entries)))
      names(cols) <- entries
      # Fill any that didn't match with rainbow
      .missing <- is.na(cols) | cols == ""
      if (any(.missing)) {
        cols[.missing] <- rainbow(sum(.missing))
      }
    } else {
      cols <- rainbow(length(entries))
      names(cols) <- entries
    }
    annotation_colors[[row.annotation]] <- cols
  }else{
    row_annotation <- NULL
    if(is.null(col.annotation)){
      annotation_colors <- NULL
    }
  }
  
  # Get col annotations
  if(!is.null(col.annotation)){
    col_annotation <- influence_df %>%
      dplyr::filter(!is.na(seed)) %>%
      dplyr::select(eval(col.annotation), seed) %>%
      dplyr::distinct(seed, .keep_all = TRUE) %>%
      as.data.frame()
    col_annotation[[col.annotation]][is.na(col_annotation[[col.annotation]])] <- "other"
    rownames(col_annotation) <- col_annotation$seed
    col_annotation <- col_annotation[rownames(col_annotation) %in% colnames(influence_matrix),]
    col_annotation$seed <- NULL 
    entries <- na.omit(unique(col_annotation[[col.annotation]]))
    if (exists("paper.cols") && is.character(paper.cols)) {
      cols <- ifelse(entries %in% names(paper.cols),
                     paper.cols[entries],
                     rainbow(length(entries)))
      names(cols) <- entries
      .missing <- is.na(cols) | cols == ""
      if (any(.missing)) cols[.missing] <- rainbow(sum(.missing))
    } else {
      cols <- rainbow(length(entries))
      names(cols) <- entries
    }
    annotation_colors[[col.annotation]] <- cols
  }else{
    col_annotation <- NULL
    if(is.null(row.annotation)){
      annotation_colors <- NULL
    }
  }
  
  # Order
  if(isTRUE(row.order)&!is.null(row.annotation)){
    # Apply semi-supervised clustering
    groups <- split(rownames(row_annotation), row_annotation[[row.annotation]])
    groups <- lapply(groups, function(g) if(length(g) >= 2) g else NULL)
    groups <- groups[!sapply(groups, is.null)]
    clustering_result <- hclust_semisupervised(data = influence_matrix,
                                               groups = groups,
                                               dist_method = "euclidean",
                                               hclust_method = "ward.D2")
    influence_matrix <- clustering_result$data
    row_annotation <- row_annotation[rownames(row_annotation) %in% rownames(influence_matrix), , drop = FALSE]
    hclust_rows <- clustering_result$hclust
  }else if(is.character(row.order)){
    row.order <- intersect(row.order,rownames(influence_matrix))
    influence_matrix <- influence_matrix[row.order,]
    hclust_rows <- FALSE
    row.dend <- FALSE
  }else{
    if(method=="cosine"){
      cosine_sim_matrix_rows <- lsa::cosine(t(influence_matrix))
      cosine_sim_matrix_rows[is.na(cosine_sim_matrix_rows)] <- 0
      hclust_rows <- hclust(as.dist(1 - cosine_sim_matrix_rows), 
                            method = "ward.D2")
    }else{
      row_dist <- dist(influence_matrix, method = method)
      hclust_rows <- hclust(row_dist, method = "ward.D2")
    }
  }
  if(isTRUE(col.order)&!is.null(col.annotation)){
    groups <- split(rownames(col_annotation), col_annotation[[col.annotation]])
    groups <- lapply(groups, function(g) if(length(g) >= 2) g else NULL)
    groups <- groups[!sapply(groups, is.null)]
    clustering_result <- hclust_semisupervised(data = t(influence_matrix),
                                               groups = groups,
                                               dist_method = "euclidean",
                                               hclust_method = "ward.D2")
    influence_matrix <- t(clustering_result$data)
    col_annotation <- col_annotation[rownames(col_annotation) %in% colnames(influence_matrix), , drop = FALSE]
    hclust_cols <- clustering_result$hclust
  }else if(is.character(col.order)){
    col.order <- intersect(col.order,colnames(influence_matrix))
    influence_matrix <- influence_matrix[,col.order]
    hclust_cols <- FALSE
    col.dend <- FALSE
  }else{
    if(method=="cosine"){
      cosine_sim_matrix_cols <- lsa::cosine(influence_matrix)
      cosine_sim_matrix_cols[is.na(cosine_sim_matrix_cols)] <- 0
      hclust_cols <- hclust(as.dist(1 - cosine_sim_matrix_cols), method = "ward.D2")
    }else{
      col_dist <- dist(t(influence_matrix), method = method)
      hclust_cols <- hclust(col_dist, method = "ward.D2")
    }
  }

  # target rows and columns
  if(symmetric){
    hclust_rows = hclust_cols
    row.dend = col.dend
  }
  if(is.null(row.dend)){
    row.dend = hclust_rows
  }else if (!isFALSE(row.dend)){
    missing <- setdiff(labels(row.dend),rownames(influence_matrix))
    for(m in missing){
      mrow <- matrix(NA,ncol=ncol(influence_matrix),nrow=1)
      rownames(mrow) <- m
      influence_matrix <- rbind(influence_matrix,mrow)
    }
  }
  if(is.null(col.dend)){
    col.dend = hclust_cols
  }else if (!isFALSE(col.dend)){
    missing <- setdiff(labels(col.dend),colnames(influence_matrix))
    for(m in missing){
      mrow <- matrix(NA,nrow=nrow(influence_matrix),ncol=1)
      colnames(mrow) <- m
      influence_matrix <- cbind(influence_matrix,mrow)
    }
  }
  
  # Remove diagonal
  if(!diagonal){
    diag(influence_matrix) <- NA
  }
  
  # Create scaled color palette 5
  n_breaks <- 100
  if(is.null(color.min)){
    color.min <- quantile(influence_matrix, 0.1, na.rm=TRUE)
  }
  if(is.null(color.max)){
    color.max <- quantile(influence_matrix, 0.99, na.rm=TRUE)
  }
  scaled_heatmap_breaks <- seq(color.min, color.max, length.out = n_breaks)
  scaled_heatmap_palette <- colorRampPalette(c("#1f4e79", "#4a90a4", "#7ba7bc", "#a67c8a", "#c4967d", "#b22222"))(n_breaks - 1)
  
  # Toggle showing annotation bars
  if(!show.annotation){
    row_annotation <- NULL
    col_annotation <- NULL
    annotation_colors <- NULL
  }
  
  # Create the heatmap
  if(is.null(plot.name)){
    plot.name <- inf.metric
  }
  if(rev){
    ph.influence <- pheatmap(
      clustering_method = "ward.D2",
      t(influence_matrix),
      display_numbers = numbers,
      number_format = "%.2f",
      cluster_rows = col.dend,
      cluster_cols = row.dend,
      color = scaled_heatmap_palette,
      breaks = scaled_heatmap_breaks,
      annotation_col = row_annotation,
      annotation_row = col_annotation,
      annotation_colors = annotation_colors,
      show_rownames = show.rownames,
      show_colnames = show.colnames,
      treeheight_row = 0, 
      treeheight_col = 0, 
      fontsize_col = 8,
      fontsize_row = 8,
      cellwidth = cellwidth,
      cellheight = cellheight,
      width = width, 
      height = height,
      border_color = NA,
      annotation_legend = TRUE,
      annotation_names_row = FALSE,
      annotation_names_col = FALSE,
      legend = TRUE,
      filename = file.path(save.path,plot.name),
      main = paste0(inf.metric, "\n(row: source, col: target)"),
      na_col = "lightgrey"
    )
  }else{
    ph.influence <- pheatmap(
      clustering_method = "ward.D2",
      influence_matrix,
      display_numbers = numbers,
      number_format = "%.2f",
      cluster_rows = row.dend,
      cluster_cols = col.dend,
      color = scaled_heatmap_palette,
      breaks = scaled_heatmap_breaks,
      annotation_col = col_annotation,
      annotation_row = row_annotation,
      annotation_colors = annotation_colors,
      show_rownames = show.rownames,
      show_colnames = show.colnames,
      treeheight_row = 0, 
      treeheight_col = 0, 
      fontsize_col = 8,
      fontsize_row = 8,
      cellwidth = cellwidth,
      cellheight = cellheight,
      width = width, 
      height = height,
      border_color = NA,
      annotation_legend = TRUE,
      annotation_names_row = FALSE,
      annotation_names_col = FALSE,
      legend = TRUE,
      filename = file.path(save.path,plot.name),
      main = paste0(inf.metric, "\n(row: target, col: source)"),
      na_col = "lightgrey"
      )    
  }

  # Return some useful data
  return(list(
    influence.matrix = influence_matrix,
    row.dend = row.dend,
    col.dend = col.dend
  ))
}



# Rvcg
find_closest_region <- function(df, mesh_list, max.dist = 5000) {
  
  # Function to find distance from point to mesh
  point_to_mesh_distance <- function(point, mesh) {
    distances <- Rvcg::vcgClostKD(mesh=mesh, x=point)
    distances <- distances$quality
    point$distances <- distances
    return(point)
  }
  
  # Iterate through rows where region is NA
  distances <- pbapply::pblapply(mesh_list$RegionList, function(reg){
    p <- point_to_mesh_distance(xyzmatrix(df), mesh = as.mesh3d(subset(mesh_list,reg)))
    p$distances
  })
  
  # Find nearest mesh
  distances.m <- do.call(cbind,distances)
  colnames(distances.m) <- mesh_list$RegionList
  chosen <- apply(abs(distances.m), 1, which.min)
  min.dists <- apply(abs(distances.m), 1, function(row) min(row)<max.dist)
  df$neuropil <- colnames(distances.m)[chosen]
  
  # Assign
  df <- df %>%
    dplyr::mutate(region = dplyr::case_when(
      grepl("vnc",neuropil) ~ "vnc",
      grepl("optic",neuropil) ~ "optic_lobes",
      grepl("GNG|CAN|FLA|AMMC|SAD|PRW",neuropil) ~ "sez",
      grepl("midbrain",neuropil) ~ "central_brain",
    ))
  
  # Determine that some are outside
  df$neuropil[!min.dists] <- paste0("outside_",df$neuropil[!min.dists])
  df$region[!min.dists] <- paste0("outside_",df$region[!min.dists])
  
  # Return
  return(df)
}

# Find which neuropil surfaces synapses are nearest to
pointsnearby_banc <- function(x,id="id"){
  
  # Get volume list
  volumes <- c(subset(banc_vnc_neuropils.surf,"COURT"),subset(banc_brain_neuropils.surf,"ITO"))
  
  # Neuropil missing
  x.no.neuropil <- x %>%
    dplyr::filter((is.na(region)|is.na(neuropil)|grepl("^brain|outside",region))|grepl("outside",neuropil))
  x.neuropil <- x %>%
    anti_join(x.no.neuropil, by=id)
  x.corrected <- find_closest_region(x.no.neuropil, volumes)
  
  # Re-combine and return
  rbind(x.neuropil,x.corrected)
  
}

# Find which neuropil synapses are inside of
pointsinside_banc <- function(x,
                              neuropils = list(banc_brain_neuropils.surf,
                                               banc_vnc_neuropils.surf),
                              volumes = list(neck = banc_neck_connective.surf,
                                             brain = banc_brain_neuropil.surf,
                                             optic_lobes = as.hxsurf(as.mesh3d(subset(banc_brain_neuropils.surf,"optic"))),
                                             sez = as.hxsurf(as.mesh3d(subset(banc_brain_neuropils.surf,"GNG|CAN|FLA|AMMC|SAD|PRW"))),
                                             central_brain = as.hxsurf(as.mesh3d(subset(banc_brain_neuropils.surf,"midbrain"))),
                                             vnc = banc_vnc_neuropil.surf),
                              alpha = 50000,
                              scaling = NULL){
  df = as.data.frame(x)
  df$neuropil = NA
  df$region = NA
  df$side = NA
  df$neuropil <- ""
  df$region <- ""
  df$side <- ""
  points = nat::xyzmatrix(df)
  if(!is.null(scaling)){
    points = points/scaling
  } 
  lrdiffs <- bancr:::banc_lr_position(points,units = "nm")
  sides <- ifelse(lrdiffs>0,"right","left")
  df$side <- sides
  for(vol in 1:length(volumes)){
    neuropil = volumes[[vol]]
    reg = names(volumes)[vol]
    if (!is.null(alpha)) {
      neuropil = alphashape3d::ashape3d(nat::xyzmatrix(neuropil), 
                                        alpha = alpha)
      a = alphashape3d::inashape3d(points = points, 
                                   as3d = neuropil, 
                                   indexAlpha = "ALL")
    }
    else {
      a = nat::pointsinside(x = points, surf = neuropil)
    }
    if(sum(a)) df$region[which(a == T)] = reg
  } 
  for(brain in neuropils){
    nps = sort(brain$RegionList)
    for (np in nps) {
      neuropil <- subset(brain, np)
      region <- NA
      if(is.na(region)) region = ifelse(np %in%banc_vnc_neuropils.surf$RegionList,"vnc",NA)
      if(is.na(region)) region = ifelse(grepl("^LO|^ME|^AME|^LOP",np),"optic_lobes",NA)
      if(is.na(region)) region = ifelse(grepl("^CAN|^GNG|^FLA|^AMMC|^SAD|^PRW",np),"optic_lobes",NA)
      if(is.na(region)) region = ifelse(np %in%banc_brain_neuropils.surf$RegionList,"central_brain",NA)
      if (!is.null(alpha)) {
        neuropil = alphashape3d::ashape3d(nat::xyzmatrix(neuropil), 
                                          alpha = alpha)
        a = alphashape3d::inashape3d(points = points, 
                                     as3d = neuropil, 
                                     indexAlpha = "ALL")
      }
      else {
        a = nat::pointsinside(x = points, surf = neuropil)
      }
      if(sum(a)){
        df$neuropil[which(a)] = sapply(df$neuropil[which(a)], function(x) paste(unique(unlist(strsplit(paste(x,np,sep=","),split=","))),sep=",",collapse=","))
        df$region[which(a)] = region
      }
    } 
  }
  df <- df %>%
    dplyr::mutate(neuropil = ifelse(neuropil=="","outside",neuropil),
                  region = ifelse(region=="","outside",region)) %>%
    dplyr::mutate(neuropil = gsub("^,","",neuropil),
                  region = gsub("^,","",region)) 
  df
}

# round numbers
round_dataframe <- function(x, exclude=NULL, digits = 4, ...) {
  numcols <- names(x)[sapply(x, function(c) is.numeric(c) && !inherits(c, 'integer64'))]
  numcols <- setdiff(numcols, exclude)
  for(i in numcols) {
    col=x[[i]]
    # does it look like an int, if so, make it one
    intcol=try(checkmate::asInteger(col), silent = TRUE)
    if((sum(is.na(col))==length(col))){
      x[[i]]=col
    }else if(is.integer(intcol)){
      x[[i]]=intcol
    }
    else{
      x[[i]]= signif(col, digits)
    }
  }
  x
}


# ---- Extracted helpers (Task #19, 2026-05-21) -----------------------------

#' Find the maximum-angle elbow in a (rank, value) curve over a given range
#'
#' Walks a downsampled curve with a sliding three-point window, computes
#' the angle between the two segments at each point, and returns the
#' (rank, value) of the maximum-angle point inside the [start_rank,
#' end_rank] range. Used to identify the elbow of the cumulative
#' adjusted-influence distribution (panels_body_parts.R yields the
#' canonical 17.28 cutoff that downstream panels read from
#' data/determined_thresholds/influence_norm_log_elbow_threshold.csv).
#'
#' @param ranks numeric vector of rank positions, ascending.
#' @param values numeric vector of values aligned with `ranks`.
#' @param start_rank numeric; lower bound of the search range.
#' @param end_rank numeric; upper bound of the search range.
#' @param window_size integer; half-width of the sliding window
#'   (default 100); the function skips ranges with fewer than
#'   `2 * window_size` points.
#' @return list with `rank` and `value` of the maximum-angle point,
#'   or both `NA` if the range is too small.
#' @details Vectors are normalised before the dot product so the returned
#'   angle is in degrees (0–180). Used by panels_body_parts.R (ED Fig. 5e
#'   elbow) and panels_mbx_cx_control.R (Fig. 6f threshold sanity check).
#' @section Used by:
#'   R/figures/panels_body_parts.R; R/figures/panels_mbx_cx_control.R
find_angle_change_in_range <- function(ranks, values, start_rank, end_rank, window_size = 100) {
  idx_in_range <- which(ranks >= start_rank & ranks <= end_rank)
  ranks_subset <- ranks[idx_in_range]
  values_subset <- values[idx_in_range]

  if (length(ranks_subset) <= 2 * window_size) {
    return(list(rank = NA, value = NA))
  }

  angles <- numeric(length(ranks_subset) - 2 * window_size)
  for (i in (window_size + 1):(length(ranks_subset) - window_size)) {
    p1 <- c(ranks_subset[i - window_size], values_subset[i - window_size])
    p2 <- c(ranks_subset[i],                values_subset[i])
    p3 <- c(ranks_subset[i + window_size], values_subset[i + window_size])

    v1 <- c(p2[1] - p1[1], p2[2] - p1[2])
    v2 <- c(p3[1] - p2[1], p3[2] - p2[2])

    v1 <- v1 / sqrt(sum(v1^2))
    v2 <- v2 / sqrt(sum(v2^2))

    angles[i - window_size] <- acos(sum(v1 * v2)) * (180 / pi)
  }

  max_angle_idx   <- which.max(angles) + window_size
  max_angle_rank  <- ranks_subset[max_angle_idx]
  max_angle_value <- values_subset[max_angle_idx]

  list(rank = max_angle_rank, value = max_angle_value)
}

#' Shannon entropy of a non-negative vector
#'
#' Normalises `values` to a probability distribution (dropping zeros) and
#' returns its Shannon entropy in bits (log base 2). Used to quantify
#' how concentrated influence is across cell-type groups (e.g. pre-
#' effector influence diversity in panels_pre_effector_influence.R).
#'
#' Renamed from `calculate_entropy` on extraction so the metric is
#' explicit at the call site (there are several distinct "entropy"
#' formulations in the literature; this one is Shannon-in-bits).
#'
#' @param values non-negative numeric vector. NAs are dropped.
#' @return numeric scalar; entropy in bits.
#' @section Used by:
#'   R/figures/panels_pre_effector_influence.R
shannon_entropy <- function(values) {
  probs <- values / sum(values, na.rm = TRUE)
  probs <- probs[probs > 0]
  -sum(probs * log2(probs), na.rm = TRUE)
}

#' Majority vote on a character vector, dropping NAs and empty strings
#'
#' Returns the most frequent value, with ties broken by the order
#' returned by `table()` (which is alphabetic by default).
#'
#' @param x character vector.
#' @return scalar character of the modal value, or `NA_character_`
#'   when `x` is empty after dropping NAs / empty strings.
#' @section Used by:
#'   R/figures/panels_vignette_networks.R (super_class, super_cluster
#'   and majority pre-neurotransmitter aggregation per display_name).
majority_vote <- function(x) {
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) return(NA_character_)
  names(sort(table(x), decreasing = TRUE))[1]
}

#' Kruskal–Wallis + Dunn pairwise summary against highlighted groups
#'
#' Runs a Kruskal–Wallis test of `value_col ~ group_col` plus a Dunn
#' pairwise post-hoc (Holm-corrected), then filters the Dunn output to
#' pairs that involve any of the `highlights` groups and where the
#' highlighted side has the higher median. Returns the raw scalars and
#' the filtered Dunn table so the caller can render both the on-plot
#' bracket summary and the .txt sidecar.
#'
#' @param data data.frame containing `value_col` and `group_col`.
#' @param value_col bare column name (NSE) of the numeric value.
#' @param group_col bare column name (NSE) of the grouping factor
#'   (default `super_class`).
#' @param highlights character vector of group levels whose pairs we
#'   want to retain (default `c("ascending","descending")`).
#' @return list with elements `kw_p`, `max_pairwise_p`, `dunn_table`
#'   (filtered to highlighted-higher pairs), `dunn_full` (all pairs
#'   touching `highlights`), `n_expected`, `n_significant`,
#'   `other_groups`, and `meds` (per-group medians).
#' @section Used by:
#'   R/figures/panels_betweenness_layers.R (Fig. 3a sensory-to-effector
#'   betweenness, ED Fig. 5a all-to-all betweenness).
kw_dunn_summary <- function(data, value_col, group_col = super_class,
                            highlights = c("ascending","descending")) {

  gsym <- rlang::ensym(group_col)
  vsym <- rlang::ensym(value_col)

  df <- data %>%
    dplyr::select(!!gsym, !!vsym) %>%
    dplyr::filter(is.finite(!!vsym)) %>%
    dplyr::mutate(!!gsym := droplevels(as.factor(!!gsym)))

  fml <- stats::as.formula(paste(rlang::as_string(vsym), "~", rlang::as_string(gsym)))
  kw  <- rstatix::kruskal_test(df, formula = fml)
  kw_p <- kw$p

  meds <- df %>%
    dplyr::group_by(!!gsym) %>%
    dplyr::summarise(med = stats::median(!!vsym, na.rm = TRUE), .groups = "drop") %>%
    dplyr::rename(group = !!gsym)

  dunn <- rstatix::dunn_test(df, formula = fml, p.adjust.method = "holm")

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

  dunn_full <- dunn %>%
    dplyr::filter(group1 %in% highlights | group2 %in% highlights) %>%
    dplyr::left_join(meds %>% dplyr::rename(group1 = group, med1 = med), by = "group1") %>%
    dplyr::left_join(meds %>% dplyr::rename(group2 = group, med2 = med), by = "group2") %>%
    dplyr::mutate(
      hl    = ifelse(group1 %in% highlights, group1, group2),
      other = ifelse(group1 %in% highlights, group2, group1)
    )

  all_groups   <- levels(df[[rlang::as_string(gsym)]])
  other_groups <- setdiff(all_groups, highlights)
  n_expected   <- length(highlights) * length(other_groups)

  list(kw_p = kw_p, max_pairwise_p = max_p,
       dunn_table = dunn_hl, dunn_full = dunn_full,
       n_expected = n_expected, n_significant = nrow(dunn_hl),
       other_groups = other_groups, meds = meds)
}
