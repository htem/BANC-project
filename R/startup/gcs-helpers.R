###########################################################
### GCS Helper Functions for BANC-project
###
### Provides access to connectome data from Google Cloud
### Storage, with support for feather and parquet formats.
###
### Adapted from fly_connectome_data_tutorial.
###
### Requirements:
###   - reticulate R package
###   - Python: gcsfs, pyarrow, pandas
###   - gcloud auth application-default login
###########################################################

#' Setup GCS Filesystem
#'
#' Creates an authenticated Google Cloud Storage filesystem object using gcsfs.
#'
#' @param token Authentication method. "google_default" for gcloud auth,
#'   "anon" for public buckets (default: "google_default")
#' @return A Python gcsfs.GCSFileSystem object
setup_gcs_filesystem <- function(token = "google_default") {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Package 'reticulate' is required for GCS access. Install with: install.packages('reticulate')")
  }
  gcsfs <- reticulate::import("gcsfs")
  message("Authenticating with Google Cloud Storage...")
  gcsfs$GCSFileSystem(token = token)
}

#' Read Feather File from GCS or Local Path
#'
#' @param path Full path to feather file (gs:// for GCS or local path)
#' @param gcs_filesystem GCS filesystem object (required for GCS paths)
#' @param show_progress Show progress messages (default: TRUE)
#' @return A tibble with the file contents
read_feather_smart <- function(path, gcs_filesystem = NULL, show_progress = TRUE) {
  is_gcs <- grepl("^gs://", path)

  if (is_gcs) {
    if (is.null(gcs_filesystem)) {
      stop("gcs_filesystem argument required for GCS paths. Create with setup_gcs_filesystem()")
    }
    if (show_progress) message("Reading from GCS: ", path)

    builtins <- reticulate::import_builtins()
    tmp <- tempfile(fileext = ".feather")
    on.exit(unlink(tmp), add = TRUE)

    if (show_progress) message("Downloading from GCS...")
    gcs_path_clean <- sub("^gs://", "", path)
    gcs_file <- gcs_filesystem$open(gcs_path_clean, "rb")
    py_bytes <- gcs_file$read()
    gcs_file$close()

    if (show_progress) message("Downloaded ", round(length(py_bytes) / 1024^2, 2), " MB from GCS")

    out_file <- builtins$open(tmp, "wb")
    out_file$write(py_bytes)
    out_file$close()

    result <- arrow::read_feather(tmp)
    if (show_progress) message("Loaded ", nrow(result), " rows")
    result
  } else {
    if (show_progress) message("Reading from local path: ", path)
    result <- arrow::read_feather(path)
    if (show_progress) message("Loaded ", nrow(result), " rows")
    result
  }
}

#' Read Feather with Automatic GCS Detection
#'
#' @param path Full path to feather file (gs:// for GCS or local path)
#' @param use_gcs Force GCS mode (default: auto-detect from path)
#' @param show_progress Show progress messages
#' @return A tibble with the file contents
read_feather_gcs <- function(path, use_gcs = FALSE, show_progress = TRUE) {
  if (use_gcs || grepl("^gs://", path)) {
    gcs_fs <- setup_gcs_filesystem()
    read_feather_smart(path, gcs_filesystem = gcs_fs, show_progress = show_progress)
  } else {
    read_feather_smart(path, gcs_filesystem = NULL, show_progress = show_progress)
  }
}

#' Read CSV File from GCS or Local Path
#'
#' @param path Full path to CSV file (gs:// for GCS or local path)
#' @param col_types Column types (passed to readr::read_csv)
#' @param show_progress Show progress messages (default: TRUE)
#' @param ... Additional arguments passed to readr::read_csv
#' @return A tibble with the file contents
read_csv_gcs <- function(path, col_types = NULL, show_progress = TRUE, ...) {
  is_gcs <- grepl("^gs://", path)

  if (is_gcs) {
    gcs_fs <- setup_gcs_filesystem()
    if (show_progress) message("Reading CSV from GCS: ", path)

    builtins <- reticulate::import_builtins()
    tmp <- tempfile(fileext = ".csv")
    on.exit(unlink(tmp), add = TRUE)

    if (show_progress) message("Downloading from GCS...")
    gcs_path_clean <- sub("^gs://", "", path)
    gcs_file <- gcs_fs$open(gcs_path_clean, "rb")
    py_bytes <- gcs_file$read()
    gcs_file$close()

    if (show_progress) message("Downloaded ", round(length(py_bytes) / 1024^2, 2), " MB from GCS")

    out_file <- builtins$open(tmp, "wb")
    out_file$write(py_bytes)
    out_file$close()

    result <- readr::read_csv(tmp, col_types = col_types, show_col_types = FALSE, ...)
    if (show_progress) message("Loaded ", nrow(result), " rows")
    result
  } else {
    if (show_progress) message("Reading from local path: ", path)
    result <- readr::read_csv(path, col_types = col_types, show_col_types = FALSE, ...)
    if (show_progress) message("Loaded ", nrow(result), " rows")
    result
  }
}

#' Query Parquet from GCS with Server-Side Filtering
#'
#' Uses a Python backend to read Parquet from GCS with filters applied
#' on the server before downloading.
#'
#' @param path GCS path to Parquet file (gs://bucket/file.parquet)
#' @param gcs_filesystem GCS filesystem object (kept for interface compatibility)
#' @param filters SQL WHERE clause as string (e.g., "neuropil LIKE 'MB%'")
#' @param columns Character vector of column names to load (NULL = all)
#' @return Tibble with filtered results
query_parquet_gcs <- function(path, gcs_filesystem, filters = NULL, columns = NULL) {
  if (!grepl("^gs://", path)) {
    stop("This function is for GCS paths. Use arrow::open_dataset() for local files.")
  }

  message("Reading Parquet from GCS using Python backend...")

  # Find the Python script
  possible_paths <- c(
    file.path(dirname(sys.frame(1)$ofile %||% ""), "gcs_parquet_reader.py"),
    "R/startup/gcs_parquet_reader.py",
    file.path(getwd(), "R", "startup", "gcs_parquet_reader.py")
  )
  python_script <- Find(file.exists, possible_paths)
  if (is.null(python_script)) {
    stop("Cannot find gcs_parquet_reader.py. Tried:\n  ",
         paste(possible_paths, collapse = "\n  "))
  }

  reticulate::source_python(python_script)

  # Parse filter to extract column and prefix
  filter_column <- NULL
  filter_prefix <- NULL
  if (!is.null(filters) && is.character(filters)) {
    if (grepl("LIKE\\s+'([^']+)'", filters, ignore.case = TRUE)) {
      pattern <- sub(".*LIKE\\s+'([^']+)'.*", "\\1", filters, ignore.case = TRUE)
      filter_column <- trimws(sub("(.+)\\s+LIKE.*", "\\1", filters, ignore.case = TRUE))
      if (grepl("%$", pattern)) filter_prefix <- sub("%$", "", pattern)
    }
  }

  result_df <- query_parquet_gcs(
    gcs_path = path,
    columns = columns,
    filter_column = filter_column,
    filter_prefix = filter_prefix,
    show_progress = TRUE
  )
  tibble::as_tibble(result_df)
}

#' Query a Multi-Chunk Parquet Dataset on GCS with an IN-list Filter
#'
#' Wraps `query_influence_chunks_isin` in gcs_parquet_reader.py. Use this for
#' directories of parquet chunks (e.g. v888 influence/all_to_all/) when you
#' need to slice by `upstream_id IN <list>`. Predicate pushdown happens
#' server-side via pyarrow.dataset so only the matching rows transfer.
#'
#' @param gcs_dir GCS directory containing parquet chunks (gs://bucket/dir/).
#' @param upstream_ids Character vector of values to keep.
#' @param columns Character vector of columns to load (NULL = all).
#' @param upstream_col Column name to filter (default "upstream_id").
#' @return Tibble with filtered rows.
query_parquet_gcs_isin <- function(gcs_dir, upstream_ids, columns = NULL,
                                    upstream_col = "upstream_id") {
  if (!grepl("^gs://", gcs_dir)) {
    stop("This function is for GCS directories. Got: ", gcs_dir)
  }
  python_script <- Find(file.exists, c(
    "R/startup/gcs_parquet_reader.py",
    file.path(getwd(), "R", "startup", "gcs_parquet_reader.py")
  ))
  if (is.null(python_script)) stop("Cannot find gcs_parquet_reader.py")
  reticulate::source_python(python_script)
  result_df <- query_influence_chunks_isin(
    gcs_dir       = gcs_dir,
    upstream_ids  = as.character(upstream_ids),
    columns       = columns,
    upstream_col  = upstream_col,
    show_progress = TRUE
  )
  tibble::as_tibble(result_df)
}

#' Construct Dataset Paths
#'
#' @param data_root Root data directory (can be gs:// or local path)
#' @param dataset Dataset name with version (e.g., "banc_850")
#' @param file_type Type: "meta", "metrics", "synapses", "edgelist", "edgelist_simple"
#' @param flavour Flavour suffix appended to edgelist_simple filenames on
#'   v888+ (e.g. "v2", "v3"). Ignored for other file_types and for pre-v888
#'   datasets, which use the unsuffixed naming convention. Default "v2"
#'   matches the consumer-side default flipped on 2026-04-21.
#' @return Full path to the requested file
construct_path <- function(data_root, dataset, file_type = "meta", flavour = "v2") {
  dataset_name <- strsplit(dataset, "_")[[1]][1]
  version_num <- as.integer(strsplit(dataset, "_")[[1]][2])
  extension <- switch(file_type,
    "meta" = ".feather",
    "metrics" = ".feather",
    "synapses" = ".parquet",
    "edgelist" = ".feather",
    "edgelist_simple" = ".feather",
    stop("Unknown file_type. Choose: meta, metrics, synapses, edgelist, edgelist_simple")
  )
  # v850+ uses nested subdirectory and different naming conventions.
  # v888+ additionally adopts a flavour suffix on edgelist_simple
  # (banc_888_edgelist_simple_v2.feather etc.); v850 stayed unsuffixed.
  if (!is.na(version_num) && version_num >= 850) {
    edgelist_simple_name <- if (!is.na(version_num) && version_num >= 888 && nzchar(flavour)) {
      paste0(dataset, "_edgelist_simple_", flavour, extension)
    } else {
      paste0(dataset, "_edgelist_simple", extension)
    }
    filename <- switch(file_type,
      "edgelist_simple" = edgelist_simple_name,
      "synapses" = if (!is.na(version_num) && version_num >= 888 && nzchar(flavour)) {
        paste0(dataset, "_synapses_", flavour, "_enriched", extension)
      } else {
        paste0(dataset, "_synapses_enriched", extension)
      },
      paste0(dataset, "_", file_type, extension)
    )
    file.path(data_root, dataset, filename)
  } else {
    # Legacy: v746 and earlier (flat structure)
    if (file_type == "edgelist_simple") {
      filename <- paste0(dataset, "_simple_edgelist", extension)
    } else {
      filename <- paste0(dataset, "_", file_type, extension)
    }
    file.path(data_root, dataset, filename)
  }
}

#' Cosine Similarity for Sparse Matrices
#'
#' @param X A sparse matrix (features x samples)
#' @return Cosine similarity matrix (samples x samples)
cosine_similarity_sparse <- function(X) {
  col_norms <- sqrt(colSums(X^2))
  col_norms[col_norms == 0] <- 1
  X_norm <- t(t(X) / col_norms)
  as.matrix(crossprod(X_norm))
}
