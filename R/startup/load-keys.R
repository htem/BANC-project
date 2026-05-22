###########################################################################
### Load private identifiers (Google Doc / Sheet / Drive IDs, etc.)
###
### Reads `data/private/keys.csv` into a named list `banc.keys` exposed to
### the global environment. Each row of the CSV is `name,value,description`.
### The file is gitignored (`data/private/`) — see CLAUDE.md / README for
### the canonical contents.
###
### Soft failure: if the file is missing, `banc.keys` is left as an empty
### list and a warning is emitted. Downstream code that needs a specific
### key (e.g. `banc.keys$gsheet_banc_variables_id` in numbers.R) should
### handle a missing key gracefully — typically by skipping the
### Drive-write step.
###########################################################################

banc.keys <- list()

.keys_path <- file.path("data", "private", "keys.csv")
if (file.exists(.keys_path)) {
  .keys_df <- tryCatch(
    readr::read_csv(.keys_path, show_col_types = FALSE,
                    col_types = readr::cols(.default = readr::col_character())),
    error = function(e) {
      warning("Could not parse ", .keys_path, ": ", conditionMessage(e))
      NULL
    }
  )
  if (!is.null(.keys_df) && all(c("name", "value") %in% colnames(.keys_df))) {
    banc.keys <- setNames(as.list(.keys_df$value), .keys_df$name)
    message(sprintf("Loaded %d private keys from %s",
                    length(banc.keys), .keys_path))
  } else {
    warning(.keys_path, " missing required columns `name` and `value`; ",
            "banc.keys left empty.")
  }
  rm(.keys_df)
} else {
  message(sprintf(
    "No %s found — banc.keys is empty. Drive/Sheet write operations (e.g. the Google Sheet update at the end of numbers.R, or download_and_clean_gdoc.R) will be skipped. To enable them, create the file with rows: name,value,description (see CLAUDE.md or the project README for the canonical entries).",
    .keys_path))
}
rm(.keys_path)
