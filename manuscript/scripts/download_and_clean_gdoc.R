#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# download_and_clean_gdoc.R
#
# Downloads the BANC main manuscript Google Doc as a Word .docx file, then
# strips every Paperpile (`paperpile.com/...`) and variable-placeholder
# (`http://var/...`) hyperlink — leaving the visible text (citation numbers,
# rendered variable values like "171,513") intact.
#
# Outputs (next to this script):
#   theBANC_main.raw.docx    — raw Drive export, hyperlinks unchanged
#   theBANC_main.clean.docx  — Paperpile + var/ hyperlinks unwrapped
#
# AUTH: uses the cached gargle OAuth token that R's Google packages share
# (the same one googlesheets4 uses when numbers.R writes the variables
# sheet). The first run will pop up a browser to grant Drive read access if
# no cached token exists.
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(googledrive)
  library(xml2)
})

# Doc ID is loaded from data/private/keys.csv (gitignored) by the
# project's startup. Don't hardcode it here.
if (!exists("banc.keys")) source("R/startup/load-keys.R")
DOC_ID <- banc.keys$gdoc_banc_main_id
if (is.null(DOC_ID) || !nzchar(DOC_ID)) {
  stop("banc.keys$gdoc_banc_main_id not set in data/private/keys.csv — ",
       "cannot resolve which Google Doc to download.")
}

# Locate the script's own directory so output lands next to the script no
# matter the working directory used to invoke Rscript.
.script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  m <- sub("^--file=", "", grep("^--file=", args, value = TRUE))
  if (length(m)) return(dirname(normalizePath(m[1])))
  if (sys.nframe() > 0) {
    ofile <- sys.frames()[[1]]$ofile
    if (!is.null(ofile)) return(dirname(normalizePath(ofile)))
  }
  normalizePath("manuscript/print/text")
}
HERE      <- .script_dir()
RAW_OUT   <- file.path(HERE, "theBANC_main.raw.docx")
CLEAN_OUT <- file.path(HERE, "theBANC_main.clean.docx")

# ---------------------------------------------------------------------------
# 1. Download from Drive as .docx
# ---------------------------------------------------------------------------
message("Authenticating with Google Drive (cached token if available)...")
googledrive::drive_auth()

message("Downloading: ", DOC_ID, " -> ", RAW_OUT)
googledrive::drive_download(
  file = googledrive::as_id(DOC_ID),
  path = RAW_OUT,
  type = "docx",
  overwrite = TRUE
)
message(sprintf("  raw .docx: %.2f MB", file.info(RAW_OUT)$size / 1024^2))

# ---------------------------------------------------------------------------
# 2. Strip Paperpile + var/ hyperlinks
#
# A .docx is a ZIP. Hyperlinks live in two places:
#   word/_rels/document.xml.rels — <Relationship Id="rIdN" Target="https://..."/>
#   word/document.xml             — <w:hyperlink r:id="rIdN">...children...</w:hyperlink>
# We find every relationship whose Target matches paperpile.com or "://var/",
# remove the relationship, then in document.xml unwrap every <w:hyperlink>
# referencing those rIds (replace with its inner runs).
# ---------------------------------------------------------------------------
BAD_URL_RE <- "paperpile\\.com|://var/"

tmp <- tempfile("docx_"); dir.create(tmp)
on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
unzip(RAW_OUT, exdir = tmp)

rels_path <- file.path(tmp, "word", "_rels", "document.xml.rels")
doc_path  <- file.path(tmp, "word", "document.xml")

rels <- xml2::read_xml(rels_path)
pr_ns <- c(d1 = "http://schemas.openxmlformats.org/package/2006/relationships")
rel_nodes <- xml2::xml_find_all(rels, ".//d1:Relationship", pr_ns)
targets <- xml2::xml_attr(rel_nodes, "Target")
bad <- grepl(BAD_URL_RE, targets, ignore.case = TRUE)
bad_rids <- xml2::xml_attr(rel_nodes[bad], "Id")
message(sprintf("Relationships: %d total -> %d to strip (paperpile + var/)",
                length(rel_nodes), length(bad_rids)))
for (n in rel_nodes[bad]) xml2::xml_remove(n)
xml2::write_xml(rels, rels_path)

doc <- xml2::read_xml(doc_path)
w_ns <- c(
  w = "http://schemas.openxmlformats.org/wordprocessingml/2006/main",
  r = "http://schemas.openxmlformats.org/officeDocument/2006/relationships"
)
hls <- xml2::xml_find_all(doc, ".//w:hyperlink", w_ns)
message(sprintf("Hyperlinks in document.xml: %d", length(hls)))

unwrapped <- 0L
for (hl in hls) {
  attrs <- xml2::xml_attrs(hl)
  rid <- NA_character_
  if (length(attrs)) {
    idx <- grep("(^|:|\\})id$", names(attrs))
    if (length(idx)) rid <- unname(attrs[idx[1]])
  }
  if (!is.na(rid) && rid %in% bad_rids) {
    # Move children of the hyperlink up to its parent (preserves run order),
    # then blank every <w:t> and strip <w:u> underline directives from any
    # inner <w:rPr> — so the unwrapped runs contribute no visible text and
    # carry no hyperlink-style underline.
    children <- xml2::xml_children(hl)
    for (ch in children) xml2::xml_add_sibling(hl, ch, .where = "before")
    xml2::xml_remove(hl)
    for (ch in children) {
      # Blank text nodes (preserve the run wrapper so paragraph structure
      # is unchanged).
      for (t in xml2::xml_find_all(ch, ".//w:t", w_ns)) {
        xml2::xml_text(t) <- ""
      }
      # Remove any <w:u .../> nodes inside the run's properties.
      for (u in xml2::xml_find_all(ch, ".//w:rPr/w:u", w_ns)) {
        xml2::xml_remove(u)
      }
    }
    unwrapped <- unwrapped + 1L
  }
}
message(sprintf("Hyperlinks unwrapped: %d", unwrapped))
xml2::write_xml(doc, doc_path)

# ---------------------------------------------------------------------------
# 3. Re-zip into CLEAN_OUT
# ---------------------------------------------------------------------------
if (file.exists(CLEAN_OUT)) file.remove(CLEAN_OUT)
files <- list.files(tmp, recursive = TRUE, all.files = TRUE, full.names = FALSE)
old_wd <- setwd(tmp); on.exit(setwd(old_wd), add = TRUE)
zip(zipfile = CLEAN_OUT, files = files, flags = "-rq")
setwd(old_wd)
message(sprintf("Wrote: %s (%.2f MB)",
                CLEAN_OUT, file.info(CLEAN_OUT)$size / 1024^2))
