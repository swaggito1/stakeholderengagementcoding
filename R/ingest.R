# ingest.R
# Read corpus files in .txt / .md / .docx / .pdf into plain text, and assemble
# a coding-ready corpus tibble by resolving the manifest's documents on disk.

suppressPackageStartupMessages({
  library(tibble)
  library(dplyr)
})

SUPPORTED_EXTS <- c("txt", "md", "markdown", "docx", "pdf")

#' Read a single file into a plain-text string.
#' Returns NULL with a warning if the extension is unsupported.
ingest_file <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (!ext %in% SUPPORTED_EXTS) {
    warning("Unsupported file extension: ", ext, " (", basename(path), ")")
    return(NULL)
  }
  text <- tryCatch({
    if (ext %in% c("txt", "md", "markdown")) {
      paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    } else if (ext == "docx") {
      if (!requireNamespace("readtext", quietly = TRUE)) {
        stop("readtext package required for .docx")
      }
      as.character(readtext::readtext(path)$text)
    } else if (ext == "pdf") {
      if (!requireNamespace("pdftools", quietly = TRUE)) {
        stop("pdftools package required for .pdf")
      }
      paste(pdftools::pdf_text(path), collapse = "\n\n")
    }
  }, error = function(e) {
    warning("Failed to read ", basename(path), ": ", conditionMessage(e))
    NULL
  })
  text
}

#' Read a vector of file paths into a corpus tibble(doc_id, text, source_path).
ingest_paths <- function(paths, doc_ids = NULL) {
  if (is.null(doc_ids)) doc_ids <- basename(paths)
  out <- tibble::tibble(
    doc_id      = character(),
    text        = character(),
    source_path = character()
  )
  for (i in seq_along(paths)) {
    txt <- ingest_file(paths[i])
    if (is.null(txt) || !nzchar(trimws(txt))) next
    out <- dplyr::bind_rows(out, tibble::tibble(
      doc_id      = doc_ids[i],
      text        = txt,
      source_path = paths[i]
    ))
  }
  out
}

#' Resolve one manifest row to a file path under `docs_dir`.
#' Prefers the `path` column (as given, then by basename), falling back to
#' `doc_id` treated as a filename. Returns NA_character_ if nothing matches.
resolve_doc_path <- function(row, docs_dir) {
  tryp <- function(name) {
    if (is.null(name) || length(name) == 0 || is.na(name) || !nzchar(name)) {
      return(NA_character_)
    }
    p1 <- file.path(docs_dir, name)
    if (file.exists(p1)) return(normalizePath(p1, winslash = "/"))
    p2 <- file.path(docs_dir, basename(name))
    if (file.exists(p2)) return(normalizePath(p2, winslash = "/"))
    NA_character_
  }
  cand <- NA_character_
  if ("path" %in% names(row)) cand <- tryp(as.character(row$path))
  if (is.na(cand)) cand <- tryp(as.character(row$doc_id))
  cand
}

#' Build a coding-ready corpus tibble by reading the manifest's documents from
#' a directory on disk. Each row is resolved by `path` (preferred) or by
#' `doc_id` treated as a filename, both looked up within `docs_dir`.
#'
#' @param manifest_csv tibble with doc_id, helix, tier, year, optional path
#' @param docs_dir directory containing the referenced documents
#' @return tibble(doc_id, text, source_path, helix, tier, year)
stage_from_dir <- function(manifest_csv, docs_dir) {
  if (!dir.exists(docs_dir)) stop("Documents directory not found: ", docs_dir)
  rows <- vector("list", nrow(manifest_csv))
  for (i in seq_len(nrow(manifest_csv))) {
    row       <- manifest_csv[i, ]
    candidate <- resolve_doc_path(row, docs_dir)
    if (is.na(candidate)) {
      warning("No file found in ", docs_dir, " for doc_id ", row$doc_id)
      next
    }
    txt <- ingest_file(candidate)
    if (is.null(txt) || !nzchar(trimws(txt))) {
      warning("Empty or unreadable text for doc_id ", row$doc_id,
              " (", candidate, ")")
      next
    }
    rows[[i]] <- tibble::tibble(
      doc_id      = as.character(row$doc_id),
      text        = txt,
      source_path = candidate,
      helix       = as.character(row$helix),
      tier        = as.character(row$tier),
      year        = as.integer(row$year)
    )
  }
  dplyr::bind_rows(rows[!vapply(rows, is.null, logical(1))])
}
