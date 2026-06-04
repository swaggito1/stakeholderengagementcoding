# validate.R
# Validate the corpus-manifest CSV submitted alongside files.

suppressPackageStartupMessages({
  library(dplyr)
})

VALID_HELIX <- c("U", "G", "I", "hybrid", "T")
VALID_TIER  <- c("T1", "T2", "T3", "T4")
YEAR_MIN    <- 1995L
YEAR_MAX    <- 2030L
REQUIRED_COLS <- c("doc_id", "helix", "tier", "year")

#' Validate a parsed corpus-manifest CSV.
#'
#' @param csv_df tibble produced by readr::read_csv() on the uploaded CSV.
#' @return list(errors = character(), warnings = character()).
#'   `length(errors) == 0L` means clean.
validate_manifest <- function(csv_df) {
  errors   <- character()
  warnings <- character()

  if (is.null(csv_df) || !inherits(csv_df, "data.frame")) {
    return(list(errors = "CSV could not be parsed as a table.",
                warnings = character()))
  }

  # 1. required columns
  missing_cols <- setdiff(REQUIRED_COLS, names(csv_df))
  if (length(missing_cols) > 0) {
    errors <- c(errors, sprintf("Missing required column(s): %s",
                                paste(missing_cols, collapse = ", ")))
    return(list(errors = errors, warnings = warnings))
  }

  # 2. doc_id non-empty, non-NA, unique
  if (any(is.na(csv_df$doc_id) | !nzchar(trimws(as.character(csv_df$doc_id))))) {
    errors <- c(errors, "Some doc_id values are empty or NA.")
  }
  dups <- csv_df$doc_id[duplicated(csv_df$doc_id)]
  if (length(dups) > 0) {
    errors <- c(errors, sprintf("Duplicate doc_id(s): %s",
                                paste(unique(dups), collapse = ", ")))
  }

  # 3. helix value set
  bad_helix <- setdiff(unique(csv_df$helix), VALID_HELIX)
  bad_helix <- bad_helix[!is.na(bad_helix)]
  if (length(bad_helix) > 0) {
    errors <- c(errors, sprintf("Invalid helix value(s): %s (allowed: %s)",
                                paste(bad_helix, collapse = ", "),
                                paste(VALID_HELIX, collapse = ", ")))
  }

  # 4. tier value set
  bad_tier <- setdiff(unique(csv_df$tier), VALID_TIER)
  bad_tier <- bad_tier[!is.na(bad_tier)]
  if (length(bad_tier) > 0) {
    errors <- c(errors, sprintf("Invalid tier value(s): %s (allowed: %s)",
                                paste(bad_tier, collapse = ", "),
                                paste(VALID_TIER, collapse = ", ")))
  }

  # 5. year integer in [1995, 2030]
  year_int <- suppressWarnings(as.integer(csv_df$year))
  bad_year_rows <- which(is.na(year_int) |
                         year_int < YEAR_MIN |
                         year_int > YEAR_MAX)
  if (length(bad_year_rows) > 0) {
    errors <- c(errors, sprintf(
      "year must be an integer in [%d, %d]; offending rows: %s",
      YEAR_MIN, YEAR_MAX, paste(bad_year_rows, collapse = ", ")))
  }

  # 6. path duplicates (if path column exists)
  if ("path" %in% names(csv_df)) {
    present <- !is.na(csv_df$path) & nzchar(trimws(csv_df$path))
    paths   <- csv_df$path[present]
    if (any(duplicated(paths))) {
      dupes <- unique(paths[duplicated(paths)])
      errors <- c(errors, sprintf("Duplicate path value(s): %s",
                                  paste(dupes, collapse = ", ")))
    }
    n_missing_path <- sum(!present)
    if (n_missing_path > 0) {
      warnings <- c(warnings, sprintf(
        "%d row(s) have no path; doc_id will be matched to filename.",
        n_missing_path))
    }
  } else {
    warnings <- c(warnings,
                  "No path column; every doc_id will be matched to filename.")
  }

  list(errors = errors, warnings = warnings)
}
