# standards.R
# Named-standard mention index. Scans each hit's KWIC window for named
# cryptographic standards and produces a per-doc rollup.

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(tibble)
})

# Canonical standards dictionary: name -> Perl regex.
# Sources: NIST FIPS series, NIST PQC programme, ETSI cyber, ITU-T QKD,
# ISO/IEC crypto standards, BSI TR-02102, ANSSI, ENISA PQC reports.
STANDARDS_DICT <- list(
  "FIPS 203"        = "\\bFIPS\\s*203\\b",
  "FIPS 204"        = "\\bFIPS\\s*204\\b",
  "FIPS 205"        = "\\bFIPS\\s*205\\b",
  "FIPS 140"        = "\\bFIPS\\s*140(?:-?\\d+)?\\b",
  "NIST PQC"        = "\\bNIST\\s+PQC\\b",
  "ETSI"            = "\\bETSI\\s+(?:GS|TS|EN)\\s*[A-Z0-9 ]+\\d",
  "ITU-T QKD"       = "\\bITU-T\\s+(?:Y\\.380[01]|X\\.1702)\\b",
  "ISO/IEC 18031"   = "\\bISO[/ ]IEC\\s*18031\\b",
  "ISO/IEC 18033"   = "\\bISO[/ ]IEC\\s*18033\\b",
  "BSI TR-02102"    = "\\bBSI\\s+TR-?02102\\b",
  "ANSSI"           = "\\bANSSI\\b",
  "ENISA PQC"       = "\\bENISA\\s+(?:PQC|report)\\b"
)

#' Per-document rollup of named-standard mentions.
#'
#' @param hits tibble with `doc_id` and the three KWIC columns
#'   `kwic_left`, `kwic_match`, `kwic_right`.
#' @return tibble(doc_id, n_standards, mentions_named_standard) covering
#'   every doc_id seen in `hits`. doc_ids with zero standard mentions
#'   appear with n_standards = 0 and mentions_named_standard = FALSE.
compute_standards <- function(hits) {
  if (is.null(hits) || nrow(hits) == 0) {
    return(tibble::tibble(
      doc_id                   = character(),
      n_standards              = integer(),
      mentions_named_standard  = logical()
    ))
  }

  windows <- paste(hits$kwic_left, hits$kwic_match, hits$kwic_right)

  # For each (hit, standard), TRUE if that standard's regex fires.
  hit_flags <- vapply(STANDARDS_DICT, function(rx) {
    stringr::str_detect(windows, stringr::regex(rx, ignore_case = TRUE))
  }, logical(length(windows)))
  if (!is.matrix(hit_flags)) {
    hit_flags <- matrix(hit_flags, nrow = length(windows))
  }

  # Per-hit standards count: sum across columns.
  per_hit <- tibble::tibble(
    doc_id     = hits$doc_id,
    standards  = rowSums(hit_flags)
  )

  per_doc <- per_hit |>
    dplyr::group_by(doc_id) |>
    dplyr::summarise(
      n_standards             = sum(standards),
      mentions_named_standard = sum(standards) > 0L,
      .groups = "drop"
    ) |>
    dplyr::mutate(n_standards = as.integer(n_standards))

  per_doc
}
