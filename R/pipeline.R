# pipeline.R
# Per-document pipeline-completeness flags. v2.0 covers only the two
# families present in the codebook (EXP, INST). When the codebook
# expands to include TRANS / TENT / OPER, add their has_* columns here.

suppressPackageStartupMessages({
  library(tibble)
})

#' Per-document presence flags for each family in the codebook.
#'
#' @param documents_wide tibble with `doc_id`, optional `helix`, `tier`,
#'   `year`, and integer columns `n_EXP`, `n_INST`.
#' @return tibble(doc_id, helix, tier, year, has_EXP, has_INST).
compute_pipeline_completeness <- function(documents_wide) {
  if (is.null(documents_wide) || nrow(documents_wide) == 0) {
    return(tibble::tibble(
      doc_id   = character(),
      helix    = character(),
      tier     = character(),
      year     = integer(),
      has_EXP  = logical(),
      has_INST = logical()
    ))
  }

  helix <- if ("helix" %in% names(documents_wide)) documents_wide$helix else NA_character_
  tier  <- if ("tier"  %in% names(documents_wide)) documents_wide$tier  else NA_character_
  year  <- if ("year"  %in% names(documents_wide)) documents_wide$year  else NA_integer_

  tibble::tibble(
    doc_id   = documents_wide$doc_id,
    helix    = helix,
    tier     = tier,
    year     = year,
    has_EXP  = documents_wide$n_EXP  > 0L,
    has_INST = documents_wide$n_INST > 0L
  )
}
