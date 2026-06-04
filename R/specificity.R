# specificity.R
# Specificity index for EXP excerpts. 0-3 score per EXP hit, scoring three
# orthogonal dimensions on the KWIC window (pre + match + post):
#   1. deadline verb paired with a 4-digit year
#   2. named actor (governance body / standards setter)
#   3. named cryptographic standard
# Non-EXP rows get NA_integer_.

suppressPackageStartupMessages({
  library(stringr)
})

# Default actor list. Promote to a YAML file once the qsc-country-scraper
# output is wired in.
DEFAULT_ACTORS <- c(
  "ANSSI", "BSI", "NCSC", "NIST", "ENISA",
  "CISA", "NSA", "NATO", "EU", "ISO", "IETF"
)

DEADLINE_REGEX <- "\\b(?:by|until|before|within)\\s+\\d{4}\\b"

#' Add a specificity_score column (0-3 for EXP rows, NA for non-EXP).
#'
#' @param hits tibble with `family`, `kwic_left`, `kwic_match`, `kwic_right`.
#' @param actors character vector of actor names; default DEFAULT_ACTORS.
#' @param standards_dict named list of standard -> regex; default STANDARDS_DICT.
compute_specificity <- function(hits,
                                actors = NULL,
                                standards_dict = NULL) {
  if (is.null(actors)) actors <- DEFAULT_ACTORS
  if (is.null(standards_dict)) {
    standards_dict <- if (exists("STANDARDS_DICT", inherits = TRUE)) {
      get("STANDARDS_DICT", inherits = TRUE)
    } else list()
  }

  if (is.null(hits) || nrow(hits) == 0) {
    hits$specificity_score <- integer()
    return(hits)
  }

  windows <- paste(hits$kwic_left, hits$kwic_match, hits$kwic_right)
  is_exp  <- hits$family == "EXP"

  d_deadline <- stringr::str_detect(
    windows, stringr::regex(DEADLINE_REGEX, ignore_case = TRUE))

  actor_rx <- paste0("\\b(?:", paste(actors, collapse = "|"), ")\\b")
  d_actor <- stringr::str_detect(
    windows, stringr::regex(actor_rx, ignore_case = TRUE))

  d_standard <- if (length(standards_dict) > 0) {
    rxs <- vapply(standards_dict, function(rx) {
      stringr::str_detect(windows, stringr::regex(rx, ignore_case = TRUE))
    }, logical(length(windows)))
    if (!is.matrix(rxs)) rxs <- matrix(rxs, nrow = length(windows))
    apply(rxs, 1, any)
  } else {
    rep(FALSE, length(windows))
  }

  score <- as.integer(d_deadline) + as.integer(d_actor) + as.integer(d_standard)
  score[!is_exp] <- NA_integer_

  hits$specificity_score <- score
  hits
}
