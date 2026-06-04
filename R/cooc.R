# cooc.R
# Document-level co-occurrence between families. v2.0 ships only the
# EXP x INST cell (the only non-empty cell given the 12-sub-code codebook).

suppressPackageStartupMessages({
  library(tibble)
})

#' Family x family co-occurrence (long form).
#'
#' @param documents_wide tibble with integer columns `n_EXP` and `n_INST`.
#' @return tibble(family_a, family_b, cooc_count, jaccard, share_a_with_b).
#'   v2.0 emits one row for EXP x INST.
compute_cooc <- function(documents_wide) {
  if (is.null(documents_wide) || nrow(documents_wide) == 0 ||
      !all(c("n_EXP", "n_INST") %in% names(documents_wide))) {
    return(tibble::tibble(
      family_a       = character(),
      family_b       = character(),
      cooc_count     = integer(),
      jaccard        = numeric(),
      share_a_with_b = numeric()
    ))
  }

  has_exp  <- documents_wide$n_EXP  > 0L
  has_inst <- documents_wide$n_INST > 0L

  n_a    <- sum(has_exp)
  n_b    <- sum(has_inst)
  n_ab   <- sum(has_exp & has_inst)
  n_aub  <- sum(has_exp | has_inst)

  jaccard        <- if (n_aub == 0L) NA_real_ else n_ab / n_aub
  share_a_with_b <- if (n_a   == 0L) NA_real_ else n_ab / n_a

  tibble::tibble(
    family_a       = "EXP",
    family_b       = "INST",
    cooc_count     = as.integer(n_ab),
    jaccard        = jaccard,
    share_a_with_b = share_a_with_b
  )
}
