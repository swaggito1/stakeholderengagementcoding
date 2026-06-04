# imbalance.R
# EXP / INST imbalance: delta, B, and the two "expectation-heavy" flags.
# Provenance: B(d) = (n_EXP - n_INST) / (n_EXP + n_INST) is the standard
# polarity-score form (VADER; Rauh 2018; Boumans & Trilling 2016).

suppressPackageStartupMessages({
  library(tibble)
})

#' Add delta, B, expect_heavy_strict, expect_heavy_soft to documents_wide.
#'
#' @param documents_wide tibble with integer columns `n_EXP` and `n_INST`.
#' @param tau numeric threshold (default 0.50) for expect_heavy_soft.
#' @return the input tibble with four columns appended:
#'   - delta               n_EXP - n_INST                              (integer)
#'   - B                   (n_EXP - n_INST) / (n_EXP + n_INST)         (numeric;
#'                         NA_real_ when n_EXP + n_INST == 0)
#'   - expect_heavy_strict (n_INST == 0L) & (n_EXP >= 2L)              (logical)
#'   - expect_heavy_soft   !is.na(B) & B >= tau                        (logical)
compute_imbalance <- function(documents_wide, tau = 0.50) {
  stopifnot(all(c("n_EXP", "n_INST") %in% names(documents_wide)))

  n_exp  <- as.integer(documents_wide$n_EXP)
  n_inst <- as.integer(documents_wide$n_INST)
  denom  <- n_exp + n_inst

  delta <- n_exp - n_inst
  B     <- ifelse(denom == 0L, NA_real_, (n_exp - n_inst) / denom)

  documents_wide$delta               <- delta
  documents_wide$B                   <- B
  documents_wide$expect_heavy_strict <- (n_inst == 0L) & (n_exp >= 2L)
  documents_wide$expect_heavy_soft   <- !is.na(B) & B >= tau

  documents_wide
}
