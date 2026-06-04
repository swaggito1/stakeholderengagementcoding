# coder.R
# Dictionary-based content-analysis engine for QSC Coder v2.
#
# Engine is identical to v1: build a quanteda dictionary, tokenise, run
# kwic() against the dictionary, apply each sub-code's threshold_regex to
# the KWIC window, retain qualified hits. What changed in v2 is the return
# shape and the post-aggregation steps (imbalance, standards, specificity,
# co-occurrence, pipeline completeness).
#
# kwic() gotcha preserved: pass the dictionary directly. Wrapping in phrase()
# would put the matched keyword in `pattern` instead of the dictionary key
# (sub-code name) and break downstream linkage.

suppressPackageStartupMessages({
  library(quanteda)
  library(dplyr)
  library(tibble)
  library(stringr)
})

#' Run the coder over a staged corpus and return v2's structured result.
#'
#' @param corpus_df tibble(doc_id, text, helix, tier, year).
#' @param codebook  object returned by load_codebook().
#' @param config    list with at least `expect_heavy_threshold` (numeric,
#'                  default 0.50).
#' @return list with:
#'   $hits             tibble(doc_id, family, sub_code, keyword,
#'                            kwic_left, kwic_match, kwic_right,
#'                            threshold_match, specificity_score, page)
#'   $documents_wide   tibble(doc_id, helix, tier, year, n_tokens,
#'                            n_<SUBCODE>..., n_EXP, n_INST,
#'                            mentions_named_standard, n_standards,
#'                            specificity_mean, delta, B,
#'                            expect_heavy_strict, expect_heavy_soft)
#'   $documents_long   tibble(doc_id, family, sub_code, count,
#'                            helix, tier, year)
#'   $cooc             tibble(family_a, family_b, cooc_count, jaccard,
#'                            share_a_with_b)
#'   $pipeline         tibble(doc_id, helix, tier, year, has_EXP, has_INST)
code_corpus <- function(corpus_df, codebook, config = list()) {
  stopifnot(all(c("doc_id", "text") %in% names(corpus_df)))
  tau <- as.numeric(config$expect_heavy_threshold %||% 0.50)

  all_subcodes <- codebook$subcodes$subcode
  all_families <- unique(codebook$subcodes$family)

  if (nrow(corpus_df) == 0) {
    return(empty_result(corpus_df, codebook, tau))
  }

  # --- 1. tokenise -----------------------------------------------------------
  dict <- quanteda::dictionary(codebook$dictionary)

  corp <- quanteda::corpus(corpus_df$text, docnames = corpus_df$doc_id)
  toks <- quanteda::tokens(
    corp,
    remove_punct  = FALSE,
    remove_symbols = TRUE,
    remove_numbers = FALSE
  )
  toks <- quanteda::tokens_tolower(toks)

  n_tokens <- as.integer(quanteda::ntoken(toks))
  names(n_tokens) <- corpus_df$doc_id

  # --- 2. kwic --------------------------------------------------------------
  kw <- quanteda::kwic(
    toks,
    pattern   = dict,
    window    = codebook$window,
    separator = " "
  )

  if (nrow(kw) == 0) {
    return(empty_result(corpus_df, codebook, tau, n_tokens))
  }

  hits_raw <- tibble::as_tibble(as.data.frame(kw, stringsAsFactors = FALSE)) |>
    dplyr::mutate(
      doc_id    = as.character(docname),
      sub_code  = as.character(pattern),
      keyword   = as.character(keyword),
      kwic_left = as.character(pre),
      kwic_match = as.character(keyword),
      kwic_right = as.character(post)
    ) |>
    dplyr::select(doc_id, sub_code, keyword, kwic_left, kwic_match, kwic_right)

  # --- 3. join family + threshold_regex -------------------------------------
  meta <- codebook$subcodes |>
    dplyr::select(sub_code = subcode, family, threshold_regex)

  hits_raw <- dplyr::left_join(hits_raw, meta, by = "sub_code")
  hits_raw$window_text <- paste(hits_raw$kwic_left,
                                hits_raw$kwic_match,
                                hits_raw$kwic_right)

  # --- 4. apply threshold ---------------------------------------------------
  hits_raw$threshold_match <- vapply(seq_len(nrow(hits_raw)), function(i) {
    rgx <- hits_raw$threshold_regex[i]
    if (is.na(rgx) || !nzchar(rgx)) return(TRUE)
    grepl(rgx, hits_raw$window_text[i], perl = TRUE, ignore.case = TRUE)
  }, logical(1))

  hits <- hits_raw[hits_raw$threshold_match, , drop = FALSE]
  hits$page <- NA_integer_

  # --- 5. specificity (EXP only) --------------------------------------------
  hits <- compute_specificity(hits)

  # --- 6. tidy hits to the write_run.R schema -------------------------------
  hits_out <- tibble::tibble(
    doc_id            = hits$doc_id,
    family            = hits$family,
    sub_code          = hits$sub_code,
    keyword           = hits$keyword,
    kwic_left         = hits$kwic_left,
    kwic_match        = hits$kwic_match,
    kwic_right        = hits$kwic_right,
    threshold_match   = hits$threshold_match,
    specificity_score = hits$specificity_score,
    page              = hits$page
  )

  # --- 7. documents_long: one row per (doc, family, sub_code) ---------------
  counts <- hits_out |>
    dplyr::count(doc_id, family, sub_code, name = "count")

  grid <- expand.grid(
    doc_id   = corpus_df$doc_id,
    sub_code = all_subcodes,
    stringsAsFactors = FALSE
  ) |>
    dplyr::left_join(
      codebook$subcodes |> dplyr::select(sub_code = subcode, family),
      by = "sub_code"
    )

  documents_long <- grid |>
    dplyr::left_join(counts, by = c("doc_id", "family", "sub_code")) |>
    dplyr::mutate(count = ifelse(is.na(count), 0L, as.integer(count))) |>
    dplyr::left_join(
      corpus_df |> dplyr::select(doc_id, helix, tier, year),
      by = "doc_id"
    ) |>
    dplyr::select(doc_id, family, sub_code, count, helix, tier, year)

  # --- 8. documents_wide ----------------------------------------------------
  per_subcode <- documents_long |>
    tidyr::pivot_wider(
      id_cols     = doc_id,
      names_from  = sub_code,
      values_from = count,
      names_prefix = "n_"
    )
  # ensure every sub-code column exists, in codebook order
  for (sc in all_subcodes) {
    col <- paste0("n_", sc)
    if (!col %in% names(per_subcode)) per_subcode[[col]] <- 0L
  }
  per_subcode <- per_subcode[, c("doc_id", paste0("n_", all_subcodes))]

  per_family <- documents_long |>
    dplyr::group_by(doc_id, family) |>
    dplyr::summarise(n = sum(count), .groups = "drop") |>
    tidyr::pivot_wider(
      id_cols     = doc_id,
      names_from  = family,
      values_from = n,
      names_prefix = "n_"
    )
  for (fam in all_families) {
    col <- paste0("n_", fam)
    if (!col %in% names(per_family)) per_family[[col]] <- 0L
  }
  per_family <- per_family[, c("doc_id", paste0("n_", all_families))]

  # specificity_mean per doc (mean over EXP hits, NA_real_ if none)
  spec_mean <- hits_out |>
    dplyr::filter(family == "EXP") |>
    dplyr::group_by(doc_id) |>
    dplyr::summarise(specificity_mean = mean(specificity_score, na.rm = TRUE),
                     .groups = "drop")

  documents_wide <- tibble::tibble(
    doc_id   = corpus_df$doc_id,
    helix    = corpus_df$helix,
    tier     = corpus_df$tier,
    year     = corpus_df$year,
    n_tokens = unname(n_tokens[corpus_df$doc_id])
  ) |>
    dplyr::left_join(per_subcode, by = "doc_id") |>
    dplyr::left_join(per_family,  by = "doc_id") |>
    dplyr::left_join(spec_mean,   by = "doc_id")

  # zero-fill the count columns left as NA by the joins
  count_cols <- c(paste0("n_", all_subcodes), paste0("n_", all_families))
  for (col in count_cols) {
    documents_wide[[col]][is.na(documents_wide[[col]])] <- 0L
    documents_wide[[col]] <- as.integer(documents_wide[[col]])
  }

  # --- 9. standards rollup (folded into documents_wide) ---------------------
  std <- compute_standards(hits_out)
  documents_wide <- documents_wide |>
    dplyr::left_join(std, by = "doc_id") |>
    dplyr::mutate(
      n_standards             = ifelse(is.na(n_standards), 0L, as.integer(n_standards)),
      mentions_named_standard = ifelse(is.na(mentions_named_standard), FALSE, mentions_named_standard)
    )

  # --- 10. imbalance (delta, B, two flags) ----------------------------------
  documents_wide <- compute_imbalance(documents_wide, tau = tau)

  # --- 11. cooc + pipeline --------------------------------------------------
  cooc     <- compute_cooc(documents_wide)
  pipeline <- compute_pipeline_completeness(documents_wide)

  list(
    hits           = hits_out,
    documents_wide = documents_wide,
    documents_long = documents_long,
    cooc           = cooc,
    pipeline       = pipeline
  )
}

empty_result <- function(corpus_df, codebook, tau = 0.50, n_tokens = NULL) {
  all_subcodes <- codebook$subcodes$subcode
  all_families <- unique(codebook$subcodes$family)
  doc_ids <- if (is.null(corpus_df) || nrow(corpus_df) == 0) character(0) else corpus_df$doc_id

  documents_wide <- tibble::tibble(
    doc_id   = doc_ids,
    helix    = if (length(doc_ids)) corpus_df$helix else character(0),
    tier     = if (length(doc_ids)) corpus_df$tier  else character(0),
    year     = if (length(doc_ids)) corpus_df$year  else integer(0),
    n_tokens = if (is.null(n_tokens)) rep(0L, length(doc_ids))
               else unname(n_tokens[doc_ids])
  )
  for (sc in all_subcodes) documents_wide[[paste0("n_", sc)]] <- rep(0L, length(doc_ids))
  for (fam in all_families) documents_wide[[paste0("n_", fam)]] <- rep(0L, length(doc_ids))
  documents_wide$mentions_named_standard <- rep(FALSE, length(doc_ids))
  documents_wide$n_standards             <- rep(0L, length(doc_ids))
  documents_wide$specificity_mean        <- rep(NA_real_, length(doc_ids))
  documents_wide <- compute_imbalance(documents_wide, tau = tau)

  documents_long <- expand.grid(
    doc_id   = doc_ids,
    sub_code = all_subcodes,
    stringsAsFactors = FALSE
  ) |>
    dplyr::left_join(
      codebook$subcodes |> dplyr::select(sub_code = subcode, family),
      by = "sub_code"
    ) |>
    dplyr::mutate(count = 0L) |>
    dplyr::left_join(
      tibble::tibble(doc_id = doc_ids,
                     helix  = if (length(doc_ids)) corpus_df$helix else character(0),
                     tier   = if (length(doc_ids)) corpus_df$tier  else character(0),
                     year   = if (length(doc_ids)) corpus_df$year  else integer(0)),
      by = "doc_id"
    ) |>
    dplyr::select(doc_id, family, sub_code, count, helix, tier, year)

  list(
    hits = tibble::tibble(
      doc_id = character(), family = character(), sub_code = character(),
      keyword = character(), kwic_left = character(), kwic_match = character(),
      kwic_right = character(), threshold_match = logical(),
      specificity_score = integer(), page = integer()
    ),
    documents_wide = documents_wide,
    documents_long = documents_long,
    cooc           = compute_cooc(documents_wide),
    pipeline       = compute_pipeline_completeness(documents_wide)
  )
}

# null-coalescing helper if codebook.R was not sourced first
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
}
