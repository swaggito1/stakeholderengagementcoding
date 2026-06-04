# write_run.R — single output writer for QSC dictionary-coder v2.
#
# One function. Takes the structured results object from code_corpus() and
# writes every output file for the run. Keeps the schema in one place so the
# manifest and the CSVs cannot drift apart.
#
# Usage (from qsc_code.R, after coding completes):
#
#   write_run(
#     run_dir       = run_dir,           # e.g. "output/runs/2026-05-08T14-12-03Z/"
#     results       = results,           # named list — see fields below
#     manifest_csv  = manifest_csv,      # the parsed input CSV (corpus manifest)
#     config        = config             # the run configuration list
#   )
#
# Required fields in `results`:
#   $hits             tibble: doc_id, family, sub_code, keyword,
#                            kwic_left, kwic_match, kwic_right,
#                            threshold_match, specificity_score, page
#   $documents_wide   tibble: doc_id, helix, tier, year, n_tokens,
#                            n_<FAMILY> ..., n_<SUB_CODE> ...,
#                            n_EXP, n_INST, delta, B,
#                            expect_heavy_strict, expect_heavy_soft
#   $documents_long   tibble: doc_id, family, sub_code, count,
#                            helix, tier, year
#
# Side-effects:
#   creates run_dir if missing
#   writes manifest.json, codebook_used.yml, request fingerprint, and all CSVs
#   returns the manifest list invisibly for upstream inspection
#
# Pure-ish: only file I/O + jsonlite/yaml/readr serialisation; no model calls.

suppressPackageStartupMessages({
  library(readr)
  library(jsonlite)
  library(yaml)
  library(digest)
})

write_run <- function(run_dir, results, manifest_csv, config,
                      codebook_path = "backend/codebook/qsc_codebook.yml",
                      tool_version  = "2.0.0") {

  # ---------- 0. ensure run_dir exists -------------------------------------
  if (!dir.exists(run_dir)) dir.create(run_dir, recursive = TRUE)

  # ---------- 1. snapshot the codebook in use ------------------------------
  stopifnot(file.exists(codebook_path))
  codebook_raw  <- readLines(codebook_path, warn = FALSE)
  codebook_sha  <- digest::digest(codebook_raw, algo = "sha256",
                                  serialize = FALSE)
  writeLines(codebook_raw, file.path(run_dir, "codebook_used.yml"))

  # ---------- 2. build the manifest ---------------------------------------
  manifest <- list(
    run_id            = basename(run_dir),
    timestamp_utc     = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    tool_version      = tool_version,
    r_version         = R.version.string,
    package_versions  = as.list(installed_package_versions(c(
                          "quanteda", "quanteda.textstats",
                          "readtext", "pdftools", "yaml",
                          "jsonlite", "dplyr", "stringr",
                          "readr", "digest"))),
    git_commit        = git_commit_or_na(),
    codebook_sha256   = codebook_sha,
    codebook_path     = codebook_path,
    config            = config,
    n_documents       = nrow(manifest_csv),
    n_hits_total      = nrow(results$hits),
    families_present  = sort(unique(results$hits$family))
  )
  jsonlite::write_json(manifest,
                       file.path(run_dir, "manifest.json"),
                       auto_unbox = TRUE, pretty = TRUE)

  # ---------- 3. write every CSV in one place -----------------------------
  csv_targets <- list(
    "hits.csv"           = results$hits,
    "documents.csv"      = results$documents_wide,
    "documents_long.csv" = results$documents_long
  )

  for (filename in names(csv_targets)) {
    df <- csv_targets[[filename]]
    if (is.null(df) || nrow(df) == 0) {
      # write an empty file with headers so reviewers see the schema
      readr::write_csv(empty_with_schema(filename), file.path(run_dir, filename))
    } else {
      readr::write_csv(df, file.path(run_dir, filename))
    }
  }

  # ---------- 4. write the JSON twin (for Cowork interpretive refinement) -
  json_payload <- c(list(manifest = manifest), csv_targets)
  jsonlite::write_json(json_payload,
                       file.path(run_dir, sprintf("qsc_run_%s.json",
                                                  manifest$run_id)),
                       auto_unbox = TRUE, na = "null", pretty = FALSE)

  invisible(manifest)
}

# ---- helpers --------------------------------------------------------------

installed_package_versions <- function(pkgs) {
  setNames(
    vapply(pkgs, function(p)
      tryCatch(as.character(utils::packageVersion(p)),
               error = function(e) NA_character_),
      character(1)),
    pkgs
  )
}

git_commit_or_na <- function() {
  out <- tryCatch(
    system("git rev-parse HEAD", intern = TRUE, ignore.stderr = TRUE),
    error = function(e) NA_character_
  )
  if (length(out) == 0) NA_character_ else out[[1]]
}

empty_with_schema <- function(filename) {
  schemas <- list(
    "hits.csv" = data.frame(
      doc_id            = character(),
      family            = character(),
      sub_code          = character(),
      keyword           = character(),
      kwic_left         = character(),
      kwic_match        = character(),
      kwic_right        = character(),
      threshold_match   = logical(),
      specificity_score = integer(),
      page              = integer(),
      stringsAsFactors  = FALSE),
    "documents.csv" = data.frame(
      doc_id              = character(),
      helix               = character(),
      tier                = character(),
      year                = integer(),
      n_tokens            = integer(),
      n_EXP               = integer(),
      n_INST              = integer(),
      delta               = integer(),
      B                   = numeric(),
      expect_heavy_strict = logical(),
      expect_heavy_soft   = logical(),
      stringsAsFactors    = FALSE),
    "documents_long.csv" = data.frame(
      doc_id   = character(),
      family   = character(),
      sub_code = character(),
      count    = integer(),
      helix    = character(),
      tier     = character(),
      year     = integer(),
      stringsAsFactors = FALSE)
  )
  schemas[[filename]] %||% data.frame()
}

`%||%` <- function(a, b) if (is.null(a)) b else a
