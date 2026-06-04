#!/usr/bin/env Rscript
# qsc_code.R — headless command-line runner for the QSC Coder.
#
# Codes a corpus of documents against the v2 codebook and writes a single,
# provenance-stamped run directory (CSVs + manifest.json + codebook snapshot).
# No browser and no HTTP server: this script is the canonical entry point.
#
# Usage:
#   Rscript qsc_code.R --csv <manifest.csv> --docs <dir> [options]
#
# Required:
#   --csv  <path>     corpus manifest CSV (doc_id, helix, tier, year, [path])
#   --docs <dir>      directory of referenced documents (.txt/.md/.docx/.pdf)
#
# Options:
#   --tau <num>       expect_heavy_soft threshold (default 0.50)
#   --out <dir>       run output directory (default output/runs/<UTC-stamp>)
#   --codebook <path> codebook YAML (default backend/codebook/qsc_codebook.yml)
#   --version         print tool version and exit
#   --help            print this help and exit
#
# Exit codes: 0 ok · 1 usage/validation error · 2 no documents staged.

TOOL_VERSION <- "2.0.0"

## ---- locate project root (directory of this script) ----------------------
arg_full  <- commandArgs(trailingOnly = FALSE)
file_arg  <- sub("^--file=", "", arg_full[grep("^--file=", arg_full)])
PROJ_ROOT <- if (length(file_arg) && nzchar(file_arg)) {
  normalizePath(dirname(file_arg), winslash = "/")
} else {
  normalizePath(getwd(), winslash = "/")
}

## ---- minimal argument parser ---------------------------------------------
args     <- commandArgs(trailingOnly = TRUE)
has_flag <- function(f) f %in% args
get_opt  <- function(key, default = NULL) {
  i <- which(args == key)
  if (length(i) == 0 || i[1] == length(args)) return(default)
  args[i[1] + 1]
}

print_usage <- function() {
  cat(
    "QSC Coder — headless runner\n\n",
    "Usage:\n",
    "  Rscript qsc_code.R --csv <manifest.csv> --docs <dir> [options]\n\n",
    "Required:\n",
    "  --csv  <path>     corpus manifest CSV (doc_id, helix, tier, year, [path])\n",
    "  --docs <dir>      directory of referenced documents (.txt/.md/.docx/.pdf)\n\n",
    "Options:\n",
    "  --tau <num>       expect_heavy_soft threshold (default 0.50)\n",
    "  --out <dir>       run output directory (default output/runs/<UTC-stamp>)\n",
    "  --codebook <path> codebook YAML (default backend/codebook/qsc_codebook.yml)\n",
    "  --version         print tool version and exit\n",
    "  --help            print this help and exit\n",
    sep = ""
  )
}

err <- function(msg, code = 1L) {
  message("ERROR: ", msg)
  quit(status = code, save = "no")
}

if (has_flag("--help"))    { print_usage(); quit(status = 0, save = "no") }
if (has_flag("--version")) { cat(TOOL_VERSION, "\n"); quit(status = 0, save = "no") }

csv_path <- get_opt("--csv")
docs_dir <- get_opt("--docs")
tau_chr  <- get_opt("--tau", "0.50")
out_dir  <- get_opt("--out", NULL)
cb_path  <- get_opt("--codebook",
                    file.path(PROJ_ROOT, "backend", "codebook", "qsc_codebook.yml"))

tau <- suppressWarnings(as.numeric(tau_chr))
if (is.null(csv_path)) { print_usage(); err("--csv is required.") }
if (is.null(docs_dir)) { print_usage(); err("--docs is required.") }
if (!file.exists(csv_path)) err(paste0("CSV not found: ", csv_path))
if (!dir.exists(docs_dir))  err(paste0("Docs directory not found: ", docs_dir))
if (!file.exists(cb_path))  err(paste0("Codebook not found: ", cb_path))
if (is.na(tau))             err("--tau must be numeric.")

## ---- load engine modules (dependency order) ------------------------------
mod <- function(f) source(file.path(PROJ_ROOT, "backend", "R", f), local = FALSE)
suppressPackageStartupMessages({
  mod("codebook.R");  mod("ingest.R");      mod("validate.R")
  mod("imbalance.R"); mod("standards.R");   mod("specificity.R")
  mod("cooc.R");      mod("pipeline.R");    mod("manifest.R")
  mod("coder.R");     mod("write_run.R")
})

## ---- read + validate the corpus manifest ---------------------------------
manifest_csv <- tryCatch(
  readr::read_csv(csv_path, show_col_types = FALSE),
  error = function(e) err(paste0("Could not read CSV: ", conditionMessage(e)))
)

v <- validate_manifest(manifest_csv)
if (length(v$warnings) > 0) for (w in v$warnings) message("WARNING: ", w)
if (length(v$errors) > 0) {
  for (e in v$errors) message("REJECTED: ", e)
  err("Manifest validation failed.", 1L)
}

## ---- stage documents from disk -------------------------------------------
staged <- stage_from_dir(manifest_csv, docs_dir)
if (nrow(staged) == 0) err("No documents could be matched to manifest rows.", 2L)
if (nrow(staged) < nrow(manifest_csv)) {
  message(sprintf("NOTE: staged %d of %d manifest rows (see warnings above).",
                  nrow(staged), nrow(manifest_csv)))
}

## ---- code ----------------------------------------------------------------
cb      <- load_codebook(cb_path)
config  <- list(expect_heavy_threshold = tau, specificity_window = cb$window)
results <- code_corpus(staged, cb, config)

## ---- write the run -------------------------------------------------------
if (is.null(out_dir)) {
  run_dir <- new_run_directory(PROJ_ROOT)
} else {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  run_dir <- normalizePath(out_dir, winslash = "/")
}

manifest <- write_run(run_dir, results, manifest_csv, config,
                      codebook_path = cb_path, tool_version = TOOL_VERSION)

# the two auxiliary CSVs the canonical writer does not handle
readr::write_csv(results$cooc,     file.path(run_dir, "cooc_matrix.csv"))
readr::write_csv(results$pipeline, file.path(run_dir, "pipeline_completeness.csv"))

## ---- summary -------------------------------------------------------------
B        <- results$documents_wide$B
B        <- B[!is.na(B)]
median_B <- if (length(B)) round(stats::median(B), 3) else NA_real_
n_strict <- sum(results$documents_wide$expect_heavy_strict, na.rm = TRUE)
n_soft   <- sum(results$documents_wide$expect_heavy_soft,   na.rm = TRUE)

cat("\n=== QSC Coder run complete ===\n")
cat("  run_id           : ", basename(run_dir), "\n", sep = "")
cat("  output           : ", run_dir, "\n", sep = "")
cat("  documents        : ", manifest$n_documents, "\n", sep = "")
cat("  qualified hits   : ", manifest$n_hits_total, "\n", sep = "")
cat("  families present : ", paste(manifest$families_present, collapse = ", "), "\n", sep = "")
cat("  median B         : ", ifelse(is.na(median_B), "NA", median_B), "\n", sep = "")
cat(sprintf("  expect_heavy     : strict=%d  soft(tau=%.2f)=%d\n", n_strict, tau, n_soft))
cat("  codebook_sha256  : ", manifest$codebook_sha256, "\n", sep = "")
cat("\nFiles written:\n")
for (f in sort(list.files(run_dir))) cat("  - ", f, "\n", sep = "")

quit(status = 0, save = "no")
