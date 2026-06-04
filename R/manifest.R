# manifest.R
# Helpers shared between write_run.R and qsc_code.R. Keeps run-directory
# naming and reproducibility metadata in one place.

#' Create a fresh per-run output directory.
#'
#' @param proj_root absolute path to the project root.
#' @return absolute path to the newly created directory.
new_run_directory <- function(proj_root) {
  stamp   <- format(Sys.time(), "%Y-%m-%dT%H-%M-%SZ", tz = "UTC")
  run_dir <- file.path(proj_root, "output", "runs", stamp)
  dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
  normalizePath(run_dir, winslash = "/")
}

#' Resolve a run_id + file name to an absolute path, guarding against traversal.
run_file_path <- function(proj_root, run_id, file_name) {
  safe_run  <- gsub("[^A-Za-z0-9_\\-]", "", run_id)
  safe_file <- gsub("[^A-Za-z0-9_\\.\\-]", "", file_name)
  file.path(proj_root, "output", "runs", safe_run, safe_file)
}
