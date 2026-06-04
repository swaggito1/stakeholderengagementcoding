# install_deps.R
# Run once:  Rscript install_deps.R
# Installs every package the QSC Coder needs. Headless: no web-server or
# upload-decoding dependencies.

required <- c(
  "quanteda",             # dictionary content analysis (Benoit et al. 2018, JOSS)
  "quanteda.textstats",   # KWIC + frequency stats
  "readtext",             # read .docx
  "pdftools",             # read .pdf
  "yaml",                 # codebook config
  "jsonlite",             # JSON serialisation (manifest, run twin)
  "dplyr",                # data wrangling
  "tidyr",                # pivot_wider for documents.csv
  "stringr",              # threshold regex
  "tibble",               # tidy data frames
  "readr",                # CSV read/write
  "digest",               # codebook sha256 in manifest
  "tools",                # file ext (base, usually present)
  "testthat"              # unit tests
)

installed   <- rownames(installed.packages())
to_install  <- setdiff(required, installed)

if (length(to_install) > 0) {
  message("Installing: ", paste(to_install, collapse = ", "))
  install.packages(to_install, repos = "https://cloud.r-project.org")
} else {
  message("All required packages already installed.")
}

invisible(lapply(required, function(p) {
  suppressPackageStartupMessages(library(p, character.only = TRUE))
}))

message("\nAll dependencies loaded.")
message("Verify the codebook:  Rscript scripts/lint_codebook.R")
message("Run the tests:        Rscript -e 'testthat::test_dir(\"tests\")'")
message("Code a corpus:        Rscript qsc_code.R --csv <manifest.csv> --docs <dir>")
