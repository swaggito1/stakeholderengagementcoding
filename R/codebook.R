# codebook.R
# Load and validate a QSC codebook from YAML.

# Force a UTF-8 locale once at load time. The YAML carries UTF-8 punctuation
# (em-dashes etc.) and yaml::read_yaml silently returns an empty list when R
# is in the default "C" locale — every entry point that sources this file
# inherits this setting.
invisible(tryCatch(
  Sys.setlocale("LC_ALL", "en_US.UTF-8"),
  warning = function(w) invisible(NULL)
))

suppressPackageStartupMessages({
  library(yaml)
  library(tibble)
  library(dplyr)
  library(digest)
})

# null-coalescing helper
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

#' Load a QSC codebook from disk.
#'
#' @param path Path to a YAML codebook. Defaults to the shipped codebook.
#' @return A list with elements `version`, `window`, `families`, `subcodes`
#'   (flattened tibble), and `dictionary` (a named list suitable for
#'   quanteda::dictionary()).
load_codebook <- function(path = NULL) {
  if (is.null(path)) {
    path <- file.path(
      dirname(dirname(sys.frame(1)$ofile %||% "")),
      "codebook", "qsc_codebook.yml"
    )
  }
  if (!file.exists(path)) {
    stop("Codebook not found at: ", path)
  }
  raw <- yaml::read_yaml(path)
  validate_codebook(raw)

  subcodes <- dplyr::bind_rows(lapply(raw$families, function(fam) {
    dplyr::bind_rows(lapply(fam$subcodes, function(sc) {
      tibble::tibble(
        family         = fam$code,
        family_label   = fam$label,
        subcode        = sc$code,
        col            = sc$col %||% NA_integer_,
        rule           = trimws(sc$rule %||% ""),
        threshold      = trimws(sc$threshold %||% ""),
        threshold_regex = sc$threshold_regex %||% NA_character_,
        keywords       = list(tolower(sc$keywords))
      )
    }))
  }))

  # Build a named list for quanteda::dictionary() — one entry per sub-code.
  dict_list <- setNames(
    lapply(subcodes$keywords, function(kws) as.character(kws)),
    subcodes$subcode
  )

  list(
    version    = raw$version %||% "0.0.0",
    window     = as.integer(raw$window %||% 10),
    path       = normalizePath(path, winslash = "/"),
    families   = vapply(raw$families, function(x) x$code, character(1)),
    subcodes   = subcodes,
    dictionary = dict_list
  )
}

#' Convert a codebook to a JSON-serialisable summary for the front-end.
codebook_summary <- function(cb) {
  families <- unique(cb$subcodes$family)
  lapply(families, function(f) {
    sub <- cb$subcodes[cb$subcodes$family == f, ]
    list(
      code  = f,
      label = sub$family_label[[1]],
      subcodes = lapply(seq_len(nrow(sub)), function(i) {
        list(
          code      = sub$subcode[i],
          col       = sub$col[i],
          rule      = sub$rule[i],
          threshold = sub$threshold[i],
          keywords  = sub$keywords[[i]]
        )
      })
    )
  })
}

#' SHA-256 of the codebook YAML file's contents (line-based, matching write_run.R).
codebook_sha256 <- function(path) {
  raw <- readLines(path, warn = FALSE)
  digest::digest(raw, algo = "sha256", serialize = FALSE)
}

validate_codebook <- function(raw) {
  if (is.null(raw$families) || length(raw$families) == 0) {
    stop("Codebook is missing `families:`")
  }
  for (fam in raw$families) {
    if (is.null(fam$code) || is.null(fam$subcodes)) {
      stop("Each family needs a `code` and `subcodes`.")
    }
    for (sc in fam$subcodes) {
      if (is.null(sc$code) || is.null(sc$keywords)) {
        stop("Sub-code ", sc$code %||% "<unnamed>",
             " is missing `code` or `keywords`.")
      }
      if (!is.null(sc$threshold_regex)) {
        ok <- tryCatch({
          grepl(sc$threshold_regex, "", perl = TRUE, ignore.case = TRUE)
          TRUE
        }, error = function(e) FALSE)
        if (!isTRUE(ok)) {
          stop("Invalid threshold_regex on sub-code ", sc$code, ": ",
               sc$threshold_regex)
        }
      }
    }
  }
  invisible(TRUE)
}
