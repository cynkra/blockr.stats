.onLoad <- function(libname, pkgname) {
  # nocov start

  # Only register if blockr.core is available
  if (requireNamespace("blockr.core", quietly = TRUE)) {
    register_stats_blocks()
  }

  invisible(NULL)
} # nocov end
