# Placeholders inside bquote()/bbquote() call templates, not real bindings.
utils::globalVariables(c("data", "ty"))

.onLoad <- function(libname, pkgname) {
  # nocov start

  # The controls in inst/js/stats-controls.js send arrays as JSON, which
  # Shiny hands over as lists. Multi-selects want a character vector.
  shiny::registerInputHandler(
    "blockr.stats.control",
    function(x, shinysession, name) {
      if (is.list(x)) as.character(unlist(x)) else x
    },
    force = TRUE
  )

  # Only register if blockr.core is available
  if (requireNamespace("blockr.core", quietly = TRUE)) {
    register_stats_blocks()
  }

  invisible(NULL)
} # nocov end
