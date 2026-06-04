#' Formula-input widget: HTML dependency
#'
#' JS + CSS for the formula-input widget. Exported so other blockr packages can
#' embed the widget (mirrors `blockr.dplyr::filter_block_dep()`). Version is
#' pinned to the package version so editing `inst/js` busts the browser cache
#' after a Version bump + reinstall.
#'
#' @return An [htmltools::tagList] of [htmltools::htmlDependency] objects.
#' @export
formula_input_dep <- function() {
  htmltools::tagList(
    htmltools::htmlDependency(
      name = "formula-input-js",
      version = utils::packageVersion("blockr.stats"),
      src = system.file("js", package = "blockr.stats"),
      script = "formula-input.js"
    ),
    htmltools::htmlDependency(
      name = "formula-input-css",
      version = utils::packageVersion("blockr.stats"),
      src = system.file("css", package = "blockr.stats"),
      stylesheet = "formula-input.css"
    )
  )
}

#' Formula-input widget: UI container
#'
#' Pulls the shared blockr JS primitives (from blockr.dplyr) and the
#' formula-input dependency, then the namespaced container the JS binds to.
#'
#' @param id Module id (namespace).
#' @return A UI tagList.
#' @export
formula_input_ui <- function(id) {
  htmltools::tagList(
    blockr.dplyr::blockr_core_js_dep(),
    blockr.dplyr::blockr_blocks_css_dep(),
    blockr.dplyr::blockr_select_dep(),
    blockr.dplyr::blockr_input_dep(),
    formula_input_dep(),
    shiny::div(
      id = shiny::NS(id, "formula_input"),
      class = "formula-input-container"
    )
  )
}

#' Lightweight typed-column summary for the formula-input JS
#' @keywords internal
#' @noRd
build_formula_columns <- function(df) {
  lapply(colnames(df), function(col) {
    v <- df[[col]]
    type <- if (is.numeric(v)) {
      "numeric"
    } else if (is.factor(v) || is.ordered(v)) {
      "factor"
    } else if (is.logical(v)) {
      "logical"
    } else {
      "character"
    }
    lbl <- attr(v, "label", exact = TRUE)
    list(name = col, type = type, label = if (is.null(lbl)) col else lbl)
  })
}

#' Map a data frame's columns to a name -> class list (for `classify_term()`)
#' @keywords internal
#' @noRd
formula_col_types <- function(df) {
  as.list(vapply(df, function(x) class(x)[1L], character(1)))
}

#' Normalise a formula state before `sendCustomMessage` (auto-unbox guard)
#'
#' `sendCustomMessage` serialises with `auto_unbox = TRUE`, collapsing length-1
#' vectors to JSON scalars. Wrap the array-valued fields in `as.list()` so they
#' always emit JSON arrays.
#' @keywords internal
#' @noRd
normalize_formula_for_js <- function(state) {
  if (is.null(state)) {
    return(state)
  }
  state$terms <- lapply(state$terms %||% list(), function(t) {
    if (!is.null(t$vars)) t$vars <- as.list(t$vars)
    t
  })
  if (is.list(state$response) && !is.null(state$response$args)) {
    state$response$args <- as.list(state$response$args)
  }
  state
}

# small null-coalesce used locally (not relying on base/imported %||%)
`%||%` <- function(x, y) if (is.null(x)) y else x
