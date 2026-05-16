#' Ensure a value is a reactiveVal
#' @noRd
as_rv <- function(x, default = x) {
  if (inherits(x, "reactiveVal")) x else shiny::reactiveVal(default)
}

#' Null coalescing operator
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x
