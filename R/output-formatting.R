#' @keywords internal
new_htest_block <- function(server, ui, class, ctor = sys.parent(), ...) {
  new_block(server, ui, c(class, "htest_block", "transform_block"), ctor, ...)
}

#' @export
block_output.htest_block <- function(x, result, session) {
  format_decimals <- function(x) {
    ifelse(abs(x) >= 100, round(x),
      ifelse(abs(x) >= 10, round(x, 1),
        ifelse(abs(x) >= 1, round(x, 2),
          signif(x, 2))))
  }

  if (!is.null(result)) {
    cols <- names(result)[vapply(result, is.numeric, logical(1))]
    cols <- setdiff(cols, c("conf.level", "null.diff", "null.shift", "null.mean", "null.location"))
    result[cols] <- Map(format_decimals, result[cols])
  }

  NextMethod("block_output")
}

#' @export
block_ui.htest_block <- function(id, x, ...) {
  NextMethod("block_ui")
}
