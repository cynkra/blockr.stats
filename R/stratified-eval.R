#' Apply a function with optional group-by stratification
#'
#' Splits data by grouping columns, applies the function to each subset,
#' and binds the results.
#'
#' @param data A data.frame.
#' @param by_cols Character vector of column names to group by.
#' @param fn Function to apply. Signature: fn(data, values, groups, params).
#' @param ... Additional arguments passed to fn.
#'
#' @return A data.frame of combined results.
#' @noRd
stratified_eval <- function(data, by_cols, fn, ...) {
  if (length(by_cols) == 0 || all(by_cols == "")) {
    return(fn(data, ...))
  }

  groups <- split(data, data[by_cols], drop = TRUE)

  results <- lapply(seq_along(groups), function(i) {
    group_data <- groups[[i]]
    result <- fn(group_data, ...)

    if (nrow(result) > 0) {
      group_labels <- group_data[1, by_cols, drop = FALSE]
      for (col in by_cols) {
        col_name <- paste0("by.", col)
        args <- stats::setNames(
          list(as.character(group_labels[[col]])),
          col_name
        )
        result <- do.call(
          tibble::add_column,
          c(list(.data = result), args, list(.after = "method"))
        )
      }
    }
    result
  })

  dplyr::bind_rows(results)
}
