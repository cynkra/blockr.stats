#' Per-variable descriptive statistics (base R)
#'
#' One row per numeric variable: n, n_missing, mean, sd, median, q1,
#' q3, min, max. No easystats dependency.
#'
#' @param data A data frame.
#' @param vars Numeric column names to summarise (default: all numeric).
#' @return A tidy data frame.
#' @examples
#' describe_numeric(mtcars, vars = c("mpg", "hp", "wt"))
#' @export
describe_numeric <- function(data, vars = NULL) {
  if (is.null(vars) || length(vars) == 0) {
    vars <- names(data)[vapply(data, is.numeric, logical(1L))]
  }
  vars <- intersect(vars, names(data))
  vars <- vars[vapply(data[vars], is.numeric, logical(1L))]
  if (length(vars) == 0L) {
    return(data.frame(message = "Select numeric variable(s)",
                      stringsAsFactors = FALSE))
  }
  rows <- lapply(vars, function(v) {
    x <- data[[v]]
    n_miss <- sum(is.na(x))
    x <- x[!is.na(x)]
    q <- if (length(x)) stats::quantile(x, c(0.25, 0.5, 0.75),
                                        names = FALSE) else rep(NA_real_, 3)
    data.frame(
      variable  = v,
      n         = length(x),
      n_missing = n_miss,
      mean      = if (length(x)) mean(x) else NA_real_,
      sd        = if (length(x) > 1L) stats::sd(x) else NA_real_,
      median    = q[2L],
      q1        = q[1L],
      q3        = q[3L],
      min       = if (length(x)) min(x) else NA_real_,
      max       = if (length(x)) max(x) else NA_real_,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

# NOTE (under review): usefulness debated. Convenience block; its per-variable
# summary largely overlaps dplyr `summarise(across(where(is.numeric), ...))`.
# Kept for now as a common first-step block; candidate for removal.
#' Descriptives Block
#'
#' Transform block wrapping [describe_numeric()]. Emits a tidy data
#' frame (one row per variable) for the generic renderers.
#'
#' @param vars Numeric column names to summarise.
#' @param ... Forwarded to [blockr.core::new_transform_block()].
#' @return A `descriptives_block` transform block.
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   serve(new_descriptives_block(), data = list(data = mtcars))
#' }
#' @export
new_descriptives_block <- function(vars = character(), ...) {
  lifecycle::deprecate_soft(
    "0.0.0",
    "new_descriptives_block()",
    details = paste(
      "Unregistered; use blockr.bi::new_summary_table_block (mixed-type,",
      "by-group, tidy). Constructor kept so existing boards still load."
    )
  )
  new_transform_block(
    server = function(id, data) {
      moduleServer(id, function(input, output, session) {
        r_vars <- reactiveVal(vars)
        r_initialized <- reactiveVal(FALSE)

        observeEvent(input$vars, r_vars(input$vars))

        observe({
          if (!r_initialized() && length(colnames(data())) > 0) {
            d <- data()
            num_cols <- colnames(d)[vapply(d, is.numeric, logical(1))]
            updateSelectizeInput(session, "vars",
              choices = num_cols, selected = r_vars())
            r_initialized(TRUE)
          }
        })

        observeEvent(colnames(data()), {
          if (r_initialized()) {
            req(data())
            d <- data()
            num_cols <- colnames(d)[vapply(d, is.numeric, logical(1))]
            new_vars <- intersect(r_vars(), num_cols)
            r_vars(new_vars)
            updateSelectizeInput(session, "vars",
              choices = num_cols, selected = new_vars)
          }
        }, ignoreNULL = FALSE)

        list(
          expr = reactive({
            v <- r_vars()
            v <- v[nzchar(v)]
            bquote(
              blockr.stats::describe_numeric(data, vars = .(v)),
              list(v = v)
            )
          }),
          state = list(vars = r_vars)
        )
      })
    },
    ui = function(id) {
      tagList(
        div(
          class = "block-container",
          selectizeInput(
            NS(id, "vars"),
            label = "Variables",
            choices = vars,
            selected = vars,
            multiple = TRUE,
            width = "100%",
            options = list(
              plugins = list("drag_drop", "remove_button"),
              placeholder = "Pick numeric variables..."
            )
          )
        )
      )
    },
    class = "descriptives_block",
    allow_empty_state = "vars",
    ...
  )
}
