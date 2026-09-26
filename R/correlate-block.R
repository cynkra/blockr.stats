#' Pairwise correlation matrix (base R)
#'
#' Correlation matrix of the selected numeric columns, returned as a
#' tidy data frame: a leading `var` character column (the row variable)
#' plus one numeric column per selected variable. Values are rounded
#' correlations. Uses `use = "pairwise.complete.obs"`. Renders directly
#' as a heatmap via the table block (`rowname = "var"` + a diverging
#' `cell_color`).
#'
#' @param data A data frame.
#' @param vars Numeric column names to correlate (default: all numeric).
#' @param method One of `"pearson"`, `"spearman"`, `"kendall"`.
#' @param digits Decimal places to round the correlations to.
#' @return A tidy correlation-matrix data frame.
#' @examples
#' correlate_numeric(mtcars, vars = c("mpg", "hp", "wt"))
#' @export
correlate_numeric <- function(data, vars = NULL, method = "pearson",
                              digits = 2L) {
  if (is.null(vars) || length(vars) == 0) {
    vars <- names(data)[vapply(data, is.numeric, logical(1L))]
  }
  vars <- intersect(vars, names(data))
  vars <- vars[vapply(data[vars], is.numeric, logical(1L))]
  if (length(vars) < 2L) {
    return(data.frame(message = "Select >= 2 numeric variables",
                      stringsAsFactors = FALSE))
  }
  m <- stats::cor(data[, vars, drop = FALSE],
                  use = "pairwise.complete.obs", method = method)
  m <- round(m, digits)
  out <- as.data.frame(m, stringsAsFactors = FALSE, check.names = FALSE)
  tibble::rownames_to_column(out, "var")
}

#' Correlate Block
#'
#' Transform block wrapping [correlate_numeric()]. Emits a tidy
#' correlation matrix (a `var` row-label column plus one numeric column
#' per selected variable) for the generic renderers; feed it to the
#' table block with `rowname = "var"` and a diverging `cell_color` for a
#' heatmap.
#'
#' @param vars Numeric column names to correlate.
#' @param method One of `"pearson"`, `"spearman"`, `"kendall"`.
#' @param ... Forwarded to [blockr.core::new_transform_block()].
#' @return A `correlate_block` transform block.
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   serve(new_correlate_block(), data = list(data = mtcars))
#' }
#' @export
new_correlate_block <- function(vars = character(), method = "pearson", ...) {
  new_transform_block(
    server = function(id, data) {
      moduleServer(id, function(input, output, session) {
        r_vars <- reactiveVal(vars)
        r_method <- reactiveVal(method)
        r_initialized <- reactiveVal(FALSE)

        observeEvent(input$vars, r_vars(input$vars))
        observeEvent(input$method, r_method(input$method))

        observe({
          if (!r_initialized() && length(colnames(data())) > 0) {
            d <- data()
            num_cols <- colnames(d)[vapply(d, is.numeric, logical(1))]
            update_control(session, "vars", choices = num_cols,
                           selected = r_vars())
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
            update_control(session, "vars", choices = num_cols,
                           selected = new_vars)
          }
        }, ignoreNULL = FALSE)

        list(
          expr = reactive({
            v <- r_vars()
            v <- v[nzchar(v)]
            m <- r_method()
            bquote(
              blockr.stats::correlate_numeric(data, vars = .(v), method = .(m)),
              list(v = v, m = m)
            )
          }),
          state = list(vars = r_vars, method = r_method)
        )
      })
    },
    ui = function(id) {
      tagList(
        stats_controls_dep(),
        div(
          class = "block-container blockr-stats-face",
          multi_field(
            NS(id, "vars"), "Variables",
            choices = vars, selected = vars,
            placeholder = "Pick numeric variables"
          ),
          segmented_field(
            NS(id, "method"), "Method",
            choices = c(Pearson = "pearson", Spearman = "spearman",
                        Kendall = "kendall"),
            selected = method
          )
        )
      )
    },
    class = "correlate_block",
    allow_empty_state = "vars",
    ...
  )
}
