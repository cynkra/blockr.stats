#' Pairwise correlation matrix (long, tidy)
#'
#' `stats::cor()` over the numeric columns, emitted long
#' (`var_x, var_y, r`) so the generic drilldown table/heatmap renders
#' it. The one matrix-valued step base R has no tidy verb for.
#'
#' @param data A data frame.
#' @param method `"pearson"`, `"spearman"`, or `"kendall"`.
#' @param max_cols Cap on numeric columns (default 20).
#' @return Long tidy data frame, or a one-row message frame.
#' @export
correlation_matrix <- function(data, method = "pearson", max_cols = 20L) {
  num <- vapply(data, is.numeric, logical(1L))
  cols <- names(data)[num]
  if (length(cols) < 2L) {
    return(data.frame(message = "Need >= 2 numeric columns",
                      stringsAsFactors = FALSE))
  }
  if (length(cols) > max_cols) cols <- cols[seq_len(max_cols)]
  m <- stats::cor(data[, cols, drop = FALSE], method = method,
                  use = "pairwise.complete.obs")
  m <- round(m, 3L)
  g <- expand.grid(var_x = cols, var_y = cols,
                   KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  g$r <- mapply(function(a, b) m[a, b], g$var_x, g$var_y)
  g
}

#' Correlation Matrix Block
#'
#' Transform block wrapping [correlation_matrix()]. Emits the long
#' correlation frame for the generic renderers (diverging-coloured
#' drilldown table / heatmap).
#'
#' @param method,max_cols Forwarded to [correlation_matrix()].
#' @param ... Forwarded to [new_transform_block()].
#' @return A transform block of class `correlation_matrix_block`.
#' @export
new_correlation_matrix_block <- function(method = "pearson",
                                         max_cols = 20L, ...) {
  new_transform_block(
    server = function(id, data) {
      moduleServer(id, function(input, output, session) {
        r_method <- reactiveVal(method)
        r_max <- reactiveVal(max_cols)
        observeEvent(input$method, r_method(input$method))
        observeEvent(input$max_cols, r_max(as.integer(input$max_cols)))
        list(
          expr = reactive({
            bquote(
              blockr.stats::correlation_matrix(data, method = .(m),
                max_cols = .(mx)),
              list(m = r_method(), mx = as.integer(r_max()))
            )
          }),
          state = list(method = r_method, max_cols = r_max)
        )
      })
    },
    ui = function(id) {
      tagList(
        div(
          class = "block-container",
          selectInput(NS(id, "method"), "Method",
            choices = c("Pearson" = "pearson",
                        "Spearman" = "spearman",
                        "Kendall" = "kendall"),
            selected = method, width = "100%"),
          numericInput(NS(id, "max_cols"), "Max numeric columns",
            value = max_cols, min = 2, max = 50, step = 1,
            width = "100%")
        )
      )
    },
    class = "correlation_matrix_block",
    allow_empty_state = c("method", "max_cols"),
    ...
  )
}
