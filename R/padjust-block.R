#' P-Value Adjustment Block
#'
#' Adjusts p-values for multiple comparisons using [stats::p.adjust()].
#'
#' Takes a data frame as input and adds \code{p.adjusted} and
#' \code{p.adjust.method} columns to the result. Auto-detects the p-value
#' column based on common naming patterns.
#'
#' @param pcol column name containing the p-values (default: auto-detected).
#' @param method method for p-value adjustment (default: "bonferroni").
#' @param ... forwarded to [new_block()].
#'
#' @return A htest block object of class \code{padjust_block}.
#'
#' @export
new_padjust_block <- function(
  pcol   = character(),
  method = "bonferroni",
  ...
) {
  choices_method <- c(
    Bonferroni = "bonferroni", FDR = "fdr", Holm = "holm", Hommel = "hommel",
    Hochberg = "hochberg", "Benjamini-Hochberg" = "BH",
    "Benjamini-Yekutieli" = "BY"
  )

  patterns_pcol <- c(
    "p.value",  "pvalue",  "p_value",  "p-value",
    "p.values", "pvalues", "p_values", "p-values",
    "p.val",    "pval",    "p_val",    "p-val",
    "p.vals",   "pvals",   "p_vals",   "p-vals",
    "p"
  )

  is_pval <- function(x) {
    if (!is.numeric(x)) return(FALSE)
    x <- na.omit(x)
    length(x) == 0 || (min(x) >= 0 && max(x) <= 1)
  }

  ui <- function(id) {
    tagList(
      div(
        class = "block-container",
        div(
          class = "block-form-grid",
          div(
            selectizeInput(
              NS(id, "pcol"),
              label = "P-value column",
              choices = pcol,
              selected = pcol,
              width = "100%",
              options = list(placeholder = "Select a p-value column...")
            )
          ),
          div(
            selectInput(
              NS(id, "method"),
              label = "Adjustment method",
              choices = choices_method,
              selected = method,
              width = "100%"
            )
          )
        )
      )
    )
  }

  server <- function(id, data) {
    moduleServer(id, function(input, output, session) {
      r_pcol   <- reactiveVal(pcol)
      r_method <- reactiveVal(method)

      observeEvent(input$pcol, r_pcol(input$pcol), ignoreNULL = FALSE)
      observeEvent(input$method, r_method(input$method))

      observeEvent(colnames(data()), {
        req(data())
        selected <- r_pcol()

        cols <- colnames(data())[vapply(data(), is_pval, logical(1))]

        if (length(selected) == 0 || !selected %in% cols) {
          matched  <- cols[tolower(cols) %in% patterns_pcol]
          selected <- head(matched, 1)
        }

        placeholder <- if (length(cols) == 0) "No valid p-value columns" else
          "Select a p-value column..."

        updateSelectizeInput(session, "pcol",
          choices = cols, selected = selected,
          options = list(placeholder = placeholder))
      }, ignoreNULL = FALSE)

      list(
        expr = reactive({
          q_pcol   <- r_pcol()
          q_method <- r_method()
          q_name   <- names(choices_method)[match(q_method, choices_method)]

          if (is.null(q_pcol) || !any(nzchar(q_pcol))) {
            return(bquote(NULL))
          }

          bquote({
            res <- data
            tibble::add_column(res,
              p.adjusted = stats::p.adjust(res[[.(q_pcol)]], .(q_method)),
              p.adjust.method = .(q_name),
              .after = .(q_pcol)
            )
          })
        }),
        state = list(pcol = r_pcol, method = r_method)
      )
    })
  }

  new_htest_block(
    server, ui, "padjust_block",
    dat_valid = function(data) stopifnot(is.data.frame(data)),
    expr_type = "bquoted",
    allow_empty_state = c("pcol"),
    ...
  )
}
