#' Linear Model Block
#'
#' Fit a linear model using [stats::lm()]. This block takes a data frame as
#' input and outputs an lm model object that can be consumed by downstream
#' diagnostic blocks (coefficients, ANOVA, plots).
#'
#' @param response Response variable column name (single column)
#' @param predictors Predictor variable column names (can be multiple)
#' @param intercept Logical. Include intercept term in the model (default TRUE)
#' @param ... Forwarded to [new_transform_block()]
#'
#' @return A transform block object of class \code{lm_block} that outputs an lm model object.
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   serve(new_lm_block(), list(data = mtcars))
#' }
#'
#' @export
new_lm_block <- function(
  response = character(),
  predictors = character(),
  intercept = TRUE,
  ...
) {
  new_transform_block(
    server = function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {
          # Initialize reactive values
          r_response <- reactiveVal(response)
          r_predictors <- reactiveVal(predictors)
          r_intercept <- reactiveVal(intercept)
          r_initialized <- reactiveVal(FALSE)

          # Update reactive values when inputs change
          observeEvent(input$response, {
            r_response(input$response)
          })

          observeEvent(input$predictors, {
            r_predictors(input$predictors)
          })

          observeEvent(input$intercept, {
            r_intercept(input$intercept)
          })

          # Restore initial selection once on startup
          observe({
            if (!r_initialized() && length(colnames(data())) > 0) {
              cols <- colnames(data())
              # Filter to numeric columns for initial choices
              numeric_cols <- cols[vapply(data(), is.numeric, logical(1))]

              updateSelectizeInput(
                session,
                "response",
                choices = numeric_cols,
                selected = r_response()
              )
              updateSelectizeInput(
                session,
                "predictors",
                choices = numeric_cols,
                selected = r_predictors()
              )
              r_initialized(TRUE)
            }
          })

          # Update choices when data changes, preserve selection
          observeEvent(
            colnames(data()),
            {
              if (r_initialized()) {
                req(data())
                cols <- colnames(data())
                numeric_cols <- cols[vapply(data(), is.numeric, logical(1))]

                new_resp <- intersect(r_response(), numeric_cols)
                new_preds <- intersect(r_predictors(), numeric_cols)
                r_response(new_resp)
                r_predictors(new_preds)

                updateSelectizeInput(
                  session,
                  "response",
                  choices = numeric_cols,
                  selected = new_resp
                )
                updateSelectizeInput(
                  session,
                  "predictors",
                  choices = numeric_cols,
                  selected = new_preds
                )
              }
            },
            ignoreNULL = FALSE
          )

          list(
            expr = reactive({
              resp <- r_response()
              preds <- r_predictors()
              incl_intercept <- r_intercept()

              # Require response variable
              if (is.null(resp) || length(resp) == 0 || resp == "") {
                return(quote(NULL))
              }

              # Build formula string
              if (length(preds) == 0 || all(preds == "")) {
                # No predictors - intercept only model
                if (incl_intercept) {
                  formula_str <- paste0("`", resp, "` ~ 1")
                } else {
                  # No intercept, no predictors - can't fit
                  return(quote(NULL))
                }
              } else {
                # Build predictor string with backticks
                pred_str <- paste0("`", preds, "`", collapse = " + ")
                if (incl_intercept) {
                  formula_str <- paste0("`", resp, "` ~ ", pred_str)
                } else {
                  formula_str <- paste0("`", resp, "` ~ 0 + ", pred_str)
                }
              }

              # Build lm expression
              expr_text <- glue::glue("stats::lm({formula_str}, data = data)")
              parse(text = expr_text)[[1]]
            }),
            state = list(
              response = r_response,
              predictors = r_predictors,
              intercept = r_intercept
            )
          )
        }
      )
    },
    ui = function(id) {
      tagList(
        # CSS utilities
        css_responsive_grid(),
        css_single_column("lm"),

        div(
          class = "block-container lm-block-container",

          div(
            class = "block-form-grid",

            # Model specification section
            div(
              class = "block-section",
              div(
                class = "block-section-grid",

                # Help text
                div(
                  class = "block-help-text",
                  "Select response and predictor variables to fit a linear model."
                ),

                # Response variable (single select)
                div(
                  class = "block-input-wrapper",
                  style = "grid-column: 1 / -1;",
                  selectizeInput(
                    NS(id, "response"),
                    label = "Response variable (Y)",
                    choices = response,
                    selected = response,
                    multiple = FALSE,
                    width = "100%",
                    options = list(
                      placeholder = "Select response variable..."
                    )
                  )
                ),

                # Predictor variables (multi select with drag/drop)
                div(
                  class = "block-input-wrapper",
                  style = "grid-column: 1 / -1;",
                  selectizeInput(
                    NS(id, "predictors"),
                    label = "Predictor variables (X)",
                    choices = predictors,
                    selected = predictors,
                    multiple = TRUE,
                    width = "100%",
                    options = list(
                      plugins = list("drag_drop", "remove_button"),
                      persist = FALSE,
                      placeholder = "Select predictor columns..."
                    )
                  )
                ),

                # Intercept checkbox
                div(
                  class = "block-input-wrapper",
                  checkboxInput(
                    NS(id, "intercept"),
                    label = "Include intercept",
                    value = intercept
                  )
                )
              )
            )
          )
        )
      )
    },
    class = "lm_block",
    allow_empty_state = c("response", "predictors"),
    ...
  )
}

#' @export
block_output.lm_block <- function(x, result, session) {
  renderPrint({
    if (is.null(result)) {
      cat("Select response and predictor variables to fit a model.")
    } else {
      summary(result)
    }
  })
}

#' @export
block_ui.lm_block <- function(id, x, ...) {
  tagList(
    verbatimTextOutput(NS(id, "result"))
  )
}
