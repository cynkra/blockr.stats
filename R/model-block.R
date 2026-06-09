#' Model Block
#'
#' Fit a statistical model: linear (`lm`) or generalized linear (`glm`:
#' logistic / Poisson / Gamma). The model formula is authored with the
#' **formula-input widget** (response, predictors, interactions, transforms,
#' splines, intercept). Returns the **fitted model object**; downstream the
#' broom adapter (tidy / glance / augment / anova) turns it into tidy frames
#' for the generic renderers. The block's own preview is a generic
#' `summary(model)`.
#'
#' ANOVA-as-model is not a model type here: it is the broom adapter's `anova`
#' mode over an `lm` fit. (ANOVA-as-test lives in the adaptive test block.)
#'
#' @param model_type One of `"lm"`, `"logistic"`, `"poisson"`, `"gamma"`.
#' @param formula Model formula as a plain string, e.g. `"mpg ~ hp + wt"`
#'   (response on the left; `+` additive, `*` interaction, `:` interaction-only).
#'   The visual formula-input widget keeps an internal AST seeded from this
#'   string; the block's state round-trips as the string.
#' @param weights,offset Optional column-name strings used as the fit's case
#'   weights / model offset (or `NULL` to omit).
#' @param ... Forwarded to [new_transform_block()].
#' @return A transform block of class `model_block`.
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   serve(
#'     new_model_block(model_type = "lm", formula = "mpg ~ hp + wt"),
#'     data = list(data = mtcars)
#'   )
#' }
#' @export
new_model_block <- function(
  model_type = "lm",
  formula = "",
  weights = NULL,
  offset = NULL,
  ...
) {
  model_choices <- c(
    "Linear (lm)"    = "lm",
    "Logistic (glm)" = "logistic",
    "Poisson (glm)"  = "poisson",
    "Gamma (glm)"    = "gamma"
  )

  new_transform_block(
    server = function(id, data) {
      moduleServer(id, function(input, output, session) {
        r_model_type <- reactiveVal(model_type)
        observeEvent(input$model_type, r_model_type(input$model_type))
        r_weights <- reactiveVal(weights)
        r_offset  <- reactiveVal(offset)

        # Formula is authored as a STRING (block state + AI surface). The shared
        # formula-input widget keeps a structured AST internally: seed it by
        # parsing the string, and project the AST back to a string for state.
        r_state <- formula_input_server(
          input, output, session, data, parse_formula_safe(formula)
        )
        r_formula <- reactiveVal(formula)
        observeEvent(r_state(), {
          txt <- formula_ast_to_text(r_state())
          if (!identical(isolate(r_formula()), txt)) {
            r_formula(txt)
          }
        }, ignoreInit = TRUE)

        list(
          expr = reactive({
            f <- make_model_formula(r_state())
            if (is.null(f)) {
              return(quote(NULL))
            }
            build_model_call(r_model_type(), f, r_weights(), r_offset())
          }),
          state = list(
            model_type = r_model_type,
            formula    = r_formula,
            weights    = r_weights,
            offset     = r_offset
          )
        )
      })
    },
    ui = function(id) {
      tagList(
        css_responsive_grid(),
        css_single_column("model"),
        div(
          class = "block-container model-block-container",
          div(
            class = "block-form-grid",
            div(
              class = "block-section",
              div(
                class = "block-section-grid",
                div(
                  class = "block-input-wrapper formula-model-type",
                  style = "grid-column: 1 / -1;",
                  shinyWidgets::radioGroupButtons(
                    NS(id, "model_type"),
                    label = "Model type",
                    choices = model_choices,
                    selected = model_type,
                    size = "sm"
                  )
                ),
                div(
                  class = "block-input-wrapper",
                  style = "grid-column: 1 / -1;",
                  formula_input_ui(id)
                )
              )
            )
          )
        )
      )
    },
    class = "model_block",
    expr_type = "bquoted",
    external_ctrl = TRUE,
    allow_empty_state = c("formula", "weights", "offset"),
    ...
  )
}

#' @export
block_output.model_block <- function(x, result, session) {
  renderUI({
    tagList(css_model_summary(), model_summary_html(result))
  })
}

#' @export
block_ui.model_block <- function(id, x, ...) {
  tagList(uiOutput(NS(id, "result")))
}
