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
#' @param formula Structured formula-input state: a list with `response`,
#'   `intercept`, `terms`, `bars`, `offset`, `weights` (see `parse_formula()`).
#' @param ... Forwarded to [new_transform_block()].
#' @return A transform block of class `model_block`.
#' @export
new_model_block <- function(
  model_type = "lm",
  formula = list(
    response = NULL, intercept = TRUE,
    terms = list(), bars = list(), offset = NULL, weights = NULL
  ),
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
        ns <- session$ns
        r_model_type <- reactiveVal(model_type)
        r_state <- reactiveVal(formula)

        # Bidirectional sync guard (prevents R -> JS -> R loops)
        self_write <- new.env(parent = emptyenv())
        self_write$active <- FALSE

        observeEvent(input$model_type, r_model_type(input$model_type))

        # Typed column metadata -> JS, on data change
        observeEvent(data(), {
          session$sendCustomMessage(
            "formula-columns",
            list(
              id = ns("formula_input"),
              columns = build_formula_columns(data())
            )
          )
        })

        # Text-mode: JS sends raw formula text -> R parses -> normalized state
        observeEvent(input$formula_input_parse_request, {
          parsed <- parse_formula(
            input$formula_input_parse_request,
            formula_col_types(data())
          )
          # role boxes are not parsed from the RHS; keep the current values
          parsed$offset <- r_state()$offset
          parsed$weights <- r_state()$weights
          self_write$active <- TRUE
          r_state(parsed)
          session$sendCustomMessage(
            "formula-update",
            list(id = ns("formula_input"), state = normalize_formula_for_js(parsed))
          )
        })

        # JS -> R: builder changed the model
        observeEvent(input$formula_input, {
          self_write$active <- TRUE
          r_state(input$formula_input)
        })

        # R -> JS: external/programmatic state change
        observeEvent(r_state(), {
          if (self_write$active) {
            self_write$active <- FALSE
          } else {
            session$sendCustomMessage(
              "formula-update",
              list(
                id = ns("formula_input"),
                state = normalize_formula_for_js(r_state())
              )
            )
          }
        })

        list(
          expr = reactive({
            f <- make_model_formula(r_state())
            if (is.null(f)) {
              return(quote(NULL))
            }
            build_model_call(r_model_type(), f)
          }),
          state = list(
            model_type = r_model_type,
            formula = r_state
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
    allow_empty_state = "formula",
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
