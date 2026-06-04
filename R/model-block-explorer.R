#' Model Explorer Block (experimental)
#'
#' EXPERIMENTAL variant of [new_model_block()]. Identical behaviour and R-side
#' formula handling, but the UI uses the **formula-explorer widget**
#' ([formula_explorer_ui()]) instead of the formula-input widget. The explorer
#' forks `Blockr.Select` (as `BlockrX.multi`) to support clicking predictor
#' chips to select them and right-clicking the selection to cross them into an
#' interaction term (or remove them).
#'
#' All R-side helpers are shared with the model block
#' (`parse_formula()` / `make_model_formula()` / `build_model_call()` /
#' `build_formula_columns()` / `formula_col_types()` /
#' `normalize_formula_for_js()`), so interactions/transforms/splines refit
#' automatically with no additional R code.
#'
#' @inheritParams new_model_block
#' @return A transform block of class `model_explorer_block`.
#' @export
new_model_explorer_block <- function(
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
                  class = "block-input-wrapper",
                  style = "grid-column: 1 / -1;",
                  selectInput(
                    NS(id, "model_type"), "Model type",
                    choices = model_choices, selected = model_type,
                    width = "100%"
                  )
                ),
                div(
                  class = "block-input-wrapper",
                  style = "grid-column: 1 / -1;",
                  formula_explorer_ui(id)
                )
              )
            )
          )
        )
      )
    },
    class = "model_explorer_block",
    expr_type = "bquoted",
    external_ctrl = TRUE,
    allow_empty_state = "formula",
    ...
  )
}

#' Formula-explorer widget: HTML dependency
#'
#' JS + CSS for the EXPERIMENTAL formula-explorer widget. Bundles the vendored
#' `BlockrX.multi` fork (`blockr-select-multi.js`), the explorer logic
#' (`formula-explorer.js`) and its styling (`formula-explorer.css`). Version is
#' pinned to the package version so editing `inst/js` busts the browser cache
#' after a Version bump + reinstall.
#'
#' @return An [htmltools::tagList] of [htmltools::htmlDependency] objects.
#' @export
formula_explorer_dep <- function() {
  htmltools::tagList(
    htmltools::htmlDependency(
      name = "blockr-select-multi-js",
      version = utils::packageVersion("blockr.stats"),
      src = system.file("js", package = "blockr.stats"),
      script = "blockr-select-multi.js"
    ),
    htmltools::htmlDependency(
      name = "formula-explorer-js",
      version = utils::packageVersion("blockr.stats"),
      src = system.file("js", package = "blockr.stats"),
      script = "formula-explorer.js"
    ),
    htmltools::htmlDependency(
      name = "formula-explorer-css",
      version = utils::packageVersion("blockr.stats"),
      src = system.file("css", package = "blockr.stats"),
      stylesheet = "formula-explorer.css"
    )
  )
}

#' Formula-explorer widget: UI container
#'
#' Pulls the shared blockr JS primitives (from blockr.dplyr) and the
#' formula-explorer dependency, then the namespaced container the JS binds to.
#'
#' @param id Module id (namespace).
#' @return A UI tagList.
#' @export
formula_explorer_ui <- function(id) {
  htmltools::tagList(
    blockr.dplyr::blockr_core_js_dep(),
    blockr.dplyr::blockr_blocks_css_dep(),
    blockr.dplyr::blockr_select_dep(),
    blockr.dplyr::blockr_input_dep(),
    formula_explorer_dep(),
    shiny::div(
      id = shiny::NS(id, "formula_input"),
      class = "formula-explorer-container"
    )
  )
}

#' @export
block_output.model_explorer_block <- function(x, result, session) {
  renderUI({
    tagList(css_model_summary(), model_summary_html(result))
  })
}

#' @export
block_ui.model_explorer_block <- function(id, x, ...) {
  tagList(uiOutput(NS(id, "result")))
}
