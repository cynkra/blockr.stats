#' Model Summary Block
#'
#' Takes a **fitted model** and renders it as a dashboard-quality card: a
#' one-line model facts stripe (kind, n, the fit measures that suit the model
#' class) over a coefficient table whose first column is an inline forest --
#' estimate dot, uncertainty whisker, reference line and a shared axis.
#'
#' The block feeds on the model object directly. It calls `broom::tidy()` and
#' `broom::glance()` itself, so no adapter block is needed in between and any
#' model with a broom method works, whether or not blockr.stats has a block
#' that fits it: an `lm` from the model block, a `coxph` from the survival
#' block, or anything fitted in a function block. A tidy coefficient frame
#' (`term` + `estimate`) is accepted too, in which case the model facts are
#' simply unavailable.
#'
#' Its **value** is the coefficient data frame, so a table, chart or report
#' exhibit downstream sees an ordinary tidy frame; the card is that frame
#' drawn. Every option lives in the gear -- the card itself is the block's
#' face.
#'
#' @param uncertainty What the interval column and whisker show: `"ci95"`
#'   (default), `"ci90"`, `"ci99"`, `"se"` or `"none"`.
#' @param significance `"chips"` (default; a neutral badge at 0.1% / 1% / 5%
#'   / 10%), `"p"` (a p-value column), `"stars"` or `"none"`.
#' @param scale `"auto"` (default), `"raw"` or `"ratio"`.
#' @param effect_column,facts,intercept Show the forest column / the model
#'   facts line / the intercept row (all `TRUE` by default).
#' @param ... Forwarded to [new_transform_block()].
#' @return A transform block of class `model_summary_block`.
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   serve(
#'     new_model_summary_block(),
#'     data = list(data = lm(mpg ~ wt + hp, mtcars))
#'   )
#' }
#' @export
new_model_summary_block <- function(uncertainty = "ci95",
                                    significance = "chips",
                                    scale = "auto",
                                    effect_column = TRUE,
                                    facts = TRUE,
                                    intercept = TRUE,
                                    ...) {
  new_transform_block(
    server = function(id, data) {
      moduleServer(id, function(input, output, session) {
        r_uncertainty <- reactiveVal(uncertainty)
        r_significance <- reactiveVal(significance)
        r_scale <- reactiveVal(scale)
        r_effect <- reactiveVal(isTRUE(effect_column))
        r_facts <- reactiveVal(isTRUE(facts))
        r_intercept <- reactiveVal(isTRUE(intercept))

        observeEvent(input$uncertainty, r_uncertainty(input$uncertainty))
        observeEvent(input$significance, r_significance(input$significance))
        observeEvent(input$scale, r_scale(input$scale))
        observeEvent(input$effect_column, r_effect(isTRUE(input$effect_column)))
        observeEvent(input$facts, r_facts(isTRUE(input$facts)))
        observeEvent(input$intercept, r_intercept(isTRUE(input$intercept)))

        list(
          expr = reactive({
            build_model_summary_call(
              r_uncertainty(), r_significance(), r_scale(),
              r_effect(), r_facts(), r_intercept()
            )
          }),
          state = list(
            uncertainty = r_uncertainty,
            significance = r_significance,
            scale = r_scale,
            effect_column = r_effect,
            facts = r_facts,
            intercept = r_intercept
          )
        )
      })
    },
    ui = function(id) {
      ns <- NS(id)
      tagList(
        stats_controls_dep(),
        div(
          class = "block-container",
          # No face controls: the card in the output slot is the block's face,
          # so everything configurable sits behind the gear.
          gear_tray(
            ns,
            select_field(
              ns("uncertainty"), "Uncertainty",
              c(
                "95% confidence interval" = "ci95",
                "90% confidence interval" = "ci90",
                "99% confidence interval" = "ci99",
                "estimate \u00b1 one standard error (~68%)" = "se",
                "estimate only, no interval" = "none"
              ),
              uncertainty, label_only = FALSE, size = "large"
            ),
            select_field(
              ns("significance"), "Significance",
              c(
                "badges: 0.1% / 1% / 5% / 10%" = "chips",
                "a p-value column" = "p",
                "stars: *** / ** / * / ." = "stars",
                "not shown" = "none"
              ),
              significance, label_only = FALSE, size = "large"
            ),
            select_field(
              ns("scale"), "Coefficient scale",
              c(
                "ratio for glm / Cox, raw for lm" = "auto",
                "raw, on the link scale" = "raw",
                "exponentiated, reference at 1" = "ratio"
              ),
              scale, label_only = FALSE, size = "large"
            ),
            checkbox_field(ns("effect_column"), "Effect column (forest)",
                           effect_column),
            checkbox_field(ns("facts"), "Model facts line", facts),
            checkbox_field(ns("intercept"), "Intercept row", intercept),
            label = "Summary settings"
          )
        )
      )
    },
    class = "model_summary_block",
    expr_type = "bquoted",
    allow_empty_state = c("uncertainty", "significance", "scale",
                          "effect_column", "facts", "intercept"),
    ...
  )
}

#' @export
block_output.model_summary_block <- function(x, result, session) {
  renderUI({
    tagList(
      blockr.ui::theme_dep(),
      css_summary_card(),
      # The sort travels with the card, not with the controls: it is a
      # property of the rendered table, and it is browser-only.
      model_summary_sort_dep(),
      model_summary_card(result)
    )
  })
}

#' @export
block_ui.model_summary_block <- function(id, x, ...) {
  tagList(uiOutput(NS(id, "result")))
}
