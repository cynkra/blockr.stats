#' Broom Adapter Block
#'
#' Transform block: takes a fitted model (upstream) and emits a tidy
#' data frame via standard `broom` verbs. One block, three outputs
#' (`tidy`/`glance`/`augment`) selectable in the UI. The tidy frame is
#' the renderer boundary -- feed it to the generic drilldown chart/table
#' (coef plot, glance table, residual/QQ scatter).
#'
#' The verb picker is the whole face of the block. Everything else is
#' verb-specific and lives in the gear tray, which shows only the options
#' the selected verb actually has: `conf.int` / `conf.level` for `tidy`
#' (broom's own arguments) and QQ columns for `augment` (a blockr.stats
#' convenience, so a QQ plot is a plain scatter). `glance()` has no
#' options, so on `glance` there is no gear.
#'
#' The generated expression is plain `broom::tidy()` / `glance()` /
#' `augment()`; no blockr.stats function appears in the generated code.
#'
#' @param output One of `"tidy"`, `"glance"`, `"augment"`.
#' @param conf_int,conf_level Add CIs to `tidy` (default `TRUE`, `0.95`).
#' @param qq Logical, add QQ columns to `augment` (default `FALSE`).
#' @param parametric Logical, `tidy` a GAM's PARAMETRIC coefficients instead of
#'   its smooth terms (default `FALSE`). Only `broom::tidy.gam` takes this;
#'   leave it off for every other model.
#' @param response Logical, `augment` on the RESPONSE scale rather than the
#'   link scale (default `FALSE`). Only meaningful for a glm or gam, where
#'   `.fitted` is otherwise log-odds / log-counts.
#' @param ... Forwarded to [new_transform_block()].
#' @return A transform block of class `broom_block`.
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   serve(
#'     new_broom_block(output = "tidy"),
#'     data = list(data = lm(mpg ~ wt + hp, mtcars))
#'   )
#' }
#' @export
new_broom_block <- function(output = "tidy", conf_int = TRUE,
                            conf_level = 0.95, qq = FALSE,
                            parametric = FALSE, response = FALSE, ...) {
  new_transform_block(
    server = function(id, data) {
      moduleServer(id, function(input, output_s, session) {
        r_output     <- reactiveVal(output)
        r_conf_int   <- reactiveVal(isTRUE(conf_int))
        r_conf_level <- reactiveVal(conf_level)
        r_qq         <- reactiveVal(isTRUE(qq))
        r_parametric <- reactiveVal(isTRUE(parametric))
        r_response   <- reactiveVal(isTRUE(response))

        observeEvent(input$output, r_output(input$output))
        observeEvent(input$conf_int, r_conf_int(isTRUE(input$conf_int)))
        observeEvent(input$conf_level, r_conf_level(input$conf_level))
        observeEvent(input$qq, r_qq(isTRUE(input$qq)))
        observeEvent(input$parametric, r_parametric(isTRUE(input$parametric)))
        observeEvent(input$response, r_response(isTRUE(input$response)))

        list(
          expr = reactive({
            build_broom_call(r_output(), r_conf_int(),
                             r_conf_level(), r_qq(), r_parametric(),
                             r_response())
          }),
          state = list(
            output = r_output, conf_int = r_conf_int,
            conf_level = r_conf_level, qq = r_qq,
            parametric = r_parametric, response = r_response
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
          # Gear and tray together are conditional: glance() has no options,
          # so it gets no gear at all rather than one that opens an empty
          # tray. tidy and augment each see only their own options.
          conditionalPanel(
            "input.output != 'glance'", ns = ns,
            gear_tray(
              ns,
              # Shiny draws a conditionalPanel as display: contents, so the
              # fields inside are the tray grid's own.
              conditionalPanel(
                "input.output == 'tidy'", ns = ns,
                checkbox_field(ns("conf_int"), "Confidence intervals",
                               conf_int, size = "large"),
                number_field(ns("conf_level"), "Confidence level",
                             conf_level, min = 0.5, max = 0.999,
                             step = 0.01),
                checkbox_field(ns("parametric"),
                  "Parametric terms (GAM: coefficients, not smooths)",
                  parametric, size = "full")
              ),
              conditionalPanel(
                "input.output == 'augment'", ns = ns,
                checkbox_field(ns("qq"),
                  "QQ columns (.qq_theoretical / .qq_sample)",
                  qq, size = "full"),
                checkbox_field(ns("response"),
                  "Response scale (glm/gam: .fitted as a rate, not log-odds)",
                  response, size = "full")
              ),
              label = "Output settings"
            )
          ),
          div(
            class = "blockr-stats-face",
            select_field(ns("output"), "Output",
              choices = c("Coefficients (tidy)" = "tidy",
                          "Fit summary (glance)" = "glance",
                          "Per-observation (augment)" = "augment"),
              selected = output)
          )
        )
      )
    },
    class = "broom_block",
    expr_type = "bquoted",
    allow_empty_state = c("output", "conf_int", "conf_level", "qq",
                          "parametric", "response"),
    ...
  )
}
