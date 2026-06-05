#' Broom Adapter Block
#'
#' Transform block: takes a fitted model (upstream) and emits a tidy
#' data frame via standard `broom` verbs. One block, three outputs
#' (`tidy`/`glance`/`augment`) selectable in the UI. The tidy frame is
#' the renderer boundary -- feed it to the generic drilldown chart/table
#' (coef plot, glance table, residual/QQ scatter).
#'
#' The generated expression is plain `broom::tidy()` / `glance()` /
#' `augment()` with the conveniences inlined: a `conf.int` fallback,
#' optional QQ columns on `augment`, and model-aware column `label`
#' attributes the renderers display. No blockr.stats function appears in
#' the generated code.
#'
#' @param output One of `"tidy"`, `"glance"`, `"augment"`.
#' @param conf_int,conf_level Add CIs to `tidy` (default `TRUE`, `0.95`).
#' @param qq Logical, add QQ columns to `augment` (default `FALSE`).
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
                            conf_level = 0.95, qq = FALSE, ...) {
  new_transform_block(
    server = function(id, data) {
      moduleServer(id, function(input, output_s, session) {
        r_output     <- reactiveVal(output)
        r_conf_int   <- reactiveVal(isTRUE(conf_int))
        r_conf_level <- reactiveVal(conf_level)
        r_qq         <- reactiveVal(isTRUE(qq))

        observeEvent(input$output, r_output(input$output))
        observeEvent(input$conf_int, r_conf_int(isTRUE(input$conf_int)))
        observeEvent(input$conf_level, r_conf_level(input$conf_level))
        observeEvent(input$qq, r_qq(isTRUE(input$qq)))

        list(
          expr = reactive({
            build_broom_call(r_output(), r_conf_int(),
                             r_conf_level(), r_qq())
          }),
          state = list(
            output = r_output, conf_int = r_conf_int,
            conf_level = r_conf_level, qq = r_qq
          )
        )
      })
    },
    ui = function(id) {
      tagList(
        div(
          class = "block-container",
          selectInput(NS(id, "output"), "Output",
            choices = c("Coefficients (tidy)" = "tidy",
                        "Fit summary (glance)" = "glance",
                        "Per-observation (augment)" = "augment"),
            selected = output, width = "100%"),
          checkboxInput(NS(id, "conf_int"), "Confidence intervals (tidy)",
            value = isTRUE(conf_int)),
          numericInput(NS(id, "conf_level"), "Confidence level",
            value = conf_level, min = 0.5, max = 0.999, step = 0.01,
            width = "100%"),
          checkboxInput(NS(id, "qq"), "Add QQ columns (augment)",
            value = isTRUE(qq))
        )
      )
    },
    class = "broom_block",
    expr_type = "bquoted",
    allow_empty_state = c("output", "conf_int", "conf_level", "qq"),
    ...
  )
}
