#' Apply a broom verb to a fitted model
#'
#' The single generic adapter that turns any fitted model into a tidy
#' data frame for the generic renderers. `tidy` -> coefficient table
#' (coef plot via drilldown lo/hi); `glance` -> one-row fit summary;
#' `augment` -> per-observation frame for diagnostic scatters. With
#' `qq = TRUE` the augmented frame also gets `.qq_theoretical` /
#' `.qq_sample` so a QQ plot is a plain scatter.
#'
#' @param model A fitted model object.
#' @param output One of `"tidy"`, `"glance"`, `"augment"`.
#' @param conf_int Logical, add CIs to `tidy` (default TRUE).
#' @param conf_level Confidence level (default 0.95).
#' @param qq Logical, add QQ columns to `augment` (default FALSE).
#' @return A tidy `data.frame`.
#' @export
broom_apply <- function(model, output = "tidy", conf_int = TRUE,
                        conf_level = 0.95, qq = FALSE) {
  if (is.null(model)) {
    return(data.frame(message = "No model", stringsAsFactors = FALSE))
  }
  res <- switch(
    output,
    "glance"  = broom::glance(model),
    "augment" = {
      a <- tryCatch(broom::augment(model), error = function(e) NULL)
      if (is.null(a)) {
        data.frame(message = "augment() unavailable for this model",
                   stringsAsFactors = FALSE)
      } else {
        a <- as.data.frame(a)
        if (isTRUE(qq) && ".std.resid" %in% names(a)) {
          q <- stats::qqnorm(a$.std.resid, plot.it = FALSE)
          a$.qq_theoretical <- q$x
          a$.qq_sample <- q$y
        }
        a
      }
    },
    {
      tidy_args <- list(x = model)
      if (isTRUE(conf_int)) {
        tidy_args$conf.int <- TRUE
        tidy_args$conf.level <- conf_level
      }
      tryCatch(
        do.call(broom::tidy, tidy_args),
        error = function(e) broom::tidy(model)
      )
    }
  )
  res <- as.data.frame(res)
  if (identical(output, "tidy")) res <- label_tidy_columns(res, model)
  res
}

#' Attach human-readable `label` attributes to a tidy frame
#'
#' Generic renderers (drilldown chart/table axis titles) prefer a
#' column's `label` attribute over its raw name. broom's tidy columns
#' (`estimate`, `time`, `term`, ...) are terse and model-dependent, so
#' we annotate them with context-aware labels keyed off the fitted
#' model's class. Names are unchanged — only the attribute is added.
#'
#' @param df A tidy data frame from [broom_apply()].
#' @param model The fitted model it came from.
#' @return `df` with per-column `label` attributes.
#' @keywords internal
label_tidy_columns <- function(df, model) {
  labs <- c(
    term = "Term", estimate = "Estimate", std.error = "Std. error",
    statistic = "Statistic", p.value = "p-value",
    conf.low = "Lower CI", conf.high = "Upper CI",
    time = "Time", n.risk = "At risk", n.event = "Events",
    n.censor = "Censored", strata = "Group", group = "Group"
  )
  if (inherits(model, "survfit")) {
    labs["estimate"] <- "Survival probability"
    labs["time"] <- "Time (days)"
  } else if (inherits(model, "cuminc")) {
    labs["estimate"] <- "Cumulative incidence"
    labs["time"] <- "Time (days)"
  } else if (inherits(model, "coxph")) {
    labs["estimate"] <- "log(Hazard ratio)"
    labs["term"] <- "Comparison"
  }
  for (nm in intersect(names(df), names(labs))) {
    attr(df[[nm]], "label") <- unname(labs[nm])
  }
  df
}

#' Broom Adapter Block
#'
#' Transform block: takes a fitted model (upstream) and emits a tidy
#' data frame via [broom_apply()]. One block, three outputs
#' (`tidy`/`glance`/`augment`) selectable in the UI. The tidy frame is
#' the renderer boundary — feed it to the generic drilldown chart/table
#' (coef plot, glance table, residual/QQ scatter).
#'
#' (Future Advanced "woven" option: a robust/clustered `se`/`vcov`
#' path on `tidy` via `sandwich`/`lmtest` — not implemented in v1.)
#'
#' @param output One of `"tidy"`, `"glance"`, `"augment"`.
#' @param conf_int,conf_level,qq Forwarded to [broom_apply()].
#' @param ... Forwarded to [new_transform_block()].
#' @return A transform block of class `broom_block`.
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
            bquote(
              blockr.stats::broom_apply(
                data, output = .(o), conf_int = .(ci),
                conf_level = .(lvl), qq = .(q)
              ),
              list(o = r_output(), ci = r_conf_int(),
                   lvl = r_conf_level(), q = r_qq())
            )
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
    allow_empty_state = c("output", "conf_int", "conf_level", "qq"),
    ...
  )
}
