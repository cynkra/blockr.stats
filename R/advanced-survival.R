#' Tidy a cmprsk::cuminc object (broom has no method)
#'
#' Long form: `group`, `time`, `estimate` (cumulative incidence) so it
#' renders as a drilldown line+step like KM.
#'
#' @param x A `cuminc` object.
#' @param ... Unused.
#' @return A tidy data frame.
#' @method tidy cuminc
#' @export
tidy.cuminc <- function(x, ...) {
  parts <- x[names(x) != "Tests"]
  rows <- lapply(names(parts), function(nm) {
    p <- parts[[nm]]
    data.frame(group = nm, time = p$time, estimate = p$est,
               stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

#' Build the formula-input widget state from survival constructor args
#' @keywords internal
#' @noRd
survival_state <- function(time_var, event_var, group_var) {
  tv <- if (length(time_var)) time_var[[1L]] else ""
  ev <- if (length(event_var)) event_var[[1L]] else ""
  g <- if (length(group_var) && nzchar(group_var[[1L]])) group_var[[1L]] else NULL
  list(
    response = list(fn = "Surv", time = tv, event = ev, eventLevel = NULL),
    intercept = TRUE,
    terms = if (is.null(g)) list() else {
      list(list(kind = "factor", label = g, var = g))
    },
    bars = list(),
    offset = NULL,
    weights = NULL
  )
}

#' Build the survival fit expression from the widget state
#'
#' Emits standard R: KM/Cox go through `make_model_formula()`
#' (`survival::Surv(...) ~ rhs`) into `survival::survfit()` / `coxph()`; CIF
#' emits an inline `cmprsk::cuminc()` call with NA-filtering. No blockr.stats
#' function appears in the generated code.
#' @keywords internal
#' @noRd
build_survival_call <- function(type, state) {
  resp <- state$response
  if (is.null(resp) || is.null(resp$time) || !nzchar(resp$time) ||
      is.null(resp$event) || !nzchar(resp$event)) {
    return(quote(NULL))
  }
  if (identical(type, "cif")) {
    tv <- resp$time
    ev <- resp$event
    tl <- state$terms
    g <- if (length(tl)) tl[[1L]]$var else NULL
    grp_expr <- if (is.null(g)) {
      quote(rep("all", nrow(data)))
    } else {
      bquote(as.character(data[[.(g)]]), list(g = g))
    }
    return(bquote({
      ftime <- data[[.(tv)]]
      fstatus <- as.numeric(data[[.(ev)]])
      grp <- .(ge)
      keep <- !is.na(ftime) & !is.na(fstatus) & !is.na(grp)
      cmprsk::cuminc(ftime = ftime[keep], fstatus = fstatus[keep],
                     group = grp[keep])
    }, list(tv = tv, ev = ev, ge = grp_expr)))
  }
  f <- make_model_formula(state)
  if (is.null(f)) {
    return(quote(NULL))
  }
  if (identical(type, "cox")) {
    bquote(survival::coxph(.(f), data = data), list(f = f))
  } else {
    bquote(survival::survfit(.(f), data = data), list(f = f))
  }
}

#' Survival Block (Advanced)
#'
#' KM / Cox / competing-risks, authored with the **formula-input widget** in
#' survival mode (`Surv(time, event) ~ ...`). Returns the fitted model object;
#' feed downstream into the broom adapter: `tidy(survfit)` -> curve points,
#' `tidy(coxph)` -> HRs, `tidy(cuminc)` -> cumulative-incidence curves.
#'
#' @param type `"km"`, `"cox"`, or `"cif"`.
#' @param time_var,event_var Follow-up time and event/status columns
#'   (event coded 1 = event). Map to the `Surv(time, event)` response.
#' @param group_var Optional grouping/covariate column (the RHS).
#' @param ... Forwarded to [new_transform_block()].
#' @return A transform block of class `survival_block`.
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   serve(new_survival_block(type = "km"), data = list(data = survival::lung))
#' }
#' @export
new_survival_block <- function(type = "km", time_var = character(),
                               event_var = character(),
                               group_var = character(), ...) {
  type_choices <- c(
    "Kaplan-Meier"    = "km",
    "Cox PH"          = "cox",
    "Competing risks" = "cif"
  )
  init_state <- survival_state(time_var, event_var, group_var)

  new_transform_block(
    server = function(id, data) {
      moduleServer(id, function(input, output, session) {
        r_type <- reactiveVal(type)
        observeEvent(input$surv_type, r_type(input$surv_type))

        # Shared formula-input widget, in Surv(time, event) response mode
        r_state <- formula_input_server(
          input, output, session, data, init_state, response_mode = "surv"
        )

        list(
          expr = reactive(build_survival_call(r_type(), r_state())),
          state = list(
            type = r_type,
            time_var = reactive({
              t <- r_state()$response$time
              if (is.null(t) || !nzchar(t)) character() else t
            }),
            event_var = reactive({
              e <- r_state()$response$event
              if (is.null(e) || !nzchar(e)) character() else e
            }),
            group_var = reactive({
              tl <- r_state()$terms
              if (length(tl)) tl[[1L]]$var else character()
            })
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
                    NS(id, "surv_type"),
                    label = "Survival model",
                    choices = type_choices,
                    selected = type,
                    size = "sm"
                  )
                ),
                div(
                  class = "block-input-wrapper",
                  style = "grid-column: 1 / -1;",
                  formula_input_ui(id, response_mode = "surv")
                )
              )
            )
          )
        )
      )
    },
    dat_valid = function(data) {
      if (!is.data.frame(data)) stop("Input must be a data frame")
    },
    class = "survival_block",
    expr_type = "bquoted",
    allow_empty_state = c("time_var", "event_var", "group_var"),
    ...
  )
}

#' @export
block_output.survival_block <- function(x, result, session) {
  renderUI({
    tagList(css_model_summary(), model_summary_html(result))
  })
}

#' @export
block_ui.survival_block <- function(id, x, ...) {
  tagList(uiOutput(NS(id, "result")))
}
