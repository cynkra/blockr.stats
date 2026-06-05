#' Fit a survival model (low-level engine for the CIF path)
#'
#' Internal worker behind `survival_fit_safe()`. KM/Cox go through the
#' formula path in `survival_fit_safe()`; this handles the non-formula
#' competing-risks (`cif`) case.
#'
#' @param data A data frame.
#' @param type `"km"` (Kaplan-Meier), `"cox"` (Cox PH), `"cif"`
#'   (competing risks / cumulative incidence).
#' @param time_var Follow-up time column.
#' @param event_var Status column. KM/Cox: 1 = event, 0 = censor.
#'   CIF: 0 = censor, 1.. = competing causes.
#' @param group_var Optional grouping column.
#' @return A `survfit` / `coxph` / `cuminc` object.
#' @keywords internal
#' @noRd
fit_survival <- function(data, type = "km", time_var, event_var,
                         group_var = NULL) {
  stopifnot(is.data.frame(data))
  has_grp <- !is.null(group_var) && length(group_var) == 1L &&
    nzchar(group_var) && group_var %in% names(data)
  tm <- data[[time_var]]
  ev <- data[[event_var]]
  if (identical(type, "cif")) {
    grp <- if (has_grp) as.character(data[[group_var]]) else
      rep("all", length(tm))
    keep <- !is.na(tm) & !is.na(ev) & !is.na(grp)
    return(cmprsk::cuminc(ftime = tm[keep],
                          fstatus = as.numeric(ev[keep]),
                          group = grp[keep]))
  }
  rhs <- if (has_grp) paste0("`", group_var, "`") else "1"
  form <- stats::as.formula(
    sprintf("survival::Surv(`%s`, `%s`) ~ %s",
            time_var, event_var, rhs)
  )
  if (identical(type, "cox")) {
    if (!has_grp) stop("Cox needs a grouping/covariate.")
    survival::coxph(form, data = data)
  } else {
    survival::survfit(form, data = data)
  }
}

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

#' Fit a survival model, returning `NULL` instead of erroring
#'
#' Wraps the fit in `tryCatch` so an invalid intermediate selection (e.g. a
#' time/event pair with no non-missing observations, mid-interaction) yields a
#' `NULL` placeholder preview rather than a hard error.
#'
#' @param type `"km"`, `"cox"`, or `"cif"`.
#' @param formula Model formula (KM/Cox).
#' @param data Data frame.
#' @param time_var,event_var,group_var Columns (CIF path).
#' @return The fitted object, or `NULL` on error.
#' @examples
#' survival_fit_safe(
#'   "km",
#'   formula = survival::Surv(time, status) ~ sex,
#'   data = survival::lung
#' )
#' @export
survival_fit_safe <- function(type, formula = NULL, data,
                              time_var = NULL, event_var = NULL,
                              group_var = NULL) {
  tryCatch(
    if (identical(type, "cif")) {
      fit_survival(data, type = "cif", time_var = time_var,
                   event_var = event_var, group_var = group_var)
    } else if (identical(type, "cox")) {
      survival::coxph(formula, data = data)
    } else {
      survival::survfit(formula, data = data)
    },
    error = function(e) NULL
  )
}

#' Build the bquoted survival fit call from the widget state
#'
#' KM/Cox go through `make_model_formula()` (`survival::Surv(...) ~ rhs`) into
#' `survfit`/`coxph`; CIF uses the non-formula `fit_survival()` `cuminc` path.
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
    return(blockr.core::bbquote(
      blockr.stats::survival_fit_safe("cif", data = .(data),
        time_var = .(tv), event_var = .(ev), group_var = .(g)),
      list(tv = tv, ev = ev, g = g)
    ))
  }
  f <- make_model_formula(state)
  if (is.null(f)) {
    return(quote(NULL))
  }
  blockr.core::bbquote(
    blockr.stats::survival_fit_safe(.(ty), formula = .(f), data = .(data)),
    list(ty = type, f = f)
  )
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
