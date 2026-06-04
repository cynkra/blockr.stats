#' Fit a survival model
#'
#' The Advanced-tier survival family (one separable cluster — the
#' single extraction seam to a future domain package). Returns the
#' fitted object; downstream the broom adapter tidies it for the
#' generic renderers.
#'
#' @param data A data frame.
#' @param type `"km"` (Kaplan-Meier), `"cox"` (Cox PH), `"cif"`
#'   (competing risks / cumulative incidence).
#' @param time_var Follow-up time column.
#' @param event_var Status column. KM/Cox: 1 = event, 0 = censor.
#'   CIF: 0 = censor, 1.. = competing causes.
#' @param group_var Optional grouping column.
#' @return A `survfit` / `coxph` / `cuminc` object.
#' @export
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

#' Survival Block (Advanced)
#'
#' KM / Cox / competing-risks. Returns the fitted model object; the
#' block's preview is a generic `summary()`. Feed downstream into the
#' broom adapter: `tidy(survfit)` -> curve points (drilldown
#' line+step), `tidy(coxph)` -> HRs (drilldown coef plot),
#' `tidy(cuminc)` -> cumulative-incidence curves.
#'
#' @param type,time_var,event_var,group_var Forwarded to
#'   [fit_survival()].
#' @param ... Forwarded to [new_transform_block()].
#' @return A transform block of class `survival_block`.
#' @export
new_survival_block <- function(type = "km", time_var = character(),
                               event_var = character(),
                               group_var = character(), ...) {
  new_transform_block(
    server = function(id, data) {
      moduleServer(id, function(input, output, session) {
        r_type  <- reactiveVal(type)
        r_time  <- reactiveVal(time_var)
        r_event <- reactiveVal(event_var)
        r_group <- reactiveVal(group_var)
        r_init  <- reactiveVal(FALSE)

        observeEvent(input$type, r_type(input$type))
        observeEvent(input$time_var, r_time(input$time_var))
        observeEvent(input$event_var, r_event(input$event_var))
        observeEvent(input$group_var,
          r_group(if (is.null(input$group_var)) "" else input$group_var))

        observe({
          if (!r_init() && length(colnames(data())) > 0) {
            d <- data()
            num <- colnames(d)[vapply(d, is.numeric, logical(1))]
            all <- colnames(d)
            updateSelectizeInput(session, "time_var",
              choices = num, selected = r_time())
            updateSelectizeInput(session, "event_var",
              choices = all, selected = r_event())
            updateSelectizeInput(session, "group_var",
              choices = c("(none)" = "", stats::setNames(all, all)),
              selected = r_group())
            r_init(TRUE)
          }
        })

        list(
          expr = reactive({
            tv <- r_time(); ev <- r_event()
            if (is.null(tv) || !nzchar(tv) || is.null(ev) ||
                !nzchar(ev)) return(quote(NULL))
            g <- r_group()
            g <- if (is.null(g) || !nzchar(g)) NULL else g
            bquote(
              blockr.stats::fit_survival(data, type = .(ty),
                time_var = .(tv), event_var = .(ev),
                group_var = .(g)),
              list(ty = r_type(), tv = tv, ev = ev, g = g)
            )
          }),
          state = list(
            type = r_type, time_var = r_time,
            event_var = r_event, group_var = r_group
          )
        )
      })
    },
    ui = function(id) {
      ns <- NS(id)
      tagList(
        div(
          class = "block-container",
          selectInput(ns("type"), "Survival model",
            choices = c("Kaplan-Meier" = "km", "Cox PH" = "cox",
                        "Competing risks (CIF)" = "cif"),
            selected = type, width = "100%"),
          selectizeInput(ns("time_var"), "Time",
            choices = time_var, selected = time_var,
            multiple = FALSE, width = "100%",
            options = list(placeholder = "Follow-up time column...")),
          selectizeInput(ns("event_var"), "Status / event",
            choices = event_var, selected = event_var,
            multiple = FALSE, width = "100%",
            options = list(
              placeholder = "1 = event, 0 = censor (CIF: cause codes)")),
          selectizeInput(ns("group_var"), "Group (optional)",
            choices = c("(none)" = "", stats::setNames(group_var, group_var)),
            selected = group_var, multiple = FALSE, width = "100%")
        )
      )
    },
    dat_valid = function(data) {
      if (!is.data.frame(data)) stop("Input must be a data frame")
    },
    class = "survival_block",
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
