#' Build a model formula from the structured formula-input state
#'
#' Assembles a `response ~ rhs` formula from the structured model produced by
#' `parse_formula()` / the formula-input widget. Core terms and opaque/bar
#' terms are emitted by their stored labels; the intercept is handled via
#' [stats::reformulate()]'s `intercept` argument. Returns `NULL` when there is
#' no response (so the consuming block can pass through / emit `NULL`).
#'
#' Offset and weights are NOT part of the formula — they are role-box state
#' passed as arguments to the fitting call by the consuming model block.
#'
#' @param state Structured formula model (see `parse_formula()`).
#' @return A `formula`, or `NULL`.
#' @keywords internal
#' @noRd
make_model_formula <- function(state) {
  if (is.null(state) || is.null(state$response)) {
    return(NULL)
  }
  resp <- response_to_text(state$response)
  if (is.null(resp) || !nzchar(resp)) {
    return(NULL)
  }

  terms_list <- if (is.null(state$terms)) list() else state$terms
  bars_list  <- if (is.null(state$bars)) list() else state$bars

  labels <- vapply(terms_list, function(t) {
    lbl <- t$label
    # ns()/bs() live in the splines package (not attached): emit prefixed so the
    # fit can find them. Covers both menu- and text-added spline terms.
    if (identical(t$kind, "spline")) {
      lbl <- sub("^(ns|bs)\\(", "splines::\\1(", lbl)
    }
    lbl
  }, character(1))
  bars   <- vapply(bars_list, function(b) sprintf("(%s)", b$raw), character(1))
  rhs    <- c(labels, bars)

  intercept <- is.null(state$intercept) || isTRUE(state$intercept)

  if (!length(rhs)) {
    if (!intercept) {
      return(NULL)          # no terms and no intercept = empty model
    }
    rhs <- "1"
  }

  stats::reformulate(rhs, response = resp, intercept = intercept)
}

#' Build the bquoted fitting call for a model type
#'
#' Splices the formula `f` as a value and leaves `.(data)` for blockr.core to
#' resolve (`expr_type = "bquoted"`). Weights/offset wiring is deferred to the
#' role-box UI (the widget's JS layer).
#'
#' @param model_type One of `"lm"`, `"logistic"`, `"poisson"`, `"gamma"`.
#' @param f A `formula` (from `make_model_formula()`).
#' @keywords internal
#' @noRd
build_model_call <- function(model_type, f) {
  switch(
    model_type,
    logistic = blockr.core::bbquote(
      stats::glm(.(f), data = .(data), family = stats::binomial()), list(f = f)),
    poisson = blockr.core::bbquote(
      stats::glm(.(f), data = .(data), family = stats::poisson()), list(f = f)),
    gamma = blockr.core::bbquote(
      stats::glm(.(f), data = .(data), family = stats::Gamma()), list(f = f)),
    blockr.core::bbquote(stats::lm(.(f), data = .(data)), list(f = f))
  )
}

#' Build the standard-R broom expression for the selected output
#'
#' Emits plain `broom::tidy()` / `glance()` / `augment()` with the broom
#' block's conveniences inlined (CI fallback, optional QQ columns, model-aware
#' column `label` attributes). No blockr.stats function appears in the result.
#'
#' @param output One of `"tidy"`, `"glance"`, `"augment"`.
#' @param conf_int,conf_level CI controls for `tidy`.
#' @param qq Add QQ columns to `augment`.
#' @return A language object using only `broom` / base R.
#' @keywords internal
#' @noRd
build_broom_call <- function(output, conf_int = TRUE, conf_level = 0.95,
                             qq = FALSE) {
  switch(
    output,
    glance = bquote(as.data.frame(broom::glance(data))),
    augment = if (isTRUE(qq)) {
      bquote({
        out <- as.data.frame(broom::augment(data))
        if (".std.resid" %in% names(out)) {
          qn <- stats::qqnorm(out$.std.resid, plot.it = FALSE)
          out$.qq_theoretical <- qn$x
          out$.qq_sample <- qn$y
        }
        out
      })
    } else {
      bquote(as.data.frame(broom::augment(data)))
    },
    {
      tidy_call <- if (isTRUE(conf_int)) {
        bquote(broom::tidy(data, conf.int = TRUE, conf.level = .(cl)),
               list(cl = conf_level))
      } else {
        quote(broom::tidy(data))
      }
      bquote({
        out <- as.data.frame(
          tryCatch(.(tc), error = function(e) broom::tidy(data))
        )
        labs <- c(
          term = "Term", estimate = "Estimate", std.error = "Std. error",
          statistic = "Statistic", p.value = "p-value",
          conf.low = "Lower CI", conf.high = "Upper CI",
          time = "Time", n.risk = "At risk", n.event = "Events",
          n.censor = "Censored", strata = "Group", group = "Group"
        )
        if (inherits(data, "survfit")) {
          labs["estimate"] <- "Survival probability"
          labs["time"] <- "Time (days)"
        } else if (inherits(data, "cuminc")) {
          labs["estimate"] <- "Cumulative incidence"
          labs["time"] <- "Time (days)"
        } else if (inherits(data, "coxph")) {
          labs["estimate"] <- "log(Hazard ratio)"
          labs["term"] <- "Comparison"
        }
        for (nm in intersect(names(out), names(labs))) {
          attr(out[[nm]], "label") <- unname(labs[nm])
        }
        out
      }, list(tc = tidy_call))
    }
  )
}
