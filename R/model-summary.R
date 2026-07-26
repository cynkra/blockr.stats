#' Summarise a fitted model for display
#'
#' Takes a **fitted model object** and returns its coefficient table
#' (`broom::tidy()`) with the fit statistics (`broom::glance()`) and the
#' display options carried along as attributes. This is the value behind the
#' model summary block: the returned object is an ordinary data frame of
#' coefficients downstream, and [model_summary_card()] renders it as the card.
#'
#' Any model with a `broom::tidy()` method works, whether or not blockr.stats
#' has a block for it -- an `lm` from the model block, a `coxph` from the
#' survival block, or something fitted in a function block. No broom adapter
#' block in between: the tidying happens here.
#'
#' A tidy coefficient **frame** (`term` + `estimate`, e.g. straight from the
#' broom adapter block) is also accepted and used as-is; only the model facts
#' are then unavailable.
#'
#' @param x A fitted model object, or a tidy coefficient data frame.
#' @param uncertainty What the interval column and whisker show: `"ci95"`
#'   (default), `"ci90"`, `"ci99"`, `"se"` (estimate +/- one standard error,
#'   about 68%, *not* a confidence interval) or `"none"`.
#' @param significance How significance is displayed: `"chips"` (default;
#'   0.1% / 1% / 5% coloured, 10% grey), `"p"` (a p-value column), `"stars"`
#'   (`***` / `**` / `*` / `.`) or `"none"`.
#' @param scale `"auto"` (default) exponentiates coefficients to odds / rate /
#'   hazard ratios when the model uses a log or logit link and leaves them
#'   alone otherwise; `"ratio"` and `"raw"` force it either way. On the ratio
#'   scale the reference line moves from 0 to 1 and the axis becomes
#'   logarithmic.
#' @param effect_column Draw the inline forest column (default `TRUE`).
#' @param facts Show the one-line model facts stripe (default `TRUE`).
#' @param intercept Keep the intercept row (default `TRUE`). It is always kept
#'   out of the forest's scale, so it never squashes the other terms.
#' @return A data frame of coefficients, of class `model_summary`, with the
#'   glance frame, model facts and display options attached as attributes.
#' @examples
#' model_summary(lm(mpg ~ wt + hp, mtcars))
#' @export
model_summary <- function(x,
                          uncertainty = "ci95",
                          significance = "chips",
                          scale = "auto",
                          effect_column = TRUE,
                          facts = TRUE,
                          intercept = TRUE) {
  uncertainty  <- match.arg(uncertainty, ms_choices$uncertainty)
  significance <- match.arg(significance, ms_choices$significance)
  scale        <- match.arg(scale, ms_choices$scale)

  want_ci <- uncertainty %in% c("ci95", "ci90", "ci99")
  conf_level <- switch(uncertainty, ci90 = 0.90, ci99 = 0.99, 0.95)
  ratio <- switch(scale, ratio = TRUE, raw = FALSE, ms_is_ratio_model(x))

  from_frame <- ms_is_tidy_frame(x)
  coefs <- if (from_frame) as.data.frame(x) else ms_tidy(x, want_ci, conf_level, ratio)
  glance_df <- if (from_frame) NULL else ms_glance(x)

  if (is.null(coefs)) {
    coefs <- data.frame()
  }
  has_terms <- all(c("term", "estimate") %in% names(coefs)) && nrow(coefs) > 0L

  # `intercept = FALSE` drops the row from the value too, so the frame a
  # downstream block sees is the table the card shows.
  if (has_terms && !isTRUE(intercept)) {
    coefs <- coefs[coefs$term != "(Intercept)", , drop = FALSE]
    if (!nrow(coefs)) has_terms <- FALSE
  }

  structure(
    coefs,
    class = unique(c("model_summary", class(coefs))),
    ms_has_terms = has_terms,
    ms_glance = glance_df,
    ms_kind = if (from_frame) NULL else ms_model_kind(x),
    ms_nobs = if (from_frame) NULL else ms_nobs(x, glance_df),
    ms_labels = if (has_terms) ms_term_labels(coefs$term, ms_xlevels(x)) else NULL,
    ms_ratio_label = if (ratio) ms_ratio_label(x) else NULL,
    ms_opts = list(
      uncertainty = uncertainty,
      significance = significance,
      scale = if (ratio) "ratio" else "raw",
      conf_level = conf_level,
      effect_column = isTRUE(effect_column),
      facts = isTRUE(facts),
      intercept = isTRUE(intercept)
    )
  )
}

#' @export
print.model_summary <- function(x, ...) {
  facts <- ms_facts(x)
  if (length(facts)) {
    cat(paste(vapply(facts, ms_fact_text, character(1L)), collapse = " \u00b7 "), "\n")
  }
  print(as.data.frame(unclass_ms(x)), ...)
  invisible(x)
}

# Option vocabularies, shared by the block UI, the expression and match.arg().
ms_choices <- list(
  uncertainty = c("ci95", "ci90", "ci99", "se", "none"),
  significance = c("chips", "p", "stars", "none"),
  scale = c("auto", "raw", "ratio")
)

ms_defaults <- list(
  uncertainty = "ci95", significance = "chips", scale = "auto",
  effect_column = TRUE, facts = TRUE, intercept = TRUE
)

# --- broom access ----------------------------------------------------------

# tidy() with graceful fallback: not every method takes conf.int (or
# exponentiate), and a model that cannot be tidied at all yields NULL rather
# than an error, so the card can say so.
ms_tidy <- function(x, want_ci, conf_level, ratio) {
  attempts <- list()
  if (ratio) {
    attempts <- c(attempts, list(function() {
      ms_mute_exponentiate(
        broom::tidy(x, conf.int = want_ci, conf.level = conf_level,
                    exponentiate = TRUE)
      )
    }))
  }
  attempts <- c(attempts, list(
    function() broom::tidy(x, conf.int = want_ci, conf.level = conf_level),
    function() broom::tidy(x)
  ))

  for (f in attempts) {
    out <- tryCatch(f(), error = function(e) NULL)
    if (is.data.frame(out)) return(as.data.frame(out))
  }
  NULL
}

ms_glance <- function(x) {
  out <- tryCatch(broom::glance(x), error = function(e) NULL)
  if (is.data.frame(out) && nrow(out)) return(as.data.frame(out))
  ms_glance_fallback(x)
}

# broom refuses to glance a survfit with more than one stratum, which is
# exactly the Kaplan-Meier the card degrades to. Since the facts line is then
# the only thing the card can show, take n and events off the object itself
# rather than show a bare model name.
ms_glance_fallback <- function(x) {
  if (!inherits(x, "survfit")) return(NULL)
  n <- tryCatch(sum(x$n), error = function(e) NULL)
  events <- tryCatch(sum(x$n.event), error = function(e) NULL)
  if (is.null(n) && is.null(events)) return(NULL)
  data.frame(
    records = n %||% NA_real_,
    events = events %||% NA_real_
  )
}

# broom warns when you exponentiate a model without a log/logit link. On
# `scale = "auto"` that never fires; when the user forces the ratio scale it
# is their decision, and a warning banner over the block is not the way to
# report it -- the axis already says "ratio".
ms_mute_exponentiate <- function(expr) {
  withCallingHandlers(
    expr,
    warning = function(w) {
      if (grepl("xponentiat", conditionMessage(w))) invokeRestart("muffleWarning")
    }
  )
}

# --- model interrogation ---------------------------------------------------

# The link, not the family, is what decides: exponentiating is meaningful
# exactly when the coefficients are on a log or logit scale.
ms_is_ratio_model <- function(x) {
  if (inherits(x, c("coxph", "clogit", "crr", "coxphf"))) return(TRUE)
  link <- tryCatch(stats::family(x)$link, error = function(e) NULL)
  isTRUE(link %in% c("log", "logit"))
}

# On the ratio scale the estimate column is no longer an "estimate": name it
# for what it is, so a Cox column never reads as a coefficient.
ms_ratio_label <- function(x) {
  if (inherits(x, c("coxph", "clogit", "crr", "coxphf"))) return("HR")
  link <- tryCatch(stats::family(x)$link, error = function(e) NULL)
  if (identical(link, "logit")) return("OR")
  if (identical(link, "log")) return("RR")
  "Ratio"
}

ms_is_tidy_frame <- function(x) {
  is.data.frame(x) && all(c("term", "estimate") %in% names(x))
}

ms_model_kind <- function(x) {
  switch(
    class(x)[1L],
    lm      = "Linear model",
    glm     = paste0("GLM", tryCatch(paste0(" (", x$family$family, ")"),
                                     error = function(e) "")),
    aov     = "ANOVA",
    coxph   = "Cox proportional hazards",
    survfit = "Kaplan-Meier",
    cuminc  = "Cumulative incidence",
    class(x)[1L]
  )
}

# `n` before `nobs`, deliberately. For a coxph, `nobs()` counts EVENTS, and
# broom carries that straight into glance's `nobs` column: on the lung data
# glance reports n = 228 and nobs = 165. Printing "165 obs" for a 228-patient
# model would be wrong in the one direction a clinician would notice.
ms_nobs <- function(x, glance_df) {
  n <- NULL
  for (nm in c("n", "nobs", "records")) {
    if (is.null(n) && !is.null(glance_df) && nm %in% names(glance_df)) {
      n <- glance_df[[nm]][1L]
    }
  }
  if (is.null(n)) n <- tryCatch(stats::nobs(x), error = function(e) NULL)
  if (is.numeric(n) && length(n) == 1L && is.finite(n)) as.integer(n) else NULL
}

ms_xlevels <- function(x) {
  # `$` on a tibble warns about the missing column, so ask the type first
  if (is.data.frame(x)) return(NULL)
  lv <- tryCatch(x$xlevels, error = function(e) NULL)
  if (is.list(lv) && length(lv)) lv else NULL
}

# --- term labels -----------------------------------------------------------

# `speciesGentoo` reads as "species Gentoo": the factor level is the value,
# the variable is the label. Always done, never an option. Interactions are
# split part by part and rejoined with a times sign.
ms_term_labels <- function(terms, xlevels) {
  lapply(terms, function(term) {
    parts <- strsplit(term, ":", fixed = TRUE)[[1L]]
    split <- lapply(parts, ms_split_level, xlevels = xlevels)
    list(
      var = paste(vapply(split, `[[`, character(1L), "var"), collapse = " \u00d7 "),
      level = paste(
        Filter(nzchar, vapply(split, `[[`, character(1L), "level")),
        collapse = " \u00d7 "
      )
    )
  })
}

ms_split_level <- function(part, xlevels) {
  if (!is.null(xlevels)) {
    for (v in names(xlevels)) {
      if (startsWith(part, v)) {
        lvl <- substring(part, nchar(v) + 1L)
        if (nzchar(lvl) && lvl %in% xlevels[[v]]) {
          return(list(var = v, level = lvl))
        }
      }
    }
  }
  list(var = part, level = "")
}

# --- model facts -----------------------------------------------------------

# Which facts appear is per model class, not a user choice. Each entry is
# list(label, value, digits); label "" means a bare word (the model kind).
ms_facts <- function(res) {
  kind <- attr(res, "ms_kind")
  n <- attr(res, "ms_nobs")
  g <- attr(res, "ms_glance")
  pick <- function(nm) {
    if (!is.null(g) && nm %in% names(g) && is.finite(g[[nm]][1L])) g[[nm]][1L] else NULL
  }

  out <- list()
  if (!is.null(kind)) out <- c(out, list(list("", kind, NA)))
  if (!is.null(n)) out <- c(out, list(list("", paste(n, "obs"), NA)))

  add <- function(out, label, value, digits = 2L) {
    if (is.null(value)) out else c(out, list(list(label, value, digits)))
  }

  # Order matters: a Cox fit reports both concordance and an r.squared, and
  # concordance is the one that means something for it.
  if (!is.null(pick("concordance"))) {
    out <- add(out, "concordance", pick("concordance"), 3L)
    out <- add(out, "events", pick("nevent"), 0L)
  } else if (!is.null(pick("adj.r.squared"))) {
    out <- add(out, "adj. R\u00b2", pick("adj.r.squared"), 3L)
    out <- add(out, "residual SD", pick("sigma"), NA)
  } else if (!is.null(pick("r.squared"))) {
    out <- add(out, "R\u00b2", pick("r.squared"), 3L)
  } else if (!is.null(pick("records"))) {
    # A curve rather than a fit (Kaplan-Meier): the facts line is all the card
    # can show, so it carries what a survival reader actually wants.
    out <- add(out, "events", pick("events"), 0L)
    out <- add(out, "median survival", pick("median"), NA)
  } else if (!is.null(pick("deviance")) && !is.null(pick("null.deviance"))) {
    out <- add(out, "pseudo-R\u00b2",
               1 - pick("deviance") / pick("null.deviance"), 3L)
  }
  out <- add(out, "AIC", pick("AIC"), 0L)
  out
}

ms_fact_text <- function(fact) {
  val <- fact[[2L]]
  digits <- fact[[3L]]
  txt <- if (is.character(val)) {
    val
  } else if (is.na(digits)) {
    ms_fmt(val, ms_decimals(val))
  } else {
    ms_fmt(val, digits)
  }
  if (nzchar(fact[[1L]])) paste(fact[[1L]], txt) else txt
}

unclass_ms <- function(x) {
  attrs <- grep("^ms_", names(attributes(x)), value = TRUE)
  for (a in attrs) attr(x, a) <- NULL
  class(x) <- setdiff(class(x), "model_summary")
  x
}
