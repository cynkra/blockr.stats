#' Noncentral-F CI for a partial effect size
#'
#' Inverts the noncentral F to get a `conf`-level CI on partial
#' eta-squared from an observed F. Returns `NA` bounds on
#' non-convergence rather than erroring.
#'
#' @param f_val Observed F statistic.
#' @param df1,df2 Numerator / denominator df.
#' @param conf Confidence level.
#' @return Named list `low`, `high` (partial eta^2 scale).
#' @keywords internal
#' @noRd
es_ncp_ci <- function(f_val, df1, df2, conf = 0.95) {
  na <- list(low = NA_real_, high = NA_real_)
  if (!is.finite(f_val) || f_val <= 0) return(na)
  a <- (1 - conf) / 2
  ncp_to_pe <- function(ncp) ncp / (ncp + df1 + df2 + 1)
  solve_ncp <- function(target) {
    f <- function(ncp) stats::pf(f_val, df1, df2, ncp = ncp) - target
    if (f(0) < 0) return(0)
    hi <- 1
    while (f(hi) > 0 && hi < 1e7) hi <- hi * 4
    if (f(hi) > 0) return(NA_real_)
    tryCatch(stats::uniroot(f, c(0, hi))$root, error = function(e) NA_real_)
  }
  lo <- solve_ncp(1 - a)
  hi <- solve_ncp(a)
  if (is.na(lo) || is.na(hi)) return(na)
  list(low = ncp_to_pe(lo), high = ncp_to_pe(hi))
}

#' Effect sizes from a fitted model (base R + effsize)
#'
#' @param model A fitted `lm`/`aov` (Cohen's d also needs a 2-level
#'   factor in the model frame).
#' @param measure One of `"eta2"`, `"partial_eta2"`, `"omega2"`,
#'   `"r2"`, `"d"`, `"g"`.
#' @param conf_level Confidence level.
#' @return Tidy data frame: `term, measure, estimate, conf.low,
#'   conf.high`. No interpretation labels.
#' @examples
#' fit <- aov(mpg ~ factor(cyl), data = mtcars)
#' effect_size(fit, measure = "partial_eta2")
#' @export
effect_size <- function(model, measure = "partial_eta2",
                        conf_level = 0.95) {
  msg <- function(m) data.frame(message = m, stringsAsFactors = FALSE)
  if (is.null(model)) return(msg("No model"))

  if (measure %in% c("d", "g")) {
    mf <- tryCatch(stats::model.frame(model), error = function(e) NULL)
    if (is.null(mf) || ncol(mf) < 2) return(msg("Need response ~ factor"))
    y <- mf[[1L]]
    fac <- NULL
    for (j in 2:ncol(mf)) {
      v <- mf[[j]]
      if ((is.factor(v) || is.character(v)) &&
          length(unique(stats::na.omit(v))) == 2L) {
        fac <- v
        break
      }
    }
    if (is.null(fac)) return(msg("Need a 2-level factor for d/g"))
    cd <- tryCatch(
      effsize::cohen.d(y, factor(fac),
        hedges.correction = identical(measure, "g"),
        conf.level = conf_level),
      error = function(e) NULL
    )
    if (is.null(cd)) return(msg("Cohen's d failed"))
    return(data.frame(
      term = "group", measure = measure,
      estimate = unname(cd$estimate),
      conf.low = cd$conf.int[[1L]], conf.high = cd$conf.int[[2L]],
      stringsAsFactors = FALSE
    ))
  }

  if (measure == "r2") {
    s <- tryCatch(summary(model), error = function(e) NULL)
    r2 <- if (!is.null(s) && !is.null(s$r.squared)) s$r.squared else NA_real_
    return(data.frame(term = "model", measure = "r2",
      estimate = r2, conf.low = NA_real_, conf.high = NA_real_,
      stringsAsFactors = FALSE))
  }

  aov_tab <- tryCatch(stats::anova(model), error = function(e) NULL)
  if (is.null(aov_tab) || !"Sum Sq" %in% names(aov_tab)) {
    return(msg("eta2/omega2 need an lm/aov model"))
  }
  ss <- aov_tab[["Sum Sq"]]
  df <- aov_tab[["Df"]]
  terms <- rownames(aov_tab)
  res_i <- length(ss)
  ss_res <- ss[res_i]
  df_res <- df[res_i]
  ms_res <- ss_res / df_res
  ss_tot <- sum(ss)
  fvals <- aov_tab[["F value"]]
  out <- lapply(seq_len(res_i - 1L), function(i) {
    est <- switch(
      measure,
      "eta2"         = ss[i] / ss_tot,
      "partial_eta2" = ss[i] / (ss[i] + ss_res),
      "omega2"       = (ss[i] - df[i] * ms_res) / (ss_tot + ms_res),
      NA_real_
    )
    ci <- list(low = NA_real_, high = NA_real_)
    if (!is.null(fvals) && is.finite(fvals[i])) {
      ci <- es_ncp_ci(fvals[i], df[i], df_res, conf_level)
    }
    data.frame(term = trimws(terms[i]), measure = measure,
      estimate = est, conf.low = ci$low, conf.high = ci$high,
      stringsAsFactors = FALSE)
  })
  do.call(rbind, out)
}

# NOTE (under review): usefulness debated. Niche block, only meaningful
# immediately after an ANOVA/lm fit; the weakest of the spine blocks.
# Kept for now; candidate for removal.
#' Effect Size Block
#'
#' Transform block wrapping [effect_size()]. Consumes a fitted model,
#' emits a tidy effect-size frame (no interpretation labels) for the
#' generic renderers.
#'
#' @param measure,conf_level Forwarded to [effect_size()].
#' @param ... Forwarded to [new_transform_block()].
#' @return A transform block of class `effect_size_block`.
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   serve(
#'     new_effect_size_block(measure = "partial_eta2"),
#'     data = list(data = aov(mpg ~ factor(cyl), mtcars))
#'   )
#' }
#' @export
new_effect_size_block <- function(measure = "partial_eta2",
                                  conf_level = 0.95, ...) {
  new_transform_block(
    server = function(id, data) {
      moduleServer(id, function(input, output, session) {
        r_measure <- reactiveVal(measure)
        r_conf <- reactiveVal(conf_level)
        observeEvent(input$measure, r_measure(input$measure))
        observeEvent(input$conf_level, r_conf(input$conf_level))
        list(
          expr = reactive({
            bquote(
              blockr.stats::effect_size(data, measure = .(m),
                conf_level = .(cl)),
              list(m = r_measure(), cl = r_conf())
            )
          }),
          state = list(measure = r_measure, conf_level = r_conf)
        )
      })
    },
    ui = function(id) {
      tagList(
        div(
          class = "block-container",
          selectInput(NS(id, "measure"), "Effect size",
            choices = c("Eta-squared" = "eta2",
                        "Partial eta-squared" = "partial_eta2",
                        "Omega-squared" = "omega2",
                        "R-squared" = "r2",
                        "Cohen's d" = "d", "Hedges' g" = "g"),
            selected = measure, width = "100%"),
          numericInput(NS(id, "conf_level"), "Confidence level",
            value = conf_level, min = 0.5, max = 0.999, step = 0.01,
            width = "100%")
        )
      )
    },
    class = "effect_size_block",
    allow_empty_state = c("measure", "conf_level"),
    ...
  )
}
