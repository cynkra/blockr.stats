#' Generic HTML model card (report-card layout)
#'
#' Render any fitted model as a compact report card: a headline (outcome
#' formula, model kind, n) with a fit-quality chip, a coefficient forest
#' plot (estimate dot + CI whisker, reference line at 0, coloured by sign
#' / significance) as the hero, and a collapsible "details" drawer with
#' the coefficient / statistics / diagnostics tables. Generic across
#' `lm` / `glm` / `aov` / `coxph` via broom; pieces degrade gracefully
#' when a model lacks them. Used as the `block_output` preview of the
#' model and survival blocks.
#'
#' @param model A fitted model object (or `NULL`).
#' @param conf_level Confidence level for the coefficient CIs.
#' @param digits Significant digits for numeric formatting.
#' @return An [htmltools::tagList()].
#' @noRd
model_summary_html <- function(model, conf_level = 0.95, digits = 3, id = NULL) {
  if (is.null(model)) {
    return(tags$div(class = "smb-card smb-empty", "Pick variables to fit a model."))
  }
  if (is.null(id)) id <- paste0("smb-", basename(tempfile("")))

  tidy_df <- tryCatch(
    broom::tidy(model, conf.int = TRUE, conf.level = conf_level),
    error = function(e) tryCatch(broom::tidy(model), error = function(e) NULL)
  )
  glance_df <- tryCatch(broom::glance(model), error = function(e) NULL)

  rtext <- tryCatch({
    out <- utils::capture.output(summary(model))
    if (length(out) > 200L) out <- c(out[seq_len(200L)], "...")
    paste(out, collapse = "\n")
  }, error = function(e) "summary() not available")

  # class-keyed radio toggle: visual (default) vs R text, no server round-trip
  vid <- paste0(id, "-v"); rid <- paste0(id, "-r")
  tags$div(class = "smb-card",
    tags$input(type = "radio", name = id, id = vid, class = "smb-radio smb-radio-v",
               checked = NA),
    tags$input(type = "radio", name = id, id = rid, class = "smb-radio smb-radio-r"),
    tags$div(class = "smb-switch",
      tags$label(`for` = vid, class = "smb-seg smb-seg-v", "Visual"),
      tags$label(`for` = rid, class = "smb-seg smb-seg-r", "R")),
    tags$div(class = "smb-visual",
      smb_headline(model, glance_df),
      smb_forest(tidy_df)),
    tags$pre(class = "smb-rtext", rtext))
}

# --- headline: kind . n + fit chip (formula lives in the block name) --------
smb_headline <- function(model, glance_df) {
  n <- tryCatch(stats::nobs(model), error = function(e) NULL)
  label <- smb_model_kind(model)
  if (is.numeric(n) && length(n) == 1L && is.finite(n)) {
    label <- paste0(label, " · ", n, " obs")
  }

  tags$div(class = "smb-hl",
    tags$div(class = "smb-hl-kind", label),
    smb_fit_chip(glance_df, model))
}

smb_model_kind <- function(model) {
  cl <- class(model)[1]
  switch(cl,
    lm      = "Linear model",
    glm     = paste0("GLM", tryCatch(paste0(" (", model$family$family, ")"),
                                     error = function(e) "")),
    aov     = "ANOVA",
    coxph   = "Cox proportional hazards",
    survfit = "Kaplan-Meier",
    cuminc  = "Cumulative incidence",
    cl)
}

smb_fit_chip <- function(glance_df, model) {
  g <- function(nm) if (!is.null(glance_df) && nm %in% names(glance_df)) glance_df[[nm]][1] else NULL
  spec <- NULL
  if (!is.null(g("r.squared")))        spec <- list("R²", g("r.squared"), g("r.squared"))
  else if (!is.null(g("concordance"))) spec <- list("C", g("concordance"), g("concordance"))
  else if (!is.null(g("deviance")) && !is.null(g("null.deviance"))) {
    pr2 <- 1 - g("deviance") / g("null.deviance")
    spec <- list("pseudo-R²", pr2, pr2)
  } else if (!is.null(g("AIC")))       spec <- list("AIC", g("AIC"), NA)

  if (is.null(spec)) return(NULL)
  frac <- spec[[3]]
  has_bar <- is.numeric(frac) && is.finite(frac) && frac >= 0 && frac <= 1
  val <- if (spec[[1]] == "AIC") sprintf("%.0f", spec[[2]]) else sprintf("%.2f", spec[[2]])

  tags$div(class = "smb-chip",
    tags$div(class = "smb-chip-row",
      tags$span(class = "smb-chip-k", spec[[1]]),
      tags$span(class = "smb-chip-v", val)),
    if (has_bar) tags$div(class = "smb-chip-bar",
      tags$div(class = "smb-chip-fill",
        style = paste0("width:", round(100 * frac), "%;"))))
}

# --- forest plot: estimate dot + CI whisker, ref line at 0 ------------------
smb_forest <- function(tidy_df) {
  if (is.null(tidy_df) || !("estimate" %in% names(tidy_df)) || !nrow(tidy_df)) return(NULL)
  if ("time" %in% names(tidy_df) && nrow(tidy_df) > 20) return(NULL)  # curve points

  d <- tidy_df
  if (nrow(d) > 1 && "(Intercept)" %in% d$term) d <- d[d$term != "(Intercept)", , drop = FALSE]
  if (!nrow(d)) return(NULL)

  has_ci <- all(c("conf.low", "conf.high") %in% names(d))
  lo <- if (has_ci) d$conf.low else d$estimate
  hi <- if (has_ci) d$conf.high else d$estimate
  rng <- range(c(lo, hi, 0), na.rm = TRUE)
  if (!all(is.finite(rng))) return(NULL)
  if (diff(rng) == 0) rng <- rng + c(-1, 1)
  sx <- function(v) max(0, min(100, (v - rng[1]) / diff(rng) * 100))
  x0 <- sx(0)

  rows <- lapply(seq_len(nrow(d)), function(i) {
    est <- d$estimate[i]; l <- lo[i]; h <- hi[i]
    p <- if ("p.value" %in% names(d)) d$p.value[i] else NA_real_
    sig <- !is.na(p) && p < 0.05
    col <- if (!sig) "var(--blockr-grey-400, #9ca3af)"
           else if (est >= 0) "var(--blockr-blue-600, #2563eb)"
           else "var(--blockr-color-danger, #dc3545)"
    wl <- sx(l); wr <- sx(h)

    tags$tr(
      tags$td(class = "smb-fterm", d$term[i]),
      tags$td(class = "smb-fbar",
        tags$div(class = "smb-track",
          tags$div(class = "smb-ref", style = paste0("left:", x0, "%;")),
          tags$div(class = "smb-whisk",
            style = paste0("left:", wl, "%; width:", max(0, wr - wl), "%; background:", col, ";")),
          tags$div(class = "smb-dot",
            style = paste0("left:", sx(est), "%; background:", col, ";")))),
      tags$td(class = "smb-fval", sprintf("%.3g", est)))
  })

  tags$table(class = "smb-forest", tags$tbody(rows))
}

# --- collapsible details: coef / stats / diagnostics ------------------------
smb_details <- function(tidy_df, glance_df, model, digits) {
  tags$details(class = "smb-details",
    tags$summary(class = "smb-summary", "Fit & diagnostics"),
    tags$div(class = "smb-panel",
      smb_column("Coefficients", smb_coefs(tidy_df, digits)),
      smb_column("Statistics",   smb_stats(glance_df, model, digits)),
      smb_column("Diagnostics",  smb_tests(model))))
}

smb_column <- function(title, body) {
  tags$div(class = "smb-col",
    tags$div(class = "smb-col-head", title), body)
}
smb_empty_col <- function(msg) tags$div(class = "smb-col-empty", msg)

smb_coefs <- function(tidy_df, digits) {
  if (is.null(tidy_df) || !nrow(tidy_df)) return(smb_empty_col("No coefficients"))
  if ("time" %in% names(tidy_df) && nrow(tidy_df) > 20) {
    return(smb_empty_col("Curve estimate (see plot)"))
  }
  has_est <- "estimate" %in% names(tidy_df)
  rows <- lapply(seq_len(nrow(tidy_df)), function(i) {
    row <- tidy_df[i, ]
    p <- if ("p.value" %in% names(row)) row$p.value else NA_real_
    tags$tr(
      tags$td(class = "smb-term", row$term),
      tags$td(class = "smb-est", if (has_est) sprintf("%.3g", row$estimate) else ""),
      tags$td(class = "smb-sig", smb_sig_badge(p)))
  })
  tags$table(class = "smb-tbl", tags$tbody(rows))
}

smb_sig_badge <- function(p) {
  if (is.na(p)) return("")
  spec <- if (p < 0.001) list("0.1%", "var(--blockr-blue-600, #2563eb)")
          else if (p < 0.01) list("1%", "#0d9488")
          else if (p < 0.05) list("5%", "var(--blockr-grey-500, #6b7280)")
          else if (p < 0.1)  list("10%", "var(--blockr-grey-400, #9ca3af)")
          else return("")
  tags$span(class = "smb-badge", style = paste0("background:", spec[[2]], ";"), spec[[1]])
}

smb_stats <- function(glance_df, model, digits) {
  items <- list(c("Model", class(model)[1]))
  g <- function(nm) if (!is.null(glance_df) && nm %in% names(glance_df)) glance_df[[nm]][1] else NULL
  add <- function(items, label, val, fmt = "%.4g") {
    if (is.null(val) || (is.numeric(val) && !is.finite(val))) return(items)
    c(items, list(c(label, if (is.numeric(val)) sprintf(fmt, val) else as.character(val))))
  }
  n <- tryCatch(stats::nobs(model), error = function(e) NULL)
  if (is.numeric(n) && length(n) == 1L) items <- add(items, "Observations", n, "%d")
  items <- add(items, "R²",      g("r.squared"))
  items <- add(items, "Adj. R²", g("adj.r.squared"))
  items <- add(items, "Residual SE",  g("sigma"))
  items <- add(items, "F-statistic",  g("statistic"), "%.2f")
  items <- add(items, "AIC",          g("AIC"), "%.1f")

  if (length(items) <= 1L && is.null(glance_df)) return(smb_empty_col("No statistics"))
  tags$table(class = "smb-tbl",
    tags$tbody(lapply(items, function(it)
      tags$tr(tags$td(class = "smb-k", it[1]), tags$td(class = "smb-v", it[2])))))
}

smb_tests <- function(model) {
  resid <- tryCatch(stats::residuals(model), error = function(e) NULL)
  fitted <- tryCatch(stats::fitted(model), error = function(e) NULL)
  tests <- list()
  if (!is.null(resid) && length(resid) >= 3) {
    rs <- if (length(resid) > 5000) resid[seq_len(5000)] else resid
    sw <- tryCatch(stats::shapiro.test(rs), error = function(e) NULL)
    if (!is.null(sw)) tests <- c(tests, list(list(
      name = "Shapiro-Wilk", h = "normal residuals",
      stat = sprintf("W=%.3f", sw$statistic), p = sw$p.value)))
    dw <- sum(diff(resid)^2) / sum(resid^2)
    tests <- c(tests, list(list(
      name = "Durbin-Watson", h = "no autocorrelation",
      stat = sprintf("DW=%.2f", dw), p = NA_real_)))
    if (!is.null(fitted) && length(fitted) == length(resid)) {
      bp <- tryCatch(stats::cor.test(fitted, resid^2), error = function(e) NULL)
      if (!is.null(bp)) tests <- c(tests, list(list(
        name = "Heteroscedasticity", h = "constant variance",
        stat = sprintf("r=%.2f", bp$estimate), p = bp$p.value)))
    }
  }
  if (!length(tests)) return(smb_empty_col("Not available"))
  tags$table(class = "smb-tbl",
    tags$tbody(lapply(tests, function(t)
      tags$tr(
        tags$td(class = "smb-test",
          tags$div(tags$strong(t$name)),
          tags$div(class = "smb-test-h", t$h)),
        tags$td(class = "smb-v", t$stat),
        tags$td(class = "smb-sig", smb_verdict_badge(t$p))))))
}

smb_verdict_badge <- function(p) {
  spec <- if (is.na(p)) list("?", "var(--blockr-grey-400, #9ca3af)", "#fff")
          else if (p < 0.01) list("Reject", "var(--blockr-color-danger, #dc3545)", "#fff")
          else if (p < 0.05) list("Caution", "#f59e0b", "#1f2937")
          else list("OK", "#16a34a", "#fff")
  tags$span(class = "smb-badge",
    style = paste0("background:", spec[[2]], "; color:", spec[[3]], ";"), spec[[1]])
}
