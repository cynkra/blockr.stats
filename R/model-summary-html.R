#' Generic HTML model card
#'
#' Render any fitted model as a three-column panel: coefficients (term,
#' estimate, significance badge), curated fit statistics, and diagnostic
#' checks (normality / autocorrelation / heteroscedasticity with verdict
#' badges). Generic across `lm` / `glm` / `aov` / `coxph` via broom; the
#' diagnostics column degrades gracefully when residuals aren't available.
#' Used as the `block_output` preview of the model and survival blocks.
#'
#' @param model A fitted model object (or `NULL`).
#' @param conf_level Confidence level for the coefficient CIs.
#' @param digits Significant digits for numeric formatting.
#' @return An [htmltools::tagList()].
#' @noRd
model_summary_html <- function(model, conf_level = 0.95, digits = 3) {
  if (is.null(model)) {
    return(tags$div(class = "smb-card smb-empty", "Pick variables to fit a model."))
  }

  tidy_df <- tryCatch(
    broom::tidy(model, conf.int = TRUE, conf.level = conf_level),
    error = function(e) tryCatch(broom::tidy(model), error = function(e) NULL)
  )
  glance_df <- tryCatch(broom::glance(model), error = function(e) NULL)

  tags$div(
    class = "smb-card",
    smb_column("Coefficients", smb_coefs(tidy_df)),
    smb_column("Statistics",   smb_stats(glance_df, model, digits)),
    smb_column("Diagnostics",  smb_tests(model))
  )
}

smb_column <- function(title, body) {
  tags$div(class = "smb-col",
    tags$div(class = "smb-col-head", title),
    body)
}

smb_empty_col <- function(msg) {
  tags$div(class = "smb-col-empty", msg)
}

# --- coefficients: term . estimate . significance badge ---------------------
smb_coefs <- function(tidy_df) {
  if (is.null(tidy_df) || !nrow(tidy_df)) return(smb_empty_col("No coefficients"))
  # survfit/cif tidy is curve points, not coefficients
  if ("time" %in% names(tidy_df) && nrow(tidy_df) > 20) {
    return(smb_empty_col("Curve estimate (see plot)"))
  }

  has_est <- "estimate" %in% names(tidy_df)
  rows <- lapply(seq_len(nrow(tidy_df)), function(i) {
    row <- tidy_df[i, ]
    p <- if ("p.value" %in% names(row)) row$p.value else NA_real_
    tags$tr(
      tags$td(class = "smb-term", row$term),
      tags$td(class = "smb-est", if (has_est) sprintf("%.2f", row$estimate) else ""),
      tags$td(class = "smb-sig", smb_sig_badge(p))
    )
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

# --- statistics: curated key/value rows -------------------------------------
smb_stats <- function(glance_df, model, digits) {
  items <- list(c("Model", class(model)[1]))
  g <- function(nm) if (!is.null(glance_df) && nm %in% names(glance_df)) glance_df[[nm]][1] else NULL

  add <- function(items, label, val, fmt = "%.4g") {
    if (is.null(val) || (is.numeric(val) && !is.finite(val))) return(items)
    c(items, list(c(label, if (is.numeric(val)) sprintf(fmt, val) else as.character(val))))
  }
  n <- tryCatch(stats::nobs(model), error = function(e) NULL)
  if (is.numeric(n) && length(n) == 1L) items <- add(items, "Observations", n, "%d")
  items <- add(items, "R²",        g("r.squared"))
  items <- add(items, "Adj. R²",   g("adj.r.squared"))
  items <- add(items, "Residual SE",    g("sigma"))
  items <- add(items, "F-statistic",    g("statistic"), "%.2f")
  items <- add(items, "AIC",            g("AIC"), "%.1f")

  if (length(items) <= 1L && is.null(glance_df)) return(smb_empty_col("No statistics"))

  tags$table(class = "smb-tbl",
    tags$tbody(lapply(items, function(it)
      tags$tr(tags$td(class = "smb-k", it[1]),
              tags$td(class = "smb-v", it[2])))))
}

# --- diagnostics: residual-based checks with verdict badges -----------------
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
    style = paste0("background:", spec[[2]], "; color:", spec[[3]], ";"),
    spec[[1]])
}
