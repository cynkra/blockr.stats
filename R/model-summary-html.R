#' Minimal HTML model preview
#'
#' The model / survival block's `block_output` preview: a small card with
#' a Visual / R toggle (default Visual). The visual side is a headline
#' (model kind, n) + fit-quality chip and a coefficient **forest plot**
#' (estimate dot + CI whisker, reference line at 0, coloured by sign /
#' significance). The R side is the raw `summary()` text. Deliberately
#' minimal — richer views (coefficient tables, fit indices, diagnostics)
#' are separate downstream blocks (broom adapter + drilldown renderers),
#' not crammed into the preview.
#'
#' Generic across `lm` / `glm` / `aov` / `coxph` via broom; pieces degrade
#' gracefully when a model lacks them.
#'
#' @param model A fitted model object (or `NULL`).
#' @param conf_level Confidence level for the coefficient CIs.
#' @param id Unique id for the toggle's radio group (auto-generated).
#' @return An [htmltools::tagList()] / tag.
#' @noRd
model_summary_html <- function(model, conf_level = 0.95, id = NULL) {
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

  # class-keyed radio toggle: Visual (default) vs R text, no server round-trip
  vid <- paste0(id, "-v"); rid <- paste0(id, "-r")
  tags$div(class = "smb-card",
    tags$input(type = "radio", name = id, id = vid, class = "smb-radio smb-radio-v",
               checked = NA),
    tags$input(type = "radio", name = id, id = rid, class = "smb-radio smb-radio-r"),
    tags$div(class = "smb-switch",
      tags$label(`for` = vid, class = "smb-seg smb-seg-v", "Visual"),
      tags$label(`for` = rid, class = "smb-seg smb-seg-r", "R")),
    # headline (kind, n, fit chip) belongs to the visual view only
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
  # one fit measure only; adjusted R² preferred over raw R² for lm
  if (!is.null(g("adj.r.squared")))    spec <- list("adj. R²", g("adj.r.squared"), g("adj.r.squared"))
  else if (!is.null(g("r.squared")))   spec <- list("R²", g("r.squared"), g("r.squared"))
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
