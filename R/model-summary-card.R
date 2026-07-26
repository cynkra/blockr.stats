#' Render a model summary as an HTML card
#'
#' Draws the value of [model_summary()]: a one-line model facts stripe over a
#' coefficient table whose first column is an inline forest (estimate dot,
#' uncertainty whisker, reference line, shared axis). Which columns appear is
#' decided by the display options carried on the object, so the card and the
#' value can never disagree.
#'
#' The scale is set by the plotted terms only. The intercept is kept out of it
#' and, when it falls outside, is drawn as an arrow at the edge rather than
#' silently squashing every other term into a smear.
#'
#' @param res The result of [model_summary()] (or any tidy coefficient frame).
#' @return An [htmltools::tagList()] / tag.
#' @examples
#' model_summary_card(model_summary(lm(mpg ~ wt + hp, mtcars)))
#' @export
model_summary_card <- function(res) {
  if (is.null(res)) {
    return(tags$div(class = "msc-card msc-empty", "Connect a fitted model."))
  }
  opts <- attr(res, "ms_opts") %||% ms_defaults
  facts <- if (isTRUE(opts$facts)) ms_facts_line(res) else NULL

  if (!isTRUE(attr(res, "ms_has_terms"))) {
    return(tags$div(
      class = "msc-card", facts,
      tags$div(
        class = "msc-note",
        paste(
          "This model has no coefficients to summarise:",
          "tidy() describes it as curve points rather than terms.",
          "Use a chart block downstream to draw the curve."
        )
      )
    ))
  }

  tags$div(class = "msc-card", facts, ms_table(res, opts))
}

# --- facts stripe ----------------------------------------------------------

# S2 from dev/model-summary-line-proposals.html: one line, two jobs. What the
# model IS sits left (kind, n); how well it FITS sits right, where the numbers
# line up with the table's numeric columns. The dot-separated run it replaces
# put five facts of four different kinds at one pitch, so nothing was findable.
ms_facts_line <- function(res) {
  facts <- ms_facts(res)
  if (!length(facts)) return(NULL)

  # facts without a label are the identity (kind, n obs); the labelled ones
  # are the fit measures.
  labelled <- vapply(facts, function(f) nzchar(f[[1L]]), logical(1L))
  identity <- facts[!labelled]
  fit <- facts[labelled]

  tags$div(
    class = "msc-facts",
    if (length(identity)) {
      tags$span(
        class = "msc-id",
        lapply(seq_along(identity), function(i) {
          val <- ms_fact_value(identity[[i]])
          if (i == 1L) val else tagList(
            tags$span(class = "msc-sep", "\u00b7"),
            tags$span(class = "msc-n", val)
          )
        })
      )
    },
    if (length(fit)) {
      tags$span(
        class = "msc-fit",
        lapply(fit, function(f) {
          tags$span(class = "msc-pair", f[[1L]], " ",
                    tags$b(ms_fact_value(f)))
        })
      )
    }
  )
}

ms_fact_value <- function(f) {
  if (is.character(f[[2L]])) {
    f[[2L]]
  } else if (is.na(f[[3L]])) {
    ms_fmt(f[[2L]], ms_decimals(f[[2L]]))
  } else {
    ms_fmt(f[[2L]], f[[3L]])
  }
}

# --- the coefficient table -------------------------------------------------

ms_table <- function(res, opts) {
  d <- as.data.frame(res)
  ratio <- identical(opts$scale, "ratio")
  ref <- if (ratio) 1 else 0

  est <- ms_num(d$estimate)
  bounds <- ms_bounds(d, opts$uncertainty, ratio)
  is_int <- d$term == "(Intercept)"
  plotted <- !is_int
  if (!any(plotted)) plotted <- rep(TRUE, nrow(d))

  geom <- if (isTRUE(opts$effect_column)) {
    ms_geom(bounds$lo[plotted], bounds$hi[plotted], est[plotted], ratio)
  }

  # The intercept is excluded from the format the way it is excluded from the
  # scale: one nuisance term in the thousands must not decide how the terms
  # anyone reads are rounded.
  est_dec <- ms_decimals(est[plotted])
  int_dec <- ms_decimals(c(bounds$lo[plotted], bounds$hi[plotted]))
  fmt_est <- if (ratio) ms_fmt_sig else function(v) ms_fmt(v, est_dec)
  fmt_int <- if (ratio) ms_fmt_sig else function(v) ms_fmt(v, int_dec)

  labels <- attr(res, "ms_labels")
  p <- if ("p.value" %in% names(d)) ms_num(d$p.value) else rep(NA_real_, nrow(d))

  head_est <- if (ratio) attr(res, "ms_ratio_label") %||% "Ratio" else "Estimate"
  head_int <- ms_interval_header(opts)
  head_sig <- switch(opts$significance, p = "p", stars = "", chips = "", NULL)

  # The intercept goes last. Not a re-ranking of the terms (they keep model
  # order): it is the baseline rather than an effect, it is out of the shared
  # scale, and reading starts with the terms someone asked a question about.
  ord <- c(which(!is_int), which(is_int))

  rows <- lapply(seq_along(ord), function(k) {
    i <- ord[k]
    tags$tr(
      class = if (is_int[i]) "msc-int",
      # Emission order, so a browser-side sort can always be undone back to
      # the model's own order.
      `data-ms-row` = k,
      # Sort keys as data, not as rendered text: the cells carry a unicode
      # minus, "<0.001" and an en-dash range, none of which sort correctly as
      # strings. The intercept flag keeps the baseline pinned last in every
      # order -- it is not a competitor to the terms.
      `data-ms-term` = tolower(d$term[i]),
      `data-ms-estimate` = if (is.finite(est[i])) est[i],
      `data-ms-significance` = if (is.finite(p[i])) p[i],
      `data-ms-pin` = if (is_int[i]) "last",
      tags$td(class = "msc-term", ms_term_cell(d$term[i], labels[[i]])),
      if (!is.null(geom)) {
        tags$td(class = "msc-eff",
          ms_effect_cell(est[i], bounds$lo[i], bounds$hi[i], p[i], geom, ref))
      },
      tags$td(class = "msc-num", fmt_est(est[i])),
      if (!is.null(head_int)) {
        tags$td(class = "msc-num", ms_interval_cell(bounds, i, fmt_int))
      },
      if (!is.null(head_sig)) tags$td(class = "msc-sig", ms_sig_cell(p[i], opts))
    )
  })

  tags$table(
    class = "msc-ct blockr-table",
    tags$thead(tags$tr(
      # The blockr column header (blockr.ui's table preview + viz's table
      # block): 14px medium in primary ink, numeric columns right-aligned.
      # No sub-label tier -- the interval belongs in the name it qualifies.
      ms_th("Term", "term", sortable = TRUE),
      if (!is.null(geom)) ms_th(ms_effect_header(opts), "effect"),
      ms_th(head_est, "estimate", numeric = TRUE, sortable = TRUE),
      if (!is.null(head_int)) ms_th(head_int, "interval", numeric = TRUE),
      if (!is.null(head_sig)) {
        ms_th(head_sig, "significance", numeric = TRUE, sortable = TRUE,
              class = "msc-sig")
      }
    )),
    tags$tbody(rows),
    if (!is.null(geom)) ms_axis_row(geom)
  )
}

# One column header. `sortable` marks the columns the browser can reorder on
# (see inst/js/model-summary-sort.js): term alphabetically, estimate by size,
# p by strength of evidence. The sort is a reading aid held in the browser, so
# the header carries only what JS needs to do it.
ms_th <- function(label, key, numeric = FALSE, sortable = FALSE, class = NULL) {
  cls <- c(
    "blockr-col-header",
    if (numeric) "dt-col-num" else "dt-col-txt",
    if (sortable) "blockr-sortable",
    class
  )
  tags$th(
    class = paste(cls, collapse = " "),
    `data-ms-sort` = if (sortable) key,
    tags$span(
      class = "dt-th-namerow",
      tags$span(class = "blockr-col-name", label),
      if (sortable) tags$span(class = "blockr-sort-icon")
    )
  )
}

# The whisker's ends: a confidence interval from broom, or estimate +/- one
# standard error, or nothing at all.
#
# On the ratio scale the SE stays on the LOG scale -- that is what broom
# returns and what the standard error of a log coefficient means -- so the
# whisker has to be multiplicative: est / exp(se) to est * exp(se). Adding it
# would put the lower end below zero for any ratio near 0 and silently drop
# the whisker, or draw a plausible-looking wrong one for a ratio near 1.
ms_bounds <- function(d, uncertainty, ratio = FALSE) {
  n <- nrow(d)
  na <- rep(NA_real_, n)
  if (uncertainty == "se") {
    if (!"std.error" %in% names(d)) return(list(lo = na, hi = na, kind = "none"))
    est <- ms_num(d$estimate)
    se <- ms_num(d$std.error)
    if (ratio) {
      return(list(lo = est / exp(se), hi = est * exp(se), se = se, kind = "se"))
    }
    return(list(lo = est - se, hi = est + se, se = se, kind = "se"))
  }
  if (uncertainty == "none" || !all(c("conf.low", "conf.high") %in% names(d))) {
    return(list(lo = na, hi = na, kind = "none"))
  }
  list(lo = ms_num(d$conf.low), hi = ms_num(d$conf.high), kind = "ci")
}

ms_interval_header <- function(opts) {
  ratio <- identical(opts$scale, "ratio")
  switch(
    opts$uncertainty,
    # On the ratio scale the number in this column is the standard error of the
    # log coefficient, not of the ratio: say so rather than let it read as
    # "plus or minus this many hazard ratios".
    se = if (ratio) "SE (log)" else "SE",
    none = NULL,
    paste0(round(opts$conf_level * 100), "% CI")
  )
}

ms_effect_header <- function(opts) {
  switch(
    opts$uncertainty,
    se = "Effect (\u00b1 SE)",
    none = "Effect",
    paste0("Effect (", round(opts$conf_level * 100), "% CI)")
  )
}

ms_interval_cell <- function(bounds, i, fmt) {
  if (identical(bounds$kind, "se")) return(fmt(bounds$se[i]))
  if (!is.finite(bounds$lo[i]) || !is.finite(bounds$hi[i])) return("")
  paste0(fmt(bounds$lo[i]), " \u2013 ", fmt(bounds$hi[i]))
}

ms_term_cell <- function(term, label) {
  if (is.null(label) || !nzchar(label$level)) {
    return(if (is.null(label)) term else label$var)
  }
  tagList(label$var, tags$span(class = "msc-lvl", paste0(" ", label$level)))
}

# --- significance ----------------------------------------------------------

ms_sig_cell <- function(p, opts) {
  if (!is.finite(p)) return("")
  switch(
    opts$significance,
    p = ms_fmt_p(p),
    stars = ms_stars(p),
    chips = {
      lab <- ms_chip_label(p)
      if (is.null(lab)) {
        ""
      } else {
        step <- if (p < 0.001) {
          ""
        } else if (p < 0.01) {
          " msc-chip--1"
        } else if (p < 0.05) {
          " msc-chip--5"
        } else {
          " msc-chip--10"
        }
        tags$span(class = paste0("msc-chip", step), lab)
      }
    },
    ""
  )
}

# Four levels, matching the stars: 0.1% / 1% / 5% are the conventional
# thresholds and all three are coloured, because in most applied fields 5% IS
# the line attention is paid to. The 10% level (R's ".") gets a chip too, but a
# grey one -- it is worth marking as "borderline", not as a result.
ms_chip_label <- function(p) {
  if (p < 0.001) {
    "0.1%"
  } else if (p < 0.01) {
    "1%"
  } else if (p < 0.05) {
    "5%"
  } else if (p < 0.1) {
    "10%"
  } else {
    NULL
  }
}

ms_stars <- function(p) {
  if (p < 0.001) "***" else if (p < 0.01) "**" else if (p < 0.05) "*" else
    if (p < 0.1) "." else ""
}

ms_fmt_p <- function(p) {
  if (!is.finite(p)) return("")
  if (p < 0.001) "<0.001" else formatC(p, format = "f", digits = 3)
}

# --- forest geometry -------------------------------------------------------

# One scale for the whole column, from the plotted terms only. On the ratio
# scale positions are logarithmic, so a doubling and a halving sit the same
# distance either side of 1.
ms_geom <- function(lo, hi, est, ratio) {
  ref <- if (ratio) 1 else 0
  tf <- if (ratio) function(v) log(pmax(v, .Machine$double.xmin)) else identity
  vals <- c(lo, hi, est)
  vals <- vals[is.finite(vals) & (!ratio | vals > 0)]
  if (!length(vals)) return(NULL)

  rng <- range(c(tf(vals), tf(ref)), na.rm = TRUE)
  if (!all(is.finite(rng))) return(NULL)
  if (diff(rng) == 0) rng <- rng + c(-1, 1)
  rng <- rng + c(-1, 1) * diff(rng) * 0.06

  pos <- function(v) {
    if (!is.finite(v) || (ratio && v <= 0)) return(NA_real_)
    100 * (tf(v) - rng[1L]) / diff(rng)
  }
  list(
    pos = pos,
    ref = pos(ref),
    ref_value = ref,
    ratio = ratio,
    lo_value = if (ratio) exp(rng[1L]) else rng[1L],
    hi_value = if (ratio) exp(rng[2L]) else rng[2L]
  )
}

ms_effect_cell <- function(est, lo, hi, p, geom, ref) {
  x_est <- geom$pos(est)
  x_lo <- geom$pos(lo)
  x_hi <- geom$pos(hi)

  # Off scale (only the intercept can be, it is excluded from the range):
  # an arrow at the edge it ran off, never a rescale of everyone else.
  if (!is.finite(x_est) || x_est < 0 || x_est > 100) {
    side <- if (!is.finite(x_est) || x_est < 0) "left" else "right"
    return(tags$div(class = "msc-track",
      tags$div(class = "msc-ref", style = ms_left(geom$ref)),
      tags$div(
        class = paste0("msc-off msc-off--", side),
        style = if (side == "left") "left:0;" else "right:0;",
        title = "off scale",
        if (side == "left") "\u25c0" else "\u25b6"
      )))
  }

  col <- ms_colour(est, lo, hi, p, ref)
  has_whisk <- is.finite(x_lo) && is.finite(x_hi)
  tags$div(
    class = "msc-track",
    tags$div(class = "msc-ref", style = ms_left(geom$ref)),
    if (has_whisk) {
      tags$div(
        class = "msc-whisk",
        style = paste0(ms_left(max(0, x_lo)), "width:",
                       ms_pct(min(100, x_hi) - max(0, x_lo)),
                       ";background:", col, ";")
      )
    },
    tags$div(class = "msc-dot",
             style = paste0(ms_left(x_est), "background:", col, ";"))
  )
}

# Grey means "not distinguishable from no effect", and the p-value decides it
# whenever there is one -- so the colour means the same thing in every state of
# the Uncertainty option. Reading it off the drawn interval instead would make
# the colour follow the whisker: a 68% (+/- SE) whisker clear of the reference
# would paint a term with p = 0.064 as a real effect. With the default 95% CI
# the two rules agree by construction.
ms_colour <- function(est, lo, hi, p, ref) {
  covers <- if (is.finite(p)) {
    p >= 0.05
  } else if (is.finite(lo) && is.finite(hi)) {
    lo <= ref && hi >= ref
  } else {
    FALSE
  }
  # The MARK grey, one step lighter than the quiet-text grey on purpose: this
  # is a third data colour beside the blue and the red, and it has to recede
  # from both. Grey text elsewhere in the card means "quieter"; grey here means
  # "no effect", and keeping the two tones apart keeps the two meanings apart.
  if (covers) return("var(--blockr-color-text-subtle, #9ca3af)")
  if (est >= ref) "var(--blockr-blue-600, #2563eb)" else
    "var(--blockr-color-danger, #dc2626)"
}

ms_axis_row <- function(geom) {
  labs <- c(geom$lo_value, geom$ref_value, geom$hi_value)
  at <- c(0, geom$ref, 100)
  # When the reference sits near an end, its label and the end label would
  # overprint each other. The reference is the one that has to be readable,
  # so the end label goes.
  keep <- c(at[2L] > 16, TRUE, at[2L] < 84)
  tags$tfoot(tags$tr(
    tags$td(),
    tags$td(class = "msc-eff", tags$div(
      class = "msc-axis",
      lapply(which(keep), function(i) {
        tags$span(
          class = if (i == 2L) "msc-axis-ref",
          style = ms_left(at[i]),
          ms_fmt_axis(labs[i])
        )
      })
    )),
    tags$td(colspan = 3)
  ))
}

ms_left <- function(x) paste0("left:", ms_pct(x), ";")

ms_pct <- function(x) paste0(round(max(0, min(100, x)), 2), "%")

# --- number formatting -----------------------------------------------------

ms_num <- function(x) {
  if (is.null(x)) return(numeric(0))
  suppressWarnings(as.numeric(x))
}

# One decimal count for the whole column, so the numbers line up and can be
# compared down the page. It comes from the column's TYPICAL magnitude (the
# median), not the smallest value: a single tiny coefficient among large ones
# would otherwise drag every row to four decimals of trailing zeros. A floor
# then guarantees the smallest value still shows a significant digit rather
# than rendering as 0.00. No digits spinner: the data decides.
ms_decimals <- function(x) {
  x <- x[is.finite(x) & x != 0]
  if (!length(x)) return(2L)
  typical <- floor(log10(stats::median(abs(x)))) + 1L
  d <- max(0L, min(4L, 4L - typical))
  smallest <- floor(log10(min(abs(x)))) + 1L
  as.integer(max(d, min(6L, 1L - smallest)))
}

# Axis ticks are read at a glance, not compared digit by digit: two
# significant digits, and the reference tick prints as the bare 0 or 1 it is.
ms_fmt_axis <- function(v) {
  if (!is.finite(v)) return("")
  if (v == 0) return("0")
  av <- abs(v)
  if (av < 1e-3 || av >= 1e5) {
    return(ms_minus(format(signif(v, 2), scientific = TRUE)))
  }
  v <- signif(v, 2)
  ms_fmt(v, max(0L, 2L - (floor(log10(abs(v))) + 1L)))
}

ms_fmt <- function(x, digits = 2L) {
  if (!length(x) || !is.finite(x)) return("")
  ms_minus(formatC(x, format = "f", digits = digits, big.mark = ""))
}

# Significant digits, per value. Ratios live on a multiplicative scale where a
# shared decimal count is meaningless: an odds ratio of 0.0003 and one of 1.04
# are both three digits of information, and fixed decimals would print one of
# them as 0.0000 or the other as 156145483.5063.
ms_fmt_sig <- function(x, sig = 3L) {
  if (!length(x) || !is.finite(x)) return("")
  if (x == 0) return("0")
  ax <- abs(x)
  if (ax < 1e-3 || ax >= 1e5) {
    return(ms_minus(format(signif(x, sig), scientific = TRUE)))
  }
  ms_fmt(x, max(0L, min(6L, sig - (floor(log10(ax)) + 1L))))
}

ms_minus <- function(x) sub("^-", "\u2212", x)
