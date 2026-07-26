# model_summary() is the value behind the summary block; the card renders that
# value and nothing else, so these tests pin the contract between them.

test_that("model_summary tidies a model and carries the facts", {
  res <- model_summary(lm(mpg ~ wt + hp, mtcars))

  expect_s3_class(res, "model_summary")
  expect_s3_class(res, "data.frame")
  expect_true(all(c("term", "estimate", "conf.low", "conf.high") %in% names(res)))
  expect_equal(attr(res, "ms_kind"), "Linear model")
  expect_equal(attr(res, "ms_nobs"), 32L)
  expect_equal(attr(res, "ms_opts")$scale, "raw")
})

test_that("intercept = FALSE drops the row from the value, not just the card", {
  res <- model_summary(lm(mpg ~ wt, mtcars), intercept = FALSE)
  expect_false("(Intercept)" %in% res$term)
})

test_that("uncertainty picks the confidence level and the SE branch", {
  wide <- model_summary(lm(mpg ~ wt, mtcars), uncertainty = "ci99")
  narrow <- model_summary(lm(mpg ~ wt, mtcars), uncertainty = "ci90")
  expect_true(all(wide$conf.high - wide$conf.low >
                    narrow$conf.high - narrow$conf.low))

  none <- model_summary(lm(mpg ~ wt, mtcars), uncertainty = "none")
  expect_false("conf.low" %in% names(none))
})

test_that("auto scale exponentiates a logit model but not a linear one", {
  lin <- model_summary(lm(mpg ~ wt, mtcars))
  expect_equal(attr(lin, "ms_opts")$scale, "raw")

  logit <- model_summary(glm(am ~ wt, mtcars, family = binomial()))
  expect_equal(attr(logit, "ms_opts")$scale, "ratio")
  expect_equal(attr(logit, "ms_ratio_label"), "OR")
  # exponentiated coefficients are positive by construction
  expect_true(all(logit$estimate > 0))

  forced <- model_summary(lm(mpg ~ wt, mtcars), scale = "ratio")
  expect_equal(attr(forced, "ms_opts")$scale, "ratio")
  expect_true(all(forced$estimate > 0))
})

test_that("a Cox fit reports patients, not events, as n", {
  fit <- survival::coxph(
    survival::Surv(time, status) ~ age + sex, survival::lung
  )
  res <- model_summary(fit)
  # nobs() on a coxph counts events (165); the card must show the 228 patients
  expect_equal(attr(res, "ms_nobs"), 228L)
  expect_equal(attr(res, "ms_ratio_label"), "HR")
  facts <- vapply(ms_facts(res), ms_fact_text, character(1L))
  expect_true(any(grepl("concordance", facts)))
  expect_true(any(grepl("events 165", facts)))
  expect_false(any(grepl("R\u00b2", facts)))
})

test_that("a Kaplan-Meier fit degrades instead of drawing curve points", {
  fit <- survival::survfit(
    survival::Surv(time, status) ~ sex, survival::lung
  )
  res <- model_summary(fit)
  expect_false(isTRUE(attr(res, "ms_has_terms")))

  html <- as.character(model_summary_card(res))
  expect_match(html, "no coefficients")
  # broom refuses to glance a multi-strata survfit: the facts still appear
  expect_match(html, "228 obs")
  expect_match(html, "events")
})

test_that("a tidy frame renders without a model", {
  tidy_df <- broom::tidy(lm(mpg ~ wt, mtcars), conf.int = TRUE)
  res <- model_summary(tidy_df)
  expect_true(isTRUE(attr(res, "ms_has_terms")))
  expect_null(attr(res, "ms_kind"))
  expect_match(as.character(model_summary_card(res)), "msc-ct")
})

test_that("term labels split the factor level off the variable name", {
  fit <- lm(mpg ~ wt + factor(cyl), mtcars)
  labs <- attr(model_summary(fit), "ms_labels")
  terms <- vapply(labs, `[[`, character(1L), "var")
  levels <- vapply(labs, `[[`, character(1L), "level")
  expect_equal(levels[terms == "factor(cyl)"], c("6", "8"))
  expect_equal(levels[terms == "wt"], "")
})

test_that("the card honours the display options", {
  fit <- lm(mpg ~ wt + hp, mtcars)

  full <- as.character(model_summary_card(model_summary(fit)))
  expect_match(full, "msc-facts")
  expect_match(full, "msc-track")

  bare <- as.character(model_summary_card(
    model_summary(fit, effect_column = FALSE, facts = FALSE,
                  significance = "chips")
  ))
  expect_false(grepl("msc-facts", bare))
  expect_false(grepl("msc-track", bare))
  expect_match(bare, "msc-chip")

  stars <- as.character(model_summary_card(
    model_summary(fit, significance = "stars")
  ))
  expect_match(stars, "\\*\\*\\*")
})

test_that("the intercept is last, off the scale, and out of the format", {
  # wt and hp are single digits, the intercept is in the thirties: if the
  # intercept drove the shared decimal count, hp's 0.03 would round to 0.0
  fit <- lm(mpg ~ wt + hp, mtcars)
  html <- as.character(model_summary_card(model_summary(fit)))

  expect_lt(regexpr("wt", html), regexpr("Intercept", html))
  expect_match(html, "msc-off")
  expect_match(html, "\u22120.0")
})

test_that("number formatting adapts to magnitude and scale", {
  # shared decimals off the typical term, not the smallest
  expect_equal(ms_decimals(c(40.71, 266.81, -206.51)), 1L)
  expect_equal(ms_decimals(c(-3.87, -0.0318, -3.36)), 3L)
  # a value that would otherwise vanish keeps a digit
  expect_gte(ms_decimals(c(0.0004, 5)), 4L)

  # ratios are multiplicative: significant digits, not decimals
  expect_equal(ms_fmt_sig(1.0369), "1.04")
  expect_match(ms_fmt_sig(156145483), "e\\+08")
  expect_match(ms_fmt_sig(0.000309), "e\\-04")

  expect_equal(ms_fmt_axis(0), "0")
  expect_equal(ms_fmt_axis(-8.1737), "\u22128.2")
})

test_that("the chip ladder marks 5% as a result and 10% as borderline", {
  expect_equal(ms_chip_label(0.0001), "0.1%")
  expect_equal(ms_chip_label(0.005), "1%")
  expect_equal(ms_chip_label(0.03), "5%")
  expect_equal(ms_chip_label(0.08), "10%")
  expect_null(ms_chip_label(0.2))

  chip <- function(p) {
    as.character(ms_sig_cell(p, list(significance = "chips")))
  }
  # the three significant levels are coloured and distinct; 10% is the
  # neutral grey badge, so it reads as borderline rather than as a finding
  expect_false(grepl("msc-chip--", chip(0.0001)))
  expect_match(chip(0.005), "msc-chip--1\\b")
  expect_match(chip(0.03), "msc-chip--5\\b")
  expect_match(chip(0.08), "msc-chip--10\\b")
  expect_equal(chip(0.2), "")
})

test_that("the card uses blockr's column header, not its own", {
  html <- as.character(model_summary_card(model_summary(lm(mpg ~ wt, mtcars))))

  # the house recipe (blockr.ui table preview / viz table block)
  expect_match(html, "blockr-col-name")
  expect_match(html, "blockr-col-header")
  expect_match(html, "dt-col-num")
  # no sub-label tier, and no invented micro-caps header
  expect_false(grepl("blockr-col-label", html))
  expect_false(grepl("msc-eff-h", html))
})

test_that("sorting is browser-side only: keys in the DOM, model order in the value", {
  res <- model_summary(lm(mpg ~ wt + hp, mtcars))
  html <- as.character(model_summary_card(res))

  # the three sortable columns announce themselves
  expect_match(html, "data-ms-sort=\"term\"")
  expect_match(html, "data-ms-sort=\"estimate\"")
  expect_match(html, "data-ms-sort=\"significance\"")
  # rows carry numeric sort keys, because the rendered text has a unicode
  # minus and "<0.001" and would sort wrongly as strings
  expect_match(html, "data-ms-estimate")
  expect_match(html, "data-ms-significance")
  expect_match(html, "data-ms-row")
  # the intercept is pinned out of every order
  expect_match(html, "data-ms-pin=\"last\"")

  # and none of it touches the value: the frame stays in model order
  expect_equal(res$term[1L], "(Intercept)")
  expect_null(attr(res, "ms_opts")$sort)
})

test_that("the facts line separates identity from fit (S2)", {
  html <- as.character(model_summary_card(model_summary(lm(mpg ~ wt, mtcars))))
  expect_match(html, "msc-id")
  expect_match(html, "msc-fit")
  expect_match(html, "msc-pair")
})

test_that("SE and the ratio scale compose multiplicatively", {
  d <- data.frame(term = "x", estimate = 0.5, std.error = 0.7)

  add <- ms_bounds(d, "se", ratio = FALSE)
  expect_equal(add$lo, -0.2)
  expect_equal(add$hi, 1.2)

  # additive bounds on a ratio would put the lower end below zero, which has no
  # place on a log axis: est / exp(se) .. est * exp(se) instead
  mult <- ms_bounds(d, "se", ratio = TRUE)
  expect_gt(mult$lo, 0)
  expect_equal(mult$lo, 0.5 / exp(0.7))
  expect_equal(mult$hi, 0.5 * exp(0.7))

  expect_equal(ms_interval_header(list(uncertainty = "se", scale = "ratio")),
               "SE (log)")
  expect_equal(ms_interval_header(list(uncertainty = "se", scale = "raw")),
               "SE")
})

test_that("the p-value decides the colour in every uncertainty state", {
  blue <- ms_colour(1, 0.5, 1.5, p = 0.001, ref = 0)
  grey <- ms_colour(1, 0.5, 1.5, p = 0.064, ref = 0)
  # a +/- SE whisker clear of the reference must not repaint a p = 0.064 term
  expect_match(grey, "subtle")
  expect_match(blue, "blue")

  # with no p at all, the interval is the fallback
  expect_match(ms_colour(1, -0.2, 2, p = NA_real_, ref = 0), "subtle")
  expect_match(ms_colour(1, 0.5, 2, p = NA_real_, ref = 0), "blue")
  expect_match(ms_colour(-1, -2, -0.5, p = NA_real_, ref = 0), "danger")
})

test_that("the emitted call prunes defaults and keeps changes", {
  bare <- paste(deparse(build_model_summary_call(
    "ci95", "chips", "auto", TRUE, TRUE, TRUE
  )), collapse = " ")
  expect_match(bare, "blockr.stats::model_summary")
  expect_false(grepl("uncertainty", bare))
  expect_false(grepl("significance", bare))
  expect_false(grepl("intercept", bare))

  changed <- paste(deparse(build_model_summary_call(
    "se", "stars", "ratio", FALSE, TRUE, FALSE
  )), collapse = " ")
  expect_match(changed, "uncertainty = \"se\"")
  expect_match(changed, "significance = \"stars\"")
  expect_match(changed, "scale = \"ratio\"")
  expect_match(changed, "effect_column = FALSE")
  expect_match(changed, "intercept = FALSE")
  expect_false(grepl("facts", changed))
})
