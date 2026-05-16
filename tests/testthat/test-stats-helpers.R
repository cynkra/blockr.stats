# Unit tests for the pure helpers — the bulk of correctness per
# blockr.docs (no Shiny needed).

test_that("describe_numeric returns one tidy row per numeric var", {
  d <- data.frame(a = c(1, 2, 3, NA), b = c(4, 5, 6, 7),
                   g = letters[1:4])
  out <- describe_numeric(d)
  expect_setequal(out$variable, c("a", "b"))
  ra <- out[out$variable == "a", ]
  expect_equal(ra$n, 3L)
  expect_equal(ra$n_missing, 1L)
  expect_equal(ra$mean, 2)
  expect_equal(ra$median, 2)
})

test_that("describe_numeric messages with no numeric columns", {
  out <- describe_numeric(data.frame(g = letters[1:3]))
  expect_true("message" %in% names(out))
})

test_that("tabulate_freq one-way and two-way", {
  d <- data.frame(s = c("x", "x", "y"), t = c("p", "q", "p"))
  ow <- tabulate_freq(d, "s")
  expect_setequal(names(ow), c("variable", "level", "n", "proportion"))
  expect_equal(sum(ow$n), 3L)
  tw <- tabulate_freq(d, "s", by = "t")
  expect_true(all(c("by", "by_level") %in% names(tw)))
  expect_equal(sum(tw$n), 3L)
})

test_that("correlation_matrix is long with var_x/var_y/r", {
  out <- correlation_matrix(mtcars)
  expect_setequal(names(out), c("var_x", "var_y", "r"))
  diag <- out[out$var_x == out$var_y, ]
  expect_true(all(abs(diag$r - 1) < 1e-8))
  expect_true("message" %in%
    names(correlation_matrix(data.frame(a = 1:3))))
})

test_that("broom_apply tidy/glance/augment + qq", {
  m <- lm(mpg ~ wt + hp, mtcars)
  ti <- broom_apply(m, "tidy")
  expect_true(all(c("term", "estimate", "conf.low") %in% names(ti)))
  gl <- broom_apply(m, "glance")
  expect_equal(nrow(gl), 1L)
  ag <- broom_apply(m, "augment", qq = TRUE)
  expect_true(all(c(".qq_theoretical", ".qq_sample") %in% names(ag)))
  expect_true("message" %in% names(broom_apply(NULL)))
})

test_that("es_ncp_ci brackets the estimate and is NA-safe", {
  ci <- es_ncp_ci(20, 2, 50, 0.95)
  expect_true(is.finite(ci$low) && is.finite(ci$high))
  expect_true(ci$low <= ci$high)
  bad <- es_ncp_ci(0, 2, 50)
  expect_true(is.na(bad$low) && is.na(bad$high))
})

test_that("effect_size eta2/partial_eta2/omega2/d/r2", {
  a <- aov(mpg ~ factor(cyl), mtcars)
  pe <- effect_size(a, "partial_eta2")
  expect_true(pe$estimate[1] > 0 && pe$estimate[1] < 1)
  expect_setequal(names(pe),
    c("term", "measure", "estimate", "conf.low", "conf.high"))
  expect_true(effect_size(a, "eta2")$estimate[1] > 0)
  expect_true(effect_size(a, "omega2")$estimate[1] > 0)
  d <- effect_size(lm(mpg ~ factor(am), mtcars), "d")
  expect_equal(d$measure, "d")
  expect_true(is.finite(d$estimate))
  r2 <- effect_size(lm(mpg ~ wt, mtcars), "r2")
  expect_equal(r2$measure, "r2")
})

test_that("fit_survival returns the right objects", {
  d <- survival::lung
  expect_s3_class(fit_survival(d, "km", "time", "status", "sex"),
                  "survfit")
  expect_s3_class(fit_survival(d, "cox", "time", "status", "sex"),
                  "coxph")
  set.seed(1)
  cf <- data.frame(ft = rexp(80, .1),
                   fs = sample(0:2, 80, TRUE),
                   g = sample(c("A", "B"), 80, TRUE))
  expect_s3_class(fit_survival(cf, "cif", "ft", "fs", "g"), "cuminc")
})

test_that("tidy.cuminc is long group/time/estimate", {
  set.seed(2)
  cf <- data.frame(ft = rexp(60, .1),
                   fs = sample(0:1, 60, TRUE),
                   g = sample(c("A", "B"), 60, TRUE))
  ci <- fit_survival(cf, "cif", "ft", "fs", "g")
  td <- tidy.cuminc(ci)
  expect_setequal(names(td), c("group", "time", "estimate"))
  expect_gt(nrow(td), 0)
})

test_that("ported + new test fns return tidy frames", {
  d <- data.frame(pre = c(5, 6, 7, 8, 9), post = c(6, 7, 8, 8, 11),
                   v = rnorm(5),
                   a = factor(c("x", "x", "y", "y", "y")),
                   b = factor(c("p", "q", "p", "q", "q")))
  pt <- test_t_paired(d, c("pre", "post"), NULL, list())
  expect_true("p.value" %in% names(pt))
  wp <- test_wilcoxon_paired(d, c("pre", "post"), NULL, list())
  expect_true("p.value" %in% names(wp))
  ci <- test_chisq_independence(d, "a", "b", list())
  expect_true(all(c("statistic", "p.value") %in% names(ci)))
  expect_true("categorical" %in% names(test_categories))
  expect_true(all(c("t_test_paired", "wilcoxon_paired") %in%
    category_tests("mean_median")))
})
