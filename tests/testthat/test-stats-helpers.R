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
  expect_setequal(
    names(ow),
    c("variable", "level", "n", "proportion", "pct", ".fmt")
  )
  expect_equal(sum(ow$n), 3L)
  tw <- tabulate_freq(d, "s", by = "t")
  expect_true(all(c("by", "by_level") %in% names(tw)))
  expect_equal(sum(tw$n), 3L)
})

test_that("tabulate_freq carries a .fmt template referencing real columns", {
  d <- data.frame(s = c("x", "x", "y"))
  ow <- tabulate_freq(d, "s")
  # The hidden template column is present and additive (raw numbers kept).
  expect_true(".fmt" %in% names(ow))
  expect_true(all(c("n", "proportion", "pct") %in% names(ow)))
  expect_equal(unique(ow$.fmt), "{n} ({pct:1}%)")
  # Every {token} in the template names a real column on the frame.
  toks <- regmatches(ow$.fmt[1], gregexpr("\\{([^{}:]+)", ow$.fmt[1]))[[1]]
  toks <- sub("\\{", "", toks)
  expect_true(all(toks %in% names(ow)))
  # pct is the percentage companion of the 0-1 proportion.
  expect_equal(ow$pct, ow$proportion * 100)
  # Two-way carries the template too.
  tw <- tabulate_freq(data.frame(s = c("x", "x", "y"),
                                 t = c("p", "q", "p")), "s", by = "t")
  expect_true(".fmt" %in% names(tw))
  expect_equal(unique(tw$.fmt), "{n} ({pct:1}%)")
})

test_that("build_broom_call emits standard broom code with extras inlined", {
  m <- lm(mpg ~ wt + hp, mtcars)
  run <- function(call) eval(call, list(data = m))
  ti <- run(build_broom_call("tidy", conf_int = TRUE))
  expect_true(all(c("term", "estimate", "conf.low") %in% names(ti)))
  expect_equal(attr(ti$estimate, "label"), "Estimate")
  gl <- run(build_broom_call("glance"))
  expect_equal(nrow(gl), 1L)
  ag <- run(build_broom_call("augment", qq = TRUE))
  expect_true(all(c(".qq_theoretical", ".qq_sample") %in% names(ag)))
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

test_that("tidy.cuminc is long group/time/estimate", {
  set.seed(2)
  cf <- data.frame(ft = rexp(60, .1),
                   fs = sample(0:1, 60, TRUE),
                   g = sample(c("A", "B"), 60, TRUE))
  ci <- cmprsk::cuminc(ftime = cf$ft, fstatus = cf$fs, group = cf$g)
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
