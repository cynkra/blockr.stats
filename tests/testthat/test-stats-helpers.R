# Unit tests for the pure helpers — the bulk of correctness per
# blockr.docs (no Shiny needed).

test_that("build_broom_call emits standard broom code with extras inlined", {
  m <- lm(mpg ~ wt + hp, mtcars)
  # build_broom_call() leaves `.(data)` unresolved (blockr.core's bbquote
  # pattern, same as head_block/tail_block) -- block-server.R resolves it
  # via a second bquote() pass before eval; mirror that here.
  run <- function(call) {
    eval(do.call(bquote, list(call, list(data = as.name("data")))),
         list(data = m))
  }
  ti <- run(build_broom_call("tidy", conf_int = TRUE))
  expect_true(all(c("term", "estimate", "conf.low") %in% names(ti)))
  gl <- run(build_broom_call("glance"))
  expect_equal(nrow(gl), 1L)
  ag <- run(build_broom_call("augment", qq = TRUE))
  expect_true(all(c(".qq_theoretical", ".qq_sample") %in% names(ag)))
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
