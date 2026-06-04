test_that("classify_term splits main vs factor by column type", {
  ct <- list(hp = "numeric", cyl = "factor", flag = "logical")
  expect_equal(classify_term("hp", ct)$kind, "main")
  expect_equal(classify_term("cyl", ct)$kind, "factor")
  expect_equal(classify_term("flag", ct)$kind, "factor")
  # unknown type defaults to main effect
  expect_equal(classify_term("xyz", ct)$kind, "main")
})

test_that("classify_term recognises interaction / poly / spline / transform", {
  i <- classify_term("a:b")
  expect_equal(i$kind, "interaction")
  expect_equal(i$vars, c("a", "b"))

  p <- classify_term("poly(hp, 2)")
  expect_equal(p$kind, "poly")
  expect_equal(p$var, "hp")
  expect_equal(p$degree, 2L)

  s <- classify_term("ns(wt, 3)")
  expect_equal(s$kind, "spline")
  expect_equal(s$fn, "ns")
  expect_equal(s$df, 3L)

  expect_equal(classify_term("log(disp)")$kind, "transform")
  expect_equal(classify_term("I(hp/wt)")$kind, "transform")
})

test_that("parse_formula splits core terms from bar terms and expands crossing", {
  ct <- list(hp = "numeric", cyl = "factor", g = "factor")
  p <- parse_formula("mpg ~ hp*cyl + (1 | g)", ct)

  expect_equal(p$response, "mpg")
  expect_true(p$intercept)

  kinds <- vapply(p$terms, `[[`, character(1), "kind")
  # hp (main), cyl (factor), hp:cyl (interaction)
  expect_setequal(kinds, c("main", "factor", "interaction"))

  expect_length(p$bars, 1L)
  expect_equal(p$bars[[1]]$raw, "1 | g")
})

test_that("parse_formula handles no-intercept and cbind LHS", {
  p0 <- parse_formula("y ~ 0 + x", list(x = "numeric"))
  expect_false(p0$intercept)

  pc <- parse_formula("cbind(dead, alive) ~ dose", list(dose = "numeric"))
  expect_equal(pc$response, list(fn = "cbind", args = c("dead", "alive")))
})

test_that("multiple bar terms are all carried", {
  p <- parse_formula("y ~ x + (1 | g) + (1 | h)", list(x = "numeric"))
  raws <- vapply(p$bars, `[[`, character(1), "raw")
  expect_setequal(raws, c("1 | g", "1 | h"))
})
