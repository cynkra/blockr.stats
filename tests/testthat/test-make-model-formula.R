test_that("make_model_formula round-trips through parse_formula (semantic identity)", {
  ct <- list(hp = "numeric", cyl = "factor", wt = "numeric")
  txt <- "mpg ~ hp + cyl + poly(wt, 2) + hp:cyl"

  st <- parse_formula(txt, ct)
  f <- make_model_formula(st)

  expect_s3_class(f, "formula")
  expect_setequal(
    attr(terms(f), "term.labels"),
    attr(terms(stats::as.formula(txt)), "term.labels")
  )
  expect_equal(attr(terms(f), "intercept"), 1L)
})

test_that("make_model_formula carries bar terms and respects no-intercept", {
  st <- parse_formula("y ~ 0 + x + (1 | g)", list(x = "numeric", g = "factor"))
  f <- make_model_formula(st)

  expect_match(deparse1(f), "(1 | g)", fixed = TRUE)
  expect_equal(attr(terms(f), "intercept"), 0L)
})

test_that("cbind LHS round-trips", {
  st <- parse_formula("cbind(s, f) ~ dose", list(dose = "numeric"))
  f <- make_model_formula(st)
  expect_equal(deparse1(f[[2L]]), "cbind(s, f)")
})

test_that("make_model_formula returns NULL with no response or empty model", {
  expect_null(make_model_formula(list(response = NULL, terms = list())))
  expect_null(make_model_formula(NULL))
  # no terms, no intercept -> empty -> NULL
  expect_null(make_model_formula(list(
    response = "y", intercept = FALSE, terms = list(), bars = list()
  )))
})

test_that("intercept-only model is valid", {
  f <- make_model_formula(list(
    response = "y", intercept = TRUE, terms = list(), bars = list()
  ))
  expect_equal(deparse1(f), "y ~ 1")
})

test_that("opaque term is carried verbatim, never dropped", {
  st <- parse_formula("y ~ x + I(x/z)", list(x = "numeric", z = "numeric"))
  f <- make_model_formula(st)
  expect_true(any(grepl("I(x/z)", attr(terms(f), "term.labels"), fixed = TRUE)))
})
