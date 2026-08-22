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

# A block evaluates its expression with `baseenv()` as the parent (see
# blockr.core::eval_env()), so `stats` is not on the search path and a bare
# `poly()` term fits in the console but dies in a board. These pin the
# qualification that stops that.

test_that("non-base formula functions are namespace-qualified", {
  expect_equal(qualify_term_fns("poly(x, 2)"), "stats::poly(x, 2)")
  expect_equal(qualify_term_fns("poly(x, degree = 3)"),
               "stats::poly(x, degree = 3)")
  expect_equal(qualify_term_fns("ns(x, df = 4)"), "splines::ns(x, df = 4)")
  expect_equal(qualify_term_fns("bs(x, 3)"), "splines::bs(x, 3)")
})

test_that("base functions and bare names are left alone", {
  for (lbl in c("x", "A:B", "log(x)", "sqrt(x)", "scale(x)", "factor(g)",
                "I(x^2)")) {
    expect_equal(qualify_term_fns(lbl), lbl)
  }
})

test_that("qualification is idempotent and reaches nested calls", {
  expect_equal(qualify_term_fns("stats::poly(x, 2)"), "stats::poly(x, 2)")
  expect_equal(qualify_term_fns("splines::ns(x, 4)"), "splines::ns(x, 4)")
  expect_equal(qualify_term_fns("log(poly(x, 2))"), "log(stats::poly(x, 2))")
})

test_that("an unknown or unparseable term is carried verbatim", {
  expect_equal(qualify_term_fns("no_such_fn(x)"), "no_such_fn(x)")
  expect_equal(qualify_term_fns("x +++ ("), "x +++ (")
})

test_that("the widget text and saved state stay unqualified", {
  # Qualifying in make_model_formula() would rewrite the user's term under
  # their cursor and store it that way, and parse_term() would then no longer
  # recognise it as a poly term on restore.
  st <- parse_formula("y ~ x + poly(t, 2)",
                      list(x = "numeric", t = "numeric"))
  expect_true("poly(t, 2)" %in% attr(terms(make_model_formula(st)),
                                     "term.labels"))
  expect_equal(formula_ast_to_text(st), "y ~ x + poly(t, 2)")
})

test_that("build_model_call qualifies the formula it fits", {
  st <- parse_formula("y ~ x + poly(t, 2)",
                      list(x = "numeric", t = "numeric"))
  txt <- paste(deparse(build_model_call("lm", make_model_formula(st))),
               collapse = "")
  expect_match(txt, "stats::poly(t, 2)", fixed = TRUE)
})

test_that("a poly() term fits where a block would evaluate it", {
  # The regression itself: baseenv() as parent is what a block gives you.
  skip_if_not_installed("stats")
  d <- data.frame(y = c(1, 3, 2, 5, 4, 8, 6, 9), t = 1:8, g = rep(c("a", "b"), 4))
  f <- qualify_model_formula(make_model_formula(
    parse_formula("y ~ g + poly(t, 2)", list(t = "numeric", g = "factor"))
  ))
  env <- list2env(list(d = d), parent = baseenv())
  expect_no_error(eval(bquote(stats::lm(.(f), data = d)), env))

  # ... and the unqualified formula is the failure this guards against.
  bad <- stats::as.formula("y ~ g + poly(t, 2)")
  environment(bad) <- env
  expect_error(eval(bquote(stats::lm(.(bad), data = d)), env), "poly")
})
