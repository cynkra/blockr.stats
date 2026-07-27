# testServer coverage for the core blocks (blockr.docs Pattern B).

test_that("model_block fits an lm from a seeded formula", {
  ct <- list(mpg = "numeric", wt = "numeric", hp = "numeric")
  blk <- new_model_block(
    model_type = "lm",
    formula = parse_formula("mpg ~ wt + hp", ct)
  )
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$flushReact()
      res <- session$returned$result()
      expect_s3_class(res, "lm")
      expect_equal(session$returned$state$model_type(), "lm")
      expect_equal(session$returned$state$formula()$response, "mpg")
    },
    args = list(x = blk, data = list(data = function() mtcars))
  )
})

test_that("broom_block tidies an upstream model", {
  blk <- new_broom_block(output = "tidy")
  m <- lm(mpg ~ wt + hp, mtcars)
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$flushReact()
      res <- session$returned$result()
      expect_true(all(c("term", "estimate") %in% names(res)))
      expect_equal(session$returned$state$output(), "tidy")
    },
    args = list(x = blk, data = list(data = function() m))
  )
})

test_that("model_summary_block returns the coefficient frame for a model", {
  blk <- new_model_summary_block()
  m <- lm(mpg ~ wt + hp, mtcars)
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$flushReact()
      res <- session$returned$result()
      # the block's VALUE is an ordinary tidy frame: downstream table / chart
      # / report blocks see coefficients, not a rendered card
      expect_s3_class(res, "data.frame")
      expect_true(all(c("term", "estimate", "conf.low", "p.value") %in% names(res)))
      expect_true("(Intercept)" %in% res$term)
      expect_equal(attr(res, "ms_kind"), "Linear model")
      expect_equal(attr(res, "ms_nobs"), 32L)
      expect_true(isTRUE(attr(res, "ms_has_terms")))
      expect_equal(session$returned$state$uncertainty(), "ci95")
    },
    args = list(x = blk, data = list(data = function() m))
  )
})

test_that("model_summary_block options reach the expression and the value", {
  blk <- new_model_summary_block(
    uncertainty = "ci90", significance = "stars", intercept = FALSE
  )
  m <- lm(mpg ~ wt + hp, mtcars)
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$flushReact()
      code <- paste(deparse(session$returned$expr()), collapse = " ")
      expect_match(code, "blockr.stats::model_summary")
      expect_match(code, "ci90")
      expect_match(code, "stars")
      # defaults are pruned from the emitted call, changed options are not
      expect_false(grepl("scale", code))
      expect_false(grepl("facts", code))

      res <- session$returned$result()
      expect_false("(Intercept)" %in% res$term)
      # a 90% interval is strictly narrower than the 95% one
      expect_true(all(res$conf.high - res$conf.low <
                        stats::confint(m)[-1, 2] - stats::confint(m)[-1, 1]))
    },
    args = list(x = blk, data = list(data = function() m))
  )
})

test_that("correlate_block emits a tidy correlation matrix", {
  blk <- new_correlate_block(vars = c("mpg", "hp", "wt"),
                             method = "spearman")
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$flushReact()
      res <- session$returned$result()
      # leading `var` char column + one numeric column per selected variable
      expect_identical(names(res), c("var", "mpg", "hp", "wt"))
      expect_type(res$var, "character")
      expect_setequal(res$var, c("mpg", "hp", "wt"))
      # square-ish matrix: nrow == number of value columns
      expect_equal(nrow(res), length(names(res)) - 1L)
      num_cols <- setdiff(names(res), "var")
      expect_true(all(vapply(res[num_cols], is.numeric, logical(1L))))
      # diagonal is 1 (a variable correlates perfectly with itself)
      expect_equal(res$mpg[res$var == "mpg"], 1)
    },
    args = list(x = blk, data = list(data = function() mtcars))
  )
})

test_that("correlate_block expr carries the selected method", {
  blk <- new_correlate_block(vars = c("mpg", "hp"), method = "kendall")
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$flushReact()
      ex <- session$returned$expr()
      expect_true(grepl("kendall", paste(deparse(ex), collapse = " ")))
      expect_equal(session$returned$state$method(), "kendall")
    },
    args = list(x = blk, data = list(data = function() mtcars))
  )
})

test_that("survival_block returns a survfit", {
  blk <- new_survival_block(type = "km", time_var = "time",
                            event_var = "status", group_var = "sex")
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$flushReact()
      expect_s3_class(session$returned$result(), "survfit")
    },
    args = list(x = blk, data = list(data = function() survival::lung))
  )
})
