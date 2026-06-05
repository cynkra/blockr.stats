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

test_that("descriptives_block emits a tidy per-variable frame", {
  blk <- new_descriptives_block(vars = c("mpg", "wt"))
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$flushReact()
      res <- session$returned$result()
      expect_setequal(res$variable, c("mpg", "wt"))
      expect_true(all(c("n", "mean", "sd", "median") %in% names(res)))
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

test_that("frequencies_block emits one-way counts", {
  blk <- new_frequencies_block(vars = "wool")
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$flushReact()
      res <- session$returned$result()
      expect_true(all(c("variable", "level", "n", "proportion") %in% names(res)))
      expect_setequal(res$level, levels(warpbreaks$wool))
    },
    args = list(x = blk, data = list(data = function() warpbreaks))
  )
})

test_that("padjust_block adds an adjusted p-value column", {
  df <- data.frame(term = letters[1:4], p.value = c(.01, .04, .2, .5))
  blk <- new_padjust_block(pcol = "p.value", method = "BH")
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$flushReact()
      res <- session$returned$result()
      expect_true("p.adjusted" %in% names(res))
      expect_equal(res$p.adjusted, stats::p.adjust(df$p.value, "BH"))
    },
    args = list(x = blk, data = list(data = function() df))
  )
})

test_that("effect_size_block emits a tidy effect-size frame", {
  fit <- aov(mpg ~ factor(cyl), data = mtcars)
  blk <- new_effect_size_block(measure = "partial_eta2")
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$flushReact()
      res <- session$returned$result()
      expect_true(all(c("term", "measure", "estimate") %in% names(res)))
      expect_equal(unique(res$measure), "partial_eta2")
    },
    args = list(x = blk, data = list(data = function() fit))
  )
})
