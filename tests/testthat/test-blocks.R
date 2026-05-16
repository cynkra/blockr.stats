# testServer coverage for the core blocks (blockr.docs Pattern B).

test_that("model_block fits an lm after the Fit commit", {
  blk <- new_model_block(model_type = "lm", response = "mpg",
                         predictors = c("wt", "hp"))
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$flushReact()
      res <- session$returned$result()
      expect_s3_class(res, "lm")
      expect_equal(session$returned$state$model_type(), "lm")
      expect_equal(session$returned$state$response(), "mpg")
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

test_that("correlation_matrix_block emits long var_x/var_y/r", {
  blk <- new_correlation_matrix_block()
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", blk),
    {
      session$flushReact()
      res <- session$returned$result()
      expect_setequal(names(res), c("var_x", "var_y", "r"))
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
