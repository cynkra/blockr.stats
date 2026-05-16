test_that("model summary block expression generation", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("blockr.core")
  skip_if_not_installed("broom")

  model <- lm(mpg ~ cyl + hp, data = mtcars)
  test_data <- reactive(model)

  blk <- new_model_summary_block()

  shiny::testServer(
    blk$expr_server,
    args = list(data = test_data),
    {
      session$flushReact()

      result <- session$returned
      expect_true(is.reactive(result$expr))
      expect_true(is.list(result$state))

      # Check expression contains broom calls
      expr_result <- result$expr()
      expect_true(inherits(expr_result, "{"))
      expr_text <- deparse(expr_result)
      expect_true(any(grepl("broom::glance", expr_text)))
      expect_true(any(grepl("broom::tidy", expr_text)))
    }
  )
})

test_that("model summary block UI generation", {
  blk <- new_model_summary_block()

  ui_output <- blk$expr_ui("test_id")

  # Should be empty tagList (custom display in block_ui method)
  expect_s3_class(ui_output, "shiny.tag.list")
})

test_that("significance_badge returns correct badges", {
  # Access internal function
  sig_badge <- blockr.stats:::significance_badge

  # Test different significance levels
  badge_001 <- sig_badge(0.0005)
  expect_true(grepl("0\\.1%", as.character(badge_001)))

  badge_01 <- sig_badge(0.005)
  expect_true(grepl("1%", as.character(badge_01)))

  badge_05 <- sig_badge(0.03)
  expect_true(grepl("5%", as.character(badge_05)))

  badge_10 <- sig_badge(0.08)
  expect_true(grepl("10%", as.character(badge_10)))

  # Non-significant should return empty
  badge_ns <- sig_badge(0.5)
  expect_equal(badge_ns, "")

  # NA should return empty
  badge_na <- sig_badge(NA)
  expect_equal(badge_na, "")
})

test_that("html_model_coefs generates valid HTML", {
  model <- lm(mpg ~ cyl + hp, data = mtcars)
  tidy_df <- broom::tidy(model, conf.int = TRUE)

  html_coefs <- blockr.stats:::html_model_coefs
  result <- html_coefs(tidy_df)

  expect_s3_class(result, "shiny.tag")
  html_text <- as.character(result)

  # Should contain coefficient terms
  expect_true(grepl("cyl", html_text))
  expect_true(grepl("hp", html_text))
  expect_true(grepl("Intercept", html_text))
})

test_that("html_model_stats generates valid HTML", {
  model <- lm(mpg ~ cyl + hp, data = mtcars)
  glance_df <- broom::glance(model)

  html_stats <- blockr.stats:::html_model_stats
  result <- html_stats(glance_df, model)

  expect_s3_class(result, "shiny.tag")
  html_text <- as.character(result)

  # Should contain common statistics
expect_true(grepl("R", html_text))  # R-squared
  expect_true(grepl("AIC", html_text))
  expect_true(grepl("BIC", html_text))
})

test_that("html_model_tests generates valid HTML", {
  model <- lm(mpg ~ cyl + hp, data = mtcars)

  html_tests <- blockr.stats:::html_model_tests
  result <- html_tests(model)

  expect_s3_class(result, "shiny.tag")
  html_text <- as.character(result)

  # Should contain test names
  expect_true(grepl("Shapiro", html_text))
  expect_true(grepl("Durbin", html_text))
})

test_that("model summary works with glm", {
  skip_if_not_installed("broom")

  # Logistic regression
  model <- glm(am ~ mpg + hp, data = mtcars, family = binomial)

  # Test that broom works
  tidy_df <- broom::tidy(model)
  glance_df <- broom::glance(model)

  expect_true(nrow(tidy_df) > 0)
  expect_true(nrow(glance_df) == 1)

  # Test html generation
  html_coefs <- blockr.stats:::html_model_coefs
  result <- html_coefs(tidy_df)
  expect_s3_class(result, "shiny.tag")
})

test_that("model_summary block produces a glance tibble via block_server", {
  m <- lm(yA ~ xA1 + xA2, data = .tdf_a())
  block <- new_model_summary_block()
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_s3_class(result, "data.frame")
      expect_true("r.squared" %in% names(result))
      # the model + tidy are attached as attributes for the custom render
      expect_false(is.null(attr(result, "model")))
      expect_false(is.null(attr(result, "tidy")))
    },
    args = list(x = block, data = list(data = function() m))
  )
})
