test_that("residual explorer block expression generation - default", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("blockr.core")

  model <- lm(mpg ~ cyl + hp, data = mtcars)
  test_data <- reactive(model)

  blk <- new_residual_explorer_block(x_var = "fitted", y_var = "residuals")

  shiny::testServer(
    blk$expr_server,
    args = list(data = test_data),
    {
      session$flushReact()

      result <- session$returned
      expect_true(is.reactive(result$expr))
      expect_true(is.list(result$state))

      # Set inputs
      session$setInputs(x_var = "fitted", y_var = "residuals")
      session$flushReact()

      # Check expression
      expr_result <- result$expr()
      expect_true(inherits(expr_result, "call"))
      expr_text <- deparse(expr_result)
      expect_true(any(grepl("create_residual_plot", expr_text)))
      expect_true(any(grepl("fitted", expr_text)))
      expect_true(any(grepl("residuals", expr_text)))
    }
  )
})

test_that("residual explorer block expression generation - standardized residuals", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("blockr.core")

  model <- lm(mpg ~ cyl, data = mtcars)
  test_data <- reactive(model)

  blk <- new_residual_explorer_block(x_var = "index", y_var = "standardized")

  shiny::testServer(
    blk$expr_server,
    args = list(data = test_data),
    {
      session$flushReact()

      result <- session$returned

      # Set inputs
      session$setInputs(x_var = "index", y_var = "standardized")
      session$flushReact()

      # Check expression
      expr_result <- result$expr()
      expr_text <- deparse(expr_result)
      expect_true(any(grepl("index", expr_text)))
      expect_true(any(grepl("standardized", expr_text)))
    }
  )
})

test_that("residual explorer block reactive state updates", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("blockr.core")

  model <- lm(mpg ~ cyl, data = mtcars)
  test_data <- reactive(model)

  blk <- new_residual_explorer_block(x_var = "fitted", y_var = "residuals")

  shiny::testServer(
    blk$expr_server,
    args = list(data = test_data),
    {
      session$flushReact()

      result <- session$returned

      # Set initial inputs
      session$setInputs(x_var = "fitted", y_var = "residuals")
      session$flushReact()

      expect_equal(result$state$x_var(), "fitted")
      expect_equal(result$state$y_var(), "residuals")

      # Update x_var
      session$setInputs(x_var = "index")
      session$flushReact()
      expect_equal(result$state$x_var(), "index")

      # Update y_var
      session$setInputs(y_var = "studentized")
      session$flushReact()
      expect_equal(result$state$y_var(), "studentized")
    }
  )
})

test_that("residual explorer block UI generation", {
  blk <- new_residual_explorer_block()

  ui_output <- blk$expr_ui("test_id")

  expect_s3_class(ui_output, "shiny.tag.list")
  ui_text <- as.character(ui_output)

  # Should contain selectInput for x_var
  expect_true(grepl("X-axis", ui_text, ignore.case = TRUE))

  # Should contain selectInput for y_var
  expect_true(grepl("Y-axis", ui_text, ignore.case = TRUE))

  # Should contain options
  expect_true(grepl("Fitted", ui_text, ignore.case = TRUE))
  expect_true(grepl("Residual", ui_text, ignore.case = TRUE))
})

test_that("create_residual_plot_echarts helper function works", {
  skip_if_not_installed("echarts4r")

  model <- lm(mpg ~ cyl + hp, data = mtcars)

  # Access internal function
  create_plot <- blockr.stats:::create_residual_plot_echarts

  # Test with default parameters
  plot <- create_plot(model, x_var = "fitted", y_var = "residuals")
  expect_s3_class(plot, "echarts4r")

  # Test with index on x-axis
  plot <- create_plot(model, x_var = "index", y_var = "residuals")
  expect_s3_class(plot, "echarts4r")

  # Test with standardized residuals
  plot <- create_plot(model, x_var = "fitted", y_var = "standardized")
  expect_s3_class(plot, "echarts4r")

  # Test with studentized residuals
  plot <- create_plot(model, x_var = "fitted", y_var = "studentized")
  expect_s3_class(plot, "echarts4r")
})

test_that("create_residual_plot_echarts works with glm models", {
  skip_if_not_installed("echarts4r")

  # Logistic regression
  model <- glm(am ~ mpg + hp, data = mtcars, family = binomial())

  create_plot <- blockr.stats:::create_residual_plot_echarts

  # Should work with glm

  plot <- create_plot(model, x_var = "fitted", y_var = "residuals")
  expect_s3_class(plot, "echarts4r")
})

test_that("residual_explorer block produces output via block_server", {
  m <- lm(yA ~ xA1 + xA2, data = .tdf_a())
  block <- new_residual_explorer_block()
  shiny::testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      expect_false(is.null(session$returned$result()))
    },
    args = list(x = block, data = list(data = function() m))
  )
})
