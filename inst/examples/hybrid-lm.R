# Hybrid LM Demo: Combining blockr.stats blocks with blockr.extra function blocks
#
# This example demonstrates how to extend blockr.stats's specialized modeling blocks
# with blockr.extra's new_function_block() for custom outputs like gtsummary tables.
# It also demonstrates async execution with new_async_function_block().
#
# Run with: source("inst/examples/hybrid-lm.R")

library(blockr)
pkgload::load_all()
pkgload::load_all("../blockr.extra")

# Setup mirai daemons for async execution
mirai::daemons(2)
shiny::onStop(function() mirai::daemons(0))

run_app(
  blocks = c(
    # Data source - using mtcars
    data = new_dataset_block("mtcars"),

    # === blockr.stats specialized blocks ===

    # Fit model: mpg ~ cyl + hp + wt
    model = new_model_block(
      model_type = "lm",
      response = "mpg",
      predictors = c("cyl", "hp", "wt")
    ),

    # Diagnostic plots (residuals vs fitted, Q-Q, etc.)
    plots = new_diagnostic_plot_block(),

    # Coefficient plot with confidence intervals
    coefplot = new_coefplot_block(),

    # Interactive residual explorer
    resid = new_residual_explorer_block(),

    # Broom summary for lm model (should show p-value badges)
    model_broom = new_broom_summary_block(),

    # === blockr.extra function blocks for custom functionality ===

    # Use gtsummary for publication-ready regression table
    summary_tbl = new_function_block(
      fn = "function(data) {
        gtsummary::tbl_regression(data) |> gtsummary::as_gt()
      }"
    ),

    # Custom model: Robust regression using MASS::rlm (ASYNC)
    # (not natively supported by blockr.stats)
    # - formula: text input for model specification
    # - psi: dropdown for robust estimation method
    # Uses async execution - click "Run" to execute, "Cancel" to stop
    # Note: Async blocks require user to click "Run" before result is available.
    # The result is displayed directly in the block's output area.
    robust_model = new_async_function_block(
      fn = "function(data,
                     formula = 'mpg ~ cyl + hp + wt',
                     psi = c('psi.huber', 'psi.hampel', 'psi.bisquare')) {
        Sys.sleep(2)  # Simulate slow computation
        MASS::rlm(as.formula(formula), data = data, psi = psi)
      }"
    ),

    # Broom summary for the robust model (from blockr.extra)
    # Shows adaptive HTML summary using broom::tidy/glance/augment
    robust_summary = new_broom_summary_block(),

    # Example of async block returning a ggplot
    async_plot = new_async_function_block(
      fn = "function(data,
                     x = c('wt', 'hp', 'disp', 'cyl'),
                     y = c('mpg', 'qsec', 'drat')) {
        Sys.sleep(1)
        ggplot2::ggplot(data, ggplot2::aes(x = .data[[x]], y = .data[[y]])) +
          ggplot2::geom_point() +
          ggplot2::geom_smooth(method = 'lm') +
          ggplot2::theme_minimal()
      }"
    ),

    # === 3D visualization branch ===

    # Second model with 2 predictors for 3D visualization
    model_2pred = new_model_block(
      model_type = "lm",
      response = "mpg",
      predictors = c("wt", "hp")
    ),

    # 3D regression surface plot
    plot3d = new_regression_3d_block()
  ),
  links = c(
    # Main model branch
    new_link("data", "model", "data"),
    new_link("model", "plots", "data"),
    new_link("model", "coefplot", "data"),
    new_link("model", "resid", "data"),
    new_link("model", "summary_tbl", "data"),
    new_link("model", "model_broom", "data"),

    # Async blocks (standalone - output displayed directly in block)
    new_link("data", "robust_model", "data"),
    new_link("robust_model", "robust_summary", "data"),
    new_link("data", "async_plot", "data"),

    # 3D visualization branch
    new_link("data", "model_2pred", "data"),
    new_link("model_2pred", "plot3d", "data")
  ),
  title = "Hybrid LM Demo"
)
