# Demo app for blockr.stats
#
# Run with: source("inst/examples/app.R")

library(blockr)
pkgload::load_all()

# Run the demo app showing modeling workflow
run_app(
  blocks = c(
    # Data source - using mtcars
    data = new_dataset_block("mtcars"),

    # Fit model: mpg ~ cyl + hp + wt
    # Use new_model_block() which supports lm, logistic, poisson, gamma
    model = new_model_block(
      model_type = "lm",
      response = "mpg",
      predictors = c("cyl", "hp", "wt")
    ),

    # Model with 2 predictors for 3D visualization
    model_2pred = new_model_block(
      model_type = "lm",
      response = "mpg",
      predictors = c("wt", "hp")
    ),

    # Model summary (HTML display with coefficients, stats, diagnostics)
    summary = new_model_summary_block(),

    # Coefficient plot
    coefplot = new_coefplot_block(),

    # Diagnostic plots
    plots = new_diagnostic_plot_block(),

    # Interactive residual explorer
    resid = new_residual_explorer_block(x_var = "fitted", y_var = "residuals"),

    # 3D regression plot (requires model with exactly 2 predictors)
    plot3d = new_regression_3d_block()
  ),
  links = c(
    new_link("data", "model", "data"),
    new_link("data", "model_2pred", "data"),
    new_link("model", "summary", "data"),
    new_link("model", "coefplot", "data"),
    new_link("model", "plots", "data"),
    new_link("model", "resid", "data"),
    new_link("model_2pred", "plot3d", "data")
  ),
  title = "Model Demo"
)
