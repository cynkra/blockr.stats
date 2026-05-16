#' 3D Regression Plot Block
#'
#' Interactive 3D scatter plot with regression plane using plotly. Visualizes
#' a linear model with exactly 2 predictors, showing the data points and the
#' fitted regression plane.
#'
#' @param ... Forwarded to [new_block()]
#'
#' @return A block object of class `regression_3d_block` that outputs an
#'   interactive plotly 3D plot.
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   model <- lm(mpg ~ wt + hp, data = mtcars)
#'   serve(new_regression_3d_block(), list(data = model))
#' }
#'
#' @export
new_regression_3d_block <- function(...) {
  blockr.core::new_block(
    server = function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {
          list(
            expr = reactive({
              quote(blockr.stats:::create_regression_3d_plot(data))
            }),
            state = list()
          )
        }
      )
    },
    ui = function(id) {
      tagList(
        div(
          class = "block-container regression-3d-block-container",
          div(
            class = "block-help-text",
            "Interactive 3D plot. Drag to rotate, scroll to zoom."
          )
        )
      )
    },
    dat_valid = function(data) {
      stopifnot(inherits(data, "lm"))
      n_predictors <- length(attr(stats::terms(data), "term.labels"))
      if (n_predictors != 2) {
        stop("3D plot requires exactly 2 predictors, got ", n_predictors)
      }
    },
    class = "regression_3d_block",
    ...
  )
}

#' Create interactive 3D regression plot with plotly
#'
#' Internal helper function that creates the plotly 3D scatter plot with
#' regression plane.
#'
#' @param model An lm model object with exactly 2 predictors
#' @return A plotly object
#' @keywords internal
create_regression_3d_plot <- function(model) {
  # Extract data from model

  mf <- stats::model.frame(model)
  response_name <- names(mf)[1]
  predictor_names <- attr(stats::terms(model), "term.labels")

  # Get the actual values

  y <- mf[[1]]
  x1 <- mf[[predictor_names[1]]]
  x2 <- mf[[predictor_names[2]]]

  # Get observation names for hover

  obs_names <- rownames(mf)
  if (is.null(obs_names)) {
    obs_names <- as.character(seq_len(nrow(mf)))
  }

  # Create grid for regression plane
  x1_range <- range(x1)
  x2_range <- range(x2)
  n_grid <- 25

  x1_seq <- seq(x1_range[1], x1_range[2], length.out = n_grid)
  x2_seq <- seq(x2_range[1], x2_range[2], length.out = n_grid)

  grid_data <- expand.grid(x1_seq, x2_seq)
  names(grid_data) <- predictor_names

  # Predict on grid

  z_pred <- stats::predict(model, newdata = grid_data)
  z_matrix <- matrix(z_pred, nrow = n_grid, ncol = n_grid)

  # Create hover text for scatter points
  hover_text <- paste0(
    "<b>", obs_names, "</b><br>",
    predictor_names[1], ": ", round(x1, 3), "<br>",
    predictor_names[2], ": ", round(x2, 3), "<br>",
    response_name, ": ", round(y, 3)
  )

  # Build plotly 3D plot

  plotly::plot_ly() |>
    plotly::add_trace(
      type = "scatter3d",
      mode = "markers",
      x = x1,
      y = x2,
      z = y,
      marker = list(
        size = 5,
        color = "#3498db",
        opacity = 0.8
      ),
      hoverinfo = "text",
      text = hover_text,
      name = "Data"
    ) |>
    plotly::add_surface(
      x = x1_seq,
      y = x2_seq,
      z = z_matrix,
      opacity = 0.6,
      colorscale = list(c(0, 1), c("#e74c3c", "#e74c3c")),
      showscale = FALSE,
      name = "Regression Plane",
      hoverinfo = "skip"
    ) |>
    plotly::layout(
      scene = list(
        xaxis = list(title = predictor_names[1]),
        yaxis = list(title = predictor_names[2]),
        zaxis = list(title = response_name)
      ),
      margin = list(l = 0, r = 0, t = 30, b = 0)
    )
}

#' @export
block_output.regression_3d_block <- function(x, result, session) {
  plotly::renderPlotly(result)
}

#' @export
block_ui.regression_3d_block <- function(id, x, ...) {
  tagList(
    plotly::plotlyOutput(shiny::NS(id, "result"), height = "500px")
  )
}
