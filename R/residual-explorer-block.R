#' Residual Explorer Block
#'
#' Interactive residual exploration using echarts4r. Allows users to explore
#' the relationship between fitted values and residuals, with hover tooltips
#' to identify observations.
#'
#' @param x_var What to plot on x-axis: "fitted" (fitted values) or "index" (observation number)
#' @param y_var What to plot on y-axis: "residuals", "standardized", or "studentized"
#' @param ... Forwarded to [new_block()]
#'
#' @return A block object of class `residual_explorer_block` that outputs an
#'   interactive echarts4r plot.
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   model <- lm(mpg ~ cyl + hp, data = mtcars)
#'   serve(new_residual_explorer_block(), list(data = model))
#' }
#'
#' @export
new_residual_explorer_block <- function(
  x_var = "fitted",
  y_var = "residuals",
  ...
) {
  blockr.core::new_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {
          # Initialize reactive values
          r_x_var <- reactiveVal(x_var)
          r_y_var <- reactiveVal(y_var)

          # Update reactive values when inputs change
          observeEvent(input$x_var, {
            r_x_var(input$x_var)
          })

          observeEvent(input$y_var, {
            r_y_var(input$y_var)
          })

          list(
            expr = reactive({
              x <- r_x_var()
              y <- r_y_var()

              expr_text <- glue::glue(
                "blockr.stats:::create_residual_plot_echarts(data, x_var = '{x}', y_var = '{y}')"
              )
              parse(text = expr_text)[[1]]
            }),
            state = list(
              x_var = r_x_var,
              y_var = r_y_var
            )
          )
        }
      )
    },
    function(id) {
      tagList(
        shinyjs::useShinyjs(),

        # CSS utilities
        css_responsive_grid(),
        css_single_column("residual-explorer"),

        div(
          class = "block-container residual-explorer-block-container",

          div(
            class = "block-form-grid",

            div(
              class = "block-section",
              div(
                class = "block-section-grid",

                # Help text
                div(
                  class = "block-help-text",
                  "Hover over points to see observation details."
                ),

                # X-axis variable
                div(
                  class = "block-input-wrapper",
                  selectInput(
                    NS(id, "x_var"),
                    label = "X-axis",
                    choices = c(
                      "Fitted Values" = "fitted",
                      "Observation Index" = "index"
                    ),
                    selected = x_var,
                    width = "100%"
                  )
                ),

                # Y-axis variable
                div(
                  class = "block-input-wrapper",
                  selectInput(
                    NS(id, "y_var"),
                    label = "Y-axis",
                    choices = c(
                      "Residuals" = "residuals",
                      "Standardized Residuals" = "standardized",
                      "Studentized Residuals" = "studentized"
                    ),
                    selected = y_var,
                    width = "100%"
                  )
                )
              )
            )
          )
        )
      )
    },
    dat_valid = function(data) {
      stopifnot(inherits(data, "lm") || inherits(data, "glm"))
    },
    class = "residual_explorer_block",
    ...
  )
}

#' Create interactive residual plot with echarts4r
#'
#' Internal helper function that creates the echarts4r residual plot.
#'
#' @param model An lm or glm model object
#' @param x_var What to plot on x-axis: "fitted" or "index"
#' @param y_var What to plot on y-axis: "residuals", "standardized", or "studentized"
#' @return An echarts4r object
#' @keywords internal
create_residual_plot_echarts <- function(model, x_var = "fitted", y_var = "residuals") {
  # Extract data from model
  fitted_vals <- stats::fitted(model)
  resid_vals <- stats::residuals(model)

  # Get observation names/indices
  obs_names <- names(fitted_vals)
  if (is.null(obs_names)) {
    obs_names <- as.character(seq_along(fitted_vals))
  }

  # Calculate x values
  x_vals <- if (x_var == "fitted") {
    fitted_vals
  } else {
    seq_along(fitted_vals)
  }

  x_label <- if (x_var == "fitted") "Fitted Values" else "Observation Index"

  # Calculate y values
  y_vals <- switch(
    y_var,
    "residuals" = resid_vals,
    "standardized" = tryCatch(
      stats::rstandard(model),
      error = function(e) resid_vals / stats::sd(resid_vals)
    ),
    "studentized" = tryCatch(
      stats::rstudent(model),
      error = function(e) resid_vals / stats::sd(resid_vals)
    ),
    resid_vals
  )

  y_label <- switch(
    y_var,
    "residuals" = "Residuals",
    "standardized" = "Standardized Residuals",
    "studentized" = "Studentized Residuals",
    "Residuals"
  )

  # Create data frame for plotting with tooltip column
  plot_data <- data.frame(
    x = x_vals,
    y = y_vals,
    tooltip = paste0(
      "<strong>", obs_names, "</strong><br/>",
      "Fitted: ", round(fitted_vals, 3), "<br/>",
      "Residual: ", round(resid_vals, 3)
    ),
    stringsAsFactors = FALSE
  )

  # Build echarts4r plot
  plot_data |>
    echarts4r::e_charts(x) |>
    echarts4r::e_scatter(
      y,
      symbol_size = 10,
      bind = tooltip,
      legend = FALSE
    ) |>
    echarts4r::e_mark_line(
      data = list(yAxis = 0),
      lineStyle = list(type = "dashed", color = "#e74c3c", width = 1),
      symbol = "none",
      silent = TRUE
    ) |>
    echarts4r::e_tooltip(
      trigger = "item",
      formatter = htmlwidgets::JS("function(params) { return params.name; }")
    ) |>
    echarts4r::e_x_axis(name = x_label, nameLocation = "center", nameGap = 25, scale = TRUE) |>
    echarts4r::e_y_axis(name = y_label, nameLocation = "middle", nameGap = 35, nameRotate = 90) |>
    echarts4r::e_color("#3498db") |>
    echarts4r::e_grid(left = "55px", right = "16px", bottom = "50px", top = "20px")
}

#' @export
block_output.residual_explorer_block <- function(x, result, session) {
  echarts4r::renderEcharts4r(result)
}

#' @export
block_ui.residual_explorer_block <- function(id, x, ...) {
  tagList(
    echarts4r::echarts4rOutput(NS(id, "result"), height = "400px")
  )
}
