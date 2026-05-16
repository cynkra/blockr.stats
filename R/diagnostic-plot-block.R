#' Diagnostic Plot Block
#'
#' Generate standard diagnostic plots for a linear model. Displays a 2x2 grid
#' of diagnostic plots including Residuals vs Fitted, Q-Q plot,
#' Scale-Location, and Residuals vs Leverage.
#'
#' @param ... Forwarded to [new_plot_block()]
#'
#' @return A plot block object of class `diagnostic_plot_block`.
#'
#' @details
#' The four diagnostic plots help assess model assumptions:
#' - **Residuals vs Fitted**: Check for non-linearity and heteroscedasticity
#' - **Normal Q-Q**: Check if residuals are normally distributed
#' - **Scale-Location**: Check for homoscedasticity (constant variance)
#' - **Residuals vs Leverage**: Identify influential observations
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   model <- lm(mpg ~ cyl + hp, data = mtcars)
#'   serve(new_diagnostic_plot_block(), list(data = model))
#' }
#'
#' @export
new_diagnostic_plot_block <- function(...) {
  new_plot_block(
    server = function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {
          list(
            expr = reactive({
              # Use evaluate-compatible expression for base R plots
              parse(text = "blockr.stats:::create_diagnostic_plots(data)")[[1]]
            }),
            state = list()
          )
        }
      )
    },
    ui = function(id) {
      tagList(
        shinyjs::useShinyjs(),

        # CSS utilities
        css_responsive_grid(),
        css_single_column("diagnostic-plot"),

        div(
          class = "block-container diagnostic-plot-block-container",

          div(
            class = "block-form-grid",

            div(
              class = "block-section",
              div(
                class = "block-section-grid",

                # Help text
                div(
                  class = "block-help-text",
                  "Diagnostic plots to assess model assumptions: ",
                  "linearity, normality, homoscedasticity, and influential points."
                )
              )
            )
          )
        )
      )
    },
    dat_valid = function(data) {
      stopifnot(inherits(data, "lm"))
    },
    class = "diagnostic_plot_block",
    ...
  )
}

#' Create diagnostic plots for lm object
#'
#' Internal helper function that creates the 4-panel diagnostic plot.
#'
#' @param model An lm model object
#' @return NULL (plots are created as side effect)
#' @keywords internal
create_diagnostic_plots <- function(model) {
  # Set up 2x2 plot layout
  old_par <- par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
  on.exit(par(old_par))

  # Create the 4 diagnostic plots
  # which: 1 = Residuals vs Fitted
  #        2 = Normal Q-Q
  #        3 = Scale-Location
  #        5 = Residuals vs Leverage (skip 4 = Cook's distance)
  plot(model, which = c(1, 2, 3, 5), add.smooth = TRUE)

  invisible(NULL)
}
