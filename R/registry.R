#' @importFrom blockr.core register_blocks
register_stats_blocks <- function() {
  blockr.core::register_blocks(
    c(
      "new_model_block",
      "new_lm_block",
      "new_descriptives_block",
      "new_frequencies_block"
    ),
    name = c(
      "Model",
      "Linear Model",
      "Descriptives",
      "Frequencies"
    ),
    description = c(
      "Fit a statistical model (lm / glm / aov). Returns the fitted model object for downstream broom adapters.",
      "Fit a linear model with stats::lm(). Returns the fitted model object.",
      "Per-variable summary statistics (n, mean, SD, median, quartiles, range).",
      "Frequency counts and proportions; one-way or two-way (crosstab)."
    ),
    category = c(
      "transform",
      "transform",
      "transform",
      "transform"
    ),
    icon = c(
      "calculator",
      "graph-up",
      "clipboard2-data",
      "bar-chart-line"
    ),
    package = utils::packageName(),
    overwrite = TRUE
  )
}
