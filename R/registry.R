#' @importFrom blockr.core register_blocks
register_stats_blocks <- function() {
  blockr.core::register_blocks(
    c(
      "new_model_block",
      "new_broom_block",
      "new_descriptives_block",
      "new_frequencies_block",
      "new_stat_test_block",
      "new_padjust_block",
      "new_effect_size_block",
      "new_survival_block"
    ),
    name = c(
      "Model",
      "Broom Adapter",
      "Descriptives",
      "Frequencies",
      "Statistical Test",
      "P-Value Adjustment",
      "Effect Size",
      "Survival"
    ),
    description = c(
      "Fit a regression model (lm / glm) from a formula. Returns the fitted model object for downstream broom adapters.",
      "Tidy a fitted model: tidy (coefficients) / glance (fit) / augment (per-observation, optional QQ columns) / anova (ANOVA-as-model, SS type I/II/III).",
      "Per-variable summary statistics (n, mean, SD, median, quartiles, range).",
      "Frequency counts and proportions; one-way or two-way (crosstab).",
      "Adaptive hypothesis test: normality, mean/median (incl. paired), variance, correlation, categorical independence, nonparametric. Stratified, tidy output.",
      "Adjust p-values for multiple comparisons via stats::p.adjust().",
      "Effect sizes: Cohen's d / Hedges' g (effsize), eta^2 / partial eta^2 / omega^2 / r^2 (base) with CIs.",
      "Advanced: Kaplan-Meier / Cox PH / competing-risks (CIF). Returns the fitted survival model object."
    ),
    category = c(
      "transform",
      "transform",
      "transform",
      "transform",
      "transform",
      "transform",
      "transform",
      "transform"
    ),
    icon = c(
      "calculator",
      "diagram-3",
      "clipboard2-data",
      "bar-chart-line",
      "clipboard-check",
      "sliders",
      "rulers",
      "activity"
    ),
    package = utils::packageName(),
    overwrite = TRUE
  )
}
