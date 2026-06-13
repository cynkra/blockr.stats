#' @importFrom blockr.core register_blocks
register_stats_blocks <- function() {
  blockr.core::register_blocks(
    c(
      "new_model_block",
      "new_broom_block",
      "new_descriptives_block",
      "new_frequencies_block",
      "new_correlate_block",
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
      "Correlate",
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
      "Pairwise correlation matrix of numeric columns (pearson / spearman / kendall) as a tidy `var` + per-variable frame; renders as a heatmap table.",
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
      "transform",
      "transform"
    ),
    icon = c(
      "calculator",
      "diagram-3",
      "clipboard2-data",
      "bar-chart-line",
      "grid-3x3",
      "clipboard-check",
      "sliders",
      "rulers",
      "activity"
    ),
    arguments = list(
      # new_model_block: flat, fully AI-accessible surface.
      structure(
        c(
          model_type = paste(
            "Model family: one of 'lm' (linear, continuous outcome),",
            "'logistic' (binary 0/1 outcome), 'poisson' (counts),",
            "'gamma' (positive continuous)."
          ),
          formula = paste(
            "Model formula as a string, e.g. 'mpg ~ hp + wt'. Response on the",
            "left of ~, predictors on the right: '+' adds terms, '*' is a full",
            "interaction (main effects + product), ':' is interaction-only."
          ),
          weights = paste(
            "Optional: a column name to use as case weights. Omit unless the",
            "task explicitly calls for weighting."
          ),
          offset = paste(
            "Optional: a column name to use as a model offset (e.g. log",
            "exposure for a Poisson rate). Omit unless explicitly needed."
          )
        ),
        examples = list(
          model_type = "lm",
          formula = "mpg ~ hp + wt"
        ),
        prompt = paste(
          "Fit a regression model. Pick model_type for the outcome",
          "(lm/logistic/poisson/gamma) and write formula as a string:",
          "'response ~ predictor1 + predictor2'. weights and offset are",
          "optional column names; leave them out unless the request needs them."
        )
      ),
      NULL, # new_broom_block
      NULL, # new_descriptives_block
      NULL, # new_frequencies_block
      NULL, # new_correlate_block
      NULL, # new_stat_test_block
      NULL, # new_padjust_block
      NULL, # new_effect_size_block
      NULL  # new_survival_block
    ),
    package = utils::packageName(),
    overwrite = TRUE
  )
}
