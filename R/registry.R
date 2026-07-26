#' @importFrom blockr.core register_blocks new_arg_specs new_arg_spec
#'   arg_string arg_enum arg_boolean
register_stats_blocks <- function() {
  blockr.core::register_blocks(
    c(
      "new_model_block",
      "new_model_summary_block",
      "new_broom_block",
      "new_correlate_block",
      "new_stat_test_block",
      "new_survival_block"
    ),
    name = c(
      "Model",
      "Model Summary",
      "Broom Adapter",
      "Correlate",
      "Statistical Test",
      "Survival"
    ),
    description = c(
      "Fit a regression model (lm / glm) from a formula. Returns the fitted model object for downstream broom adapters.",
      "Render a fitted model as a summary card: model facts line + coefficient table with an inline forest (estimate, CI whisker, significance chips). Feeds on the model object directly (tidy + glance inside), works for any broom-supported model; its value is the tidy coefficient frame.",
      "Tidy a fitted model: tidy (coefficients) / glance (fit) / augment (per-observation, optional QQ columns) / anova (ANOVA-as-model, SS type I/II/III).",
      "Pairwise correlation matrix of numeric columns (pearson / spearman / kendall) as a tidy `var` + per-variable frame; renders as a heatmap table.",
      "Adaptive hypothesis test: normality, mean/median (incl. paired), variance, correlation, categorical independence, nonparametric. Stratified, tidy output.",
      "Advanced: Kaplan-Meier / Cox PH / competing-risks (CIF). Returns the fitted survival model object."
    ),
    category = c(
      "transform",
      "transform",
      "transform",
      "transform",
      "transform",
      "transform"
    ),
    icon = c(
      "calculator",
      "card-list",
      "diagram-3",
      "grid-3x3",
      "clipboard-check",
      "activity"
    ),
    guidance = c(
      # new_model_block:
      paste(
        "Fit a regression model. Pick model_type for the outcome",
        "(lm/logistic/poisson/gamma) and write formula as a string:",
        "'response ~ predictor1 + predictor2'. weights and offset are",
        "optional column names; leave them out unless the request needs them."
      ),
      # new_model_summary_block:
      paste(
        "Render a fitted model as a card. Connect it straight to a model /",
        "survival / function block that returns a model object -- do NOT put",
        "a broom adapter in between, this block tidies the model itself.",
        "All six arguments are display choices; the defaults are the",
        "intended look, so set one only when the request asks for it."
      ),
      "", # new_broom_block
      "", # new_correlate_block
      "", # new_stat_test_block
      ""  # new_survival_block
    ),
    arguments = list(
      # new_model_block: flat, fully AI-accessible surface.
      new_arg_specs(
        model_type = new_arg_spec(
          paste(
            "Model family: one of 'lm' (linear, continuous outcome),",
            "'logistic' (binary 0/1 outcome), 'poisson' (counts),",
            "'gamma' (positive continuous)."
          ),
          example = "lm",
          type = arg_enum(c("lm", "logistic", "poisson", "gamma"))
        ),
        formula = new_arg_spec(
          paste(
            "Model formula as a string, e.g. 'mpg ~ hp + wt'. Response on the",
            "left of ~, predictors on the right: '+' adds terms, '*' is a full",
            "interaction (main effects + product), ':' is interaction-only."
          ),
          example = "mpg ~ hp + wt",
          type = arg_string()
        ),
        # weights / offset are optional column names whose value varies in type
        # (column name string or NULL); left untyped, with no worked example.
        weights = new_arg_spec(
          paste(
            "Optional: a column name to use as case weights. Omit unless the",
            "task explicitly calls for weighting."
          ),
          example = NULL
        ),
        offset = new_arg_spec(
          paste(
            "Optional: a column name to use as a model offset (e.g. log",
            "exposure for a Poisson rate). Omit unless explicitly needed."
          ),
          example = NULL
        )
      ),
      # new_model_summary_block: display-only surface, all six options.
      new_arg_specs(
        uncertainty = new_arg_spec(
          paste(
            "What the interval column and the whisker show: 'ci95' (default),",
            "'ci90', 'ci99', 'se' (estimate +/- one standard error, about 68%,",
            "NOT a confidence interval) or 'none'."
          ),
          example = "ci95",
          type = arg_enum(c("ci95", "ci90", "ci99", "se", "none"))
        ),
        significance = new_arg_spec(
          paste(
            "How significance is shown: 'chips' (default: 0.1% / 1% / 5%",
            "coloured badges, 10% grey), 'p' (a p-value column), 'stars', or",
            "'none' -- with a CI drawn, significance is already visible as",
            "'does the whisker cross the reference line'."
          ),
          example = "chips",
          type = arg_enum(c("chips", "p", "stars", "none"))
        ),
        scale = new_arg_spec(
          paste(
            "'auto' (default) exponentiates to odds / rate / hazard ratios",
            "when the model uses a log or logit link and leaves linear models",
            "alone; 'ratio' and 'raw' force it. On the ratio scale the",
            "reference line moves from 0 to 1 and the axis becomes log."
          ),
          example = "auto",
          type = arg_enum(c("auto", "raw", "ratio"))
        ),
        effect_column = new_arg_spec(
          paste(
            "Draw the inline forest column (default TRUE). FALSE gives a plain",
            "numeric table, for a narrow panel or beside a chart that already",
            "carries the picture."
          ),
          example = TRUE,
          type = arg_boolean()
        ),
        facts = new_arg_spec(
          paste(
            "Show the one-line model facts stripe: kind, n, and the fit",
            "measures that suit the model class (default TRUE)."
          ),
          example = TRUE,
          type = arg_boolean()
        ),
        intercept = new_arg_spec(
          paste(
            "Keep the intercept row (default TRUE). It never enters the",
            "forest's scale, so keeping it cannot squash the other terms."
          ),
          example = TRUE,
          type = arg_boolean()
        )
      ),
      NULL, # new_broom_block
      NULL, # new_correlate_block
      NULL, # new_stat_test_block
      NULL  # new_survival_block
    ),
    package = utils::packageName(),
    overwrite = TRUE
  )
}
