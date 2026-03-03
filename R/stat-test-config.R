#' Test group labels for the UI
#' @noRd
test_groups <- list(
  one_sample  = "One-sample",
  comparison  = "Comparison",
  association = "Association"
)

#' Configuration for all supported statistical tests
#'
#' Each entry defines:
#' - group: which UI row (one_sample, comparison, association)
#' - label: display name
#' - icon: Bootstrap icon name
#' - inputs: which column selectors to show (values, groups)
#' - params: test-specific parameters with UI metadata
#' - test_fn: name of the pluggable test function
#'
#' @noRd
test_config <- list(
  normality = list(
    group = "one_sample",
    label = "Normality",
    icon = "activity",
    inputs = list(
      values = list(role = "required", type = "numeric", multiple = TRUE),
      groups = list(role = "hidden")
    ),
    params = list(
      method = list(
        type = "select",
        label = "Method",
        choices = c(
          "Shapiro-Wilk"     = "shapiro.wilk",
          "Anderson-Darling" = "anderson.darling",
          "Shapiro-Francia"  = "shapiro.francia",
          "Lilliefors"       = "lilliefors",
          "Cramer-von Mises" = "cramer.vonmises",
          "Jarque-Bera"      = "jarque.bera"
        ),
        default = "shapiro.wilk"
      )
    ),
    test_fn = "test_normality"
  ),
  t_test = list(
    group = "comparison",
    label = "T-test",
    icon = "arrows-expand",
    inputs = list(
      values = list(role = "required", type = "numeric", multiple = TRUE),
      groups = list(role = "required", type = "factor")
    ),
    params = list(
      variant = list(
        type = "select",
        label = "Variance assumption",
        choices = c("Welch" = "welch", "Pooled" = "pooled"),
        default = "welch"
      ),
      alternative = list(
        type = "select",
        label = "Alternative",
        choices = c("Two sided" = "two.sided", "Greater" = "greater", "Less" = "less"),
        default = "two.sided",
        advanced = TRUE
      ),
      conf_level = list(
        type = "numeric",
        label = "Confidence level",
        default = 0.95,
        min = 0, max = 1, step = 0.01,
        advanced = TRUE
      ),
      null = list(
        type = "numeric",
        label = "Null difference",
        default = 0,
        step = 0.1,
        advanced = TRUE
      )
    ),
    test_fn = "test_t_twosample"
  ),
  wilcoxon = list(
    group = "comparison",
    label = "Wilcoxon",
    icon = "bar-chart-steps",
    inputs = list(
      values = list(role = "required", type = "numeric", multiple = TRUE),
      groups = list(role = "required", type = "factor")
    ),
    params = list(
      alternative = list(
        type = "select",
        label = "Alternative",
        choices = c("Two sided" = "two.sided", "Greater" = "greater", "Less" = "less"),
        default = "two.sided",
        advanced = TRUE
      ),
      conf_level = list(
        type = "numeric",
        label = "Confidence level",
        default = 0.95,
        min = 0, max = 1, step = 0.01,
        advanced = TRUE
      ),
      null = list(
        type = "numeric",
        label = "Null location shift",
        default = 0,
        step = 0.1,
        advanced = TRUE
      )
    ),
    test_fn = "test_wilcoxon"
  ),
  kruskal_wallis = list(
    group = "comparison",
    label = "Kruskal-Wallis",
    icon = "distribute-vertical",
    inputs = list(
      values = list(role = "required", type = "numeric", multiple = TRUE),
      groups = list(role = "required", type = "factor")
    ),
    params = list(),
    test_fn = "test_kruskal_wallis"
  ),
  homogeneity = list(
    group = "comparison",
    label = "Homogeneity",
    icon = "symmetry-vertical",
    inputs = list(
      values = list(role = "required", type = "numeric", multiple = TRUE),
      groups = list(role = "required", type = "factor")
    ),
    params = list(
      method = list(
        type = "select",
        label = "Method",
        choices = c(
          "Bartlett"        = "bartlett",
          "Fligner-Killeen" = "fligner",
          "Levene"          = "levene",
          "Brown-Forsythe"  = "brown.forsythe"
        ),
        default = "bartlett"
      )
    ),
    test_fn = "test_homogeneity"
  ),
  correlation = list(
    group = "association",
    label = "Correlation",
    icon = "graph-up",
    inputs = list(
      values = list(role = "required", type = "numeric", multiple = TRUE, min_select = 2),
      groups = list(role = "hidden")
    ),
    params = list(
      method = list(
        type = "select",
        label = "Method",
        choices = c("Pearson" = "pearson", "Spearman" = "spearman", "Kendall" = "kendall"),
        default = "pearson"
      ),
      alternative = list(
        type = "select",
        label = "Alternative",
        choices = c("Two sided" = "two.sided", "Greater" = "greater", "Less" = "less"),
        default = "two.sided",
        advanced = TRUE
      ),
      conf_level = list(
        type = "numeric",
        label = "Confidence level",
        default = 0.95,
        min = 0, max = 1, step = 0.01,
        advanced = TRUE
      )
    ),
    test_fn = "test_correlation"
  )
)
