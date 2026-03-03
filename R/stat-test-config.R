#' Category definitions for the UI
#' @noRd
test_categories <- list(
  normality   = list(label = "Normality",   icon = "chart-area"),
  mean_median = list(label = "Mean/Median", icon = "not-equal"),
  variance    = list(label = "Variance",    icon = "balance-scale"),
  correlation = list(label = "Correlation", icon = "chart-line")
)

#' Configuration for all supported statistical tests
#'
#' Each entry defines:
#' - category: which UI category (normality, mean_median, variance, correlation)
#' - label: display name
#' - icon: Bootstrap icon name
#' - inputs: which column selectors to show (values, groups)
#' - params: test-specific parameters with UI metadata
#' - test_fn: name of the pluggable test function
#'
#' @noRd
test_config <- list(
  normality = list(
    category = "normality",
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
    category = "mean_median",
    label = "Two-sample t-test",
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
    category = "mean_median",
    label = "Two-sample Wilcoxon",
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
    category = "mean_median",
    label = "Kruskal-Wallis",
    icon = "distribute-vertical",
    inputs = list(
      values = list(role = "required", type = "numeric", multiple = TRUE),
      groups = list(role = "required", type = "factor")
    ),
    params = list(),
    test_fn = "test_kruskal_wallis"
  ),
  anova_oneway = list(
    category = "mean_median",
    label = "One-way ANOVA",
    icon = "bar-chart",
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
      )
    ),
    test_fn = "test_anova_oneway"
  ),
  t_test_one = list(
    category = "mean_median",
    label = "One-sample t-test",
    icon = "arrows-expand",
    inputs = list(
      values = list(role = "required", type = "numeric", multiple = TRUE),
      groups = list(role = "hidden")
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
        label = "Null mean",
        default = 0,
        step = 0.1,
        advanced = TRUE
      )
    ),
    test_fn = "test_t_onesample"
  ),
  wilcoxon_one = list(
    category = "mean_median",
    label = "One-sample Wilcoxon",
    icon = "bar-chart-steps",
    inputs = list(
      values = list(role = "required", type = "numeric", multiple = TRUE),
      groups = list(role = "hidden")
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
        label = "Null location",
        default = 0,
        step = 0.1,
        advanced = TRUE
      )
    ),
    test_fn = "test_wilcoxon_onesample"
  ),
  homogeneity = list(
    category = "variance",
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
    category = "correlation",
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

#' Look up which category a test type belongs to
#' @noRd
type_to_category <- function(type) {
  test_config[[type]]$category
}

#' Get all test keys belonging to a category
#' @noRd
category_tests <- function(category) {
  keys <- names(test_config)
  keys[vapply(keys, function(k) identical(test_config[[k]]$category, category), logical(1))]
}
