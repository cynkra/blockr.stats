# Statistical test block demo
#
# Showcases:
#   - Normality test (one-sample) on iris, stratified by Species
#   - T-test (comparison) on iris Sepal.Length by Species
#   - Correlation (association) between Sepal.Length and Petal.Length
#   - P-value adjustment on the t-test results
#
library(blockr)
pkgload::load_all("blockr.stats")

run_app(
  blocks = c(
    data = new_dataset_block(dataset = "iris"),

    # One-sample: Shapiro-Wilk normality per species
    normality = new_stat_test_block(
      type = "normality",
      values = "Sepal.Length",
      by = "Species",
      method = "shapiro.wilk"
    ),

    # Comparison: Welch t-test on Sepal.Length across Species
    ttest = new_stat_test_block(
      type = "t_test",
      values = "Sepal.Length",
      groups = "Species",
      variant = "welch"
    ),

    # Adjust p-values from the t-test
    adjusted = new_padjust_block(
      method = "bonferroni"
    ),

    # Association: Pearson correlation between sepal and petal measurements
    correlation = new_stat_test_block(
      type = "correlation",
      values = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
      method = "pearson"
    )
  ),
  links = c(
    new_link("data", "normality", "data"),
    new_link("data", "ttest", "data"),
    new_link("ttest", "adjusted", "data"),
    new_link("data", "correlation", "data")
  )
)
