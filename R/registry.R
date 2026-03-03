register_stats_blocks <- function() {
  blockr.core::register_blocks(
    c("new_stat_test_block", "new_padjust_block"),
    name = c("Statistical Test", "P-Value Adjustment"),
    description = c(
      "Run statistical tests (normality, t-test, Wilcoxon, Kruskal-Wallis, homogeneity, correlation) with optional grouping",
      "Adjust p-values for multiple comparisons (Bonferroni, FDR, Holm, etc.)"
    ),
    category = c("transform", "transform"),
    icon = c("clipboard2-pulse", "calculator"),
    package = utils::packageName(),
    overwrite = TRUE
  )
}
