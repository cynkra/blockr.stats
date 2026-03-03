test_that("test_normality returns correct structure", {
  result <- blockr.stats:::test_normality(
    iris, values = "Sepal.Length", groups = character(),
    params = list(method = "shapiro.wilk")
  )
  expect_s3_class(result, "data.frame")
  expect_true(all(c("method", "values", "n", "statistic", "p.value") %in% names(result)))
  expect_equal(nrow(result), 1)
  expect_equal(result$values, "Sepal.Length")
  expect_true(result$n == 150)
})

test_that("test_normality handles multiple columns", {
  result <- blockr.stats:::test_normality(
    iris, values = c("Sepal.Length", "Sepal.Width"), groups = character(),
    params = list(method = "shapiro.wilk")
  )
  expect_equal(nrow(result), 2)
  expect_equal(result$values, c("Sepal.Length", "Sepal.Width"))
})

test_that("test_normality handles different methods", {
  for (m in c("shapiro.wilk", "anderson.darling", "lilliefors",
              "cramer.vonmises", "jarque.bera")) {
    result <- blockr.stats:::test_normality(
      iris, values = "Sepal.Length", groups = character(),
      params = list(method = m)
    )
    expect_equal(nrow(result), 1)
    expect_false(is.na(result$p.value))
  }
})

test_that("test_normality warns on too few observations", {
  small_data <- data.frame(x = c(1, 2))
  expect_warning(
    result <- blockr.stats:::test_normality(
      small_data, values = "x", groups = character(),
      params = list(method = "shapiro.wilk")
    ),
    "fewer than 3"
  )
  expect_true(is.na(result$method))
})

test_that("test_t_twosample returns correct structure", {
  result <- blockr.stats:::test_t_twosample(
    iris[iris$Species != "virginica", ],
    values = "Sepal.Length", groups = "Species",
    params = list(alternative = "two.sided", variant = "welch",
                  conf_level = 0.95, null = 0)
  )
  expect_s3_class(result, "data.frame")
  expect_true(all(c("method", "values", "groups", "group.x", "group.y",
                     "n.x", "n.y", "statistic", "p.value") %in% names(result)))
  expect_equal(nrow(result), 1)
})

test_that("test_t_twosample does pairwise for >2 groups", {
  result <- blockr.stats:::test_t_twosample(
    iris, values = "Sepal.Length", groups = "Species",
    params = list(alternative = "two.sided", variant = "welch",
                  conf_level = 0.95, null = 0)
  )
  # 3 species -> 3 pairwise comparisons
  expect_equal(nrow(result), 3)
})

test_that("test_wilcoxon returns correct structure", {
  result <- blockr.stats:::test_wilcoxon(
    iris[iris$Species != "virginica", ],
    values = "Sepal.Length", groups = "Species",
    params = list(alternative = "two.sided", conf_level = 0.95, null = 0)
  )
  expect_s3_class(result, "data.frame")
  expect_true("p.value" %in% names(result))
  expect_equal(nrow(result), 1)
})

test_that("test_kruskal_wallis returns correct structure", {
  result <- blockr.stats:::test_kruskal_wallis(
    iris, values = "Sepal.Length", groups = "Species",
    params = list()
  )
  expect_s3_class(result, "data.frame")
  expect_true(all(c("n", "n.groups", "statistic", "df", "p.value") %in% names(result)))
  expect_equal(nrow(result), 1)
  expect_equal(result$n.groups, 3)
})

test_that("test_homogeneity returns correct structure for bartlett and fligner", {
  for (m in c("bartlett", "fligner")) {
    result <- blockr.stats:::test_homogeneity(
      iris, values = "Sepal.Length", groups = "Species",
      params = list(method = m)
    )
    expect_s3_class(result, "data.frame")
    expect_true(all(c("method", "statistic", "p.value") %in% names(result)))
    expect_equal(nrow(result), 1)
    expect_false(is.na(result$p.value))
  }
})

test_that("test_homogeneity returns correct structure for levene and brown.forsythe", {
  skip_if_not_installed("car")
  for (m in c("levene", "brown.forsythe")) {
    result <- blockr.stats:::test_homogeneity(
      iris, values = "Sepal.Length", groups = "Species",
      params = list(method = m)
    )
    expect_s3_class(result, "data.frame")
    expect_true(all(c("method", "statistic", "p.value") %in% names(result)))
    expect_equal(nrow(result), 1)
    expect_false(is.na(result$p.value))
  }
})

test_that("test_correlation returns correct structure", {
  result <- blockr.stats:::test_correlation(
    iris, values = c("Sepal.Length", "Sepal.Width"), groups = character(),
    params = list(method = "pearson", alternative = "two.sided", conf_level = 0.95)
  )
  expect_s3_class(result, "data.frame")
  expect_true(all(c("method", "values.x", "values.y", "cor", "p.value") %in% names(result)))
  expect_equal(nrow(result), 1)
})

test_that("test_correlation does pairwise for >2 columns", {
  result <- blockr.stats:::test_correlation(
    iris, values = c("Sepal.Length", "Sepal.Width", "Petal.Length"),
    groups = character(),
    params = list(method = "pearson", alternative = "two.sided", conf_level = 0.95)
  )
  # 3 columns -> 3 pairs
  expect_equal(nrow(result), 3)
})

test_that("test_correlation requires at least 2 columns", {
  expect_warning(
    result <- blockr.stats:::test_correlation(
      iris, values = "Sepal.Length", groups = character(),
      params = list(method = "pearson", alternative = "two.sided", conf_level = 0.95)
    ),
    "at least 2"
  )
  expect_equal(nrow(result), 0)
})
