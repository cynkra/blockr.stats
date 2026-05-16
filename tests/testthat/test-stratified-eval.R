test_that("stratified_eval passes through when no grouping", {
  dummy_fn <- function(data, values, groups, params) {
    tibble::tibble(method = "test", n = nrow(data))
  }

  result <- blockr.stats:::stratified_eval(
    iris, by_cols = character(),
    fn = dummy_fn, values = "Sepal.Length", groups = character(),
    params = list()
  )
  expect_equal(nrow(result), 1)
  expect_equal(result$n, 150)
  expect_false("by.Species" %in% names(result))
})

test_that("stratified_eval splits by one column", {
  dummy_fn <- function(data, values, groups, params) {
    tibble::tibble(method = "test", n = nrow(data))
  }

  result <- blockr.stats:::stratified_eval(
    iris, by_cols = "Species",
    fn = dummy_fn, values = "Sepal.Length", groups = character(),
    params = list()
  )
  expect_equal(nrow(result), 3)
  expect_true("by.Species" %in% names(result))
  expect_equal(sort(result$by.Species), c("setosa", "versicolor", "virginica"))
  expect_true(all(result$n == 50))
})

test_that("stratified_eval works with real test function", {
  result <- blockr.stats:::stratified_eval(
    iris, by_cols = "Species",
    fn = blockr.stats:::test_normality,
    values = "Sepal.Length", groups = character(),
    params = list(method = "shapiro.wilk")
  )
  expect_equal(nrow(result), 3)
  expect_true("by.Species" %in% names(result))
  expect_true(all(!is.na(result$p.value)))
})

test_that("stratified_eval handles empty by_cols string", {
  dummy_fn <- function(data, values, groups, params) {
    tibble::tibble(method = "test", n = nrow(data))
  }

  result <- blockr.stats:::stratified_eval(
    iris, by_cols = "",
    fn = dummy_fn, values = "Sepal.Length", groups = character(),
    params = list()
  )
  expect_equal(nrow(result), 1)
  expect_equal(result$n, 150)
})
