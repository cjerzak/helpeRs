# Tests for helper utility functions: f2n, fixZeroEndings, vcovCluster, WidenMargins

test_that("f2n converts factors with numeric levels correctly", {
  # Factor with numeric string levels
  x_factor <- factor(c("1.5", "2.3", "3.1"))
  result <- f2n(x_factor)

  expect_type(result, "double")
  expect_equal(result, c(1.5, 2.3, 3.1))
})

test_that("f2n handles integer factors correctly", {
  x_int_factor <- factor(c("10", "20", "30"))
  result <- f2n(x_int_factor)

  expect_equal(result, c(10, 20, 30))
})

test_that("f2n handles character vectors", {
  x_char <- c("1.5", "2.5", "3.5")
  result <- f2n(x_char)

  expect_equal(result, c(1.5, 2.5, 3.5))
})

test_that("f2n returns NA for non-numeric strings", {
  x_mixed <- factor(c("1.5", "abc", "3.0"))

  expect_warning(
    result <- f2n(x_mixed),
    "NAs introduced by coercion"
  )
  expect_equal(result[1], 1.5)
  expect_true(is.na(result[2]))
  expect_equal(result[3], 3.0)
})

test_that("f2n handles numeric input (passthrough)", {
  x_num <- c(1.5, 2.5, 3.5)
  result <- f2n(x_num)

  expect_equal(result, x_num)
})

test_that("f2n handles negative numbers", {
  x_neg <- factor(c("-1.5", "-2.0", "0", "1.5"))
  result <- f2n(x_neg)

  expect_equal(result, c(-1.5, -2.0, 0, 1.5))
})

# fixZeroEndings tests

test_that("fixZeroEndings pads to 2 decimal places by default", {
  input <- c("1.5", "2", "3.14")
  result <- fixZeroEndings(input)

  expect_equal(result, c("1.50", "2.00", "3.14"))
})

test_that("fixZeroEndings pads to custom decimal places", {
  input <- c(1.5, 2.33, 3)
  result <- fixZeroEndings(input, roundAt = 3)

  expect_equal(result, c("1.500", "2.330", "3.000"))
})

test_that("fixZeroEndings handles integers", {
  input <- c(1, 2, 3)
  result <- fixZeroEndings(input, roundAt = 2)

  expect_equal(result, c("1.00", "2.00", "3.00"))
})

test_that("fixZeroEndings handles single value", {
  result <- fixZeroEndings("5.1", roundAt = 3)

  expect_equal(result, "5.100")
})

test_that("fixZeroEndings handles values already at correct precision", {
  input <- c("1.00", "2.50", "3.99")
  result <- fixZeroEndings(input, roundAt = 2)

  expect_equal(result, c("1.00", "2.50", "3.99"))
})

test_that("fixZeroEndings handles values with more decimals than roundAt", {
  # Note: this function assumes input is pre-rounded
  # If input has more decimals than roundAt, it will error
  # This tests that proper input (correctly rounded) works
  input <- c("1.12")  # Properly rounded to 2 decimals
  result <- fixZeroEndings(input, roundAt = 2)

  expect_equal(result, "1.12")
})

test_that("fixZeroEndings handles zero", {
  result <- fixZeroEndings(0, roundAt = 2)

  expect_equal(result, "0.00")
})

test_that("fixZeroEndings handles negative numbers", {
  input <- c("-1.5", "-2")
  result <- fixZeroEndings(input, roundAt = 2)

  expect_equal(result, c("-1.50", "-2.00"))
})

# WidenMargins tests

test_that("WidenMargins adds adjustwidth environment", {
  input <- c(
    "\\begin{table}[htbp]",
    "\\centering",
    "Table content",
    "\\end{table}"
  )
  result <- WidenMargins(input)

  expect_true(any(grepl("adjustwidth", result)))
  expect_true(any(grepl("-.5in", result)))
})

test_that("WidenMargins preserves table content", {
  input <- c(
    "\\begin{table}[htbp]",
    "\\centering",
    "Some important content",
    "\\end{table}"
  )
  result <- WidenMargins(input)

  expect_true(any(grepl("Some important content", result)))
})

test_that("WidenMargins handles input without table environment gracefully", {
  input <- c("Some text without table environment")
  result <- WidenMargins(input)

  # Should pass through unchanged
  expect_equal(result, input)
})

# vcovCluster tests

test_that("vcovCluster returns a matrix", {
  skip_if_not_installed("sandwich")

  data(mtcars)
  fit <- lm(mpg ~ wt + hp, data = mtcars)

  V_clust <- vcovCluster(fit, "cyl")

  expect_true(is.matrix(V_clust))
})

test_that("vcovCluster returns correct dimensions", {
  skip_if_not_installed("sandwich")

  data(mtcars)
  fit <- lm(mpg ~ wt + hp, data = mtcars)

  V_clust <- vcovCluster(fit, "cyl")

  # Should be 3x3 for intercept, wt, hp

expect_equal(dim(V_clust), c(3, 3))
})

test_that("vcovCluster produces symmetric matrix", {
  skip_if_not_installed("sandwich")

  data(mtcars)
  fit <- lm(mpg ~ wt + hp, data = mtcars)

  V_clust <- vcovCluster(fit, "cyl")

  expect_equal(V_clust, t(V_clust))
})

test_that("vcovCluster produces positive definite matrix", {
  skip_if_not_installed("sandwich")

  data(mtcars)
  fit <- lm(mpg ~ wt + hp, data = mtcars)

  V_clust <- vcovCluster(fit, "cyl")

  # Check all eigenvalues are positive
  eigenvalues <- eigen(V_clust)$values
  expect_true(all(eigenvalues > 0))
})

test_that("vcovCluster handles single covariate model", {
  skip_if_not_installed("sandwich")

  data(mtcars)
  fit <- lm(mpg ~ wt, data = mtcars)

  V_clust <- vcovCluster(fit, "cyl")

  expect_equal(dim(V_clust), c(2, 2))
})

test_that("vcovCluster differs from standard vcov", {
  skip_if_not_installed("sandwich")

  data(mtcars)
  fit <- lm(mpg ~ wt + hp, data = mtcars)

  V_clust <- vcovCluster(fit, "cyl")
  V_standard <- vcov(fit)

  # Clustered SEs should generally differ from standard SEs
  expect_false(isTRUE(all.equal(V_clust, V_standard)))
})

test_that("vcovCluster works with different cluster variables", {
  skip_if_not_installed("sandwich")

  data(mtcars)
  fit <- lm(mpg ~ wt + hp, data = mtcars)

  V_cyl <- vcovCluster(fit, "cyl")
  V_gear <- vcovCluster(fit, "gear")

  # Different cluster structures should give different results
  expect_false(isTRUE(all.equal(V_cyl, V_gear)))
})
