# Tests for data utility functions: colSummmary, cols2numeric

# colSummmary tests

test_that("colSummmary returns mean for numeric columns", {
  df <- data.frame(
    x = c(1, 2, 3, 4, 5),
    y = c(10, 20, 30, 40, 50)
  )
  result <- colSummmary(df)

  expect_equal(as.numeric(result["x"]), 3)
  expect_equal(as.numeric(result["y"]), 30)
})

test_that("colSummmary returns mode for character columns", {
  df <- data.frame(
    category = c("a", "b", "a", "a", "c"),
    stringsAsFactors = FALSE
  )
  result <- colSummmary(df)

  expect_equal(result["category"], c(category = "a"))
})

test_that("colSummmary handles mixed data types", {
  df <- data.frame(
    numeric_col = c(1, 2, 3, 4),
    char_col = c("x", "y", "x", "x"),
    stringsAsFactors = FALSE
  )
  result <- colSummmary(df)

  expect_equal(as.numeric(result["numeric_col"]), 2.5)
  expect_equal(result["char_col"], c(char_col = "x"))
})

test_that("colSummmary handles factors with numeric levels", {
  df <- data.frame(
    factor_num = factor(c("1.5", "2.5", "3.5", "4.5"))
  )
  result <- colSummmary(df)

  # f2n should convert this to numeric, then take mean
  expect_equal(as.numeric(result["factor_num"]), 3.0)
})

test_that("colSummmary handles factors with non-numeric levels", {
  df <- data.frame(
    factor_char = factor(c("cat", "dog", "cat", "cat"))
  )
  result <- colSummmary(df)

  expect_equal(result["factor_char"], c(factor_char = "cat"))
})

test_that("colSummmary works with single column data frame", {
  df <- data.frame(x = c(1, 2, 3, 4, 5))
  result <- colSummmary(df)

  expect_length(result, 1)
  expect_equal(as.numeric(result), 3)
})

test_that("colSummmary handles NA values in numeric columns", {
  df <- data.frame(
    x = c(1, 2, NA, 4, 5)
  )
  result <- colSummmary(df)

  expect_equal(as.numeric(result["x"]), 3)  # mean ignores NA
})

test_that("colSummmary uses mtcars correctly", {
  result <- colSummmary(mtcars[, 1:4])

  expect_length(result, 4)
  expect_equal(as.numeric(result["mpg"]), mean(mtcars$mpg))
  expect_equal(as.numeric(result["cyl"]), mean(mtcars$cyl))
})

# cols2numeric tests

test_that("cols2numeric converts factor columns with numeric levels", {
  df <- data.frame(
    a = factor(c("1.5", "2.5", "3.5")),
    b = c(1, 2, 3)
  )
  result <- cols2numeric(df)

  expect_equal(as.numeric(result[, "a"]), c(1.5, 2.5, 3.5))
  expect_equal(as.numeric(result[, "b"]), c(1, 2, 3))
})

test_that("cols2numeric preserves non-numeric character columns", {
  df <- data.frame(
    a = factor(c("1.5", "2.5", "3.5")),
    b = c("x", "y", "z"),
    stringsAsFactors = FALSE
  )

  # Suppress warnings about NAs
  result <- suppressWarnings(cols2numeric(df))

  expect_equal(as.numeric(result[, "a"]), c(1.5, 2.5, 3.5))
  # Non-numeric columns should stay as character
  expect_equal(result[, "b"], c("x", "y", "z"))
})

test_that("cols2numeric handles already numeric columns", {
  df <- data.frame(
    x = c(1.5, 2.5, 3.5),
    y = c(10, 20, 30)
  )
  result <- cols2numeric(df)

  expect_equal(as.numeric(result[, "x"]), c(1.5, 2.5, 3.5))
  expect_equal(as.numeric(result[, "y"]), c(10, 20, 30))
})

test_that("cols2numeric handles integer columns stored as character", {
  df <- data.frame(
    int_char = c("1", "2", "3"),
    stringsAsFactors = FALSE
  )
  result <- cols2numeric(df)

  expect_equal(as.numeric(result[, "int_char"]), c(1, 2, 3))
})

test_that("cols2numeric handles single column", {
  df <- data.frame(
    a = factor(c("1.5", "2.5", "3.5"))
  )
  result <- cols2numeric(df)

  expect_equal(as.numeric(result), c(1.5, 2.5, 3.5))
})

test_that("cols2numeric handles negative numbers in character form", {
  df <- data.frame(
    neg = factor(c("-1.5", "-2.5", "0", "1.5"))
  )
  result <- cols2numeric(df)

  expect_equal(as.numeric(result[, "neg"]), c(-1.5, -2.5, 0, 1.5))
})

test_that("cols2numeric handles mixed convertible and non-convertible", {
  df <- data.frame(
    num_factor = factor(c("1", "2", "3")),
    char_factor = factor(c("a", "b", "c")),
    stringsAsFactors = FALSE
  )

  # Suppress warnings for NA coercion
  result <- suppressWarnings(cols2numeric(df))

  expect_equal(as.numeric(result[, "num_factor"]), c(1, 2, 3))
  # char_factor should remain unchanged
  expect_equal(result[, "char_factor"], c("a", "b", "c"))
})

test_that("cols2numeric handles empty data frame gracefully", {
  df <- data.frame(x = numeric(0), y = character(0))

  # This may or may not work depending on edge case handling
  # Just check it doesn't error catastrophically
  expect_no_error(result <- suppressWarnings(cols2numeric(df)))
})

test_that("cols2numeric preserves row count", {
  df <- data.frame(
    a = factor(c("1", "2", "3", "4", "5")),
    b = c("x", "y", "z", "w", "v"),
    stringsAsFactors = FALSE
  )
  result <- suppressWarnings(cols2numeric(df))

  expect_equal(nrow(result), 5)
})
