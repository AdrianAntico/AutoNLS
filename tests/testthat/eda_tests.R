library(data.table)
library(testthat)
library(AutoNLS)

sample_data <- data.table::data.table(
  A = stats::rnorm(100),
  B = stats::rnorm(100, 10, 5),
  C = sample(letters[1:5], 100, replace = TRUE),
  D = stats::rpois(100, 2)
)

test_that("correlate computes Pearson and Spearman correlations correctly", {
  # Example dataset
  data <- data.table::data.table(
    x1 = rnorm(100),
    x2 = runif(100, 0, 10),
    x3 = sample(letters[1:5], 100, replace = TRUE), # Non-numeric
    y = 10 * rnorm(100)
  )

  # Initialize EDA class
  eda <- EDA$new(data)

  # Test correlation with target variable 'y'
  correlations <- eda$correlate(target_col = "y")

  # Validate structure and contents of the output
  expect_true(data.table::is.data.table(correlations))
  expect_equal(names(correlations), c("Target", "Predictor", "Pearson", "Spearman", "Difference"))

  # Ensure that only numeric columns are included
  expect_setequal(correlations$Predictor, c("x1", "x2"))

  # Ensure that Pearson and Spearman correlations are numeric
  expect_true(all(sapply(correlations[, .(Pearson, Spearman)], is.numeric)))
})

test_that("correlate handles datasets with insufficient numeric columns", {
  # Dataset with no numeric predictors
  data <- data.table::data.table(
    group = sample(letters[1:5], 100, replace = TRUE),
    category = sample(letters[6:10], 100, replace = TRUE)
  )

  # Initialize EDA class
  eda <- EDA$new(data)

  # Test correlation
  testthat::expect_error(eda$correlate(target_col = "category"))
})

eda <- EDA$new(sample_data)

test_that("EDA initializes correctly", {
  expect_true(R6::is.R6(eda))
  expect_equal(eda$data, sample_data)
})

test_that("summarize calculates statistics correctly", {
  summary_stats <- eda$summarize()
  expect_true(is.data.table(summary_stats))
  expect_equal(names(summary_stats), c("Variable", "Mean", "Median", "Variance", "NA_Count"))
})

test_that("visualize_distributions generates valid plots", {
  eda$visualize_distributions(
    title_prefix = "Test Distribution of",
    bins = 10,
    add_density = TRUE
  )
  expect_true(is.list(eda$plots))
  expect_true(all(sapply(eda$plots, function(p) inherits(p, "echarts4r"))))
})

test_that("visualize_scatterplots generates valid scatterplots", {
  eda$visualize_scatterplots(target_col = "B", input_cols = "A")
  expect_true(is.list(eda$plots))
  numeric_cols <- names(sample_data)[sapply(sample_data, is.numeric)]
  expect_true(all(sapply(eda$plots, function(p) inherits(p, "echarts4r"))))
})
