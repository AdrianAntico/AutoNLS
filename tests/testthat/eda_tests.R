library(data.table)
library(testthat)

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
  expect_equal(names(correlations), c("Predictor", "Pearson", "Spearman", "Difference"))

  # Ensure that only numeric columns are included
  expect_setequal(correlations$Predictor, c("x1", "x2"))

  # Ensure that Pearson and Spearman correlations are numeric
  expect_true(all(sapply(correlations[, .(Pearson, Spearman)], is.numeric)))

  # Ensure that the Difference column is non-negative
  expect_true(all(correlations$Difference >= 0))
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
  correlations <- eda$correlate(target_col = "category")

  # Validate the output message
  expect_equal(correlations, "No numeric predictors available for correlation with the target.")
})


eda <- EDA$new(sample_data)

test_that("EDA initializes correctly", {
  expect_true(R6::is.R6(eda))
  expect_equal(eda$data, sample_data)
})

test_that("summarize calculates statistics correctly", {
  summary_stats <- eda$summarize()
  expect_true(is.data.table(summary_stats))
  expect_equal(names(summary_stats), c("Mean", "Median", "Variance", "NA_Count"))
  expect_true(all(sapply(summary_stats, is.list)))
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
  eda$visualize_scatterplots()
  expect_true(is.list(eda$plots))
  numeric_cols <- names(sample_data)[sapply(sample_data, is.numeric)]
  expected_combinations <- choose(length(numeric_cols), 2)
  expect_equal(length(eda$plots), expected_combinations)
  expect_true(all(sapply(eda$plots, function(p) inherits(p, "echarts4r"))))
})

test_that("render_all executes all methods and returns expected outputs", {
  results <- eda$render_all(y_col = "A")
  expect_true(is.list(results))
  expect_equal(names(results), c("Summary", "Correlation", "Plots"))
  expect_true(is.data.table(results$Summary))
  expect_true(is.list(results$Plots))
  expect_true(all(sapply(results$Plots, function(p) inherits(p, "echarts4r"))))
})

test_that("EDA visualize_distributions resets plots", {
  eda <- EDA$new(sample_data)
  eda$visualize_scatterplots()
  expect_true("A_vs_B" %in% names(eda$plots))
  eda$visualize_distributions()
  expect_false("x_vs_y" %in% names(eda$plots))
  expect_true(all(c("A", "B") %in% names(eda$plots)))
})

