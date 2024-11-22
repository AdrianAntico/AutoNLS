library(data.table)
library(testthat)

test_that("generate_comparison_plot generates valid plots", {
  # Sample dataset
  sample_data <- data.table::data.table(
    x = seq(1, 100, by = 1),
    y = 10 * seq(1, 100, by = 1)^1.2 / (50 + seq(1, 100, by = 1)^1.2) + rnorm(100, mean = 0, sd = 0.5)
  )

  # Ensure response variable remains positive
  sample_data[, y := pmax(y, 1e-3)]

  # Initialize NonLinearFitter
  fitter <- NonLinearFitter$new(sample_data)
  fitter$add_model("Hill")

  # Fit models
  fit_results <- fitter$fit_models(x_col = "x", y_col = "y")

  # Initialize NonLinearModelEvaluator
  evaluator <- NonLinearModelEvaluator$new(fit_results, data = sample_data)

  # Test 1: Function generates a list of plots
  plots <- evaluator$generate_comparison_plot(sample_data, x_col = "x", y_col = "y")
  expect_true(is.list(plots))
  expect_equal(length(plots), length(fit_results))
  expect_true(all(sapply(plots, function(p) inherits(p, "echarts4r"))))
})

test_that("generate_comparison_plot fails with missing columns", {
  # Sample dataset
  sample_data <- data.table::data.table(
    x = seq(1, 100, by = 1),
    y = 10 * seq(1, 100, by = 1)^1.2 / (50 + seq(1, 100, by = 1)^1.2) + rnorm(100, mean = 0, sd = 0.5)
  )

  # Ensure response variable remains positive
  sample_data[, y := pmax(y, 1e-3)]

  # Initialize NonLinearFitter
  fitter <- NonLinearFitter$new(sample_data)
  fitter$add_model("Hill")

  # Fit models
  fit_results <- fitter$fit_models(x_col = "x", y_col = "y")

  # Initialize NonLinearModelEvaluator
  evaluator <- NonLinearModelEvaluator$new(fit_results, data = sample_data)

  # Test 2: Missing predictor column
  expect_error(
    evaluator$generate_comparison_plot(sample_data, x_col = "z", y_col = "y"),
    "x_col and y_col must exist in the dataset."
  )

  # Test 3: Missing response column
  expect_error(
    evaluator$generate_comparison_plot(sample_data, x_col = "x", y_col = "z"),
    "x_col and y_col must exist in the dataset."
  )
})

test_that("generate_comparison_plot handles empty or NULL fit_results", {
  # Sample dataset
  sample_data <- data.table::data.table(
    x = seq(1, 100, by = 1),
    y = 10 * seq(1, 100, by = 1)^1.2 / (50 + seq(1, 100, by = 1)^1.2) + rnorm(100, mean = 0, sd = 0.5)
  )

  # Ensure response variable remains positive
  sample_data[, y := pmax(y, 1e-3)]

  # Initialize NonLinearModelEvaluator with empty fit_results
  evaluator <- NonLinearModelEvaluator$new(list(), data = sample_data)

  # Test 4: No fitted models to evaluate
  expect_error(
    evaluator$generate_comparison_plot(data = sample_data, x_col = "x", y_col = "y"),
    "No fitted models to evaluate."
  )
})
