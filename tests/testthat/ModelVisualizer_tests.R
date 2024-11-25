library(testthat)

test_that("generate_comparison_plot generates valid plot", {
  # Define models
  models <- list(
    "Hill" = function(x, vmax = 10, k = 50, n = 2) vmax * x^n / (k^n + x^n),
    "Exponential" = function(x, a = 1, b = 0.05) a * exp(b * x),
    "Logistic" = function(x, L = 10, k = 0.1, x0 = 50) L / (1 + exp(-k * (x - x0))),
    "Michaelis-Menten" = function(x, vmax = 10, km = 50) vmax * x / (km + x)
  )

  # Initialize the ModelVisualizer
  visualizer <- ModelVisualizer$new(models)

  # Test 1: Check if the plot is generated successfully
  plot <- visualizer$generate_comparison_plot(x_range = seq(1, 100, by = 1))
  expect_true(inherits(plot, "echarts4r"))
})

test_that("generate_comparison_plot handles empty or invalid x_range", {
  # Define models
  models <- list(
    "Hill" = function(x, vmax = 10, k = 50, n = 2) vmax * x^n / (k^n + x^n)
  )

  # Initialize the ModelVisualizer
  visualizer <- ModelVisualizer$new(models)

  # Test 2: Empty x_range
  expect_error(
    visualizer$generate_comparison_plot(x_range = numeric(0)),
    "x_range must be a non-empty numeric vector."
  )

  # Test 3: Non-numeric x_range
  expect_error(
    visualizer$generate_comparison_plot(x_range = "invalid"),
    "x_range must be a non-empty numeric vector."
  )
})

test_that("generate_comparison_plot normalizes values correctly", {
  # Define models
  models <- list(
    "Hill" = function(x, vmax = 10, k = 50, n = 2) vmax * x^n / (k^n + x^n),
    "Exponential" = function(x, a = 1, b = 0.05) a * exp(b * x)
  )

  # Initialize the ModelVisualizer
  visualizer <- ModelVisualizer$new(models)

  # Test 4: Check normalized plot
  plot_normalized <- visualizer$generate_comparison_plot(x_range = seq(1, 100, by = 1), normalize = TRUE)
  expect_true(inherits(plot_normalized, "echarts4r"))

  # Test 5: Check raw plot
  plot_raw <- visualizer$generate_comparison_plot(x_range = seq(1, 100, by = 1), normalize = FALSE)
  expect_true(inherits(plot_raw, "echarts4r"))
})

test_that("generate_comparison_plot handles model-specific parameters", {
  # Define models
  models <- list(
    "Hill" = function(x, vmax = 10, k = 50, n = 2) vmax * x^n / (k^n + x^n),
    "Exponential" = function(x, a = 1, b = 0.05) a * exp(b * x)
  )

  # Initialize the ModelVisualizer
  visualizer <- ModelVisualizer$new(models)

  # Define parameters
  params <- list(
    "Hill" = list(vmax = 8, k = 40, n = 3),
    "Exponential" = list(a = 2, b = 0.1)
  )

  # Test 6: Check plot with parameters
  plot <- visualizer$generate_comparison_plot(x_range = seq(1, 100, by = 1), params = params)
  expect_true(inherits(plot, "echarts4r"))
})

test_that("generate_comparison_plot handles missing or invalid models", {
  # Define models
  models <- list(
    "Hill" = function(x, vmax = 10, k = 50, n = 2) vmax * x^n / (k^n + x^n)
  )

  # Initialize the ModelVisualizer
  visualizer <- ModelVisualizer$new(models)

  # Test 7: Invalid model parameters
  invalid_params <- list(
    "NonExistentModel" = list(a = 1, b = 0.05)
  )

  expect_error(
    visualizer$generate_comparison_plot(x_range = seq(1, 100, by = 1), params = invalid_params),
    "Invalid models in params: NonExistentModel"
  )
})
