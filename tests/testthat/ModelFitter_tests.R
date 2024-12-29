# Load the necessary libraries
library(testthat)
library(data.table)

# Sample data for testing
sample_data <- data.table(
  `x-value` = 1:100,
  y = 5 / (1 + exp(-0.1 * (1:100 - 50)))
)

# Initialize ModelFitter class
test_that("Initialization works correctly", {
  fitter <- ModelFitter$new(sample_data)
  expect_true(inherits(fitter, "ModelFitter"))
  expect_true(is.data.table(fitter$data))
  expect_equal(nrow(fitter$data), 100)
  expect_equal(names(fitter$data), c("x", "y"))
})

# Test list_models()
test_that("list_models returns a valid table of models", {
  fitter <- ModelFitter$new(sample_data)
  models <- fitter$list_models()
  expect_true(is.data.table(models))
  expect_equal(names(models), c("Model", "Description", "Formula"))
  expect_true("Hill" %in% models$Model)
  expect_true("Logistic" %in% models$Model)
})

# Test add_model()
test_that("add_model works correctly with pre-defined models", {
  fitter <- ModelFitter$new(sample_data)
  fitter$add_model("Hill")
  expect_true("Hill" %in% names(fitter$models))
  expect_equal(fitter$models$Hill$formula, fitter$model_library$Hill$formula)
})

test_that("add_model works correctly with custom models", {
  fitter <- ModelFitter$new(sample_data)
  custom_formula <- y ~ a * exp(-b * x)
  custom_params <- list(a = 1, b = 0.1)
  fitter$add_model("Custom", formula = custom_formula, start_params = custom_params)
  expect_true("Custom" %in% names(fitter$models))
  expect_equal(fitter$models$Custom$formula, custom_formula)
  expect_equal(fitter$models$Custom$start_params, custom_params)
})

# Test fit_models()
test_that("fit_models works correctly for pre-defined models", {
  fitter <- ModelFitter$new(sample_data)
  fitter$add_model("Hill")
  fit_results <- fitter$fit_models(x_col = "x-value", y_col = "y")
  expect_true(length(fit_results) > 0)
  expect_true(inherits(fit_results[[1]], "nls"))
})

test_that("fit_models handles errors gracefully", {
  faulty_data <- data.table(x = 1:10, z = 1:10)
  fitter <- ModelFitter$new(faulty_data)
  fitter$add_model("Hill")
  expect_error(
    fitter$fit_models(x_col = "x", y_col = "y"),
    "x_col and y_col must exist in the dataset."
  )
})

# Test model_comparison_plot()
test_that("model_comparison_plot generates valid plots", {
  fitter <- ModelFitter$new(sample_data)

  # Add multiple models to test visualization
  fitter$add_model("Hill")
  fitter$add_model("Logistic")
  fitter$add_model("ExponentialDecay")

  # Generate the plot
  plot <- fitter$model_comparison_plot(x_range = seq(1, 100, by = 1), normalize = TRUE, theme = "macarons")

  # Check that the result is an echarts4r plot
  expect_true(inherits(plot, "echarts4r"))
})
