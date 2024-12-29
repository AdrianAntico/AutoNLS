library(data.table)
library(testthat)

test_that("score_new_data generates valid predictions", {
  # Sample training data
  train_data <- data.table::data.table(
    `x-variable` = seq(1, 100, by = 1),
    y = 10 * seq(1, 100, by = 1)^1.2 / (50 + seq(1, 100, by = 1)^1.2) + rnorm(100, mean = 0, sd = 0.5)
  )

  # Sample new data
  new_data <- data.table::data.table(`x-variable` = seq(101, 200, by = 1))

  # Initialize ModelFitter
  fitter <- ModelFitter$new(train_data)
  fitter$add_model("Hill")

  # Fit models
  fit_results <- fitter$fit_models(x_col = "x-variable", y_col = "y")

  # Initialize ModelScorer
  scorer <- ModelScorer$new(fit_results)

  # Test 1: Scoring generates a list of predictions
  scored_data <- scorer$score_new_data(new_data, x_col = "x-variable")
  expect_true(is.list(scored_data))
  expect_equal(length(scored_data), length(fit_results))
  expect_true(all(sapply(scored_data, function(s) inherits(s, "data.table"))))

  # Test 2: Predictions contain the expected columns
  expect_true("Hill" %in% names(scored_data))
  hill_predictions <- scored_data[["Hill"]]
})

test_that("generate_score_plot generates valid plot", {
  # Sample training data
  train_data <- data.table::data.table(
    x = seq(1, 100, by = 1),
    y = 10 * seq(1, 100, by = 1)^1.2 / (50 + seq(1, 100, by = 1)^1.2) + rnorm(100, mean = 0, sd = 0.5)
  )

  # Sample new data
  new_data <- data.table::data.table(x = seq(101, 200, by = 1))

  # Initialize ModelFitter
  fitter <- ModelFitter$new(train_data)
  fitter$add_model("Hill")

  # Fit models
  fit_results <- fitter$fit_models(x_col = "x", y_col = "y")

  # Initialize ModelScorer
  scorer <- ModelScorer$new(fit_results)

  # Score new data
  scored_data <- scorer$score_new_data(new_data, x_col = "x")

  # Test 3: Plot generation produces a valid echarts4r object
  plot <- scorer$generate_score_plot("Hill", new_data, x_col = "x")
  expect_true(inherits(plot, "echarts4r"))
})

test_that("score_new_data handles invalid inputs", {
  # Sample training data
  train_data <- data.table::data.table(
    x = seq(1, 100, by = 1),
    y = 10 * seq(1, 100, by = 1)^1.2 / (50 + seq(1, 100, by = 1)^1.2) + rnorm(100, mean = 0, sd = 0.5)
  )

  # Sample new data
  new_data <- data.table::data.table(x = seq(101, 200, by = 1))

  # Initialize ModelFitter
  fitter <- ModelFitter$new(train_data)
  fitter$add_model("Hill")

  # Fit models
  fit_results <- fitter$fit_models(x_col = "x", y_col = "y")

  # Initialize ModelScorer
  scorer <- ModelScorer$new(fit_results)

  # Test 4: Invalid new_data
  expect_error(scorer$score_new_data(NULL, x_col = "x"), "new_data must be a data.table.")

  # Test 5: Missing x_col
  invalid_data <- data.table::data.table(z = seq(101, 200, by = 1))
  expect_error(scorer$score_new_data(invalid_data, x_col = "x"), "x_col must exist in the dataset.")
})

test_that("generate_score_plot handles invalid inputs", {
  # Sample training data
  train_data <- data.table::data.table(
    x = seq(1, 100, by = 1),
    y = 10 * seq(1, 100, by = 1)^1.2 / (50 + seq(1, 100, by = 1)^1.2) + rnorm(100, mean = 0, sd = 0.5)
  )

  # Sample new data
  new_data <- data.table::data.table(x = seq(101, 200, by = 1))

  # Initialize ModelFitter
  fitter <- ModelFitter$new(train_data)
  fitter$add_model("Hill")

  # Fit models
  fit_results <- fitter$fit_models(x_col = "x", y_col = "y")

  # Initialize ModelScorer
  scorer <- ModelScorer$new(fit_results)

  # Test 6: Plot for non-scored model
  expect_error(scorer$generate_score_plot("NonExistentModel", new_data, x_col = "x"),
               "Model 'NonExistentModel' not found in fit_results")

  # Test 7: Missing x_col
  expect_error(scorer$generate_score_plot("Hill", new_data, x_col = "z"),
               "x_col must exist in the dataset.")
})

