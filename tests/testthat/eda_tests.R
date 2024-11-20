library(data.table)
library(echarts4r)
library(testthat)

sample_data <- data.table::data.table(
  A = stats::rnorm(100),
  B = stats::rnorm(100, 10, 5),
  C = sample(letters[1:5], 100, replace = TRUE),
  D = stats::rpois(100, 2)
)

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

test_that("correlate computes the correlation matrix correctly", {
  correlation_matrix <- eda$correlate()
  numeric_cols <- names(sample_data)[sapply(sample_data, is.numeric)]
  if(length(numeric_cols) > 1) {
    expect_true(is.matrix(correlation_matrix))
    expect_equal(dim(correlation_matrix), c(length(numeric_cols), length(numeric_cols)))
  } else {
    expect_type(correlation_matrix, "character")
  }
})

test_that("visualize_distributions generates valid plots", {
  eda$visualize_distributions(
    title_prefix = "Test Distribution of",
    bins = 10,
    add_density = TRUE,
    density_color = "#654321"
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
  results <- eda$render_all()
  expect_true(is.list(results))
  expect_equal(names(results), c("Summary", "Correlation", "Plots"))
  expect_true(is.data.table(results$Summary))
  expect_true(is.matrix(results$Correlation) || is.character(results$Correlation))
  expect_true(is.list(results$Plots))
  expect_true(all(sapply(results$Plots, function(p) inherits(p, "echarts4r"))))
})
