testthat::test_that("vNext fitted derivatives agree with score finite differences", {
  set.seed(23)
  x <- seq(0, 100, length.out = 180)
  fixtures <- list(
    Linear = 4 + 1.7 * x,
    Logistic = 70 / (1 + exp(-0.09 * (x - 48)))
  )

  for (family in names(fixtures)) {
    d <- data.table::data.table(x = x, y = fixtures[[family]] + rnorm(length(x), 0, .02))
    fit <- AutoNLS(d, x = "x", y = "y", models = family,
      n_starts = 4, seed = 31, interval_method = "none")
    probe <- data.table::data.table(x = seq(15, 85, length.out = 20))
    analytic <- fit$derivative(probe, model = family)$derivative
    h <- 1e-3
    upper <- fit$predict(data.table::data.table(x = probe$x + h), model = family)$prediction
    lower <- fit$predict(data.table::data.table(x = probe$x - h), model = family)$prediction
    finite_difference <- (upper - lower) / (2 * h)

    testthat::expect_true(all(is.finite(analytic)), info = family)
    testthat::expect_equal(analytic, finite_difference,
      tolerance = 1e-4, info = family)
  }
})
