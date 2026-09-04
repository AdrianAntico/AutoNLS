testthat::test_that("All model_library derivatives agree with finite differences", {
  # The vNext registry is the single current authority for equation derivatives.
  ml <- AutoNLS:::nls_model_registry()

  fd <- function(f, x, params, h = 1e-6) {
    (f(x + h, params) - f(x - h, params)) / (2 * h)
  }

  for (nm in names(ml)) {
    mi <- ml[[nm]]

    testthat::expect_true(is.function(mi$model_function), info = nm)
    testthat::expect_true(is.function(mi$derivative_function), info = nm)

    x <- seq(0.05, 0.95, length.out = 25)  # scaled-domain probe
    params <- mi$start_params

    y     <- mi$model_function(x, params)
    dy    <- mi$derivative_function(x, params)
    dy_fd <- fd(mi$model_function, x, params)

    # be tolerant of scalar returns (should still behave like vectorized)
    if (length(y) == 1L && length(x) > 1L)     y <- rep(y, length(x))
    if (length(dy) == 1L && length(x) > 1L)   dy <- rep(dy, length(x))
    if (length(dy_fd) == 1L && length(x) > 1L) dy_fd <- rep(dy_fd, length(x))

    testthat::expect_true(length(y)  == length(x), info = nm)
    testthat::expect_true(length(dy) == length(x), info = nm)

    testthat::expect_true(all(is.finite(y)),  info = nm)
    testthat::expect_true(all(is.finite(dy)), info = nm)
    testthat::expect_true(all(is.finite(dy_fd)), info = nm)

    # tolerance: loosen a bit for stiff models / sharp transitions
    err <- max(abs(dy - dy_fd), na.rm = TRUE)

    testthat::expect_true(
      err < 1e-3,
      info = paste(nm, "max abs err =", format(err, scientific = TRUE))
    )
  }
})
