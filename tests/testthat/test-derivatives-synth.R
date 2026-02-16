testthat::test_that("Fitted model derivatives (analytic) agree with finite differences on synthetic data", {

  # helper: finite-diff derivative of a scalar->vector function
  fd_pred <- function(pred_fn, x_raw, h_raw) {
    (pred_fn(x_raw + h_raw) - pred_fn(x_raw - h_raw)) / (2 * h_raw)
  }

  # Build fitter just to access the library
  ml <- AutoNLSFitter$new(data.table::data.table(x = 1, y = 1))$model_library

  for (nm in names(ml)) {
    mi <- ml[[nm]]

    testthat::expect_true(is.function(mi$model_function), info = nm)
    testthat::expect_true(is.function(mi$deriv_function), info = nm)
    testthat::expect_true(is.list(mi$start_params), info = nm)

    # ---------------------------
    # Synthetic data generation
    # ---------------------------
    n <- 250L
    x_raw <- sort(stats::runif(n, min = 0, max = 100))

    # what the fitter does internally (scale x to [0,1] anchored at min_x)
    min_x <- min(x_raw)
    max_x <- max(x_raw)
    scale_x <- max_x - min_x
    if (!is.finite(scale_x) || scale_x <= 0) scale_x <- 1
    x_s <- (x_raw - min_x) / scale_x

    params <- mi$start_params

    # generate y on the scaled x-domain, then move to a nicer raw y-scale
    y_s <- mi$model_function(x_s, params)
    testthat::expect_true(all(is.finite(y_s)), info = nm)

    # mild noise so fitting isn't degenerate but stays easy
    noise_sd <- 0.005
    y_s_noisy <- y_s + stats::rnorm(length(y_s), sd = noise_sd)

    # affine transform to "raw y"; fitter will rescale anyway
    y_raw <- 10 + 50 * y_s_noisy

    dt <- data.table::data.table(x = x_raw, y = y_raw)

    # ---------------------------
    # Fit the model
    # ---------------------------
    fitr <- AutoNLSFitter$new(dt)
    fitr$add_model(nm)  # pulls deriv_function from model_library now
    res <- fitr$fit_models(
      x_col = "x",
      y_col = "y",
      loss = "mse",
      method = "BFGS",
      control = list(maxit = 6000, reltol = 1e-10),
      compute_hessian = FALSE
    )

    fr <- res[[nm]]
    testthat::expect_true(isTRUE(fr$ok), info = paste(nm, "optim failed"))

    # ---------------------------
    # Compare analytic derivative vs finite diff on predict()
    # Avoid boundaries where clipping could bite.
    # ---------------------------
    x_probe <- seq(10, 90, length.out = 40)  # safely away from [0,100] edges
    newdata <- data.table::data.table(x = x_probe)

    # analytic dy/dx (original scale)
    d_ana <- fr$derivative(newdata, method = "analytic")[[".dydx"]]
    testthat::expect_true(all(is.finite(d_ana)), info = nm)

    # finite-diff dy/dx using predict() on original scale
    pred_fn <- function(xv) fr$predict(data.table::data.table(x = xv))
    h_raw <- 1e-4 * (max_x - min_x)  # raw units step
    d_fd <- fd_pred(pred_fn, x_probe, h_raw)

    testthat::expect_true(all(is.finite(d_fd)), info = nm)

    # tolerance: a bit forgiving for very stiff / sharp-transition models
    err <- max(abs(d_ana - d_fd), na.rm = TRUE)
    testthat::expect_true(
      err < 5e-2,
      info = paste(nm, "max abs err =", signif(err, 4))
    )

    # optional sanity: auto should match analytic when analytic exists
    d_auto <- fr$derivative(newdata, method = "auto")[[".dydx"]]
    testthat::expect_true(
      max(abs(d_auto - d_ana), na.rm = TRUE) < 1e-10,
      info = paste(nm, "auto != analytic")
    )
  }
})
