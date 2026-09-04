bayes_fixture <- function(n = 80L, seed = 19L) {
  set.seed(seed)
  sentiment <- runif(n, -.4, 0)
  impressions <- runif(n, 80, 140)
  response <- 58 / (1 + exp(-11 * (sentiment + .16))) +
    .09 * (impressions - 110) + rnorm(n, 0, 1.4)
  data.table::data.table(sentiment, impressions, engaged_visits = response)
}

fit_bayes_fixture <- function(seed = 42L, priors = NULL, family = "Logistic") {
  AutoNLSBayes(bayes_fixture(), "sentiment", "engaged_visits",
    family = family, controls = "impressions", model_domain = c(-1, 1),
    priors = priors, chains = 2, iter = 500, burnin = 250, thin = 2,
    seed = seed)
}

testthat::test_that("Bayesian response jointly estimates focal geometry and controls", {
  fit <- fit_bayes_fixture()
  testthat::expect_true(inherits(fit, "AutoNLSBayesFit"))
  testthat::expect_equal(fit$observed_support, range(fit$data$sentiment))
  testthat::expect_equal(fit$model_domain, c(-1, 1))
  testthat::expect_true("control:impressions" %in% names(fit$posterior_draws))
  testthat::expect_true(abs(mean(fit$posterior_draws[["control:impressions"]])) > .05)
  testthat::expect_equal(nrow(fit$posterior_predictive_draws), nrow(fit$posterior_draws))
  testthat::expect_equal(fit$provenance$observations, nrow(fit$data))
  testthat::expect_equal(fit$provenance$likelihood_rows, seq_len(nrow(fit$data)))
  testthat::expect_equal(fit$control_coefficients()$scale,
    "ORIGINAL_CONTROL_UNITS")
})

testthat::test_that("support and posterior curve semantics remain honest", {
  fit <- fit_bayes_fixture()
  curve <- fit$posterior_curve(c(-.4, -.2, 0, .3), list(impressions = 110))
  testthat::expect_equal(curve$support_status,
    c("BOUNDARY", "INTERPOLATION", "BOUNDARY", "EXTRAPOLATION"))
  testthat::expect_equal(curve$distance_outside_support[[4L]], .3, tolerance = .02)
  testthat::expect_true(all(curve$upper >= curve$lower))
  testthat::expect_true(all(curve$prediction_sd > 0))
  focal <- fit$posterior_function(c(-.4, 0, .3))
  testthat::expect_equal(focal$support_status,
    c("BOUNDARY", "BOUNDARY", "EXTRAPOLATION"))
  testthat::expect_equal(nrow(fit$posterior_function(c(-.4, 0), "draws")),
    nrow(fit$posterior_draws))
  testthat::expect_equal(nrow(fit$data), 80L)
  before <- data.table::copy(fit$data)
  fit$posterior_function(seq(-1, 1, length.out = 201), "draws")
  testthat::expect_identical(fit$data, before)
})

testthat::test_that("control scenarios and effects replay without refit", {
  fit <- fit_bayes_fixture()
  low <- fit$predict(data.frame(sentiment = -.1, impressions = 90))
  high <- fit$predict(data.frame(sentiment = -.1, impressions = 130))
  testthat::expect_gt(high$prediction_mean, low$prediction_mean)
  d <- fit$derivative(data.frame(sentiment = c(-.2, .2), impressions = 110))
  e <- fit$elasticity(data.frame(sentiment = c(-.2, .2), impressions = 110))
  inc <- fit$incremental_response(-.2, .2, list(impressions = 110))
  testthat::expect_true(all(c("INTERPOLATION", "EXTRAPOLATION") %in% d$support_status))
  testthat::expect_equal(e$refit, rep(FALSE, 2))
  testthat::expect_false(inc$refit)
  path <- tempfile(fileext = ".rds")
  saveRDS(fit, path); replay <- readRDS(path)
  testthat::expect_equal(replay$predict(data.frame(sentiment = .2, impressions = 110)),
    fit$predict(data.frame(sentiment = .2, impressions = 110)))
  testthat::expect_identical(replay$fitted_state_id, fit$fitted_state_id)
})

testthat::test_that("caller control draws are used but never invented", {
  fit <- fit_bayes_fixture()
  nd <- data.frame(sentiment = .1, impressions = 110)
  supplied <- list(impressions = rep(c(90, 130), length.out = nrow(fit$posterior_draws)))
  testthat::expect_equal(nrow(fit$predict(nd, type = "draws", control_draws = supplied)),
    nrow(fit$posterior_draws))
  testthat::expect_error(fit$predict(nd, control_draws = list(impressions = 100)),
    "posterior draw count")
  testthat::expect_error(fit$posterior_curve(c(-.2, .2)), "controls must be supplied")
})

testthat::test_that("prior sensitivity is visible outside limited support", {
  p_low <- list(c = autonls_bayesian_prior("normal", -.15, .04))
  p_high <- list(c = autonls_bayesian_prior("normal", .35, .04))
  low <- fit_bayes_fixture(seed = 51, priors = p_low)
  high <- fit_bayes_fixture(seed = 51, priors = p_high)
  scenario <- data.frame(sentiment = .6, impressions = 110)
  delta <- abs(low$predict(scenario)$prediction_mean - high$predict(scenario)$prediction_mean)
  testthat::expect_gt(delta, 1)
  testthat::expect_equal(low$prior_source, "CALLER_OVERRIDDEN")
  testthat::expect_equal(low$predict(scenario)$support_status, "EXTRAPOLATION")
})

testthat::test_that("families fit independently and artifacts remain neutral", {
  logistic <- fit_bayes_fixture(family = "Logistic")
  gompertz <- fit_bayes_fixture(family = "Gompertz")
  testthat::expect_false(identical(logistic$fitted_state_id, gompertz$fitted_state_id))
  testthat::expect_equal(logistic$artifact()$family, "Logistic")
  testthat::expect_equal(gompertz$artifact()$family, "Gompertz")
  testthat::expect_equal(logistic$artifact()$claim_class, "PREDICTIVE_RESPONSE")
  testthat::expect_equal(logistic$artifact()$fitted_state_id,
    logistic$fitted_state_id)
  testthat::expect_true(all(logistic$diagnostics()$status %in%
    c("PASS", "POOR_CONVERGENCE")))
})

testthat::test_that("Bayesian input and prior hostiles fail closed", {
  d <- bayes_fixture()
  testthat::expect_error(AutoNLSBayes(d, "sentiment", "engaged_visits", family = "Nope"),
    "unsupported equation")
  testthat::expect_error(AutoNLSBayes(d, "sentiment", "engaged_visits",
    controls = "missing"), "must contain")
  testthat::expect_error(autonls_bayesian_prior("normal", scale = -1), "positive")
  testthat::expect_error(AutoNLSBayes(d, "sentiment", "engaged_visits",
    model_domain = c(-.2, .2)), "containing observed support")
  testthat::expect_error(AutoNLSBayes(d, "sentiment", "engaged_visits",
    priors = list(b = autonls_bayesian_prior("uniform", lower = 60, upper = 70))),
    "incompatible")
})

testthat::test_that("frequentist API remains available and unchanged", {
  f <- AutoNLS(data.table::data.table(x = 1:20, y = 2 + 3 * (1:20)),
    x = "x", y = "y", models = "Linear", n_starts = 2,
    interval_method = "none")
  testthat::expect_s3_class(f, "R6")
  testthat::expect_false(inherits(f, "AutoNLSBayesFit"))
})
