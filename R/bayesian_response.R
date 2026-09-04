bnls_or <- function(x, default) if (is.null(x) || !length(x)) default else x

bnls_hash <- function(x) {
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  saveRDS(x, path, version = 3)
  unname(tools::md5sum(path))
}

#' Define an explicit AutoNLS Bayesian prior
#'
#' @param distribution One of `normal`, `lognormal`, `uniform`, or `half_normal`.
#' @param location Location parameter (mean for normal, meanlog for lognormal).
#' @param scale Positive scale parameter (sd, sdlog, or half-normal scale).
#' @param lower,upper Bounds for a uniform prior.
#' @return A validated prior specification.
#' @examples
#' autonls_bayesian_prior("normal", location = 0, scale = 2)
#' autonls_bayesian_prior("uniform", lower = -1, upper = 1)
#' @export
autonls_bayesian_prior <- function(distribution = c("normal", "lognormal",
    "uniform", "half_normal"), location = 0, scale = 1,
    lower = -Inf, upper = Inf) {
  distribution <- match.arg(distribution)
  if (!is.numeric(location) || length(location) != 1L || !is.finite(location))
    stop("prior location must be one finite number.")
  if (!is.numeric(scale) || length(scale) != 1L || !is.finite(scale) || scale <= 0)
    stop("prior scale must be one positive finite number.")
  if (!is.numeric(lower) || !is.numeric(upper) || length(lower) != 1L ||
      length(upper) != 1L || is.na(lower) || is.na(upper) || lower >= upper)
    stop("prior bounds must be scalar and satisfy lower < upper.")
  if (identical(distribution, "uniform") &&
      (!is.finite(lower) || !is.finite(upper)))
    stop("uniform prior requires lower < upper.")
  structure(list(distribution = distribution, location = as.numeric(location),
    scale = as.numeric(scale), lower = as.numeric(lower), upper = as.numeric(upper)),
    class = "autonls_bayesian_prior")
}

bnls_log_prior <- function(value, prior) {
  if (value < prior$lower || value > prior$upper) return(-Inf)
  switch(prior$distribution,
    normal = stats::dnorm(value, prior$location, prior$scale, log = TRUE),
    lognormal = if (value <= 0) -Inf else
      stats::dlnorm(value, prior$location, prior$scale, log = TRUE),
    uniform = stats::dunif(value, prior$lower, prior$upper, log = TRUE),
    half_normal = if (value < 0) -Inf else
      log(2) + stats::dnorm(value, 0, prior$scale, log = TRUE))
}

bnls_default_priors <- function(spec, x, y, controls) {
  y_scale <- max(stats::sd(y), diff(range(y)), 1e-3, na.rm = TRUE)
  x_scale <- max(stats::sd(x), diff(range(x)), 1e-3, na.rm = TRUE)
  out <- lapply(spec$parameter_names, function(nm) {
    lo <- spec$lower_bounds[[nm]]
    hi <- spec$upper_bounds[[nm]]
    start <- spec$start_params[[nm]]
    if (is.finite(lo) && is.finite(hi))
      autonls_bayesian_prior("uniform", lower = lo, upper = hi)
    else if (is.finite(lo) && lo >= 0)
      autonls_bayesian_prior("half_normal", scale = max(abs(start) * 3, y_scale * 3, 1), lower = lo)
    else {
      scale <- if (nm %in% c("c", "d", "midpoint", "xmid"))
        x_scale * 3 else if (nm %in% c("b", "slope", "steepness"))
        max(10 / x_scale, 2) else y_scale * 3
      location <- if (nm %in% c("c", "midpoint", "xmid")) mean(range(x)) else start
      autonls_bayesian_prior("normal", location = location,
        scale = scale, lower = lo, upper = hi)
    }
  })
  names(out) <- spec$parameter_names
  for (nm in controls)
    out[[paste0("control:", nm)]] <- autonls_bayesian_prior("normal", 0, y_scale * 2)
  out$sigma <- autonls_bayesian_prior("half_normal", scale = y_scale * 2,
    lower = .Machine$double.eps)
  out
}

bnls_validate_priors <- function(priors, required, bounds) {
  if (!is.list(priors) || !all(required %in% names(priors)))
    stop("priors must be a named list covering: ", paste(required, collapse = ", "))
  unknown <- setdiff(names(priors), required)
  if (length(unknown)) stop("unknown prior name(s): ", paste(unknown, collapse = ", "))
  for (nm in required) {
    p <- priors[[nm]]
    if (!inherits(p, "autonls_bayesian_prior"))
      stop("prior '", nm, "' must be created by autonls_bayesian_prior().")
    if (!is.null(bounds[[nm]])) {
      b <- bounds[[nm]]
      if (p$upper < b[[1L]] || p$lower > b[[2L]])
        stop("prior support for '", nm, "' is incompatible with equation bounds.")
    }
  }
  invisible(TRUE)
}

bnls_rhat <- function(chains) {
  m <- length(chains)
  if (m < 2L) return(rep(NA_real_, ncol(chains[[1L]])))
  n <- min(vapply(chains, nrow, integer(1)))
  means <- do.call(rbind, lapply(chains, function(x) colMeans(utils::tail(x, n))))
  vars <- do.call(rbind, lapply(chains, function(x) apply(utils::tail(x, n), 2, stats::var)))
  W <- colMeans(vars)
  B <- n * apply(means, 2, stats::var)
  sqrt(pmax(((n - 1) / n * W + B / n) / W, 0))
}

bnls_ess <- function(x, max_lag = 100L) {
  n <- length(x)
  if (n < 4L || !is.finite(stats::var(x)) || stats::var(x) == 0) return(NA_real_)
  lag_max <- min(max_lag, n - 2L)
  ac <- stats::acf(x, plot = FALSE, lag.max = lag_max)$acf[-1L]
  pos <- ac[is.finite(ac) & ac > 0]
  n / max(1, 1 + 2 * sum(pos))
}

#' Fit a Bayesian nonlinear response curve with optional linear controls
#'
#' Fits `y = f(x; theta) + Z beta + error` using one existing AutoNLS equation
#' family and zero or more jointly estimated linear controls. Bayesian fitting is
#' explicit and does not alter [AutoNLS()].
#'
#' @param data A `data.table` containing observed rows only. Objects coercible to
#'   `data.table` are accepted for compatibility.
#' @param x,y Focal nonlinear predictor and outcome column names.
#' @param family One qualified equation name from [list_nls_models()].
#' @param controls Optional character vector of linear control columns.
#' @param model_domain Optional numeric length-two natural/domain support for `x`.
#' @param priors Optional named prior overrides. Names are equation parameters,
#'   `control:<column>`, and `sigma`. Unspecified priors are weak, generated,
#'   recorded, and returned on the fit.
#' @param chains Number of independent random-walk Metropolis chains.
#' @param iter,burnin,thin MCMC iterations, discarded warmup, and thinning.
#' @param seed Random seed.
#' @param proposal_scale Positive proposal multiplier.
#' @param control_standardize Center and scale controls internally for sampling.
#' @return An `AutoNLSBayesFit` R6 object retaining draws and diagnostics.
#' @examples
#' set.seed(4)
#' d <- data.table::data.table(
#'   sentiment = runif(80, -.4, 0), impressions = runif(80, 80, 140))
#' d$visits <- 20 + 35 / (1 + exp(-10 * (d$sentiment + .15))) +
#'   .08 * (d$impressions - 110) + rnorm(80, 0, 2)
#' fit <- AutoNLSBayes(d, "sentiment", "visits", family = "Logistic",
#'   controls = "impressions", model_domain = c(-1, 1),
#'   chains = 2, iter = 400, burnin = 200)
#' fit$prior_table()
#' fit$predict(data.table::data.table(
#'   sentiment = c(-.2, .3), impressions = 110))
#' @export
AutoNLSBayes <- function(data, x, y, family = "Logistic", controls = NULL,
    model_domain = NULL, priors = NULL, chains = 4L, iter = 2000L,
    burnin = 1000L, thin = 2L, seed = 42L, proposal_scale = 1,
    control_standardize = TRUE) {
  fit <- AutoNLSBayesFit$new(data, x, y, family, controls, model_domain,
    priors, chains, iter, burnin, thin, seed, proposal_scale,
    control_standardize)
  fit$fit()
  fit
}

#' AutoNLS Bayesian fitted response object
#' @export
AutoNLSBayesFit <- R6::R6Class("AutoNLSBayesFit", public = list(
  data = NULL, x = NULL, y = NULL, family = NULL, controls = NULL,
  observed_support = NULL, model_domain = NULL, priors = NULL,
  prior_source = NULL, prior_overrides = NULL,
  posterior_draws = NULL, convergence = NULL,
  posterior_predictive_draws = NULL,
  control_center = NULL, control_scale = NULL, registry_spec = NULL,
  fitted_state_id = NULL, provenance = NULL, refit = FALSE,
  posterior_predictive_available = TRUE,

  initialize = function(data, x, y, family, controls, model_domain, priors,
      chains, iter, burnin, thin, seed, proposal_scale, control_standardize) {
    DT <- data.table::as.data.table(data)
    controls <- unique(as.character(bnls_or(controls, character())))
    controls <- controls[nzchar(controls)]
    required_cols <- c(x, y, controls)
    if (length(x) != 1L || length(y) != 1L || !all(required_cols %in% names(DT)))
      stop("data must contain one focal x, one y, and every control column.")
    if (x %in% controls || y %in% controls) stop("controls cannot contain x or y.")
    if (!all(vapply(DT[, ..required_cols], is.numeric, logical(1))))
      stop("x, y, and controls must be numeric.")
    if (any(!is.finite(as.matrix(DT[, ..required_cols]))))
      stop("x, y, and controls must contain only finite observed values.")
    registry <- nls_model_registry()
    if (!family %in% names(registry)) stop("unsupported equation family: ", family)
    if (nrow(DT) < 8L || data.table::uniqueN(DT[[x]]) < 4L)
      stop("Bayesian fitting requires at least 8 rows and 4 unique focal values.")
    observed <- range(DT[[x]])
    if (is.null(model_domain)) model_domain <- observed
    if (!is.numeric(model_domain) || length(model_domain) != 2L ||
        any(!is.finite(model_domain)) || model_domain[[1L]] >= model_domain[[2L]] ||
        model_domain[[1L]] > observed[[1L]] || model_domain[[2L]] < observed[[2L]])
      stop("model_domain must be an ordered finite range containing observed support.")
    chains <- as.integer(chains); iter <- as.integer(iter); burnin <- as.integer(burnin)
    thin <- as.integer(thin)
    if (chains < 1L || iter <= burnin || burnin < 0L || thin < 1L)
      stop("require chains >= 1, iter > burnin >= 0, and thin >= 1.")
    self$data <- data.table::copy(DT); self$x <- x; self$y <- y
    self$family <- family; self$controls <- controls
    self$observed_support <- observed; self$model_domain <- model_domain
    self$registry_spec <- registry[[family]]
    private$chains <- chains; private$iter <- iter; private$burnin <- burnin
    private$thin <- thin; private$seed <- seed; private$proposal_scale <- proposal_scale
    private$control_standardize <- isTRUE(control_standardize)
    z <- if (length(controls)) as.matrix(DT[, ..controls]) else matrix(numeric(), nrow(DT), 0L)
    self$control_center <- if (ncol(z)) as.numeric(colMeans(z)) else numeric()
    self$control_scale <- if (ncol(z)) as.numeric(apply(z, 2, stats::sd)) else numeric()
    self$control_scale[!is.finite(self$control_scale) | self$control_scale == 0] <- 1
    if (!private$control_standardize) {
      self$control_center[] <- 0; self$control_scale[] <- 1
    }
    defaults <- bnls_default_priors(self$registry_spec, DT[[x]], DT[[y]], controls)
    self$prior_source <- if (!length(priors)) "GENERATED_WEAK" else "CALLER_OVERRIDDEN"
    self$prior_overrides <- if (!length(priors)) character() else names(priors)
    if (!is.null(priors) && (!is.list(priors) || is.null(names(priors))))
      stop("priors must be a named list of prior overrides.")
    self$priors <- defaults
    if (length(priors)) self$priors[names(priors)] <- priors
    control_parameters <- if (length(controls)) paste0("control:", controls) else character()
    required <- c(self$registry_spec$parameter_names, control_parameters, "sigma")
    bounds <- lapply(required, function(nm) c(-Inf, Inf)); names(bounds) <- required
    for (nm in self$registry_spec$parameter_names)
      bounds[[nm]] <- c(self$registry_spec$lower_bounds[[nm]], self$registry_spec$upper_bounds[[nm]])
    bounds$sigma <- c(.Machine$double.eps, Inf)
    bnls_validate_priors(self$priors, required, bounds)
    invisible(self)
  },

  fit = function() {
    x <- self$data[[self$x]]; y <- self$data[[self$y]]
    z <- private$control_matrix(self$data)
    control_parameters <- if (length(self$controls))
      paste0("control:", self$controls) else character()
    par_names <- c(self$registry_spec$parameter_names, control_parameters, "sigma")
    start <- c(self$registry_spec$start_params,
      stats::setNames(rep(0, length(control_parameters)), control_parameters),
      sigma = max(stats::sd(y), 1e-3))
    objective <- function(p) -private$log_posterior(p, x, y, z)
    map <- tryCatch(stats::optim(start, objective, method = "Nelder-Mead",
      control = list(maxit = 3000)), error = function(e) NULL)
    center <- if (!is.null(map) && is.finite(map$value)) map$par else start
    names(center) <- par_names
    base_step <- pmax(abs(center) * .03, .02) * private$proposal_scale
    chain_results <- vector("list", private$chains)
    acceptance <- matrix(0, private$chains, length(center))
    set.seed(private$seed)
    for (ch in seq_len(private$chains)) {
      current <- center + stats::rnorm(length(center), 0, base_step * .2)
      names(current) <- par_names
      lp <- private$log_posterior(current, x, y, z)
      if (!is.finite(lp)) { current <- center; lp <- private$log_posterior(current, x, y, z) }
      draws <- matrix(NA_real_, private$iter, length(center), dimnames = list(NULL, par_names))
      accepted <- integer(length(center)); step <- base_step
      for (i in seq_len(private$iter)) {
        for (j in seq_along(current)) {
          proposal <- current
          proposal[[j]] <- proposal[[j]] + stats::rnorm(1, 0, step[[j]])
          lp_new <- private$log_posterior(proposal, x, y, z)
          if (is.finite(lp_new) && log(stats::runif(1)) < lp_new - lp) {
            current <- proposal; lp <- lp_new; accepted[[j]] <- accepted[[j]] + 1L
          }
        }
        draws[i, ] <- current
        if (i <= private$burnin && i %% 50L == 0L) {
          rate <- accepted / i
          step <- step * ifelse(rate < .2, .75, ifelse(rate > .55, 1.3, 1))
        }
      }
      keep <- seq.int(private$burnin + 1L, private$iter, by = private$thin)
      chain_results[[ch]] <- draws[keep, , drop = FALSE]
      acceptance[ch, ] <- accepted / private$iter
    }
    rhat <- bnls_rhat(chain_results)
    combined <- data.table::rbindlist(lapply(seq_along(chain_results), function(ch)
      data.table::as.data.table(chain_results[[ch]])[, chain := ch]), fill = TRUE)
    data.table::setcolorder(combined, c("chain", par_names))
    self$posterior_draws <- combined
    ess <- vapply(par_names, function(nm) bnls_ess(combined[[nm]]), numeric(1))
    self$convergence <- data.table::data.table(parameter = par_names,
      rhat = as.numeric(rhat), ess = as.numeric(ess),
      acceptance_rate = colMeans(acceptance),
      diagnostic_class = "SAMPLER_COMPUTATIONAL",
      status = ifelse(is.na(rhat) | (rhat <= 1.1 & ess >= 50), "PASS", "POOR_CONVERGENCE"))
    self$fitted_state_id <- paste0("autonls-bayes-", bnls_hash(list(
      family = self$family, x = x, y = y, controls = z,
      priors = self$priors, draws = combined)))
    self$provenance <- list(package = "AutoNLS", version = "1.1.0",
      method = "native_random_walk_metropolis", observations = nrow(self$data),
      likelihood_rows = seq_len(nrow(self$data)), seed = private$seed,
      sampler = list(chains = private$chains, iter = private$iter,
        burnin = private$burnin, thin = private$thin,
        proposal_scale = private$proposal_scale),
      fitted_state_id = self$fitted_state_id)
    set.seed(private$seed + 100000L)
    self$posterior_predictive_draws <- self$predict(self$data, type = "draws",
      include_residual = TRUE)
    self$refit <- FALSE
    invisible(self)
  },

  predict = function(new_data, type = c("summary", "draws"),
      include_residual = FALSE, probs = c(.025, .5, .975), control_draws = NULL) {
    type <- match.arg(type)
    private$require_fitted()
    ND <- data.table::as.data.table(new_data)
    if (!all(c(self$x, self$controls) %in% names(ND)))
      stop("new_data must contain focal predictor and every fitted control.")
    prediction_columns <- c(self$x, self$controls)
    if (any(!is.finite(as.matrix(ND[, prediction_columns, with = FALSE]))))
      stop("prediction inputs must be finite.")
    draws <- self$posterior_draws
    xval <- ND[[self$x]]
    private$validate_prediction_domain(xval)
    z <- private$control_matrix(ND)
    if (!is.null(control_draws)) {
      if (nrow(ND) != 1L || !is.list(control_draws) ||
          !identical(sort(names(control_draws)), sort(self$controls)))
        stop("control_draws must be a named list for every control and one prediction row.")
      lens <- vapply(control_draws, length, integer(1))
      if (any(lens != nrow(draws))) stop("each control draw vector must match posterior draw count.")
    }
    out <- matrix(NA_real_, nrow(draws), nrow(ND))
    theta_names <- self$registry_spec$parameter_names
    for (m in seq_len(nrow(draws))) {
      theta <- as.numeric(draws[m, ..theta_names]); names(theta) <- theta_names
      mu <- self$registry_spec$model_function(xval, theta)
      if (length(self$controls)) {
        zm <- z
        if (!is.null(control_draws))
          zm[1, ] <- (vapply(control_draws, `[[`, numeric(1), m) -
            self$control_center) / self$control_scale
        beta_columns <- paste0("control:", self$controls)
        beta <- as.numeric(draws[m, beta_columns, with = FALSE])
        mu <- mu + as.numeric(zm %*% beta)
      }
      if (isTRUE(include_residual)) mu <- mu + stats::rnorm(length(mu), 0, draws$sigma[[m]])
      out[m, ] <- mu
    }
    if (identical(type, "draws")) {
      ans <- data.table::as.data.table(out)
      data.table::setnames(ans, paste0("row_", seq_len(nrow(ND))))
      ans[, draw := seq_len(.N)]
      data.table::setcolorder(ans, "draw")
      return(ans[])
    }
    qs <- apply(out, 2, stats::quantile, probs = probs, names = FALSE)
    if (is.vector(qs)) qs <- matrix(qs, nrow = length(probs))
    status <- private$support_status(xval)
    data.table::data.table(row_id = seq_len(nrow(ND)), x = xval,
      prediction_mean = colMeans(out), prediction_sd = apply(out, 2, stats::sd),
      lower = qs[1L, ], median = qs[ceiling(length(probs) / 2), ], upper = qs[nrow(qs), ],
      support_status = status$status, distance_outside_support = status$distance,
      fitted_state_id = self$fitted_state_id, refit = FALSE)
  },

  posterior_curve = function(grid, controls, probs = c(.025, .5, .975)) {
    ND <- data.table::data.table(value = as.numeric(grid)); data.table::setnames(ND, self$x)
    if (length(self$controls)) {
      if (missing(controls) || is.null(controls)) stop("controls must be supplied for posterior curves.")
      CD <- data.table::as.data.table(controls)
      if (nrow(CD) == 1L) CD <- CD[rep(1L, length(grid))]
      if (nrow(CD) != length(grid) || !all(self$controls %in% names(CD)))
        stop("controls must provide one row or one row per grid value.")
      ND <- cbind(ND, CD[, self$controls, with = FALSE])
    }
    self$predict(ND, probs = probs)
  },

  posterior_function = function(grid, type = c("summary", "draws"),
      probs = c(.025, .5, .975)) {
    type <- match.arg(type)
    private$require_fitted()
    xval <- as.numeric(grid)
    if (any(!is.finite(xval))) stop("grid must contain only finite values.")
    private$validate_prediction_domain(xval)
    draws <- self$posterior_draws
    theta_names <- self$registry_spec$parameter_names
    out <- matrix(NA_real_, nrow(draws), length(xval))
    for (m in seq_len(nrow(draws))) {
      theta <- as.numeric(draws[m, ..theta_names]); names(theta) <- theta_names
      out[m, ] <- self$registry_spec$model_function(xval, theta)
    }
    if (identical(type, "draws")) {
      ans <- data.table::as.data.table(out)
      data.table::setnames(ans, paste0("grid_", seq_along(xval)))
      ans[, draw := seq_len(.N)]
      data.table::setcolorder(ans, "draw")
      return(ans[])
    }
    qs <- apply(out, 2, stats::quantile, probs = probs, names = FALSE)
    if (is.vector(qs)) qs <- matrix(qs, nrow = length(probs))
    status <- private$support_status(xval)
    data.table::data.table(x = xval, function_mean = colMeans(out),
      function_sd = apply(out, 2, stats::sd), lower = qs[1L, ],
      median = qs[ceiling(length(probs) / 2), ], upper = qs[nrow(qs), ],
      support_status = status$status,
      distance_outside_support = status$distance,
      fitted_state_id = self$fitted_state_id, refit = FALSE)
  },

  control_coefficients = function(probs = c(.025, .5, .975)) {
    private$require_fitted()
    if (!length(self$controls)) return(data.table::data.table())
    out <- lapply(seq_along(self$controls), function(j) {
      nm <- self$controls[[j]]
      value <- self$posterior_draws[[paste0("control:", nm)]] / self$control_scale[[j]]
      q <- stats::quantile(value, probs = probs, names = FALSE)
      data.table::data.table(control = nm, coefficient_mean = mean(value),
        coefficient_sd = stats::sd(value), lower = q[[1L]],
        median = q[[ceiling(length(q) / 2)]], upper = q[[length(q)]],
        scale = "ORIGINAL_CONTROL_UNITS")
    })
    data.table::rbindlist(out)
  },

  prior_table = function() {
    names <- names(self$priors)
    data.table::rbindlist(lapply(names, function(nm) {
      p <- self$priors[[nm]]
      data.table::data.table(parameter = nm, distribution = p$distribution,
        location = p$location, scale = p$scale, lower = p$lower,
        upper = p$upper,
        source = if (nm %in% self$prior_overrides)
          "CALLER_SPECIFIED" else "GENERATED_WEAK")
    }))
  },

  derivative = function(new_data, probs = c(.025, .5, .975)) {
    private$effect_summary(new_data, "derivative", probs)
  },

  elasticity = function(new_data, probs = c(.025, .5, .975)) {
    private$effect_summary(new_data, "elasticity", probs)
  },

  incremental_response = function(x0, x1, controls) {
    if (length(self$controls) && (missing(controls) || is.null(controls)))
      stop("controls must be supplied for incremental response.")
    base <- data.table::data.table(value = c(x0, x1)); data.table::setnames(base, self$x)
    if (length(self$controls)) {
      cv <- data.table::as.data.table(as.list(controls))
      base <- cbind(base, cv[rep(1L, 2L), self$controls, with = FALSE])
    }
    d <- self$predict(base, type = "draws")
    delta <- d$row_2 - d$row_1
    data.table::data.table(x0 = x0, x1 = x1, mean = mean(delta), sd = stats::sd(delta),
      lower = stats::quantile(delta, .025), median = stats::median(delta),
      upper = stats::quantile(delta, .975), fitted_state_id = self$fitted_state_id,
      refit = FALSE)
  },

  diagnostics = function() data.table::copy(self$convergence),
  draws = function() data.table::copy(self$posterior_draws),
  artifact = function() autonls_bayesian_agent_artifact(self)

), private = list(
    chains = NULL, iter = NULL, burnin = NULL, thin = NULL, seed = NULL,
    proposal_scale = NULL, control_standardize = NULL,
    control_matrix = function(DT) {
      if (!length(self$controls)) return(matrix(numeric(), nrow(DT), 0L))
      z <- as.matrix(DT[, self$controls, with = FALSE])
      sweep(sweep(z, 2, self$control_center, "-"), 2, self$control_scale, "/")
    },
    log_posterior = function(p, x, y, z) {
      control_parameters <- if (length(self$controls))
        paste0("control:", self$controls) else character()
      names(p) <- c(self$registry_spec$parameter_names, control_parameters, "sigma")
      lp <- sum(vapply(names(p), function(nm) bnls_log_prior(p[[nm]], self$priors[[nm]]), numeric(1)))
      if (!is.finite(lp) || p[["sigma"]] <= 0) return(-Inf)
      theta <- p[self$registry_spec$parameter_names]
      mu <- self$registry_spec$model_function(x, theta)
      if (length(self$controls)) mu <- mu + as.numeric(z %*% p[paste0("control:", self$controls)])
      if (any(!is.finite(mu))) return(-Inf)
      lp + sum(stats::dnorm(y, mu, p[["sigma"]], log = TRUE))
    },
    require_fitted = function() if (is.null(self$posterior_draws)) stop("fit has no posterior draws."),
    support_status = function(x) {
      lo <- self$observed_support[[1L]]; hi <- self$observed_support[[2L]]
      tol <- max(diff(self$observed_support) * .01, sqrt(.Machine$double.eps))
      status <- ifelse(x < lo - tol | x > hi + tol, "EXTRAPOLATION",
        ifelse(abs(x - lo) <= tol | abs(x - hi) <= tol, "BOUNDARY", "INTERPOLATION"))
      distance <- pmax(lo - x, 0) + pmax(x - hi, 0)
      list(status = status, distance = distance)
    },
    validate_prediction_domain = function(x) {
      if (any(x < self$model_domain[[1L]] | x > self$model_domain[[2L]]))
        stop("prediction focal values must lie inside the fitted model_domain.")
      invisible(TRUE)
    },
    effect_summary = function(new_data, what, probs) {
      private$require_fitted(); ND <- data.table::as.data.table(new_data)
      if (!all(c(self$x, self$controls) %in% names(ND)))
        stop("new_data must contain focal predictor and every fitted control.")
      xval <- ND[[self$x]]; draws <- self$posterior_draws
      private$validate_prediction_domain(xval)
      mat <- matrix(NA_real_, nrow(draws), nrow(ND)); theta_names <- self$registry_spec$parameter_names
      pred <- if (identical(what, "elasticity")) self$predict(ND, type = "draws") else NULL
      for (m in seq_len(nrow(draws))) {
        theta <- as.numeric(draws[m, ..theta_names]); names(theta) <- theta_names
        val <- self$registry_spec$derivative_function(xval, theta)
        if (identical(what, "elasticity")) {
          denom <- as.numeric(pred[m, -1L, with = FALSE])
          val <- val * xval / denom
          val[!is.finite(val) | abs(denom) < 1e-8] <- NA_real_
        }
        mat[m, ] <- val
      }
      qs <- apply(mat, 2, stats::quantile, probs = probs, na.rm = TRUE, names = FALSE)
      if (is.vector(qs)) qs <- matrix(qs, nrow = length(probs))
      s <- private$support_status(xval)
      data.table::data.table(row_id = seq_len(nrow(ND)), x = xval,
        effect = what, mean = colMeans(mat, na.rm = TRUE), sd = apply(mat, 2, stats::sd, na.rm = TRUE),
        lower = qs[1L, ], median = qs[ceiling(length(probs) / 2), ], upper = qs[nrow(qs), ],
        support_status = s$status, distance_outside_support = s$distance,
        fitted_state_id = self$fitted_state_id, refit = FALSE)
    }
  )
)

#' Create a neutral downstream artifact from an AutoNLS Bayesian fit
#'
#' @param fit An `AutoNLSBayesFit` object.
#' @return A list suitable for bounded downstream inspection and RI consumption.
#' @export
autonls_bayesian_agent_artifact <- function(fit) {
  if (!inherits(fit, "AutoNLSBayesFit")) stop("fit must be an AutoNLSBayesFit.")
  conv <- fit$diagnostics()
  list(schema_version = "autonls_bayesian_response_artifact_v1",
    owner = "AutoNLS", family = fit$family, focal_variable = fit$x,
    controls = fit$controls, observed_support = fit$observed_support,
    model_domain = fit$model_domain, priors = fit$priors,
    prior_source = fit$prior_source, posterior_draws_available = TRUE,
    prior_table = fit$prior_table(),
    posterior_function_draws_available = TRUE,
    posterior_predictive_draws_available = TRUE,
    control_parameterization = list(internal = "CENTERED_AND_SCALED",
      reported = "ORIGINAL_CONTROL_UNITS",
      center = fit$control_center, scale = fit$control_scale),
    convergence = conv, convergence_status = if (all(conv$status == "PASS")) "PASS" else "POOR_CONVERGENCE",
    identification = list(status = "NOT_ASSESSED_BY_SAMPLER_DIAGNOSTICS",
      prior_sensitivity = "CALLER_MUST_COMPARE_SCIENTIFICALLY_REASONABLE_PRIORS",
      note = "Poor mixing and weak identification are distinct concepts."),
    extrapolation_status = "POINT_SPECIFIC_AT_PREDICTION",
    fitted_state_id = fit$fitted_state_id, refit = FALSE,
    claim_class = "PREDICTIVE_RESPONSE",
    limitations = c("Posterior width does not eliminate extrapolation risk.",
      "Controls are linear and are not forecast by AutoNLS.",
      "Out-of-support geometry may be prior sensitive."),
    provenance = fit$provenance)
}
