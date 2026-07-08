#' Fit AutoNLS vNext models with one workflow
#'
#' @param data A data.frame or data.table.
#' @param x Predictor column name.
#' @param y Target column name.
#' @param models Character vector of model names, or "all".
#' @param weights_col Optional non-negative weights column.
#' @param loss One of "mse", "mae", "huber", or "quantile".
#' @param n_starts Number of deterministic starts per model.
#' @param seed Random seed used for start generation.
#' @param optimizer One of "optim" or "nlsLM". `nlsLM` is used only when available.
#' @param quantile_level Quantile used when loss = "quantile".
#' @param huber_delta Delta used when loss = "huber".
#' @param lower_bounds Optional named vector/list overriding lower bounds.
#' @param upper_bounds Optional named vector/list overriding upper bounds.
#' @param maxit Maximum optimizer iterations.
#' @param reltol Relative tolerance for `optim`.
#' @param scale_x Logical. Fit models on scaled x.
#' @param scale_y Logical. Fit models on scaled y and back-transform predictions.
#' @param model_status One of "stable", "experimental", or "all".
#' @param validation_fraction Optional holdout fraction between 0 and 0.5.
#' @param validation_seed Seed used for validation split.
#' @param interval_method One of "none", "residual_bootstrap", or "parametric_simulation".
#' @param interval_level Confidence level for prediction intervals.
#' @param interval_n Number of bootstrap/simulation draws when intervals are enabled.
#' @param interval_seed Seed used for interval simulation.
#' @param interval_models One of "best" or "all".
#' @param interval_max_rows Maximum x-grid rows used for interval curves.
#' @param start_strategy Internal start strategy. Defaults to family-aware starts.
#' @param theme Plot theme metadata retained on the fit object.
#' @return An AutoNLSFit R6 object.
#' @export
AutoNLS <- function(
  data,
  x,
  y,
  models = c("Hill", "Logistic", "Gompertz"),
  weights_col = NULL,
  loss = c("mse", "mae", "huber", "quantile"),
  n_starts = 25,
  seed = 42,
  optimizer = c("optim", "nlsLM"),
  quantile_level = 0.5,
  huber_delta = 1,
  lower_bounds = NULL,
  upper_bounds = NULL,
  maxit = 5000,
  reltol = 1e-8,
  scale_x = TRUE,
  scale_y = TRUE,
  model_status = c("stable", "experimental", "all"),
  validation_fraction = 0,
  validation_seed = 42,
  interval_method = c("none", "residual_bootstrap", "parametric_simulation"),
  interval_level = 0.95,
  interval_n = 200,
  interval_seed = 42,
  interval_models = c("best", "all"),
  interval_max_rows = 1000,
  start_strategy = c("family", "generic", "log_transformed", "family_transformed"),
  theme = "dark"
) {
  fit <- AutoNLSFit$new(
    data = data,
    x = x,
    y = y,
    models = models,
    weights_col = weights_col,
    loss = match.arg(loss),
    n_starts = n_starts,
    seed = seed,
    optimizer = match.arg(optimizer),
    quantile_level = quantile_level,
    huber_delta = huber_delta,
    lower_bounds = lower_bounds,
    upper_bounds = upper_bounds,
    maxit = maxit,
    reltol = reltol,
    scale_x = scale_x,
    scale_y = scale_y,
    model_status = match.arg(model_status),
    validation_fraction = validation_fraction,
    validation_seed = validation_seed,
    interval_method = match.arg(interval_method),
    interval_level = interval_level,
    interval_n = interval_n,
    interval_seed = interval_seed,
    interval_models = match.arg(interval_models),
    interval_max_rows = interval_max_rows,
    start_strategy = match.arg(start_strategy),
    theme = theme
  )
  fit$fit()
  fit
}

#' AutoNLS vNext fitted result object
#'
#' @export
AutoNLSFit <- R6::R6Class(
  "AutoNLSFit",
  public = list(
    data = NULL,
    x = NULL,
    y = NULL,
    weights_col = NULL,
    models = NULL,
    loss = NULL,
    n_starts = NULL,
    seed = NULL,
    optimizer = NULL,
    quantile_level = NULL,
    huber_delta = NULL,
    lower_bounds = NULL,
    upper_bounds = NULL,
    maxit = NULL,
    reltol = NULL,
    scale_x = NULL,
    scale_y = NULL,
    model_status = NULL,
    validation_fraction = NULL,
    validation_seed = NULL,
    interval_method = NULL,
    interval_level = NULL,
    interval_n = NULL,
    interval_seed = NULL,
    interval_models = NULL,
    interval_max_rows = NULL,
    start_strategy = NULL,
    theme = NULL,
    registry = NULL,
    scale_params = NULL,
    train_index = NULL,
    validation_index = NULL,
    fit_results = NULL,
    metrics_table = NULL,
    diagnostics = NULL,
    domain_diagnostics = NULL,
    model_suitability = NULL,
    parameter_stability = NULL,
    validation_metrics = NULL,
    ranking_summary = NULL,
    prediction_intervals = NULL,
    interval_diagnostics = NULL,
    fit_warnings = NULL,
    fit_recommendations = NULL,
    artifacts_cache = NULL,

    initialize = function(data, x, y, models, weights_col, loss, n_starts, seed, optimizer, quantile_level, huber_delta, lower_bounds, upper_bounds, maxit, reltol, scale_x, scale_y, model_status, validation_fraction, validation_seed, interval_method, interval_level, interval_n, interval_seed, interval_models, interval_max_rows, start_strategy, theme) {
      DT <- data.table::as.data.table(data)
      if (!x %in% names(DT)) stop("x column not found in data.")
      if (!y %in% names(DT)) stop("y column not found in data.")
      if (!is.null(weights_col) && !weights_col %in% names(DT)) stop("weights_col not found in data.")
      if (!is.numeric(DT[[x]]) || !is.numeric(DT[[y]])) stop("x and y columns must be numeric.")

      self$data <- data.table::copy(DT)
      self$x <- x
      self$y <- y
      self$weights_col <- weights_col
      self$loss <- loss
      self$n_starts <- max(1L, as.integer(n_starts))
      self$seed <- seed
      self$optimizer <- optimizer
      self$quantile_level <- quantile_level
      self$huber_delta <- huber_delta
      self$lower_bounds <- if (is.null(lower_bounds)) NULL else unlist(lower_bounds)
      self$upper_bounds <- if (is.null(upper_bounds)) NULL else unlist(upper_bounds)
      self$maxit <- maxit
      self$reltol <- reltol
      self$scale_x <- isTRUE(scale_x)
      self$scale_y <- isTRUE(scale_y)
      self$model_status <- model_status
      self$validation_fraction <- max(0, min(0.5, as.numeric(validation_fraction)))
      self$validation_seed <- validation_seed
      self$interval_method <- interval_method
      self$interval_level <- max(0.5, min(0.999, as.numeric(interval_level)))
      self$interval_n <- max(1L, as.integer(interval_n))
      self$interval_seed <- interval_seed
      self$interval_models <- interval_models
      self$interval_max_rows <- max(10L, as.integer(interval_max_rows))
      self$start_strategy <- start_strategy
      self$theme <- theme
      self$registry <- nls_model_registry()

      eligible <- names(self$registry)
      if (!identical(model_status, "all")) {
        eligible <- eligible[vapply(self$registry[eligible], function(z) identical(z$status, model_status), logical(1))]
      }
      if (length(models) == 1L && identical(models, "all")) models <- eligible
      unknown <- setdiff(models, names(self$registry))
      if (length(unknown) > 0) stop("Unknown model(s): ", paste(unknown, collapse = ", "))
      blocked <- setdiff(models, eligible)
      if (length(blocked) > 0) stop("Model(s) not allowed by model_status='", model_status, "': ", paste(blocked, collapse = ", "))
      self$models <- models
      invisible(self)
    },

    fit = function() {
      raw_x <- as.numeric(self$data[[self$x]])
      raw_y <- as.numeric(self$data[[self$y]])
      self$scale_params <- private$build_scale_params(raw_x, raw_y)
      x_fit <- private$scale_x_values(raw_x)
      y_fit <- private$scale_y_values(raw_y)
      w_vec <- private$weights()
      private$make_validation_split(length(raw_x))
      set.seed(self$seed)

      self$fit_results <- list()
      diag_rows <- list()
      domain_rows <- list()
      suitability_rows <- list()
      stability_rows <- list()
      x_train <- x_fit[self$train_index]
      y_train <- y_fit[self$train_index]
      w_train <- if (is.null(w_vec)) NULL else w_vec[self$train_index]

      for (model_name in self$models) {
        spec <- private$spec_with_overrides(self$registry[[model_name]])
        domain_rows[[model_name]] <- private$domain_diagnostics_for_model(spec, raw_x, raw_y)
        suitability_rows[[model_name]] <- private$suitability_for_model(spec, raw_x, raw_y, domain_rows[[model_name]])
        skip_msg <- private$blocking_domain_message(domain_rows[[model_name]])
        if (!is.na(skip_msg)) {
          diag_rows[[model_name]] <- data.table::data.table(
            model_name = model_name,
            start_id = NA_integer_,
            status = "skipped",
            objective = Inf,
            convergence_code = NA_integer_,
            message = skip_msg,
            elapsed_time = 0
          )
          self$fit_results[[model_name]] <- list(
            model_name = model_name,
            spec = spec,
            converged = FALSE,
            warning_summary = skip_msg,
            failure_explanation = private$explain_failure(spec, domain_rows[[model_name]], NULL)
          )
          next
        }
        starts <- private$make_starts(spec, x_train, y_train)
        start_results <- lapply(seq_len(nrow(starts)), function(i) {
          private$fit_one_start(spec, x_train, y_train, w_train, starts[i, ], i)
        })
        successful <- Filter(function(z) identical(z$status, "converged") && is.finite(z$objective), start_results)
        stability_rows[[model_name]] <- private$parameter_stability_for_model(spec, start_results)
        diag_rows[[model_name]] <- data.table::rbindlist(lapply(start_results, function(z) {
          data.table::data.table(
            model_name = model_name,
            start_id = z$start_id,
            status = z$status,
            objective = z$objective,
            convergence_code = z$convergence_code,
            message = z$message,
            elapsed_time = z$elapsed_time
          )
        }), fill = TRUE)

        if (length(successful) == 0) {
          self$fit_results[[model_name]] <- list(
            model_name = model_name,
            spec = spec,
            converged = FALSE,
            warning_summary = private$message_summary(start_results),
            failure_explanation = private$explain_failure(spec, domain_rows[[model_name]], start_results)
          )
          next
        }

        best <- successful[[which.min(vapply(successful, `[[`, numeric(1), "objective"))]]
        pred_fit <- spec$model_function(x_fit, best$params)
        pred_train <- spec$model_function(x_train, best$params)
        pred <- private$unscale_y_values(pred_fit)
        self$fit_results[[model_name]] <- list(
          model_name = model_name,
          spec = spec,
          params = best$params,
          objective = best$objective,
          fitted_values = pred,
          fitted_values_scaled = pred_fit,
          train_fitted_values_scaled = pred_train,
          residuals = raw_y - pred,
          residuals_scaled = y_fit - pred_fit,
          train_residuals_scaled = y_train - pred_train,
          convergence_code = best$convergence_code,
          converged = TRUE,
          best_start_id = best$start_id,
          n_starts_attempted = length(start_results),
          n_starts_converged = length(successful),
          warning_summary = private$message_summary(start_results),
          failure_explanation = ""
        )
      }

      self$diagnostics <- data.table::rbindlist(diag_rows, fill = TRUE)
      self$domain_diagnostics <- data.table::rbindlist(domain_rows, fill = TRUE)
      self$model_suitability <- data.table::rbindlist(suitability_rows, fill = TRUE)
      self$parameter_stability <- data.table::rbindlist(stability_rows, fill = TRUE)
      self$metrics_table <- private$build_metrics()
      self$validation_metrics <- private$build_validation_metrics()
      self$ranking_summary <- private$build_ranking_summary()
      interval_result <- private$build_prediction_intervals()
      self$prediction_intervals <- interval_result$prediction_intervals
      self$interval_diagnostics <- interval_result$interval_diagnostics
      guidance <- private$build_fit_guidance()
      self$fit_warnings <- guidance$fit_warnings
      self$fit_recommendations <- guidance$fit_recommendations
      invisible(self)
    },

    summary = function() {
      bm <- self$best_model()
      data.table::data.table(
        x = self$x,
        y = self$y,
        n = nrow(self$data),
        models_requested = length(self$models),
        models_converged = sum(self$metrics_table$converged, na.rm = TRUE),
        best_model = if (is.null(bm)) NA_character_ else bm$model_name,
        loss = self$loss,
        optimizer = self$optimizer,
        scale_x = self$scale_x,
        scale_y = self$scale_y,
        validation_fraction = self$validation_fraction,
        start_strategy = self$start_strategy
      )
    },

    metrics = function() data.table::copy(self$metrics_table),

    fitted_values = function(model = "best") {
      fits <- private$get_fits(model, require_derivative = FALSE)
      data.table::rbindlist(lapply(fits, function(fit) {
        data.table::data.table(
          row_id = seq_len(nrow(self$data)),
          model_name = fit$model_name,
          x = self$data[[self$x]],
          observed = self$data[[self$y]],
          fitted = fit$fitted_values
        )
      }), fill = TRUE)
    },

    residuals = function(model = "best") {
      fits <- private$get_fits(model, require_derivative = FALSE)
      data.table::rbindlist(lapply(fits, function(fit) {
        data.table::data.table(
          row_id = seq_len(nrow(self$data)),
          model_name = fit$model_name,
          x = self$data[[self$x]],
          residual = fit$residuals
        )
      }), fill = TRUE)
    },

    predict = function(new_data, model = "best") {
      self$score(new_data = new_data, model = model)[, .(row_id, model_name, prediction)]
    },

    score = function(new_data, model = "best") {
      ND <- data.table::as.data.table(new_data)
      if (!self$x %in% names(ND)) stop("new_data must include x column: ", self$x)
      row_id <- seq_len(nrow(ND))
      x_raw <- as.numeric(ND[[self$x]])
      x_fit <- private$scale_x_values(x_raw)
      fits <- private$get_fits(model, require_derivative = FALSE)
      out <- data.table::rbindlist(lapply(fits, function(fit) {
        pred_scaled <- fit$spec$model_function(x_fit, fit$params)
        data.table::data.table(
          row_id = row_id,
          model_name = fit$model_name,
          x = x_raw,
          prediction = private$unscale_y_values(pred_scaled)
        )
      }), fill = TRUE)
      data.table::setorder(out, model_name, row_id)
      out[]
    },

    derivative = function(new_data, model = "best") {
      ND <- data.table::as.data.table(new_data)
      if (!self$x %in% names(ND)) stop("new_data must include x column: ", self$x)
      row_id <- seq_len(nrow(ND))
      x_raw <- as.numeric(ND[[self$x]])
      x_fit <- private$scale_x_values(x_raw)
      fits <- private$get_fits(model, require_derivative = TRUE)
      out <- data.table::rbindlist(lapply(fits, function(fit) {
        d_scaled <- fit$spec$derivative_function(x_fit, fit$params)
        data.table::data.table(
          row_id = row_id,
          model_name = fit$model_name,
          x = x_raw,
          derivative = d_scaled * private$dy_dx_multiplier()
        )
      }), fill = TRUE)
      data.table::setorder(out, model_name, row_id)
      out[]
    },

    elasticity = function(new_data, model = "best") {
      sc <- self$score(new_data, model = model)
      de <- self$derivative(new_data, model = model)
      out <- merge(sc, de[, .(row_id, model_name, derivative)], by = c("row_id", "model_name"), sort = FALSE)
      denom <- out$prediction
      x_val <- out$x
      elas <- out$derivative * x_val / denom
      elas[!is.finite(elas) | abs(denom) < 1e-8 | abs(x_val) < 1e-12] <- NA_real_
      out[, elasticity := elas]
      out[, .(row_id, model_name, x, elasticity)]
    },

    plots = function() {
      best <- self$best_model()
      if (is.null(best)) {
        return(list(compare_data = data.table::data.table()))
      }
      grid <- seq(min(self$data[[self$x]], na.rm = TRUE), max(self$data[[self$x]], na.rm = TRUE), length.out = 100)
      grid_dt <- data.table::data.table(x_tmp = grid)
      data.table::setnames(grid_dt, "x_tmp", self$x)
      list(
        observations = data.table::data.table(
          row_id = seq_len(nrow(self$data)),
          x = self$data[[self$x]],
          y = self$data[[self$y]],
          split = ifelse(seq_len(nrow(self$data)) %in% self$validation_index, "validation", "train")
        ),
        compare_data = self$compare_plot(grid = grid, model = "all"),
        best_fit = self$score(grid_dt, model = best$model_name),
        validation_fit = if (length(self$validation_index) == 0) data.table::data.table() else self$fitted_values(best$model_name)[row_id %in% self$validation_index],
        derivative_curve = self$derivative(grid_dt, model = best$model_name),
        elasticity_curve = self$elasticity(grid_dt, model = best$model_name),
        parameter_table = data.table::data.table(
          model_name = best$model_name,
          parameter = names(best$params),
          estimate = as.numeric(best$params)
        ),
        confidence_ribbon = if (is.null(self$prediction_intervals) || nrow(self$prediction_intervals) == 0) NULL else self$prediction_intervals[model_name == best$model_name],
        warnings_summary = data.table::copy(self$fit_warnings),
        diagnostics_panel = data.table::data.table(
          model_name = best$model_name,
          overall_score = self$ranking_summary[model_name == best$model_name, overall_score][1],
          ranking_position = self$ranking_summary[model_name == best$model_name, ranking_position][1],
          confidence_score = max(0, min(100, 100 - self$ranking_summary[model_name == best$model_name, stability_penalty][1] - nrow(self$fit_warnings[code != "none"]) * 5)),
          warning_count = nrow(self$fit_warnings[code != "none"])
        )
      )
    },

    compare_plot = function(grid = NULL, model = "all") {
      if (is.null(grid)) {
        xr <- range(self$data[[self$x]], na.rm = TRUE)
        grid <- seq(xr[1], xr[2], length.out = 100)
      }
      grid_dt <- data.table::data.table(x_tmp = grid)
      data.table::setnames(grid_dt, "x_tmp", self$x)
      self$score(grid_dt, model = model)
    },

    best_model = function() {
      if (!is.null(self$ranking_summary) && nrow(self$ranking_summary[converged == TRUE]) > 0) {
        row <- self$ranking_summary[converged == TRUE][order(overall_score, model_name)][1]
        return(self$fit_results[[row$model_name]])
      }
      if (is.null(self$metrics_table) || nrow(self$metrics_table[converged == TRUE]) == 0) return(NULL)
      row <- self$metrics_table[converged == TRUE][order(objective, model_name)][1]
      self$fit_results[[row$model_name]]
    },

    artifacts = function() {
      if (is.null(self$artifacts_cache)) {
        self$artifacts_cache <- generate_autonls_artifacts(self)
      }
      self$artifacts_cache
    },

    report = function() {
      list(
        summary = self$summary(),
        metrics = self$metrics(),
        diagnostics = data.table::copy(self$diagnostics),
        artifacts = self$artifacts()
      )
    }
  ),

  private = list(
    build_scale_params = function(x, y) {
      xr <- range(x, na.rm = TRUE)
      yr <- range(y, na.rm = TRUE)
      x_scale <- if (self$scale_x) diff(xr) else 1
      y_scale <- if (self$scale_y) diff(yr) else 1
      if (!is.finite(x_scale) || x_scale == 0) x_scale <- 1
      if (!is.finite(y_scale) || y_scale == 0) y_scale <- 1
      list(
        x_center = if (self$scale_x) xr[1] else 0,
        x_scale = x_scale,
        y_center = if (self$scale_y) yr[1] else 0,
        y_scale = y_scale
      )
    },

    scale_x_values = function(x) (as.numeric(x) - self$scale_params$x_center) / self$scale_params$x_scale,
    scale_y_values = function(y) (as.numeric(y) - self$scale_params$y_center) / self$scale_params$y_scale,
    unscale_y_values = function(y) as.numeric(y) * self$scale_params$y_scale + self$scale_params$y_center,
    dy_dx_multiplier = function() self$scale_params$y_scale / self$scale_params$x_scale,

    make_validation_split = function(n) {
      self$train_index <- seq_len(n)
      self$validation_index <- integer(0)
      if (self$validation_fraction <= 0 || n < 10) return(invisible(NULL))
      n_valid <- max(1L, floor(n * self$validation_fraction))
      n_valid <- min(n_valid, n - 3L)
      set.seed(self$validation_seed)
      self$validation_index <- sort(sample(seq_len(n), n_valid))
      self$train_index <- setdiff(seq_len(n), self$validation_index)
      invisible(NULL)
    },

    weights = function() {
      if (is.null(self$weights_col)) return(NULL)
      w <- as.numeric(self$data[[self$weights_col]])
      w[!is.finite(w) | w < 0] <- 0
      if (sum(w) == 0) return(NULL)
      w
    },

    spec_with_overrides = function(spec) {
      if (!is.null(self$lower_bounds)) {
        take <- intersect(names(self$lower_bounds), spec$parameter_names)
        spec$lower_bounds[take] <- self$lower_bounds[take]
      }
      if (!is.null(self$upper_bounds)) {
        take <- intersect(names(self$upper_bounds), spec$parameter_names)
        spec$upper_bounds[take] <- self$upper_bounds[take]
      }
      spec
    },

    model_family_key = function(spec) {
      tags <- tolower(c(spec$family, spec$tags, spec$model_name))
      if ("linear" %in% tags) return("linear")
      if (any(c("michaelismenten", "kinetics") %in% tags) || identical(spec$model_name, "MichaelisMenten")) return("michaelis_menten")
      if (any(c("gompertz") %in% tags) || grepl("gompertz", tolower(spec$model_name))) return("gompertz")
      if (any(c("weibull") %in% tags)) return("weibull")
      if (any(c("richards") %in% tags) || identical(spec$model_name, "Richards")) return("richards")
      if (any(c("sigmoid", "growth") %in% tags)) return("sigmoid")
      if (any(c("decay") %in% tags)) return("decay")
      if (any(c("power", "scaling") %in% tags)) return("power")
      if (any(c("log") %in% tags)) return("logarithmic")
      if (any(c("hyperbola", "saturation", "hill") %in% tags)) return("saturation")
      "experimental"
    },

    make_starts = function(spec, x_vec, y_vec) {
      base <- private$family_start_base(spec, x_vec, y_vec)
      starts <- matrix(rep(base, each = self$n_starts), nrow = self$n_starts)
      colnames(starts) <- names(base)
      lb <- spec$lower_bounds[colnames(starts)]
      ub <- spec$upper_bounds[colnames(starts)]
      starts[1, ] <- pmin(pmax(starts[1, ], lb), ub)

      if (self$n_starts > 1) {
        multipliers <- seq(0.55, 1.75, length.out = self$n_starts)
        shifts <- seq(-0.25, 0.25, length.out = self$n_starts)
        for (i in 2:self$n_starts) {
          candidate <- base
          finite_bounds <- is.finite(lb) & is.finite(ub)
          if (any(finite_bounds)) {
            span <- ub[finite_bounds] - lb[finite_bounds]
            fraction <- ((i - 1) %% self$n_starts) / max(1, self$n_starts - 1)
            candidate[finite_bounds] <- lb[finite_bounds] + span * fraction
          }
          free <- !finite_bounds
          if (any(free)) {
            candidate[free] <- base[free] * multipliers[i] + shifts[i]
          }
          starts[i, ] <- pmin(pmax(candidate, lb), ub)
        }
      }
      data.table::as.data.table(starts)
    },

    family_start_base = function(spec, x_vec, y_vec) {
      if (identical(self$start_strategy, "generic")) {
        return(private$generic_start_base(spec, x_vec, y_vec))
      }
      base <- spec$start_params
      x <- as.numeric(x_vec)
      y <- as.numeric(y_vec)
      y_min <- min(y, na.rm = TRUE)
      y_max <- max(y, na.rm = TRUE)
      y_rng <- max(1e-8, y_max - y_min)
      x_med <- stats::median(x, na.rm = TRUE)
      key <- private$model_family_key(spec)
      slope <- private$safe_lm_slope(x, y)

      if (identical(key, "linear") && all(c("a", "b") %in% names(base))) {
        co <- private$safe_lm_coef(x, y)
        base[["a"]] <- co[1]
        base[["b"]] <- co[2]
      }
      if (key %in% c("saturation", "michaelis_menten", "weibull")) {
        if ("a" %in% names(base)) base[["a"]] <- y_max
        if ("Vmax" %in% names(base)) base[["Vmax"]] <- y_max
        if ("c" %in% names(base)) base[["c"]] <- max(1e-4, x_med)
        if ("Km" %in% names(base)) base[["Km"]] <- max(1e-4, x_med)
        if ("b" %in% names(base)) base[["b"]] <- max(0.25, min(3, abs(slope) + 1))
        if ("d" %in% names(base)) base[["d"]] <- y_min
      }
      if (key %in% c("decay", "gompertz")) {
        if ("a" %in% names(base)) base[["a"]] <- y_max
        if ("d" %in% names(base)) base[["d"]] <- y_min
        if ("c" %in% names(base)) base[["c"]] <- max(0.05, abs(slope))
        if ("b" %in% names(base)) base[["b"]] <- private$decay_rate_start(x, y)
      }
      if (identical(key, "sigmoid") || identical(key, "richards")) {
        if ("a" %in% names(base)) base[["a"]] <- y_max
        if ("d" %in% names(base)) base[["d"]] <- y_min
        if ("c" %in% names(base)) base[["c"]] <- private$midpoint_start(x, y)
        if ("b" %in% names(base)) base[["b"]] <- max(0.2, abs(slope) * 4)
      }
      if (identical(key, "power") && all(c("a", "b") %in% names(base))) {
        co <- private$log_regression_start(x, y)
        base[["a"]] <- co[1]
        base[["b"]] <- co[2]
      }
      if (identical(key, "logarithmic") && all(c("a", "b") %in% names(base))) {
        co <- private$log_x_regression_start(x, y)
        base[["a"]] <- co[1]
        base[["b"]] <- abs(co[2])
      }
      if ("s" %in% names(base)) base[["s"]] <- x_med
      if ("k" %in% names(base)) base[["k"]] <- 5
      if ("e" %in% names(base) && !is.finite(base[["e"]])) base[["e"]] <- 1
      if (self$start_strategy %in% c("log_transformed", "family_transformed")) {
        base <- private$transformed_start_overlay(spec, base, x, y)
      }
      base[!is.finite(base)] <- spec$start_params[!is.finite(base)]
      base <- pmin(pmax(base, spec$lower_bounds[names(base)]), spec$upper_bounds[names(base)])
      base
    },

    generic_start_base = function(spec, x_vec, y_vec) {
      base <- spec$start_params
      yr <- range(y_vec, na.rm = TRUE)
      if ("a" %in% names(base)) base[["a"]] <- if (max(abs(yr)) > 0) max(y_vec, na.rm = TRUE) else 1
      if ("d" %in% names(base)) base[["d"]] <- min(y_vec, na.rm = TRUE)
      if ("c" %in% names(base)) base[["c"]] <- stats::median(x_vec, na.rm = TRUE)
      if ("Km" %in% names(base)) base[["Km"]] <- stats::median(x_vec, na.rm = TRUE)
      if ("Vmax" %in% names(base)) base[["Vmax"]] <- max(y_vec, na.rm = TRUE)
      if (identical(spec$model_name, "Linear") && all(c("a", "b") %in% names(base))) {
        co <- private$safe_lm_coef(x_vec, y_vec)
        base[["a"]] <- co[1]
        base[["b"]] <- co[2]
      }
      base[!is.finite(base)] <- spec$start_params[!is.finite(base)]
      pmin(pmax(base, spec$lower_bounds[names(base)]), spec$upper_bounds[names(base)])
    },

    transformed_start_overlay = function(spec, base, x, y) {
      if (all(c("a", "b") %in% names(base))) {
        co <- private$log_regression_start(x + 1e-8, pmax(y - min(y, na.rm = TRUE) + 1e-8, 1e-8))
        if (identical(self$start_strategy, "log_transformed") || private$model_family_key(spec) %in% c("power", "decay", "logarithmic", "saturation")) {
          base[["a"]] <- if (is.finite(co[1])) co[1] else base[["a"]]
          base[["b"]] <- if (is.finite(co[2])) abs(co[2]) else base[["b"]]
        }
      }
      if ("c" %in% names(base)) {
        lx <- log1p(pmax(x, 0))
        base[["c"]] <- exp(stats::median(lx, na.rm = TRUE)) - 1
      }
      if ("Km" %in% names(base)) {
        lx <- log1p(pmax(x, 0))
        base[["Km"]] <- exp(stats::median(lx, na.rm = TRUE)) - 1
      }
      if (private$model_family_key(spec) == "decay" && "b" %in% names(base)) {
        base[["b"]] <- private$decay_rate_start(log1p(pmax(x, 0)), y)
      }
      base
    },

    safe_lm_coef = function(x, y) {
      co <- tryCatch(stats::coef(stats::lm(y ~ x)), error = function(e) c(`(Intercept)` = mean(y), x = 0))
      if (length(co) < 2 || any(!is.finite(co))) co <- c(mean(y), 0)
      unname(co[1:2])
    },

    safe_lm_slope = function(x, y) private$safe_lm_coef(x, y)[2],

    log_regression_start = function(x, y) {
      keep <- x > 0 & y > 0 & is.finite(x) & is.finite(y)
      if (sum(keep) < 3) return(c(a = max(y, na.rm = TRUE), b = 1))
      co <- tryCatch(stats::coef(stats::lm(log(y[keep]) ~ log(x[keep]))), error = function(e) c(0, 1))
      c(a = exp(co[1]), b = co[2])
    },

    log_x_regression_start = function(x, y) {
      keep <- x > 0 & is.finite(x) & is.finite(y)
      if (sum(keep) < 3) return(c(a = mean(y, na.rm = TRUE), b = 1))
      co <- tryCatch(stats::coef(stats::lm(y[keep] ~ log(x[keep]))), error = function(e) c(mean(y), 1))
      c(a = co[1], b = co[2])
    },

    decay_rate_start = function(x, y) {
      o <- order(x)
      yy <- pmax(y[o] - min(y, na.rm = TRUE) + 1e-6, 1e-6)
      xx <- x[o]
      if (length(unique(xx)) < 2) return(0.1)
      rate <- tryCatch(-stats::coef(stats::lm(log(yy) ~ xx))[2], error = function(e) 0.1)
      max(0.01, min(10, abs(rate)))
    },

    midpoint_start = function(x, y) {
      target <- min(y, na.rm = TRUE) + 0.5 * diff(range(y, na.rm = TRUE))
      x[which.min(abs(y - target))]
    },

    domain_diagnostics_for_model = function(spec, raw_x, raw_y) {
      rows <- list()
      add <- function(status, warning, severity, recommendation) {
        rows[[length(rows) + 1L]] <<- data.table::data.table(
          model = spec$model_name,
          status = status,
          warning = warning,
          severity = severity,
          recommendation = recommendation
        )
      }
      if (length(raw_x) < length(spec$parameter_names) + 3L) {
        add("warn", "sample size is small relative to parameter count", "medium", "Use fewer-parameter models or collect more observations.")
      }
      if (length(unique(raw_x[is.finite(raw_x)])) < 3L) {
        add("block", "insufficient unique x values", "high", "Use at least three unique x values before fitting nonlinear curves.")
      }
      if (stats::var(raw_x, na.rm = TRUE) == 0) {
        add("block", "x is constant", "high", "A functional curve cannot be estimated from constant x.")
      }
      if (stats::var(raw_y, na.rm = TRUE) == 0) {
        add("warn", "y is constant", "medium", "Prefer a constant baseline; nonlinear parameters may be unidentifiable.")
      }
      domain <- tolower(spec$domain)
      if (grepl("positive", domain) && any(raw_x <= 0, na.rm = TRUE)) {
        add("warn", "x contains non-positive values for a positive-domain model", "medium", "Use a shifted x variable or choose models without positive-domain assumptions.")
      }
      if (any(!is.finite(raw_x)) || any(!is.finite(raw_y))) {
        add("block", "x or y contains non-finite values", "high", "Remove or impute non-finite values before fitting.")
      }
      if (length(spec$parameter_names) > max(3L, floor(length(raw_x) / 4))) {
        add("warn", "parameter identifiability risk from high parameter count", "medium", "Increase n, use stronger starts, or prefer simpler models.")
      }
      mono <- private$observed_monotonicity(raw_x, raw_y)
      if (grepl("increasing", tolower(spec$monotonic)) && identical(mono, "decreasing")) {
        add("warn", "observed relationship is decreasing while model expects increasing behavior", "medium", "Consider decay models or inspect the data.")
      }
      if (grepl("decreasing", tolower(spec$monotonic)) && identical(mono, "increasing")) {
        add("warn", "observed relationship is increasing while model expects decreasing behavior", "medium", "Consider growth or saturation models.")
      }
      if (length(rows) == 0) {
        add("ok", "domain checks passed", "none", "Fit model.")
      }
      data.table::rbindlist(rows)
    },

    observed_monotonicity = function(x, y) {
      o <- order(x)
      yy <- y[o]
      rho <- suppressWarnings(stats::cor(seq_along(yy), yy, method = "spearman", use = "complete.obs"))
      if (!is.finite(rho) || abs(rho) < 0.25) return("mixed")
      if (rho > 0) "increasing" else "decreasing"
    },

    blocking_domain_message = function(domain_dt) {
      bad <- domain_dt[status == "block"]
      if (nrow(bad) == 0) return(NA_character_)
      paste(bad$warning, collapse = " | ")
    },

    suitability_for_model = function(spec, raw_x, raw_y, domain_dt) {
      score <- 100
      reasons <- character(0)
      high <- nrow(domain_dt[severity == "high"])
      med <- nrow(domain_dt[severity == "medium"])
      score <- score - high * 45 - med * 12
      if (high > 0) reasons <- c(reasons, "domain blocker")
      if (med > 0) reasons <- c(reasons, "domain warning")
      if (identical(spec$status, "experimental")) {
        score <- score - 15
        reasons <- c(reasons, "experimental penalty")
      }
      mono <- private$observed_monotonicity(raw_x, raw_y)
      family_key <- private$model_family_key(spec)
      if (identical(mono, "mixed") && family_key %in% c("saturation", "decay", "sigmoid", "power")) {
        score <- score - 8
        reasons <- c(reasons, "weak monotonic signal")
      }
      if (length(unique(raw_x)) < length(spec$parameter_names) * 2) {
        score <- score - 10
        reasons <- c(reasons, "few unique x values")
      }
      score <- max(0, min(100, score))
      data.table::data.table(
        model_name = spec$model_name,
        family = spec$family,
        status = spec$status,
        suitability_score = score,
        domain_compatible = nrow(domain_dt[status == "block"]) == 0,
        observed_monotonicity = mono,
        reasons = if (length(reasons) == 0) "suitable" else paste(unique(reasons), collapse = "; ")
      )
    },

    parameter_stability_for_model = function(spec, start_results) {
      successful <- Filter(function(z) identical(z$status, "converged") && is.finite(z$objective), start_results)
      if (length(successful) == 0) {
        return(data.table::data.table(
          model_name = spec$model_name,
          parameter = spec$parameter_names,
          mean = NA_real_,
          sd = NA_real_,
          coefficient_of_variation = NA_real_,
          best_solution_frequency = 0,
          number_converged = 0,
          objective_spread = NA_real_
        ))
      }
      par_mat <- do.call(rbind, lapply(successful, function(z) z$params[spec$parameter_names]))
      objectives <- vapply(successful, `[[`, numeric(1), "objective")
      best_id <- which.min(objectives)
      data.table::data.table(
        model_name = spec$model_name,
        parameter = colnames(par_mat),
        mean = colMeans(par_mat, na.rm = TRUE),
        sd = apply(par_mat, 2, stats::sd, na.rm = TRUE),
        coefficient_of_variation = apply(par_mat, 2, function(v) {
          m <- mean(v, na.rm = TRUE)
          s <- stats::sd(v, na.rm = TRUE)
          if (!is.finite(m) || abs(m) < 1e-8) NA_real_ else abs(s / m)
        }),
        best_solution_frequency = 1 / length(successful),
        number_converged = length(successful),
        objective_spread = max(objectives) - min(objectives)
      )
    },

    explain_failure = function(spec, domain_dt, start_results) {
      reasons <- character(0)
      if (nrow(domain_dt[status == "block"]) > 0) reasons <- c(reasons, domain_dt[status == "block", warning])
      if (nrow(domain_dt[grepl("identifiability", warning)]) > 0) reasons <- c(reasons, "parameter not identifiable")
      if (!is.null(start_results) && length(start_results) > 0) {
        objectives <- vapply(start_results, function(z) z$objective, numeric(1))
        if (all(!is.finite(objectives))) reasons <- c(reasons, "all starts produced invalid objectives")
        if (length(unique(vapply(start_results, `[[`, character(1), "status"))) > 1) reasons <- c(reasons, "local minima or poor initialization")
      }
      if (grepl("saturation|sigmoid|hill", paste(tolower(c(spec$family, spec$tags)), collapse = " "))) reasons <- c(reasons, "insufficient curvature")
      if (length(reasons) == 0) reasons <- "optimizer did not find a valid solution"
      paste(unique(reasons), collapse = "; ")
    },

    fit_one_start = function(spec, x_vec, y_vec, w_vec, start, start_id) {
      t0 <- proc.time()[["elapsed"]]
      par0 <- as.numeric(start)
      names(par0) <- names(start)
      lb <- spec$lower_bounds[names(par0)]
      ub <- spec$upper_bounds[names(par0)]
      objective <- function(par) {
        names(par) <- names(par0)
        pred <- tryCatch(spec$model_function(x_vec, par), error = function(e) rep(NA_real_, length(y_vec)))
        if (length(pred) != length(y_vec) || all(!is.finite(pred)) || any(!is.finite(pred))) return(Inf)
        r <- y_vec - pred
        loss_vec <- switch(
          self$loss,
          mse = r^2,
          mae = abs(r),
          huber = ifelse(abs(r) <= self$huber_delta, 0.5 * r^2, self$huber_delta * (abs(r) - 0.5 * self$huber_delta)),
          quantile = ifelse(r >= 0, self$quantile_level * r, (self$quantile_level - 1) * r)
        )
        if (is.null(w_vec)) sum(loss_vec) else sum(w_vec * loss_vec)
      }

      opt_name <- self$optimizer
      if (identical(opt_name, "nlsLM") && !requireNamespace("minpack.lm", quietly = TRUE)) opt_name <- "optim"
      res <- tryCatch({
        if (identical(opt_name, "nlsLM") && identical(self$loss, "mse")) {
          private$fit_with_nlslm(spec, x_vec, y_vec, w_vec, par0, lb, ub)
        } else {
          stats::optim(
            par0,
            objective,
            method = "L-BFGS-B",
            lower = lb,
            upper = ub,
            control = list(
              maxit = self$maxit,
              factr = self$reltol / .Machine$double.eps,
              pgtol = self$reltol
            )
          )
        }
      }, error = function(e) e)

      elapsed <- proc.time()[["elapsed"]] - t0
      if (inherits(res, "error")) {
        return(list(start_id = start_id, status = "error", objective = Inf, convergence_code = NA_integer_, message = res$message, elapsed_time = elapsed))
      }
      params <- res$par
      names(params) <- names(par0)
      value <- objective(params)
      converged <- is.finite(value) && (is.null(res$convergence) || identical(res$convergence, 0L))
      list(
        start_id = start_id,
        status = if (converged) "converged" else "failed",
        objective = value,
        params = params,
        convergence_code = if (is.null(res$convergence)) 0L else res$convergence,
        message = if (is.null(res$message)) "" else res$message,
        elapsed_time = elapsed
      )
    },

    fit_with_nlslm = function(spec, x_vec, y_vec, w_vec, par0, lb, ub) {
      fn <- function(par) {
        names(par) <- names(par0)
        r <- y_vec - spec$model_function(x_vec, par)
        if (!is.null(w_vec)) r <- sqrt(w_vec) * r
        r
      }
      fit <- minpack.lm::nls.lm(
        par = par0,
        fn = fn,
        lower = lb,
        upper = ub,
        control = minpack.lm::nls.lm.control(maxiter = self$maxit, ftol = self$reltol, ptol = self$reltol)
      )
      list(par = fit$par, convergence = if (fit$info %in% 1:4) 0L else fit$info, message = fit$message)
    },

    build_metrics = function() {
      y_vec <- as.numeric(self$data[[self$y]])
      rows <- lapply(names(self$fit_results), function(model_name) {
        fit <- self$fit_results[[model_name]]
        diag <- self$diagnostics[model_name == fit$model_name]
        if (!isTRUE(fit$converged)) {
          return(data.table::data.table(
            model_name = model_name,
            family = fit$spec$family,
            status = fit$spec$status,
            converged = FALSE,
            objective = Inf,
            n = length(y_vec),
            n_params = length(fit$spec$parameter_names),
            rmse = NA_real_,
            mae = NA_real_,
            train_rmse = NA_real_,
            train_mae = NA_real_,
            mape = NA_real_,
            smape = NA_real_,
            r_squared = NA_real_,
            adj_r_squared = NA_real_,
            aic = NA_real_,
            bic = NA_real_,
            best_start_id = NA_integer_,
            n_starts_attempted = nrow(diag),
            n_starts_converged = nrow(diag[status == "converged"]),
            warning_summary = fit$warning_summary,
            failure_explanation = fit$failure_explanation
          ))
        }
        r <- fit$residuals
        r_train <- fit$residuals[self$train_index]
        n <- length(r)
        k <- length(fit$params)
        rss <- sum(r^2)
        train_rss <- sum(r_train^2)
        tss <- sum((y_vec - mean(y_vec))^2)
        rmse <- sqrt(mean(r^2))
        mae <- mean(abs(r))
        train_rmse <- sqrt(mean(r_train^2))
        train_mae <- mean(abs(r_train))
        mape <- mean(ifelse(abs(y_vec) < 1e-8, NA_real_, abs(r / y_vec) * 100), na.rm = TRUE)
        smape <- mean(ifelse(abs(y_vec) + abs(fit$fitted_values) < 1e-8, NA_real_, 200 * abs(r) / (abs(y_vec) + abs(fit$fitted_values))), na.rm = TRUE)
        r2 <- if (tss == 0) NA_real_ else 1 - rss / tss
        adj <- if (is.na(r2) || n - k - 1 <= 0) NA_real_ else 1 - (1 - r2) * (n - 1) / (n - k - 1)
        s2 <- rss / n
        loglik <- if (s2 <= 0 || !is.finite(s2)) NA_real_ else -0.5 * n * (log(2 * pi) + log(s2) + 1)
        data.table::data.table(
          model_name = model_name,
          family = fit$spec$family,
          status = fit$spec$status,
          converged = TRUE,
          objective = fit$objective,
          n = n,
          n_params = k,
          rmse = rmse,
          mae = mae,
          train_rmse = train_rmse,
          train_mae = train_mae,
          mape = mape,
          smape = smape,
          r_squared = r2,
          adj_r_squared = adj,
          aic = if (is.na(loglik)) NA_real_ else -2 * loglik + 2 * k,
          bic = if (is.na(loglik)) NA_real_ else -2 * loglik + log(n) * k,
          best_start_id = fit$best_start_id,
          n_starts_attempted = fit$n_starts_attempted,
          n_starts_converged = fit$n_starts_converged,
          warning_summary = fit$warning_summary,
          failure_explanation = fit$failure_explanation
        )
      })
      data.table::rbindlist(rows, fill = TRUE)
    },

    build_validation_metrics = function() {
      if (length(self$validation_index) == 0) {
        return(data.table::data.table())
      }
      y_vec <- as.numeric(self$data[[self$y]])
      rows <- lapply(names(self$fit_results), function(model_name) {
        fit <- self$fit_results[[model_name]]
        if (!isTRUE(fit$converged)) return(NULL)
        idx <- self$validation_index
        r <- fit$residuals[idx]
        obs <- y_vec[idx]
        pred <- fit$fitted_values[idx]
        train_r <- fit$residuals[self$train_index]
        rmse <- sqrt(mean(r^2))
        train_rmse <- sqrt(mean(train_r^2))
        data.table::data.table(
          model_name = model_name,
          validation_n = length(idx),
          validation_rmse = rmse,
          validation_mae = mean(abs(r)),
          validation_r_squared = {
            tss <- sum((obs - mean(obs))^2)
            if (tss == 0) NA_real_ else 1 - sum(r^2) / tss
          },
          validation_mape = mean(ifelse(abs(obs) < 1e-8, NA_real_, abs(r / obs) * 100), na.rm = TRUE),
          validation_smape = mean(ifelse(abs(obs) + abs(pred) < 1e-8, NA_real_, 200 * abs(r) / (abs(obs) + abs(pred))), na.rm = TRUE),
          overfit_indicator = rmse - train_rmse,
          overfit_ratio = if (!is.finite(train_rmse) || train_rmse < 1e-8) NA_real_ else rmse / train_rmse
        )
      })
      data.table::rbindlist(rows, fill = TRUE)
    },

    build_ranking_summary = function() {
      metrics <- data.table::copy(self$metrics_table)
      if (nrow(metrics) == 0) return(metrics)
      suitability <- data.table::copy(self$model_suitability)
      stability <- data.table::copy(self$parameter_stability)
      if (nrow(stability) == 0 || !"model_name" %in% names(stability)) {
        stability_summary <- data.table::data.table(model_name = character(), max_parameter_cv = numeric(), objective_spread = numeric())
      } else {
        stability_summary <- stability[, .(
          max_parameter_cv = suppressWarnings(max(coefficient_of_variation, na.rm = TRUE)),
          objective_spread = suppressWarnings(max(objective_spread, na.rm = TRUE))
        ), by = model_name]
      }
      stability_summary[!is.finite(max_parameter_cv), max_parameter_cv := NA_real_]
      stability_summary[!is.finite(objective_spread), objective_spread := NA_real_]
      if (nrow(suitability) == 0 || !"model_name" %in% names(suitability)) {
        suitability <- data.table::data.table(model_name = metrics$model_name, suitability_score = 50, domain_compatible = FALSE)
      }
      out <- merge(metrics, suitability[, .(model_name, suitability_score, domain_compatible)], by = "model_name", all.x = TRUE, sort = FALSE)
      out <- merge(out, stability_summary, by = "model_name", all.x = TRUE, sort = FALSE)
      if (nrow(self$validation_metrics) > 0) {
        out <- merge(out, self$validation_metrics[, .(model_name, validation_rmse, overfit_ratio)], by = "model_name", all.x = TRUE, sort = FALSE)
      }
      converged <- out$converged == TRUE
      out[, rmse_score := private$scaled_rank_score(rmse, lower_is_better = TRUE)]
      out[, mae_score := private$scaled_rank_score(mae, lower_is_better = TRUE)]
      out[, r2_score := private$scaled_rank_score(r_squared, lower_is_better = FALSE)]
      out[, aic_score := private$scaled_rank_score(aic, lower_is_better = TRUE)]
      out[, bic_score := private$scaled_rank_score(bic, lower_is_better = TRUE)]
      out[, convergence_score := ifelse(n_starts_attempted > 0, 100 * n_starts_converged / n_starts_attempted, 0)]
      out[is.na(suitability_score), suitability_score := 50]
      out[, complexity_penalty := pmin(25, n_params * 2)]
      out[, experimental_penalty := ifelse(status == "experimental", 15, 0)]
      out[, stability_penalty := pmin(20, data.table::fifelse(is.na(max_parameter_cv), 10, max_parameter_cv * 10))]
      out[, validation_penalty := 0]
      if ("overfit_ratio" %in% names(out)) {
        out[is.finite(overfit_ratio) & overfit_ratio > 1, validation_penalty := pmin(20, (overfit_ratio - 1) * 10)]
      }
      out[, `:=`(
        rmse_contribution = 0.25 * (100 - rmse_score),
        mae_contribution = 0.15 * (100 - mae_score),
        validation_contribution = validation_penalty,
        convergence_contribution = 0.10 * (100 - convergence_score),
        stability_contribution = stability_penalty,
        suitability_bonus = 0.10 * suitability_score
      )]
      out[, overall_score := (
        rmse_contribution +
          mae_contribution +
          0.15 * (100 - r2_score) +
          0.10 * (100 - aic_score) +
          0.10 * (100 - bic_score) +
          0.10 * (100 - suitability_score) +
          convergence_contribution +
          complexity_penalty +
          experimental_penalty +
          stability_contribution +
          validation_contribution
      )]
      out[!converged, overall_score := Inf]
      out[, ranking_position := data.table::frank(overall_score, ties.method = "first")]
      out[, rank := ranking_position]
      data.table::setorder(out, overall_score, model_name)
      out[, reason_code := private$ranking_reason_code(.SD), by = model_name]
      out[, explanation := private$ranking_explanation(.SD), by = model_name]
      out[]
    },

    ranking_reason_code = function(row) {
      if ("ranking_position" %in% names(row) && isTRUE(as.integer(row$ranking_position) == 1L) && isTRUE(row$converged)) return("SELECTED")
      if (!isTRUE(row$converged)) return("FAILED_CONVERGENCE")
      if ("domain_compatible" %in% names(row) && !isTRUE(row$domain_compatible)) return("DOMAIN_FAILURE")
      if (!is.na(row$suitability_score) && row$suitability_score < 60) return("LOW_SUITABILITY")
      if (!is.na(row$validation_penalty) && row$validation_penalty > 8) return("POOR_VALIDATION")
      if (!is.na(row$stability_penalty) && row$stability_penalty > 10) return("UNSTABLE_PARAMETERS")
      if (!is.na(row$complexity_penalty) && row$complexity_penalty >= 10) return("HIGH_COMPLEXITY")
      if (!is.na(row$experimental_penalty) && row$experimental_penalty > 0) return("EXPERIMENTAL_PENALTY")
      "COMPETITIVE_BUT_NOT_SELECTED"
    },

    ranking_explanation = function(row) {
      if (!isTRUE(row$converged)) {
        return(paste0(row$model_name, " did not converge and was not eligible for selection."))
      }
      pos <- row$ranking_position
      if (identical(pos, 1L) || identical(as.integer(pos), 1L)) {
        strengths <- character(0)
        if (is.finite(row$rmse_score) && row$rmse_score >= 80) strengths <- c(strengths, "strong RMSE")
        if ("validation_penalty" %in% names(row) && is.finite(row$validation_penalty) && row$validation_penalty <= 2) strengths <- c(strengths, "validation support")
        if (is.finite(row$stability_penalty) && row$stability_penalty <= 5) strengths <- c(strengths, "parameter stability")
        if (is.finite(row$suitability_score) && row$suitability_score >= 80) strengths <- c(strengths, "curve-family suitability")
        if (length(strengths) == 0) strengths <- "the lowest overall penalty score"
        return(paste0(row$model_name, " ranked #1 because it combined ", paste(strengths, collapse = ", "), "."))
      }
      code <- private$ranking_reason_code(row)
      reason <- switch(
        code,
        DOMAIN_FAILURE = "domain diagnostics made it less suitable",
        LOW_SUITABILITY = "pre-fit suitability was weak",
        POOR_VALIDATION = "validation performance was weaker than training performance",
        HIGH_COMPLEXITY = "complexity penalty was high relative to competing models",
        UNSTABLE_PARAMETERS = "parameters were unstable across starts",
        FAILED_CONVERGENCE = "it failed convergence",
        EXPERIMENTAL_PENALTY = "it carries an experimental-model penalty",
        "other models had a lower combined score"
      )
      paste0(row$model_name, " ranked #", as.integer(pos), " because ", reason, ".")
    },

    build_prediction_intervals = function() {
      if (identical(self$interval_method, "none")) {
        return(list(
          prediction_intervals = data.table::data.table(),
          interval_diagnostics = data.table::data.table(
            model_name = NA_character_,
            interval_method = self$interval_method,
            status = "skipped",
            message = "interval_method is 'none'",
            n = 0L,
            level = self$interval_level
          )
        ))
      }
      fits <- tryCatch(private$get_fits(self$interval_models, require_derivative = FALSE), error = function(e) list())
      if (length(fits) == 0) {
        return(list(
          prediction_intervals = data.table::data.table(),
          interval_diagnostics = data.table::data.table(
            model_name = NA_character_,
            interval_method = self$interval_method,
            status = "error",
            message = "no converged models available for intervals",
            n = 0L,
            level = self$interval_level
          )
        ))
      }

      grid_n <- min(100L, self$interval_max_rows)
      grid <- seq(min(self$data[[self$x]], na.rm = TRUE), max(self$data[[self$x]], na.rm = TRUE), length.out = grid_n)
      grid_dt <- data.table::data.table(x_tmp = grid)
      data.table::setnames(grid_dt, "x_tmp", self$x)
      alpha <- (1 - self$interval_level) / 2
      set.seed(self$interval_seed)

      interval_rows <- list()
      diag_rows <- list()
      x_fit <- private$scale_x_values(grid)

      for (fit in fits) {
        res <- tryCatch({
          pred <- self$score(grid_dt, model = fit$model_name)
          if (identical(self$interval_method, "residual_bootstrap")) {
            residuals <- fit$residuals
            if (length(residuals) < 2 || all(!is.finite(residuals))) stop("not enough finite residuals for bootstrap")
            sim <- replicate(self$interval_n, pred$prediction + sample(residuals, nrow(pred), replace = TRUE))
          } else {
            param_sd <- private$parameter_sd_for_interval(fit)
            sim <- replicate(self$interval_n, {
              draw <- stats::rnorm(length(fit$params), mean = fit$params, sd = param_sd)
              names(draw) <- names(fit$params)
              private$unscale_y_values(fit$spec$model_function(x_fit, draw))
            })
          }
          lower <- apply(sim, 1, stats::quantile, probs = alpha, na.rm = TRUE)
          upper <- apply(sim, 1, stats::quantile, probs = 1 - alpha, na.rm = TRUE)
          interval_rows[[fit$model_name]] <- data.table::data.table(
            row_id = pred$row_id,
            model_name = fit$model_name,
            x = pred$x,
            prediction = pred$prediction,
            lower = as.numeric(lower),
            upper = as.numeric(upper),
            interval_status = "ok",
            interval_warning = "",
            interval_method = self$interval_method,
            interval_level = self$interval_level
          )
          diag_rows[[fit$model_name]] <- data.table::data.table(
            model_name = fit$model_name,
            interval_method = self$interval_method,
            status = "ok",
            message = "",
            n = self$interval_n,
            level = self$interval_level
          )
          TRUE
        }, error = function(e) {
          diag_rows[[fit$model_name]] <- data.table::data.table(
            model_name = fit$model_name,
            interval_method = self$interval_method,
            status = "error",
            message = e$message,
            n = self$interval_n,
            level = self$interval_level
          )
          FALSE
        })
      }

      list(
        prediction_intervals = data.table::rbindlist(interval_rows, fill = TRUE),
        interval_diagnostics = data.table::rbindlist(diag_rows, fill = TRUE)
      )
    },

    parameter_sd_for_interval = function(fit) {
      stab <- self$parameter_stability[model_name == fit$model_name]
      out <- abs(fit$params) * 0.05 + 1e-6
      if (nrow(stab) > 0) {
        idx <- match(names(out), stab$parameter)
        sdv <- stab$sd[idx]
        take <- is.finite(sdv) & sdv > 0
        out[take] <- sdv[take]
      }
      out
    },

    build_fit_guidance = function() {
      warning_rows <- list()
      rec_rows <- list()
      add_warning <- function(code, severity, message, model_name = NA_character_) {
        warning_rows[[length(warning_rows) + 1L]] <<- data.table::data.table(
          model_name = model_name,
          code = code,
          severity = severity,
          warning = message
        )
      }
      add_rec <- function(code, recommendation, model_name = NA_character_) {
        rec_rows[[length(rec_rows) + 1L]] <<- data.table::data.table(
          model_name = model_name,
          code = code,
          recommendation = recommendation
        )
      }

      best <- self$best_model()
      if (!is.null(best) && identical(best$spec$status, "experimental")) {
        add_warning("best_model_experimental", "medium", "Best model is experimental; review diagnostics before relying on it.", best$model_name)
        add_rec("best_model_experimental", "Compare against the best stable model and inspect parameter stability.", best$model_name)
      }
      skipped <- unique(self$diagnostics[status == "skipped", model_name])
      if (length(skipped) > 0) {
        add_warning("models_skipped_domain", "medium", "Several models were skipped due to domain incompatibility.")
        add_rec("models_skipped_domain", "Inspect domain_diagnostics and consider transformed x/y inputs.")
      }
      if (nrow(self$validation_metrics) > 0) {
        bad <- self$validation_metrics[is.finite(overfit_ratio) & overfit_ratio > 1.5]
        if (nrow(bad) > 0) {
          add_warning("validation_overfit", "medium", "Validation RMSE is much worse than training RMSE; possible overfit.")
          add_rec("validation_overfit", "Prefer simpler models, reduce experimental candidates, or collect more observations.")
        }
      }
      unstable <- if (!is.null(self$parameter_stability) && nrow(self$parameter_stability) > 0 && "coefficient_of_variation" %in% names(self$parameter_stability)) {
        self$parameter_stability[is.finite(coefficient_of_variation) & coefficient_of_variation > 1]
      } else {
        data.table::data.table()
      }
      if (nrow(unstable) > 0) {
        add_warning("parameter_instability", "medium", "Parameter stability is weak across starts; fit may not be identifiable.")
        add_rec("parameter_instability", "Increase n_starts, inspect data curvature, or prefer lower-parameter families.")
      }
      low_unique <- self$domain_diagnostics[grepl("unique x", warning)]
      if (nrow(low_unique) > 0) {
        add_warning("low_unique_x", "high", "Low unique x count limits nonlinear curve identification.")
        add_rec("low_unique_x", "Use more distinct x values before relying on nonlinear effects.")
      }
      flat <- self$domain_diagnostics[grepl("y is constant", warning)]
      if (nrow(flat) > 0) {
        add_warning("flat_signal", "medium", "Target is flat or has very low signal; nonlinear effects may be unreliable.")
        add_rec("flat_signal", "Prefer a simple baseline and avoid interpreting derivatives or elasticity.")
      }
      if (!is.null(best)) {
        boundary <- private$boundary_hit_parameters(best)
        if (nrow(boundary) > 0) {
          add_warning("boundary_parameters", "medium", "Some best-model parameters are near optimizer bounds.", best$model_name)
          add_rec("boundary_parameters", "Review parameter bounds and domain suitability.", best$model_name)
        }
        effect_grid <- data.table::data.table(x_tmp = seq(min(self$data[[self$x]], na.rm = TRUE), max(self$data[[self$x]], na.rm = TRUE), length.out = 50))
        data.table::setnames(effect_grid, "x_tmp", self$x)
        effects_ok <- tryCatch({
          d <- self$derivative(effect_grid, best$model_name)$derivative
          e <- self$elasticity(effect_grid, best$model_name)$elasticity
          if (mean(!is.finite(d)) > 0.2 || mean(!is.finite(e)) > 0.2) {
            add_warning("unstable_effects", "medium", "Derivative or elasticity is unstable for part of the fitted range.", best$model_name)
            add_rec("unstable_effects", "Inspect effect curves before using them as effect-shape artifacts.", best$model_name)
          }
          TRUE
        }, error = function(e) FALSE)
      }

      if (length(warning_rows) == 0) {
        warning_rows[[1L]] <- data.table::data.table(
          model_name = NA_character_,
          code = "none",
          severity = "none",
          warning = "No major deterministic fit warnings."
        )
      }
      if (length(rec_rows) == 0) {
        rec_rows[[1L]] <- data.table::data.table(
          model_name = NA_character_,
          code = "none",
          recommendation = "Review metrics, ranking, and effect curves before production use."
        )
      }
      list(
        fit_warnings = data.table::rbindlist(warning_rows, fill = TRUE),
        fit_recommendations = data.table::rbindlist(rec_rows, fill = TRUE)
      )
    },

    boundary_hit_parameters = function(fit) {
      params <- fit$params
      lb <- fit$spec$lower_bounds[names(params)]
      ub <- fit$spec$upper_bounds[names(params)]
      near <- (is.finite(lb) & abs(params - lb) < 1e-6) | (is.finite(ub) & abs(params - ub) < 1e-6)
      data.table::data.table(
        model_name = fit$model_name,
        parameter = names(params)[near],
        estimate = as.numeric(params[near])
      )
    },

    scaled_rank_score = function(x, lower_is_better = TRUE) {
      x <- as.numeric(x)
      ok <- is.finite(x)
      score <- rep(0, length(x))
      if (sum(ok) == 0) return(score)
      rng <- range(x[ok])
      if (diff(rng) == 0) {
        score[ok] <- 100
        return(score)
      }
      scaled <- (x[ok] - rng[1]) / diff(rng)
      if (lower_is_better) scaled <- 1 - scaled
      score[ok] <- 100 * scaled
      score
    },

    get_fits = function(model, require_derivative = FALSE) {
      if (identical(model, "best")) {
        bm <- self$best_model()
        if (is.null(bm)) stop("No converged models are available.")
        fits <- list(bm)
      } else if (identical(model, "all")) {
        fits <- Filter(function(z) isTRUE(z$converged), self$fit_results)
      } else {
        if (!model %in% names(self$fit_results)) stop("Model not found: ", model)
        fits <- list(self$fit_results[[model]])
      }
      fits <- Filter(function(z) isTRUE(z$converged), fits)
      if (require_derivative) {
        fits <- Filter(function(z) isTRUE(z$spec$supports_derivative) && is.function(z$spec$derivative_function), fits)
      }
      if (length(fits) == 0) stop("No matching converged models are available.")
      fits
    },

    message_summary = function(start_results) {
      msgs <- unique(vapply(start_results, function(z) if (is.null(z$message)) "" else z$message, character(1)))
      msgs <- msgs[nzchar(msgs)]
      if (length(msgs) == 0) "" else paste(utils::head(msgs, 3), collapse = " | ")
    }
  )
)
