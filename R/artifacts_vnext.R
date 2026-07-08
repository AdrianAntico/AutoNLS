#' Generate AutoNLS vNext artifacts
#'
#' @param fit An AutoNLSFit object.
#' @return A named list of data.table artifacts.
#' @export
generate_autonls_artifacts <- function(fit) {
  best <- fit$best_model()
  if (is.null(best)) stop("No converged model is available for artifacts.")

  train_data <- fit$data
  prediction_grid <- data.table::data.table(
    x = seq(min(train_data[[fit$x]], na.rm = TRUE), max(train_data[[fit$x]], na.rm = TRUE), length.out = 100)
  )
  data.table::setnames(prediction_grid, "x", fit$x)

  residuals <- fit$residuals(model = "all")
  residual_summary <- residuals[, .(
    mean_residual = mean(residual, na.rm = TRUE),
    median_residual = stats::median(residual, na.rm = TRUE),
    sd_residual = stats::sd(residual, na.rm = TRUE),
    min_residual = min(residual, na.rm = TRUE),
    max_residual = max(residual, na.rm = TRUE)
  ), by = model_name]
  curve_values <- private_autonls_curve_values(fit, prediction_grid)
  curve_diagnostics <- private_autonls_curve_diagnostics(fit)
  selected_model <- private_autonls_selected_model(fit)
  model_confidence <- private_autonls_model_confidence(fit)

  list(
    model_registry_table = list_nls_models(model_status = "all"),
    model_metrics = fit$metrics(),
    fitted_values = fit$fitted_values(model = "all"),
    residual_summary = residual_summary,
    convergence_diagnostics = data.table::copy(fit$diagnostics),
    domain_diagnostics = data.table::copy(fit$domain_diagnostics),
    model_suitability = data.table::copy(fit$model_suitability),
    parameter_stability = data.table::copy(fit$parameter_stability),
    validation_metrics = data.table::copy(fit$validation_metrics),
    ranking_summary = data.table::copy(fit$ranking_summary),
    prediction_intervals = data.table::copy(fit$prediction_intervals),
    interval_diagnostics = data.table::copy(fit$interval_diagnostics),
    fit_warnings = data.table::copy(fit$fit_warnings),
    fit_recommendations = data.table::copy(fit$fit_recommendations),
    curve_values = curve_values,
    curve_diagnostics = curve_diagnostics,
    selected_model = selected_model,
    model_confidence = model_confidence,
    prediction_curve = fit$score(prediction_grid, model = "all"),
    derivative_curve = fit$derivative(prediction_grid, model = "all"),
    elasticity_curve = fit$elasticity(prediction_grid, model = "all"),
    best_model_summary = data.table::data.table(
      model_name = best$model_name,
      family = best$spec$family,
      status = best$spec$status,
      objective = best$objective,
      best_start_id = best$best_start_id,
      n_starts_converged = best$n_starts_converged
    )
  )
}

private_autonls_curve_values <- function(fit, prediction_grid) {
  score <- fit$score(prediction_grid, model = "all")
  deriv <- fit$derivative(prediction_grid, model = "all")
  elas <- fit$elasticity(prediction_grid, model = "all")
  out <- merge(score, deriv[, .(row_id, model_name, derivative)], by = c("row_id", "model_name"), sort = FALSE)
  out <- merge(out, elas[, .(row_id, model_name, elasticity)], by = c("row_id", "model_name"), sort = FALSE)
  intervals <- fit$prediction_intervals
  if (!is.null(intervals) && nrow(intervals) > 0) {
    out <- merge(out, intervals[, .(model_name, x, lower, upper)], by = c("model_name", "x"), all.x = TRUE, sort = FALSE)
  } else {
    out[, `:=`(lower = NA_real_, upper = NA_real_)]
  }
  best <- fit$best_model()
  out[, `:=`(
    y_hat = prediction,
    is_best_model = !is.null(best) & model_name == best$model_name,
    curve_type = "prediction",
    problem_type = "curve",
    family = vapply(model_name, function(m) fit$fit_results[[m]]$spec$family, character(1)),
    status = vapply(model_name, function(m) fit$fit_results[[m]]$spec$status, character(1)),
    supports_derivative = TRUE,
    supports_elasticity = TRUE,
    original_scale = TRUE,
    x_original_scale = TRUE,
    y_original_scale = TRUE
  )]
  out[, .(model_name, family, status, problem_type, curve_type, supports_derivative, supports_elasticity, original_scale, x, y_hat, derivative, elasticity, lower, upper, is_best_model, x_original_scale, y_original_scale)]
}

private_autonls_curve_diagnostics <- function(fit) {
  rank <- data.table::copy(fit$ranking_summary)
  if (nrow(rank) == 0) return(data.table::data.table())
  warn_counts <- fit$fit_warnings[code != "none", .(warning_count = .N), by = model_name]
  domain <- fit$domain_diagnostics[, .(domain_status = if (any(status == "block")) "blocked" else if (any(status == "warn")) "warning" else "ok"), by = .(model_name = model)]
  out <- merge(rank, domain, by = "model_name", all.x = TRUE, sort = FALSE)
  out <- merge(out, warn_counts, by = "model_name", all.x = TRUE, sort = FALSE)
  out[is.na(warning_count), warning_count := 0L]
  out[is.na(domain_status), domain_status := "unknown"]
  out[, `:=`(
    selected = rank == min(rank[is.finite(rank)], na.rm = TRUE),
    convergence_quality = ifelse(n_starts_attempted > 0, n_starts_converged / n_starts_attempted, NA_real_),
    parameter_stability_flag = data.table::fifelse(is.na(max_parameter_cv), "unknown", data.table::fifelse(max_parameter_cv > 1, "weak", "ok"))
  )]
  val_col <- if ("validation_rmse" %in% names(out)) out$validation_rmse else NA_real_
  data.table::data.table(
    model_name = out$model_name,
    family = out$family,
    status = out$status,
    problem_type = "curve",
    curve_type = "diagnostic",
    supports_derivative = TRUE,
    supports_elasticity = TRUE,
    original_scale = TRUE,
    selected = out$selected,
    rank = out$ranking_position,
    overall_score = out$overall_score,
    rmse = out$rmse,
    mae = out$mae,
    r_squared = out$r_squared,
    validation_rmse = val_col,
    convergence_quality = out$convergence_quality,
    parameter_stability_flag = out$parameter_stability_flag,
    domain_status = out$domain_status,
    warning_count = out$warning_count
  )
}

private_autonls_selected_model <- function(fit) {
  best <- fit$best_model()
  if (is.null(best)) return(list())
  rank <- fit$ranking_summary[model_name == best$model_name][1]
  list(
    model_name = best$model_name,
    family = best$spec$family,
    status = best$spec$status,
    problem_type = "curve",
    curve_type = "selected_model",
    supports_derivative = isTRUE(best$spec$supports_derivative),
    supports_elasticity = isTRUE(best$spec$supports_derivative),
    original_scale = TRUE,
    formula = best$spec$formula,
    parameters = as.list(best$params),
    ranking_reason = rank$explanation,
    warnings = fit$fit_warnings[is.na(model_name) | model_name == best$model_name]
  )
}

private_autonls_model_confidence <- function(fit) {
  best <- fit$best_model()
  if (is.null(best) || nrow(fit$ranking_summary) == 0) {
    return(data.table::data.table())
  }
  rank <- data.table::copy(fit$ranking_summary)
  best_row <- rank[model_name == best$model_name][1]
  second <- rank[is.finite(overall_score) & model_name != best$model_name][order(overall_score)][1]
  top_margin <- if (nrow(second) == 0) NA_real_ else second$overall_score - best_row$overall_score
  warnings <- fit$fit_warnings[code != "none" & (is.na(model_name) | model_name == best$model_name)]
  stability_score <- max(0, 100 - ifelse(is.na(best_row$stability_penalty), 10, best_row$stability_penalty * 5))
  validation_gap <- if ("overfit_ratio" %in% names(best_row) && is.finite(best_row$overfit_ratio)) best_row$overfit_ratio - 1 else NA_real_
  confidence_score <- 100
  confidence_score <- confidence_score - ifelse(is.finite(best_row$convergence_score), 100 - best_row$convergence_score, 25) * 0.25
  confidence_score <- confidence_score - ifelse(is.finite(best_row$stability_penalty), best_row$stability_penalty, 10)
  confidence_score <- confidence_score - ifelse(is.finite(best_row$validation_penalty), best_row$validation_penalty, 0)
  confidence_score <- confidence_score - ifelse(isTRUE(best_row$domain_compatible), 0, 25)
  confidence_score <- confidence_score - min(20, nrow(warnings) * 5)
  confidence_score <- confidence_score + ifelse(is.finite(top_margin), min(10, top_margin / 2), 5)
  confidence_score <- max(0, min(100, confidence_score))
  level <- cut(
    confidence_score,
    breaks = c(-Inf, 20, 40, 60, 80, Inf),
    labels = c("Very Low", "Low", "Moderate", "High", "Very High"),
    right = TRUE
  )
  evidence <- c(
    paste0("rank=", best_row$ranking_position),
    paste0("overall_score=", round(best_row$overall_score, 4)),
    paste0("convergence_score=", round(best_row$convergence_score, 2)),
    paste0("stability_score=", round(stability_score, 2)),
    paste0("top_model_margin=", ifelse(is.na(top_margin), "NA", round(top_margin, 4)))
  )
  data.table::data.table(
    model_name = best$model_name,
    family = best$spec$family,
    status = best$spec$status,
    problem_type = "curve",
    curve_type = "model_confidence",
    supports_derivative = isTRUE(best$spec$supports_derivative),
    supports_elasticity = isTRUE(best$spec$supports_derivative),
    original_scale = TRUE,
    confidence_score = confidence_score,
    confidence_level = as.character(level),
    supporting_evidence = paste(evidence, collapse = "; "),
    warning_count = nrow(warnings),
    stability_score = stability_score,
    validation_gap = validation_gap,
    top_model_margin = top_margin
  )
}

#' Validate raw-scale, scaled, and transformed-start fitting strategies
#'
#' @param data A data.frame or data.table with original-scale data.
#' @param x Predictor column name.
#' @param y Target column name.
#' @param models Model names passed to AutoNLS.
#' @param n_starts Number of starts per strategy.
#' @param seed Deterministic seed.
#' @return A list of data.table validation outputs.
#' @export
validate_autonls_fit_strategies <- function(
  data,
  x,
  y,
  models = c("Linear", "Hill", "Logistic", "ExponentialDecay", "PowerCurve"),
  n_starts = 5,
  seed = 42
) {
  DT <- data.table::as.data.table(data)
  strategies <- data.table::data.table(
    strategy = c("raw_original_scale", "scaled_xy", "log_log1p_transformed_starts", "family_specific_transformed_initialization"),
    scale_x = c(FALSE, TRUE, TRUE, TRUE),
    scale_y = c(FALSE, TRUE, TRUE, TRUE),
    start_strategy = c("family", "family", "log_transformed", "family_transformed")
  )

  fits <- list()
  metric_rows <- list()
  selected_rows <- list()
  warning_rows <- list()
  prediction_rows <- list()

  for (i in seq_len(nrow(strategies))) {
    st <- strategies[i]
    fit <- tryCatch(
      AutoNLS(
        data = DT,
        x = x,
        y = y,
        models = models,
        n_starts = n_starts,
        seed = seed,
        scale_x = st$scale_x,
        scale_y = st$scale_y,
        start_strategy = st$start_strategy
      ),
      error = function(e) e
    )
    fits[[st$strategy]] <- fit
    if (inherits(fit, "error")) {
      metric_rows[[st$strategy]] <- data.table::data.table(
        strategy = st$strategy,
        model_name = NA_character_,
        converged = FALSE,
        rmse = NA_real_,
        mae = NA_real_,
        r_squared = NA_real_,
        overall_score = Inf,
        failure_reason = fit$message
      )
      selected_rows[[st$strategy]] <- data.table::data.table(strategy = st$strategy, selected_model = NA_character_, selected_family = NA_character_, selected_status = NA_character_)
      warning_rows[[st$strategy]] <- data.table::data.table(strategy = st$strategy, model_name = NA_character_, code = "strategy_error", severity = "high", warning = fit$message)
      next
    }

    metrics <- data.table::copy(fit$ranking_summary)
    metrics[, strategy := st$strategy]
    metric_rows[[st$strategy]] <- metrics[, .(strategy, model_name, converged, rmse, mae, r_squared, overall_score, failure_reason = failure_explanation)]

    best <- fit$best_model()
    selected_rows[[st$strategy]] <- data.table::data.table(
      strategy = st$strategy,
      selected_model = if (is.null(best)) NA_character_ else best$model_name,
      selected_family = if (is.null(best)) NA_character_ else best$spec$family,
      selected_status = if (is.null(best)) NA_character_ else best$spec$status
    )

    warn <- data.table::copy(fit$fit_warnings)
    warn[, strategy := st$strategy]
    warning_rows[[st$strategy]] <- warn[, .(strategy, model_name, code, severity, warning)]

    probe <- data.table::data.table(x_probe = stats::quantile(DT[[x]], probs = c(0.1, 0.5, 0.9), na.rm = TRUE))
    data.table::setnames(probe, "x_probe", x)
    pred <- tryCatch(fit$score(probe, model = "best"), error = function(e) data.table::data.table())
    prediction_rows[[st$strategy]] <- data.table::data.table(
      strategy = st$strategy,
      predictions_returned = nrow(pred) == nrow(probe),
      x_original_scale = nrow(pred) > 0 && all(abs(pred$x - probe[[x]]) < 1e-8),
      y_original_scale = nrow(pred) > 0 && all(is.finite(pred$prediction)) && max(abs(pred$prediction), na.rm = TRUE) > 1,
      prediction_min = if (nrow(pred) > 0) min(pred$prediction, na.rm = TRUE) else NA_real_,
      prediction_max = if (nrow(pred) > 0) max(pred$prediction, na.rm = TRUE) else NA_real_
    )
  }

  metrics_by_strategy <- data.table::rbindlist(metric_rows, fill = TRUE)
  convergence_rate <- metrics_by_strategy[, .(
    models_attempted = .N,
    models_converged = sum(converged == TRUE, na.rm = TRUE),
    convergence_rate = mean(converged == TRUE, na.rm = TRUE)
  ), by = strategy]

  list(
    convergence_rate_by_strategy = convergence_rate,
    metrics_by_strategy = metrics_by_strategy,
    selected_model_by_strategy = data.table::rbindlist(selected_rows, fill = TRUE),
    warnings_by_strategy = data.table::rbindlist(warning_rows, fill = TRUE),
    original_scale_prediction_check = data.table::rbindlist(prediction_rows, fill = TRUE)
  )
}

#' QA checks for the AutoNLS model registry
#'
#' @return TRUE invisibly when checks pass.
#' @export
qa_autonls_model_registry <- function() {
  registry <- nls_model_registry()
  required <- c("model_name", "family", "description", "formula", "parameter_names", "start_params", "lower_bounds", "upper_bounds", "domain", "model_function", "tags", "monotonic", "supports_derivative", "status")
  x_grid <- seq(0.05, 1, length.out = 20)

  stopifnot(is.list(registry), length(registry) >= 30)
  for (nm in names(registry)) {
    spec <- registry[[nm]]
    missing <- setdiff(required, names(spec))
    if (length(missing) > 0) stop("Missing registry fields for ", nm, ": ", paste(missing, collapse = ", "))
    stopifnot(identical(spec$model_name, nm))
    stopifnot(all(spec$parameter_names %in% names(spec$start_params)))
    stopifnot(identical(names(spec$start_params), spec$parameter_names))
    stopifnot(identical(names(spec$lower_bounds), spec$parameter_names))
    stopifnot(identical(names(spec$upper_bounds), spec$parameter_names))
    y <- spec$model_function(x_grid, spec$start_params)
    stopifnot(is.numeric(y), length(y) == length(x_grid), !all(!is.finite(y)))
    if (isTRUE(spec$supports_derivative)) {
      d <- spec$derivative_function(x_grid, spec$start_params)
      stopifnot(is.numeric(d), length(d) == length(x_grid), !all(!is.finite(d)))
    }
  }

  models <- list_nls_models(model_status = "all")
  stopifnot(data.table::is.data.table(models), all(c("model_name", "family", "description", "formula", "n_params", "supports_derivative", "status", "tags") %in% names(models)))
  invisible(TRUE)
}

#' QA checks for family-aware initialization
#'
#' @return TRUE invisibly when checks pass.
#' @export
qa_autonls_family_initialization <- function() {
  set.seed(42)
  DT <- data.table::data.table(Spend = seq(1, 100, length.out = 80))
  DT[, Sales := 10 + 150 * Spend^1.2 / (30^1.2 + Spend^1.2) + stats::rnorm(.N, sd = 1)]
  fit <- AutoNLS(
    data = DT,
    x = "Spend",
    y = "Sales",
    models = c("Linear", "Hill", "Logistic", "ExponentialDecay", "PowerCurve"),
    n_starts = 5,
    seed = 42
  )
  stopifnot(data.table::is.data.table(fit$diagnostics))
  stopifnot(all(fit$diagnostics[, .N, by = model_name]$N == 5))
  first_starts_ok <- fit$diagnostics[start_id == 1, all(is.finite(objective) | status %in% c("failed", "error"))]
  stopifnot(isTRUE(first_starts_ok))
  invisible(TRUE)
}

#' QA checks for domain diagnostics
#'
#' @return TRUE invisibly when checks pass.
#' @export
qa_autonls_domain_checks <- function() {
  DT <- data.table::data.table(Spend = c(-2, -1, 0, 1, 2, 3), Sales = c(1, 2, 3, 4, 5, 6))
  fit <- AutoNLS(
    data = DT,
    x = "Spend",
    y = "Sales",
    models = c("PowerCurve", "Hill"),
    n_starts = 2
  )
  stopifnot(data.table::is.data.table(fit$domain_diagnostics))
  stopifnot(nrow(fit$domain_diagnostics[severity %in% c("medium", "high")]) > 0)
  stopifnot(data.table::is.data.table(fit$model_suitability))
  invisible(TRUE)
}

#' QA checks for deterministic model ranking
#'
#' @return TRUE invisibly when checks pass.
#' @export
qa_autonls_model_ranking <- function() {
  set.seed(42)
  DT <- data.table::data.table(Spend = seq(1, 80, length.out = 80))
  DT[, Sales := 3 + 2 * Spend + stats::rnorm(.N, sd = 1)]
  fit1 <- AutoNLS(DT, x = "Spend", y = "Sales", models = c("Linear", "Hill", "PowerCurve"), n_starts = 4, seed = 42)
  fit2 <- AutoNLS(DT, x = "Spend", y = "Sales", models = c("Linear", "Hill", "PowerCurve"), n_starts = 4, seed = 42)
  stopifnot(data.table::is.data.table(fit1$ranking_summary))
  stopifnot(identical(fit1$ranking_summary$model_name, fit2$ranking_summary$model_name))
  stopifnot(identical(fit1$best_model()$model_name, fit2$best_model()$model_name))
  invisible(TRUE)
}

#' QA checks for validation split metrics
#'
#' @return TRUE invisibly when checks pass.
#' @export
qa_autonls_validation <- function() {
  set.seed(42)
  DT <- data.table::data.table(Spend = seq(1, 100, length.out = 100))
  DT[, Sales := 4 + 2 * Spend + stats::rnorm(.N, sd = 1)]
  fit <- AutoNLS(
    data = DT,
    x = "Spend",
    y = "Sales",
    models = c("Linear", "PowerCurve"),
    validation_fraction = 0.2,
    validation_seed = 42,
    n_starts = 3
  )
  stopifnot(length(fit$validation_index) == 20)
  stopifnot(data.table::is.data.table(fit$validation_metrics))
  stopifnot(nrow(fit$validation_metrics) > 0)
  stopifnot(all(c("validation_rmse", "overfit_indicator", "overfit_ratio") %in% names(fit$validation_metrics)))
  invisible(TRUE)
}

#' QA checks for parameter stability artifacts
#'
#' @return TRUE invisibly when checks pass.
#' @export
qa_autonls_parameter_stability <- function() {
  set.seed(42)
  DT <- data.table::data.table(Spend = seq(1, 80, length.out = 80))
  DT[, Sales := 5 + 120 * Spend / (25 + Spend) + stats::rnorm(.N, sd = 1)]
  fit <- AutoNLS(DT, x = "Spend", y = "Sales", models = c("Hill", "Linear"), n_starts = 5, seed = 42)
  stopifnot(data.table::is.data.table(fit$parameter_stability))
  stopifnot(all(c("parameter", "coefficient_of_variation", "objective_spread", "number_converged") %in% names(fit$parameter_stability)))
  stopifnot(nrow(fit$parameter_stability) > 0)
  invisible(TRUE)
}

#' QA checks for optional interval estimation
#'
#' @return TRUE invisibly when checks pass.
#' @export
qa_autonls_intervals <- function() {
  set.seed(42)
  DT <- data.table::data.table(Spend = seq(1, 80, length.out = 80))
  DT[, Sales := 3 + 2 * Spend + stats::rnorm(.N, sd = 1)]
  no_int <- AutoNLS(DT, x = "Spend", y = "Sales", models = c("Linear", "Hill"), n_starts = 3)
  stopifnot(data.table::is.data.table(no_int$prediction_intervals), nrow(no_int$prediction_intervals) == 0)
  stopifnot(data.table::is.data.table(no_int$interval_diagnostics))

  with_int <- AutoNLS(
    DT,
    x = "Spend",
    y = "Sales",
    models = c("Linear", "Hill"),
    n_starts = 3,
    interval_method = "residual_bootstrap",
    interval_n = 20,
    interval_seed = 42
  )
  stopifnot(data.table::is.data.table(with_int$prediction_intervals))
  stopifnot(all(c("lower", "upper", "interval_method") %in% names(with_int$prediction_intervals)))
  stopifnot(nrow(with_int$interval_diagnostics[status == "ok"]) >= 1)
  invisible(TRUE)
}

#' QA checks for fit warnings and recommendations
#'
#' @return TRUE invisibly when checks pass.
#' @export
qa_autonls_fit_warnings <- function() {
  DT <- data.table::data.table(Spend = rep(1, 8), Sales = rep(5, 8))
  fit <- AutoNLS(DT, x = "Spend", y = "Sales", models = c("Linear", "Hill"), n_starts = 2)
  stopifnot(data.table::is.data.table(fit$fit_warnings))
  stopifnot(data.table::is.data.table(fit$fit_recommendations))
  stopifnot(nrow(fit$fit_warnings) > 0)
  stopifnot(nrow(fit$fit_recommendations) > 0)
  invisible(TRUE)
}

#' QA checks for AutoQuant-consumable curve artifact contract
#'
#' @return TRUE invisibly when checks pass.
#' @export
qa_autonls_curve_artifact_contract <- function() {
  set.seed(42)
  DT <- data.table::data.table(Spend = seq(1, 60, length.out = 60))
  DT[, Sales := 4 + 100 * Spend / (25 + Spend) + stats::rnorm(.N, sd = 1)]
  fit <- AutoNLS(
    DT,
    x = "Spend",
    y = "Sales",
    models = c("Linear", "Hill", "PowerCurve"),
    n_starts = 4,
    interval_method = "residual_bootstrap",
    interval_n = 15
  )
  artifacts <- generate_autonls_artifacts(fit)
  stopifnot(all(c("curve_values", "curve_diagnostics", "selected_model", "fit_warnings", "fit_recommendations") %in% names(artifacts)))
  stopifnot(all(c("model_name", "family", "status", "problem_type", "curve_type", "supports_derivative", "supports_elasticity", "original_scale", "x", "y_hat", "derivative", "elasticity", "lower", "upper", "is_best_model", "x_original_scale", "y_original_scale") %in% names(artifacts$curve_values)))
  stopifnot(all(c("model_name", "family", "status", "problem_type", "curve_type", "selected", "rank", "overall_score", "rmse", "mae", "r_squared", "convergence_quality", "parameter_stability_flag", "domain_status", "warning_count") %in% names(artifacts$curve_diagnostics)))
  stopifnot(is.list(artifacts$selected_model), !is.null(artifacts$selected_model$model_name))
  stopifnot(data.table::is.data.table(artifacts$model_confidence), nrow(artifacts$model_confidence) == 1)
  invisible(TRUE)
}

#' QA checks for model confidence
#'
#' @return TRUE invisibly when checks pass.
#' @export
qa_autonls_model_confidence <- function() {
  set.seed(42)
  DT <- data.table::data.table(Spend = seq(1, 80, length.out = 80))
  DT[, Sales := 5 + 120 * Spend / (30 + Spend) + stats::rnorm(.N, sd = 1)]
  fit <- AutoNLS(DT, x = "Spend", y = "Sales", models = c("Linear", "Hill", "PowerCurve"), n_starts = 4)
  conf <- fit$artifacts()$model_confidence
  stopifnot(data.table::is.data.table(conf), nrow(conf) == 1)
  stopifnot(all(c("confidence_score", "confidence_level", "supporting_evidence", "warning_count", "stability_score", "validation_gap", "top_model_margin") %in% names(conf)))
  stopifnot(conf$confidence_level %in% c("Very High", "High", "Moderate", "Low", "Very Low"))
  invisible(TRUE)
}

#' QA checks for deterministic ranking explanations and loss reasons
#'
#' @return TRUE invisibly when checks pass.
#' @export
qa_autonls_ranking_explanations <- function() {
  set.seed(42)
  DT <- data.table::data.table(Spend = seq(1, 80, length.out = 80))
  DT[, Sales := 2 + 3 * Spend + stats::rnorm(.N, sd = 1)]
  fit <- AutoNLS(DT, x = "Spend", y = "Sales", models = c("Linear", "Hill", "Logistic", "PowerCurve"), n_starts = 4)
  rs <- fit$ranking_summary
  stopifnot(all(c("ranking_position", "reason_code", "explanation", "rmse_contribution", "mae_contribution", "validation_contribution", "convergence_contribution", "stability_contribution", "complexity_penalty", "experimental_penalty", "suitability_bonus") %in% names(rs)))
  stopifnot(all(nzchar(rs[is.finite(overall_score)]$explanation)))
  stopifnot(all(nzchar(rs[ranking_position > 1 & is.finite(overall_score)]$reason_code)))
  invisible(TRUE)
}

#' QA checks interval estimation contract
#'
#' @return TRUE invisibly when checks pass.
#' @export
qa_autonls_interval_estimation <- function() {
  set.seed(42)
  DT <- data.table::data.table(Spend = seq(1, 70, length.out = 70))
  DT[, Sales := 3 + 2 * Spend + stats::rnorm(.N, sd = 1)]
  fit <- AutoNLS(DT, x = "Spend", y = "Sales", models = c("Linear", "Hill"), n_starts = 3, interval_method = "residual_bootstrap", interval_n = 20)
  stopifnot(data.table::is.data.table(fit$prediction_intervals))
  stopifnot(all(c("lower", "upper", "interval_status", "interval_warning") %in% names(fit$prediction_intervals)))
  stopifnot(data.table::is.data.table(fit$interval_diagnostics))
  invisible(TRUE)
}

#' QA checks for realistic synthetic curve families
#'
#' @return TRUE invisibly when checks pass.
#' @export
qa_autonls_realistic_curve_families <- function() {
  set.seed(42)
  cases <- list(
    linear = data.table::data.table(x = seq(1, 80, length.out = 80))[, y := 2 + 3 * x + stats::rnorm(.N, sd = 1)][],
    saturation = data.table::data.table(x = seq(1, 100, length.out = 90))[, y := 5 + 120 * x / (30 + x) + stats::rnorm(.N, sd = 1)][],
    sigmoid = data.table::data.table(x = seq(1, 100, length.out = 90))[, y := 100 / (1 + exp(-0.12 * (x - 45))) + stats::rnorm(.N, sd = 1)][],
    decay = data.table::data.table(x = seq(1, 100, length.out = 90))[, y := 80 * exp(-0.03 * x) + stats::rnorm(.N, sd = 1)][],
    power = data.table::data.table(x = seq(1, 80, length.out = 80))[, y := 2 * x^1.4 + stats::rnorm(.N, sd = 2)][],
    flat = data.table::data.table(x = seq(1, 30, length.out = 30), y = rep(5, 30)),
    sparse = data.table::data.table(x = c(1, 2, 3, 4), y = c(2, 3, 3.5, 4)),
    invalid = data.table::data.table(x = c(-2, -1, 0, 1, 2, 3), y = c(5, 4, 3, 2, 1, 0))
  )
  fits <- lapply(cases, function(DT) {
    AutoNLS(DT, x = "x", y = "y", models = c("Linear", "Hill", "Logistic", "ExponentialDecay", "PowerCurve"), n_starts = 4)
  })
  stopifnot(!is.null(fits$linear$best_model()))
  stopifnot(!is.null(fits$saturation$best_model()))
  stopifnot(data.table::is.data.table(fits$flat$fit_warnings))
  stopifnot(nrow(fits$invalid$domain_diagnostics[severity %in% c("medium", "high")]) > 0)
  stopifnot(data.table::is.data.table(fits$sparse$model_suitability))
  invisible(TRUE)
}

#' QA checks for experimental model safety
#'
#' @return TRUE invisibly when checks pass.
#' @export
qa_autonls_experimental_model_safety <- function() {
  experimental <- list_nls_models(model_status = "experimental")
  stopifnot(data.table::is.data.table(experimental), nrow(experimental) > 0)
  set.seed(42)
  DT <- data.table::data.table(Spend = seq(1, 60, length.out = 60))
  DT[, Sales := 4 + 80 * Spend / (20 + Spend) + stats::rnorm(.N, sd = 1)]
  stable <- AutoNLS(DT, x = "Spend", y = "Sales", models = "all", model_status = "stable", n_starts = 2)
  exp_fit <- AutoNLS(DT, x = "Spend", y = "Sales", models = head(experimental$model_name, 3), model_status = "experimental", n_starts = 2)
  all_fit <- AutoNLS(DT, x = "Spend", y = "Sales", models = c("Linear", head(experimental$model_name, 2)), model_status = "all", n_starts = 2)
  stopifnot(!is.null(stable$best_model()))
  stopifnot(data.table::is.data.table(exp_fit$diagnostics))
  stopifnot(data.table::is.data.table(all_fit$diagnostics))
  invisible(TRUE)
}

#' QA checks raw-scale, scaled, and transformed-start strategy validation
#'
#' @return TRUE invisibly when checks pass.
#' @export
qa_autonls_raw_scale_strategy_validation <- function() {
  set.seed(42)
  DT <- data.table::data.table(Spend = seq(1, 1000, length.out = 100))
  DT[, Sales := 25 + 350 * Spend^1.25 / (250^1.25 + Spend^1.25) + stats::rnorm(.N, sd = 5)]
  out <- validate_autonls_fit_strategies(
    data = DT,
    x = "Spend",
    y = "Sales",
    models = c("Linear", "Hill", "Logistic", "PowerCurve"),
    n_starts = 4,
    seed = 42
  )
  stopifnot(is.list(out))
  stopifnot(all(c("convergence_rate_by_strategy", "metrics_by_strategy", "selected_model_by_strategy", "warnings_by_strategy", "original_scale_prediction_check") %in% names(out)))
  stopifnot(data.table::is.data.table(out$convergence_rate_by_strategy))
  stopifnot(nrow(out$convergence_rate_by_strategy) == 4)
  stopifnot(all(out$original_scale_prediction_check$predictions_returned))
  stopifnot(all(out$original_scale_prediction_check$x_original_scale))
  stopifnot(all(out$original_scale_prediction_check$y_original_scale))
  invisible(TRUE)
}

#' QA checks for AutoNLS deterministic multi-start fitting
#'
#' @return TRUE invisibly when checks pass.
#' @export
qa_autonls_optimizer_multistart <- function() {
  set.seed(42)
  DT <- data.table::data.table(Spend = seq(1, 100, length.out = 100))
  DT[, Sales := 8 + 150 * Spend^1.3 / (35^1.3 + Spend^1.3) + stats::rnorm(.N, sd = 2)]

  fit <- AutoNLS(
    data = DT,
    x = "Spend",
    y = "Sales",
    models = c("Linear", "Hill", "Logistic", "Gompertz", "ExponentialDecay", "PowerCurve"),
    n_starts = 6,
    seed = 42,
    model_status = "stable"
  )

  stopifnot(data.table::is.data.table(fit$diagnostics))
  stopifnot(nrow(fit$diagnostics) == 6 * 6)
  stopifnot(all(c("model_name", "start_id", "status", "objective", "convergence_code", "message", "elapsed_time") %in% names(fit$diagnostics)))
  stopifnot(!is.null(fit$best_model()))
  stopifnot(all(c("best_start_id", "n_starts_attempted", "n_starts_converged") %in% names(fit$metrics())))
  stopifnot(data.table::is.data.table(fit$ranking_summary))
  invisible(TRUE)
}

#' QA checks for prediction, derivative, and elasticity
#'
#' @return TRUE invisibly when checks pass.
#' @export
qa_autonls_prediction_derivative_elasticity <- function() {
  set.seed(42)
  DT <- data.table::data.table(Spend = seq(1, 80, length.out = 80))
  DT[, Sales := 5 + 2.5 * Spend + stats::rnorm(.N, sd = 1)]
  fit <- AutoNLS(
    data = DT,
    x = "Spend",
    y = "Sales",
    models = c("Linear", "Hill", "Logistic", "Gompertz", "ExponentialDecay", "PowerCurve"),
    n_starts = 4,
    seed = 42,
    model_status = "stable"
  )

  new_data <- data.table::data.table(Spend = c(0, 10, 20, 30))
  pred <- fit$predict(new_data, model = "all")
  score <- fit$score(new_data, model = "all")
  deriv <- fit$derivative(new_data, model = "all")
  elas <- fit$elasticity(new_data, model = "all")
  artifacts <- generate_autonls_artifacts(fit)

  stopifnot(data.table::is.data.table(pred), data.table::is.data.table(score), data.table::is.data.table(deriv), data.table::is.data.table(elas))
  stopifnot(all(c("row_id", "model_name", "prediction") %in% names(pred)))
  stopifnot(all(c("row_id", "model_name", "derivative") %in% names(deriv)))
  stopifnot(all(c("row_id", "model_name", "elasticity") %in% names(elas)))
  stopifnot(all(pred$row_id %in% seq_len(nrow(new_data))))
  stopifnot(all(c("model_registry_table", "model_metrics", "fitted_values", "residual_summary", "convergence_diagnostics", "domain_diagnostics", "model_suitability", "parameter_stability", "validation_metrics", "ranking_summary", "prediction_intervals", "interval_diagnostics", "fit_warnings", "fit_recommendations", "curve_values", "curve_diagnostics", "selected_model", "prediction_curve", "derivative_curve", "elasticity_curve", "best_model_summary") %in% names(artifacts)))
  invisible(TRUE)
}

#' QA checks for the AutoNLS vNext Phase 2 skeleton
#'
#' @return TRUE invisibly when checks pass.
#' @export
qa_autonls_vnext <- function() {
  qa_autonls_model_registry()
  qa_autonls_family_initialization()
  qa_autonls_domain_checks()
  qa_autonls_model_ranking()
  qa_autonls_validation()
  qa_autonls_parameter_stability()
  qa_autonls_intervals()
  qa_autonls_interval_estimation()
  qa_autonls_fit_warnings()
  qa_autonls_curve_artifact_contract()
  qa_autonls_model_confidence()
  qa_autonls_ranking_explanations()
  qa_autonls_realistic_curve_families()
  qa_autonls_experimental_model_safety()
  qa_autonls_raw_scale_strategy_validation()
  qa_autonls_optimizer_multistart()
  qa_autonls_prediction_derivative_elasticity()
  stopifnot(exists("ModelFitter"), exists("ModelEvaluator"), exists("ModelScorer"))
  invisible(TRUE)
}
