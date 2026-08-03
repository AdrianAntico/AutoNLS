# Bid Simulator + Historical Actuals Calibration Prototype
# R edition: data.table + AutoQuant + AutoNLS
#
# Creates:
# 1. bid_simulator.csv
# 2. historical_actuals.csv
# 3. surface_summary.csv
# 4. modeling_episodes.csv
# 5. model_comparison.csv
# 6. heldout_predictions.csv
# 7. corrected_surfaces.csv
#
# Modeling design:
# - Raw Google simulator estimate
# - Rolling bias calibration
# - Contextual historical retrieval
# - AutoQuant supervised level calibration
# - AutoNLS residual-shape calibration across tROAS
# - Blended point prediction
# - Full corrected held-out simulator surfaces
#
# Identification constraint:
# Only one tROAS point is realized per portfolio/day. Full-surface correction
# therefore requires an explicit assumption about how point-level historical
# errors transfer across the unobserved counterfactual curve.

suppressPackageStartupMessages({
  library(data.table)
  library(AutoQuant)
  library(AutoNLS)
})

EPS <- 1e-9
TARGETS <- c("cost", "events", "value")


# ------------------------------------------------------------------------------
# Configuration
# ------------------------------------------------------------------------------

default_config <- function() {
  list(
    seed = 7L,
    n_portfolios = 6L,
    n_days = 520L,
    start_date = as.IDate("2025-01-01"),
    train_fraction = 0.75,
    neighbor_count = 30L,
    output_dir = ".",
    autoquant_iterations = 350L,
    autoquant_depth = 5L,
    autoquant_learning_rate = 0.045,
    autoquant_l2 = 0.5,
    autoquant_thread_count = 4,
    autonls_models = c(
      "Linear",
      "Hill",
      "Logistic",
      "Gompertz",
      "PowerCurve"
    ),
    autonls_n_starts = 25L,
    autonls_validation_fraction = 0.20,
    autonls_shape_shrinkage = 0.35,
    contextual_recency_days = 180,
    min_portfolio_shape_rows = 40L
  )
}


# ------------------------------------------------------------------------------
# General helpers
# ------------------------------------------------------------------------------

safe_divide <- function(numerator, denominator, eps = EPS) {
  numerator / pmax(abs(denominator), eps)
}

safe_mape <- function(actual, predicted, eps = EPS) {
  mean(abs(actual - predicted) / pmax(abs(actual), eps), na.rm = TRUE)
}

rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2, na.rm = TRUE))
}

clip <- function(x, lower, upper) {
  pmin(pmax(x, lower), upper)
}

nearest_grid_value <- function(value, grid) {
  grid[which.min(abs(grid - clip(value, min(grid), max(grid))))]
}

weighted_mean_safe <- function(x, w) {
  keep <- is.finite(x) & is.finite(w) & w > 0
  if (!any(keep)) {
    return(0)
  }
  sum(x[keep] * w[keep]) / sum(w[keep])
}

detect_prediction_column <- function(scored, target_col = NULL) {
  candidates <- c(
    if (!is.null(target_col)) paste0("Predict_", target_col) else character(),
    "Predict_target",
    "prediction",
    "Prediction",
    "predicted",
    "Predicted"
  )
  present <- candidates[candidates %chin% names(scored)]
  if (length(present) > 0L) {
    return(present[[1L]])
  }

  heuristic <- grep(
    "^(predict|prediction|predicted)",
    names(scored),
    ignore.case = TRUE,
    value = TRUE
  )
  if (length(heuristic) == 1L) {
    return(heuristic)
  }

  stop(
    "Unable to identify AutoQuant prediction column. Available columns: ",
    paste(names(scored), collapse = ", ")
  )
}

extract_autoquant_scored_data <- function(score_result) {
  if (is.data.table(score_result)) {
    return(copy(score_result))
  }
  if (is.data.frame(score_result)) {
    return(as.data.table(score_result))
  }

  candidate_names <- c(
    "scored_data",
    "scored",
    "predictions",
    "prediction_artifact",
    "data"
  )
  for (nm in candidate_names) {
    if (!is.null(score_result[[nm]])) {
      candidate <- score_result[[nm]]
      if (is.data.frame(candidate)) {
        return(as.data.table(candidate))
      }
    }
  }

  stop("AutoQuant score result did not expose a recognizable scored data table.")
}


# ------------------------------------------------------------------------------
# Synthetic data
# ------------------------------------------------------------------------------

generate_synthetic_data <- function(cfg) {
  set.seed(cfg$seed)

  dates <- seq(
    from = cfg$start_date,
    by = 1L,
    length.out = cfg$n_days
  )
  portfolios <- sprintf("P%02d", seq_len(cfg$n_portfolios))
  troas_grid <- round(seq(0.8, 6.0, length.out = 27L), 2)

  portfolio_state <- data.table(
    portfolio_id = portfolios,
    base_cost = runif(cfg$n_portfolios, 8000, 28000),
    base_events = runif(cfg$n_portfolios, 180, 900),
    base_value_per_event = runif(cfg$n_portfolios, 80, 280),
    cost_elasticity = runif(cfg$n_portfolios, 0.25, 0.52),
    event_elasticity = runif(cfg$n_portfolios, 0.15, 0.38),
    google_level_bias = rnorm(cfg$n_portfolios, 0, 0.035),
    google_shape_bias = rnorm(cfg$n_portfolios, 0, 0.025)
  )
  setkey(portfolio_state, portfolio_id)

  previous_surface <- vector("list", length(portfolios))
  names(previous_surface) <- portfolios

  repeated_days <- integer(length(portfolios))
  names(repeated_days) <- portfolios

  sim_chunks <- vector("list", cfg$n_days * cfg$n_portfolios)
  actual_chunks <- vector("list", cfg$n_days * cfg$n_portfolios)
  chunk_id <- 0L

  for (day_idx in seq_along(dates)) {
    current_date <- dates[[day_idx]]
    zero_based_day <- day_idx - 1L

    common_demand <- (
      1 +
        0.13 * sin(2 * pi * zero_based_day / 365.25) +
        0.05 * sin(2 * pi * zero_based_day / 7) +
        rnorm(1L, 0, 0.025)
    )
    common_competition <- (
      1 +
        0.10 * cos(2 * pi * zero_based_day / 91) +
        rnorm(1L, 0, 0.025)
    )
    common_quality <- (
      1 +
        0.09 * sin(2 * pi * zero_based_day / 127 + 0.6) +
        rnorm(1L, 0, 0.02)
    )

    for (portfolio in portfolios) {
      chunk_id <- chunk_id + 1L
      state <- portfolio_state[portfolio]

      demand <- common_demand * (1 + rnorm(1L, 0, 0.035))
      competition <- common_competition * (1 + rnorm(1L, 0, 0.03))
      quality <- common_quality * (1 + rnorm(1L, 0, 0.03))

      freeze_surface <- (
        !is.null(previous_surface[[portfolio]]) &&
          runif(1L) < 0.24
      )

      if (freeze_surface) {
        prior <- previous_surface[[portfolio]]
        google_demand <- prior$google_demand
        google_competition <- prior$google_competition
        google_quality <- prior$google_quality
        surface_id <- prior$surface_id
        repeated_days[[portfolio]] <- repeated_days[[portfolio]] + 1L
      } else {
        google_demand <- demand * (
          1 +
            state$google_level_bias +
            rnorm(1L, 0, 0.025)
        )
        google_competition <- competition * (1 + rnorm(1L, 0, 0.025))
        google_quality <- quality * (1 + rnorm(1L, 0, 0.025))
        surface_id <- sprintf(
          "%s_%s",
          portfolio,
          format(current_date, "%Y%m%d")
        )
        previous_surface[[portfolio]] <- list(
          google_demand = google_demand,
          google_competition = google_competition,
          google_quality = google_quality,
          surface_id = surface_id
        )
        repeated_days[[portfolio]] <- 0L
      }

      policy_center <- (
        2.6 +
          0.45 * (quality - 1) -
          0.35 * (competition - 1) +
          0.22 * sin(2 * pi * zero_based_day / 60)
      )

      chosen_target_roas <- nearest_grid_value(
        rnorm(1L, policy_center, 0.55),
        troas_grid
      )

      surface <- data.table(target_roas = troas_grid)
      surface[, centered := target_roas - 0.8]

      surface[, google_cost := (
        state$base_cost *
          google_demand *
          exp(
            -(
              state$cost_elasticity +
                state$google_shape_bias
            ) * centered
          ) /
          google_competition^0.28
      )]

      surface[, google_events := (
        state$base_events *
          google_demand *
          google_quality *
          exp(
            -(
              state$event_elasticity +
                0.5 * state$google_shape_bias
            ) * centered
          ) /
          google_competition^0.12
      )]

      surface[, google_value := (
        google_events *
          state$base_value_per_event *
          (1 + 0.035 * centered)
      )]

      surface[, cost_log_correction_true := (
        0.055 * (competition - 1) -
          0.045 * (demand - 1) +
          0.022 * centered +
          0.016 * centered^2 / 10 +
          0.035 * sin(2 * pi * zero_based_day / 67) +
          0.025 * repeated_days[[portfolio]]
      )]

      surface[, event_log_correction_true := (
        0.075 * (quality - 1) +
          0.045 * (demand - 1) -
          0.028 * centered -
          0.012 * centered^2 / 10 -
          0.025 * cos(2 * pi * zero_based_day / 83) -
          0.012 * repeated_days[[portfolio]]
      )]

      surface[, value_log_correction_true := (
        event_log_correction_true +
          0.025 * (quality - 1) +
          0.012 * centered
      )]

      surface[, true_cost := google_cost * exp(cost_log_correction_true)]
      surface[, true_events := google_events * exp(event_log_correction_true)]
      surface[, true_value := google_value * exp(value_log_correction_true)]

      sim_chunks[[chunk_id]] <- surface[, .(
        portfolio_id = portfolio,
        date = current_date,
        surface_id = surface_id,
        surface_repeat_age = repeated_days[[portfolio]],
        target_roas,
        google_cost,
        google_events,
        google_value,
        google_implied_roas = safe_divide(google_value, google_cost),
        google_implied_cpa = safe_divide(google_cost, google_events),
        demand_proxy = demand + rnorm(.N, 0, 0.02),
        competition_proxy = competition + rnorm(.N, 0, 0.02),
        quality_proxy = quality + rnorm(.N, 0, 0.02)
      )]

      realized <- surface[target_roas == chosen_target_roas]
      if (nrow(realized) != 1L) {
        stop("Chosen tROAS did not match exactly one simulator row.")
      }

      actual_cost <- realized$true_cost * rlnorm(1L, 0, 0.055)
      actual_events <- realized$true_events * rlnorm(1L, 0, 0.075)
      actual_value <- realized$true_value * rlnorm(1L, 0, 0.07)

      actual_chunks[[chunk_id]] <- data.table(
        portfolio_id = portfolio,
        date = current_date,
        chosen_target_roas = chosen_target_roas,
        actual_cost = actual_cost,
        actual_events = actual_events,
        actual_value = actual_value,
        actual_roas = safe_divide(actual_value, actual_cost),
        actual_cpa = safe_divide(actual_cost, actual_events),
        demand_proxy = demand + rnorm(1L, 0, 0.02),
        competition_proxy = competition + rnorm(1L, 0, 0.02),
        quality_proxy = quality + rnorm(1L, 0, 0.02)
      )
    }
  }

  bid_simulator <- rbindlist(sim_chunks, use.names = TRUE)
  historical_actuals <- rbindlist(actual_chunks, use.names = TRUE)

  setkey(
    bid_simulator,
    portfolio_id,
    date,
    target_roas
  )
  setkey(
    historical_actuals,
    portfolio_id,
    date,
    chosen_target_roas
  )

  list(
    bid_simulator = bid_simulator,
    historical_actuals = historical_actuals
  )
}


# ------------------------------------------------------------------------------
# Surface summaries
# ------------------------------------------------------------------------------

quadratic_summary <- function(x, y) {
  fit <- lm.fit(
    x = cbind(1, x, x^2),
    y = y
  )
  coef <- fit$coefficients
  coef[!is.finite(coef)] <- 0

  list(
    mean = mean(y),
    sd = sd(y),
    slope = unname(coef[[2L]]),
    curvature = unname(coef[[3L]])
  )
}

build_surface_summary <- function(bid_simulator) {
  bid_simulator[
    order(target_roas),
    {
      cost_fit <- quadratic_summary(target_roas, google_cost)
      event_fit <- quadratic_summary(target_roas, google_events)
      value_fit <- quadratic_summary(target_roas, google_value)

      implied_roas <- safe_divide(google_value, google_cost)
      implied_cpa <- safe_divide(google_cost, google_events)

      .(
        surface_id = first(surface_id),
        surface_repeat_age = first(surface_repeat_age),
        demand_proxy_sim = mean(demand_proxy),
        competition_proxy_sim = mean(competition_proxy),
        quality_proxy_sim = mean(quality_proxy),
        surface_cost_mean = cost_fit$mean,
        surface_cost_sd = cost_fit$sd,
        surface_cost_slope = cost_fit$slope,
        surface_cost_curvature = cost_fit$curvature,
        surface_event_mean = event_fit$mean,
        surface_event_sd = event_fit$sd,
        surface_event_slope = event_fit$slope,
        surface_event_curvature = event_fit$curvature,
        surface_value_mean = value_fit$mean,
        surface_value_slope = value_fit$slope,
        surface_value_curvature = value_fit$curvature,
        surface_implied_roas_mean = mean(implied_roas),
        surface_implied_roas_sd = sd(implied_roas),
        surface_cpa_mean = mean(implied_cpa),
        surface_cpa_sd = sd(implied_cpa),
        surface_troas_min = min(target_roas),
        surface_troas_max = max(target_roas),
        surface_grid_points = .N
      )
    },
    by = .(portfolio_id, date)
  ]
}


# ------------------------------------------------------------------------------
# Episode table and leakage-safe historical features
# ------------------------------------------------------------------------------

shifted_roll_mean <- function(x, n, min_obs) {
  shifted <- shift(x, 1L)

  result <- frollmean(
    shifted,
    n = n,
    align = "right",
    na.rm = TRUE,
    hasNA = TRUE
  )

  observed <- frollsum(
    as.integer(!is.na(shifted)),
    n = n,
    align = "right",
    na.rm = TRUE,
    hasNA = TRUE
  )

  result[is.na(observed) | observed < min_obs] <- NA_real_
  result
}

shifted_ewm <- function(x, span, min_obs) {
  alpha <- 2 / (span + 1)
  shifted <- shift(x, 1L)
  out <- rep(NA_real_, length(shifted))

  running <- NA_real_
  seen <- 0L
  for (i in seq_along(shifted)) {
    value <- shifted[[i]]
    if (!is.na(value)) {
      running <- if (is.na(running)) {
        value
      } else {
        alpha * value + (1 - alpha) * running
      }
      seen <- seen + 1L
    }
    if (seen >= min_obs) {
      out[[i]] <- running
    }
  }
  out
}

build_episode_table <- function(
  bid_simulator,
  historical_actuals
) {
  surface_summary <- build_surface_summary(bid_simulator)

  chosen_sim <- bid_simulator[
    historical_actuals,
    on = .(
      portfolio_id,
      date,
      target_roas = chosen_target_roas
    ),
    nomatch = 0L,
    .(
      portfolio_id,
      date,
      chosen_target_roas = x.target_roas,
      google_cost_at_choice = x.google_cost,
      google_events_at_choice = x.google_events,
      google_value_at_choice = x.google_value,
      google_implied_roas_at_choice = x.google_implied_roas,
      google_implied_cpa_at_choice = x.google_implied_cpa
    )
  ]

  episodes <- merge(
    historical_actuals,
    chosen_sim,
    by = c(
      "portfolio_id",
      "date",
      "chosen_target_roas"
    ),
    all = FALSE,
    sort = FALSE
  )

  episodes <- merge(
    episodes,
    surface_summary,
    by = c("portfolio_id", "date"),
    all = FALSE,
    sort = FALSE
  )

  setorder(episodes, portfolio_id, date)

  for (target in TARGETS) {
    actual_col <- paste0("actual_", target)
    google_col <- paste0("google_", target, "_at_choice")
    error_col <- paste0(target, "_log_error")
    ratio_col <- paste0(target, "_ratio")

    episodes[, (error_col) := log(
      pmax(get(actual_col), EPS) /
        pmax(get(google_col), EPS)
    )]

    episodes[, (ratio_col) := (
      get(actual_col) /
        pmax(get(google_col), EPS)
    )]
  }

  episodes[, day_of_week := as.integer(format(date, "%u")) - 1L]
  episodes[, month := as.integer(format(date, "%m"))]
  episodes[, day_index := as.integer(date - min(date))]

  for (target in TARGETS) {
    error_col <- paste0(target, "_log_error")

    episodes[, paste0(target, "_error_lag1") :=
      shift(get(error_col), 1L),
    by = portfolio_id]

    episodes[, paste0(target, "_error_roll7") :=
      shifted_roll_mean(get(error_col), 7L, 2L),
    by = portfolio_id]

    episodes[, paste0(target, "_error_roll28") :=
      shifted_roll_mean(get(error_col), 28L, 5L),
    by = portfolio_id]

    episodes[, paste0(target, "_error_ewm") :=
      shifted_ewm(get(error_col), 21L, 3L),
    by = portfolio_id]
  }

  change_features <- c(
    "surface_cost_mean",
    "surface_cost_slope",
    "surface_cost_curvature",
    "surface_event_mean",
    "surface_event_slope",
    "surface_event_curvature"
  )

  for (feature in change_features) {
    output_col <- paste0(feature, "_change")
    episodes[, (output_col) := {
      prior <- shift(get(feature), 1L)
      fifelse(
        is.na(prior) | abs(prior) < EPS,
        NA_real_,
        (get(feature) - prior) / abs(prior)
      )
    }, by = portfolio_id]
  }

  list(
    episodes = episodes,
    surface_summary = surface_summary
  )
}


# ------------------------------------------------------------------------------
# Features
# ------------------------------------------------------------------------------

feature_lists <- function() {
  categorical <- c("portfolio_id")

  numeric <- c(
    "chosen_target_roas",
    "google_cost_at_choice",
    "google_events_at_choice",
    "google_value_at_choice",
    "google_implied_roas_at_choice",
    "google_implied_cpa_at_choice",
    "surface_repeat_age",
    "demand_proxy",
    "competition_proxy",
    "quality_proxy",
    "surface_cost_mean",
    "surface_cost_sd",
    "surface_cost_slope",
    "surface_cost_curvature",
    "surface_event_mean",
    "surface_event_sd",
    "surface_event_slope",
    "surface_event_curvature",
    "surface_value_mean",
    "surface_value_slope",
    "surface_value_curvature",
    "surface_implied_roas_mean",
    "surface_implied_roas_sd",
    "surface_cpa_mean",
    "surface_cpa_sd",
    "day_of_week",
    "month",
    "day_index",
    "cost_error_lag1",
    "cost_error_roll7",
    "cost_error_roll28",
    "cost_error_ewm",
    "events_error_lag1",
    "events_error_roll7",
    "events_error_roll28",
    "events_error_ewm",
    "value_error_lag1",
    "value_error_roll7",
    "value_error_roll28",
    "value_error_ewm",
    "surface_cost_mean_change",
    "surface_cost_slope_change",
    "surface_cost_curvature_change",
    "surface_event_mean_change",
    "surface_event_slope_change",
    "surface_event_curvature_change"
  )

  # Chosen tROAS remains available to AutoQuant because the historical
  # selection regime itself may carry level information. Shape is estimated
  # separately from out-of-fold residuals and is centered before application.
  similarity <- c(
    "chosen_target_roas",
    "surface_repeat_age",
    "demand_proxy",
    "competition_proxy",
    "quality_proxy",
    "surface_cost_mean",
    "surface_cost_slope",
    "surface_cost_curvature",
    "surface_event_mean",
    "surface_event_slope",
    "surface_event_curvature",
    "surface_value_mean",
    "surface_value_slope",
    "surface_value_curvature",
    "surface_implied_roas_mean",
    "surface_implied_roas_sd",
    "surface_cpa_mean",
    "surface_cpa_sd",
    "day_of_week",
    "month"
  )

  list(
    categorical = categorical,
    numeric = numeric,
    similarity = similarity
  )
}


# ------------------------------------------------------------------------------
# Contextual historical retrieval
# ------------------------------------------------------------------------------

standardization_stats <- function(train, features) {
  rbindlist(lapply(features, function(feature) {
    values <- train[[feature]]
    feature_mean <- mean(values, na.rm = TRUE)
    feature_sd <- sd(values, na.rm = TRUE)

    if (!is.finite(feature_mean)) {
      feature_mean <- 0
    }
    if (!is.finite(feature_sd) || feature_sd < EPS) {
      feature_sd <- 1
    }

    data.table(
      feature = feature,
      mean = feature_mean,
      sd = feature_sd
    )
  }))
}

standardize_features <- function(data, stats, prefix) {
  result <- data.table(row_id = seq_len(nrow(data)))

  for (i in seq_len(nrow(stats))) {
    feature <- stats$feature[[i]]
    values <- data[[feature]]
    values[is.na(values)] <- 0

    result[, paste0(prefix, feature) := (
      values - stats$mean[[i]]
    ) / stats$sd[[i]]]
  }

  result
}

contextual_predictions <- function(
  train,
  test,
  target,
  similarity_features,
  neighbor_count,
  recency_days = 180
) {
  stats <- standardization_stats(train, similarity_features)

  train_scaled <- standardize_features(train, stats, "z_")
  train_scaled[, train_row_id := .I]
  train_scaled[, date := train$date]
  train_scaled[, historical_value := train[[paste0(target, "_log_error")]]]

  test_scaled <- standardize_features(test, stats, "q_")
  test_scaled[, test_row_id := .I]
  test_scaled[, test_date := test$date]

  predictions <- numeric(nrow(test))

  z_names <- paste0("z_", similarity_features)
  q_names <- paste0("q_", similarity_features)

  # Batched row-wise distance calculation avoids materializing the full
  # train-test cross join while preserving exact Euclidean top-k retrieval.
  for (i in seq_len(nrow(test))) {
    query <- as.numeric(test_scaled[i, ..q_names])

    distance_sq <- numeric(nrow(train_scaled))
    for (j in seq_along(similarity_features)) {
      distance_sq <- distance_sq + (
        train_scaled[[z_names[[j]]]] - query[[j]]
      )^2
    }

    distances <- sqrt(distance_sq)
    k <- min(neighbor_count, length(distances))
    neighbor_idx <- head(order(distances), k)

    age_days <- pmax(
      as.integer(
        test_scaled$test_date[[i]] -
          train_scaled$date[neighbor_idx]
      ),
      0L
    )

    similarity_weight <- 1 / pmax(distances[neighbor_idx], 1e-5)
    recency_weight <- exp(-age_days / recency_days)
    weights <- similarity_weight * recency_weight

    predictions[[i]] <- weighted_mean_safe(
      train_scaled$historical_value[neighbor_idx],
      weights
    )
  }

  predictions
}


# ------------------------------------------------------------------------------
# AutoQuant level calibration
# ------------------------------------------------------------------------------

make_autoquant_spec <- function(
  target_col,
  feature_cols,
  cfg,
  dataset_id
) {
  aq_model_spec(
    task = "regression",
    target = target_col,
    features = feature_cols,
    engine_params = list(
      iterations = cfg$autoquant_iterations,
      depth = cfg$autoquant_depth,
      learning_rate = cfg$autoquant_learning_rate,
      l2_leaf_reg = cfg$autoquant_l2,
      thread_count = cfg$autoquant_thread_count,
      random_seed = 17L,
      verbose = FALSE
    ),
    dataset_id = dataset_id
  )
}

fit_autoquant_level <- function(
  train,
  test,
  target,
  feature_cols,
  cfg
) {
  target_col <- paste0(target, "_log_error")

  train_model <- copy(train[, c(feature_cols, target_col), with = FALSE])
  test_model <- copy(test[, c(feature_cols, target_col), with = FALSE])

  numeric_cols <- names(train_model)[
    vapply(train_model, is.numeric, logical(1L))
  ]
  for (col in numeric_cols) {
    set(train_model, which(is.na(train_model[[col]])), col, 0)
    set(test_model, which(is.na(test_model[[col]])), col, 0)
  }

  spec <- make_autoquant_spec(
    target_col = target_col,
    feature_cols = feature_cols,
    cfg = cfg,
    dataset_id = paste0("bid_calibration_", target)
  )

  fit <- aq_fit_model(spec, train_model)

  scored_result <- aq_score_model(
    fit,
    test_model,
    outcome_col = target_col
  )
  scored <- extract_autoquant_scored_data(scored_result)
  prediction_col <- detect_prediction_column(scored, target_col)

  list(
    fit = fit,
    predictions = scored[[prediction_col]],
    scored = scored,
    prediction_column = prediction_col
  )
}


# ------------------------------------------------------------------------------
# Out-of-fold AutoQuant residuals for shape modeling
# ------------------------------------------------------------------------------

create_time_folds <- function(train, n_folds = 5L) {
  unique_dates <- sort(unique(train$date))
  fold_id <- cut(
    seq_along(unique_dates),
    breaks = n_folds,
    labels = FALSE,
    include.lowest = TRUE
  )

  data.table(
    date = unique_dates,
    fold_id = fold_id
  )
}

autoquant_oof_predictions <- function(
  train,
  target,
  feature_cols,
  cfg,
  n_folds = 5L
) {
  fold_map <- create_time_folds(train, n_folds)
  working <- merge(
    copy(train),
    fold_map,
    by = "date",
    all.x = TRUE,
    sort = FALSE
  )
  working[, oof_prediction := NA_real_]

  fold_ids <- sort(unique(working$fold_id))

  for (fold in fold_ids) {
    valid_dates <- fold_map[fold_id == fold, date]
    fold_start <- min(valid_dates)

    fold_train <- working[date < fold_start]
    fold_valid <- working[fold_id == fold]

    # Earliest fold has no prior history and is intentionally left unavailable.
    if (nrow(fold_train) < 50L || nrow(fold_valid) == 0L) {
      next
    }

    result <- fit_autoquant_level(
      train = fold_train,
      test = fold_valid,
      target = target,
      feature_cols = feature_cols,
      cfg = cfg
    )

    valid_rows <- which(working$fold_id == fold)
    working$oof_prediction[valid_rows] <- result$predictions
  }

  working[, .(
    portfolio_id,
    date,
    chosen_target_roas,
    actual_log_error = get(paste0(target, "_log_error")),
    oof_level_prediction = oof_prediction,
    shape_residual = get(paste0(target, "_log_error")) - oof_prediction
  )]
}


# ------------------------------------------------------------------------------
# AutoNLS residual-shape calibration
# ------------------------------------------------------------------------------

safe_autonls_fit <- function(
  data,
  x_col,
  y_col,
  cfg
) {
  fit_data <- copy(data)[
    is.finite(get(x_col)) &
      is.finite(get(y_col))
  ]

  if (nrow(fit_data) < 20L || uniqueN(fit_data[[x_col]]) < 4L) {
    return(NULL)
  }

  tryCatch(
    AutoNLS(
      data = fit_data,
      x = x_col,
      y = y_col,
      models = cfg$autonls_models,
      loss = "mse",
      n_starts = cfg$autonls_n_starts,
      seed = 42L,
      validation_fraction = cfg$autonls_validation_fraction,
      validation_seed = 42L,
      interval_method = "none",
      scale_x = TRUE,
      scale_y = TRUE
    ),
    error = function(e) {
      warning(
        "AutoNLS fit failed: ",
        conditionMessage(e),
        call. = FALSE
      )
      NULL
    }
  )
}

predict_autonls <- function(fit, x_values, x_col) {
  if (is.null(fit)) {
    return(rep(0, length(x_values)))
  }

  new_data <- data.table(x_values)
  setnames(new_data, "x_values", x_col)

  result <- tryCatch(
    fit$predict(new_data),
    error = function(e) NULL
  )

  if (is.null(result)) {
    return(rep(0, length(x_values)))
  }

  if (is.numeric(result)) {
    return(as.numeric(result))
  }

  result_dt <- as.data.table(result)
  prediction_candidates <- c(
    "prediction",
    "predicted",
    "Prediction",
    "yhat"
  )
  present <- prediction_candidates[
    prediction_candidates %chin% names(result_dt)
  ]

  if (length(present) > 0L) {
    return(as.numeric(result_dt[[present[[1L]]]]))
  }

  numeric_columns <- names(result_dt)[
    vapply(result_dt, is.numeric, logical(1L))
  ]

  if (length(numeric_columns) == 1L) {
    return(as.numeric(result_dt[[numeric_columns]]))
  }

  stop(
    "Unable to identify AutoNLS prediction column. Available columns: ",
    paste(names(result_dt), collapse = ", ")
  )
}

fit_shape_models <- function(
  oof_predictions,
  target,
  cfg
) {
  usable <- copy(oof_predictions)[
    is.finite(shape_residual)
  ]

  # Center chosen tROAS within portfolio. This lets the global model describe
  # relative shape without letting portfolio-level policy centers dominate.
  portfolio_centers <- usable[
    ,
    .(portfolio_troas_center = median(chosen_target_roas)),
    by = portfolio_id
  ]

  usable <- merge(
    usable,
    portfolio_centers,
    by = "portfolio_id",
    all.x = TRUE,
    sort = FALSE
  )

  usable[, centered_troas := (
    chosen_target_roas - portfolio_troas_center
  )]

  global_fit <- safe_autonls_fit(
    data = usable,
    x_col = "centered_troas",
    y_col = "shape_residual",
    cfg = cfg
  )

  portfolio_fits <- list()
  portfolio_status <- list()

  for (portfolio in unique(usable$portfolio_id)) {
    portfolio_data <- usable[portfolio_id == portfolio]

    if (
      nrow(portfolio_data) < cfg$min_portfolio_shape_rows ||
        uniqueN(portfolio_data$centered_troas) < 4L
    ) {
      portfolio_fits[[portfolio]] <- NULL
      portfolio_status[[portfolio]] <- "global_fallback_insufficient_support"
      next
    }

    portfolio_fit <- safe_autonls_fit(
      data = portfolio_data,
      x_col = "centered_troas",
      y_col = "shape_residual",
      cfg = cfg
    )

    if (is.null(portfolio_fit)) {
      portfolio_fits[[portfolio]] <- NULL
      portfolio_status[[portfolio]] <- "global_fallback_fit_failure"
    } else {
      portfolio_fits[[portfolio]] <- portfolio_fit
      portfolio_status[[portfolio]] <- "portfolio_autonls"
    }
  }

  list(
    target = target,
    global_fit = global_fit,
    portfolio_fits = portfolio_fits,
    portfolio_status = portfolio_status,
    portfolio_centers = portfolio_centers,
    training_data = usable
  )
}

predict_centered_shape <- function(
  shape_models,
  portfolio,
  target_roas,
  reference_troas
) {
  center_row <- shape_models$portfolio_centers[
    portfolio_id == portfolio
  ]
  if (nrow(center_row) == 0L) {
    center_value <- reference_troas
  } else {
    center_value <- center_row$portfolio_troas_center[[1L]]
  }

  x_grid <- target_roas - center_value
  x_reference <- reference_troas - center_value

  fit <- shape_models$portfolio_fits[[portfolio]]
  if (is.null(fit)) {
    fit <- shape_models$global_fit
  }

  if (is.null(fit)) {
    return(rep(0, length(target_roas)))
  }

  grid_prediction <- predict_autonls(
    fit,
    x_values = x_grid,
    x_col = "centered_troas"
  )
  reference_prediction <- predict_autonls(
    fit,
    x_values = x_reference,
    x_col = "centered_troas"
  )[[1L]]

  grid_prediction - reference_prediction
}


# ------------------------------------------------------------------------------
# Model evaluation
# ------------------------------------------------------------------------------

evaluate_prediction_columns <- function(
  test,
  target,
  cutoff_date,
  n_train
) {
  actual_col <- paste0("actual_", target)

  models <- c(
    "raw",
    "rolling",
    "autoquant",
    "contextual",
    "blend"
  )

  rbindlist(lapply(models, function(model_name) {
    pred_col <- paste0(
      "pred_",
      target,
      "_",
      model_name
    )

    actual <- test[[actual_col]]
    predicted <- test[[pred_col]]

    data.table(
      target = target,
      model = model_name,
      cutoff_date = cutoff_date,
      n_train = n_train,
      n_test = nrow(test),
      mae = mean(abs(actual - predicted), na.rm = TRUE),
      rmse = rmse(actual, predicted),
      mape = safe_mape(actual, predicted),
      mean_bias = mean(predicted - actual, na.rm = TRUE)
    )
  }))
}

evaluate_models <- function(
  episodes,
  cfg
) {
  unique_dates <- sort(unique(episodes$date))
  cutoff_index <- floor(
    length(unique_dates) * cfg$train_fraction
  ) + 1L
  cutoff_date <- unique_dates[[cutoff_index]]

  train <- copy(episodes[date < cutoff_date])
  test <- copy(episodes[date >= cutoff_date])

  features <- feature_lists()
  feature_cols <- c(
    features$categorical,
    features$numeric
  )

  fitted_models <- list()
  shape_models <- list()
  metric_chunks <- list()

  for (target in TARGETS) {
    google_col <- paste0(
      "google_",
      target,
      "_at_choice"
    )

    rolling_col <- paste0(
      target,
      "_rolling_log_correction"
    )
    test[, (rolling_col) := (
      0.65 * fifelse(
        is.na(get(paste0(target, "_error_ewm"))),
        0,
        get(paste0(target, "_error_ewm"))
      ) +
        0.35 * fifelse(
          is.na(get(paste0(target, "_error_roll28"))),
          0,
          get(paste0(target, "_error_roll28"))
        )
    )]

    autoquant <- fit_autoquant_level(
      train = train,
      test = test,
      target = target,
      feature_cols = feature_cols,
      cfg = cfg
    )
    fitted_models[[target]] <- autoquant$fit

    autoquant_col <- paste0(
      target,
      "_autoquant_log_correction"
    )
    test[, (autoquant_col) := autoquant$predictions]

    contextual_col <- paste0(
      target,
      "_contextual_log_correction"
    )
    test[, (contextual_col) := contextual_predictions(
      train = train,
      test = test,
      target = target,
      similarity_features = features$similarity,
      neighbor_count = cfg$neighbor_count,
      recency_days = cfg$contextual_recency_days
    )]

    blend_col <- paste0(
      target,
      "_blend_log_correction"
    )
    test[, (blend_col) := (
      0.50 * get(autoquant_col) +
        0.30 * get(contextual_col) +
        0.20 * get(rolling_col)
    )]

    test[, paste0("pred_", target, "_raw") :=
      get(google_col)]

    test[, paste0("pred_", target, "_rolling") :=
      get(google_col) * exp(get(rolling_col))]

    test[, paste0("pred_", target, "_autoquant") :=
      get(google_col) * exp(get(autoquant_col))]

    test[, paste0("pred_", target, "_contextual") :=
      get(google_col) * exp(get(contextual_col))]

    test[, paste0("pred_", target, "_blend") :=
      get(google_col) * exp(get(blend_col))]

    # AutoNLS is fit on time-respecting AutoQuant out-of-fold residuals.
    oof <- autoquant_oof_predictions(
      train = train,
      target = target,
      feature_cols = feature_cols,
      cfg = cfg,
      n_folds = 5L
    )
    shape_models[[target]] <- fit_shape_models(
      oof_predictions = oof,
      target = target,
      cfg = cfg
    )

    metric_chunks[[target]] <- evaluate_prediction_columns(
      test = test,
      target = target,
      cutoff_date = cutoff_date,
      n_train = nrow(train)
    )
  }

  list(
    metrics = rbindlist(metric_chunks, use.names = TRUE),
    test_predictions = test,
    autoquant_models = fitted_models,
    shape_models = shape_models,
    cutoff_date = cutoff_date
  )
}


# ------------------------------------------------------------------------------
# Full corrected surfaces
# ------------------------------------------------------------------------------

create_corrected_surfaces <- function(
  bid_simulator,
  test_predictions,
  shape_models,
  cfg
) {
  held_out_keys <- unique(
    test_predictions[, .(portfolio_id, date)]
  )

  surfaces <- bid_simulator[
    held_out_keys,
    on = .(portfolio_id, date),
    nomatch = 0L
  ]

  correction_columns <- c(
    "portfolio_id",
    "date",
    "chosen_target_roas",
    paste0("google_", TARGETS, "_at_choice"),
    paste0("pred_", TARGETS, "_blend")
  )

  corrections <- copy(
    test_predictions[, ..correction_columns]
  )

  for (target in TARGETS) {
    corrections[
      ,
      paste0(target, "_blend_log_correction") := log(
        pmax(get(paste0("pred_", target, "_blend")), EPS) /
          pmax(get(paste0("google_", target, "_at_choice")), EPS)
      )
    ]
  }

  surfaces <- merge(
    surfaces,
    corrections,
    by = c("portfolio_id", "date"),
    all.x = TRUE,
    sort = FALSE
  )

  for (target in TARGETS) {
    shape_col <- paste0(
      target,
      "_shape_log_correction"
    )

    surfaces[, (shape_col) := {
      predict_centered_shape(
        shape_models = shape_models[[target]],
        portfolio = portfolio_id[[1L]],
        target_roas = target_roas,
        reference_troas = chosen_target_roas[[1L]]
      )
    }, by = .(portfolio_id, date)]

    total_col <- paste0(
      target,
      "_total_log_correction"
    )

    surfaces[, (total_col) := (
      get(paste0(target, "_blend_log_correction")) +
        cfg$autonls_shape_shrinkage *
          get(shape_col)
    )]

    surfaces[
      ,
      paste0("corrected_", target) := (
        get(paste0("google_", target)) *
          exp(get(total_col))
      )
    ]
  }

  surfaces[, corrected_implied_roas := safe_divide(
    corrected_value,
    corrected_cost
  )]

  surfaces[, corrected_implied_cpa := safe_divide(
    corrected_cost,
    corrected_events
  )]

  keep <- c(
    "portfolio_id",
    "date",
    "surface_id",
    "surface_repeat_age",
    "target_roas",
    "chosen_target_roas",
    "google_cost",
    "google_events",
    "google_value",
    "google_implied_roas",
    "google_implied_cpa",
    "corrected_cost",
    "corrected_events",
    "corrected_value",
    "corrected_implied_roas",
    "corrected_implied_cpa",
    paste0(TARGETS, "_blend_log_correction"),
    paste0(TARGETS, "_shape_log_correction"),
    paste0(TARGETS, "_total_log_correction")
  )

  setorder(
    surfaces,
    portfolio_id,
    date,
    target_roas
  )

  surfaces[, ..keep]
}


# ------------------------------------------------------------------------------
# Diagnostics
# ------------------------------------------------------------------------------

shape_model_diagnostics <- function(shape_models) {
  rbindlist(lapply(names(shape_models), function(target) {
    model_set <- shape_models[[target]]

    portfolio_rows <- rbindlist(
      lapply(names(model_set$portfolio_status), function(portfolio) {
        data.table(
          target = target,
          scope = "portfolio",
          portfolio_id = portfolio,
          status = model_set$portfolio_status[[portfolio]]
        )
      }),
      fill = TRUE
    )

    global_row <- data.table(
      target = target,
      scope = "global",
      portfolio_id = NA_character_,
      status = if (is.null(model_set$global_fit)) {
        "unavailable"
      } else {
        "available"
      }
    )

    rbind(global_row, portfolio_rows, fill = TRUE)
  }))
}


# ------------------------------------------------------------------------------
# Main
# ------------------------------------------------------------------------------

main <- function(cfg = default_config()) {
  dir.create(
    cfg$output_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  generated <- generate_synthetic_data(cfg)
  bid_simulator <- generated$bid_simulator
  historical_actuals <- generated$historical_actuals

  built <- build_episode_table(
    bid_simulator = bid_simulator,
    historical_actuals = historical_actuals
  )
  episodes <- built$episodes
  surface_summary <- built$surface_summary

  evaluated <- evaluate_models(
    episodes = episodes,
    cfg = cfg
  )

  corrected_surfaces <- create_corrected_surfaces(
    bid_simulator = bid_simulator,
    test_predictions = evaluated$test_predictions,
    shape_models = evaluated$shape_models,
    cfg = cfg
  )

  diagnostics <- shape_model_diagnostics(
    evaluated$shape_models
  )

  outputs <- list(
    "bid_simulator.csv" = bid_simulator,
    "historical_actuals.csv" = historical_actuals,
    "surface_summary.csv" = surface_summary,
    "modeling_episodes.csv" = episodes,
    "model_comparison.csv" = evaluated$metrics,
    "heldout_predictions.csv" = evaluated$test_predictions,
    "corrected_surfaces.csv" = corrected_surfaces,
    "shape_model_diagnostics.csv" = diagnostics
  )

  for (filename in names(outputs)) {
    fwrite(
      outputs[[filename]],
      file.path(cfg$output_dir, filename)
    )
  }

  cat("\nModel comparison (MAPE %):\n")
  print(
    dcast(
      evaluated$metrics[
        ,
        .(model, target, mape_pct = 100 * mape)
      ],
      model ~ target,
      value.var = "mape_pct"
    )[order(model)]
  )

  cat("\nAutoNLS shape model status:\n")
  print(diagnostics)

  cat(
    "\nFiles written to: ",
    normalizePath(cfg$output_dir),
    "\n",
    sep = ""
  )

  invisible(list(
    config = cfg,
    bid_simulator = bid_simulator,
    historical_actuals = historical_actuals,
    surface_summary = surface_summary,
    episodes = episodes,
    metrics = evaluated$metrics,
    heldout_predictions = evaluated$test_predictions,
    corrected_surfaces = corrected_surfaces,
    autoquant_models = evaluated$autoquant_models,
    shape_models = evaluated$shape_models,
    shape_model_diagnostics = diagnostics
  ))
}


if (sys.nframe() == 0L) {
  results <- main()
}

# Overall results
results$metrics[
  order(target, mape),
  .(
    target,
    model,
    mape_pct = round(100 * mape, 2),
    mae,
    rmse,
    mean_bias
  )
]

# Inspect holdout predictions
results$heldout_predictions[
  ,
  .(
    portfolio_id,
    date,
    chosen_target_roas,
    actual_value,
    google_value_at_choice,
    pred_value_autoquant,
    pred_value_contextual,
    pred_value_blend
  )
]

# Actual vs predicted
results$heldout_predictions[
  ,
  .(
    actual = actual_value,
    google = google_value_at_choice,
    corrected = pred_value_blend
  )
]

# Primary deliverable: corrected data
results$corrected_surfaces


one_surface <- results$corrected_surfaces[
  portfolio_id == "P01" &
    date == min(date)
]

one_surface[
  ,
  .(
    target_roas,
    google_cost,
    corrected_cost,
    google_events,
    corrected_events,
    google_value,
    corrected_value,
    corrected_implied_roas,
    corrected_implied_cpa
  )
]

recommended_value <- results$corrected_surfaces[
  ,
  .SD[which.max(corrected_value)],
  by = .(portfolio_id, date)
]

