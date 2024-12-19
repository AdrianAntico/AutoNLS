#' NonLinearModelEvaluator
#'
#' An R6 class to evaluate non-linear regression models.
#' Includes tools to generate tables of statistics and visualizations
#' to compare models against data.
#'
#' @export
NonLinearModelEvaluator <- R6::R6Class(
  "NonLinearModelEvaluator",
  public = list(
    #' @field fit_results A list of fitted model objects.
    fit_results = NULL,

    #' @field evaluation_metrics A data.table containing model evaluation metrics.
    evaluation_metrics = NULL,

    #' @field plots A list of visualizations comparing models against data.
    plots = list(),

    #' @field data The original dataset used for fitting models.
    data = NULL,

    #' Initialize the NonLinearModelEvaluator class
    #'
    #' @param fit_results A list of fitted model objects (e.g., output from NonLinearFitter).
    #' @param data The original dataset used for fitting models.
    #' @return A new instance of the NonLinearModelEvaluator class.
    initialize = function(fit_results, data) {
      if (!is.list(fit_results)) stop("fit_results must be a list of model objects.")
      if (!data.table::is.data.table(data)) stop("data must be a data.table.")
      self$fit_results <- fit_results
      self$data <- data
    },

    #' @description Computes and summarizes key performance metrics for all fitted models,
    #' including goodness-of-fit statistics, residual standard errors, and information criteria.
    #' The metrics provide a comprehensive evaluation of each model's performance on the
    #' given dataset.
    #'
    #' @details This method evaluates each fitted model by calculating various metrics such as
    #' Akaike Information Criterion (AIC), Bayesian Information Criterion (BIC), residual
    #' standard error, and R-squared values. It ensures compatibility across all models
    #' and gracefully handles cases where a model fails to fit properly by excluding it from
    #' the final summary.
    #'
    #' @param y_col target variable
    #' @param x_col x variable
    #' @return A data.table of evaluation metrics with fitted equations.
    #' @export
    generate_metrics = function(y_col = NULL, x_col = NULL) {
      if (is.null(self$fit_results)) stop("No fitted models to evaluate.")

      metrics <- lapply(names(self$fit_results), function(model_name) {
        fit <- self$fit_results[[model_name]]
        if (is.null(fit)) return(NULL)

        tryCatch({

          # Extract parameters
          params <- fit$coefficients
          model_function <- fit$model_function

          # Observed and predicted values
          observed <- self$data[[y_col]]
          x_scaled <- (self$data[[x_col]] - fit$scale_params$min_x) / fit$scale_params$scale_factor_x
          predicted <- fit$back_transform(predictions = model_function(x = x_scaled, params = params), scale_params = fit$scale_params)
          residuals <- observed - predicted

          # Weighted residual sum of squares
          if (!is.null(fit$weights)) {
            wrss <- sum(residuals^2 * fit$weights)
          } else {
            wrss <- sum(residuals^2)
          }

          # Compute R-squared
          ss_res <- sum(residuals^2)
          ss_tot <- sum((observed - mean(observed))^2)
          r_squared <- 1 - ss_res / ss_tot

          # Compute AIC and BIC manually
          n <- length(observed)
          k <- length(params)
          log_likelihood <- -1.0 * (n/2 * (log(2 * pi) + log(wrss / n) + 1))
          aic <- -2 * log_likelihood + 2 * k
          bic <- -2 * log_likelihood + log(n) * k

          # Create fitted model string
          fitted_equation <- deparse(fit$formula)

          # Replace new line issues
          fitted_equation <- gsub("\\s+", " ", paste(fitted_equation, collapse = ""))
          for (param_name in names(params)) {
            fitted_equation <- gsub(
              paste0("\\b", param_name, "\\b"),
              format(params[[param_name]], digits = 4),
              fitted_equation
            )
          }

          # Replace double negatives (-- becomes "")
          fitted_equation <- gsub("--", "", fitted_equation, fixed = TRUE)

          # Replace double negatives (- - becomes " + ")
          fitted_equation <- gsub("- -", "+ ", fitted_equation, fixed = TRUE)

          # Compile metrics
          list(
            `Model Name` = model_name,
            Formula = gsub("\\s+", " ", paste(deparse(fit$formula), collapse = "")),
            `Model (standardized)` = fitted_equation,  # Use the formula with coefficients
            AIC = aic,
            BIC = bic,
            Resid_Std_Err = sqrt(mean(residuals^2)),
            R_Sq = r_squared
          )

        }, error = function(e) {
          message("Error processing model: ", e$message)
          NULL
        })
      })

      self$evaluation_metrics <- data.table::rbindlist(metrics, fill = TRUE)
      return(self$evaluation_metrics)
    },

    #' @description Creates visualizations comparing the fitted models against the observed data
    #' to assess their fit and predictive behavior. The plots include the fitted curves
    #' overlaid on the original data points for easy comparison.
    #'
    #' @details This method generates a comparison plot for each fitted model, allowing
    #' users to visually assess how well the models align with the observed data. It
    #' supports customization options such as theming and dynamic adjustment of the
    #' x-axis range. Models that fail to fit are gracefully excluded, ensuring clean
    #' and informative outputs.
    #'
    #' @param data A `data.table` or `data.frame` containing the dataset used for evaluation.
    #' @param x_col A string specifying the name of the x variable in the dataset.
    #' @param y_col A string specifying the name of the y variable in the dataset.
    #' @param theme Echarts theme
    #' @param lower_bound Lower bound probability. Defaults to 0.025
    #' @param upper_bound Upper bound probability. Defaults to 0.975
    #' @param n_sim Number of simulations to run for prediction interval
    #' @return An `echarts4r` plot showing observed vs. predicted data, with weighted comparisons if available.
    #' @export
    generate_comparison_plot = function(
    data,
    x_col,
    y_col,
    theme = "westeros",
    lower_bound = 0.025,
    upper_bound = 0.975,
    n_sim = 1000) {
      if (is.null(self$fit_results) || length(self$fit_results) == 0) {
        stop("No fitted models to evaluate.")
      }
      if (!all(c(x_col, y_col) %in% names(data))) stop("x_col and y_col must exist in the dataset.")

      # Retrieve metrics (including R-squared)
      metrics <- self$generate_metrics(y_col = y_col, x_col = x_col)

      # Check if metrics were generated successfully
      if (nrow(metrics) == 0 || is.null(metrics)) {
        stop("No metrics available for the models.")
      }

      # Generate plots for all models
      self$plots <- setNames(lapply(names(self$fit_results), function(model_name) {
        fit <- self$fit_results[[model_name]]
        tryCatch({

          # Handle weighted and unweighted models
          is_weighted <- inherits(fit, "custom_nls")
          if (is_weighted) {
            predictions <- data.table::data.table(
              x = data[[x_col]],
              y_pred = fit$back_transform(fit$fitted.values, scale_params = fit$scale_params)
            )
          } else {
            predictions <- data.table::data.table(
              x = self$data[[x_col]],
              y_pred = fit$back_transform(predict(fit, newdata = fit$scaled_data), scale_params = fit$scale_params)
            )
          }

          # Simulate lower and upper prediction bounds
          x_values <- (predictions$x - fit$scale_params$min_x) / (fit$scale_params$max_x - fit$scale_params$min_x)
          bounds <- tryCatch({
            private$simulate_prediction_bounds(fit, x_values, lower_bound, upper_bound)
          }, error = function(e) {
            message("Error processing model plot: ", e$message)
            NULL
          })

          # Merge predictions with observed data
          combined_data <- data.table::data.table(
            x = data[[x_col]],
            y = data[[y_col]],
            y_pred = predictions$y_pred
          )

          if (!is.null(bounds)) {
            combined_data[, y_lower := fit$back_transform(bounds$lower, fit$scale_params)]
            combined_data[, y_upper := fit$back_transform(bounds$upper, fit$scale_params)]
          }

          # Get R-squared from metrics
          r_squared <- metrics[`Model Name` == eval(model_name)][["R_Sq"]]

          # Create plot
          plot <- combined_data |>
            echarts4r::e_charts(x) |>
            echarts4r::e_scatter(y, name = "Observed") |>
            echarts4r::e_line(y_pred, name = "Predicted", smooth = TRUE, showSymbol = FALSE) |>
            echarts4r::e_theme(name = theme) |>
            echarts4r::e_title(
              text = paste("Model Fit:", model_name),
              subtext = paste("R-Sq: ", round(r_squared, 4))
            ) |>
            echarts4r::e_datazoom(x_index = c(0, 1)) |>
            echarts4r::e_toolbox_feature(feature = c("saveAsImage", "dataZoom"))

          if ("y_lower" %in% names(combined_data)) {
            plot <- echarts4r::e_line(e = plot, y_lower, name = "Lower Bound", smooth = TRUE, showSymbol = FALSE, lineStyle = list(type = "dotted")) |>
              echarts4r::e_line(y_upper, name = "Upper Bound", smooth = TRUE, showSymbol = FALSE, lineStyle = list(type = "dotted"))
          }
          plot
        }, error = function(e) {
          message("Error processing model plot: ", e$message)
          NULL
        })
      }), names(self$fit_results))

      return(self$plots)
    }
  ),

  private = list(
    simulate_prediction_bounds = function(fit, x_values, lower_bound, upper_bound, n_sim = 1000) {
      params <- fit$coefficients
      if ("SE" %in% names(fit$confidence_intervals)) {
        se_params <- fit$confidence_intervals$SE
      } else {
        se_params <- NULL
      }

      if (is.null(se_params)) return(NULL)

      # Simulate parameter sets
      sim_matrix <- replicate(n_sim, {
        sim_params <- rnorm(length(params), mean = unlist(params), sd = unlist(se_params))
        param_list <- as.list(sim_params)
        names(param_list) <- names(params)
        fit$model_function(x = x_values, params = param_list)
      })

      # Calculate lower and upper bounds
      lower <- apply(sim_matrix, 1, quantile, probs = lower_bound)
      upper <- apply(sim_matrix, 1, quantile, probs = upper_bound)

      return(list(lower = lower, upper = upper))
    }
  )
)
