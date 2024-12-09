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

    #' @param y_col target variable
    #' @param x_col x variable
    #' @return A data.table of evaluation metrics with fitted equations.
    generate_metrics = function(y_col = NULL, x_col = NULL) {
      if (is.null(self$fit_results)) stop("No fitted models to evaluate.")

      metrics <- lapply(names(self$fit_results), function(model_name) {
        fit <- self$fit_results[[model_name]]
        if (is.null(fit)) return(NULL)

        tryCatch({

          # Extract parameters
          params <- coef(fit)
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
            Formula = deparse(fit$formula),
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

    #' @param data A `data.table` or `data.frame` containing the dataset used for evaluation.
    #' @param x_col A string specifying the name of the x variable in the dataset.
    #' @param y_col A string specifying the name of the y variable in the dataset.
    #' @param theme Echarts theme
    #' @return An `echarts4r` plot showing observed vs. predicted data, with weighted comparisons if available.
    generate_comparison_plot = function(data, x_col, y_col, theme = "westeros") {
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

          # Merge predictions with observed data
          combined_data <- data.table::data.table(
            x = data[[x_col]],
            y = data[[y_col]]
          )
          combined_data <- merge(combined_data, predictions, by = "x", all = TRUE)

          # Get R-squared from metrics
          r_squared <- metrics[`Model Name` == eval(model_name)][["R_Sq"]]

          # Create plot
          combined_data |>
            echarts4r::e_charts(x) |>
            echarts4r::e_scatter(y, name = "Observed") |>
            echarts4r::e_line(y_pred, name = "Predicted") |>
            echarts4r::e_theme(name = theme) |>
            echarts4r::e_title(
              text = paste("Model Fit:", model_name),
              subtext = paste("R-Sq: ", round(r_squared, 4))
            ) |>
            echarts4r::e_datazoom(x_index = c(0, 1)) |>
            echarts4r::e_toolbox_feature(feature = c("saveAsImage", "dataZoom"))
        }, error = function(e) {
          message("Error processing model plot: ", e$message)
          NULL
        })
      }), names(self$fit_results))

      return(self$plots)
    }
  )
)
