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
    #' @return A data.table of evaluation metrics with fitted equations.
    generate_metrics = function(y_col = NULL) {
      if (is.null(self$fit_results)) stop("No fitted models to evaluate.")

      # For numeric equation output
      format_equation <- function(equation) {
        # Replace double negatives with a single positive sign
        formatted_equation <- gsub("-\\s*-", "+ ", equation)
        return(formatted_equation)
      }

      metrics <- lapply(names(self$fit_results), function(model_name) {
        fit <- self$fit_results[[model_name]]
        if (is.null(fit)) return(NULL)

        tryCatch({
          summary_fit <- summary(fit)
          aic <- AIC(fit)
          bic <- BIC(fit)
          residual_std_error <- summary_fit$sigma

          # Access the formula stored in the fitted model
          formula <- fit$formula

          # Extract observed values
          observed <- self$data[[y_col]]
          if (!is.numeric(observed)) stop("Observed values are not numeric.")

          # Compute predicted values using the fitted model
          predicted <- predict(fit, newdata = self$data)

          # Compute R-squared
          ss_res <- sum((observed - predicted)^2)
          ss_tot <- sum((observed - mean(observed))^2)
          r_squared <- 1 - ss_res / ss_tot

          # Extract fitted parameter values
          params <- coef(fit)

          # Replace parameter names in the formula with their fitted values
          fitted_equation <- deparse(formula)
          for (param_name in names(params)) {
            fitted_equation <- gsub(param_name, format(params[[param_name]], digits = 3), fitted_equation)
          }

          # Compile metrics
          list(
            `Model Name` = model_name,
            Formula = deparse(formula),              # Original model formula
            Model = format_equation(fitted_equation),     # Fitted model with parameter values
            AIC = aic,
            BIC = bic,
            Resid_Std_Err = residual_std_error,
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
    #' @return An `echarts4r` plot showing observed vs. predicted data.
    generate_comparison_plot = function(data, x_col, y_col, theme = "macarons") {
      if (is.null(self$fit_results) || length(self$fit_results) == 0) {
        stop("No fitted models to evaluate.")
      }
      if (!all(c(x_col, y_col) %in% names(data))) stop("x_col and y_col must exist in the dataset.")

      # Retrieve metrics (including R-squared)
      metrics <- self$generate_metrics(y_col = y_col)

      # Generate plots
      self$plots <- setNames(lapply(names(self$fit_results), function(model_name) {
        fit <- self$fit_results[[model_name]]
        tryCatch({

          # Generate predictions
          predictions <- data.table::data.table(
            x = data[[x_col]],
            y_pred = predict(fit, newdata = data)
          )

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
            echarts4r::e_datazoom(x_index = c(0,1)) |>
            echarts4r::e_toolbox_feature(feature = c("saveAsImage","dataZoom"))
        }, error = function(e) {
          message("Error processing model plot: ", e$message)
          NULL
        })
      }), names(self$fit_results))

      return(self$plots)
    }
  )
)
