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

    #' @return A data.table of evaluation metrics.
    generate_metrics = function() {
      if (is.null(self$fit_results)) stop("No fitted models to evaluate.")

      metrics <- lapply(self$fit_results, function(fit) {
        if (is.null(fit)) return(NULL)

        tryCatch({
          summary_fit <- summary(fit)
          aic <- AIC(fit)
          bic <- BIC(fit)
          residual_std_error <- summary_fit$sigma

          # Access the formula stored in the fitted model
          formula <- fit$formula
          response_var <- all.vars(formula)[1] # Extract the response variable name
          predictor_var <- all.vars(formula)[2] # Extract the predictor variable name

          # Directly access observed values from the original dataset
          observed <- self$data[[response_var]]

          # Ensure observed values are numeric
          if (!is.numeric(observed)) {
            stop("Observed values are not numeric.")
          }

          # Compute predicted values using the fitted model
          predicted <- predict(fit, newdata = self$data)

          # Compute R-squared
          ss_res <- sum((observed - predicted)^2)
          ss_tot <- sum((observed - mean(observed))^2)
          r_squared <- 1 - ss_res / ss_tot

          list(
            Model = deparse(formula),
            AIC = aic,
            BIC = bic,
            Residual_Std_Error = residual_std_error,
            R_Squared = r_squared
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
    #' @return An `echarts4r` plot showing observed vs. predicted data.
    generate_comparison_plot = function(data, x_col, y_col) {
      if (is.null(self$fit_results) || length(self$fit_results) == 0) {
        stop("No fitted models to evaluate.")
      }
      if (!all(c(x_col, y_col) %in% names(data))) stop("x_col and y_col must exist in the dataset.")

      self$plots <- lapply(self$fit_results, function(fit) {
        if (is.null(fit)) return(NULL)

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

          # Create plot
          combined_data |>
            echarts4r::e_charts(x) |>
            echarts4r::e_scatter(y, name = "Observed") |>
            echarts4r::e_line(y_pred, name = "Predicted") |>
            echarts4r::e_title(
              text = paste("Model Fit:", deparse(fit$call$formula))
            )
        }, error = function(e) {
          message("Error processing model plot: ", e$message)
          NULL
        })
      })

      return(self$plots)
    }
  )
)
