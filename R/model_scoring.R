#' ModelScorer
#'
#' An R6 class to score non-linear regression models on new data
#' and visualize the results.
#' @export
ModelScorer <- R6::R6Class(
  "ModelScorer",
  public = list(
    #' @field fit_results A list of fitted model objects.
    fit_results = NULL,

    #' @field scored_data A list of data.tables containing scored data.
    scored_data = list(),

    #' @field score_plots A list of plots visualizing scored data.
    score_plots = list(),

    #' @param fit_results A list of fitted model objects (e.g., output from NonLinearFitter).
    #' @return A new instance of the ModelScorer class.
    initialize = function(fit_results) {
      if (!is.list(fit_results)) {
        message("fit_results must be a list of model objects.")
        return(NULL)
      }
      self$fit_results <- fit_results
    },

    #' @description Generates predictions for new data using the fitted non-linear models.
    #' This method applies each model to the new dataset and returns the predicted values.
    #'
    #' @details The `score_new_data` method enables users to evaluate how well the fitted models generalize
    #' to unseen data. It uses the stored parameters of the fitted models and applies them to the specified
    #' independent variable (`x_col`) in the new dataset. If the fitted models include transformations,
    #' the predictions are back-transformed to match the scale of the original data.
    #'
    #' The output is a data frame containing the original data and the predicted values for each model,
    #' making it easy to compare model predictions side-by-side.
    #'
    #' @param new_data A data.table containing the new data to score.
    #' @param x_col The predictor column in `new_data`.
    #' @param get_prediction_bounds TRUE to return prediction bounds
    #' @param lower_bound Lower bound of prediction interval. Defaults to 0.025
    #' @param upper_bound Upper bound of prediction interval. Defaults to 0.975
    #' @return A list of data.tables with predicted values for each model.
    #' @export
    score_new_data = function(new_data, x_col, get_prediction_bounds = FALSE, lower_bound = 0.025, upper_bound = 0.975) {
      if (!data.table::is.data.table(new_data)) message("new_data must be a data.table.")
      if (!x_col %in% names(new_data)) {
        message("x_col must exist in the dataset.")
        return(NULL)
      }

      self$scored_data <- lapply(self$fit_results, function(fit) {
        if (is.null(fit)) return(NULL)

        tryCatch({

          # Scale x_col
          scaled_x <- (new_data[[x_col]] - fit$scale_params$min_x) / fit$scale_params$scale_factor_x

          # Simulate lower and upper prediction bounds
          if (get_prediction_bounds) {
            bounds <- tryCatch({
              private$simulate_prediction_bounds(fit, scaled_x, lower_bound, upper_bound)
            }, error = function(e) {
              message("Error generating prediction bounds: ", e$message)
              NULL
            })
          } else {
            bounds <- NULL
          }

          # Generate predictions
          predictions <- data.table::data.table(
            x = new_data[[x_col]],
            y_pred = fit$back_transform(
              predictions = fit$model_function(x = scaled_x, params = fit$coefficients),
              scale_params = fit$scale_params
            )
          )

          if (!is.null(bounds)) {
            predictions[, y_lower := fit$back_transform(bounds$lower, fit$scale_params)]
            predictions[, y_upper := fit$back_transform(bounds$upper, fit$scale_params)]
          }
          predictions
        }, error = function(e) {
          message("Error scoring model: ", e$message)
          NULL
        })
      })

      names(self$scored_data) <- names(self$fit_results)
      return(self$scored_data)
    },

    #' @description Creates a visual representation of predictions versus actual values for a given fitted model
    #' and a new dataset. This plot helps evaluate the model's performance on unseen data.
    #'
    #' @details The `generate_score_plot` method produces an interactive plot comparing the predicted values
    #' of a fitted model against the actual observed values from the new dataset. The independent variable
    #' (`x_col`) is used on the x-axis, while the predicted and actual dependent variable values are
    #' displayed on the y-axis. This visualization can be used to assess how well the model generalizes
    #' to new data and to identify areas where predictions deviate from observations.
    #'
    #' The plot leverages the `echarts4r` package for interactive and customizable visualizations.
    #'
    #' @param model_name The name of the model to plot.
    #' @param x_col The predictor column in scored data.
    #' @param theme Echarts theme.
    #' @return A plot visualizing the scored data.
    #' @export
    generate_score_plot = function(model_name, x_col, theme = "westeros") {

      # Validate that the model exists in fit_results
      if (!model_name %in% names(self$fit_results)) {
        message("Model '", model_name, "' not found in fit_results. Available models: ",
             paste(names(self$fit_results), collapse = ", "))
        return(NULL)
      }

      # Validate that the model was scored
      if (is.null(self$scored_data[[model_name]])) {
        message("Model '", model_name, "' has not been scored. Please run score_new_data() first.")
        return(NULL)
      }

      # Extract scored predictions
      predictions <- self$scored_data[[model_name]]

      # Create plot
      plot <- echarts4r::e_charts(data = predictions, x) |>
        echarts4r::e_line(y_pred, name = "Predicted", smooth = TRUE, showSymbol = FALSE) |>
        echarts4r::e_title(text = paste("Scored Data: Model -", model_name)) |>
        echarts4r::e_tooltip(trigger = "axis", backgroundColor = "aliceblue") |>
        echarts4r::e_x_axis(name = x_col) |>
        echarts4r::e_y_axis(name = "Predicted Values") |>
        echarts4r::e_legend(right = 120) |>
        echarts4r::e_datazoom(x_index = c(0,1)) |>
        echarts4r::e_toolbox_feature(feature = c("saveAsImage","dataZoom")) |>
        echarts4r::e_theme(name = theme)

      if ("y_lower" %in% names(predictions)) {
        plot <- echarts4r::e_line(e = plot, y_lower, name = "Lower Bound", smooth = TRUE, showSymbol = FALSE, lineStyle = list(type = "dotted")) |>
          echarts4r::e_line(y_upper, name = "Upper Bound", smooth = TRUE, showSymbol = FALSE, lineStyle = list(type = "dotted"))
      }

      self$score_plots[[model_name]] <- plot
      return(plot)
    }
  ),

  private = list(
    simulate_prediction_bounds = function(fit, x_values, lower_bound, upper_bound, n_sim = 1000) {
      params <- fit$coefficients
      se_params <- fit$confidence_intervals$SE
      if (is.null(se_params)) {
        message("se_params is NULL")
        return(NULL)
      }

      # Simulate parameter sets
      sim_matrix <- replicate(n_sim, {
        sim_params <- rnorm(length(params), mean = unlist(params), sd = unlist(se_params))
        param_list <- as.list(sim_params)
        names(param_list) <- names(params)
        fit$model_function(x = x_values, params = param_list)
      })

      # Calculate lower and upper bounds
      tryCatch({
        if (length(x_values) > 1) {
          lower <- apply(sim_matrix, 1, quantile, probs = lower_bound, na.rm = TRUE)
          upper <- apply(sim_matrix, 1, quantile, probs = upper_bound, na.rm = TRUE)
        } else {
          lower <- quantile(x = sim_matrix, probs = lower_bound, na.rm = TRUE)[[1]]
          upper <- quantile(sim_matrix, probs = upper_bound, na.rm = TRUE)[[1]]
        }
      }, error = function(e) {
        message("lower and upper not found: ", e$message)
        NULL
      })

      if (!exists("lower") | !exists("upper")) {
        return(NULL)
      } else {
        return(list(lower = lower, upper = upper))
      }
    }
  )
)
