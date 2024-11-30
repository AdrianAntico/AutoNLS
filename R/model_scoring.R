#' NonLinearModelScorer
#'
#' An R6 class to score non-linear regression models on new data
#' and visualize the results.
#' @export
NonLinearModelScorer <- R6::R6Class(
  "NonLinearModelScorer",
  public = list(
    #' @field fit_results A list of fitted model objects.
    fit_results = NULL,

    #' @field scored_data A list of data.tables containing scored data.
    scored_data = list(),

    #' @field score_plots A list of plots visualizing scored data.
    score_plots = list(),

    #' @param fit_results A list of fitted model objects (e.g., output from NonLinearFitter).
    #' @return A new instance of the NonLinearModelScorer class.
    initialize = function(fit_results) {
      if (!is.list(fit_results)) stop("fit_results must be a list of model objects.")
      self$fit_results <- fit_results
    },

    #' @param new_data A data.table containing the new data to score.
    #' @param x_col The predictor column in `new_data`.
    #' @return A list of data.tables with predicted values for each model.
    score_new_data = function(new_data, x_col) {
      if (!data.table::is.data.table(new_data)) stop("new_data must be a data.table.")
      if (!x_col %in% names(new_data)) stop("x_col must exist in the dataset.")

      self$scored_data <- lapply(self$fit_results, function(fit) {
        if (is.null(fit)) return(NULL)

        tryCatch({
          # Generate predictions
          predictions <- data.table::data.table(
            x = new_data[[x_col]],
            y_pred = predict(fit, newdata = new_data)
          )
          predictions
        }, error = function(e) {
          message("Error scoring model: ", e$message)
          NULL
        })
      })

      names(self$scored_data) <- names(self$fit_results)
      return(self$scored_data)
    },

    #' @param model_name The name of the model to plot.
    #' @param new_data The original new data used for scoring.
    #' @param x_col The predictor column in `new_data`.
    #' @param theme Echarts theme
    #' @return A plot visualizing the scored data.
    generate_score_plot = function(model_name, new_data, x_col, theme = "macarons") {
      # Validate x_col exists in new_data
      if (!x_col %in% names(new_data)) stop("x_col must exist in the dataset.")

      # Validate that the model exists in fit_results
      if (!model_name %in% names(self$fit_results)) {
        stop("Model '", model_name, "' not found in fit_results. Available models: ",
             paste(names(self$fit_results), collapse = ", "))
      }

      # Validate that the model was scored
      if (is.null(self$scored_data[[model_name]])) {
        stop("Model '", model_name, "' has not been scored. Please run score_new_data() first.")
      }

      # Extract scored predictions
      predictions <- self$scored_data[[model_name]]

      # Create plot
      plot <- data.table::data.table(
        x = new_data[[x_col]],
        y_pred = predictions$y_pred
      ) |>
        echarts4r::e_charts(x) |>
        echarts4r::e_line(y_pred, name = "Predicted") |>
        echarts4r::e_title(
          text = paste("Scored Data: Model -", model_name)
        ) |>
        echarts4r::e_tooltip(trigger = "axis") |>
        echarts4r::e_x_axis(name = x_col) |>
        echarts4r::e_y_axis(name = "Predicted Values") |>
        echarts4r::e_legend(right = 50) |>
        echarts4r::e_datazoom(x_index = c(0,1)) |>
        echarts4r::e_toolbox_feature(feature = c("saveAsImage","dataZoom")) |>
        echarts4r::e_theme(name = theme)

      self$score_plots[[model_name]] <- plot
      return(plot)
    }
  )
)
