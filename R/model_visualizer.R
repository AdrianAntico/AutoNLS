#' ModelVisualizer
#'
#' An R6 class to visualize the shapes of various non-linear models for comparison.
#' @export
ModelVisualizer <- R6::R6Class(
  "ModelVisualizer",
  public = list(
    #' @field models A list of non-linear models and their parameterized functions.
    models = NULL,

    #' Initialize the ModelVisualizer class
    #'
    #' @param models A list of models with parameterized functions.
    #' @return A new instance of the ModelVisualizer class.
    initialize = function(models) {
      if (!is.list(models)) stop("models must be a list of parameterized functions.")
      self$models <- models
    },

    #' @param x_range A numeric vector specifying the range of x values to evaluate (e.g., `seq(1, 100, by = 1)`).
    #' @param params A named list of parameters for each model. Defaults to an empty list, which uses default parameters for all models.
    #' @param normalize Logical. If TRUE, normalizes the y values for each model to fall between 0 and 1. Defaults to TRUE.
    #' @return An `echarts4r` object representing the comparison plot.
    generate_comparison_plot = function(x_range = seq(1, 100, by = 1), params = list(), normalize = TRUE) {
      # Validate x_range
      if (!is.numeric(x_range) || length(x_range) == 0) {
        stop("x_range must be a non-empty numeric vector.")
      }

      # Validate params
      invalid_models <- setdiff(names(params), names(self$models))
      if (length(invalid_models) > 0) {
        stop("Invalid models in params: ", paste(invalid_models, collapse = ", "))
      }

      # Generate y values for each model
      plot_data <- data.table::data.table(x = x_range)

      for (model_name in names(self$models)) {
        model_fn <- self$models[[model_name]]
        model_params <- if (!is.null(params[[model_name]])) params[[model_name]] else list()

        # Generate y values using the model function
        plot_data[[model_name]] <- vapply(
          x_range,
          function(x) do.call(model_fn, c(list(x = x), model_params)),
          numeric(1)
        )
      }

      # Normalize y values if requested
      if (normalize) {
        for (model_name in names(self$models)) {
          y_values <- plot_data[[model_name]]
          y_min <- min(y_values, na.rm = TRUE)
          y_max <- max(y_values, na.rm = TRUE)
          plot_data[[model_name]] <- (y_values - y_min) / (y_max - y_min)
        }
      }

      # Reshape data for plotting
      plot_data_long <- data.table::melt(plot_data, id.vars = "x", variable.name = "Model", value.name = "y")

      # Create the plot using grouping
      plot <- echarts4r::e_charts(
        data = plot_data_long |> dplyr::group_by(Model),
        x = x
      ) |>
        echarts4r::e_line(serie = y) |>
        echarts4r::e_title(
          text = if (normalize) "Normalized Comparison of Non-Linear Model Shapes" else "Comparison of Non-Linear Model Shapes"
        ) |>
        echarts4r::e_tooltip(trigger = "axis") |>
        echarts4r::e_legend(
          show = TRUE,
          orient = "horizontal",
          left = "right",
          top = "top"
        ) |>
        echarts4r::e_x_axis(name = "x") |>
        echarts4r::e_y_axis(name = if (normalize) "Normalized y" else "y") |>
        echarts4r::e_datazoom(x_index = c(0,1)) |>
        echarts4r::e_toolbox_feature(feature = c("saveAsImage","dataZoom"))

      return(plot)
    }
  )
)
