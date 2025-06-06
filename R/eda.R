#' @title EDA (Exploratory Data Analysis) Class
#'
#' @description Provides tools for automated exploratory data analysis, including summary
#' statistics, correlation matrices, and customizable visualizations using `echarts4r`.
#'
#' @section Methods:
#' - `initialize(data)`: Initializes the class with a `data.table`.
#' - `summarize()`: Computes summary statistics.
#' - `correlate()`: Computes a correlation matrix for numeric columns.
#' - `visualize_distributions()`: Creates histogram and density visualizations for numeric columns.
#' - `visualize_scatterplots()`: Creates pairwise scatterplots for numeric columns.
#' - `generate_3d_scatter_plot()`: Creates a 3D scatterplot for numeric columns.
#'
#'@export
EDA <- R6::R6Class(
  "EDA",
  public = list(
    #' @field data A `data.table` containing the dataset for analysis.
    data = NULL,
    #' @field summary_stats A `data.table` storing the summary statistics of the dataset.
    summary_stats = NULL,
    #' @field correlation_matrix A correlation matrix for numeric columns.
    correlation_matrix = NULL,
    #' @field plots A list of `echarts4r` plots generated during the analysis.
    plots = list(),

    #' Initialize the EDA class
    #'
    #' @param data A `data.table` containing the dataset for analysis.
    initialize = function(data) {
      if (!"data.table" %in% class(data)) {
        message("Input data must be a data.table object.")
        return(NULL)
      }
      self$data <- data
    },

    #' @description Calculates mean, median, sd, and the count of missing values for each column.
    #'
    #' @return A `data.table` containing the summary statistics.
    #' @export
    summarize = function() {
      # Process numeric columns
      numeric_cols <- names(self$data)[sapply(self$data, is.numeric)]
      numeric_summary <- if (length(numeric_cols) > 0) {
        lapply(numeric_cols, function(col_name) {
          col <- self$data[[col_name]]
          data.table::data.table(
            Variable = col_name,
            Mean = mean(col, na.rm = TRUE),
            Median = median(col, na.rm = TRUE),
            StDev = sd(col, na.rm = TRUE),
            NA_Count = sum(is.na(col))
          )
        })
      } else {
        list()
      }

      # Process categorical columns
      categorical_cols <- names(self$data)[sapply(self$data, function(col) is.factor(col) || is.character(col))]
      categorical_summary <- if (length(categorical_cols) > 0) {
        lapply(categorical_cols, function(col_name) {
          col <- self$data[[col_name]]
          data.table::data.table(
            Variable = col_name,
            Mean = NA_real_,
            Median = NA_real_,
            StDev = NA_real_,
            NA_Count = sum(is.na(col))
          )
        })
      } else {
        list()
      }

      # Combine numeric and categorical summaries
      self$summary_stats <- rbindlist(c(numeric_summary, categorical_summary), fill = TRUE)

      return(self$summary_stats)
    },

    #' @description Calculates both Pearson and Spearman correlations between the `target_col` and all (or listed via `input_cols`) numeric columns.
    #'
    #' @param target_col the target variable in the data set
    #' @param input_cols the independent variables
    #' @return A data.table with the Pearson and Spearman correlation values for each numeric predictor.
    #' @export
    correlate = function(target_col = NULL, input_cols = NULL) {

      # Identify numeric columns excluding the target column
      if(is.null(input_cols)) {
        numeric_cols <- setdiff(names(self$data)[sapply(self$data, is.numeric)], target_col)
        if (length(numeric_cols) == 0) {
          message("No columns are numeric")
          return(NULL)
        }
      } else {
        numeric_cols <- setdiff(names(self$data)[sapply(self$data, is.numeric)], target_col)
        numeric_cols <- numeric_cols[numeric_cols %in% input_cols]
        if (length(numeric_cols) == 0) {
          message("No columns are numeric")
          return(NULL)
        }
      }

      if (length(numeric_cols) > 0) {

        # Compute correlations
        correlation_results <- lapply(numeric_cols, function(col) {
          pearson_corr <- stats::cor(self$data[[col]], self$data[[target_col]], use = "complete.obs", method = "pearson")
          spearman_corr <- stats::cor(self$data[[col]], self$data[[target_col]], use = "complete.obs", method = "spearman")
          list(
            Target = target_col,
            Predictor = col,
            Pearson = pearson_corr,
            Spearman = spearman_corr,
            Difference = pearson_corr - spearman_corr # Difference to indicate nonlinearity
          )
        })

        # Convert results to a data.table
        self$correlation_matrix <- data.table::rbindlist(correlation_results)
      } else {
        self$correlation_matrix <- "No numeric predictors available for correlation with the target."
      }

      return(self$correlation_matrix)
    },

    #' @description Generates histograms for numeric columns and optionally overlays density lines.
    #'
    #' @param input_cols Names of numeric variables to plot
    #' @param title_prefix Character. Prefix for the plot title.
    #' @param bins Integer. Number of bins for the histogram. Defaults to Sturges' formula.
    #' @param add_density Logical. Whether to add a density line. Defaults to `TRUE`.
    #' @param tooltip_trigger "axis"
    #' @param theme Character. Theme for the plot
    #' @param density_opacity numeric. default 0.4
    #' @return A list of `echarts4r` histogram plots.
    #' @export
    visualize_distributions = function(
    input_cols = NULL,
    title_prefix = "Distribution of",
    bins = 20,
    add_density = TRUE,
    tooltip_trigger = "axis",
    theme = "westeros",
    density_opacity = 0.4) {

      # Clear self$plots to avoid mixing states
      self$plots <- list()

      # Identify numeric columns excluding the target column
      if(is.null(input_cols)) {
        numeric_cols <- names(self$data)[sapply(self$data, is.numeric)]
        if (length(numeric_cols) == 0) {
          message("No columns are numeric")
          return(NULL)
        }
      } else {
        numeric_cols <- names(self$data)[sapply(self$data, is.numeric)]
        numeric_cols <- numeric_cols[numeric_cols %in% input_cols]
        if (length(numeric_cols) == 0) {
          message("No input_cols are numeric")
          return(NULL)
        }
      }

      # Validate numeric columns
      if (length(numeric_cols) == 0) {
        return(list())
      }

      for (col in numeric_cols) {
        # Prepare the dataset for plotting
        plot_data <- self$data[, .(Value = get(col))]

        # Validate the column data
        if (nrow(plot_data) == 0 || !is.numeric(plot_data$Value)) {
          message(paste("Column", col, "is not numeric or contains no data."))
          return(NULL)
        }

        # Create histogram with optional density overlay
        plot <- plot_data |>
          echarts4r::e_charts() |> # Initialize the plot
          echarts4r::e_histogram(Value, name = "Histogram", breaks = bins) |>
          echarts4r::e_title(text = paste(title_prefix, col)) |>
          echarts4r::e_tooltip(trigger = tooltip_trigger) |>
          echarts4r::e_theme(theme) |>
          echarts4r::e_x_axis(name = col) |>
          echarts4r::e_y_axis(name = "Hist") |>
          echarts4r::e_legend(show = TRUE, type = "scroll", orient = "horizontal", right = 50, top = 30) |>
          echarts4r::e_datazoom(x_index = c(0,1)) |>
          echarts4r::e_toolbox_feature(feature = c("saveAsImage","dataZoom"))

        if (add_density) {
          plot <- plot |>
            echarts4r::e_density(
              Value,
              areaStyle = list(opacity = density_opacity),
              smooth = TRUE,
              name = "Density",
              y_index = 1) |>
            echarts4r::e_theme(theme)
        }

        # Save the plot
        self$plots[[col]] <- plot
      }
      return(self$plots)
    },

    #' @description Generates scatterplots for all target and input pairs of numeric columns and overlays
    #' fitted lines from Generalized Additive Models (GAM) for different `k` values.
    #'
    #' @param target_col Name of target variable
    #' @param input_cols Names of input variables
    #' @param title_prefix Character. Prefix for the plot title.
    #' @param theme Character. Theme for the plot
    #' @param k_values Numeric vector. Values of `k` (basis dimension) for GAM fits. Defaults to `c(3, 5, 7)`.
    #' @return A list of `echarts4r` scatter plots with GAM fitted lines.
    #' @export
    visualize_scatterplots = function(
    target_col = NULL,
    input_cols = NULL,
    title_prefix = "Scatterplot of",
    theme = "westeros",
    k_values = c(3, 5, 7)) {
      # Check if mgcv is available
      if (!requireNamespace("mgcv", quietly = TRUE)) {
        message("The 'mgcv' package is required for GAM fitting. Please install it.")
        return(NULL)
      }

      if (length(target_col) == 0) {
        message("You need to supply a target_col.")
        return(NULL)
      }

      # Identify numeric columns excluding the target column
      if(is.null(input_cols)) {
        numeric_cols <- setdiff(names(self$data)[sapply(self$data, is.numeric)], target_col)
        if (length(numeric_cols) == 0) {
          message("No columns are numeric")
          return(NULL)
        }
      } else {
        numeric_cols <- setdiff(names(self$data)[sapply(self$data, is.numeric)], target_col)
        numeric_cols <- numeric_cols[numeric_cols %in% input_cols]
        if (length(numeric_cols) == 0) {
          message("No columns are numeric")
          return(NULL)
        }
      }

      # Reset Plots
      self$plots <- list()

      # Ensure there are at least two numeric columns for scatterplots
      if (length(numeric_cols) < 1) {
        message("Not enough numeric columns to create scatterplots.")
        return(NULL)
      }

      # Reset the plots list
      self$plots <- list()

      # Loop through numeric_cols
      for (col in numeric_cols) {
        x_col <- col
        y_col <- target_col

        # Prepare data for plotting
        plot_data <- self$data[, .(X = get(x_col), Y = get(y_col))]

        # Add GAM fitted lines for each `k` value
        if(length(k_values) > 0 && is.numeric(k_values)) {
          for (k in k_values) { # k = k_values[1]
            # Fit GAM model
            gam_model <- mgcv::gam(Y ~ s(X, k = k), data = plot_data)

            # Generate predictions
            plot_data[, paste0("gam_k = ", k) := predict(gam_model, newdata = plot_data)]
          }
        }

        # Generate scatterplot
        plot <- echarts4r::e_charts(data = plot_data, x = X) |>
          echarts4r::e_scatter(Y, name = "Observed Data") |>
          echarts4r::e_title(text = paste(title_prefix, x_col, "and", y_col)) |>
          echarts4r::e_x_axis(name = x_col) |>
          echarts4r::e_y_axis(name = y_col, nameLocation = "middle", nameGap = 45) |>
          echarts4r::e_tooltip(trigger = "axis") |>
          echarts4r::e_theme(name = theme) |>
          echarts4r::e_legend(show = TRUE, type = "scroll", orient = "horizontal", right = 50, top = 30) |>
          echarts4r::e_datazoom(x_index = c(0,1)) |>
          echarts4r::e_toolbox_feature(feature = c("saveAsImage","dataZoom"))

        if(length(k_values) > 0 && is.numeric(k_values)) {
          for (k in k_values) {
            YVar <- paste0("gam_k = ", k)
            plot <- echarts4r::e_line_(e = plot, serie = YVar, smooth = TRUE, showSymbol = FALSE)
          }
        }

        # Save the plot with a unique key
        plot_key <- paste(x_col, y_col, sep = "_vs_")
        if (!plot_key %in% names(self$plots)) {  # Ensure no duplicates
          self$plots[[plot_key]] <- plot
        }
      }

      return(self$plots)
    },

    #' @description Generates a 3D scatter plot for three numeric variables.
    #'
    #' @param input_col1 The name of the first numeric column.
    #' @param input_col2 The name of the second numeric column.
    #' @param target_col The name of the third numeric column, the target variable.
    #' @param rank_values Logical. Whether to transform variables to their percentile ranks. Defaults to TRUE.
    #' @param theme Name of theme for `echarts4r` plots
    #' @return An `echarts4r` 3D scatter plot.
    #' @export
    generate_3d_scatter_plot = function(
    input_col1,
    input_col2,
    target_col,
    rank_values = TRUE,
    theme = "westeros") {
      if (!(input_col1 %in% names(self$data) && input_col2 %in% names(self$data) && target_col %in% names(self$data))) {
        message("Columns not found in the dataset.")
        return(NULL)
      }
      if (!is.numeric(self$data[[input_col1]]) || !is.numeric(self$data[[input_col2]]) || !is.numeric(self$data[[target_col]])) {
        message("All specified columns must be numeric.")
        return(NULL)
      }

      # Optionally rank variables
      if (rank_values) {
        plot_data <- self$data[, .(
          X = frank(get(input_col1), na.last = "keep") / sum(!is.na(get(input_col1))),
          Y = frank(get(input_col2), na.last = "keep") / sum(!is.na(get(input_col2))),
          Z = frank(get(target_col), na.last = "keep") / sum(!is.na(get(target_col)))
        )]
      } else {
        plot_data <- self$data[, .(
          X = get(input_col1),
          Y = get(input_col2),
          Z = get(target_col)
        )]
      }

      # Generate 3D scatter plot
      plot <- plot_data |>
        echarts4r::e_charts(X) |>
        echarts4r::e_scatter_3d(Y, Z, symbol_size = 5) |>
        echarts4r::e_title(text = paste("3D Scatter Plot of", input_col1, input_col2, "and", target_col)) |>
        echarts4r::e_x_axis_3d(name = input_col1) |>
        echarts4r::e_y_axis_3d(name = input_col2) |>
        echarts4r::e_z_axis_3d(name = target_col) |>
        echarts4r::e_theme(theme)

      self$plots[[paste(input_col1, input_col2, target_col, sep = "_vs_")]] <- plot
      return(plot)
    }
  )
)
