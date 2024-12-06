#' EDA (Exploratory Data Analysis) Class
#'
#' Provides tools for automated exploratory data analysis, including summary
#' statistics, correlation matrices, and customizable visualizations using `echarts4r`.
#'
#' @section Methods:
#' - `initialize(data)`: Initializes the class with a `data.table`.
#' - `summarize()`: Computes summary statistics.
#' - `correlate()`: Computes a correlation matrix for numeric columns.
#' - `visualize_distributions()`: Creates histogram and density visualizations for numeric columns.
#' - `visualize_scatterplots()`: Creates pairwise scatterplots for numeric columns.
#' - `render_all()`: Runs all methods and returns their results.
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
        stop("Input data must be a data.table object.")
      }
      self$data <- data
    },

    #' Compute summary statistics
    #'
    #' Calculates mean, median, variance, and the count of missing values for each column.
    #'
    #' @return A `data.table` containing the summary statistics.
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
            Variance = var(col, na.rm = TRUE),
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
            Variance = NA_real_,
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

    #' Compute correlation with the target variable
    #'
    #' Calculates both Pearson and Spearman correlations between all numeric columns (excluding the target variable) and the target variable.
    #'
    #' @param target_col the target variable in the data set
    #' @param input_cols the independent variables
    #' @return A data.table with the Pearson and Spearman correlation values for each numeric predictor.
    correlate = function(target_col = NULL, input_cols = NULL) {

      # Identify numeric columns excluding the target column
      if(is.null(input_cols)) {
        numeric_cols <- setdiff(names(self$data)[sapply(self$data, is.numeric)], target_col)
        if (length(numeric_cols) == 0) {
          stop("No columns are numeric")
        }
      } else {
        numeric_cols <- setdiff(names(self$data)[sapply(self$data, is.numeric)], target_col)
        numeric_cols <- numeric_cols[numeric_cols %in% input_cols]
        if (length(numeric_cols) == 0) {
          stop("No columns are numeric")
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

    #' Visualize distributions with histograms and optional density lines
    #'
    #' Generates histograms for numeric columns and optionally overlays density lines.
    #'
    #' @param input_cols Names of numeric variables to plot
    #' @param title_prefix Character. Prefix for the plot title.
    #' @param bins Integer. Number of bins for the histogram. Defaults to Sturges' formula.
    #' @param add_density Logical. Whether to add a density line. Defaults to `TRUE`.
    #' @param tooltip_trigger "axis"
    #' @param theme Character. Theme for the plot (e.g., "light", "dark"). Defaults to `"light"`.
    #' @param density_opacity numeric. default 0.4
    #' @return A list of `echarts4r` histogram plots.
    visualize_distributions = function(
    input_cols = NULL,
    title_prefix = "Distribution of",
    bins = 20,
    add_density = TRUE,
    tooltip_trigger = "axis",
    theme = "dark",
    density_opacity = 0.4) {

      # Clear self$plots to avoid mixing states
      self$plots <- list()

      # Identify numeric columns excluding the target column
      if(is.null(input_cols)) {
        numeric_cols <- names(self$data)[sapply(self$data, is.numeric)]
        if (length(numeric_cols) == 0) {
          stop("No columns are numeric")
        }
      } else {
        numeric_cols <- names(self$data)[sapply(self$data, is.numeric)]
        numeric_cols <- numeric_cols[numeric_cols %in% input_cols]
        if (length(numeric_cols) == 0) {
          stop("No input_cols are numeric")
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
          stop(paste("Column", col, "is not numeric or contains no data."))
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

    #' Visualize pairwise scatterplots with GAM fits
    #'
    #' Generates scatterplots for all pairs of numeric columns and overlays
    #' fitted lines from Generalized Additive Models (GAM) for different `k` values.
    #'
    #' @param target_col Name of target variable
    #' @param input_cols Names of input variables
    #' @param title_prefix Character. Prefix for the plot title.
    #' @param theme Character. Theme for the plot (e.g., "light", "dark"). Defaults to `"light"`.
    #' @param k_values Numeric vector. Values of `k` (basis dimension) for GAM fits. Defaults to `c(3, 5, 7)`.
    #' @return A list of `echarts4r` scatter plots with GAM fitted lines.
    visualize_scatterplots = function(
    target_col = NULL,
    input_cols = NULL,
    title_prefix = "Scatterplot of",
    theme = "dark",
    k_values = c(3, 5, 7)) {
      # Check if mgcv is available
      if (!requireNamespace("mgcv", quietly = TRUE)) {
        stop("The 'mgcv' package is required for GAM fitting. Please install it.")
      }

      if (length(target_col) == 0) {
        stop("You need to supply a target_col.")
      }

      # Identify numeric columns excluding the target column
      if(is.null(input_cols)) {
        numeric_cols <- setdiff(names(self$data)[sapply(self$data, is.numeric)], target_col)
        if (length(numeric_cols) == 0) {
          stop("No columns are numeric")
        }
      } else {
        numeric_cols <- setdiff(names(self$data)[sapply(self$data, is.numeric)], target_col)
        numeric_cols <- numeric_cols[numeric_cols %in% input_cols]
        if (length(numeric_cols) == 0) {
          stop("No columns are numeric")
        }
      }

      # Reset Plots
      self$plots <- list()

      # Ensure there are at least two numeric columns for scatterplots
      if (length(numeric_cols) < 1) {
        stop("Not enough numeric columns to create scatterplots.")
      }

      # Reset the plots list
      self$plots <- list()

      # Loop through column pairs
      for (col in numeric_cols) { # pair = col_pairs[[1]]
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
            plot <- echarts4r::e_line_(e = plot, serie = YVar, smooth = TRUE)
          }
        }

        # Save the plot with a unique key
        plot_key <- paste(x_col, y_col, sep = "_vs_")
        if (!plot_key %in% names(self$plots)) {  # Ensure no duplicates
          self$plots[[plot_key]] <- plot
        }
      }

      return(self$plots)
    }
  )
)
