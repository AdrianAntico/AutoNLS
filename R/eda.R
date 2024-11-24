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
            Mean = list(mean(col, na.rm = TRUE)),
            Median = list(median(col, na.rm = TRUE)),
            Variance = list(var(col, na.rm = TRUE)),
            NA_Count = list(sum(is.na(col)))
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
            Mean = list(NA_real_),
            Median = list(NA_real_),
            Variance = list(NA_real_),
            NA_Count = list(sum(is.na(col)))
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
    #' @return A data.table with the Pearson and Spearman correlation values for each numeric predictor.
    correlate = function(target_col = "y") {
      # Identify numeric columns excluding the target column
      numeric_cols <- setdiff(names(self$data)[sapply(self$data, is.numeric)], target_col)

      if (length(numeric_cols) > 0) {
        # Compute correlations
        correlation_results <- lapply(numeric_cols, function(col) {
          pearson_corr <- stats::cor(self$data[[col]], self$data[[target_col]], use = "complete.obs", method = "pearson")
          spearman_corr <- stats::cor(self$data[[col]], self$data[[target_col]], use = "complete.obs", method = "spearman")
          list(
            Predictor = col,
            Pearson = pearson_corr,
            Spearman = spearman_corr,
            Difference = abs(pearson_corr - spearman_corr) # Difference to indicate nonlinearity
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
    #' @param title_prefix Character. Prefix for the plot title.
    #' @param bins Integer. Number of bins for the histogram. Defaults to Sturges' formula.
    #' @param add_density Logical. Whether to add a density line. Defaults to `TRUE`.
    #' @param density_color Character. Color for the density line. Defaults to `"#EE6666"`.
    #' @param tooltip_trigger "axis"
    #' @param theme Character. Theme for the plot (e.g., "light", "dark"). Defaults to `"light"`.
    #' @param density_opacity numeric. default 0.4
    #' @return A list of `echarts4r` histogram plots.
    visualize_distributions = function(
    title_prefix = "Distribution of",
    bins = NULL,
    add_density = TRUE,
    density_color = "#EE6666",
    tooltip_trigger = "axis",
    theme = "dark",
    density_opacity = 0.4) {
      numeric_cols <- names(self$data)[sapply(self$data, is.numeric)]

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
          echarts4r::e_y_axis(name = "Density")

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
    #' @param title_prefix Character. Prefix for the plot title.
    #' @param theme Character. Theme for the plot (e.g., "light", "dark"). Defaults to `"light"`.
    #' @param k_values Numeric vector. Values of `k` (basis dimension) for GAM fits. Defaults to `c(3, 5, 7)`.
    #' @return A list of `echarts4r` scatter plots with GAM fitted lines.
    visualize_scatterplots = function(
    title_prefix = "Scatterplot of",
    theme = "dark",
    k_values = c(3, 5, 7)
    ) {
      # Check if mgcv is available
      if (!requireNamespace("mgcv", quietly = TRUE)) {
        stop("The 'mgcv' package is required for GAM fitting. Please install it.")
      }

      # Get numeric columns
      numeric_cols <- names(self$data)[sapply(self$data, is.numeric)]

      # Ensure there are at least two numeric columns for scatterplots
      if (length(numeric_cols) < 2) {
        stop("Not enough numeric columns to create scatterplots.")
      }

      # Reset the plots list
      self$plots <- list()

      # Generate all unique combinations of numeric columns
      col_pairs <- combn(numeric_cols, 2, simplify = FALSE)

      # Loop through column pairs
      for (pair in col_pairs) { # pair = col_pairs[[1]]
        x_col <- pair[1]
        y_col <- pair[2]

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
          echarts4r::e_y_axis(name = y_col) |>
          echarts4r::e_tooltip(trigger = "axis") |>
          echarts4r::e_theme(name = theme)

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
    },

    #' Render All Visualizations
    #'
    #' This method generates all visualizations, including distributions and scatterplots.
    #'
    #' @param dist_title_prefix Prefix for titles of distribution plots.
    #' @param dist_bins Number of bins for histograms in distribution plots.
    #' @param dist_add_density Logical. Whether to overlay a density line on histograms.
    #' @param dist_density_color Color for the density line.
    #' @param dist_theme Visualization theme for the distribution plots.
    #' @param scatter_title_prefix Prefix for titles of scatterplot visualizations.
    #' @return A list of generated plots.
    render_all = function(
    y_col = NULL,
    dist_title_prefix = "Distribution of",
    dist_bins = 10,
    dist_add_density = TRUE,
    dist_density_color = "#EE6666",
    dist_theme = "light",
    scatter_title_prefix = "Scatterplot of") {
      # Run all methods and store the results
      self$summary_stats <- self$summarize()
      self$correlation_matrix <- self$correlate(target_col = y_col)

      # Generate plots
      distribution_plots <- self$visualize_distributions(
        title_prefix = dist_title_prefix,
        bins = dist_bins,
        add_density = dist_add_density,
        density_color = dist_density_color,
        theme = dist_theme
      )

      scatter_plots <- self$visualize_scatterplots(
        title_prefix = scatter_title_prefix
      )

      # Combine all plots into a single flat list
      all_plots <- c(distribution_plots, scatter_plots)

      # Combine results into a single list
      results <- list(
        Summary = self$summary_stats,
        Correlation = self$correlation_matrix,
        Plots = all_plots # Flattened list of all plots
      )

      return(results)
    }
  )
)
