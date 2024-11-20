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
#' @import data.table echarts4r R6
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
    #' @examples
    #' library(data.table)
    #' data <- data.table(A = rnorm(100), B = rnorm(100, 10, 5))
    #' eda <- EDA$new(data)
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
    #' @examples
    #' eda$summarize()
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

    #' Compute correlation matrix
    #'
    #' Calculates the correlation matrix for all numeric columns in the dataset.
    #'
    #' @return A correlation matrix or a message if not enough numeric columns are present.
    #' @examples
    #' eda$correlate()
    correlate = function() {
      numeric_cols <- names(self$data)[sapply(self$data, is.numeric)]
      if (length(numeric_cols) > 1) {
        self$correlation_matrix <- stats::cor(self$data[, ..numeric_cols], use = "complete.obs")
      } else {
        self$correlation_matrix <- "Not enough numeric columns for correlation."
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
    #' @examples
    #' eda$visualize_distributions(bins = 20, add_density = TRUE, theme = "dark")
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

    #' Visualize pairwise scatterplots
    #'
    #' Generates scatterplots for all pairs of numeric columns.
    #'
    #' @param title_prefix Character. Prefix for the plot title.
    #' @param theme Character. Theme for the plot (e.g., "light", "dark"). Defaults to `"light"`.
    #' @return A list of `echarts4r` scatter plots.
    #' @examples
    #' eda$visualize_scatterplots(theme = "macarons")
    visualize_scatterplots = function(
    title_prefix = "Scatterplot of",
    theme = "dark") {
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
      for (pair in col_pairs) {
        x_col <- pair[1]
        y_col <- pair[2]

        # Prepare data for plotting
        plot_data <- self$data[, .(X = get(x_col), Y = get(y_col))]

        # Create scatterplot
        plot <- plot_data |>
          echarts4r::e_charts(X) |>
          echarts4r::e_scatter(Y, name = paste(x_col, "vs", y_col)) |>
          echarts4r::e_title(text = paste(title_prefix, x_col, "and", y_col)) |>
          echarts4r::e_x_axis(name = x_col) |>
          echarts4r::e_y_axis(name = y_col) |>
          echarts4r::e_tooltip(trigger = "axis") |>
          echarts4r::e_theme(name = theme)

        # Save the plot with a unique key
        plot_key <- paste(x_col, y_col, sep = "_vs_")
        if (!plot_key %in% names(self$plots)) {  # Ensure no duplicates
          self$plots[[plot_key]] <- plot
        }
      }

      return(self$plots)
    },

    #' Run all EDA methods
    #'
    #' Runs summary statistics, correlation, and visualizations for the dataset.
    #'
    #' @return A list containing summary statistics, correlation matrix, and plots.
    #' @examples
    #' eda$render_all()
    render_all = function(
    dist_title_prefix = "Distribution of",
    dist_bins = 10,
    dist_add_density = TRUE,
    dist_density_color = "#EE6666",
    dist_theme = "light",
    scatter_title_prefix = "Scatterplot of") {
      # Run all methods and store the results
      self$summary_stats <- self$summarize()
      self$correlation_matrix <- self$correlate()

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
