#' NonLinearFitter
#'
#' An R6 class for automatically fitting non-linear regression models.
#' Includes a library of pre-defined models to simplify selection.
#'
#' @export
NonLinearFitter <- R6::R6Class(
  "NonLinearFitter",
  public = list(
    #' @field data A data.table containing the dataset for modeling.
    data = NULL,

    #' @field models A list of non-linear models to test.
    models = NULL,

    #' @field fit_results A list to store the results of model fits.
    fit_results = list(),

    #' @field evaluation_metrics A list to store evaluation metrics for each model.
    evaluation_metrics = list(),

    #' @field plots A list to store plots of model fits.
    plots = list(),

    #' @field model_library A pre-defined library of common non-linear models.
    model_library = list(
      Hill2Model = list(
        description = "Hill equation: models dose-response relationships.",
        formula = y ~ x^b / (a^b + x^b),
        start_params = list(a = 1, b = 1),
        model_function = function(x, params) {
          a <- params[["a"]]
          b <- params[["b"]]
          if (!is.numeric(x)) stop("x must be numeric in model_function.")
          x^b / (a^b + x^b)
        }
      ),
      Hill = list(
        description = "Hill equation: models dose-response relationships.",
        formula = y ~ a * x^b / (c^b + x^b),
        start_params = list(a = 1, b = 1, c = 1),
        model_function = function(x, params) {
          a <- params[["a"]]
          b <- params[["b"]]
          c <- params[["c"]]
          if (!is.numeric(x)) stop("x must be numeric in model_function.")
          a * x^b / (c^b + x^b)
        }
      ),
      Hill5Model = list(
        description = "Five-parameter Hill equation for dose-response.",
        formula = y ~ a * (x^b) / (c^b + x^b) + d + e * x,
        start_params = list(a = 1, b = 1, c = 1, d = 0, e = 0),
        model_function = function(x, params) {
          a <- params[["a"]]
          b <- params[["b"]]
          c <- params[["c"]]
          d <- params[["d"]]
          e <- params[["e"]]
          if (!is.numeric(x)) stop("x must be numeric in model_function.")
          a * (x^b) / (c^b + x^b) + d + e * x
        }
      ),
      HillSwitchpointModel = list(
        description = "Hill equation with a smooth switch point for dose-response.",
        formula = y ~ (1 / (1 + exp(-k * (x - s)))) * (a * (x^b) / (c^b + x^b)) + (1 - (1 / (1 + exp(-k * (x - s))))) * (d * (x^e) / (f^e + x^e)),
        start_params = list(a = 1, b = 1, c = 1, d = 1, e = 1, f = 1, s = 50, k = 10),
        model_function = function(x, params) {
          a <- params[["a"]]
          b <- params[["b"]]
          c <- params[["c"]]
          d <- params[["d"]]
          e <- params[["e"]]
          f <- params[["f"]]
          s <- params[["s"]]
          k <- params[["k"]]  # Slope of the transition
          if (!is.numeric(x)) stop("x must be numeric in model_function.")
          1 / (1 + exp(-k * (x - s))) * (a * (x^b) / (c^b + x^b)) + (1 - 1 / (1 + exp(-k * (x - s)))) * (d * (x^e) / (f^e + x^e))
        }
      ),
      HillQuad = list(
        description = "Quadratic Hill model for dose-response relationships.",
        formula = y ~ a * (x^b) / (c + x^b) + d * (x^2),
        start_params = list(a = 1, b = 1, c = 1, d = 0.01),
        model_function = function(x, params) {
          a <- params[["a"]]
          b <- params[["b"]]
          c <- params[["c"]]
          d <- params[["d"]]
          if (!is.numeric(x)) stop("x must be numeric in model_function.")
          a * (x^b) / (c + x^b) + d * (x^2)
        }
      ),
      Logistic = list(
        description = "Logistic growth model.",
        formula = y ~ a / (1 + exp(-b * (x - c))),
        start_params = list(a = 1, b = 1, c = 50),
        model_function = function(x, params) {
          a <- params[["a"]]
          b <- params[["b"]]
          c <- params[["c"]]
          if (!is.numeric(x)) stop("x must be numeric in model_function.")
          a / (1 + exp(-b * (x - c)))
        }
      ),
      Logistic5Param = list(
        description = "Five-parameter logistic growth model.",
        formula = y ~ d + (a - d) / (1 + (x / c)^b)^g,
        start_params = list(a = 1, b = 1, c = 1, d = 0, g = 1),
        model_function = function(x, params) {
          a <- params[["a"]]
          b <- params[["b"]]
          c <- params[["c"]]
          d <- params[["d"]]
          g <- params[["g"]]
          if (!is.numeric(x)) stop("x must be numeric in model_function.")
          d + (a - d) / (1 + (x / c)^b)^g
        }
      ),
      ExponentialDecay = list(
        description = "Exponential decay model.",
        formula = y ~ a * exp(-b * x),
        start_params = list(a = 1, b = 0.1),
        model_function = function(x, params) {
          a <- params[["a"]]
          b <- params[["b"]]
          if (!is.numeric(x)) stop("x must be numeric in model_function.")
          a * exp(-b * x)
        }
      ),
      ExpDecayPlateau = list(
        description = "Exponential decay with a plateau.",
        formula = y ~ a * exp(-b * x) + c,
        start_params = list(a = 1, b = 0.1, c = 0.1),
        model_function = function(x, params) {
          a <- params[["a"]]
          b <- params[["b"]]
          c <- params[["c"]]
          if (!is.numeric(x)) stop("x must be numeric in model_function.")
          a * exp(-b * x) + c
        }
      ),
      Exp2OrderDecay = list(
        description = "Second-order exponential decay model.",
        formula = y ~ a * exp(-b * x) + c * exp(-d * x),
        start_params = list(a = 1, b = 0.1, c = 0.5, d = 0.05),
        model_function = function(x, params) {
          a <- params[["a"]]
          b <- params[["b"]]
          c <- params[["c"]]
          d <- params[["d"]]
          if (!is.numeric(x)) stop("x must be numeric in model_function.")
          a * exp(-b * x) + c * exp(-d * x)
        }
      ),
      Gompertz = list(
        description = "Gompertz growth model.",
        formula = y ~ a * exp(-b * exp(-c * x)),
        start_params = list(a = 1, b = 1, c = 1),
        model_function = function(x, params) {
          a <- params[["a"]]
          b <- params[["b"]]
          c <- params[["c"]]
          if (!is.numeric(x)) stop("x must be numeric in model_function.")
          a * exp(-b * exp(-c * x))
        }
      ),
      Gompertz4Param = list(
        description = "Four-parameter Gompertz model.",
        formula = y ~ a * exp(-exp(b - c * x)) + d,
        start_params = list(a = 1, b = 1, c = 0.1, d = 0),
        model_function = function(x, params) {
          a <- params[["a"]]
          b <- params[["b"]]
          c <- params[["c"]]
          d <- params[["d"]]
          if (!is.numeric(x)) stop("x must be numeric in model_function.")
          a * exp(-exp(b - c * x)) + d
        }
      ),
      MichaelisMenten = list(
        description = "Michaelis-Menten kinetics.",
        formula = y ~ (Vmax * x) / (Km + x),
        start_params = list(Vmax = 1, Km = 1),
        model_function = function(x, params) {
          Vmax <- params[["Vmax"]]
          Km <- params[["Km"]]
          if (!is.numeric(x)) stop("x must be numeric in model_function.")
          (Vmax * x) / (Km + x)
        }
      ),
      WeibullType1 = list(
        description = "Weibull Type 1 model, used in survival analysis.",
        formula = y ~ a * exp(-exp(b - c * x)),
        start_params = list(a = 1, b = 1, c = 0.1),
        model_function = function(x, params) {
          a <- params[["a"]]
          b <- params[["b"]]
          c <- params[["c"]]
          if (!is.numeric(x)) stop("x must be numeric in model_function.")
          a * exp(-exp(b - c * x))
        }
      ),
      WeibullType2 = list(
        description = "Weibull Type 2 model for sigmoidal data.",
        formula = y ~ a * (1 - exp(-b * x^c)),
        start_params = list(a = 1, b = 0.1, c = 1),
        model_function = function(x, params) {
          a <- params[["a"]]
          b <- params[["b"]]
          c <- params[["c"]]
          if (!is.numeric(x)) stop("x must be numeric in model_function.")
          a * (1 - exp(-b * x^c))
        }
      ),
      Asymptotic = list(
        description = "Asymptotic regression model for limited growth.",
        formula = y ~ a - (a - b) * exp(-c * x),
        start_params = list(a = 1, b = 1, c = 0.1),
        model_function = function(x, params) {
          a <- params[["a"]]
          b <- params[["b"]]
          c <- params[["c"]]
          if (!is.numeric(x)) stop("x must be numeric in model_function.")
          a - (a - b) * exp(-c * x)
        }
      ),
      PowerCurve = list(
        description = "Power curve model for scaling relationships.",
        formula = y ~ a * x^b,
        start_params = list(a = 1, b = 1),
        model_function = function(x, params) {
          a <- params[["a"]]
          b <- params[["b"]]
          if (!is.numeric(x)) stop("x must be numeric in model_function.")
          a * x^b
        }
      ),
      Logarithmic = list(
        description = "Logarithmic model for data leveling off.",
        formula = y ~ a + b * log(x),
        start_params = list(a = 1, b = 1),
        model_function = function(x, params) {
          a <- params[["a"]]
          b <- params[["b"]]
          if (!is.numeric(x)) stop("x must be numeric in model_function.")
          a + b * log(x)
        }
      ),
      RectangularHyperbola = list(
        description = "Rectangular hyperbola for saturation processes.",
        formula = y ~ (a * x) / (b + x),
        start_params = list(a = 1, b = 1),
        model_function = function(x, params) {
          a <- params[["a"]]
          b <- params[["b"]]
          if (!is.numeric(x)) stop("x must be numeric in model_function.")
          (a * x) / (b + x)
        }
      ),
      Richards = list(
        description = "Richards curve: a generalization of logistic growth.",
        formula = y ~ a / (1 + exp(-b * (x - c)))^d,
        start_params = list(a = 1, b = 1, c = 50, d = 1),
        model_function = function(x, params) {
          a <- params[["a"]]
          b <- params[["b"]]
          c <- params[["c"]]
          d <- params[["d"]]
          if (!is.numeric(x)) stop("x must be numeric in model_function.")
          a / (1 + exp(-b * (x - c)))^d
        }
      ),
      ChapmanRichards = list(
        description = "Chapman-Richards model for growth.",
        formula = y ~ a * (1 - exp(-b * x))^c,
        start_params = list(a = 1, b = 0.1, c = 2),
        model_function = function(x, params) {
          a <- params[["a"]]
          b <- params[["b"]]
          c <- params[["c"]]
          if (!is.numeric(x)) stop("x must be numeric in model_function.")
          a * (1 - exp(-b * x))^c
        }
      ),
      HyperbolicTangent = list(
        description = "Hyperbolic tangent model for sigmoidal data.",
        formula = y ~ a * tanh(b * x + c),
        start_params = list(a = 1, b = 0.1, c = 0),
        model_function = function(x, params) {
          a <- params[["a"]]
          b <- params[["b"]]
          c <- params[["c"]]
          if (!is.numeric(x)) stop("x must be numeric in model_function.")
          a * tanh(b * x + c)
        }
      ),
      BetaModel = list(
        description = "Beta model for dose-response with hook effects.",
        formula = y ~ a + (b - a) * (1 + (x / c)^d)^-e,
        start_params = list(a = 1, b = 1, c = 1, d = 1, e = 1),
        model_function = function(x, params) {
          a <- params[["a"]]
          b <- params[["b"]]
          c <- params[["c"]]
          d <- params[["d"]]
          e <- params[["e"]]
          if (!is.numeric(x)) stop("x must be numeric in model_function.")
          a + (b - a) * (1 + (x / c)^d)^-e
        }
      ),
      LinearModel = list(
        description = "Simple linear regression model.",
        formula = y ~ a + b * x,
        start_params = list(a = 0, b = 1),
        model_function = function(x, params) {
          a <- params[["a"]]
          b <- params[["b"]]
          if (!is.numeric(x)) stop("x must be numeric in model_function.")
          a + b * x
        }
      )
    ),

    #' Initialize the NonLinearFitter class
    #'
    #' @param data A data.table containing the dataset for modeling.
    #' Must include the predictor and response variable columns.
    #' @return A new instance of the NonLinearFitter class.
    initialize = function(data) {
      if (!data.table::is.data.table(data)) stop("Input data must be a data.table")
      self$data <- data
      self$models <- list()
    },

    #' @description Retrieves a list of all non-linear models available for fitting in the `NonLinearFitter` class,
    #' along with their descriptions and key details. This method provides an overview of the models
    #' users can select for analysis.
    #'
    #' @details This method returns a list containing information about the supported
    #' non-linear models, including their names, descriptions, and the formulas they use. It serves
    #' as a convenient reference for users to understand the available options and select the most
    #' appropriate models for their dataset and objectives.
    #'
    #' @return A list summarizing available models.
    #' @export
    list_models = function() {
      data.table::data.table(
        Model = names(self$model_library),
        Description = sapply(self$model_library, function(x) x$description),
        Formula = sapply(self$model_library, function(x) deparse(x$formula))
      )
    },

    #' @description Adds a specified non-linear model to the list of models to be fitted. This method allows users
    #' to include predefined models or define their own custom models for analysis.
    #'
    #' @details This method enables the inclusion of both built-in and custom models in the fitting
    #' process. Users can select a predefined model from the library or provide the necessary
    #' components to define a custom model, including a formula, starting parameters, and a function
    #' to calculate predictions.
    #'
    #' For custom models, the following components must be provided:
    #' - `name`: Name of the model
    #' - `formula`: A mathematical representation of the model.
    #' - `start_params`: A named list of starting parameter values for optimization.
    #' - `model_function`: A function that takes input `x` and parameter values as arguments
    #'   and returns the predicted `y` values.
    #'
    #' @param name The name of the model (e.g., "Hill").
    #' @param formula The non-linear formula for the model (optional if using pre-defined model).
    #' @param start_params A list of starting parameters for the model (optional if using pre-defined model).
    #' @param model_function A function used in fitting and prediction. See model_library for examples
    #' @return NULL
    #' @export
    add_model = function(name, formula = NULL, start_params = NULL, model_function = NULL) {
      if (is.null(formula) || is.null(start_params) || is.null(model_function)) {
        if (!name %in% names(self$model_library)) {
          stop("Model not found in library. Use list_models() to see available models.")
        }
        model_info <- self$model_library[[name]]
        formula <- model_info$formula
        start_params <- model_info$start_params
        model_function <- model_info$model_function
      }
      self$models[[name]] <- list(
        formula = formula,
        start_params = start_params,
        model_function = model_function
      )
    },

    #' @description `fit_models()` first standardizes your data before fitting the model.
    #' The fitting method depends on whether a `weights_col` is provided: `nls()` is used
    #' for unweighted fitting, while `optim()` is used for weighted fitting. The returned
    #' parameters are based on the standardized data. However, when scoring models, the
    #' results are back-transformed to align with the original data scale.
    #'
    #' @details
    #' The choice of fitting method depends on the arguments:
    #' - If `force_optim = TRUE`, the function will use `optim()` regardless of whether weights are supplied.
    #' - If `weights` is supplied, `optim()` will be used even if `force_optim = FALSE`.
    #' - If neither `force_optim` nor `weights` is supplied, the function defaults to `nls()` for unweighted fitting.
    #'
    #' **Behavior Examples**:
    #' 1. **Default behavior**: `nls()` is used when `weights = NULL` and `force_optim = FALSE`.
    #' 2. **Weighted fitting**: `optim()` is used when `weights` is provided, even if `force_optim = FALSE`.
    #' 3. **Forced optimization**: `optim()` is used when `force_optim = TRUE`, regardless of whether `weights` is supplied.
    #'
    #' @param x_col The name of the predictor variable.
    #' @param y_col The name of the response variable.
    #' @param weights_col The name of the weights variable.
    #' @param control A list of control parameters for the optimizer, such as `maxiter`.
    #' Default is `list(maxiter = 200)`.
    #' @param force_optim Logical; if `TRUE`, forces the use of `optim()` regardless of whether weights are supplied. Defaults to `FALSE`.
    #' @param ... Additional arguments to be passed to the underlying fitting functions
    #' (`nlsLM` for unweighted models or `optim` for weighted models). Examples include
    #' `trace`, `lower`, and `upper` for `nlsLM`, or `reltol`, `parscale`, and others for `optim`.
    #'
    #' @return A list of fitted model objects.
    #' @export
    fit_models = function(x_col, y_col, weights_col = NULL, control = list(maxiter = 1024), force_optim = FALSE, ...) {

      if (is.null(self$models) || length(self$models) == 0) {
        stop("No models to fit. Use add_model() to add models.")
      }

      if (!all(c(x_col, y_col) %in% names(self$data))) {
        stop("x_col and y_col must exist in the dataset.")
      }

      # Extract weights if weights_col is specified
      if (!is.null(weights_col)) {
        weights_vector <- self$data[[weights_col]]
        if(all(weights_vector) == 1) {
          standardized_weights_vector <- weights_vector
        } else {
          standardized_weights_vector <- weights_vector / sum(weights_vector, na.rm = TRUE)
        }
        if (any(is.na(standardized_weights_vector))) stop("Weights contain NA values.")
      } else {
        standardized_weights_vector <- NULL
      }

      # Ensure no missing values in weights (only if weights are provided)
      if (!is.null(standardized_weights_vector) && any(is.na(standardized_weights_vector))) {
        stop("Weights column contains missing values.")
      }

      # Create a copy of the data with renamed columns for fitting
      temp_data <- data.table::copy(self$data)
      data.table::setnames(temp_data, old = c(x_col, y_col), new = c("x", "y"))

      # Create a list of scale parameters
      scale_params <- list(
        min_x = min(temp_data$x, na.rm = TRUE),
        max_x = max(temp_data$x, na.rm = TRUE),
        min_y = min(temp_data$y, na.rm = TRUE),
        max_y = max(temp_data$y, na.rm = TRUE),
        scale_factor_x = max(temp_data$x, na.rm = TRUE) - min(temp_data$x, na.rm = TRUE),
        scale_factor_y = max(temp_data$y, na.rm = TRUE) - min(temp_data$y, na.rm = TRUE)
      )

      # Create updated data.table with scaled values
      temp_data_scaled <- data.table::copy(temp_data)
      temp_data_scaled[, x := (x - scale_params$min_x) / (scale_params$max_x - scale_params$min_x)]
      temp_data_scaled[, y := (y - scale_params$min_y) / (scale_params$max_y - scale_params$min_y)]

      # Fit model
      self$fit_results <- lapply(names(self$models), function(model_name) {
        model <- self$models[[model_name]]

        # Use the unaltered formula from the model library
        formula <- model$formula

        # Fit model with or without weights
        fit <- tryCatch({
          if (is.null(standardized_weights_vector) && !force_optim) {

            # Unweighted fitting
            model_fit <- minpack.lm::nlsLM(
              formula = formula,
              data = temp_data_scaled,
              start = model$start_params,
              control = control,
              ...
            )

            # Store coefficients
            model_fit$coefficients <- coef(model_fit)

            # Compute standard errors
            vcov_matrix <- tryCatch({
              summary(model_fit)$cov.unscaled
            }, error = function(e) {
              message("Unable to compute covariance matrix: ", e$message)
              return(NULL)
            })

            # Compute confidence intervals (95% default)
            params <- coef(model_fit)
            z_alpha <- qnorm(0.95)

            # Residual variance (sigma^2)
            residuals <- model_fit$m$resid()
            n <- length(residuals)  # Number of observations
            p <- length(coef(model_fit))  # Number of parameters
            rss <- sum(residuals^2)  # Residual sum of squares
            sigma_squared <- rss / (n - p)

            if (!is.null(vcov_matrix)) {
              standard_errors <- sqrt(diag(vcov_matrix) * sigma_squared)  # Standard errors
            } else {
              standard_errors <- NULL
            }

            if (!is.null(standard_errors)) {
              lower_bound <- params - z_alpha * standard_errors
              upper_bound <- params + z_alpha * standard_errors
            } else {
              lower_bound <- NULL
              upper_bound <- NULL
            }

            # Store confidence interval as a data.table
            model_fit$confidence_intervals <- data.table::data.table(
              Parameter = names(params),
              Estimate = params
            )

            if (!is.null(standard_errors)) {
              model_fit$confidence_intervals[, `Lower Bound` := lower_bound]
              model_fit$confidence_intervals[, `Upper Bound` := upper_bound]
              model_fit$confidence_intervals[, SE := standard_errors]
            }

          } else {

            # Weighted fitting using custom optimization
            result_params <- private$optimize_with_weights(
              x = temp_data_scaled$x,
              y = temp_data_scaled$y,
              weights = standardized_weights_vector,
              model = model$model_function,
              start_params = model$start_params,
              ...
            )

            model_fit <- list(
              coefficients = result_params$params,
              hessian = result_params$hessian,
              formula = formula,
              residuals = temp_data_scaled$y - model$model_function(temp_data_scaled$x, result_params$params),
              fitted.values = model$model_function(temp_data_scaled$x, result_params$params),
              model_function = model$model_function,
              weights = standardized_weights_vector
            )

            # Add confidence intervals
            model_fit$confidence_intervals <- tryCatch({
              private$compute_confidence_intervals(model_fit)
            }, error = function(e) {
              message("confidence intervals failed to build: ", e$message)
            })

            # Set the class properly
            class(model_fit) <- "custom_nls"
          }

          # Attach formula to fit object
          model_fit$formula <- formula
          model_fit
        }, error = function(e) {
          message("Error fitting model '", model_name, "': ", e$message)
          NULL
        })

        if (!is.null(fit)) {
          message("Successfully fitted model: ", model_name)
        }

        if(!is.null(fit)) {
          # Attach scale parameters to the fit object
          fit$scale_params <- scale_params
          fit$original_x_col <- x_col
          fit$original_y_col <- y_col
          fit$model_function <- model$model_function
          fit$scaled_data <- temp_data_scaled
          fit$back_transform <- function(predictions, scale_params) {
            if (is.null(scale_params$scale_factor_y) || is.null(scale_params$min_y)) {
              stop("Error: Scale factor or minimum value is missing.")
            }

            # Back-transform the predictions from scaled to original space
            predictions * scale_params$scale_factor_y + scale_params$min_y
          }
          fit
        } else {
          fit
        }
      })

      # Name the list using the model names
      names(self$fit_results) <- names(self$models)
      return(self$fit_results)
    },

    #' @description Creates a visual comparison of multiple fitted non-linear models against the observed data.
    #' This method helps evaluate the performance and fit of different models in a single, intuitive plot.
    #'
    #' @details This method overlays the predictions of selected fitted models onto the observed data
    #' across a specified range of the independent variable. The output is an interactive plot
    #' generated using the `echarts4r` package, allowing users to visually assess the goodness of fit,
    #' trends, and differences among models.
    #'
    #' By default, predictions are normalized to allow fair comparison across models with different
    #' scales. Users can customize the plotting range and apply specific visual themes for enhanced
    #' interpretability.
    #'
    #' @param x_range A numeric vector specifying the range of x values to evaluate.
    #' @param normalize Logical. If TRUE, normalizes the y values for each model to fall between 0 and 1.
    #' Defaults to TRUE.
    #' @param theme A string specifying the plot theme (e.g., "macarons").
    #' @return An `echarts4r` object representing the comparison plot.
    #' @export
    model_comparison_plot = function(
    x_range = seq(1, 100, by = 1),
    normalize = TRUE,
    theme = "westeros") {
      if (is.null(self$models) || length(self$models) == 0) {
        stop("No models available for visualization. Use add_model() to add models.")
      }

      plot_data <- data.table::data.table(x = x_range)

      for (model_name in names(self$models)) {
        model_fn <- self$model_library[[model_name]]$formula[[3]]
        start_params <- self$model_library[[model_name]]$start_params

        # Evaluate the model function
        plot_data[[model_name]] <- vapply(
          x_range,
          function(x) eval(model_fn, envir = c(list(x = x), start_params)),
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
        # echarts4r::e_tooltip(trigger = "axis") |>
        echarts4r::e_theme(name = theme) |>
        echarts4r::e_legend(
          type = "scroll",
          orient = "vertical",
          right = 50,
          top = 60,
          height = "240px",
          textStyle = list(fontWeight = "bold")) |>
        echarts4r::e_x_axis(name = "x") |>
        echarts4r::e_y_axis(name = if (normalize) "Normalized y" else "y") |>
        echarts4r::e_datazoom(x_index = c(0,1)) |>
        echarts4r::e_toolbox_feature(feature = c("saveAsImage","dataZoom"))

      return(plot)
    }
  ),

  private = list(
    optimize_with_weights = function(x, y, weights, model, start_params, ...) {

      # Convert start_params into a named vector
      params <- unlist(start_params)

      # Define the weighted residual sum of squares function
      wrss <- function(params_vec) {
        # Reconstruct parameters as a named list
        params_list <- as.list(params_vec)
        names(params_list) <- names(start_params)

        # Compute predictions using the model
        predicted <- model(x = x, params = params_list)

        # Ensure valid predictions
        if (length(predicted) != length(y)) {
          stop("Predicted values do not match observed values in length.")
        }

        # Calculate weighted residuals
        residuals <- y - predicted

        if (!is.null(weights)) {
          sum(weights * residuals^2)  # Return WRSS
        } else {
          sum(residuals^2)  # Return WRSS
        }
      }

      # Use optim() for minimization
      result <- optim(
        par = params,
        fn = wrss,
        hessian = TRUE,
        method = "BFGS",
        control = list(maxit = 5000, reltol = 1e-9),
        ...
      )

      if (result$convergence != 0) {
        stop("Optimization did not converge for the weighted model.")
      }
      return(list(
        params = result$par,
        hessian = result$hessian
      ))
    },
    compute_confidence_intervals = function(fit) {
      if (is.null(fit$hessian)) {
        return(NULL)  # No confidence intervals if Hessian is not available
      }

      # Get parameter coefficients
      params <- fit$coefficients

      # Compute the variance-covariance matrix
      if (!is.null(params)) {
        inv_hessian <- tryCatch({
          solve(fit$hessian)
        }, error = function(e) {
          message("Failed to compute confidence intervals: ", e$message)
          NULL
        })
      }

      if (!is.null(inv_hessian)) {
        # Residual variance (sigma^2)
        n <- length(fit$residuals)  # Number of observations
        p <- length(fit$coefficients)  # Number of parameters
        rss <- sum(fit$residuals^2)  # Residual sum of squares
        sigma_squared <- rss / (n - p)

        # Standard errors
        standard_errors <- sqrt(diag(sigma_squared * inv_hessian))

        # Confidence level
        z_alpha <- qnorm(0.975)  # 95% confidence interval

        # Compute bounds
        lower_bound <- params - z_alpha * standard_errors
        upper_bound <- params + z_alpha * standard_errors

        # Return a clean data frame
        return(data.table::data.table(
          Parameter = names(params),
          Estimate = params,
          `Lower Bound` = lower_bound,
          `Upper Bound` = upper_bound,
          SE = standard_errors
        ))
      } else {
        return(data.table::data.table(Parameter = names(params), Estimate = params))
      }
    }
  )
)
