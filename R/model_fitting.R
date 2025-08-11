#' ModelFitter
#'
#' An R6 class for automatically fitting non-linear regression models.
#' Includes a library of pre-defined models to simplify selection.
#'
#' @export
ModelFitter <- R6::R6Class(
  "ModelFitter",
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
          if (!is.numeric(x)) {
            message("x must be numeric in model_function.")
            return(NULL)
          }
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
          if (!is.numeric(x)) {
            message("x must be numeric in model_function.")
            return(NULL)
          }
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
          if (!is.numeric(x)) {
            message("x must be numeric in model_function.")
            return(NULL)
          }
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
          if (!is.numeric(x)) {
            message("x must be numeric in model_function.")
            return(NULL)
          }
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
          if (!is.numeric(x)) {
            message("x must be numeric in model_function.")
            return(NULL)
          }
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
          if (!is.numeric(x)) {
            message("x must be numeric in model_function.")
            return(NULL)
          }
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
          if (!is.numeric(x)) {
            message("x must be numeric in model_function.")
            return(NULL)
          }
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
          if (!is.numeric(x)) {
            message("x must be numeric in model_function.")
            return(NULL)
          }
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
          if (!is.numeric(x)) {
            message("x must be numeric in model_function.")
            return(NULL)
          }
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
          if (!is.numeric(x)) {
            message("x must be numeric in model_function.")
            return(NULL)
          }
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
          if (!is.numeric(x)) {
            message("x must be numeric in model_function.")
            return(NULL)
          }
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
          if (!is.numeric(x)) {
            message("x must be numeric in model_function.")
            return(NULL)
          }
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
          if (!is.numeric(x)) {
            message("x must be numeric in model_function.")
            return(NULL)
          }
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
          if (!is.numeric(x)) {
            message("x must be numeric in model_function.")
            return(NULL)
          }
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
          if (!is.numeric(x)) {
            message("x must be numeric in model_function.")
            return(NULL)
          }
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
          if (!is.numeric(x)) {
            message("x must be numeric in model_function.")
            return(NULL)
          }
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
          if (!is.numeric(x)) {
            message("x must be numeric in model_function.")
            return(NULL)
          }
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
          if (!is.numeric(x)) {
            message("x must be numeric in model_function.")
            return(NULL)
          }
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
          if (!is.numeric(x)) {
            message("x must be numeric in model_function.")
            return(NULL)
          }
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
          if (!is.numeric(x)) {
            message("x must be numeric in model_function.")
            return(NULL)
          }
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
          if (!is.numeric(x)) {
            message("x must be numeric in model_function.")
            return(NULL)
          }
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
          if (!is.numeric(x)) {
            message("x must be numeric in model_function.")
            return(NULL)
          }
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
          if (!is.numeric(x)) {
            message("x must be numeric in model_function.")
            return(NULL)
          }
          a + (b - a) * (1 + (x / c)^d)^-e
        }
      ),
      StretchedExponential = list(
        description = "Decay function",
        formula = y ~ d + (a - d)*exp(-(x/b)^c),
        start_params = list(a = 1, b = 1, c = 1, d = 1),
        model_function = function(x, params) {
          a <- params[["a"]]
          b <- params[["b"]]
          c <- params[["c"]]
          d <- params[["d"]]
          if (!is.numeric(x)) {
            message("x must be numeric in model_function.")
            return(NULL)
          }
          d + (a - d)*exp(-(x/b)^c)
        }
      ),
      HyperbolicDecay = list(
        description = "Decay function",
        formula = y ~ d + (a-d)/(1 + x/b^c),
        start_params = list(a = 1, b = 1, c = 1, d = 1),
        model_function = function(x, params) {
          a <- params[["a"]]
          b <- params[["b"]]
          c <- params[["c"]]
          d <- params[["d"]]
          if (!is.numeric(x)) {
            message("x must be numeric in model_function.")
            return(NULL)
          }
          d + (a-d)/(1 + x/b^c)
        }
      ),
      GompertzDecay = list(
        description = "Decay function",
        formula = y ~ d + (a-d)*exp(-b * exp(-c*x)),
        start_params = list(a = 1, b = 1, c = 1, d = 1),
        model_function = function(x, params) {
          a <- params[["a"]]
          b <- params[["b"]]
          c <- params[["c"]]
          d <- params[["d"]]
          if (!is.numeric(x)) {
            message("x must be numeric in model_function.")
            return(NULL)
          }
          d + (a-d)*exp(-b * exp(-c*x))
        }
      ),
      InverseHill = list(
        description = "Decay function",
        formula = y ~ d + (a-d) * (b^c / (b^c + x^c)),
        start_params = list(a = 1, b = 1, c = 1, d = 1),
        model_function = function(x, params) {
          a <- params[["a"]]
          b <- params[["b"]]
          c <- params[["c"]]
          d <- params[["d"]]
          if (!is.numeric(x)) {
            message("x must be numeric in model_function.")
            return(NULL)
          }
          d + (a-d) * (b^c / (b^c + x^c))
        }
      ),
      ShiftedExponentialDecay = list(
        description = "Decay function",
        formula = y ~ a * exp(-b*x) + c,
        start_params = list(a = 1, b = 1, c = 1),
        model_function = function(x, params) {
          a <- params[["a"]]
          b <- params[["b"]]
          c <- params[["c"]]
          if (!is.numeric(x)) {
            message("x must be numeric in model_function.")
            return(NULL)
          }
          a * exp(-b*x) + c
        }
      ),
      NegativePowerFunction = list(
        description = "Decay function",
        formula = y ~ a / ((x + d)^b) + c,
        start_params = list(a = 1, b = 1, c = 1, d = 1),
        model_function = function(x, params) {
          a <- params[["a"]]
          b <- params[["b"]]
          c <- params[["c"]]
          d <- params[["d"]]
          if (!is.numeric(x)) {
            message("x must be numeric in model_function.")
            return(NULL)
          }
          a / ((x + d)^b) + c
        }
      ),
      NegativeLogisticDecay = list(
        description = "Decay function",
        formula = y ~ a / (1 + exp(b*(x - c))) + d,
        start_params = list(a = 1, b = 1, c = 1, d = 1),
        model_function = function(x, params) {
          a <- params[["a"]]
          b <- params[["b"]]
          c <- params[["c"]]
          d <- params[["d"]]
          if (!is.numeric(x)) {
            message("x must be numeric in model_function.")
            return(NULL)
          }
          a / (1 + exp(b*(x - c))) + d
        }
      ),
      LogLinearDecay = list(
        description = "Decay function",
        formula = y ~ a - b * log(x + c),
        start_params = list(a = 1, b = 1, c = 1),
        model_function = function(x, params) {
          a <- params[["a"]]
          b <- params[["b"]]
          c <- params[["c"]]
          if (!is.numeric(x)) {
            message("x must be numeric in model_function.")
            return(NULL)
          }
          a - b * log(x + c)
        }
      ),
      PolynomialDecay = list(
        description = "Decay function",
        formula = y ~ a - b * x ^ c,
        start_params = list(a = 1, b = 1, c = 1),
        model_function = function(x, params) {
          a <- params[["a"]]
          b <- params[["b"]]
          c <- params[["c"]]
          if (!is.numeric(x)) {
            message("x must be numeric in model_function.")
            return(NULL)
          }
          a - b * x ^ c
        }
      ),
      InvertedSigma = list(
        description = "Decay function",
        formula = y ~ -a / (1 + -b * (x - c)) + d,
        start_params = list(a = 1, b = 1, c = 1, d = 1),
        model_function = function(x, params) {
          a <- params[["a"]]
          b <- params[["b"]]
          c <- params[["c"]]
          d <- params[["d"]]
          if (!is.numeric(x)) {
            message("x must be numeric in model_function.")
            return(NULL)
          }
          -a / (1 + -b * (x - c)) + d
        }
      ),
      ArctangentDecay = list(
        description = "Decay function",
        formula = y ~ -a * atan(b*x) + c,
        start_params = list(a = 1, b = 1, c = 1),
        model_function = function(x, params) {
          a <- params[["a"]]
          b <- params[["b"]]
          c <- params[["c"]]
          if (!is.numeric(x)) {
            message("x must be numeric in model_function.")
            return(NULL)
          }
          -a * atan(b*x) + c
        }
      ),
      LinearModel = list(
        description = "Simple linear regression model.",
        formula = y ~ a + b * x,
        start_params = list(a = 0, b = 1),
        model_function = function(x, params) {
          a <- params[["a"]]
          b <- params[["b"]]
          if (!is.numeric(x)) {
            message("x must be numeric in model_function.")
            return(NULL)
          }
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
      if (!data.table::is.data.table(data)) {
        message("Input data must be a data.table")
        return(NULL)
      }
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
    # add_model = function(name, formula = NULL, start_params = NULL, model_function = NULL) {
    #   if (is.null(formula) || is.null(start_params) || is.null(model_function)) {
    #     if (!name %in% names(self$model_library)) {
    #       message("Model not found in library. Use list_models() to see available models.")
    #       return(NULL)
    #     }
    #     model_info <- self$model_library[[name]]
    #     formula <- model_info$formula
    #     start_params <- model_info$start_params
    #     model_function <- model_info$model_function
    #   }
    #   self$models[[name]] <- list(
    #     formula = formula,
    #     start_params = start_params,
    #     model_function = model_function
    #   )
    # },
    add_model = function(name, formula = NULL, start_params = NULL, model_function = NULL) {
      if (is.null(formula) || is.null(start_params) || is.null(model_function)) {
        if (!name %in% names(self$model_library)) {
          message("Model not found in library. Use list_models() to see available models.")
          return(NULL)
        }
        model_info   <- self$model_library[[name]]
        formula      <- model_info$formula
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
    #' @param loss choose from 'mse' or 'quantile'
    #' @param quantile_level decimal between 0 and 1 representing the percentile of choice
    #' @param control A list of control parameters for the optimizer, such as `maxiter`.
    #' Default is `list(maxiter = 200)`.
    #' @param ... Additional arguments to be passed to the underlying fitting functions
    #' (`nlsLM` for unweighted models or `optim` for weighted models). Examples include
    #' `trace`, `lower`, and `upper` for `nlsLM`, or `reltol`, `parscale`, and others for `optim`.
    #'
    #' @return A list of fitted model objects.
    #' @export
    fit_models = function(x_col, y_col,
                          weights_col = NULL,
                          loss = "mse",
                          quantile_level = NULL,
                          control = list(maxiter = 1024),
                          cat_encoding = c("target_encoding","credibility"),
                          shift_cat = NULL,
                          param_cat = NULL,
                          param_links = NULL,
                          ...) {

      cat_encoding <- match.arg(cat_encoding)

      if (is.null(self$models) || length(self$models) == 0) {
        message("No models to fit. Use add_model() to add models.")
        return(NULL)
      }

      if (!all(c(x_col, y_col) %in% names(self$data))) {
        message("x_col and y_col must exist in the dataset.")
        return(NULL)
      }

      # Weights
      if (!is.null(weights_col)) {
        weights_vector <- self$data[[weights_col]]
        if (all(weights_vector == 1)) {
          standardized_weights_vector <- weights_vector
        } else {
          standardized_weights_vector <- weights_vector / sum(weights_vector, na.rm = TRUE)
        }
        if (any(is.na(standardized_weights_vector))) {
          message("Weights contain NA values.")
          return(NULL)
        }
      } else {
        standardized_weights_vector <- NULL
      }

      if (!is.null(standardized_weights_vector) && any(is.na(standardized_weights_vector))) {
        message("Weights column contains missing values.")
        return(NULL)
      }

      # Copy + rename for fitting
      temp_data <- data.table::copy(self$data)
      data.table::setnames(temp_data, old = c(x_col, y_col), new = c("x", "y"))

      # Scale params (kept as in your code)
      scale_params <- list(
        min_x = 0,
        max_x = max(temp_data$x, na.rm = TRUE),
        min_y = min(temp_data$y, na.rm = TRUE),
        max_y = max(temp_data$y, na.rm = TRUE),
        scale_factor_x = max(temp_data$x, na.rm = TRUE) - min(temp_data$x, na.rm = TRUE),
        scale_factor_y = max(temp_data$y, na.rm = TRUE) - min(temp_data$y, na.rm = TRUE)
      )

      # Scaled frame
      temp_data_scaled <- data.table::copy(temp_data)
      temp_data_scaled[, x := (x - scale_params$min_x) / scale_params$scale_factor_x]
      temp_data_scaled[, y := (y - scale_params$min_y) / scale_params$scale_factor_y]

      self$fit_results <- lapply(names(self$models), function(model_name) {
        model <- self$models[[model_name]]

        # ---- base param/link setup ----
        start_params <- model$start_params
        param_names  <- names(start_params)

        # param_links may be global or per-model
        links_in <- param_links
        if (is.list(links_in) && !is.null(names(links_in)) && model_name %in% names(links_in)) {
          links_in <- links_in[[model_name]]
        }
        links <- links_in
        if (is.null(links)) links <- setNames(rep("identity", length(param_names)), param_names)
        for (p in param_names) if (is.null(links[[p]])) links[[p]] <- "identity"

        # ---- resolve categoricals for THIS model ----
        shift_vars <- if (is.list(shift_cat)) shift_cat[[model_name]] else shift_cat
        if (!is.null(shift_vars) && !is.character(shift_vars)) {
          stop("shift_cat must be a character vector or a named list of character vectors keyed by model name.")
        }

        raw_param_cat <- if (is.list(param_cat) && !is.null(names(param_cat)) && model_name %in% names(param_cat)) {
          param_cat[[model_name]]
        } else {
          param_cat
        }
        if (!is.null(raw_param_cat) && (!is.list(raw_param_cat) || is.null(names(raw_param_cat)))) {
          stop("param_cat must be a named list mapping parameter names to a single column, ",
               "or a named list keyed by model name whose values are those named lists.")
        }
        param_raw <- if (is.null(raw_param_cat)) NULL else raw_param_cat[intersect(names(raw_param_cat), param_names)]

        enc_method <- cat_encoding

        # ---- PRECOMPUTE ENCODINGS ONCE PER RAW VARIABLE ----
        enc_cache <- list()

        vars_needed <- unique(c(
          if (!is.null(shift_vars)) shift_vars else character(0),
          if (!is.null(param_raw))  unlist(param_raw, use.names = FALSE) else character(0)
        ))

        if (length(vars_needed) > 0) {
          for (v in vars_needed) {
            # compute once
            enc <- private$categorical_encoding(
            # enc <- categorical_encoding(
              dt     = temp_data_scaled,
              var    = v,
              ycol   = "y",
              method = enc_method
            )
            enc_cache[[v]] <- enc
          }
        }

        # ---- SHIFT (additive) using cache ----
        X_shift <- NULL
        shift_maps  <- list()
        shift_names <- character(0)

        if (!is.null(shift_vars) && length(shift_vars) > 0) {
          mats <- lapply(shift_vars, function(v) {
            enc <- enc_cache[[v]]
            shift_maps[[v]]  <<- enc$map
            shift_names      <<- c(shift_names, enc$enc_name)
            matrix(enc$X, ncol = 1L, dimnames = list(NULL, enc$enc_name))
          })
          X_shift <- do.call(cbind, mats)
        }

        # ---- PARAM MODIFIERS using cache ----
        W_param          <- setNames(vector("list", length(param_names)), param_names)
        param_maps       <- setNames(vector("list", length(param_names)), param_names)
        param_enc_names  <- setNames(vector("list", length(param_names)), param_names)

        for (p in param_names) {
          v <- if (!is.null(param_raw)) param_raw[[p]] else NULL
          if (is.null(v)) {
            W_param[[p]] <- NULL
            next
          }
          enc <- enc_cache[[v]]
          W_param[[p]]         <- matrix(enc$X, ncol = 1L, dimnames = list(NULL, enc$enc_name))
          param_maps[[p]]      <- enc$map
          param_enc_names[[p]] <- enc$enc_name
        }

        # ---- starting vector: baselines + (param coefs) + (shift betas) ----
        par0 <- numeric(0); par_names <- character(0)
        for (p in param_names) {
          link_fun <- switch(links[[p]], log = log, logit = qlogis, identity = identity, identity)
          par0      <- c(par0, link_fun(1))
          par_names <- c(par_names, paste0(p, ":baseline"))
          if (!is.null(W_param[[p]])) {
            par0      <- c(par0, 0)
            par_names <- c(par_names, paste0(p, ":", colnames(W_param[[p]])[1L]))
          }
        }
        if (!is.null(X_shift)) {
          k <- ncol(X_shift)
          par0      <- c(par0, rep(0, k))
          par_names <- c(par_names, paste0("shift:", colnames(X_shift)))
        }
        names(par0) <- par_names

        # ---- augmented model ----
        inv_link <- function(nm) switch(nm, log = exp, logit = plogis, identity = identity, identity)

        augmented_model <- function(x, params) {
          par <- params
          params_list <- vector("list", length(param_names)); names(params_list) <- param_names
          i <- 1L
          for (p in param_names) {
            eta <- rep(par[i], length(x)); i <- i + 1L
            if (!is.null(W_param[[p]])) {
              eta <- eta + drop(W_param[[p]] %*% par[i]); i <- i + 1L
            }
            params_list[[p]] <- inv_link(links[[p]])(eta)
          }
          nl <- model$model_function(x, params_list)
          if (!is.null(X_shift)) {
            k <- ncol(X_shift)
            beta <- par[i:(i + k - 1L)]
            nl <- nl + drop(X_shift %*% beta)
          }
          nl
        }

        # ---- optimize ----
        fit <- tryCatch({
          result_params <- private$optimize_with_weights(
            x = temp_data_scaled$x,
            y = temp_data_scaled$y,
            weights = standardized_weights_vector,
            loss = loss,
            quantile_level = quantile_level,
            model = augmented_model,
            start_params = par0,
            ...
          )

          # 🔧 ensure optimized params keep names like "a:baseline", "b:channel_Credibility", "shift:month_grp_Credibility"
          if (is.null(names(result_params$params)) || any(names(result_params$params) == "")) {
            names(result_params$params) <- names(par0)
          }

          preds <- augmented_model(temp_data_scaled$x, result_params$params)

          model_fit <- list(
            coefficients   = result_params$params,
            hessian        = result_params$hessian,
            formula        = model$formula,
            residuals      = temp_data_scaled$y - preds,
            fitted.values  = preds,
            model_function = model$model_function,
            weights        = standardized_weights_vector,
            # stash encoding artifacts for predict()
            cat = list(
              method          = enc_method,
              shift_vars      = shift_vars,
              shift_names     = shift_names,
              shift_maps      = shift_maps,
              param_maps      = param_maps,
              param_enc_names = param_enc_names,
              param_raw_vars  = param_raw
            ),
            param_links    = links,
            param_names    = param_names
          )

          # ---- attach a generic encoder for scoring (shift + param mods) ----
          artifacts_by_raw <- list()

          # SHIFT
          if (!is.null(shift_vars) && length(shift_names) > 0) {
            for (i in seq_along(shift_vars)) {
              raw    <- shift_vars[[i]]
              outcol <- shift_names[[i]]
              mp     <- shift_maps[[raw]]
              if (!is.null(mp)) {
                if (is.null(artifacts_by_raw[[raw]])) artifacts_by_raw[[raw]] <- list()
                artifacts_by_raw[[raw]][[length(artifacts_by_raw[[raw]]) + 1]] <- list(
                  out_col   = outcol,                 # engineered column name (enc$enc_name)
                  map       = shift_maps[[raw]],      # enc$map (has columns: raw, outcol)
                  default   = 0,
                  key_col   = raw,                    # <-- raw categorical column name
                  value_col = outcol                  # <-- engineered column name inside map
                )
              }
            }
          }

          # PARAM MODS
          if (!is.null(param_enc_names) && !is.null(param_raw)) {
            for (p in names(param_enc_names)) {
              outcol <- param_enc_names[[p]]
              raw    <- param_raw[[p]]
              mp     <- param_maps[[p]]
              if (!is.null(outcol) && !is.null(raw) && !is.null(mp)) {
                if (is.null(artifacts_by_raw[[raw]])) artifacts_by_raw[[raw]] <- list()
                artifacts_by_raw[[raw]][[length(artifacts_by_raw[[raw]]) + 1]] <- list(
                  out_col   = outcol,                 # engineered column name (enc$enc_name)
                  map       = param_maps[[p]],        # enc$map (has columns: raw, outcol)
                  default   = 0,
                  key_col   = raw,                    # <-- raw categorical column name
                  value_col = outcol                  # <-- engineered column name inside map
                )
              }
            }
          }

          if (length(artifacts_by_raw) > 0) {
            enc <- private$build_encoder_from_maps(
              artifacts_by_raw = artifacts_by_raw,
              encoder_name = paste0("cat_", enc_method)
            )

            # encoders
            if (is.null(model_fit$encoders)) {
              model_fit$encoders <- list(enc)
            } else {
              model_fit$encoders <- c(model_fit$encoders, list(enc))
            }

            # produced_features
            if (is.null(model_fit$produced_features)) {
              model_fit$produced_features <- enc$produced_features
            } else {
              model_fit$produced_features <- unique(c(model_fit$produced_features, enc$produced_features))
            }

            # requires_features
            if (is.null(model_fit$requires_features)) {
              model_fit$requires_features <- enc$requires
            } else {
              model_fit$requires_features <- unique(c(model_fit$requires_features, enc$requires))
            }
          }

          # CIs
          model_fit$confidence_intervals <- tryCatch({
            private$compute_confidence_intervals(model_fit)
          }, error = function(e) {
            message("confidence intervals failed to build: ", e$message)
          })

          class(model_fit) <- "custom_nls"

          # scaling/back-transform
          model_fit$scale_params   <- scale_params
          model_fit$original_x_col <- x_col
          model_fit$original_y_col <- y_col
          model_fit$scaled_data    <- temp_data_scaled
          model_fit$back_transform <- function(predictions, scale_params) {
            predictions * scale_params$scale_factor_y + scale_params$min_y
          }

          model_fit
        }, error = function(e) {
          message("Error fitting model '", model_name, "': ", e$message)
          NULL
        })

        if (!is.null(fit)) message("Successfully fitted model: ", model_name)
        fit
      })

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
        message("No models available for visualization. Use add_model() to add models.")
        return(NULL)
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
    optimize_with_weights = function(x, y, weights, model, start_params, loss, quantile_level, ...) {

      # Convert start_params into a named vector
      params <- unlist(start_params)

      # Define the weighted residual sum of squares function
      if (loss == "mse") {
        wrss <- function(params_vec) {
          # Reconstruct parameters as a named list
          # params_list <- as.list(params_vec)
          # names(params_list) <- names(start_params)

          # Compute predictions using the model
          predicted <- model(x = x, params = params_vec)

          # Ensure valid predictions
          if (length(predicted) != length(y)) {
            message("Predicted values do not match observed values in length.")
            return(NULL)
          }

          # Calculate weighted residuals
          residuals <- y - predicted

          if (!is.null(weights)) {
            sum(weights * residuals^2)  # Return WRSS
          } else {
            sum(residuals^2)  # Return WRSS
          }
        }

      } else if (loss == "quantile") {

        pinball_loss <- function(residuals, q) {
          ifelse(residuals >= 0, q * residuals, (q - 1) * residuals)
        }

        wrss <- function(params_vec) {
          # params_list <- as.list(params_vec)
          # names(params_list) <- names(start_params)
          predicted <- model(x = x, params = params_vec)

          if (length(predicted) != length(y)) {
            message("Predicted values do not match observed values in length.")
            return(Inf)
          }

          residuals <- y - predicted
          if (!is.null(weights)) {
            sum(weights * pinball_loss(residuals, quantile_level))
          } else {
            sum(pinball_loss(residuals, quantile_level))
          }
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
        message("Optimization did not converge for the weighted model.")
        return(NULL)
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
    },
    categorical_encoding = function(dt,var,ycol = "y",method = c("credibility","target_encoding")) {
      method <- match.arg(method)
      if (!data.table::is.data.table(dt)) data.table::setDT(dt)
      stopifnot(is.character(var), length(var) == 1L, var %chin% names(dt))
      stopifnot(ycol %chin% names(dt), is.numeric(dt[[ycol]]))

      suffix    <- if (method == "credibility") "Credibility" else "TargetEncode"
      enc_name  <- paste0(var, "_", suffix)
      grand_mean <- mean(dt[[ycol]], na.rm = TRUE)

      # ---- build level -> value map (tiny table) ----
      if (method == "target_encoding") {
        map <- dt[, .(val = mean(get(ycol), na.rm = TRUE)), keyby = eval(var)]
        data.table::setnames(map, "val", enc_name)
      } else { # credibility (Bühlmann)
        map <- dt[, .(
          Mean = mean(get(ycol), na.rm = TRUE),
          VarY = stats::var(get(ycol), na.rm = TRUE),
          N    = .N
        ), keyby = eval(var)]

        EPV  <- mean(map$VarY, na.rm = TRUE)                               # within-group variance
        Nbar <- max(1, mean(map$N))                                         # avg group size (guard)
        VHM  <- stats::var(map$Mean - grand_mean, na.rm = TRUE) - (EPV / Nbar)
        if (!is.finite(VHM) || VHM <= 0) VHM <- 1e-8
        if (!is.finite(EPV) || EPV < 0)  EPV <- 0

        K <- EPV / VHM
        Z <- map$N / (map$N + K)
        map[, (enc_name) := Z * Mean + (1 - Z) * grand_mean]
        map[, c("Mean","VarY","N") := NULL]
      }

      # ---- align to dt rows via keyed join (no copies of dt) ----
      data.table::setkeyv(map, var)
      # join just the key column to preserve dt row order
      X <- map[ dt[, get(var)], on = var, nomatch = NA ][[enc_name]]

      # defensive fill (shouldn’t happen at fit)
      if (anyNA(X)) X[is.na(X)] <- grand_mean

      list(
        X = as.numeric(X),
        map = map,                # keep for predict()
        enc_name = enc_name,
        meta = list(
          method     = method,
          var        = var,
          ycol       = ycol,
          grand_mean = grand_mean
        )
      )
    },
    build_encoder_from_maps = function(artifacts_by_raw, encoder_name = "categorical_map") {
      stopifnot(is.list(artifacts_by_raw), length(artifacts_by_raw) > 0)

      requires <- names(artifacts_by_raw)
      produced <- unlist(lapply(artifacts_by_raw, function(outputs)
        vapply(outputs, function(o) o$out_col, character(1L))
      ), use.names = FALSE)

      transform_fn <- function(new_dt) {
        nd <- data.table::as.data.table(new_dt)

        for (raw in names(artifacts_by_raw)) {
          if (!raw %in% names(nd)) next

          for (o in artifacts_by_raw[[raw]]) {
            mp <- data.table::as.data.table(o$map)  # map already has raw + engineered cols

            # figure out which columns to use
            key_col   <- if (!is.null(o$key_col)   && o$key_col   %in% names(mp)) o$key_col   else if (raw %in% names(mp)) raw else names(mp)[1L]
            value_col <- if (!is.null(o$value_col) && o$value_col %in% names(mp)) o$value_col else if (!is.null(o$out_col) && o$out_col %in% names(mp)) o$out_col else setdiff(names(mp), key_col)[1L]

            # fast vectorized lookup with match()
            idx  <- match(nd[[raw]], mp[[key_col]])
            vals <- mp[[value_col]][idx]

            def <- if (!is.null(o$default)) o$default else 0
            vals[is.na(vals)] <- def

            nd[, (o$out_col) := vals]
          }
        }
        nd
      }

      list(
        name = encoder_name,
        requires = requires,
        produced_features = produced,
        artifacts = artifacts_by_raw,
        transform = transform_fn
      )
    }
  )
)
