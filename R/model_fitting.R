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
      Hill = list(
        description = "Hill equation: models dose-response relationships.",
        formula = y ~ a * x^b / (c + x^b),
        start_params = list(a = 1, b = 1, c = 1)
      ),
      Logistic = list(
        description = "Logistic growth model.",
        formula = y ~ a / (1 + exp(-b * (x - c))),
        start_params = list(a = 1, b = 1, c = 50)
      ),
      ExponentialDecay = list(
        description = "Exponential decay model.",
        formula = y ~ a * exp(-b * x),
        start_params = list(a = 1, b = 0.1)
      ),
      Gompertz = list(
        description = "Gompertz growth model.",
        formula = y ~ a * exp(-b * exp(-c * x)),
        start_params = list(a = 1, b = 1, c = 1)
      ),
      MichaelisMenten = list(
        description = "Michaelis-Menten kinetics.",
        formula = y ~ (Vmax * x) / (Km + x),
        start_params = list(Vmax = 1, Km = 1)
      ),
      WeibullType1 = list(
        description = "Weibull Type 1 model, used in survival analysis.",
        formula = y ~ a * exp(-exp(b - c * x)),
        start_params = list(a = 1, b = 1, c = 0.1)
      ),
      WeibullType2 = list(
        description = "Weibull Type 2 model for sigmoidal data.",
        formula = y ~ a * (1 - exp(-b * x^c)),
        start_params = list(a = 1, b = 0.1, c = 1)
      ),
      Asymptotic = list(
        description = "Asymptotic regression model for limited growth.",
        formula = y ~ a - (a - b) * exp(-c * x),
        start_params = list(a = 1, b = 1, c = 0.1)
      ),
      PowerCurve = list(
        description = "Power curve model for scaling relationships.",
        formula = y ~ a * x^b,
        start_params = list(a = 1, b = 1)
      ),
      Logarithmic = list(
        description = "Logarithmic model for data leveling off.",
        formula = y ~ a + b * log(x),
        start_params = list(a = 1, b = 1)
      ),
      RectangularHyperbola = list(
        description = "Rectangular hyperbola for saturation processes.",
        formula = y ~ (a * x) / (b + x),
        start_params = list(a = 1, b = 1)
      ),
      Richards = list(
        description = "Richards curve: a generalization of logistic growth.",
        formula = y ~ a / (1 + exp(-b * (x - c)))^d,
        start_params = list(a = 1, b = 1, c = 50, d = 1)
      ),
      ChapmanRichards = list(
        description = "Chapman-Richards model for growth.",
        formula = y ~ a * (1 - exp(-b * x))^c,
        start_params = list(a = 1, b = 0.1, c = 2)
      ),
      HyperbolicTangent = list(
        description = "Hyperbolic tangent model for sigmoidal data.",
        formula = y ~ a * tanh(b * x + c),
        start_params = list(a = 1, b = 0.1, c = 0)
      )
    ),

    #' Initialize the NonLinearFitter class
    #'
    #' @param data A data.table containing the dataset for modeling.
    #' Must include the predictor and response variable columns.
    #' @return A new instance of the NonLinearFitter class.
    #' @examples
    #' data <- data.table::data.table(x = 1:100, y = 5 / (1 + exp(-0.1 * (1:100 - 50))))
    #' fitter <- NonLinearFitter$new(data)
    initialize = function(data) {
      if (!data.table::is.data.table(data)) stop("Input data must be a data.table")
      self$data <- data
      self$models <- list()
    },

    #' @return A data.table summarizing available models.
    list_models = function() {
      data.table::data.table(
        Model = names(self$model_library),
        Description = sapply(self$model_library, function(x) x$description)
      )
    },

    #' Add a non-linear model for testing
    #'
    #' @param name The name of the model (e.g., "Hill").
    #' @param formula The non-linear formula for the model (optional if using pre-defined model).
    #' @param start_params A list of starting parameters for the model (optional if using pre-defined model).
    #' @return NULL
    #' @examples
    #' # Add a pre-defined model
    #' fitter$add_model("Hill")
    #'
    #' # Add a custom model
    #' fitter$add_model("Custom", y ~ a * exp(-b * x), list(a = 1, b = 0.1))
    add_model = function(name, formula = NULL, start_params = NULL) {
      if (is.null(formula) || is.null(start_params)) {
        if (!name %in% names(self$model_library)) {
          stop("Model not found in library. Use list_models() to see available models.")
        }
        model_info <- self$model_library[[name]]
        formula <- model_info$formula
        start_params <- model_info$start_params
      }
      self$models[[name]] <- list(
        formula = formula,
        start_params = start_params
      )
    },

    #' @param x_col The name of the predictor variable.
    #' @param y_col The name of the response variable.
    #' @param control A list of control parameters for the optimizer, such as `maxiter`.
    #' Default is `list(maxiter = 200)`.
    #' @return A list of fitted model objects.
    #' @examples
    #' fitter$fit_models(x_col = "x", y_col = "y", control = list(maxiter = 200))
    fit_models = function(x_col, y_col, control = list(maxiter = 200)) {
      if (is.null(self$models) || length(self$models) == 0) {
        stop("No models to fit. Use add_model() to add models.")
      }
      if (!all(c(x_col, y_col) %in% names(self$data))) {
        stop("x_col and y_col must exist in the dataset.")
      }

      self$fit_results <- lapply(names(self$models), function(model_name) {
        model <- self$models[[model_name]]

        # Dynamically adjust the formula to use x_col and y_col
        formula <- as.formula(
          gsub("x", x_col, gsub("y", y_col, deparse(model$formula)))
        )

        fit <- tryCatch({
          model_fit <- minpack.lm::nlsLM(
            formula = formula,
            data = self$data,
            start = model$start_params,
            control = control
          )
          # Attach the formula explicitly to the fit object
          model_fit$formula <- formula
          model_fit
        }, error = function(e) {
          message("Error fitting model '", model_name, "': ", e$message)
          NULL
        })

        if (!is.null(fit)) {
          message("Successfully fitted model: ", model_name)
        }

        fit
      })

      # Name the list using the model names
      names(self$fit_results) <- names(self$models)
      return(self$fit_results)
    }
  )
)
