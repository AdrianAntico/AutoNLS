# AutoNLSFitter ---------------------------------------------------------------
# A cleaner, “future” fitter that keeps your best ideas:
# - scale/standardize for optimizer stability
# - pragmatic x-min anchoring + safe lower extrapolation (no NaNs / no crashes)
# - categorical encoding (target / credibility)
# - parameter links (identity / log / logit)
# - optional shift terms + parameter modifiers via encoders
# - power-user derivatives (dŷ/dx) + elasticity

`%||%` <- function(a, b) if (!is.null(a)) a else b

#' AutoNLSFitter
#'
#' An R6 class for robust non-linear regression with:
#' \itemize{
#'   \item Automatic scaling for optimizer stability
#'   \item Safe scoring with controlled extrapolation policies
#'   \item Optional categorical encodings (target / credibility)
#'   \item Parameter link functions (identity, log, logit)
#'   \item Numerical derivatives and elasticity for power users
#' }
#'
#' The class is designed for business applications where model
#' stability, interpretability, and safe prediction behavior are critical.
#'
#' @section Scaling Strategy:
#' During fitting, predictors and responses are scaled to improve
#' optimization stability. During scoring, predictions are automatically
#' back-transformed to the original data scale.
#'
#' @section Extrapolation Policy:
#' The scoring method supports three policies:
#' \describe{
#'   \item{clip}{Clamp new x values to the training range (default, safest)}
#'   \item{error}{Stop if new x values fall outside training range}
#'   \item{allow}{Permit extrapolation beyond training range}
#' }
#'
#' @field data A data.table containing training data.
#' @field models A list of models added for fitting.
#' @field fit_results A list of fitted model objects.
#' @field model_library Optional predefined library of models.
#'
#' @examples
#' \dontrun{
#' library(data.table)
#'
#' dt <- data.table(
#'   x = 1:100,
#'   y = 100 * x / (50 + x)
#' )
#'
#' fitter <- AutoNLSFitter$new(dt)
#'
#' fitter$add_model(
#'   name = "MichaelisMenten",
#'   formula = y ~ (Vmax * x) / (Km + x),
#'   start_params = list(Vmax = 1, Km = 1),
#'   model_function = function(x, params) {
#'     (params$Vmax * x) / (params$Km + x)
#'   }
#' )
#'
#' fits <- fitter$fit_models("x", "y")
#'
#' fits$MichaelisMenten$predict(dt)
#' fits$MichaelisMenten$derivative(dt)
#' }
#'
#' @export
AutoNLSFitter <- R6::R6Class(
  "AutoNLSFitter",
  public = list(
    data = NULL,
    models = NULL,
    fit_results = NULL,

    # ---------------------------------------------------------------------
    # MODEL LIBRARY (now includes deriv_function directly in each entry)
    # ---------------------------------------------------------------------
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
        },
        deriv_function = function(x, params) {
          a <- params[["a"]]; b <- params[["b"]]
          denom <- (a^b + x^b)
          (b * a^b * x^(b - 1)) / (denom^2)
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
        },
        deriv_function = function(x, params) {
          a <- params[["a"]]; b <- params[["b"]]; c <- params[["c"]]
          denom <- (c^b + x^b)
          a * (b * c^b * x^(b - 1)) / (denom^2)
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
        },
        deriv_function = function(x, params) {
          a <- params[["a"]]; b <- params[["b"]]; c <- params[["c"]]
          e <- params[["e"]]
          denom <- (c^b + x^b)
          a * (b * c^b * x^(b - 1)) / (denom^2) + e
        }
      ),

      HillSwitchpointModel = list(
        description = "Hill equation with a smooth switch point for dose-response.",
        formula = y ~ (1 / (1 + exp(-k * (x - s)))) * (a * (x^b) / (c^b + x^b)) +
          (1 - (1 / (1 + exp(-k * (x - s))))) * (d * (x^e) / (f^e + x^e)),
        start_params = list(a = 1, b = 1, c = 1, d = 1, e = 1, f = 1, s = 50, k = 10),
        model_function = function(x, params) {
          a <- params[["a"]]; b <- params[["b"]]; c <- params[["c"]]
          d <- params[["d"]]; e <- params[["e"]]; f <- params[["f"]]
          s <- params[["s"]]; k <- params[["k"]]
          w <- 1 / (1 + exp(-k * (x - s)))
          H1 <- a * (x^b) / (c^b + x^b)
          H2 <- d * (x^e) / (f^e + x^e)
          w * H1 + (1 - w) * H2
        },
        deriv_function = function(x, params) {
          a <- params[["a"]]; b <- params[["b"]]; c <- params[["c"]]
          d <- params[["d"]]; e <- params[["e"]]; f <- params[["f"]]
          s <- params[["s"]]; k <- params[["k"]]

          # logistic weight + derivative
          w  <- 1 / (1 + exp(-k * (x - s)))
          wp <- k * w * (1 - w)

          # H1 and H1'
          denom1 <- (c^b + x^b)
          H1  <- a * x^b / denom1
          H1p <- a * (b * c^b * x^(b - 1)) / (denom1^2)

          # H2 and H2'
          denom2 <- (f^e + x^e)
          H2  <- d * x^e / denom2
          H2p <- d * (e * f^e * x^(e - 1)) / (denom2^2)

          # product rule on mixture:
          # y = w*H1 + (1-w)*H2
          # y' = w'*(H1 - H2) + w*H1' + (1-w)*H2'
          wp * (H1 - H2) + w * H1p + (1 - w) * H2p
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
        },
        deriv_function = function(x, params) {
          a <- params[["a"]]; b <- params[["b"]]; c <- params[["c"]]; d <- params[["d"]]
          denom <- (c + x^b)
          a * (b * c * x^(b - 1)) / (denom^2) + 2 * d * x
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
        },
        deriv_function = function(x, params) {
          a <- params[["a"]]; b <- params[["b"]]; c <- params[["c"]]
          z <- exp(-b * (x - c))
          a * b * z / (1 + z)^2
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
        },
        deriv_function = function(x, params) {
          a <- params[["a"]]; b <- params[["b"]]; c <- params[["c"]]
          d <- params[["d"]]; g <- params[["g"]]
          h <- 1 + (x / c)^b
          dhdx <- b * x^(b - 1) / (c^b)
          -(a - d) * g * h^(-g - 1) * dhdx
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
        },
        deriv_function = function(x, params) {
          a <- params[["a"]]; b <- params[["b"]]
          -a * b * exp(-b * x)
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
        },
        deriv_function = function(x, params) {
          a <- params[["a"]]; b <- params[["b"]]
          -a * b * exp(-b * x)
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
        },
        deriv_function = function(x, params) {
          a <- params[["a"]]; b <- params[["b"]]
          c <- params[["c"]]; d <- params[["d"]]
          -a * b * exp(-b * x) - c * d * exp(-d * x)
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
        },
        deriv_function = function(x, params) {
          a <- params[["a"]]; b <- params[["b"]]; c <- params[["c"]]
          t <- exp(-c * x)
          a * b * c * t * exp(-b * t)
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
        },
        deriv_function = function(x, params) {
          a <- params[["a"]]; b <- params[["b"]]; c <- params[["c"]]
          u <- exp(b - c * x)
          a * c * u * exp(-u)
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
        },
        deriv_function = function(x, params) {
          Vmax <- params[["Vmax"]]; Km <- params[["Km"]]
          (Vmax * Km) / (Km + x)^2
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
        },
        deriv_function = function(x, params) {
          a <- params[["a"]]; b <- params[["b"]]; c <- params[["c"]]
          u <- exp(b - c * x)
          a * c * u * exp(-u)
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
        },
        deriv_function = function(x, params) {
          a <- params[["a"]]; b <- params[["b"]]; c <- params[["c"]]
          a * b * c * x^(c - 1) * exp(-b * x^c)
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
        },
        deriv_function = function(x, params) {
          a <- params[["a"]]; b <- params[["b"]]; c <- params[["c"]]
          (a - b) * c * exp(-c * x)
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
        },
        deriv_function = function(x, params) {
          a <- params[["a"]]; b <- params[["b"]]
          a * b * x^(b - 1)
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
          # guard: avoid log(0) / log(negative)
          xx <- pmax(x, 1e-12)
          a + b * log(xx)
        },
        deriv_function = function(x, params) {
          b <- params[["b"]]
          if (!is.numeric(x)) {
            message("x must be numeric in deriv_function.")
            return(NULL)
          }
          xx <- pmax(x, 1e-12)
          b / xx
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
        },
        deriv_function = function(x, params) {
          a <- params[["a"]]; b <- params[["b"]]
          (a * b) / (b + x)^2
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
        },
        deriv_function = function(x, params) {
          a <- params[["a"]]; b <- params[["b"]]; c <- params[["c"]]; d <- params[["d"]]
          z <- exp(-b * (x - c))
          g <- 1 + z
          a * d * b * z / (g^(d + 1))
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
        },
        deriv_function = function(x, params) {
          a <- params[["a"]]; b <- params[["b"]]; c <- params[["c"]]
          g <- 1 - exp(-b * x)
          a * c * g^(c - 1) * b * exp(-b * x)
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
        },
        deriv_function = function(x, params) {
          a <- params[["a"]]; b <- params[["b"]]; c <- params[["c"]]
          z <- tanh(b * x + c)
          a * b * (1 - z^2)
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
        },
        deriv_function = function(x, params) {
          a <- params[["a"]]; b <- params[["b"]]; c <- params[["c"]]
          d <- params[["d"]]; e <- params[["e"]]
          h <- 1 + (x / c)^d
          dhdx <- d * x^(d - 1) / (c^d)
          (b - a) * (-e) * h^(-e - 1) * dhdx
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
        },
        deriv_function = function(x, params) {
          a <- params[["a"]]; b <- params[["b"]]; c <- params[["c"]]; d <- params[["d"]]
          t <- (x / b)^c
          dtdx <- c * x^(c - 1) / (b^c)
          -(a - d) * exp(-t) * dtdx
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
        },
        deriv_function = function(x, params) {
          a <- params[["a"]]; b <- params[["b"]]; c <- params[["c"]]; d <- params[["d"]]
          bc <- b^c
          denom <- (1 + x / bc)
          -(a - d) * (1 / bc) / (denom^2)
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
        },
        deriv_function = function(x, params) {
          a <- params[["a"]]; b <- params[["b"]]; c <- params[["c"]]; d <- params[["d"]]
          t <- exp(-c * x)
          (a - d) * b * c * t * exp(-b * t)
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
        },
        deriv_function = function(x, params) {
          a <- params[["a"]]; b <- params[["b"]]; c <- params[["c"]]; d <- params[["d"]]
          bc <- b^c
          (a - d) * (-c) * bc * x^(c - 1) / (bc + x^c)^2
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
        },
        deriv_function = function(x, params) {
          a <- params[["a"]]; b <- params[["b"]]
          -a * b * exp(-b * x)
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
        },
        deriv_function = function(x, params) {
          a <- params[["a"]]; b <- params[["b"]]; d <- params[["d"]]
          a * (-b) * (x + d)^(-b - 1)
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
        },
        deriv_function = function(x, params) {
          a <- params[["a"]]; b <- params[["b"]]; c <- params[["c"]]
          z <- exp(b * (x - c))
          -a * b * z / (1 + z)^2
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
        },
        deriv_function = function(x, params) {
          b <- params[["b"]]; c <- params[["c"]]
          -b / (x + c)
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
        },
        deriv_function = function(x, params) {
          b <- params[["b"]]; c <- params[["c"]]
          -b * c * x^(c - 1)
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
        },
        deriv_function = function(x, params) {
          a <- params[["a"]]; b <- params[["b"]]; c <- params[["c"]]
          denom <- (1 - b * (x - c))
          -a * b / (denom^2)
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
        },
        deriv_function = function(x, params) {
          a <- params[["a"]]; b <- params[["b"]]
          -a * (b / (1 + (b * x)^2))
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
        },
        deriv_function = function(x, params) {
          params[["b"]]
        }
      )
    ),

    # ---------------------------------------------------------------------
    # initialize: keep default library unless user supplies one
    # ---------------------------------------------------------------------
    initialize = function(data, model_library = NULL) {
      if (!data.table::is.data.table(data)) data.table::setDT(data)
      self$data <- data
      self$models <- list()
      self$fit_results <- list()

      # IMPORTANT: only override the built-in library if user provides one
      if (!is.null(model_library)) {
        self$model_library <- model_library
      }

      # (optional but recommended) validate the library once here
      for (nm in names(self$model_library)) {
        mi <- self$model_library[[nm]]
        if (is.null(mi$formula)) stop(sprintf("[%s] missing formula", nm))
        if (is.null(mi$start_params)) stop(sprintf("[%s] missing start_params", nm))
        if (!is.function(mi$model_function)) stop(sprintf("[%s] missing model_function", nm))
        if (!is.function(mi$deriv_function)) stop(sprintf("[%s] missing deriv_function", nm))
      }

      invisible(self)
    },

    list_models = function() {
      if (length(self$model_library) == 0) {
        return(data.table::data.table(Model=character(), Description=character(), Formula=character()))
      }
      data.table::data.table(
        Model = names(self$model_library),
        Description = vapply(self$model_library, function(x) x$description %||% "", character(1)),
        Formula = vapply(self$model_library, function(x) deparse(x$formula), character(1))
      )
    },

    # ---------------------------------------------------------------------
    # add_model: carries deriv_function through to self$models
    # ---------------------------------------------------------------------
    add_model = function(name,
                         formula = NULL,
                         start_params = NULL,
                         model_function = NULL,
                         deriv_function = NULL) {

      # If user didn't provide the core pieces, pull everything from library FIRST
      if (is.null(formula) || is.null(start_params) || is.null(model_function)) {
        if (!name %in% names(self$model_library)) {
          stop("Model not found in model_library. Provide formula/start_params/model_function or add it to model_library.")
        }
        mi <- self$model_library[[name]]
        formula        <- mi$formula
        start_params   <- mi$start_params
        model_function <- mi$model_function
        deriv_function <- mi$deriv_function %||% deriv_function
      }

      validate_model_def <- function(name, mi) {
        stopifnot(is.list(mi))
        if (is.null(mi$formula)) stop(sprintf("[%s] missing formula", name))
        if (is.null(mi$start_params) || !is.list(mi$start_params) || any(names(mi$start_params) == "")) {
          stop(sprintf("[%s] start_params must be a named list", name))
        }
        if (!is.function(mi$model_function)) stop(sprintf("[%s] model_function must be a function", name))
        if (!is.function(mi$deriv_function)) stop(sprintf("[%s] deriv_function must be a function", name))
        invisible(TRUE)
      }

      validate_model_def(name, list(
        formula = formula,
        start_params = start_params,
        model_function = model_function,
        deriv_function = deriv_function
      ))

      self$models[[name]] <- list(
        formula = formula,
        start_params = start_params,
        model_function = model_function,
        deriv_function = deriv_function
      )

      invisible(NULL)
    },

    fit_models = function(
    x_col, y_col,
    weights_col = NULL,
    loss = c("mse", "quantile"),
    quantile_level = NULL,
    control = list(maxit = 3000, reltol = 1e-8),
    method = "BFGS",
    cat_encoding = c("target_encoding", "credibility"),
    shift_cat = NULL,
    param_cat = NULL,
    param_links = NULL,
    nonnegative_x = TRUE,
    extrapolation = c("clip", "error", "allow"),
    compute_hessian = TRUE,
    ...
    ) {
      loss <- match.arg(loss)
      cat_encoding <- match.arg(cat_encoding)
      extrapolation <- match.arg(extrapolation)

      if (length(self$models) == 0) stop("No models to fit. Call add_model() first.")
      if (!all(c(x_col, y_col) %in% names(self$data))) stop("x_col and y_col must exist in data.")

      if (loss == "quantile") {
        if (is.null(quantile_level) || !is.numeric(quantile_level) || length(quantile_level) != 1L ||
            !is.finite(quantile_level) || quantile_level <= 0 || quantile_level >= 1) {
          stop("quantile_level must be a single numeric in (0,1) when loss='quantile'.")
        }
      }

      dt <- data.table::copy(self$data)
      data.table::setnames(dt, c(x_col, y_col), c("x_raw", "y_raw"))

      w <- NULL
      if (!is.null(weights_col)) {
        if (!weights_col %in% names(dt)) stop("weights_col not found in data.")
        w <- dt[[weights_col]]
        if (anyNA(w)) stop("weights contain NA.")
        if (!all(w == 1)) w <- w / sum(w)
      }

      min_x <- min(dt$x_raw, na.rm = TRUE)
      max_x <- max(dt$x_raw, na.rm = TRUE)
      min_y <- min(dt$y_raw, na.rm = TRUE)
      max_y <- max(dt$y_raw, na.rm = TRUE)

      scale_x <- max_x - min_x
      scale_y <- max_y - min_y
      if (!is.finite(scale_x) || scale_x <= 0) scale_x <- 1
      if (!is.finite(scale_y) || scale_y <= 0) scale_y <- 1

      scale <- list(
        x_col = x_col,
        y_col = y_col,
        weights_col = weights_col,
        min_x = min_x,
        max_x = max_x,
        min_y = min_y,
        max_y = max_y,
        scale_x = scale_x,
        scale_y = scale_y,
        nonnegative_x = isTRUE(nonnegative_x),
        extrapolation = extrapolation
      )

      dts <- data.table::copy(dt)
      dts[, x := (x_raw - min_x) / scale_x]
      dts[, y := (y_raw - min_y) / scale_y]

      fits <- lapply(names(self$models), function(model_name) {
        tryCatch(
          private$fit_one_model(
            model_name = model_name,
            model = self$models[[model_name]],
            dt_scaled = dts,
            weights = w,
            loss = loss,
            quantile_level = quantile_level,
            control = control,
            method = method,
            cat_encoding = cat_encoding,
            shift_cat = shift_cat,
            param_cat = param_cat,
            param_links = param_links,
            compute_hessian = compute_hessian,
            scale = scale,
            ...
          ),
          error = function(e) {
            list(
              ok = FALSE,
              model_name = model_name,
              error = list(message = e$message),
              scale = scale
            )
          }
        )
      })

      names(fits) <- names(self$models)
      self$fit_results <- fits
      fits
    }
  ),

  private = list(
    .clamp01 = function(v, eps = 1e-6) pmin(pmax(v, eps), 1 - eps),

    .link = function(link, v) {
      if (is.na(v) || !is.finite(v)) v <- 1
      switch(link,
             log = log(max(v, 1e-12)),
             logit = stats::qlogis(private$.clamp01(v)),
             identity = v,
             v
      )
    },

    .inv_link = function(link) {
      switch(link,
             log = exp,
             logit = plogis,
             identity = identity,
             identity
      )
    },

    transform_x_scaled = function(x_raw, scale) {
      x <- x_raw

      if (scale$nonnegative_x) {
        x <- pmax(x, 0)
      }

      if (scale$extrapolation == "error") {
        if (any(x < scale$min_x, na.rm = TRUE)) stop("new x has values < training min_x; set extrapolation='clip' or 'allow'.")
        if (any(x > scale$max_x, na.rm = TRUE)) stop("new x has values > training max_x; set extrapolation='clip' or 'allow'.")
      } else if (scale$extrapolation == "clip") {
        x <- pmin(pmax(x, scale$min_x), scale$max_x)
      }

      (x - scale$min_x) / scale$scale_x
    },

    back_transform_y = function(y_scaled, scale) {
      y_scaled * scale$scale_y + scale$min_y
    },

    categorical_encoding = function(dt, var, ycol = "y", method = c("credibility", "target_encoding")) {
      method <- match.arg(method)
      if (!data.table::is.data.table(dt)) data.table::setDT(dt)
      stopifnot(is.character(var), length(var) == 1L, var %chin% names(dt))
      stopifnot(ycol %chin% names(dt), is.numeric(dt[[ycol]]))

      suffix <- if (method == "credibility") "Credibility" else "TargetEncode"
      enc_name <- paste0(var, "_", suffix)
      grand_mean <- mean(dt[[ycol]], na.rm = TRUE)

      if (method == "target_encoding") {
        map <- dt[, .(val = mean(get(ycol), na.rm = TRUE)), keyby = eval(var)]
        data.table::setnames(map, "val", enc_name)
      } else {
        map <- dt[, .(
          Mean = mean(get(ycol), na.rm = TRUE),
          VarY = stats::var(get(ycol), na.rm = TRUE),
          N = .N
        ), keyby = eval(var)]

        EPV  <- mean(map$VarY, na.rm = TRUE)
        Nbar <- max(1, mean(map$N))
        VHM  <- stats::var(map$Mean - grand_mean, na.rm = TRUE) - (EPV / Nbar)
        if (!is.finite(VHM) || VHM <= 0) VHM <- 1e-8
        if (!is.finite(EPV) || EPV < 0)  EPV <- 0

        K <- EPV / VHM
        Z <- map$N / (map$N + K)

        map[, (enc_name) := Z * Mean + (1 - Z) * grand_mean]
        map[, c("Mean","VarY","N") := NULL]
      }

      data.table::setkeyv(map, var)

      X <- map[ dt[, get(var)], on = var, nomatch = NA ][[enc_name]]
      if (anyNA(X)) X[is.na(X)] <- grand_mean

      list(
        X = as.numeric(X),
        map = map,
        enc_name = enc_name,
        meta = list(method = method, var = var, ycol = ycol, grand_mean = grand_mean)
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
            mp <- data.table::as.data.table(o$map)

            key_col   <- if (!is.null(o$key_col)   && o$key_col   %in% names(mp)) o$key_col   else if (raw %in% names(mp)) raw else names(mp)[1L]
            value_col <- if (!is.null(o$value_col) && o$value_col %in% names(mp)) o$value_col else if (!is.null(o$out_col) && o$out_col %in% names(mp)) o$out_col else setdiff(names(mp), key_col)[1L]

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
    },

    optimize = function(x, y, weights, model, par0, loss, quantile_level,
                        control, method, compute_hessian, ...) {

      pinball <- function(r, q) ifelse(r >= 0, q * r, (q - 1) * r)

      safe_model_eval <- function(par) {
        # Return numeric vector or NA_real_ on any warning/error
        out <- tryCatch(
          withCallingHandlers(
            model(x = x, params = par),
            warning = function(w) {
              invokeRestart("muffleWarning")
            }
          ),
          error = function(e) NA_real_
        )
        out
      }

      obj <- function(par) {
        pred <- safe_model_eval(par)

        # kill any non-finite / wrong-length predictions
        if (length(pred) != length(y) || any(!is.finite(pred))) return(Inf)

        r <- y - pred
        if (loss == "mse") {
          if (!is.null(weights)) sum(weights * r^2) else sum(r^2)
        } else {
          if (!is.null(weights)) sum(weights * pinball(r, quantile_level)) else sum(pinball(r, quantile_level))
        }
      }

      ctrl <- modifyList(list(maxit = 3000, reltol = 1e-8), control)

      res <- stats::optim(
        par = par0,
        fn = obj,
        method = method,
        hessian = isTRUE(compute_hessian),
        control = ctrl,
        ...
      )

      ok <- is.list(res) && is.finite(res$value) && res$convergence == 0
      list(
        ok = ok,
        par = res$par,
        hessian = if (isTRUE(compute_hessian)) res$hessian else NULL,
        optim = list(
          convergence = res$convergence,
          value = res$value,
          counts = res$counts,
          message = res$message
        )
      )
    },

    fit_one_model = function(
    model_name, model, dt_scaled, weights, loss, quantile_level,
    control, method, cat_encoding, shift_cat, param_cat, param_links,
    compute_hessian, scale, ...
    ) {
      start_params <- model$start_params
      param_names <- names(start_params)

      links_in <- param_links
      if (is.list(links_in) && !is.null(names(links_in)) && model_name %in% names(links_in)) {
        links_in <- links_in[[model_name]]
      }
      links <- links_in
      if (is.null(links)) links <- setNames(rep("identity", length(param_names)), param_names)
      for (p in param_names) if (is.null(links[[p]])) links[[p]] <- "identity"

      shift_vars <- if (is.list(shift_cat)) shift_cat[[model_name]] else shift_cat
      if (!is.null(shift_vars) && !is.character(shift_vars)) {
        stop("shift_cat must be a character vector or a named list keyed by model name.")
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

      # --- precompute encodings once per raw variable ---
      enc_cache <- list()
      vars_needed <- unique(c(
        if (!is.null(shift_vars)) shift_vars else character(0),
        if (!is.null(param_raw))  unlist(param_raw, use.names = FALSE) else character(0)
      ))

      if (length(vars_needed) > 0) {
        for (v in vars_needed) {
          enc_cache[[v]] <- private$categorical_encoding(
            dt = dt_scaled,
            var = v,
            ycol = "y",
            method = cat_encoding
          )
        }
      }

      # SHIFT design
      X_shift <- NULL
      shift_maps <- list()
      shift_names <- character(0)

      if (!is.null(shift_vars) && length(shift_vars) > 0) {
        mats <- lapply(shift_vars, function(v) {
          enc <- enc_cache[[v]]
          shift_maps[[v]] <<- enc$map
          shift_names <<- c(shift_names, enc$enc_name)
          matrix(enc$X, ncol = 1L, dimnames = list(NULL, enc$enc_name))
        })
        X_shift <- do.call(cbind, mats)
      }

      # PARAM modifiers design
      W_param <- setNames(vector("list", length(param_names)), param_names)
      param_maps <- setNames(vector("list", length(param_names)), param_names)
      param_enc_names <- setNames(vector("list", length(param_names)), param_names)

      for (p in param_names) {
        v <- if (!is.null(param_raw)) param_raw[[p]] else NULL
        if (is.null(v)) next
        enc <- enc_cache[[v]]
        W_param[[p]] <- matrix(enc$X, ncol = 1L, dimnames = list(NULL, enc$enc_name))
        param_maps[[p]] <- enc$map
        param_enc_names[[p]] <- enc$enc_name
      }

      # starting vector
      par0 <- numeric(0); par_names <- character(0)
      for (p in param_names) {
        par0 <- c(par0, private$.link(links[[p]], start_params[[p]]))
        par_names <- c(par_names, paste0(p, ":baseline"))

        if (!is.null(W_param[[p]])) {
          par0 <- c(par0, 0)
          par_names <- c(par_names, paste0(p, ":", colnames(W_param[[p]])[1L]))
        }
      }
      if (!is.null(X_shift)) {
        k <- ncol(X_shift)
        par0 <- c(par0, rep(0, k))
        par_names <- c(par_names, paste0("shift:", colnames(X_shift)))
      }
      names(par0) <- par_names

      # augmented model (scaled domain)
      augmented_model <- function(x, params) {
        params_list <- vector("list", length(param_names)); names(params_list) <- param_names
        i <- 1L

        for (p in param_names) {
          eta <- rep(params[i], length(x)); i <- i + 1L
          if (!is.null(W_param[[p]])) {
            eta <- eta + drop(W_param[[p]] %*% params[i]); i <- i + 1L
          }
          inv <- private$.inv_link(links[[p]])
          params_list[[p]] <- inv(eta)
        }

        nl <- model$model_function(x, params_list)

        if (!is.null(X_shift)) {
          k <- ncol(X_shift)
          beta <- params[i:(i + k - 1L)]
          nl <- nl + drop(X_shift %*% beta)
        }

        nl
      }

      # Optimize
      opt <- private$optimize(
        x = dt_scaled$x,
        y = dt_scaled$y,
        weights = weights,
        model = augmented_model,
        par0 = par0,
        loss = loss,
        quantile_level = quantile_level,
        control = control,
        method = method,
        compute_hessian = compute_hessian,
        ...
      )

      if (!isTRUE(opt$ok)) {
        return(list(
          ok = FALSE,
          model_name = model_name,
          error = list(message = "optim failed", optim = opt$optim),
          scale = scale,
          param_names = param_names,
          param_links = links
        ))
      }

      preds_train <- augmented_model(dt_scaled$x, opt$par)
      if (length(preds_train) != nrow(dt_scaled) || any(!is.finite(preds_train))) {
        return(list(
          ok = FALSE,
          model_name = model_name,
          error = list(message = "Non-finite predictions after optimization."),
          scale = scale
        ))
      }

      # ---- build encoder artifacts (unchanged) ----
      artifacts_by_raw <- list()

      if (!is.null(shift_vars) && length(shift_names) > 0) {
        for (ii in seq_along(shift_vars)) {
          raw <- shift_vars[[ii]]
          outcol <- shift_names[[ii]]
          mp <- shift_maps[[raw]]
          if (!is.null(mp)) {
            artifacts_by_raw[[raw]] <- c(
              artifacts_by_raw[[raw]] %||% list(),
              list(list(out_col = outcol, map = mp, default = 0, key_col = raw, value_col = outcol))
            )
          }
        }
      }

      if (!is.null(param_enc_names) && !is.null(param_raw)) {
        for (p in names(param_enc_names)) {
          outcol <- param_enc_names[[p]]
          raw <- param_raw[[p]]
          mp <- param_maps[[p]]
          if (!is.null(outcol) && !is.null(raw) && !is.null(mp)) {
            artifacts_by_raw[[raw]] <- c(
              artifacts_by_raw[[raw]] %||% list(),
              list(list(out_col = outcol, map = mp, default = 0, key_col = raw, value_col = outcol))
            )
          }
        }
      }

      encoders <- list()
      produced_features <- character(0)
      requires_features <- character(0)

      if (length(artifacts_by_raw) > 0) {
        enc <- private$build_encoder_from_maps(
          artifacts_by_raw = artifacts_by_raw,
          encoder_name = paste0("cat_", cat_encoding)
        )
        encoders <- list(enc)
        produced_features <- enc$produced_features
        requires_features <- enc$requires
      }

      # ---------------------------------------------------------------------
      # Fit result object + predict + derivative + elasticity
      # (drop-in replacement inside private$fit_one_model())
      # ---------------------------------------------------------------------

      fit <- list(
        ok = TRUE,
        model_name = model_name,
        coefficients = opt$par,
        hessian = opt$hessian,
        optim = opt$optim,
        loss = list(type = loss, q = quantile_level),
        residuals = dt_scaled$y - preds_train,
        fitted = preds_train,

        formula = model$formula,
        model_function = model$model_function,
        deriv_function = model$deriv_function %||% NULL,

        param_names = param_names,
        param_links = links,
        cat = list(
          method = cat_encoding,
          shift_vars = shift_vars,
          shift_names = shift_names,
          shift_maps = shift_maps,
          param_raw_vars = param_raw,
          param_maps = param_maps,
          param_enc_names = param_enc_names
        ),

        scale = scale,
        encoders = encoders,
        produced_features = produced_features,
        requires_features = requires_features
      )

      # -------------------------------
      # Helpers shared by predict + deriv
      # -------------------------------
      .build_engineered_newdata <- function(newdata) {
        nd <- data.table::as.data.table(newdata)
        if (length(fit$encoders) > 0) {
          for (enc in fit$encoders) nd <- enc$transform(nd)
        }
        nd
      }

      .build_modifier_mats <- function(nd) {
        # PARAM modifiers (Wp): one-column matrices (or NULL)
        Wp <- setNames(vector("list", length(fit$param_names)), fit$param_names)
        for (p in fit$param_names) {
          outcol <- fit$cat$param_enc_names[[p]]
          if (!is.null(outcol) && outcol %in% names(nd)) {
            Wp[[p]] <- matrix(nd[[outcol]], ncol = 1L, dimnames = list(NULL, outcol))
          } else {
            Wp[[p]] <- NULL
          }
        }

        # SHIFT design matrix (Xs) from engineered cols (if any)
        Xs <- NULL
        if (!is.null(fit$cat$shift_names) && length(fit$cat$shift_names) > 0) {
          cols <- fit$cat$shift_names
          have <- cols[cols %in% names(nd)]
          if (length(have) > 0) Xs <- as.matrix(nd[, ..have])
        }

        list(Wp = Wp, Xs = Xs)
      }

      .build_params_list <- function(x_len, Wp) {
        par   <- fit$coefficients
        pn    <- fit$param_names
        links <- fit$param_links

        params_list <- vector("list", length(pn)); names(params_list) <- pn
        i <- 1L

        for (p in pn) {
          eta <- rep(par[i], x_len); i <- i + 1L
          if (!is.null(Wp[[p]])) {
            eta <- eta + drop(Wp[[p]] %*% par[i]); i <- i + 1L
          }
          inv <- private$.inv_link(links[[p]])
          params_list[[p]] <- inv(eta)
        }

        list(params_list = params_list, i_after_params = i)
      }

      # -------------------------------
      # predict_scaled: unchanged behavior
      # -------------------------------
      fit$predict_scaled <- function(newdata) {
        nd <- .build_engineered_newdata(newdata)
        xs <- private$transform_x_scaled(nd[[fit$scale$x_col]], fit$scale)

        mats <- .build_modifier_mats(nd)
        Wp <- mats$Wp
        Xs <- mats$Xs

        par <- fit$coefficients
        pn <- fit$param_names
        links <- fit$param_links

        augmented_new <- function(x, params) {
          params_list <- vector("list", length(pn)); names(params_list) <- pn
          i <- 1L

          for (p in pn) {
            eta <- rep(params[i], length(x)); i <- i + 1L
            if (!is.null(Wp[[p]])) {
              eta <- eta + drop(Wp[[p]] %*% params[i]); i <- i + 1L
            }
            inv <- private$.inv_link(links[[p]])
            params_list[[p]] <- inv(eta)
          }

          nl <- fit$model_function(x, params_list)

          if (!is.null(Xs)) {
            k <- ncol(Xs)
            beta <- params[i:(i + k - 1L)]
            nl <- nl + drop(Xs %*% beta)
          }
          nl
        }

        augmented_new(xs, par)
      }

      fit$predict <- function(newdata) {
        pred_s <- fit$predict_scaled(newdata)
        private$back_transform_y(pred_s, fit$scale)
      }

      # -------------------------------------------------------------------
      # derivative(): analytic when available, else finite diff
      # - analytic uses fit$deriv_function(x_scaled, params_list)
      # - shift terms do not contribute to d/dx (additive constants)
      # -------------------------------------------------------------------
      fit$derivative <- function(newdata, h = NULL, method = c("auto", "analytic", "central", "forward", "backward")) {
        method <- match.arg(method)

        nd <- .build_engineered_newdata(newdata)

        # scaled prediction + scaled x
        yhat_s <- fit$predict_scaled(nd)
        xs <- private$transform_x_scaled(nd[[fit$scale$x_col]], fit$scale)

        has_analytic <- !is.null(fit$deriv_function) && is.function(fit$deriv_function)

        # ---- analytic path ----
        if ((method %in% c("auto", "analytic")) && has_analytic) {
          mats <- .build_modifier_mats(nd)
          Wp <- mats$Wp

          pl <- .build_params_list(x_len = length(xs), Wp = Wp)
          params_list <- pl$params_list

          d_ys_d_xs <- fit$deriv_function(xs, params_list)

          dydx <- fit$scale$scale_y * d_ys_d_xs / fit$scale$scale_x

          return(data.table::data.table(
            .pred = private$back_transform_y(yhat_s, fit$scale),
            .dydx = as.numeric(dydx)
          ))
        }

        if (method == "analytic" && !has_analytic) {
          stop("This model has no deriv_function; use method='central' (finite diff) or method='auto'.")
        }

        # ---- finite difference path (scaled domain) ----
        if (is.null(h)) h <- 1e-6
        h <- as.numeric(h)
        if (!is.finite(h) || h <= 0) h <- 1e-6

        # clamp steps in scaled domain for stability
        x_plus  <- pmin(pmax(xs + h, 0), 1)
        x_minus <- pmin(pmax(xs - h, 0), 1)

        # score at explicit xs (avoid re-transforming raw x)
        score_at_xs <- function(xs_override) {
          mats <- .build_modifier_mats(nd)
          Wp <- mats$Wp
          Xs <- mats$Xs

          par <- fit$coefficients
          pn <- fit$param_names
          links <- fit$param_links

          augmented_new <- function(x, params) {
            params_list <- vector("list", length(pn)); names(params_list) <- pn
            i <- 1L

            for (p in pn) {
              eta <- rep(params[i], length(x)); i <- i + 1L
              if (!is.null(Wp[[p]])) {
                eta <- eta + drop(Wp[[p]] %*% params[i]); i <- i + 1L
              }
              inv <- private$.inv_link(links[[p]])
              params_list[[p]] <- inv(eta)
            }

            nl <- fit$model_function(x, params_list)

            if (!is.null(Xs)) {
              k <- ncol(Xs)
              beta <- params[i:(i + k - 1L)]
              nl <- nl + drop(Xs %*% beta)
            }
            nl
          }

          augmented_new(xs_override, par)
        }

        f_plus  <- score_at_xs(x_plus)
        f_minus <- score_at_xs(x_minus)

        d_ys_d_xs <- if (method == "forward") {
          (f_plus - yhat_s) / h
        } else if (method == "backward") {
          (yhat_s - f_minus) / h
        } else {
          (f_plus - f_minus) / (2 * h)
        }

        dydx <- fit$scale$scale_y * d_ys_d_xs / fit$scale$scale_x

        data.table::data.table(
          .pred = private$back_transform_y(yhat_s, fit$scale),
          .dydx = as.numeric(dydx)
        )
      }

      fit$elasticity <- function(newdata, h = NULL) {
        dt <- fit$derivative(newdata, h = h, method = "auto")
        nd <- data.table::as.data.table(newdata)
        x <- nd[[fit$scale$x_col]]
        dt[, .elasticity := data.table::fifelse(.pred == 0, NA_real_, .dydx * (x / .pred))]
        dt
      }

      fit
    }
  )
)
