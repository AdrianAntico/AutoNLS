#' Built-in AutoNLS vNext model registry
#'
#' @return A named list of nonlinear model specifications.
nls_model_registry <- function() {
  eps <- 1e-8
  safe_pos <- function(x) pmax(as.numeric(x), eps)
  safe_den <- function(x) ifelse(abs(x) < eps, ifelse(x < 0, -eps, eps), x)
  safe_exp <- function(x) exp(pmax(pmin(x, 60), -60))
  safe_log <- function(x) log(safe_pos(x))

  spec <- function(
    model_name,
    family,
    description,
    formula,
    parameter_names,
    start_params,
    model_function,
    derivative_function = NULL,
    lower_bounds = NULL,
    upper_bounds = NULL,
    domain = "x must be numeric.",
    tags = character(0),
    monotonic = "depends on fitted parameters",
    status = "stable"
  ) {
    if (is.null(lower_bounds)) lower_bounds <- stats::setNames(rep(-Inf, length(parameter_names)), parameter_names)
    if (is.null(upper_bounds)) upper_bounds <- stats::setNames(rep(Inf, length(parameter_names)), parameter_names)
    list(
      model_name = model_name,
      family = family,
      description = description,
      formula = formula,
      parameter_names = parameter_names,
      start_params = start_params[parameter_names],
      lower_bounds = lower_bounds[parameter_names],
      upper_bounds = upper_bounds[parameter_names],
      domain = domain,
      model_function = model_function,
      derivative_function = derivative_function,
      tags = tags,
      monotonic = monotonic,
      supports_derivative = is.function(derivative_function),
      status = status
    )
  }

  registry <- list(
    Linear = spec(
      "Linear", "linear", "Straight-line baseline for monotone functional relationships.",
      "y = a + b * x", c("a", "b"), c(a = 0, b = 1),
      lower_bounds = c(a = -Inf, b = -Inf), upper_bounds = c(a = Inf, b = Inf),
      tags = c("linear", "baseline"),
      monotonic = "increasing when b is positive; decreasing when b is negative",
      model_function = function(x, params) params[["a"]] + params[["b"]] * as.numeric(x),
      derivative_function = function(x, params) rep(params[["b"]], length(x))
    ),
    Hill2Model = spec(
      "Hill2Model", "saturation", "Two-parameter Hill curve scaled to a unit asymptote.",
      "y = x^b / (a^b + x^b)", c("a", "b"), c(a = 0.5, b = 1),
      lower_bounds = c(a = eps, b = eps), upper_bounds = c(a = Inf, b = Inf),
      domain = "x should be non-negative; guarded at a small epsilon.",
      tags = c("saturation", "hill", "dose_response"), monotonic = "increasing when a and b are positive",
      model_function = function(x, params) {
        xx <- safe_pos(x); a <- params[["a"]]; b <- params[["b"]]
        xx^b / safe_den(a^b + xx^b)
      },
      derivative_function = function(x, params) {
        xx <- safe_pos(x); a <- params[["a"]]; b <- params[["b"]]
        (b * a^b * xx^(b - 1)) / safe_den((a^b + xx^b)^2)
      }
    ),
    Hill = spec(
      "Hill", "saturation", "Hill saturation curve for diminishing returns and dose response.",
      "y = a * x^b / (c^b + x^b)", c("a", "b", "c"), c(a = 1, b = 1, c = 0.5),
      lower_bounds = c(a = -Inf, b = eps, c = eps), upper_bounds = c(a = Inf, b = Inf, c = Inf),
      domain = "x should be non-negative; guarded at a small epsilon.",
      tags = c("saturation", "hill", "sigmoid", "diminishing_returns"),
      monotonic = "increasing when a, b, and c are positive",
      model_function = function(x, params) {
        xx <- safe_pos(x); a <- params[["a"]]; b <- params[["b"]]; c <- params[["c"]]
        a * xx^b / safe_den(c^b + xx^b)
      },
      derivative_function = function(x, params) {
        xx <- safe_pos(x); a <- params[["a"]]; b <- params[["b"]]; c <- params[["c"]]
        a * (b * c^b * xx^(b - 1)) / safe_den((c^b + xx^b)^2)
      }
    ),
    Hill5Model = spec(
      "Hill5Model", "saturation", "Five-parameter Hill curve with baseline and linear tail.",
      "y = a * x^b / (c^b + x^b) + d + e * x", c("a", "b", "c", "d", "e"), c(a = 1, b = 1, c = 0.5, d = 0, e = 0),
      lower_bounds = c(a = -Inf, b = eps, c = eps, d = -Inf, e = -Inf), upper_bounds = c(a = Inf, b = Inf, c = Inf, d = Inf, e = Inf),
      domain = "x should be non-negative; guarded at a small epsilon.",
      tags = c("saturation", "hill", "linear_tail"), status = "experimental",
      model_function = function(x, params) {
        xx <- safe_pos(x); a <- params[["a"]]; b <- params[["b"]]; c <- params[["c"]]
        a * xx^b / safe_den(c^b + xx^b) + params[["d"]] + params[["e"]] * xx
      },
      derivative_function = function(x, params) {
        xx <- safe_pos(x); a <- params[["a"]]; b <- params[["b"]]; c <- params[["c"]]
        a * (b * c^b * xx^(b - 1)) / safe_den((c^b + xx^b)^2) + params[["e"]]
      }
    ),
    HillSwitchpointModel = spec(
      "HillSwitchpointModel", "saturation", "Mixture of two Hill curves with a smooth logistic switch point.",
      "y = w * H1(x) + (1 - w) * H2(x)", c("a", "b", "c", "d", "e", "f", "s", "k"),
      c(a = 1, b = 1, c = 0.35, d = 0.5, e = 1, f = 0.65, s = 0.5, k = 5),
      lower_bounds = c(a = -Inf, b = eps, c = eps, d = -Inf, e = eps, f = eps, s = -Inf, k = -50),
      upper_bounds = c(a = Inf, b = Inf, c = Inf, d = Inf, e = Inf, f = Inf, s = Inf, k = 50),
      domain = "x should be non-negative; guarded at a small epsilon.",
      tags = c("saturation", "hill", "switchpoint"), status = "experimental",
      model_function = function(x, params) {
        xx <- safe_pos(x); a <- params[["a"]]; b <- params[["b"]]; c <- params[["c"]]
        d <- params[["d"]]; e <- params[["e"]]; f <- params[["f"]]; s <- params[["s"]]; k <- params[["k"]]
        w <- 1 / (1 + safe_exp(-k * (xx - s)))
        h1 <- a * xx^b / safe_den(c^b + xx^b)
        h2 <- d * xx^e / safe_den(f^e + xx^e)
        w * h1 + (1 - w) * h2
      },
      derivative_function = function(x, params) {
        xx <- safe_pos(x); a <- params[["a"]]; b <- params[["b"]]; c <- params[["c"]]
        d <- params[["d"]]; e <- params[["e"]]; f <- params[["f"]]; s <- params[["s"]]; k <- params[["k"]]
        w <- 1 / (1 + safe_exp(-k * (xx - s))); wp <- k * w * (1 - w)
        h1 <- a * xx^b / safe_den(c^b + xx^b)
        h2 <- d * xx^e / safe_den(f^e + xx^e)
        h1p <- a * (b * c^b * xx^(b - 1)) / safe_den((c^b + xx^b)^2)
        h2p <- d * (e * f^e * xx^(e - 1)) / safe_den((f^e + xx^e)^2)
        wp * (h1 - h2) + w * h1p + (1 - w) * h2p
      }
    ),
    HillQuad = spec(
      "HillQuad", "saturation", "Hill curve with a quadratic term.",
      "y = a * x^b / (c + x^b) + d * x^2", c("a", "b", "c", "d"), c(a = 1, b = 1, c = 0.5, d = 0),
      lower_bounds = c(a = -Inf, b = eps, c = eps, d = -Inf), upper_bounds = c(a = Inf, b = Inf, c = Inf, d = Inf),
      domain = "x should be non-negative; guarded at a small epsilon.",
      tags = c("saturation", "hill", "quadratic"), status = "experimental",
      model_function = function(x, params) {
        xx <- safe_pos(x); a <- params[["a"]]; b <- params[["b"]]; c <- params[["c"]]
        a * xx^b / safe_den(c + xx^b) + params[["d"]] * xx^2
      },
      derivative_function = function(x, params) {
        xx <- safe_pos(x); a <- params[["a"]]; b <- params[["b"]]; c <- params[["c"]]
        a * (b * c * xx^(b - 1)) / safe_den((c + xx^b)^2) + 2 * params[["d"]] * xx
      }
    ),
    Logistic = spec(
      "Logistic", "sigmoid", "Logistic growth curve with asymptote, slope, and midpoint.",
      "y = a / (1 + exp(-b * (x - c)))", c("a", "b", "c"), c(a = 1, b = 1, c = 0.5),
      lower_bounds = c(a = -Inf, b = -50, c = -Inf), upper_bounds = c(a = Inf, b = 50, c = Inf),
      tags = c("sigmoid", "growth", "saturation"),
      monotonic = "increasing when a and b have the same sign",
      model_function = function(x, params) {
        params[["a"]] / (1 + safe_exp(-params[["b"]] * (as.numeric(x) - params[["c"]])))
      },
      derivative_function = function(x, params) {
        z <- safe_exp(-params[["b"]] * (as.numeric(x) - params[["c"]]))
        params[["a"]] * params[["b"]] * z / safe_den((1 + z)^2)
      }
    ),
    Logistic5Param = spec(
      "Logistic5Param", "sigmoid", "Five-parameter logistic curve.",
      "y = d + (a - d) / (1 + (x / c)^b)^g", c("a", "b", "c", "d", "g"), c(a = 1, b = 1, c = 0.5, d = 0, g = 1),
      lower_bounds = c(a = -Inf, b = eps, c = eps, d = -Inf, g = eps), upper_bounds = c(a = Inf, b = Inf, c = Inf, d = Inf, g = Inf),
      domain = "x should be positive; guarded at a small epsilon.",
      tags = c("sigmoid", "dose_response"), status = "experimental",
      model_function = function(x, params) {
        xx <- safe_pos(x); h <- 1 + (xx / params[["c"]])^params[["b"]]
        params[["d"]] + (params[["a"]] - params[["d"]]) / h^params[["g"]]
      },
      derivative_function = function(x, params) {
        xx <- safe_pos(x); a <- params[["a"]]; b <- params[["b"]]; c <- params[["c"]]; d <- params[["d"]]; g <- params[["g"]]
        h <- 1 + (xx / c)^b
        -(a - d) * g * h^(-g - 1) * (b * xx^(b - 1) / c^b)
      }
    ),
    ExponentialDecay = spec(
      "ExponentialDecay", "decay", "Exponential decay curve.",
      "y = a * exp(-b * x)", c("a", "b"), c(a = 1, b = 0.1),
      lower_bounds = c(a = -Inf, b = -50), upper_bounds = c(a = Inf, b = 50),
      tags = c("decay", "exponential"), monotonic = "decreasing when a and b are positive",
      model_function = function(x, params) params[["a"]] * safe_exp(-params[["b"]] * as.numeric(x)),
      derivative_function = function(x, params) -params[["a"]] * params[["b"]] * safe_exp(-params[["b"]] * as.numeric(x))
    ),
    ExpDecayPlateau = spec(
      "ExpDecayPlateau", "decay", "Exponential decay with a plateau.",
      "y = a * exp(-b * x) + c", c("a", "b", "c"), c(a = 1, b = 0.1, c = 0),
      lower_bounds = c(a = -Inf, b = -50, c = -Inf), upper_bounds = c(a = Inf, b = 50, c = Inf),
      tags = c("decay", "exponential", "plateau"),
      model_function = function(x, params) params[["a"]] * safe_exp(-params[["b"]] * as.numeric(x)) + params[["c"]],
      derivative_function = function(x, params) -params[["a"]] * params[["b"]] * safe_exp(-params[["b"]] * as.numeric(x))
    ),
    Exp2OrderDecay = spec(
      "Exp2OrderDecay", "decay", "Two-component exponential decay.",
      "y = a * exp(-b * x) + c * exp(-d * x)", c("a", "b", "c", "d"), c(a = 1, b = 0.1, c = 0.5, d = 0.05),
      lower_bounds = c(a = -Inf, b = -50, c = -Inf, d = -50), upper_bounds = c(a = Inf, b = 50, c = Inf, d = 50),
      tags = c("decay", "exponential"), status = "experimental",
      model_function = function(x, params) params[["a"]] * safe_exp(-params[["b"]] * as.numeric(x)) + params[["c"]] * safe_exp(-params[["d"]] * as.numeric(x)),
      derivative_function = function(x, params) -params[["a"]] * params[["b"]] * safe_exp(-params[["b"]] * as.numeric(x)) - params[["c"]] * params[["d"]] * safe_exp(-params[["d"]] * as.numeric(x))
    ),
    Gompertz = spec(
      "Gompertz", "sigmoid", "Asymmetric sigmoid growth curve.",
      "y = a * exp(-b * exp(-c * x))", c("a", "b", "c"), c(a = 1, b = 1, c = 1),
      lower_bounds = c(a = -Inf, b = eps, c = -50), upper_bounds = c(a = Inf, b = Inf, c = 50),
      tags = c("sigmoid", "growth", "asymmetric"),
      monotonic = "increasing when a, b, and c are positive",
      model_function = function(x, params) params[["a"]] * safe_exp(-params[["b"]] * safe_exp(-params[["c"]] * as.numeric(x))),
      derivative_function = function(x, params) {
        t <- safe_exp(-params[["c"]] * as.numeric(x))
        params[["a"]] * params[["b"]] * params[["c"]] * t * safe_exp(-params[["b"]] * t)
      }
    ),
    Gompertz4Param = spec(
      "Gompertz4Param", "sigmoid", "Four-parameter Gompertz curve with vertical shift.",
      "y = a * exp(-exp(b - c * x)) + d", c("a", "b", "c", "d"), c(a = 1, b = 1, c = 0.1, d = 0),
      lower_bounds = c(a = -Inf, b = -50, c = -50, d = -Inf), upper_bounds = c(a = Inf, b = 50, c = 50, d = Inf),
      tags = c("sigmoid", "growth", "asymmetric"), status = "experimental",
      model_function = function(x, params) params[["a"]] * safe_exp(-safe_exp(params[["b"]] - params[["c"]] * as.numeric(x))) + params[["d"]],
      derivative_function = function(x, params) {
        u <- safe_exp(params[["b"]] - params[["c"]] * as.numeric(x))
        params[["a"]] * params[["c"]] * u * safe_exp(-u)
      }
    ),
    MichaelisMenten = spec(
      "MichaelisMenten", "saturation", "Michaelis-Menten saturation kinetics.",
      "y = Vmax * x / (Km + x)", c("Vmax", "Km"), c(Vmax = 1, Km = 0.5),
      lower_bounds = c(Vmax = -Inf, Km = eps), upper_bounds = c(Vmax = Inf, Km = Inf),
      domain = "x should be non-negative; guarded at a small epsilon.",
      tags = c("saturation", "kinetics"), monotonic = "increasing when Vmax and Km are positive",
      model_function = function(x, params) {
        xx <- safe_pos(x); params[["Vmax"]] * xx / safe_den(params[["Km"]] + xx)
      },
      derivative_function = function(x, params) {
        xx <- safe_pos(x); params[["Vmax"]] * params[["Km"]] / safe_den((params[["Km"]] + xx)^2)
      }
    ),
    WeibullType1 = spec(
      "WeibullType1", "sigmoid", "Weibull type 1 curve.",
      "y = a * exp(-exp(b - c * x))", c("a", "b", "c"), c(a = 1, b = 1, c = 0.1),
      lower_bounds = c(a = -Inf, b = -50, c = -50), upper_bounds = c(a = Inf, b = 50, c = 50),
      tags = c("sigmoid", "weibull"), status = "experimental",
      model_function = function(x, params) params[["a"]] * safe_exp(-safe_exp(params[["b"]] - params[["c"]] * as.numeric(x))),
      derivative_function = function(x, params) {
        u <- safe_exp(params[["b"]] - params[["c"]] * as.numeric(x))
        params[["a"]] * params[["c"]] * u * safe_exp(-u)
      }
    ),
    WeibullType2 = spec(
      "WeibullType2", "saturation", "Weibull type 2 saturation curve.",
      "y = a * (1 - exp(-b * x^c))", c("a", "b", "c"), c(a = 1, b = 0.1, c = 1),
      lower_bounds = c(a = -Inf, b = eps, c = eps), upper_bounds = c(a = Inf, b = Inf, c = Inf),
      domain = "x should be non-negative; guarded at a small epsilon.",
      tags = c("saturation", "weibull"),
      model_function = function(x, params) {
        xx <- safe_pos(x); params[["a"]] * (1 - safe_exp(-params[["b"]] * xx^params[["c"]]))
      },
      derivative_function = function(x, params) {
        xx <- safe_pos(x); params[["a"]] * params[["b"]] * params[["c"]] * xx^(params[["c"]] - 1) * safe_exp(-params[["b"]] * xx^params[["c"]])
      }
    ),
    Asymptotic = spec(
      "Asymptotic", "saturation", "Asymptotic regression curve for limited growth.",
      "y = a - (a - b) * exp(-c * x)", c("a", "b", "c"), c(a = 1, b = 0, c = 0.1),
      lower_bounds = c(a = -Inf, b = -Inf, c = -50), upper_bounds = c(a = Inf, b = Inf, c = 50),
      tags = c("saturation", "growth", "asymptotic"),
      model_function = function(x, params) params[["a"]] - (params[["a"]] - params[["b"]]) * safe_exp(-params[["c"]] * as.numeric(x)),
      derivative_function = function(x, params) (params[["a"]] - params[["b"]]) * params[["c"]] * safe_exp(-params[["c"]] * as.numeric(x))
    ),
    PowerCurve = spec(
      "PowerCurve", "power", "Power curve for scaling relationships.",
      "y = a * x^b", c("a", "b"), c(a = 1, b = 1),
      lower_bounds = c(a = -Inf, b = -10), upper_bounds = c(a = Inf, b = 10),
      domain = "x should be positive; guarded at a small epsilon.",
      tags = c("power", "scaling"),
      model_function = function(x, params) params[["a"]] * safe_pos(x)^params[["b"]],
      derivative_function = function(x, params) params[["a"]] * params[["b"]] * safe_pos(x)^(params[["b"]] - 1)
    ),
    RectangularHyperbola = spec(
      "RectangularHyperbola", "saturation", "Rectangular hyperbola saturation curve.",
      "y = a * x / (b + x)", c("a", "b"), c(a = 1, b = 0.5),
      lower_bounds = c(a = -Inf, b = eps), upper_bounds = c(a = Inf, b = Inf),
      domain = "x should be non-negative; guarded at a small epsilon.",
      tags = c("saturation", "hyperbola"),
      model_function = function(x, params) {
        xx <- safe_pos(x); params[["a"]] * xx / safe_den(params[["b"]] + xx)
      },
      derivative_function = function(x, params) {
        xx <- safe_pos(x); params[["a"]] * params[["b"]] / safe_den((params[["b"]] + xx)^2)
      }
    ),
    Richards = spec(
      "Richards", "sigmoid", "Richards curve, a generalized logistic growth model.",
      "y = a / (1 + exp(-b * (x - c)))^d", c("a", "b", "c", "d"), c(a = 1, b = 1, c = 0.5, d = 1),
      lower_bounds = c(a = -Inf, b = -50, c = -Inf, d = eps), upper_bounds = c(a = Inf, b = 50, c = Inf, d = Inf),
      tags = c("sigmoid", "growth"), status = "experimental",
      model_function = function(x, params) {
        g <- 1 + safe_exp(-params[["b"]] * (as.numeric(x) - params[["c"]]))
        params[["a"]] / g^params[["d"]]
      },
      derivative_function = function(x, params) {
        z <- safe_exp(-params[["b"]] * (as.numeric(x) - params[["c"]]))
        params[["a"]] * params[["d"]] * params[["b"]] * z / (1 + z)^(params[["d"]] + 1)
      }
    ),
    ChapmanRichards = spec(
      "ChapmanRichards", "growth", "Chapman-Richards growth curve.",
      "y = a * (1 - exp(-b * x))^c", c("a", "b", "c"), c(a = 1, b = 0.1, c = 2),
      lower_bounds = c(a = -Inf, b = eps, c = eps), upper_bounds = c(a = Inf, b = Inf, c = Inf),
      domain = "x should be non-negative; guarded at a small epsilon.",
      tags = c("growth", "saturation"), status = "experimental",
      model_function = function(x, params) {
        g <- pmax(1 - safe_exp(-params[["b"]] * safe_pos(x)), eps)
        params[["a"]] * g^params[["c"]]
      },
      derivative_function = function(x, params) {
        xx <- safe_pos(x); g <- pmax(1 - safe_exp(-params[["b"]] * xx), eps)
        params[["a"]] * params[["c"]] * g^(params[["c"]] - 1) * params[["b"]] * safe_exp(-params[["b"]] * xx)
      }
    ),
    HyperbolicTangent = spec(
      "HyperbolicTangent", "sigmoid", "Hyperbolic tangent sigmoid curve.",
      "y = a * tanh(b * x + c)", c("a", "b", "c"), c(a = 1, b = 0.1, c = 0),
      lower_bounds = c(a = -Inf, b = -50, c = -Inf), upper_bounds = c(a = Inf, b = 50, c = Inf),
      tags = c("sigmoid", "tanh"),
      model_function = function(x, params) params[["a"]] * tanh(params[["b"]] * as.numeric(x) + params[["c"]]),
      derivative_function = function(x, params) {
        z <- tanh(params[["b"]] * as.numeric(x) + params[["c"]])
        params[["a"]] * params[["b"]] * (1 - z^2)
      }
    ),
    BetaModel = spec(
      "BetaModel", "dose_response", "Beta-like dose-response curve with hook effects.",
      "y = a + (b - a) * (1 + (x / c)^d)^-e", c("a", "b", "c", "d", "e"), c(a = 0, b = 1, c = 0.5, d = 1, e = 1),
      lower_bounds = c(a = -Inf, b = -Inf, c = eps, d = eps, e = eps), upper_bounds = c(a = Inf, b = Inf, c = Inf, d = Inf, e = Inf),
      domain = "x should be positive; guarded at a small epsilon.",
      tags = c("dose_response", "hook"), status = "experimental",
      model_function = function(x, params) {
        xx <- safe_pos(x); params[["a"]] + (params[["b"]] - params[["a"]]) * (1 + (xx / params[["c"]])^params[["d"]])^-params[["e"]]
      },
      derivative_function = function(x, params) {
        xx <- safe_pos(x); h <- 1 + (xx / params[["c"]])^params[["d"]]
        (params[["b"]] - params[["a"]]) * (-params[["e"]]) * h^(-params[["e"]] - 1) * (params[["d"]] * xx^(params[["d"]] - 1) / params[["c"]]^params[["d"]])
      }
    ),
    StretchedExponential = spec(
      "StretchedExponential", "decay", "Stretched exponential decay with floor.",
      "y = d + (a - d) * exp(-(x / b)^c)", c("a", "b", "c", "d"), c(a = 1, b = 0.5, c = 1, d = 0),
      lower_bounds = c(a = -Inf, b = eps, c = eps, d = -Inf), upper_bounds = c(a = Inf, b = Inf, c = Inf, d = Inf),
      domain = "x should be non-negative; guarded at a small epsilon.",
      tags = c("decay", "exponential"), status = "experimental",
      model_function = function(x, params) {
        xx <- safe_pos(x); params[["d"]] + (params[["a"]] - params[["d"]]) * safe_exp(-(xx / params[["b"]])^params[["c"]])
      },
      derivative_function = function(x, params) {
        xx <- safe_pos(x); t <- (xx / params[["b"]])^params[["c"]]
        -(params[["a"]] - params[["d"]]) * safe_exp(-t) * params[["c"]] * xx^(params[["c"]] - 1) / params[["b"]]^params[["c"]]
      }
    ),
    HyperbolicDecay = spec(
      "HyperbolicDecay", "decay", "Hyperbolic decay curve with floor.",
      "y = d + (a - d) / (1 + x / b^c)", c("a", "b", "c", "d"), c(a = 1, b = 0.5, c = 1, d = 0),
      lower_bounds = c(a = -Inf, b = eps, c = eps, d = -Inf), upper_bounds = c(a = Inf, b = Inf, c = Inf, d = Inf),
      domain = "x should be non-negative; guarded at a small epsilon.",
      tags = c("decay", "hyperbola"), status = "experimental",
      model_function = function(x, params) {
        xx <- safe_pos(x); params[["d"]] + (params[["a"]] - params[["d"]]) / safe_den(1 + xx / params[["b"]]^params[["c"]])
      },
      derivative_function = function(x, params) {
        xx <- safe_pos(x); den <- safe_den(1 + xx / params[["b"]]^params[["c"]])
        -(params[["a"]] - params[["d"]]) / params[["b"]]^params[["c"]] / den^2
      }
    ),
    GompertzDecay = spec(
      "GompertzDecay", "decay", "Gompertz-style asymmetric decay curve.",
      "y = d + (a - d) * exp(-b * exp(c * x))", c("a", "b", "c", "d"), c(a = 1, b = 1, c = 0.1, d = 0),
      lower_bounds = c(a = -Inf, b = eps, c = -50, d = -Inf), upper_bounds = c(a = Inf, b = Inf, c = 50, d = Inf),
      tags = c("decay", "gompertz", "asymmetric"), status = "experimental",
      model_function = function(x, params) {
        params[["d"]] + (params[["a"]] - params[["d"]]) * safe_exp(-params[["b"]] * safe_exp(params[["c"]] * as.numeric(x)))
      },
      derivative_function = function(x, params) {
        u <- safe_exp(params[["c"]] * as.numeric(x))
        -(params[["a"]] - params[["d"]]) * params[["b"]] * params[["c"]] * u * safe_exp(-params[["b"]] * u)
      }
    ),
    InverseHill = spec(
      "InverseHill", "decay", "Inverse Hill curve for saturating decay.",
      "y = a / (1 + (x / c)^b)", c("a", "b", "c"), c(a = 1, b = 1, c = 0.5),
      lower_bounds = c(a = -Inf, b = eps, c = eps), upper_bounds = c(a = Inf, b = Inf, c = Inf),
      domain = "x should be positive; guarded at a small epsilon.",
      tags = c("decay", "hill", "saturation"),
      model_function = function(x, params) {
        xx <- safe_pos(x); params[["a"]] / safe_den(1 + (xx / params[["c"]])^params[["b"]])
      },
      derivative_function = function(x, params) {
        xx <- safe_pos(x); h <- 1 + (xx / params[["c"]])^params[["b"]]
        -params[["a"]] * params[["b"]] * xx^(params[["b"]] - 1) / params[["c"]]^params[["b"]] / h^2
      }
    ),
    ShiftedExponentialDecay = spec(
      "ShiftedExponentialDecay", "decay", "Exponential decay with vertical shift.",
      "y = a * exp(-b * x) + c", c("a", "b", "c"), c(a = 1, b = 0.1, c = 0),
      lower_bounds = c(a = -Inf, b = -50, c = -Inf), upper_bounds = c(a = Inf, b = 50, c = Inf),
      tags = c("decay", "exponential", "shift"),
      model_function = function(x, params) params[["a"]] * safe_exp(-params[["b"]] * as.numeric(x)) + params[["c"]],
      derivative_function = function(x, params) -params[["a"]] * params[["b"]] * safe_exp(-params[["b"]] * as.numeric(x))
    ),
    NegativePowerFunction = spec(
      "NegativePowerFunction", "decay", "Negative power decay curve.",
      "y = a * x^-b", c("a", "b"), c(a = 1, b = 1),
      lower_bounds = c(a = -Inf, b = -10), upper_bounds = c(a = Inf, b = 10),
      domain = "x should be positive; guarded at a small epsilon.",
      tags = c("decay", "power"),
      model_function = function(x, params) params[["a"]] * safe_pos(x)^(-params[["b"]]),
      derivative_function = function(x, params) -params[["a"]] * params[["b"]] * safe_pos(x)^(-params[["b"]] - 1)
    ),
    NegativeLogisticDecay = spec(
      "NegativeLogisticDecay", "decay", "Logistic-shaped decay curve.",
      "y = a / (1 + exp(b * (x - c)))", c("a", "b", "c"), c(a = 1, b = 1, c = 0.5),
      lower_bounds = c(a = -Inf, b = -50, c = -Inf), upper_bounds = c(a = Inf, b = 50, c = Inf),
      tags = c("decay", "sigmoid"),
      model_function = function(x, params) params[["a"]] / (1 + safe_exp(params[["b"]] * (as.numeric(x) - params[["c"]]))),
      derivative_function = function(x, params) {
        z <- safe_exp(params[["b"]] * (as.numeric(x) - params[["c"]]))
        -params[["a"]] * params[["b"]] * z / safe_den((1 + z)^2)
      }
    ),
    LogLinearDecay = spec(
      "LogLinearDecay", "decay", "Log-linear decay curve.",
      "y = a - b * log(x)", c("a", "b"), c(a = 1, b = 1),
      lower_bounds = c(a = -Inf, b = -Inf), upper_bounds = c(a = Inf, b = Inf),
      domain = "x should be positive; log guarded at a small epsilon.",
      tags = c("decay", "log"), status = "experimental",
      model_function = function(x, params) params[["a"]] - params[["b"]] * safe_log(x),
      derivative_function = function(x, params) -params[["b"]] / safe_pos(x)
    ),
    PolynomialDecay = spec(
      "PolynomialDecay", "decay", "Quadratic polynomial decay/growth curve.",
      "y = a + b * x + c * x^2", c("a", "b", "c"), c(a = 1, b = -1, c = 0),
      lower_bounds = c(a = -Inf, b = -Inf, c = -Inf), upper_bounds = c(a = Inf, b = Inf, c = Inf),
      tags = c("polynomial", "quadratic"), status = "experimental",
      model_function = function(x, params) params[["a"]] + params[["b"]] * as.numeric(x) + params[["c"]] * as.numeric(x)^2,
      derivative_function = function(x, params) params[["b"]] + 2 * params[["c"]] * as.numeric(x)
    ),
    InvertedSigma = spec(
      "InvertedSigma", "decay", "Inverted sigmoid curve.",
      "y = a - b / (1 + exp(-c * (x - d)))", c("a", "b", "c", "d"), c(a = 1, b = 1, c = 1, d = 0.5),
      lower_bounds = c(a = -Inf, b = -Inf, c = -50, d = -Inf), upper_bounds = c(a = Inf, b = Inf, c = 50, d = Inf),
      tags = c("decay", "sigmoid"), status = "experimental",
      model_function = function(x, params) params[["a"]] - params[["b"]] / (1 + safe_exp(-params[["c"]] * (as.numeric(x) - params[["d"]]))),
      derivative_function = function(x, params) {
        z <- safe_exp(-params[["c"]] * (as.numeric(x) - params[["d"]]))
        -params[["b"]] * params[["c"]] * z / safe_den((1 + z)^2)
      }
    ),
    ArctangentDecay = spec(
      "ArctangentDecay", "decay", "Arctangent-shaped decay curve.",
      "y = a - b * atan(c * x)", c("a", "b", "c"), c(a = 1, b = 1, c = 1),
      lower_bounds = c(a = -Inf, b = -Inf, c = -Inf), upper_bounds = c(a = Inf, b = Inf, c = Inf),
      tags = c("decay", "arctangent"), status = "experimental",
      model_function = function(x, params) params[["a"]] - params[["b"]] * atan(params[["c"]] * as.numeric(x)),
      derivative_function = function(x, params) -params[["b"]] * params[["c"]] / (1 + (params[["c"]] * as.numeric(x))^2)
    )
  )

  registry
}

#' List available AutoNLS vNext models
#'
#' @param model_status One of "stable", "experimental", or "all".
#' @return A data.table with one row per registered model.
#' @export
list_nls_models <- function(model_status = c("stable", "experimental", "all")) {
  model_status <- match.arg(model_status)
  registry <- nls_model_registry()
  rows <- data.table::rbindlist(lapply(registry, function(spec) {
    data.table::data.table(
      model_name = spec$model_name,
      family = spec$family,
      description = spec$description,
      formula = spec$formula,
      n_params = length(spec$parameter_names),
      supports_derivative = isTRUE(spec$supports_derivative),
      status = spec$status,
      tags = paste(spec$tags, collapse = ", ")
    )
  }), fill = TRUE)
  if (!identical(model_status, "all")) rows <- rows[status == model_status]
  rows[]
}
