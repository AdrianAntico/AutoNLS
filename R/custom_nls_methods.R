#' @title Extract Coefficients from a Custom NLS Object
#'
#' @description
#'  Extracts the estimated coefficients from a model of class `custom_nls`.
#'
#' @param object An object of class `custom_nls`.
#' @param ... Additional arguments (ignored).
#'
#' @return A named numeric vector of coefficients.
#' @export
coef.custom_nls <- function(object, ...) {
  object$coefficients
}

#' @title Predict Values from a Custom NLS Object
#'
#' @description
#'  Uses a fitted `custom_nls` model to predict values for new data.
#'
#' @param object An object of class `custom_nls`.
#' @param newdata A `data.frame` or `data.table` containing the predictor variable `x`.
#' @param ... Additional arguments (ignored).
#'
#' @return A numeric vector of predicted values.
#' @export
predict.custom_nls <- function(object, newdata, ...) {
  if (!"x" %in% names(newdata)) {
    stop("The newdata argument must contain a column named 'x'.")
  }

  eval(object$formula[[3]], envir = c(list(x = newdata$x), as.list(object$coefficients)))
}

#' @title Extract Residuals from a Custom NLS Object
#'
#' @description
#'  Extracts the residuals of a fitted model of class `custom_nls`.
#'
#' @param object An object of class `custom_nls`.
#' @param ... Additional arguments (ignored).
#'
#' @return A numeric vector of residuals.
#' @export
residuals.custom_nls <- function(object, ...) {
  object$residuals
}

#' @title Extract Fitted Values from a Custom NLS Object
#'
#' @description
#'  Extracts the fitted values from a model of class `custom_nls`.
#'
#' @param object An object of class `custom_nls`.
#' @param ... Additional arguments (ignored).
#'
#' @return A numeric vector of fitted values.
#' @export
fitted.custom_nls <- function(object, ...) {
  object$fitted.values
}

#' @title Calculate Log-Likelihood for a Custom NLS Object
#'
#' @description
#'  Computes the log-likelihood for a model of class `custom_nls`.
#'
#' @param object An object of class `custom_nls`.
#' @param ... Additional arguments (ignored).
#'
#' @return The log-likelihood value as a numeric scalar.
#' @export
logLik.custom_nls <- function(object, ...) {
  n <- length(object$residuals)
  rss <- sum(object$residuals^2)
  -n / 2 * (log(2 * pi) + log(rss / n) + 1)
}

#' @title Summary for a Custom NLS Object
#'
#' @description
#'  Provides a summary of the fitted model of class `custom_nls`.
#'
#' @param object An object of class `custom_nls`.
#' @param ... Additional arguments (ignored).
#'
#' @return A list containing model coefficients, residuals, and fitted values.
#' @export
summary.custom_nls <- function(object, ...) {
  list(
    coefficients = object$coefficients,
    residuals = object$residuals,
    fitted.values = object$fitted.values
  )
}

#' @title AIC for custom_nls objects
#'
#' @description
#'  Computes the Akaike Information Criterion for models of class `custom_nls`.
#'
#' @importFrom stats BIC AIC coef
#'
#' @param object A `custom_nls` object.
#' @param ... Additional arguments (currently ignored).
#' @return The AIC value for the model
#' @export
AIC.custom_nls <- function(object, ...) {
  n <- length(object$fitted.values)
  k <- length(coef(object))
  rss <- sum(object$residuals^2)
  sigma2 <- rss / n

  if (sigma2 <= 0) stop("Residual variance (sigma^2) is non-positive, AIC cannot be computed.")

  log_likelihood <- -n / 2 * (log(2 * pi) + log(sigma2) + 1)
  aic <- -2 * log_likelihood + 2 * k
  return(aic)
}

#' @title BIC for custom_nls objects
#'
#' @description
#'  Computes the Bayesian Information Criterion for models of class `custom_nls`.
#'
#' @importFrom stats BIC AIC coef
#'
#' @param object A `custom_nls` object.
#' @param ... Additional arguments (currently ignored).
#' @return The BIC value for the model.
#' @export
BIC.custom_nls <- function(object, ...) {
  n <- length(object$fitted.values)
  k <- length(coef(object))
  rss <- sum(object$residuals^2)
  sigma2 <- rss / n

  if (sigma2 <= 0) stop("Residual variance (sigma^2) is non-positive, BIC cannot be computed.")

  log_likelihood <- -n / 2 * (log(2 * pi) + log(sigma2) + 1)
  bic <- -2 * log_likelihood + k * log(n)
  return(bic)
}
