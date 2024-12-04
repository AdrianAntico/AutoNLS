# Custom methods for "custom_nls" objects

#' Extract Coefficients from a Custom NLS Object
#'
#' Extracts the estimated coefficients from a model of class `custom_nls`.
#'
#' @param object An object of class `custom_nls`.
#' @param ... Additional arguments (ignored).
#'
#' @return A named numeric vector of coefficients.
#' @export
coef.custom_nls <- function(object, ...) {
  object$coefficients
}

#' Predict Values from a Custom NLS Object
#'
#' Uses a fitted `custom_nls` model to predict values for new data.
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

#' Extract Residuals from a Custom NLS Object
#'
#' Extracts the residuals of a fitted model of class `custom_nls`.
#'
#' @param object An object of class `custom_nls`.
#' @param ... Additional arguments (ignored).
#'
#' @return A numeric vector of residuals.
#' @export
residuals.custom_nls <- function(object, ...) {
  object$residuals
}

#' Extract Fitted Values from a Custom NLS Object
#'
#' Extracts the fitted values from a model of class `custom_nls`.
#'
#' @param object An object of class `custom_nls`.
#' @param ... Additional arguments (ignored).
#'
#' @return A numeric vector of fitted values.
#' @export
fitted.custom_nls <- function(object, ...) {
  object$fitted.values
}

#' Calculate Log-Likelihood for a Custom NLS Object
#'
#' Computes the log-likelihood for a model of class `custom_nls`.
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

#' Summary for a Custom NLS Object
#'
#' Provides a summary of the fitted model of class `custom_nls`.
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

#' AIC for custom_nls objects
#'
#' Computes the Akaike Information Criterion (AIC) for models of class `custom_nls`.
#'
#' @param object A `custom_nls` object.
#' @param ... Additional arguments (currently ignored).
#' @return The AIC value for the model.
#' @examples
#' # Assuming `fit` is a fitted `custom_nls` model
#' AIC(fit)
AIC.custom_nls <- function(object, ...) {
  n <- length(object$fitted.values)      # Number of observations
  k <- length(coef(object))             # Number of parameters
  rss <- sum(object$residuals^2)        # Residual sum of squares
  sigma2 <- rss / n                     # Residual variance
  log_likelihood <- -n / 2 * (log(2 * pi) + log(sigma2) + 1)
  aic <- -2 * log_likelihood + 2 * k    # AIC formula
  return(aic)
}

#' BIC for custom_nls objects
#'
#' Computes the Bayesian Information Criterion (BIC) for models of class `custom_nls`.
#'
#' @param object A `custom_nls` object.
#' @param ... Additional arguments (currently ignored).
#' @return The BIC value for the model.
#' @examples
#' # Assuming `fit` is a fitted `custom_nls` model
#' BIC(fit)
BIC.custom_nls <- function(object, ...) {
  n <- length(object$fitted.values)      # Number of observations
  k <- length(coef(object))             # Number of parameters
  rss <- sum(object$residuals^2)        # Residual sum of squares
  sigma2 <- rss / n                     # Residual variance
  log_likelihood <- -n / 2 * (log(2 * pi) + log(sigma2) + 1)
  bic <- -2 * log_likelihood + k * log(n) # BIC formula
  return(bic)
}
