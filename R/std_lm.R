#' Standardized Linear Models
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @param formula (`formula`)\cr the model formula. Must include `trt` to indicate the treatment variable.
#' @param data (`data.frame`)\cr the dataset to be used.
#' @param trt (`string`)\cr the treatment variable.
#' @param ... additional arguments for `lm()`.
#'
#' @details For linear models, we can use the conditional treatment effect to represent the marginal treatment effect
#' because it is collapsible. This function only conduct a `lm` regression and add a class `std_lm` to the returned object
#' to enable summary function.
#' To conduct the standardization method, it is required that `trt` is a factor of two levels: the first is control arm and
#' the second is treatment arm.
#'
#' @export
#'
#' @examples
#' std_lm(Sepal.Length ~ Species, data = subset(iris, Species != "virginica"), trt = "Species")
std_lm <- function(formula, data, trt, ...) {
  assert_formula(formula)
  assert_string(trt)
  assert_subset(trt, all.vars(formula[[length(formula)]]))
  data[[trt]] <- h_validate_treatment(data[[trt]])
  fit <- lm(formula = formula, data = data, ...)
  fit$call <- match.call()
  fit$trt <- trt
  fit$trt_levels <- levels(data[[trt]])
  class(fit) <- c("std_lm", "lm")
  fit
}

#' Methods for `std_lm` Objects
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @param object (`std_lm`)\cr the fitted std_lm object.
#' @return Depends on the method, see Details and Functions.
#'
#' @details
#' `summary` will summarize the `std_lm` model with robust covariance method.
#' @name std_lm_methods
#'
#' @examples
#' object <- std_lm(Sepal.Length ~ Species, data = iris)
NULL

#' Summary of `std_lm` object
#' @description Summarize the `std_lm` object.
#'
#' @param object (`std_lm`)\cr object.
#' @param vcov (`string`)\cr or (`function`)\cr to obtain covariance.
#' @param ... not used.
#'
#' @method summary std_lm
#' @exportS3Method
#' @keywords internal
#' @examples
#' summary(object)
summary.std_lm <- function(object, vcov = "HC3", ...) {
  components <- list(
    call = getCall(object),
    terms = terms(object),
    residuals = residuals(object),
    aliased = is.na(coef(object)),
    trt_var = object$trt,
    trt_levels = object$trt_levels
  )
  components$vcov_method <- "HC3"
  components$vcov <- sandwich::vcovHC(object, type = vcov)
  est <- coef(object)
  se <- sqrt(diag(components$vcov))
  t_val <- est / se
  p_val <- 2 * pt(abs(t_val), lower.tail = FALSE, df = object$df.residual)
  components$coefficients <- cbind(
    Estimate = est,
    `Std. Error` = diag(components$vcov),
    `t value` = t_val,
    `Pr(>|t|)` = p_val
  )
  structure(
    components,
    class = "summary.std_lm"
  )
}

#' Print of `summary.std_lm` object
#' @description Print the `summary.std_lm` object.
#'
#' @param x (`summary.std_lm`)\cr object.
#' @param ... further arguments for `stats::printCoefmat`.
#'
#' @method print summary.std_lm
#' @exportS3Method
#' @keywords internal
print.summary.std_lm <- function(x, ...) {
  cat("std_lm fit\n\n")
  cat("Formula:      ", deparse(x$call$formula), "\n")
  cat("Covariance:   ", x$vcov_method, "\n")
  cat("Treatment Var:", x$trt_var, "(", x$trt_levels[1], "vs" , x$trt_levels[2], ")\n")
  cat("Coefficients: \n")
  stats::printCoefmat(
    x$coefficients[paste0(x$trt_var, x$trt_levels[2]), , drop = FALSE],
    zap.ind = 3,
    ...
  )
}
