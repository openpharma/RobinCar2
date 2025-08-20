#' S3 Methods for `prediction_cf`
#' @param x (`prediction_cf`)\cr the obtained counter-factual prediction object.
#' @param level (`number`)\cr the significance level.
#' @name prediction_cf_methods
#' @return No return value.
NULL

#' @describeIn prediction_cf_methods prints the prediction_cf object.
#' @exportS3Method
#' @keywords internal
print.prediction_cf <- function(x, level = 0.95, ...) {
  assert_number(level, lower = 0, upper = 1)
  cat("Model        : ", deparse(as.formula(x$fit)), "\n")
  cat(
    "Randomization: ",
    deparse(x$treatment_formula),
    " (",
    randomization_schema$schema[randomization_schema$id == x$schema],
    ")\n"
  )
  cat("Variance Type: ", x$variance_name, "\n")
  cat("Marginal Mean: \n")
  stats::printCoefmat(
    confint(x, level = level),
    ...
  )
}

#' Confidence Interval
#' @description Obtain the confidence interval for the marginal mean or the contrast.
#' @name confint
#' @param object Object to construct confidence interval.
#' @param parm (`character` or `integer`) Names of the parameters to construct confidence interval.
#' @param level (`numeric`) Confidence level.
#' @param transform (`function`) Transform function.
#' @param ... Not used.
#' @export
#' @return A `matrix` of the confidence interval.
#' @examples
#' robin_res <- robin_glm(y_b ~ treatment * s1, data = glm_data, treatment = treatment ~ s1, contrast = "log_risk_ratio")
#' confint(robin_res$marginal_mean, level = 0.7)
#' confint(robin_res$contrast, parm = 1:3, level = 0.9)
confint.prediction_cf <- function(object, parm, level = 0.95, ...) {
  assert_number(level, lower = 0, upper = 1)
  if (!missing(parm)) {
    assert(
      check_integerish(parm, lower = 1, upper = length(object$estimate)),
      check_subset(parm, names(object$estimate))
    )
  }
  trt_sd <- sqrt(diag(object$variance))
  ret <- matrix(
    c(
      object$estimate,
      trt_sd,
      object$estimate + trt_sd * qnorm(0.5 - level / 2),
      object$estimate + trt_sd * qnorm(0.5 + level / 2)
    ),
    nrow = length(object$estimate)
  )
  colnames(ret) <- c("Estimate", "Std.Err", sprintf("%s %%", c(0.5 - level / 2, 0.5 + level / 2) * 100))
  row.names(ret) <- names(object$estimate)
  ret[parm, , drop = FALSE]
}
