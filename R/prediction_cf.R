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

#' @export
#' @rdname confint
confint.prediction_cf <- function(object, parm, level = 0.95, ...) {
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
  ret
}
