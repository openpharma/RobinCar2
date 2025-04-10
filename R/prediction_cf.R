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
  assert_number(level, lower = 0.5, upper = 1)
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
  trt_sd <- sqrt(diag(x$variance))
  m_mat <- matrix(
    c(
      x$estimate,
      trt_sd,
      x$estimate + trt_sd * qnorm(0.5 - level / 2),
      x$estimate + trt_sd * qnorm(0.5 + level / 2)
    ),
    nrow = length(x$estimate)
  )
  colnames(m_mat) <- c("Estimate", "Std.Err", sprintf("%s %%", c(0.5 - level / 2, 0.5 + level / 2) * 100))
  row.names(m_mat) <- names(x$estimate)
  stats::printCoefmat(
    m_mat,
    ...
  )
}
