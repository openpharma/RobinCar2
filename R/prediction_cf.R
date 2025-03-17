#' S3 Methods for `prediction_cf`
#' @param x (`prediction_cf`)\cr the obtained counter-factual prediction object.
#' @name prediction_cf_methods
NULL

#' @describeIn prediction_cf_methods prints the prediction_cf object.
#' @exportS3Method
#' @keywords internal
print.prediction_cf <- function(x, level = 0.95, ...) {
  assert_number(level, lower = 0.5, upper = 1)
  cat("Model        : ", deparse(as.formula(attr(x, "fit"))), "\n")
  cat(
    "Randomization: ",
    deparse(attr(x, "treatment_formula")),
    " (",
    randomization_schema$schema[randomization_schema$id == attr(x, "schema")],
    ")\n"
  )
  cat("Variance Type: ", attr(x, "variance_name"), "\n")
  cat("Marginal Mean: \n")
  trt_sd <- sqrt(diag(attr(x, "variance")))
  m_mat <- matrix(
    c(
      as.numeric(x),
      trt_sd,
      x + trt_sd * qnorm(0.5 - level / 2),
      x + trt_sd * qnorm(0.5 + level / 2)
    ),
    nrow = length(x)
  )
  colnames(m_mat) <- c("Estimate", "Std.Err", sprintf("%s %%", c(0.5 - level / 2, 0.5 + level / 2) * 100))
  row.names(m_mat) <- attr(x, "name")
  stats::printCoefmat(
    m_mat,
    ...
  )
}
