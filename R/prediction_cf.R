#' S3 Methods for `prediction_cf`
#' @param x (`prediction_cf`)\cr the obtained counter-factual prediction object.
#' @name prediction_cf_methods
NULL

#' @describeIn prediction_cf_methods prints the prediction_cf object.
#' @exportS3Method
#' @keywords internal
print.prediction_cf <- function(x, ...) {
  cat("counter-factual prediction\n\n")
  print(x[seq_along(x)])
  cat("\n")
}
