#' Find Data in a Fit
#' @export
#' @param fit A fit object.
#' @param ... Additional arguments.
#' @return A data frame used in the fit.
find_data <- function(fit, ...) {
  UseMethod("find_data")
}
#' @export
find_data.glm <- function(fit, ...) {
  fit$data
}
#' @export
find_data.lm <- function(fit, ...) {
  stop("data must be provided explicitly for lm objects")
}
