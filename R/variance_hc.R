#' Heteroskedasticity-consistent covariance matrix for predictions
#' @description The heteroskedasticity-consistent covariance matrix for predictions
#' is obtained with `sandwich::vocvHC` using sandwich method.
#' @param x (`prediction_cf`) Counter-factual prediction.
#' @param type (`character`) Type of HC covariance matrix.
#' @param ... Additional arguments for `sandwich::vcovHC`.
#' @export
#' @return Matrix of the heteroskedasticity-consistent covariance for the predictions.
vcovHC <- function(x, type = "HC3", ...) { # nolint
  assert_class(x, "prediction_cf")
  fit <- x$fit
  vc <- sandwich::vcovHC(fit, type = type)
  mm <- x$model_matrix
  n <- nrow(mm) / length(x$estimate)
  md <- family(fit)$mu.eta(x$predictions_linear) / n
  z <- block_sum(as.vector(md) * mm, n)
  ret <- z %*% vc %*% t(z)
  dimnames(ret) <- list(names(x$estimate), names(x$estimate))
  ret
}
