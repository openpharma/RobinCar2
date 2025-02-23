#' Heteroskedasticity-consistent covariance matrix for predictions
#' @description The heteroskedasticity-consistent covariance matrix for predictions
#' is obtained with `sandwich::vocvHC` using sandwich method.
#' @param x (`prediction_cf`) Counter-factual prediction.
#' @param type (`character`) Type of HC covariance matrix.
#' @param ... Additional arguments for `sandwich::vcovHC`.
#' @export
vcovHC <- function(x, type = "HC3", ...) {
  assert_class(x, "prediction_cf")
  fit <- attr(x, "fit")
  vc <- sandwich::vcovHC(fit, type = type)
  mm <- attr(x, "model_matrix")
  n <- nrow(mm) / length(names(x))
  md <- family(fit)$mu.eta(attr(x, "predictions_linear")) / n
  z <- block_sum(as.vector(md) * mm, n)
  ret <- z %*% vc %*% t(z)
  dimnames(ret) <- list(names(x), names(x))
  ret
}
