#### Main functions ####

#' @exportS3Method
vcovHC.prediction_cf <- function(x, type = "HC3", ...) {
  fit <- attr(x, "fit")
  vc <- vcovHC(fit, type = type)
  mm <- attr(x, "model_matrix")
  n <- nrow(mm) / length(names(x))
  md <- family(fit)$mu.eta(attr(x, "predictions_linear")) / n
  z <- block_sum(as.vector(md) * mm, n)
  ret <- z %*% vc %*% t(z)
  dimnames(ret) <- list(names(x), names(x))
  ret
}
