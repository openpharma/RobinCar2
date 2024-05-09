#' @exportS3Method
vcovHC.prediction_cf <- function(x, type = "HC3",...) {
  fit <- attr(x, "fit")
  vc <- vcovHC(fit, type = type)
  mm <- attr(x, "model_matrix")
  md <- family(fit)$mu.eta(attr(x, "predictions_linear")) / nrow(mm)
  z <- block_sum(as.vector(md) * mm, length(md) / length(names(x)))
  ret <- z %*% vc %*% t(z)
  dimnames(ret) <- list(names(x), names(x))
  ret
}
