#' Obtain treatment effect
#' @export
treatment_effect <- function(x, ...) {
  UseMethod("treatment_effect")
}

#' @export
treatment_effect.std_lm <- function(x, trt, ref, vcov_method = "HC3", ...) {
  if (missing(ref)) {
    ref <- x$ref_level
  }
  if (missing(trt)) {
    trt <- x$trt_levels[1]
  }
  assert_subset(c(trt, ref), c(x$trt_levels, x$ref_level))
  coefs <- coef(x$fit)
  cov <- sandwich::vcovHC(x$fit, vcov_method)
  sel_vec <- h_obtain_sel(paste0(x$trt, c(trt, ref)), names(coefs))
  ret <- list(
    coef = sum(sel_vec * coefs),
    robust_var = sel_vec %*% cov %*% sel_vec
  )
  return(ret)
}

treatment_effect.std_glm <- function(x, level1, level2, statistic = c("rr", "or"), ...) {
  stop("Not implemented")
}
