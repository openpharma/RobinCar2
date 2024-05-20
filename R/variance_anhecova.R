#' ANHECOVA Covariance
#'
#' @param x (`prediction_cf`) Counter-factual prediction.
#' @param decompose (`flag`) whether to use decompose method to calculate the variance.
#' @param randomization (`string`) randomization method.
#' @param ... Not used.
#'
#' @return Named covariance matrix.
#' @export
vcovAHECOVA <- function(x, decompose = TRUE, randomization = "simple", ...) {
  assert_class(x, "prediction_cf")
  assert_flag(decompose)
  assert_string(randomization)
  resi <- attr(x, "residual")
  est <- as.numeric(x)
  preds <- attr(x, "predictions")
  var_preds <- var(preds)
  y <- attr(x, "response")
  trt <- attr(x, "treatment")
  pi_t <- as.numeric(table(trt) / length(trt))
  trt_lvls <- levels(trt)
  group_idx <- attr(x, "group_idx")

  idx <- split(seq_len(length(trt)), trt)
  cov_ymu <- vapply(idx, function(is) stats::cov(y[is], preds[is, ]), FUN.VALUE = rep(0, ncol(preds)))

  if (decompose) {
    vcov_sr <- (vapply(idx, function(is) stats::var(y[is]), FUN.VALUE = 0) + diag(var_preds) - 2 * diag(cov_ymu)) / pi_t
  } else {
    vcov_sr <- vapply(idx, function(is) stats::var(resi[is]), FUN.VALUE = 0) / pi_t
  }

  v <- diag(vcov_sr) + cov_ymu + t(cov_ymu) - var_preds
  v <- v - h_get_erb(resi, group_idx, trt, pi_t, randomization)
  ret <- v / length(resi)
  dimnames(ret) <- list(trt_lvls, trt_lvls)
  return(ret)
}

#' @keywords internal
h_adjust_pi <- function(pi_t) {
  diag(pi_t) - pi_t %*% t(pi_t)
}

#' @keywords internal
h_get_erb <- function(resi, group_idx, trt, pi_t, randomization) {
  if (length(group_idx) == 1L) {
    return(0)
  }
  if (randomization %in% c("simple", "pocock-simon")) {
    return(0)
  }
  omegaz_sr <- h_adjust_pi(pi_t)
  resi_per_strata <- vapply(
    group_idx,
    function(ii) vapply(split(resi[ii], trt[ii]), mean, FUN.VALUE = 0),
    FUN.VALUE = rep(0, length(levels(trt)))
  )
  strata_props <- vapply(
    group_idx,
    length,
    FUN.VALUE = 0L
  )
  strata_props <- strata_props / sum(strata_props)
  rb_z <- resi_per_strata / as.numeric(pi_t)
  strata_levels <- length(resi_per_strata)
  n_trt <- length(pi_t)
  ind <- matrix(seq_len(strata_levels), byrow = TRUE, ncol = n_trt)
  rb_z_sum <- lapply(
    seq_len(ncol(rb_z)),
    function(x) rb_z[, x] %*% t(rb_z[, x]) * strata_props[x]
  )
  rb_z_sum <- Reduce(`+`, rb_z_sum)
  rb_z_sum * omegaz_sr
}
