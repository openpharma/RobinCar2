#' Generalized Covariance (ANHECOVA)
#'
#' @param x (`prediction_cf`) Counter-factual prediction.
#' @param decompose (`flag`) whether to use decompose method to calculate the variance.
#' @param ... Not used.
#'
#' @return Named covariance matrix.
#' @export
vcovG <- function(x, decompose = TRUE, ...) { # nolint
  assert_class(x, "prediction_cf")
  assert_flag(decompose)
  resi <- x$residual
  est <- x$estimate
  preds <- x$predictions
  var_preds <- var(preds)
  y <- x$response
  trt <- x$treatment
  pi_t <- as.numeric(table(trt) / length(trt))
  trt_lvls <- levels(trt)
  group_idx <- x$group_idx

  idx <- split(seq_len(length(trt)), trt)
  cov_ymu <- vapply(idx, function(is) stats::cov(y[is], preds[is, ]), FUN.VALUE = rep(0, ncol(preds)))

  if (decompose) {
    vcov_sr <- (vapply(idx, function(is) stats::var(y[is]), FUN.VALUE = 0) + diag(var_preds) - 2 * diag(cov_ymu)) / pi_t
  } else {
    vcov_sr <- vapply(idx, function(is) stats::var(resi[is]), FUN.VALUE = 0) / pi_t
  }

  v <- diag(vcov_sr) + cov_ymu + t(cov_ymu) - var_preds
  v <- v - h_get_erb(resi, group_idx, trt, pi_t, x$schema)
  ret <- v / length(resi)
  dimnames(ret) <- list(trt_lvls, trt_lvls)
  return(ret)
}

#' Obtain Adjustment for Proportion of Treatment Assignment
#' @keywords internal
#' @param pi (`numeric`) vector of proportions.
#' @return Numeric matrix.
h_adjust_pi <- function(pi) {
  assert_numeric(pi)
  diag(pi, nrow = length(pi)) - pi %*% t(pi)
}

#' Obtain Adjustment for Covariance Matrix
#' @param resi (`numeric`) vector of residuals.
#' @param group_idx (`list` of `integer`) index for each groups.
#' @param trt (`factor`) of treatment assignment.
#' @param pi (`numeric`) proportion of treatment assignment.
#' @param randomization (`string`) name of the randomization schema.
#' @keywords internal
h_get_erb <- function(resi, group_idx, trt, pi, randomization) {
  assert_list(group_idx, types = "integer")
  if (length(group_idx) == 1L) {
    return(0)
  }
  assert_string(randomization)
  if (randomization %in% c("sp", "ps")) {
    return(0)
  }
  assert_numeric(resi)
  assert_factor(trt)
  assert_numeric(pi)

  omegaz_sr <- h_adjust_pi(pi)
  n_tot <- length(resi)
  resi_per_strata <- vapply(
    group_idx,
    function(ii) {
      v <- vapply(split(resi[ii], trt[ii]), mean, FUN.VALUE = 0)
      v * sqrt(length(ii) / n_tot)
    },
    FUN.VALUE = rep(0, length(levels(trt)))
  )
  rb_z <- resi_per_strata / as.numeric(pi)
  tcrossprod(rb_z) * omegaz_sr
}
