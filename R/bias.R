#' Prediction Bias
#' @description Obtain prediction bias within each stratum.
#' @param residual (`numeric`) residuals.
#' @param treatment (`factor`) treatment.
#' @param group_idx (`character`) stratum index.
#'
#' @return Numeric matrix of bias in each stratum.
#' @keywords internal
bias <- function(residual, treatment, group_idx) {
  assert_numeric(residual)
  assert_factor(treatment, len = length(residual))
  assert_list(group_idx, types = "integer")
  grp <- unlist(group_idx)
  assert_integer(grp, lower = 1L, upper = max(grp))

  trt_lvls <- levels(treatment)

  d <- matrix(NA_real_, nrow = length(grp), ncol = length(trt_lvls))
  for (i in group_idx) {
    mval <- vapply(split(residual[i], treatment[i]), mean, FUN.VALUE = 0)
    d[i, ] <- matrix(mval, nrow = length(i), ncol = length(mval), byrow = TRUE)
  }
  d
}
