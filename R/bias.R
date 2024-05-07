#' Prediction Bias
#' @description Obtain prediction bias within each stratum.
#' @param residual (`numeric`) residuals.
#' @param treatment (`string`) treatment variable.
#' @param strata (`character`) stratum variable.
#' @param data (`data.frame`) input data.
#'
#' @return Numeric matrix of bias in each stratum.
#' @export
bias <- function(residual, treatment, strata, data) {
  assert_data_frame(data)
  assert_numeric(residual, len = nrow(data))
  assert_string(treatment)
  assert_character(strata, null.ok = TRUE)
  assert_subset(treatment, colnames(data))
  assert_subset(strata, colnames(data))
  trt_var <- data[[treatment]]
  if (length(strata) != 0) {
    strat_var <- data[, strata]
  } else {
    strat_var <- rep(0L, nrow(data))
  }
  if (is.factor(trt_var)) {
    trt_lvls <- levels(trt_var)
  } else {
    trt_lvls <- sort(unique(trt_var))
  }
  d <- matrix(NA_real_, nrow = nrow(data), ncol = length(trt_lvls))
  id_strat <- split(seq_len(nrow(data)), strat_var)
  for (i in id_strat) {
    df <- vapply(split(residual[i], trt_var[i]), mean, FUN.VALUE = 0)
    d[i, ] <- matrix(df, nrow = length(i), ncol = length(df), byrow = TRUE)
  }
  d
}
