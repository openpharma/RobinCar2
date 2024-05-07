#' Prediction Bias
#' @description Obtain prediction bias within each stratum.
#' @export
bias <- function(fit, treatment, data) {
  UseMethod("bias")
}

#' @export
bias.lm <- function(fit, treatment, data) {
  assert_list(
    treatment,
    types = "character"
  )
  assert_string(treatment$treatment)
  assert_character(treatment$strata, null.ok = TRUE)
  assert_data_frame(data)
  assert_subset(unlist(treatment), colnames(data))
  trt_var <- data[[treatment$treatment]]
  if (length(treatment$strata) != 0) {
    strat_var <- data[, treatment$strata]
  } else {
    strat_var <- rep(0L, nrow(data))
  }
  residuals <- residuals(fit, type = "response")

  if (is.factor(trt_var)) {
    trt_lvls <- levels(trt_var)
  } else {
    trt_lvls <- sort(unique(trt_var))
  }
  d <- matrix(NA_real_, nrow = nrow(data), ncol = length(trt_lvls))
  id_strat <- split(seq_len(nrow(data)), strat_var)
  for (i in id_strat) {
    df <- vapply(split(residuals[i], trt_var[i]), function(xx) mean(xx), FUN.VALUE = 0)
    d[i, ] <- matrix(df, nrow = length(i), ncol = length(df), byrow = TRUE)
  }
  d
}

#' @export
bias.glm <- function(fit, treatment, data = fit$data) {
  bias.lm(fit = fit, data = data, treatment = treatment)
}