#' Prediction Bias
#' @description Obtain prediction bias within each stratum.
#' @param residual (`numeric`) residuals.
#' @param treatment (`factor`) treatment.
#' @param group_idx (`list` of `integer`) indices for each stratum group.
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


#' Check Unbiased Means of Residuals Across Randomization Strata and Treatment Groups
#'
#' This function checks whether the means of residuals are approximately zero
#' across specified randomization strata for each of the two treatment groups.
#' It is used in [robin_surv()] downstream functions to verify the correct inclusion of randomization strata
#' variables in the analysis survival model.
#'
#' @param residuals_per_group (`list` of `numeric`) A named list of numeric vectors containing residuals for each
#'   of the two treatment groups.
#' @param df (`data.frame`) The data frame containing the `treatment` and randomization strata variables, produced by
#'   [h_derived_outcome_vals()] or [h_strat_derived_outcome_vals()].
#' @param randomization_strata (`character`) A character vector of names of the randomization strata variables
#'   in `df`.
#' @param eps (`numeric`) A small tolerance value to determine if means are close to zero.
#'
#' @return `TRUE` if the means of residuals across randomization strata are within the specified tolerance
#'   for both treatment groups, `FALSE` otherwise.
#'
#' @seealso [bias()] for the underlying bias (means of residuals) calculation.
#'
#' @keywords internal
h_unbiased_means_across_strata <- function(
  residuals_per_group,
  df,
  randomization_strata,
  eps = sqrt(.Machine$double.eps)
) {
  assert_list(residuals_per_group, types = "numeric", len = 2L)
  assert_data_frame(df)
  assert_character(randomization_strata, min.len = 1L, unique = TRUE)
  assert_disjunct("treatment", randomization_strata)
  assert_names(names(df), must.include = c("treatment", randomization_strata))

  residuals_overall <- numeric(nrow(df))
  for (group in names(residuals_per_group)) {
    group_indices <- which(df$treatment == group)
    residuals_overall[group_indices] <- residuals_per_group[[group]]
  }
  group_idx <- split(
    seq_len(nrow(df)),
    f = df[randomization_strata],
    drop = TRUE
  )
  bias_vals <- bias(
    residual = residuals_overall,
    treatment = df$treatment,
    group_idx = group_idx
  )
  all(
    abs(bias_vals) < eps,
    na.rm = TRUE # Because some strata may not have both treatment groups.
  )
}
