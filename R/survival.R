#' Estimate Log Hazard Ratio via Score Function
#'
#' This function estimates the log hazard ratio by finding the root of the log-rank score function.
#'
#' @details This deactivates the ties factor correction in the score function by passing
#' `use_ties_factor = FALSE` to the `score_fun`.
#'
#' @param score_fun (`function`) The log-rank score function to be used for estimation.
#' @param interval (`numeric`) A numeric vector of length 2 specifying the interval in which to search for the root.
#' @param ... Additional arguments passed to `score_fun`.
#' @return A list containing:
#' - `theta`: The estimated log hazard ratio.
#' - `se`: The standard error of the estimated log hazard ratio.
#' - `sigma_L2`: The variance of the log-rank statistic.
#' - `n`: The number of observations used in the calculation.
#'
#' @keywords internal
h_log_hr_est_via_score <- function(score_fun, interval = c(-10, 10), ...) {
  assert_function(score_fun, args = c("theta", "use_ties_factor"))
  assert_numeric(interval, len = 2L, finite = TRUE)
  assert_true(interval[1] < interval[2])

  score_solution <- stats::uniroot(
    score_fun,
    interval = interval,
    extendInt = "yes", # If the root is not found in the interval, extend the interval.
    check.conv = TRUE, # If the root cannot be found, an error is thrown.
    use_ties_factor = FALSE,
    ...
  )
  solution_attrs <- attributes(score_solution$f.root)

  list(
    theta = score_solution$root,
    se = solution_attrs$se_theta_l,
    sigma_l2 = solution_attrs$sigma_l2,
    n = solution_attrs$n
  )
}

#' Log-Rank Test via Score Function
#'
#' This function performs a log-rank test using the score function.
#'
#' @details This activates the ties factor correction in the score function by passing
#'   `use_ties_factor = TRUE` to the `score_fun`.
#'
#' @param score_fun (`function`) The log-rank score function to be used for testing.
#' @param ... Additional arguments passed to `score_fun`.
#' @return A list containing:
#' - `u_l`: The log-rank score statistic.
#' - `sigma_l2`: The variance of the log-rank statistic.
#' - `tau_l`: The log-rank test statistic.
#' - `pval`: The two-sided p-value of the log-rank test.
#' - `n`: The number of observations used in the calculation.
#'
#' @keywords internal
h_lr_test_via_score <- function(score_fun, ...) {
  assert_function(score_fun, args = c("theta", "use_ties_factor"))

  score_res <- score_fun(theta = 0, use_ties_factor = TRUE, ...)
  u_l <- as.numeric(score_res)
  score_attrs <- attributes(score_res)

  sigma_l2 <- score_attrs$sigma_l2
  n <- score_attrs$n
  tau_l <- sqrt(n) * u_l / sqrt(sigma_l2)
  pval <- 2 * stats::pnorm(-abs(tau_l))

  list(
    u_l = u_l,
    sigma_l2 = sigma_l2,
    tau_l = tau_l,
    pval = pval,
    n = n
  )
}

#' Log Hazard Ratio Estimation and Log-Rank Test via Score Function
#'
#' This function combines the estimation of the log hazard ratio and the log-rank test
#' using a score function. Only two treatment arms are being compared and the `data` is subset accordingly.
#'
#' @details If an unadjusted score function is provided in `unadj_score_fun`, then it is used to estimate the
#' log hazard ratio first. This unadjusted log hazard ratio estimate is then passed on to the adjusted
#' score function `score_fun` as `theta_hat`. This is required when the score function is adjusted for covariates.
#'
#' @param score_fun (`function`) The log-rank score function to be used for both estimation and testing.
#' @param vars (`list`) A list containing `levels`, `treatment`, and `covariates`.
#' @param data (`data.frame`) The data frame containing the survival data.
#' @param exp_level (`count`) Level of the experimental treatment arm.
#' @param control_level (`count`) Level of the control treatment arm.
#' @param unadj_score_fun (`function` or `NULL`) Optional unadjusted score function, see details.
#' @param ... Additional arguments passed to `score_fun`.
#' @return A list containing:
#' - `estimate`: The estimated log hazard ratio.
#' - `se`: The standard error of the estimated log hazard ratio.
#' - `hr_n`: The number of observations used in the estimation.
#' - `hr_sigma_l2`: The variance of the log-rank statistic used in the estimation.
#' - `test_stat`: The log-rank test statistic.
#' - `p_value`: The two-sided p-value of the log-rank test.
#' - `test_score`: The log-rank score statistic.
#' - `test_n`: The number of observations used in the log-rank test.
#' - `test_sigma_l2`: The variance of the log-rank statistic used in the log-rank test.
#'
#' @keywords internal
robin_surv_comparison <- function(score_fun, vars, data, exp_level, control_level, unadj_score_fun = NULL, ...) {
  assert_list(vars)
  assert_names(names(vars), must.include = c("levels", "treatment", "covariates"))
  assert_character(vars$levels, min.len = 2L)
  assert_data_frame(data)
  assert_string(vars$treatment)
  assert_true(is.factor(data[[vars$treatment]]))
  assert_count(exp_level)
  assert_count(control_level)
  assert_true(exp_level != control_level)

  # Subset data to the two treatment arms of interest.
  trt_levels <- vars$levels[c(control_level, exp_level)]
  in_trt_scope <- data[[vars$treatment]] %in% trt_levels
  data <- data[in_trt_scope, , drop = FALSE]
  data[[vars$treatment]] <- droplevels(data[[vars$treatment]])
  data[[vars$treatment]] <- stats::relevel(data[[vars$treatment]], ref = trt_levels[1L])

  # Prepare arguments for the test and estimation calls below.
  args <- list(
    score_fun = score_fun,
    df = data,
    ...
  )

  # Perform the log-rank test via the score function.
  test_result <- do.call(h_lr_test_via_score, args)

  # If an unadjusted score function is provided, use it to estimate the log hazard ratio first.
  if (!is.null(unadj_score_fun)) {
    assert_function(unadj_score_fun)
    assert_true(length(vars$covariates) > 0)
    unadj_args <- args[names(args) != "model"]
    unadj_args$score_fun <- unadj_score_fun
    # Get theta_hat from the unadjusted score function.
    unadj_hr_result <- do.call(h_log_hr_est_via_score, unadj_args)
    # Add this to the arguments for the adjusted score function call below.
    args$theta_hat <- unadj_hr_result$theta
  } else {
    # We enforce to have no covariates in this case.
    assert_true(length(vars$covariates) == 0L)
  }
  # Estimate the log hazard ratio via the score function.
  hr_result <- do.call(h_log_hr_est_via_score, args)

  list(
    estimate = hr_result$theta,
    se = hr_result$se,
    hr_n = hr_result$n,
    hr_sigma_l2 = hr_result$sigma_l2,
    test_stat = test_result$tau_l,
    p_value = test_result$pval,
    test_score = test_result$u_l,
    test_n = test_result$n,
    test_sigma_l2 = test_result$sigma_l2
  )
}

#' Survival Comparison without Strata or Covariates
#'
#' This is a simple wrapper around [robin_surv_comparison()] called with the log-rank score function
#' [h_lr_score_no_strata_no_cov()] without strata or covariates.
#'
#' @inheritParams robin_surv_comparison
#' @return See [robin_surv_comparison()].
#' @keywords internal
robin_surv_no_strata_no_cov <- function(vars, data, exp_level, control_level) {
  robin_surv_comparison(
    score_fun = h_lr_score_no_strata_no_cov,
    vars = vars,
    data = data,
    exp_level = exp_level,
    control_level = control_level,
    treatment = vars$treatment,
    time = vars$time,
    status = vars$status
  )
}
