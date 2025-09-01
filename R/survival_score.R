#' Log-Rank Score Functions for Survival Analysis
#'
#' These functions compute the log-rank score statistics for a survival analysis.
#' Depending on the function, these are stratified and/or adjusted for covariates.
#'
#' @param theta (`number`) The assumed log hazard ratio of the second vs. the first level of the
#'   treatment arm variable.
#' @param df (`data.frame`) The data frame containing the survival data.
#' @param treatment (`string`) The name of the treatment arm variable in `df`. It should be a factor with
#'   two levels, where the first level is the reference group.
#' @param time (`string`) The name of the time variable in `df`, representing the survival time.
#' @param status (`string`) The name of the status variable in `df`, with 0 for censored and 1 for event.
#' @param strata (`string`) The name of the strata variable in `df`, which must be a factor.
#' @param model (`formula`) The model formula for covariate adjustment, e.g., `~ cov1 + cov2`.
#' @param theta_hat (`number`) The estimated log hazard ratio when not adjusting for covariates.
#' @param n (`count`) The number of observations. Note that this can be higher than the number of rows
#'   when used in stratified analyses computations.
#' @param use_ties_factor (`flag`) Whether to use the ties factor in the variance calculation. This is used
#'   when calculating the score test statistic, but not when estimating the log hazard ratio.
#' @param hr_se_plugin_adjusted (`flag`) Defines the method for calculating the standard error of the
#'   log hazard ratio estimate when adjusting for covariates, see details.
#' @return The score function value(s), with the following attributes:
#'   - `sigma_l2`: The variance of the log-rank statistic.
#'   - `se_theta_l`: The corresponding standard error term for the log hazard ratio.
#'   - `n`: The number of observations used in the calculation.
#'
#' @details
#'   - The `hr_se_plugin_adjusted` flag is relevant only for the standard error of the covariate adjusted
#'   log hazard ratio estimate: When `TRUE`, the adjusted hazard ratio estimate
#'   is plugged in into the variance formula, as per the original publication.
#'   On the other hand, when `FALSE`, the unadjusted estimate is used instead.  This is explained in more
#'   detail in the vignette "Survival Analysis with RobinCar2" in Section "Covariate adjusted analysis without strata".
#'
#'   - Note that for the not covariate adjusted score functions, these also work
#'   with a `numeric` `theta` vector of length > 1.
#' @name survival_score_functions
#' @keywords internal
NULL

#' @describeIn survival_score_functions without strata or covariates.
h_lr_score_no_strata_no_cov <- function(
    theta,
    df,
    treatment,
    time,
    status,
    n = nrow(df),
    use_ties_factor = TRUE) {
  assert_numeric(theta, min.len = 1L, finite = TRUE)
  assert_data_frame(df)
  assert_string(treatment)
  assert_string(time)
  assert_string(status)
  assert_factor(df[[treatment]], n.levels = 2L, any.missing = FALSE)
  assert_numeric(df[[status]], any.missing = FALSE)
  assert_true(all(df[[status]] %in% c(0, 1)))
  assert_numeric(df[[time]], lower = 0, any.missing = FALSE)
  assert_count(n)
  assert_true(n >= nrow(df))
  assert_flag(use_ties_factor)

  # Standardize data set format, subset to relevant variables.
  df_stand <- data.frame(
    treatment_numeric = as.numeric(df[[treatment]]) - 1L,
    time = df[[time]],
    status = df[[status]]
  )

  # Sort by time.
  df_stand <- df_stand[order(df_stand$time), , drop = FALSE]

  # Patients with events.
  df_events <- df_stand[df_stand$status == 1L, , drop = FALSE]

  # Add number of events per unique event time.
  df_events_per_time <- h_n_events_per_time(df_stand, time = "time", status = "status")
  df_events$n_events <- df_events_per_time$n_events[match(df_events$time, df_events_per_time$time)]

  # Calculate the log rank statistic u_l and the variance sigma_l2 iteratively.
  u_l <- sigma_l2 <- 0
  for (i in seq_len(nrow(df_events))) {
    # This event time.
    t_i <- df_events$time[i]

    # Number of overall events at this time.
    n_events_ti <- df_events$n_events[i]

    # This treatment arm indicator.
    trt_i <- df_events$treatment_numeric[i]

    # Proportions of patients at risk at time t_i, per arm.

    # Note: Also here we need to use sum divided by n, otherwise
    # we will get wrong results when there are ties.
    y_bar_1_ti <- sum(df_stand$treatment_numeric & df_stand$time >= t_i) / n
    y_bar_0_ti <- sum(!df_stand$treatment_numeric & df_stand$time >= t_i) / n

    # Adjusted proportion of patients at risk in the treatment arm:
    y_bar_1_ti_coxph <- y_bar_1_ti * exp(theta)

    # Increment u_l.
    u_l <- u_l + (trt_i - y_bar_1_ti_coxph / (y_bar_1_ti_coxph + y_bar_0_ti))

    # Increment sigma_l2.
    y_bar_all_ti <- y_bar_1_ti_coxph + y_bar_0_ti

    sigma_l2_factor_ti <- if (!use_ties_factor || n_events_ti == 1) {
      1
    } else {
      (n * y_bar_all_ti - n_events_ti) / (n * y_bar_all_ti - 1)
    }
    sigma_l2_summand <- y_bar_1_ti_coxph * y_bar_0_ti * sigma_l2_factor_ti / y_bar_all_ti^2
    sigma_l2 <- sigma_l2 + sigma_l2_summand
  }
  u_l <- u_l / n
  sigma_l2 <- sigma_l2 / n
  se_theta_l <- sqrt(1 / (n * sigma_l2))
  structure(
    u_l,
    sigma_l2 = sigma_l2,
    se_theta_l = se_theta_l,
    n = n
  )
}

#' @describeIn survival_score_functions with strata but without covariates.
h_lr_score_strat <- function(theta, df, treatment, time, status, strata, use_ties_factor = TRUE) {
  assert_string(treatment)
  assert_string(time)
  assert_string(status)
  assert_string(strata)
  assert_data_frame(df)
  assert_factor(df[[strata]])

  df <- stats::na.omit(df[, c(treatment, time, status, strata)])
  n <- nrow(df)

  df[[strata]] <- droplevels(df[[strata]])
  strata_levels <- levels(df[[strata]])

  df_split <- split(df, f = df[[strata]])
  strata_results <- lapply(
    df_split,
    FUN = h_lr_score_no_strata_no_cov,
    theta = theta,
    treatment = treatment,
    time = time,
    status = status,
    n = n,
    use_ties_factor = use_ties_factor
  )

  u_sl <- sum_vectors_in_list(strata_results)
  sigma_sl2 <- sum_vectors_in_list(lapply(strata_results, attr, "sigma_l2"))
  se_theta_sl <- sqrt(1 / (n * sigma_sl2))
  structure(
    u_sl,
    sigma_l2 = sigma_sl2,
    se_theta_l = se_theta_sl,
    n = n
  )
}

#' @describeIn survival_score_functions with covariates but without strata.
h_lr_score_cov <- function(
    theta,
    df,
    treatment,
    time,
    status,
    model,
    theta_hat = theta,
    use_ties_factor = TRUE,
    hr_se_plugin_adjusted = TRUE) {
  assert_data_frame(df)
  assert_string(treatment)
  assert_string(time)
  assert_string(status)
  assert_formula(model)
  covariates <- all.vars(model)
  assert_subset(c(treatment, time, status, covariates), names(df))
  assert_factor(df[[treatment]], n.levels = 2L, any.missing = FALSE)
  assert_flag(hr_se_plugin_adjusted)

  # Subset to complete records here.
  df <- stats::na.omit(df[c(treatment, time, status, covariates)])
  n <- nrow(df)

  # Calculate derived outcomes and regress them on covariates based on theta_hat.
  df_with_covs_ovals <- h_derived_outcome_vals(
    theta = theta_hat,
    df = df,
    treatment = treatment,
    time = time,
    status = status,
    covariates = covariates,
    n = n
  )
  lm_input <- h_get_lm_input(df = df_with_covs_ovals, model = model)
  beta_est <- h_get_beta_estimates(lm_input)

  # Obtain unadjusted result.
  unadj_score <- h_lr_score_no_strata_no_cov(
    theta = theta,
    df = df,
    treatment = treatment,
    time = time,
    status = status,
    n = n,
    use_ties_factor = use_ties_factor
  )

  # Define standard error calculation.
  g_theta_cl <- if (hr_se_plugin_adjusted) {
    attr(unadj_score, "sigma_l2")
  } else {
    unadj_score_theta_hat <- h_lr_score_no_strata_no_cov(
      theta = theta_hat, # Here is the only difference.
      df = df,
      treatment = treatment,
      time = time,
      status = status,
      n = n,
      use_ties_factor = use_ties_factor
    )
    attr(unadj_score_theta_hat, "sigma_l2")
  }

  # We assume here that the observed proportion of treatment 1 in the data set corresponds to the preplanned
  # proportion of treatment 1 in the trial.
  pi <- mean(as.numeric(df[[treatment]]) - 1)

  # Overall column wise average of design matrices.
  x_all <- rbind(lm_input[[1]]$X, lm_input[[2]]$X)
  x_bar <- colMeans(x_all)

  # Center the design matrices with this overall average.
  x_0 <- scale(lm_input[[1]]$X, center = x_bar, scale = FALSE)
  x_1 <- scale(lm_input[[2]]$X, center = x_bar, scale = FALSE)

  # Compute adjustment term for the score.
  u_l_adj_term <- (sum(x_1 %*% beta_est[[2]]) - sum(x_0 %*% beta_est[[1]])) / n

  # Compute adjusted score statistic.
  u_cl <- as.numeric(unadj_score) - u_l_adj_term

  # Compute adjustment term for the variance estimate.
  cov_x <- stats::cov(x_all)
  beta_est_sum <- beta_est[[1]] + beta_est[[2]]
  sigma_l2_adj_term <- pi * (1 - pi) * as.numeric(t(beta_est_sum) %*% cov_x %*% beta_est_sum)

  # Compute standard error for theta estimate, based on above results incl. choice for `g_theta_cl`.
  sigma_cl2 <- g_theta_cl - sigma_l2_adj_term
  var_theta_cl <- sigma_cl2 / (g_theta_cl^2) / n
  se_theta_cl <- sqrt(var_theta_cl)

  structure(
    u_cl,
    se_theta_l = se_theta_cl,
    sigma_l2 = sigma_cl2,
    n = n
  )
}

#' @describeIn survival_score_functions with strata and covariates.
h_lr_score_strat_cov <- function(
    theta,
    df,
    treatment,
    time,
    status,
    strata,
    model,
    theta_hat = theta,
    use_ties_factor = TRUE,
    hr_se_plugin_adjusted = TRUE) {
  assert_data_frame(df)
  assert_string(treatment)
  assert_string(time)
  assert_string(status)
  assert_string(strata)
  assert_formula(model)
  covariates <- all.vars(model)
  assert_subset(c(treatment, time, status, strata, covariates), names(df))
  assert_flag(hr_se_plugin_adjusted)

  # Subset to complete records here.
  df <- stats::na.omit(df[c(treatment, time, status, strata, covariates)])
  n <- nrow(df)

  # Calculate derived outcomes and regress them on covariates.
  df_split_with_covs_ovals <- h_strat_derived_outcome_vals(
    theta = theta_hat,
    df,
    treatment,
    time,
    status,
    strata,
    covariates = covariates
  )
  strat_lm_input <- h_get_strat_lm_input(df_split_with_covs_ovals, model)
  beta_est <- h_get_strat_beta_estimates(strat_lm_input)

  # Obtain unadjusted results.
  strat_unadj_score <- h_lr_score_strat(
    theta,
    df,
    treatment,
    time,
    status,
    strata,
    use_ties_factor = use_ties_factor
  )

  # We assume here that the observed proportion of treatment 1 in the data set
  # corresponds to the preplanned proportion of treatment 1 in the trial.
  pi <- mean(as.numeric(df[[treatment]]) - 1)

  groups <- levels(df[[treatment]])
  cont_grp <- groups[1]
  trt_grp <- groups[2]

  # Overall column wise average of design matrices, separately for each stratum.
  strat_x_all <- lapply(strat_lm_input, function(l) rbind(l[[cont_grp]]$X, l[[trt_grp]]$X))
  strat_x_bar <- lapply(strat_x_all, colMeans)

  # Center the design matrices with this overall average.
  has_x_0 <- names(which(sapply(strat_lm_input, function(l) cont_grp %in% names(l))))
  has_x_1 <- names(which(sapply(strat_lm_input, function(l) trt_grp %in% names(l))))

  x_0 <- lapply(has_x_0, function(n) scale(strat_lm_input[[n]][[cont_grp]]$X, center = strat_x_bar[[n]], scale = FALSE))
  x_1 <- lapply(has_x_1, function(n) scale(strat_lm_input[[n]][[trt_grp]]$X, center = strat_x_bar[[n]], scale = FALSE))

  x_0 <- do.call(rbind, x_0)
  x_1 <- do.call(rbind, x_1)

  # Compute adjustment term for the stratified score.
  u_sl_adj_term <- (sum(x_1 %*% beta_est[[trt_grp]]) - sum(x_0 %*% beta_est[[cont_grp]])) / n

  # Compute adjusted covariate adjusted stratified score.
  u_csl <- as.numeric(strat_unadj_score) - u_sl_adj_term

  # Compute adjustment term for sigma_sl2.
  strat_n <- sapply(strat_x_all, nrow)
  strat_use <- names(which(strat_n > 1))
  strat_n <- strat_n[strat_use]
  overall_n <- sum(strat_n)
  strat_cov_x <- lapply(strat_x_all[strat_use], stats::cov)
  weighted_cov_x <- Map(function(x, n) x * n / overall_n, strat_cov_x, strat_n)
  weighted_sum_cov_x <- Reduce("+", weighted_cov_x)

  beta_est_sum <- beta_est[[cont_grp]] + beta_est[[trt_grp]]
  sigma_sl2_adj_term <- pi * (1 - pi) * as.numeric(t(beta_est_sum) %*% weighted_sum_cov_x %*% beta_est_sum)

  # Define standard error calculation.
  g_theta_csl <- if (hr_se_plugin_adjusted) {
    attr(strat_unadj_score, "sigma_l2")
  } else {
    strat_unadj_score_theta_hat <- h_lr_score_strat(
      theta = theta_hat, # Here is the only difference.
      df = df,
      treatment = treatment,
      time = time,
      status = status,
      strata = strata,
      use_ties_factor = use_ties_factor
    )
    attr(strat_unadj_score_theta_hat, "sigma_l2")
  }

  # Compute standard error for theta estimate.
  var_theta_csl <- (g_theta_csl - sigma_sl2_adj_term) / (g_theta_csl^2) / n
  se_theta_csl <- sqrt(var_theta_csl)

  structure(
    u_csl,
    se_theta_l = se_theta_csl,
    sigma_l2 = g_theta_csl - sigma_sl2_adj_term,
    n = n
  )
}
