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
#' @param strata (`character`) The names of the strata variables in `df`, which must be factors.
#' @param randomization_strata (`character`) The names of the randomization strata variables in `df`.
#'   These are used to check whether the means of the covariate adjustment residuals are unbiased across
#'   these strata.
#' @param model (`formula`) The model formula for covariate adjustment, e.g., `~ cov1 + cov2`.
#' @param theta_hat (`number`) The estimated log hazard ratio when not adjusting for covariates.
#' @param n (`count`) The number of observations. Note that this can be higher than the number of rows
#'   when used in stratified analyses computations.
#' @param use_ties_factor (`flag`) Whether to use the ties factor in the variance calculation. This is used
#'   when calculating the score test statistic, but not when estimating the log hazard ratio.
#' @param calculate_variance (`flag`) Whether to calculate the variance. This is useful to avoid
#'   unnecessary computations when only the score function value is needed, e.g., during root finding.
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
  randomization_strata = character(),
  n = nrow(df),
  use_ties_factor = TRUE,
  calculate_variance = TRUE,
  check_rand_strat_warning = FALSE
) {
  assert_numeric(theta, min.len = 1L, finite = TRUE)
  assert_data_frame(df)
  assert_string(treatment)
  assert_string(time)
  assert_string(status)
  assert_factor(df[[treatment]], n.levels = 2L, any.missing = FALSE)
  assert_numeric(df[[status]], any.missing = FALSE)
  assert_true(all(df[[status]] %in% c(0, 1)))
  assert_numeric(df[[time]], lower = 0, any.missing = FALSE)
  assert_character(randomization_strata)
  assert_count(n)
  assert_true(n >= nrow(df))
  assert_flag(use_ties_factor)
  assert_flag(calculate_variance)
  assert_flag(check_rand_strat_warning)

  # Check whether to warn about insufficient inclusion of randomization strata.
  give_rand_strat_warning <- (check_rand_strat_warning && length(randomization_strata) > 0)

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
  u_l <- 0
  if (calculate_variance) {
    sigma_l2 <- 0
  }
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

    if (calculate_variance) {
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
  }
  u_l <- u_l / n
  sigma_l2 <- if (calculate_variance) sigma_l2 / n else NA_real_
  se_theta_l <- if (calculate_variance) sqrt(1 / (n * sigma_l2)) else NA_real_
  structure(
    u_l,
    sigma_l2 = sigma_l2,
    se_theta_l = se_theta_l,
    n = n,
    give_rand_strat_warning = give_rand_strat_warning
  )
}

#' @describeIn survival_score_functions with strata but without covariates.
h_lr_score_strat <- function(
  theta,
  df,
  treatment,
  time,
  status,
  strata,
  randomization_strata = character(),
  use_ties_factor = TRUE,
  calculate_variance = TRUE,
  check_rand_strat_warning = FALSE
) {
  assert_string(treatment)
  assert_string(time)
  assert_string(status)
  assert_character(strata, any.missing = FALSE, min.len = 1L, unique = TRUE)
  assert_data_frame(df)
  lapply(df[strata], assert_factor)
  lapply(df[randomization_strata], assert_factor)
  assert_flag(calculate_variance)
  assert_flag(check_rand_strat_warning)

  give_rand_strat_warning <- if (check_rand_strat_warning) {
    length(randomization_strata) > 0 &&
      !all(randomization_strata %in% strata) &&
      !h_first_fct_nested_in_second(interaction(df[strata]), interaction(df[randomization_strata]))
  } else {
    FALSE
  }

  df <- stats::na.omit(df[, c(treatment, time, status, strata)])
  n <- nrow(df)

  strata_formula <- paste("~", paste(strata, collapse = "+"))
  df_split <- split(df, f = as.formula(strata_formula), drop = TRUE)

  strata_results <- lapply(
    df_split,
    FUN = h_lr_score_no_strata_no_cov,
    theta = theta,
    treatment = treatment,
    time = time,
    status = status,
    n = n,
    randomization_strata = randomization_strata,
    use_ties_factor = use_ties_factor,
    calculate_variance = calculate_variance,
    check_rand_strat_warning = FALSE
  )

  u_sl <- sum_vectors_in_list(strata_results)
  sigma_sl2 <- if (calculate_variance) sum_vectors_in_list(lapply(strata_results, attr, "sigma_l2")) else NA_real_
  se_theta_sl <- if (calculate_variance) sqrt(1 / (n * sigma_sl2)) else NA_real_
  structure(
    u_sl,
    sigma_l2 = sigma_sl2,
    se_theta_l = se_theta_sl,
    n = n,
    give_rand_strat_warning = give_rand_strat_warning
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
  randomization_strata = character(),
  theta_hat = theta,
  use_ties_factor = TRUE,
  hr_se_plugin_adjusted = TRUE,
  calculate_variance = TRUE,
  check_rand_strat_warning = FALSE
) {
  assert_data_frame(df)
  assert_string(treatment)
  assert_string(time)
  assert_string(status)
  assert_formula(model)
  assert_character(randomization_strata)
  covariates <- all.vars(model)
  assert_subset(c(treatment, time, status, covariates), names(df))
  assert_factor(df[[treatment]], n.levels = 2L, any.missing = FALSE)
  assert_flag(hr_se_plugin_adjusted)
  assert_flag(calculate_variance)
  assert_flag(check_rand_strat_warning)

  # Subset to complete records here.
  required_cols <- unique(c(treatment, time, status, covariates, randomization_strata))
  df <- stats::na.omit(df[required_cols])
  n <- nrow(df)

  # Calculate derived outcomes and regress them on covariates based on theta_hat.
  df_with_covs_ovals <- h_derived_outcome_vals(
    theta = theta_hat,
    df = df,
    treatment = treatment,
    time = time,
    status = status,
    covariates = covariates,
    randomization_strata = randomization_strata,
    n = n
  )
  lm_input <- h_get_lm_input(df = df_with_covs_ovals, model = model)
  lm_results <- h_get_lm_results(lm_input)
  beta_est <- lm_results$beta_est

  give_rand_strat_warning <- if (check_rand_strat_warning) {
    length(randomization_strata) > 0 &&
      !h_unbiased_means_across_strata(
        residuals_per_group = lm_results$residuals,
        df = df_with_covs_ovals,
        randomization_strata = randomization_strata
      )
  } else {
    FALSE
  }

  # Obtain unadjusted result.
  unadj_score <- h_lr_score_no_strata_no_cov(
    theta = theta,
    df = df,
    treatment = treatment,
    time = time,
    status = status,
    n = n,
    randomization_strata = randomization_strata,
    use_ties_factor = use_ties_factor,
    calculate_variance = calculate_variance,
    check_rand_strat_warning = FALSE
  )

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

  if (calculate_variance) {
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
        randomization_strata = randomization_strata,
        use_ties_factor = use_ties_factor,
        calculate_variance = TRUE,
        check_rand_strat_warning = FALSE
      )
      attr(unadj_score_theta_hat, "sigma_l2")
    }

    # Compute adjustment term for the variance estimate.
    cov_x <- stats::cov(x_all)
    beta_est_sum <- beta_est[[1]] + beta_est[[2]]
    sigma_l2_adj_term <- pi * (1 - pi) * as.numeric(t(beta_est_sum) %*% cov_x %*% beta_est_sum)

    # Compute standard error for theta estimate, based on above results incl. choice for `g_theta_cl`.
    sigma_cl2 <- g_theta_cl - sigma_l2_adj_term
    var_theta_cl <- sigma_cl2 / (g_theta_cl^2) / n
    se_theta_cl <- sqrt(var_theta_cl)
  }

  structure(
    u_cl,
    se_theta_l = if (calculate_variance) se_theta_cl else NA_real_,
    sigma_l2 = if (calculate_variance) sigma_cl2 else NA_real_,
    n = n,
    give_rand_strat_warning = give_rand_strat_warning
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
  randomization_strata = character(),
  theta_hat = theta,
  use_ties_factor = TRUE,
  hr_se_plugin_adjusted = TRUE,
  calculate_variance = TRUE,
  check_rand_strat_warning = FALSE
) {
  assert_data_frame(df)
  assert_string(treatment)
  assert_string(time)
  assert_string(status)
  assert_character(strata, any.missing = FALSE, min.len = 1L, unique = TRUE)
  assert_formula(model)
  assert_character(randomization_strata)
  covariates <- all.vars(model)
  assert_subset(c(treatment, time, status, strata, covariates), names(df))
  assert_flag(hr_se_plugin_adjusted)
  assert_flag(calculate_variance)
  assert_flag(check_rand_strat_warning)

  # Subset to complete records here.
  required_cols <- unique(c(treatment, time, status, covariates, strata, randomization_strata))
  df <- stats::na.omit(df[required_cols])
  n <- nrow(df)

  # Ensure that all character covariates are converted to factors.
  # (Otherwise we could just have one character value in one stratum, or incompatible
  # design matrices between the strata.)
  for (cov in covariates) {
    if (is.character(df[[cov]])) {
      df[[cov]] <- factor(df[[cov]])
    }
  }

  # Calculate derived outcomes and regress them on covariates.
  df_with_covs_ovals_stratum <- h_strat_derived_outcome_vals(
    theta = theta_hat,
    df,
    treatment,
    time,
    status,
    strata,
    covariates = covariates,
    randomization_strata = randomization_strata
  )
  strat_lm_input <- h_get_strat_lm_input(df_with_covs_ovals_stratum, model)
  lm_results <- h_get_strat_lm_results(strat_lm_input)
  beta_est <- lm_results$beta_est

  give_rand_strat_warning <- if (check_rand_strat_warning) {
    length(randomization_strata) > 0 &&
      !h_unbiased_means_across_strata(
        residuals_per_group = lm_results$residuals,
        df = df_with_covs_ovals_stratum,
        randomization_strata = randomization_strata
      )
  } else {
    FALSE
  }

  # Obtain unadjusted results.
  strat_unadj_score <- h_lr_score_strat(
    theta,
    df,
    treatment,
    time,
    status,
    strata,
    randomization_strata,
    use_ties_factor = use_ties_factor,
    calculate_variance = calculate_variance,
    check_rand_strat_warning = FALSE
  )

  # We assume here that the observed proportion of treatment 1 in the data set
  # corresponds to the preplanned proportion of treatment 1 in the trial.
  pi <- mean(as.numeric(df[[treatment]]) - 1)

  groups <- levels(df[[treatment]])
  cont_grp <- groups[1]
  trt_grp <- groups[2]

  # Overall column wise average of design matrices, separately for each stratum.
  x_0 <- strat_lm_input[[cont_grp]]$X
  x_1 <- strat_lm_input[[trt_grp]]$X
  x_all <- rbind(x_0, x_1)

  stratum_col <- match(".stratum", colnames(x_all))
  strat_x_all <- split.data.frame(x_all[, -stratum_col, drop = FALSE], f = x_all[, stratum_col])
  strat_x_bar <- lapply(strat_x_all, colMeans)
  strat_x_bar_matrix <- do.call(rbind, strat_x_bar)
  strat_x_bar_stratum <- as.integer(names(strat_x_all))

  # Center the design matrices with this overall average.
  which_x_bar_for_x_0 <- match(unname(x_0[, stratum_col]), strat_x_bar_stratum)
  x_0 <- x_0[, -stratum_col, drop = FALSE] - strat_x_bar_matrix[which_x_bar_for_x_0, , drop = FALSE]

  which_x_bar_for_x_1 <- match(unname(x_1[, stratum_col]), strat_x_bar_stratum)
  x_1 <- x_1[, -stratum_col, drop = FALSE] - strat_x_bar_matrix[which_x_bar_for_x_1, , drop = FALSE]

  # Compute adjustment term for the stratified score.
  u_sl_adj_term <- (sum(x_1 %*% beta_est[[trt_grp]]) - sum(x_0 %*% beta_est[[cont_grp]])) / n

  # Compute adjusted covariate adjusted stratified score.
  u_csl <- as.numeric(strat_unadj_score) - u_sl_adj_term

  if (calculate_variance) {
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
        randomization_strata = randomization_strata,
        use_ties_factor = use_ties_factor,
        calculate_variance = TRUE,
        check_rand_strat_warning = FALSE
      )
      attr(strat_unadj_score_theta_hat, "sigma_l2")
    }

    # Compute standard error for theta estimate.
    var_theta_csl <- (g_theta_csl - sigma_sl2_adj_term) / (g_theta_csl^2) / n
    se_theta_csl <- sqrt(var_theta_csl)
    sigma_csl2 <- g_theta_csl - sigma_sl2_adj_term
  }

  structure(
    u_csl,
    se_theta_l = if (calculate_variance) se_theta_csl else NA_real_,
    sigma_l2 = if (calculate_variance) sigma_csl2 else NA_real_,
    n = n,
    give_rand_strat_warning = give_rand_strat_warning
  )
}
