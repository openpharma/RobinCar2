#' Estimate Log Hazard Ratio via Score Function
#'
#' This function estimates the log hazard ratio by finding the root of the log-rank score function.
#'
#' @details This deactivates the ties factor correction in the score function by passing
#' `use_ties_factor = FALSE` to the `score_fun`. The root finding is done without calculating
#' the variance by passing `calculate_variance = FALSE`, which is only calculated at the solution.
#' This saves computation time, and avoids spurious warnings about negative variances during the root
#' finding process.
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
h_log_hr_est_via_score <- function(score_fun, interval = c(-5, 5), ...) {
  assert_function(score_fun, args = c("theta", "use_ties_factor", "calculate_variance"))
  assert_numeric(interval, len = 2L, finite = TRUE)
  assert_true(interval[1] < interval[2])

  score_solution <- stats::uniroot(
    score_fun,
    interval = interval,
    extendInt = "yes", # If the root is not found in the interval, extend the interval.
    check.conv = TRUE, # If the root cannot be found, an error is thrown.
    use_ties_factor = FALSE,
    calculate_variance = FALSE, # We will only do this at the solution.
    tol = .Machine$double.eps^0.1, # Use a small tolerance for convergence.
    ...
  )
  score_root <- score_solution$root
  # Now calculate the variance at the solution.
  score_result <- score_fun(
    theta = score_root,
    use_ties_factor = FALSE,
    calculate_variance = TRUE,
    ...
  )
  solution_attrs <- attributes(score_result)

  list(
    theta = score_root,
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
  assert_function(score_fun, args = c("theta", "use_ties_factor", "calculate_variance"))

  score_res <- score_fun(theta = 0, use_ties_factor = TRUE, calculate_variance = TRUE, ...)
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
    n = n,
    give_rand_strat_warning = score_attrs$give_rand_strat_warning
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
robin_surv_comparison <- function(
  score_fun,
  vars,
  data,
  exp_level,
  control_level,
  contrast,
  unadj_score_fun = NULL,
  ...
) {
  assert_list(vars)
  assert_names(names(vars), must.include = c("levels", "treatment", "covariates"))
  assert_character(vars$levels, min.len = 2L)
  assert_data_frame(data)
  assert_string(vars$treatment)
  assert_true(is.factor(data[[vars$treatment]]))
  assert_count(exp_level)
  assert_count(control_level)
  assert_true(exp_level != control_level)
  assert_string(contrast)

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

  # Estimate the log hazard ratio via the score function, if requested.
  hr_result <- if (contrast == "hazardratio") {
    # If an unadjusted score function is provided, use it to estimate the log hazard ratio first.
    if (!is.null(unadj_score_fun)) {
      assert_function(unadj_score_fun)
      assert_true(length(vars$covariates) > 0)
      args_to_drop <- c("model", "hr_se_plugin_adjusted", "check_rand_strat_warning")
      unadj_args <- args[!(names(args) %in% args_to_drop)]
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
    do.call(h_log_hr_est_via_score, args)
  } else {
    list(
      theta = NA_real_,
      se = NA_real_,
      n = NA_integer_,
      sigma_l2 = NA_real_
    )
  }

  list(
    estimate = hr_result$theta,
    se = hr_result$se,
    hr_n = hr_result$n,
    hr_sigma_l2 = hr_result$sigma_l2,
    test_stat = test_result$tau_l,
    p_value = test_result$pval,
    test_score = test_result$u_l,
    test_n = test_result$n,
    test_sigma_l2 = test_result$sigma_l2,
    give_rand_strat_warning = test_result$give_rand_strat_warning
  )
}

#' Survival Comparison Functions
#'
#' These are simple wrappers around [robin_surv_comparison()] called with the corresponding log-rank score
#' functions.
#'
#' @inheritParams robin_surv_comparison
#' @return See [robin_surv_comparison()].
#'
#' @name survival_comparison_functions
#' @keywords internal
NULL

#' @describeIn survival_comparison_functions without strata and without covariates, based on
#'   [h_lr_score_no_strata_no_cov()].
robin_surv_no_strata_no_cov <- function(
  vars,
  data,
  exp_level,
  control_level,
  contrast,
  check_rand_strat_warning = FALSE
) {
  robin_surv_comparison(
    score_fun = h_lr_score_no_strata_no_cov,
    vars = vars,
    data = data,
    exp_level = exp_level,
    control_level = control_level,
    contrast = contrast,
    treatment = vars$treatment,
    time = vars$time,
    status = vars$status,
    randomization_strata = vars$randomization_strata,
    check_rand_strat_warning = check_rand_strat_warning
  )
}

#' @describeIn survival_comparison_functions without strata and without covariates, based on
#'   [h_lr_score_strat()].
robin_surv_strata <- function(
  vars,
  data,
  exp_level,
  control_level,
  contrast,
  check_rand_strat_warning = FALSE
) {
  robin_surv_comparison(
    score_fun = h_lr_score_strat,
    vars = vars,
    data = data,
    exp_level = exp_level,
    control_level = control_level,
    contrast = contrast,
    treatment = vars$treatment,
    time = vars$time,
    status = vars$status,
    strata = vars$strata,
    randomization_strata = vars$randomization_strata,
    check_rand_strat_warning = check_rand_strat_warning
  )
}

#' @describeIn survival_comparison_functions without strata and without covariates, based on
#'   [h_lr_score_cov()] and [h_lr_score_no_strata_no_cov()] (which is used to find the unadjusted
#'   log hazard ratio estimate).
robin_surv_cov <- function(vars, data, exp_level, control_level, contrast, ...) {
  robin_surv_comparison(
    score_fun = h_lr_score_cov,
    unadj_score_fun = h_lr_score_no_strata_no_cov,
    vars = vars,
    data = data,
    exp_level = exp_level,
    control_level = control_level,
    contrast = contrast,
    treatment = vars$treatment,
    time = vars$time,
    status = vars$status,
    model = vars$model,
    randomization_strata = vars$randomization_strata,
    ...
  )
}

#' @describeIn survival_comparison_functions with strata and covariates, based on
#'   [h_lr_score_strat_cov()] and [h_lr_score_strat()] (which is used to find the unadjusted
#'   log hazard ratio estimate).
robin_surv_strata_cov <- function(vars, data, exp_level, control_level, contrast, ...) {
  robin_surv_comparison(
    score_fun = h_lr_score_strat_cov,
    unadj_score_fun = h_lr_score_strat,
    vars = vars,
    data = data,
    exp_level = exp_level,
    control_level = control_level,
    contrast = contrast,
    treatment = vars$treatment,
    time = vars$time,
    status = vars$status,
    strata = vars$strata,
    model = vars$model,
    randomization_strata = vars$randomization_strata,
    ...
  )
}

#' Log Hazard Ratio Coefficient Matrix
#'
#' This function creates a coefficient matrix for the log hazard ratio estimates.
#'
#' @param x (`list`) A list containing the log hazard ratio estimates and their standard errors.
#' @return A matrix with columns for the log hazard ratio estimate, standard error, z-value,
#'   and p-value.
#'
#' @keywords internal
h_log_hr_coef_mat <- function(x) {
  assert_list(x, names = "unique")
  assert_names(names(x), must.include = c("estimate", "se", "pair"))
  assert_numeric(x$estimate, finite = TRUE)
  assert_numeric(x$se, finite = TRUE, len = length(x$estimate), lower = .Machine$double.eps)
  assert_list(x$pair, types = "integer", len = 2L)
  assert_character(attr(x$pair, "levels"), min.len = max(unlist(x$pair)))
  assert_true(length(x$pair[[1]]) == length(x$pair[[2]]))
  assert_true(length(x$pair[[1]]) == length(x$se))

  z_value <- x$estimate / x$se
  p_value <- 2 * pnorm(-abs(z_value))
  ret <- matrix(
    c(
      x$estimate,
      x$se,
      z_value,
      p_value
    ),
    nrow = length(x$estimate)
  )
  colnames(ret) <- c("Estimate", "Std.Err", "Z Value", "Pr(>|z|)")
  pair <- x$pair
  row.names(ret) <- sprintf("%s v.s. %s", attr(pair, "levels")[pair[[1]]], attr(pair, "levels")[pair[[2]]])
  ret
}

#' Log-Rank Test Results Matrix
#'
#' This function creates a matrix summarizing the results of the log-rank test.
#'
#' @param x (`list`) A list containing the log-rank test results.
#' @return A matrix with columns for the test statistic and p-value.
#'
#' @keywords internal
h_test_mat <- function(x) {
  assert_list(x, names = "unique")
  assert_names(names(x), must.include = c("test_stat", "p_value", "pair"))
  assert_numeric(x$test_stat, finite = TRUE)
  assert_numeric(x$p_value, finite = TRUE, len = length(x$test_stat))
  assert_list(x$pair, types = "integer", len = 2L)
  assert_character(attr(x$pair, "levels"), min.len = max(unlist(x$pair)))
  assert_true(length(x$pair[[1]]) == length(x$pair[[2]]))
  assert_true(length(x$pair[[1]]) == length(x$test_stat))

  ret <- matrix(
    c(
      x$test_stat,
      x$p_value
    ),
    nrow = length(x$test_stat)
  )
  colnames(ret) <- c("Test Stat.", "Pr(>|z|)")
  pair <- x$pair
  row.names(ret) <- sprintf("%s v.s. %s", attr(pair, "levels")[pair[[1]]], attr(pair, "levels")[pair[[2]]])
  ret
}

#' Prepare Events Table
#'
#' This function creates a data frame summarizing the number of patients and events
#' for each treatment arm and stratification factor.
#'
#' @param data (`data.frame`) The data frame containing the survival data.
#' @param vars (`list`) A list containing the treatment, time, status, and strata variables.
#' @return A data frame with columns for the treatment, strata, number of patients, and number of events.
#'
#' @keywords internal
h_events_table <- function(data, vars) {
  assert_data_frame(data, col.names = "unique")
  assert_subset(with(vars, c(treatment, time, status, strata)), names(data))

  agg_res <- stats::aggregate(
    by = data[c(vars$treatment, vars$strata)], # This order leads to the expected sorting.
    x = data[vars$status],
    FUN = function(x) (c(Patients = length(x), Events = as.integer(sum(x)))),
    drop = TRUE
  )
  cbind(
    agg_res[c(vars$strata, vars$treatment)],
    agg_res[[vars$status]]
  )
}

#' Covariate Adjusted and Stratified Survival Analysis
#'
#' Calculate log-rank test as well as hazard ratio estimates for survival data, optionally adjusted
#' for covariates and a stratification factor.
#'
#' @param formula (`formula`) A formula of analysis, of the form
#'   `Surv(time, status) ~ covariates + strata(x, y, z)`.
#'   If no covariates should be adjusted for, use `1` instead on the right hand side. The intercept must not be removed.
#'   If no stratification factors should be used for the analysis, do not use `strata()` in the formula.
#' @param data (`data.frame`) Input data frame.
#' @param treatment (`formula`) A formula of treatment assignment or assignment by stratification, of the form
#'   `treatment ~ scheme(vars)`. Note that currently the randomization scheme is not used in the analysis. However,
#'   any variables that were used in the randomization scheme must be included in the model formula,
#'   either as covariates, or as `strata()`.
#' @param comparisons (`list`) An optional list of comparisons between treatment levels to be performed,
#'   see details. By default, all pairwise comparisons are performed automatically.
#' @param contrast (`character(1)`) The contrast statistic to be used, currently only `"hazardratio"`
#'   is supported. Can be disabled by specifying `"none"`, in which case only the log-rank test is performed.
#' @param test (`character(1)`) The test to be used, currently only `"logrank"` is supported.
#' @param ... Additional arguments passed to the survival analysis functions, in particular `hr_se_plugin_adjusted`
#'   (please see [here][survival_score_functions] for details).
#' @return A `surv_effect` object containing the results of the survival analysis.
#' @seealso [surv_effect_methods] for S3 methods.
#'
#' @details
#' The user can optionally specify a list of comparisons between treatment levels to be performed.
#' The list must have two elements:
#'
#' - Treatment level indices of the treatment group.
#' - Treatment level indices of the control group.
#'
#' So for example if you would like to compare level 3 with level 1, and also level 3 with level 2
#' (but not level 2 with level 1) then you can specify:
#' `comparisons = list(c(3, 3), c(1, 2))`
#'
#' @export
#'
#' @examples
#' # Adjusted for covariates meal.cal and age and adjusted for stratification by strata:
#' robin_surv(
#'   formula = Surv(time, status) ~ meal.cal + age + strata(strata),
#'   data = surv_data,
#'   treatment = sex ~ pb(strata)
#' )
#'
#' # Adjusted for stratification by strata and ecog but not for covariates:
#' robin_surv(
#'   formula = Surv(time, status) ~ 1 + strata(strata, ecog),
#'   data = surv_data,
#'   treatment = sex ~ sr(1)
#' )
#'
#' # Unadjusted for covariates and stratification:
#' robin_surv(
#'   formula = Surv(time, status) ~ 1,
#'   data = surv_data,
#'   treatment = sex ~ sr(1)
#' )
robin_surv <- function(
  formula,
  data,
  treatment,
  comparisons,
  contrast = c("hazardratio", "none"),
  test = "logrank",
  ...
) {
  attr(formula, ".Environment") <- environment()
  assert_formula(formula)
  assert_data_frame(data)
  assert_formula(treatment)
  assert_subset(all.vars(formula), names(data))
  assert_subset(all.vars(treatment), names(data))
  contrast <- match.arg(contrast)
  test <- match.arg(test)

  input <- h_prep_survival_input(formula, data, treatment)

  # Subset to complete records here, so that we can use this for the strata/events tabulation.
  data_columns_needed <- unique(c(
    input$treatment,
    input$time,
    input$status,
    input$strata,
    input$covariates,
    input$randomization_strata
  ))
  data <- stats::na.omit(input$data[data_columns_needed])
  events_table <- h_events_table(data, input)

  has_strata <- length(input$strata) > 0
  has_covariates <- length(input$covariates) > 0
  calc_function <- if (has_strata && has_covariates) {
    robin_surv_strata_cov
  } else if (has_strata) {
    robin_surv_strata
  } else if (has_covariates) {
    robin_surv_cov
  } else {
    robin_surv_no_strata_no_cov
  }

  if (missing(comparisons)) {
    comparisons <- pairwise(input$levels)
  } else {
    # Convert to integer and assign levels attribute to user defined list for user convenience.
    assert_list(comparisons, types = "integerish")
    comparisons <- lapply(comparisons, as.integer)
    attr(comparisons, "levels") <- input$levels
  }
  assert_list(comparisons, len = 2L, types = "integer")
  n_comparisons <- length(comparisons[[1]])
  assert_integer(comparisons[[1]], lower = 1L, upper = length(input$levels))
  assert_integer(comparisons[[2]], lower = 1L, upper = length(input$levels))

  # Variable to keep track whether a warning about insufficient inclusion of
  # randomization strata in the analysis model has already been required in a comparison.
  # We want to avoid checking for the need of a warning, or giving a warning,
  # multiple times.
  give_rand_strat_warning <- FALSE

  estimates <- lapply(
    seq_len(n_comparisons),
    function(i) {
      exp_level <- comparisons[[1]][i]
      control_level <- comparisons[[2]][i]
      result <- calc_function(
        vars = input,
        data = data,
        exp_level = exp_level,
        control_level = control_level,
        contrast = contrast,
        check_rand_strat_warning = !give_rand_strat_warning,
        ...
      )
      if (!give_rand_strat_warning) {
        # Only update if we have checked for it in this iteration, otherwise
        # we could overwrite a TRUE value with the default FALSE.
        give_rand_strat_warning <<- result$give_rand_strat_warning
      }
      result
    }
  )

  if (give_rand_strat_warning) {
    missing_vars <- setdiff(input$randomization_strata, c(input$covariates, input$strata))
    cov_string <- if (length(missing_vars) > 1) {
      paste0("interaction(", toString(missing_vars), ")")
    } else {
      missing_vars
    }
    strata_string <- paste0("strata(", toString(missing_vars), ")")
    warning(
      paste0(
        "It looks like you have not included all of the variables that were used ",
        "during randomization in your analysis `formula`. You can either:\n\n",
        "a. adjust for all joint levels in your `formula` using `+ ",
        cov_string,
        "` or\n",
        "b. perform a stratified test by adding to your `formula` the term `+ ",
        strata_string,
        "`\n\n",
        "NOTE: (b) changes the null hypothesis from your current model specification. ",
        "Please see the vignette `robincar-survival` for details."
      ),
      call. = FALSE
    )
  }

  result <- list(
    model = formula,
    vars = input,
    data = data,
    events_table = events_table,
    randomization = treatment,
    schema = input$schema,
    contrast = contrast,
    test = test,
    pair = comparisons,
    estimate = sapply(estimates, "[[", "estimate"),
    se = sapply(estimates, "[[", "se"),
    hr_n = sapply(estimates, "[[", "hr_n"),
    hr_sigma_l2 = sapply(estimates, "[[", "hr_sigma_l2"),
    test_stat = sapply(estimates, "[[", "test_stat"),
    p_value = sapply(estimates, "[[", "p_value"),
    test_score = sapply(estimates, "[[", "test_score"),
    test_n = sapply(estimates, "[[", "test_n"),
    test_sigma_l2 = sapply(estimates, "[[", "test_sigma_l2")
  )

  if (contrast == "hazardratio") {
    result$log_hr_coef_mat <- h_log_hr_coef_mat(result)
  }
  result$test_mat <- h_test_mat(result)

  class(result) <- "surv_effect"
  result
}
