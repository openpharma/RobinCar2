#' Derive Outcome Values Based on Log Hazard Ratio
#'
#' Compute the derived outcome values based on a given log hazard ratio.
#'
#' @inheritParams survival_score_functions
#' @param covariates (`character`) The column names in `df` to be used for covariate adjustment.
#' @return A data frame containing the same data as the input `df`, but restructured with standardized column names
#'   `index`, `treatment`, `time`, `status`, the covariates, and an additional column `O_hat` containing the
#'   derived outcome values. For the stratified version, the list of data frames is returned, one for each stratum.
#' @details Please note that the `covariates` must not include `index`, `treatment`, `time`, `status`
#'   to avoid naming conflicts.
#' @keywords internal
#' @name derived_outcome_vals
NULL

#' @describeIn derived_outcome_vals calculates the derived outcome values for the overall data set.
h_derived_outcome_vals <- function(theta, df, treatment, time, status, covariates, n = nrow(df)) {
  assert_number(theta)
  assert_string(treatment)
  assert_string(time)
  assert_string(status)
  assert_character(covariates, min.len = 1L, any.missing = FALSE, unique = TRUE)
  assert_data_frame(df)
  assert_factor(df[[treatment]], n.levels = 2L)
  assert_numeric(df[[status]])
  assert_true(all(df[[status]] %in% c(0, 1)))
  assert_numeric(df[[time]], lower = 0)
  assert_subset(covariates, names(df))
  assert_disjunct(covariates, c("index", "treatment", "time", "status", "treatment_numeric", "O_hat"))

  # Standardize data set format, subset to relevant variables.
  df <- data.frame(
    index = seq_len(nrow(df)),
    treatment = df[[treatment]],
    treatment_numeric = as.numeric(df[[treatment]]) - 1L,
    time = df[[time]],
    status = df[[status]],
    df[covariates]
  )
  assert_true(!any(is.na(df)))

  # Sort by time.
  df <- df[order(df$time), , drop = FALSE]

  # Number of patients with events at unique event times.
  df_events_unique <- h_n_events_per_time(
    df = df,
    time = "time",
    status = "status"
  )

  # Add derived outcome column.
  df$O_hat <- NA_real_

  # Calculate quantities which are the same across patients first.
  # These are in parallel to df_events_unique.

  # Hazard ratio.
  exp_theta <- exp(theta)

  # Proportions of patients at risk, per unique event time and treatment arm.
  # Corresponds to \exp(\theta) * \bar{Y}_1(t) and \bar{Y}_0(t).
  # So here theta enters.
  at_risk_matrix <- outer(df$time, df_events_unique$time, FUN = ">=")
  y_bar_1 <- exp_theta * colSums(df$treatment_numeric & at_risk_matrix) / n
  y_bar_0 <- colSums(!df$treatment_numeric & at_risk_matrix) / n
  y_bar <- y_bar_0 + y_bar_1

  # Proportion of patients having an event at this time.
  # Corresponds to d\bar{N}(t). Here we need to be careful about tied event times,
  # therefore we see how many patients have an event at each unique time and divide that by n.
  dn_bar <- df_events_unique$n_events / n

  # Loop over all patients.
  for (i in seq_len(nrow(df))) {
    # Treatment arm?
    trt_i <- df$treatment_numeric[i]

    # Event in this patient?
    delta_i <- df$status[i] == 1

    # Time for this patient.
    t_i <- df$time[i]

    # Does this patient have an event at this unique event time? Corresponds to dN_ij(t).
    dn_ij <- delta_i * (df_events_unique$time == t_i)

    # Is this patient at risk at this unique event time? Corresponds to Y_ij(t).
    # Here theta enters, too.
    y_ij <- as.numeric(t_i >= df_events_unique$time) * ifelse(trt_i, exp_theta, 1)

    # Calculate the weights, Y_bar in opposite treatment arm divided by Y_bar overall.
    weights <- (trt_i * y_bar_0 + (1 - trt_i) * y_bar_1) / y_bar

    # Compute martingale residuals.
    martingale_residuals <- dn_ij - y_ij * dn_bar / y_bar

    # Sum across all unique event times.
    df$O_hat[i] <- sum(weights * martingale_residuals)
  }

  # Return in original order with relevant columns only.
  include_cols <- c("index", "treatment", "time", "status", "O_hat", covariates)
  df[order(df$index), include_cols, drop = FALSE]
}

#' @describeIn derived_outcome_vals calculates the derived outcome values for each stratum separately.
h_strat_derived_outcome_vals <- function(theta, df, treatment, time, status, strata, covariates) {
  assert_character(strata, any.missing = FALSE, min.len = 1L, unique = TRUE)
  assert_data_frame(df)
  lapply(df[strata], assert_factor)

  assert_true(!any(is.na(df)))
  n <- nrow(df)

  strata_formula <- paste("~", paste(strata, collapse = "+"))
  df_split <- split(df, f = as.formula(strata_formula), drop = TRUE)

  lapply(
    df_split,
    FUN = h_derived_outcome_vals,
    theta = theta,
    treatment = treatment,
    time = time,
    status = status,
    covariates = covariates,
    n = n
  )
}

#' Get Linear Model Input Data
#'
#' Prepare the input data for a linear model based on the provided data frame and model formula.
#'
#' @param df (`data.frame`) Including the covariates needed for the `model`, as well as the derived outcome `O_hat`
#'   and the `treatment` factor.
#' @param df_split (`list`) A list of data frames, one for each stratum, as returned by
#'   [h_strat_derived_outcome_vals()].
#' @param model (`formula`) The right-hand side only model formula.
#' @return A list containing for each element of the `treatment` factor a list with the
#'   corresponding model matrix `X` and the response vector `y`. For the stratified version, a list of such
#'   lists is returned, one for each stratum.
#' @keywords internal
#' @name get_lm_input
NULL

#' @describeIn get_lm_input Get the linear model input data for the overall data set.
h_get_lm_input <- function(df, model) {
  assert_data_frame(df)
  assert_formula(model)
  assert_true(length(model) == 2L) # Ensures right-hand side only formula.
  assert_subset(c("treatment", "O_hat", all.vars(model)), names(df))
  assert_factor(df$treatment)

  # Add outcome:
  model <- stats::update(model, O_hat ~ .)
  includes_intercept <- attr(stats::terms(model), "intercept") == 1
  assert_true(includes_intercept)

  # Create overall design matrix and response vector.
  mf <- stats::model.frame(model, data = df)
  x <- stats::model.matrix(model, data = mf)
  assert_true(identical(colnames(x)[1L], "(Intercept)"))
  x <- x[, -1L, drop = FALSE] # Remove intercept.
  y <- stats::model.response(mf)

  # Split both design matrix and response vector by treatment arm.
  index_by_trt <- split(seq_along(y), f = df$treatment)
  lapply(
    index_by_trt,
    function(this_index) {
      list(X = x[this_index, , drop = FALSE], y = y[this_index])
    }
  )
}

#' @describeIn get_lm_input Get the linear model input data for each stratum separately.
h_get_strat_lm_input <- function(df_split, model) {
  assert_list(df_split, types = "data.frame")
  lapply(df_split, h_get_lm_input, model = model)
}

#' Calculate Coefficient Estimates from Linear Model Input
#'
#' Calculate the coefficient estimates for each treatment arm from the linear model input data.
#'
#' @param lm_input (`list`) A list containing the linear model input data for each treatment arm, as returned by
#'   [h_get_lm_input()].
#' @param strat_lm_input (`list`) A list of lists, one for each stratum, containing the linear model input data
#'   for each treatment arm, as returned by [h_get_strat_lm_input()].
#' @return A list containing the coefficient estimates for each treatment arm.
#' @keywords internal
#' @name get_beta_estimates
NULL

#' @describeIn get_beta_estimates Calculate the coefficient estimates for the overall data set.
h_get_beta_estimates <- function(lm_input) {
  assert_list(lm_input, types = "list")

  # Fit the model separately for each treatment arm.
  beta_est <- list()

  for (group in names(lm_input)) {
    assert_matrix(lm_input[[group]]$X, any.missing = FALSE)
    assert_numeric(lm_input[[group]]$y, any.missing = FALSE)

    # Get the design matrix for this treatment arm.
    x <- lm_input[[group]]$X

    # Center it.
    x <- scale(x, center = TRUE, scale = FALSE)

    # Get the derived outcome values, the response.
    y <- lm_input[[group]]$y

    # Fit the model without intercept.
    lm_fit <- stats::lm.fit(x, y, singular.ok = FALSE)

    # Save the coefficients.
    beta_est[[group]] <- lm_fit$coefficients
  }

  beta_est
}

#' @describeIn get_beta_estimates Calculate the coefficient estimates using the stratified input.
h_get_strat_beta_estimates <- function(strat_lm_input) {
  assert_list(strat_lm_input, types = "list")
  assert_list(strat_lm_input[[1]], types = "list", len = 2L, names = "unique")
  group_names <- names(strat_lm_input[[1]])

  # Get coefficient estimates separately for each treatment arm.
  beta_est <- list()

  for (group in group_names) {
    xtxs <- list()
    xtys <- list()

    for (stratum in names(strat_lm_input)) {
      # If this group exists in this stratum, save the corresponding cross products
      # for this group and stratum.
      if (group %in% names(strat_lm_input[[stratum]])) {
        # Get the design matrix for this treatment arm.
        x <- strat_lm_input[[stratum]][[group]]$X

        # Center it.
        x <- scale(x, center = TRUE, scale = FALSE)

        # Get the derived outcome values, the response.
        y <- strat_lm_input[[stratum]][[group]]$y

        # Save the cross products.
        xtxs[[stratum]] <- crossprod(x)
        xtys[[stratum]] <- crossprod(x, y)
      }
    }

    # Sum across strata.
    xtx <- Reduce("+", xtxs)
    xty <- Reduce("+", xtys)

    # Get the coefficients.
    beta_est[[group]] <- solve(xtx, xty)
  }

  beta_est
}
