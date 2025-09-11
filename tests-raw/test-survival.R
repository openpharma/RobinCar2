# Calculate RobinCar results given the function arguments lists.
calc_robin_car <- function(args) {
  has_covariates <- !is.null(args$covariate_cols)

  lr_result <- do.call(RobinCar::robincar_logrank, args)
  hr_result <- do.call(RobinCar::robincar_covhr, args)
  n <- nrow(args$df)

  lr_output <- utils::capture.output(print(lr_result))
  lr_pval_line <- grep("p-value", lr_output, value = TRUE)
  lr_pval <- as.numeric(sub("2-side p-value: ", "", lr_pval_line, fixed = TRUE))
  c(
    with(
      lr_result$result,
      list(
        test_stat = statistic,
        test_sigma_l2 = n * se^2,
        test_p_val = lr_pval
      )
    ),
    with(
      hr_result$result,
      list(
        estimate = ifelse(has_covariates, theta_CL, theta_L),
        se = ifelse(has_covariates, se_theta_CL, se_theta_L)
      )
    )
  )
}

# test: "robin_surv_no_strata_no_cov gives the same results as RobinCar functions"
robincar_args <- list(
  df = na.omit(surv_data),
  treat_col = "ecog",
  response_col = "time",
  event_col = "status",
  car_strata_cols = NULL,
  covariate_cols = NULL,
  car_scheme = "simple",
  adj_method = "CL",
  ref_arm = "0",
  p_trt = mean(surv_data$ecog == "1", na.rm = TRUE)
)
calc_robin_car(robincar_args)

# test: "robin_surv_strata gives the same results as RobinCar functions"
robincar_args <- list(
  df = na.omit(surv_data),
  treat_col = "ecog",
  response_col = "time",
  event_col = "status",
  car_strata_cols = "sex",
  covariate_cols = NULL,
  car_scheme = "permuted-block",
  adj_method = "CSL",
  ref_arm = "0",
  p_trt = mean(surv_data$ecog == "1", na.rm = TRUE)
)
calc_robin_car(robincar_args)

# test: "robin_surv_cov gives the same results as RobinCar functions"
robincar_args <- list(
  df = na.omit(surv_data),
  treat_col = "ecog",
  response_col = "time",
  event_col = "status",
  car_strata_cols = NULL,
  covariate_cols = "age",
  car_scheme = "simple",
  adj_method = "CL",
  ref_arm = "0",
  p_trt = mean(surv_data$ecog == "1", na.rm = TRUE)
)
calc_robin_car(robincar_args)

# test: "robin_surv gives the same results as RobinCar for single factor covariate"
robincar_args <- list(
  df = na.omit(surv_data),
  treat_col = "ecog",
  response_col = "time",
  event_col = "status",
  car_strata_cols = NULL,
  covariate_cols = "sex", # We use a single factor covariate here.
  car_scheme = "simple",
  adj_method = "CL",
  ref_arm = "0",
  p_trt = mean(surv_data$ecog == "1", na.rm = TRUE)
)
calc_robin_car(robincar_args)

# test: "robin_surv_strata_cov gives the same results as RobinCar functions"
robincar_args <- list(
  df = na.omit(surv_data),
  treat_col = "ecog",
  response_col = "time",
  event_col = "status",
  car_strata_cols = "sex",
  covariate_cols = "age",
  car_scheme = "permuted-block",
  adj_method = "CSL",
  ref_arm = "0",
  p_trt = mean(surv_data$ecog == "1", na.rm = TRUE)
)
calc_robin_car(robincar_args)

# test: "robin_surv_strata_cov gives the same results as RobinCar for strong correlation covariate with strata"
set.seed(2040)
surv_data2 <- surv_data
surv_data2$ecog <- as.factor(ifelse(
  surv_data$strata == 1,
  rbinom(nrow(surv_data), 1, prob = 0.2),
  surv_data$ecog
))
surv_data2 <- surv_data2[surv_data2$strata %in% c(0, 1), ]

robincar_args <- list(
  df = na.omit(surv_data2),
  treat_col = "sex",
  response_col = "time",
  event_col = "status",
  car_strata_cols = "strata",
  covariate_cols = "ecog",
  car_scheme = "permuted-block",
  adj_method = "CSL",
  ref_arm = "Female",
  p_trt = mean(surv_data2$sex == "Male", na.rm = TRUE)
)
calc_robin_car(robincar_args)
