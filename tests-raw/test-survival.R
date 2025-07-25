# Calculate RobinCar results given the function arguments lists.
calc_robin_car <- function(args) {
  has_covariates <- !is.null(args$covariate_cols)

  lr_result <- do.call(RobinCar::robincar_logrank, args)
  hr_result <- do.call(RobinCar::robincar_covhr, args)
  n <- nrow(args$df)

  c(
    with(
      lr_result$result,
      list(
        test_stat = statistic,
        test_sigma_l2 = n * se^2
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
  df = input$data,
  treat_col = "ecog",
  response_col = "time",
  event_col = "status",
  car_strata_cols = NULL,
  covariate_cols = NULL,
  car_scheme = "simple",
  adj_method = "CL",
  ref_arm = "0",
  p_trt = mean(input$data$ecog == "1")
)
calc_robin_car(robincar_args)

# test: "robin_surv_strata gives the same results as RobinCar functions"
robincar_args <- list(
  df = input$data,
  treat_col = "ecog",
  response_col = "time",
  event_col = "status",
  car_strata_cols = "sex",
  covariate_cols = NULL,
  car_scheme = "permuted-block",
  adj_method = "CSL",
  ref_arm = "0",
  p_trt = mean(input$data$ecog == "1")
)
calc_robin_car(robincar_args)

# test: "robin_surv_cov gives the same results as RobinCar functions"
robincar_args <- list(
  df = input$data,
  treat_col = "ecog",
  response_col = "time",
  event_col = "status",
  car_strata_cols = NULL,
  covariate_cols = "age",
  car_scheme = "simple",
  adj_method = "CL",
  ref_arm = "0",
  p_trt = mean(input$data$ecog == "1")
)
calc_robin_car(robincar_args)
