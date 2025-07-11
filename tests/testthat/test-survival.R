# Whether to actually run RobinCar functions in the tests.
# By default, the saved numbers will be used instead.
run_robin_car <- FALSE

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

test_that("h_log_hr_est_via_score works as expected", {
  result <- h_log_hr_est_via_score(
    h_lr_score_no_strata_no_cov,
    df = surv_data,
    treatment = "sex",
    time = "time",
    status = "status"
  )
  expect_snapshot_value(result, tolerance = 1e-4, style = "deparse")
})

test_that("h_log_hr_est_via_score extends the search interval as needed", {
  result <- h_log_hr_est_via_score(
    h_lr_score_no_strata_no_cov,
    interval = c(-0.2, 0.2),
    df = surv_data,
    treatment = "sex",
    time = "time",
    status = "status"
  )
  expect_true(result$theta > 0.2)
  expect_snapshot_value(result, tolerance = 1e-4, style = "deparse")
})

test_that("h_lr_test_via_score works as expected", {
  result <- h_lr_test_via_score(
    h_lr_score_no_strata_no_cov,
    df = surv_data,
    treatment = "sex",
    time = "time",
    status = "status"
  )
  expect_snapshot_value(result, tolerance = 1e-4, style = "deparse")
})

test_that("robin_surv_comparison works as expected without covariate adjustment", {
  input <- h_prep_survival_input(
    formula = survival::Surv(time, status) ~ sex,
    data = surv_data,
    treatment = sex ~ 1
  )
  result <- robin_surv_comparison(
    score_fun = h_lr_score_no_strata_no_cov,
    vars = input,
    data = input$data,
    exp_level = 2,
    control_level = 1,
    treatment = input$treatment,
    time = input$time,
    status = input$status
  )
  expect_snapshot_value(result, tolerance = 1e-4, style = "deparse")
})

test_that("robin_surv_no_strata_no_cov works as expected", {
  input <- h_prep_survival_input(
    formula = survival::Surv(time, status) ~ sex,
    data = surv_data,
    treatment = sex ~ 1
  )
  result <- robin_surv_no_strata_no_cov(
    vars = input,
    data = input$data,
    exp_level = 1,
    control_level = 2
  )
  expect_snapshot_value(result, tolerance = 1e-4, style = "deparse")
})

test_that("robin_surv_no_strata_no_cov gives the same results as RobinCar functions", {
  surv_data2 <- surv_data
  surv_data2$ecog <- factor(surv_data2$ph.ecog == 1, labels = c("0", "1"))
  input <- h_prep_survival_input(
    formula = survival::Surv(time, status) ~ ecog,
    data = surv_data2,
    treatment = ecog ~ 1
  )
  input$data <- na.omit(input$data)
  result <- robin_surv_no_strata_no_cov(
    vars = input,
    data = input$data,
    exp_level = 2,
    control_level = 1
  )
  robincar_result <- if (run_robin_car) {
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
  } else {
    # These values are extracted from above RobinCar (version 1.0.0) results.
    list(
      test_stat = -0.6188324,
      test_sigma_l2 = 0.1782103,
      estimate = -0.1131005,
      se = 0.1830198
    )
  }
  expect_equal(result$test_stat, robincar_result$test_stat, tolerance = 1e-4)
  expect_equal(result$test_sigma_l2, robincar_result$test_sigma_l2, tolerance = 1e-4)
  expect_equal(result$estimate, robincar_result$estimate, tolerance = 1e-4)
  expect_equal(result$se, robincar_result$se, tolerance = 1e-4)
})

test_that("robin_surv_strata works as expected", {
  input <- h_prep_survival_input(
    formula = survival::Surv(time, status) ~ sex * strata,
    data = surv_data,
    treatment = sex ~ strata
  )
  result <- robin_surv_strata(
    vars = input,
    data = input$data,
    exp_level = 1,
    control_level = 2
  )
  expect_snapshot_value(result, tolerance = 1e-4, style = "deparse")
})

test_that("robin_surv_strata gives the same results as RobinCar functions", {
  surv_data2 <- surv_data
  surv_data2$ecog <- factor(surv_data2$ph.ecog == 1, labels = c("0", "1"))
  input <- h_prep_survival_input(
    formula = survival::Surv(time, status) ~ ecog * sex,
    data = surv_data2,
    treatment = ecog ~ sex
  )
  input$data <- na.omit(input$data)
  result <- robin_surv_strata(
    vars = input,
    data = input$data,
    exp_level = 2,
    control_level = 1
  )
  robincar_result <- if (run_robin_car) {
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
  } else {
    # These values are extracted from above RobinCar (version 1.0.0) results.
    list(
      test_stat = -0.6171326,
      test_sigma_l2 = 0.1749327,
      estimate = -0.1138251,
      se = 0.1847554
    )
  }
  expect_equal(result$test_stat, robincar_result$test_stat, tolerance = 1e-4)
  expect_equal(result$test_sigma_l2, robincar_result$test_sigma_l2, tolerance = 1e-4)
  expect_equal(result$estimate, robincar_result$estimate, tolerance = 1e-4)
  expect_equal(result$se, robincar_result$se, tolerance = 1e-4)
})

test_that("robin_surv_cov works as expected", {
  input <- h_prep_survival_input(
    formula = survival::Surv(time, status) ~ sex + age,
    data = surv_data,
    treatment = sex ~ 1
  )
  result <- robin_surv_cov(
    vars = input,
    data = input$data,
    exp_level = 1,
    control_level = 2
  )
  expect_snapshot_value(result, tolerance = 1e-4, style = "deparse")
})

test_that("robin_surv_cov gives the same results as RobinCar functions", {
  surv_data2 <- surv_data
  surv_data2$ecog <- factor(surv_data2$ph.ecog == 1, labels = c("0", "1"))
  input <- h_prep_survival_input(
    formula = survival::Surv(time, status) ~ ecog + age,
    data = surv_data2,
    treatment = ecog ~ 1
  )
  input$data <- na.omit(input$data)
  result <- robin_surv_cov(
    vars = input,
    data = input$data,
    exp_level = 2,
    control_level = 1
  )
  robincar_result <- if (run_robin_car) {
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
  } else {
    # These values are extracted from above RobinCar (version 1.0.0) results.
    list(
      test_stat = -0.4309412,
      test_sigma_l2 = 0.1757202,
      estimate = -0.07914235,
      se = 0.1818081
    )
  }
  expect_equal(result$test_stat, robincar_result$test_stat, tolerance = 1e-4)
  expect_equal(result$test_sigma_l2, robincar_result$test_sigma_l2, tolerance = 1e-4)
  expect_equal(result$estimate, robincar_result$estimate, tolerance = 1e-4)
  expect_equal(result$se, robincar_result$se, tolerance = 2e-4)
})
