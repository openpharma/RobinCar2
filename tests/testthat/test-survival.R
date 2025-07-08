test_that("h_lr_score_no_strata_no_cov works as expected with default options", {
  result <- h_lr_score_no_strata_no_cov(
    theta = 0,
    df = surv_dat,
    treatment = "sex",
    time = "time",
    status = "status"
  )
  expect_snapshot_value(result, tolerance = 1e-4, style = "deparse")
})

test_that("h_lr_score_no_strata_no_cov works as expected with custom n", {
  result <- h_lr_score_no_strata_no_cov(
    theta = 0,
    df = surv_dat,
    treatment = "sex",
    time = "time",
    status = "status",
    n = 400
  )
  expect_snapshot_value(result, tolerance = 1e-4, style = "deparse")
})

test_that("h_lr_score_no_strata_no_cov works as expected when not using ties factor", {
  result <- h_lr_score_no_strata_no_cov(
    theta = 0.5,
    df = surv_dat,
    treatment = "sex",
    time = "time",
    status = "status",
    use_ties_factor = FALSE
  )
  expect_snapshot_value(result, tolerance = 1e-4, style = "deparse")
})

test_that("h_lr_score_no_strata_no_cov works as expected with multiple theta values", {
  theta_vals <- c(0, 0.5, 1)
  result <- h_lr_score_no_strata_no_cov(
    theta = theta_vals,
    df = surv_dat,
    treatment = "sex",
    time = "time",
    status = "status",
    use_ties_factor = FALSE
  )
  expected <- sapply(theta_vals, function(theta) {
    h_lr_score_no_strata_no_cov(
      theta = theta,
      df = surv_dat,
      treatment = "sex",
      time = "time",
      status = "status",
      use_ties_factor = FALSE
    )
  })
  expect_identical(as.numeric(result), expected)
})

test_that("h_log_hr_est_via_score works as expected", {
  result <- h_log_hr_est_via_score(
    h_lr_score_no_strata_no_cov,
    df = surv_dat,
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
    df = surv_dat,
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
    df = surv_dat,
    treatment = "sex",
    time = "time",
    status = "status"
  )
  expect_snapshot_value(result, tolerance = 1e-4, style = "deparse")
})

test_that("robin_surv_comparison works as expected without covariate adjustment", {
  vars <- h_prep_survival_vars(
    formula = survival::Surv(time, status) ~ sex,
    data = surv_dat,
    treatment = sex ~ 1
  )
  result <- robin_surv_comparison(
    score_fun = h_lr_score_no_strata_no_cov,
    vars = vars,
    data = surv_dat,
    exp_level = 2,
    control_level = 1,
    treatment = vars$treatment,
    time = vars$time,
    status = vars$status
  )
  expect_snapshot_value(result, tolerance = 1e-4, style = "deparse")
})
