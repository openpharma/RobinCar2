test_that("h_lr_score_no_strata_no_cov works as expected with default options", {
  result <- h_lr_score_no_strata_no_cov(
    theta = 0,
    df = surv_data,
    treatment = "sex",
    time = "time",
    status = "status"
  )
  expect_snapshot_value(result, tolerance = 1e-4, style = "deparse")
})

test_that("h_lr_score_no_strata_no_cov works as expected with custom n", {
  result <- h_lr_score_no_strata_no_cov(
    theta = 0,
    df = surv_data,
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
    df = surv_data,
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
    df = surv_data,
    treatment = "sex",
    time = "time",
    status = "status",
    use_ties_factor = FALSE
  )
  expected <- sapply(theta_vals, function(theta) {
    h_lr_score_no_strata_no_cov(
      theta = theta,
      df = surv_data,
      treatment = "sex",
      time = "time",
      status = "status",
      use_ties_factor = FALSE
    )
  })
  expect_identical(as.numeric(result), expected)
})

test_that("h_lr_score_strat works as expected with default options", {
  result <- h_lr_score_strat(
    theta = 0,
    df = surv_data,
    treatment = "sex",
    time = "time",
    status = "status",
    strata = "strata"
  )
  expect_snapshot_value(result, tolerance = 1e-4, style = "deparse")
})

test_that("h_lr_score_strat works as expected when not using ties factor", {
  result <- h_lr_score_strat(
    theta = 0,
    df = surv_data,
    treatment = "sex",
    time = "time",
    status = "status",
    strata = "strata",
    use_ties_factor = FALSE
  )
  expect_snapshot_value(result, tolerance = 1e-4, style = "deparse")
})

test_that("h_lr_score_strat works as expected with multiple theta values", {
  theta_vals <- c(0, 0.5, 1)
  result <- h_lr_score_strat(
    theta = theta_vals,
    df = surv_data,
    treatment = "sex",
    time = "time",
    status = "status",
    strata = "strata",
    use_ties_factor = FALSE
  )
  expected <- sapply(theta_vals, function(theta) {
    h_lr_score_strat(
      theta = theta,
      df = surv_data,
      treatment = "sex",
      time = "time",
      status = "status",
      strata = "strata",
      use_ties_factor = FALSE
    )
  })
  expect_identical(as.numeric(result), expected)
})

test_that("h_lr_score_cov works as expected with default options", {
  surv_data_recoded <- surv_data |>
    dplyr::mutate(treatment = factor(as.numeric(sex == "Female")))
  result <- h_lr_score_cov(
    theta = 0,
    df = surv_data_recoded,
    treatment = "treatment",
    time = "time",
    status = "status",
    model = ~age
  )
  expect_snapshot_value(result, tolerance = 1e-4, style = "deparse")
})

test_that("h_lr_score_cov works as expected when not using ties factor", {
  surv_data_recoded <- surv_data |>
    dplyr::mutate(treatment = factor(as.numeric(sex == "Female")))
  result <- h_lr_score_cov(
    theta = 0,
    df = surv_data_recoded,
    treatment = "treatment",
    time = "time",
    status = "status",
    model = ~age,
    use_ties_factor = FALSE
  )
  expect_snapshot_value(result, tolerance = 1e-4, style = "deparse")
})
