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

test_that("h_lr_score_no_strata_no_cov works when not calculating the variance", {
  result <- h_lr_score_no_strata_no_cov(
    theta = 0,
    df = surv_data,
    treatment = "sex",
    time = "time",
    status = "status",
    calculate_variance = FALSE
  )
  expect_equal(as.numeric(result), 0.08955378, tolerance = 1e-6)
  expect_scalar_na(attr(result, "sigma_l2"))
  expect_scalar_na(attr(result, "se_theta_l"))
  expect_equal(attr(result, "n"), nrow(surv_data))
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

test_that("h_lr_score_strat works with multiple strata variables", {
  result <- h_lr_score_strat(
    theta = 0,
    df = surv_data,
    treatment = "sex",
    time = "time",
    status = "status",
    strata = c("strata", "ecog")
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

test_that("h_lr_score_strat works as expected when not calculating the variance", {
  result <- h_lr_score_strat(
    theta = 0,
    df = surv_data,
    treatment = "sex",
    time = "time",
    status = "status",
    strata = "strata",
    use_ties_factor = FALSE,
    calculate_variance = FALSE
  )
  expect_equal(as.numeric(result), 0.08968712, tolerance = 1e-6)
  expect_scalar_na(attr(result, "sigma_l2"))
  expect_scalar_na(attr(result, "se_theta_l"))
  expect_equal(attr(result, "n"), 227)
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
  result <- h_lr_score_cov(
    theta = 0,
    df = surv_data,
    treatment = "sex",
    time = "time",
    status = "status",
    model = ~age
  )
  expect_snapshot_value(result, tolerance = 1e-4, style = "deparse")
})

test_that("h_lr_score_cov works as expected when not using ties factor", {
  result <- h_lr_score_cov(
    theta = 0,
    df = surv_data,
    treatment = "sex",
    time = "time",
    status = "status",
    model = ~age,
    use_ties_factor = FALSE
  )
  expect_snapshot_value(result, tolerance = 1e-4, style = "deparse")
})

test_that("h_lr_score_cov works as expected when not calculating the variance", {
  result <- h_lr_score_cov(
    theta = 0,
    df = surv_data,
    treatment = "sex",
    time = "time",
    status = "status",
    model = ~age,
    use_ties_factor = FALSE,
    calculate_variance = FALSE
  )
  expect_equal(as.numeric(result), 0.08512662, tolerance = 1e-6)
  expect_scalar_na(attr(result, "sigma_l2"))
  expect_scalar_na(attr(result, "se_theta_l"))
  expect_equal(attr(result, "n"), 228)
})

test_that("h_lr_score_cov works when using the unadjusted standard error option", {
  result <- h_lr_score_cov(
    theta = 0,
    df = surv_data,
    treatment = "sex",
    time = "time",
    status = "status",
    model = ~age,
    hr_se_plugin_adjusted = FALSE,
    use_ties_factor = FALSE,
    theta_hat = 0.2
  )
  expect_snapshot_value(result, tolerance = 1e-4, style = "deparse")

  result_default <- h_lr_score_cov(
    theta = 0,
    df = surv_data,
    treatment = "sex",
    time = "time",
    status = "status",
    model = ~age,
    use_ties_factor = FALSE,
    theta_hat = 0.2
  )
  expect_snapshot_value(result_default, tolerance = 1e-4, style = "deparse")

  expect_equal(as.numeric(result), as.numeric(result_default))
  expect_true(
    !isTRUE(all.equal(
      attr(result, "sigma_l2"),
      attr(result_default, "sigma_l2")
    ))
  )
})

test_that("h_lr_score_cov works with a single factor covariate", {
  result <- h_lr_score_cov(
    theta = 0,
    df = surv_data,
    treatment = "sex",
    time = "time",
    status = "status",
    model = ~ecog
  )
  expect_snapshot_value(result, tolerance = 1e-4, style = "deparse")
})

test_that("h_lr_score_strat_cov works as expected with default options", {
  result <- h_lr_score_strat_cov(
    theta = 0,
    df = surv_data,
    treatment = "sex",
    time = "time",
    status = "status",
    strata = "strata",
    model = ~age
  )
  expect_snapshot_value(result, tolerance = 1e-4, style = "deparse")
})

test_that("h_lr_score_strat_cov works with multiple strata variables", {
  result <- h_lr_score_strat_cov(
    theta = 0,
    df = surv_data,
    treatment = "sex",
    time = "time",
    status = "status",
    strata = c("strata", "ecog"),
    model = ~age
  )
  expect_snapshot_value(result, tolerance = 1e-4, style = "deparse")
})

test_that("h_lr_score_strat_cov works as expected when not using ties factor", {
  result <- h_lr_score_strat_cov(
    theta = 0,
    df = surv_data,
    treatment = "sex",
    time = "time",
    status = "status",
    strata = "strata",
    model = ~age,
    use_ties_factor = FALSE
  )
  expect_snapshot_value(result, tolerance = 1e-4, style = "deparse")
})

test_that("h_lr_score_strat_cov works as expected when not calculating the variance", {
  result <- h_lr_score_strat_cov(
    theta = 0,
    df = surv_data,
    treatment = "sex",
    time = "time",
    status = "status",
    strata = "strata",
    model = ~age,
    use_ties_factor = FALSE,
    calculate_variance = FALSE
  )
  expect_equal(as.numeric(result), 0.08975217, tolerance = 1e-6)
  expect_scalar_na(attr(result, "se_theta_l"))
  expect_scalar_na(attr(result, "sigma_l2"))
  expect_equal(attr(result, "n"), 227)
})

test_that("h_lr_score_strat_cov works as expected with unadjusted standard error", {
  result <- h_lr_score_strat_cov(
    theta = 0,
    df = surv_data,
    treatment = "sex",
    time = "time",
    status = "status",
    strata = "strata",
    model = ~age,
    use_ties_factor = FALSE,
    hr_se_plugin_adjusted = FALSE,
    theta_hat = 0.3
  )
  expect_snapshot_value(result, tolerance = 1e-4, style = "deparse")

  result_default <- h_lr_score_strat_cov(
    theta = 0,
    df = surv_data,
    treatment = "sex",
    time = "time",
    status = "status",
    strata = "strata",
    model = ~age,
    use_ties_factor = FALSE,
    hr_se_plugin_adjusted = FALSE,
    theta_hat = 0.3
  )
  expect_snapshot_value(result, tolerance = 1e-4, style = "deparse")

  result_default <- h_lr_score_strat_cov(
    theta = 0,
    df = surv_data,
    treatment = "sex",
    time = "time",
    status = "status",
    strata = "strata",
    model = ~age,
    use_ties_factor = FALSE,
    theta_hat = 0.3
  )
  expect_snapshot_value(result_default, tolerance = 1e-4, style = "deparse")

  expect_equal(as.numeric(result), as.numeric(result_default))
  expect_true(
    !isTRUE(all.equal(
      attr(result, "sigma_l2"),
      attr(result_default, "sigma_l2")
    ))
  )
})
