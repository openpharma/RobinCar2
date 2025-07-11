test_that("h_derived_outcome_vals works as expected", {
  surv_data_full <- na.omit(surv_data)
  result <- h_derived_outcome_vals(
    theta = 0,
    df = surv_data_full,
    treatment = "sex",
    time = "time",
    status = "status",
    covariates = c("age", "ph.karno"),
    n = 400
  )
  expect_data_frame(result, ncol = 4 + 2 + 1, nrow = nrow(surv_data_full))
  head_result <- result[seq_len(6), ]
  head_expected <- data.frame(
    index = 1:6,
    treatment = c(1, 1, 1, 0, 0, 1),
    time = c(455, 210, 1022, 310, 361, 218),
    status = c(1, 1, 0, 1, 1, 1),
    age = c(68, 57, 74, 68, 71, 53),
    ph.karno = c(90, 90, 50, 70, 60, 70),
    O_hat = c(
      -0.059407303167448,
      0.280849881485069,
      -1.1574498129749,
      0.128415337453864,
      0.0645341495986705,
      0.27539351150966
    )
  )
  expect_equal(head_result, head_expected, tolerance = 1e-4, ignore_attr = TRUE)
})

test_that("h_get_lm_input works as expected", {
  set.seed(941)
  df <- data.frame(
    index = 1:5,
    treatment = factor(c(0, 1, 0, 1, 0)),
    covariate1 = rnorm(5),
    covariate2 = rnorm(5),
    O_hat = rnorm(5)
  )
  result <- h_get_lm_input(df, model = ~ covariate1 + covariate2)
  expect_list(result, len = 2L)
  expect_list(result[["0"]], len = 2L)
  expect_matrix(result[["0"]][["X"]], ncol = 2L, nrow = 3L)
  expect_numeric(result[["0"]][["y"]], len = 3L)
  expect_list(result[["1"]], len = 2L)
  expect_matrix(result[["1"]][["X"]], ncol = 2L, nrow = 2L)
  expect_numeric(result[["1"]][["y"]], len = 2L)
})

test_that("h_get_beta_estimates works as expected", {
  set.seed(941)
  nobs <- 10
  df <- data.frame(
    treatment = factor(sample(c(0, 1), nobs, replace = TRUE)),
    covariate1 = rnorm(nobs),
    covariate2 = rnorm(nobs),
    O_hat = rnorm(nobs)
  )
  lm_input <- h_get_lm_input(df, model = ~ covariate1 + covariate2)
  result <- h_get_beta_estimates(lm_input)
  expect_snapshot_value(result, tolerance = 1e-4, style = "deparse")
})
