test_that("h_derived_outcome_vals works as expected", {
  surv_data_full <- na.omit(surv_data)
  result <- h_derived_outcome_vals(
    theta = 0,
    df = surv_data_full,
    treatment = "sex",
    time = "time",
    status = "status",
    covariates = c("age", "ph.karno"),
    randomization_strata = character(),
    n = 400
  )
  expect_data_frame(result, ncol = 4 + 2 + 1, nrow = nrow(surv_data_full))
  head_result <- result[seq_len(6), ]
  head_expected <- data.frame(
    index = 1:6,
    treatment = factor(c("Male", "Male", "Male", "Female", "Female", "Male")),
    time = c(455, 210, 1022, 310, 361, 218),
    status = c(1, 1, 0, 1, 1, 1),
    O_hat = c(
      -0.059407303167448,
      0.280849881485069,
      -1.1574498129749,
      0.128415337453864,
      0.0645341495986705,
      0.27539351150966
    ),
    age = c(68, 57, 74, 68, 71, 53),
    ph.karno = c(90, 90, 50, 70, 60, 70)
  )
  expect_equal(head_result, head_expected, tolerance = 1e-4, ignore_attr = TRUE)
})

test_that("h_strat_derived_outcome_vals works as expected", {
  surv_data_full <- na.omit(surv_data)
  result <- h_strat_derived_outcome_vals(
    theta = 0,
    df = surv_data_full,
    treatment = "sex",
    time = "time",
    status = "status",
    strata = "strata",
    covariates = c("age", "ph.karno"),
    randomization_strata = character()
  )
  expect_data_frame(result, nrow = nrow(surv_data_full))
  expect_names(
    names(result),
    identical.to = c("index", "treatment", "time", "status", "O_hat", "age", "ph.karno", ".stratum")
  )
  expect_identical(as.integer(table(result$.stratum)), as.integer(table(surv_data_full$strata)))
})

test_that("h_strat_derived_outcome_vals works with multiple strata", {
  surv_data_full <- na.omit(surv_data)
  result <- h_strat_derived_outcome_vals(
    theta = 0,
    df = surv_data_full,
    treatment = "sex",
    time = "time",
    status = "status",
    strata = c("strata", "ecog"),
    covariates = c("age", "ph.karno"),
    randomization_strata = "ecog"
  )
  expect_data_frame(result, nrow = nrow(surv_data_full))
  expect_names(
    names(result),
    identical.to = c("index", "treatment", "time", "status", "O_hat", "age", "ph.karno", "ecog", ".stratum")
  )
  expect_identical(
    as.integer(table(result$.stratum)),
    as.integer(table(interaction(
      surv_data_full$strata,
      surv_data_full$ecog,
      drop = TRUE
    )))
  )
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

test_that("h_get_lm_input fails when there is no intercept", {
  set.seed(941)
  df <- data.frame(
    index = 1:5,
    treatment = factor(c(0, 1, 0, 1, 0)),
    covariate1 = rnorm(5),
    covariate2 = rnorm(5),
    O_hat = rnorm(5)
  )
  expect_error(
    h_get_lm_input(df, model = ~ 0 + covariate1 + covariate2),
    "Assertion on 'includes_intercept' failed"
  )
})

test_that("h_get_lm_input works correctly across treatment levels with character covariates", {
  set.seed(941)
  df <- data.frame(
    index = 1:5,
    treatment = factor(rep(c(0, 1), each = 5)),
    # Here we have A, B for treatment 0 only, C, D for treatment 1 only:
    covariate1 = c("A", "B", "A", "B", "A", "C", "D", "C", "D", "C"),
    covariate2 = rnorm(5),
    O_hat = rnorm(5)
  )
  result <- h_get_lm_input(df, model = ~ covariate1 + covariate2)
  expect_list(result, len = 2L)
  x_cols <- lapply(result, function(lst) colnames(lst[["X"]]))
  expect_true(identical(x_cols[[1]], x_cols[[2]]))
  # Note that this case will lead to rank-deficient design matrices, but at least
  # the user will get an appropriate error message, instead of a wrong result downstream.
})

test_that("h_get_strat_lm_input works as expected", {
  set.seed(941)
  df_with_stratum <- rbind(
    data.frame(
      index = 1:3,
      treatment = factor(c(0, 1, 0), labels = c("A", "B")),
      covariate1 = rnorm(3),
      covariate2 = rnorm(3),
      O_hat = rnorm(3),
      .stratum = 1
    ),
    data.frame(
      index = 4:5,
      treatment = factor(c(1, 0), labels = c("A", "B")),
      covariate1 = rnorm(2),
      covariate2 = rnorm(2),
      O_hat = rnorm(2),
      .stratum = 2
    )
  )
  result <- h_get_strat_lm_input(df_with_stratum, model = ~ covariate1 + covariate2)
  expect_list(result, len = 2L)
  expect_names(names(result), identical.to = c("A", "B"))
  # Check treatment group A:
  expect_list(result[["A"]], len = 2L)
  expect_matrix(result[["A"]][["X"]], ncol = 3L, nrow = 3L)
  expect_subset(".stratum", colnames(result[["A"]][["X"]]))
  expect_numeric(result[["A"]][["y"]], len = 3L)
  # Check treatment group B:
  expect_list(result[["B"]], len = 2L)
  expect_matrix(result[["B"]][["X"]], ncol = 3L, nrow = 2L)
  expect_subset(".stratum", colnames(result[["B"]][["X"]]))
  expect_numeric(result[["B"]][["y"]], len = 2L)
})

test_that("h_get_lm_results works as expected", {
  set.seed(941)
  nobs <- 10
  df <- data.frame(
    treatment = factor(sample(c("A", "B"), nobs, replace = TRUE)),
    covariate1 = rnorm(nobs),
    covariate2 = rnorm(nobs),
    O_hat = rnorm(nobs)
  )
  lm_input <- h_get_lm_input(df, model = ~ covariate1 + covariate2)
  result <- h_get_lm_results(lm_input)
  expect_snapshot_value(result, tolerance = 1e-4, style = "deparse")
})

test_that("h_get_strat_lm_results works as expected", {
  set.seed(941)
  nobs <- 10
  df_with_stratum <- rbind(
    data.frame(
      treatment = factor(sample(c("A", "B"), nobs, replace = TRUE)),
      covariate1 = rnorm(nobs),
      covariate2 = rnorm(nobs),
      O_hat = rnorm(nobs),
      .stratum = 1
    ),
    data.frame(
      treatment = factor(sample(c("A", "B"), nobs, replace = TRUE)),
      covariate1 = rnorm(nobs),
      covariate2 = rnorm(nobs),
      O_hat = rnorm(nobs),
      .stratum = 2
    )
  )
  strat_lm_input <- h_get_strat_lm_input(df_with_stratum, model = ~ covariate1 + covariate2)
  result <- h_get_strat_lm_results(strat_lm_input)
  expect_list(result, len = 2L)
  expect_names(names(result), identical.to = c("beta_est", "residuals"))
  expect_snapshot_value(result, tolerance = 1e-4, style = "serialize")
})
