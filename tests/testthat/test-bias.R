test_that("bias works for guassian", {
  expect_snapshot_value(
    bias(residuals(fit_glm), treatment = glm_data$treatment, group_idx = list(seq_len(nrow(glm_data)))),
    style = "deparse",
    tolerance = 1e-4
  )
})

test_that("bias can return NaN when there is a single patient in a group", {
  result <- bias(
    residual = c(1.0, 2.0, 3.0),
    treatment = factor(c("A", "B", "B")),
    group_idx = list(
      c(1L, 2L),
      c(3L)
    )
  )
  expected <- matrix(
    c(1, 1, NaN, 2, 2, 3),
    nrow = 3,
    ncol = 2
  )
  expect_equal(result, expected)
})

test_that("h_unbiased_means_across_strata works for zero residuals", {
  n_obs_per_group <- 10
  n_obs_total <- n_obs_per_group * 2

  residuals_per_group <- list(
    group1 = rep(0, n_obs_per_group),
    group2 = rep(0, n_obs_per_group)
  )
  df <- data.frame(
    treatment = factor(rep(c("group1", "group2"), each = n_obs_per_group)),
    strat_var = factor(rep(c("A", "B"), length.out = n_obs_total))
  )
  randomization_strata <- "strat_var"
  result <- expect_silent(h_unbiased_means_across_strata(
    residuals_per_group = residuals_per_group,
    df = df,
    randomization_strata = randomization_strata
  ))
  expect_true(result)
})

test_that("h_unbiased_means_across_strata works for non-zero residuals", {
  n_obs_per_group <- 10
  n_obs_total <- n_obs_per_group * 2

  residuals_per_group <- list(
    group1 = rep(0.1, n_obs_per_group),
    group2 = rep(0, n_obs_per_group)
  )
  df <- data.frame(
    treatment = factor(rep(c("group1", "group2"), each = n_obs_per_group)),
    strat_var = factor(rep(c("A", "B"), length.out = n_obs_total))
  )
  randomization_strata <- "strat_var"
  result <- expect_silent(h_unbiased_means_across_strata(
    residuals_per_group = residuals_per_group,
    df = df,
    randomization_strata = randomization_strata
  ))
  expect_false(result)
})

test_that("h_unbiased_means_across_strata works if one stratum just has one patient", {
  n_obs_per_group <- 10
  n_obs_total <- n_obs_per_group * 2

  residuals_per_group <- list(
    group1 = rep(0.1, n_obs_per_group),
    group2 = rep(0, n_obs_per_group)
  )
  df <- data.frame(
    treatment = factor(rep(c("group1", "group2"), each = n_obs_per_group)),
    # Stratum C just contains a single patient.
    strat_var = factor(c(rep(c("A", "B"), length.out = n_obs_total - 1), "C"))
  )
  randomization_strata <- "strat_var"
  result <- expect_silent(h_unbiased_means_across_strata(
    residuals_per_group = residuals_per_group,
    df = df,
    randomization_strata = randomization_strata
  ))
  expect_false(result)
})
