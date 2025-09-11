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

test_that("h_log_hr_est_via_score does not give spurious warning", {
  result <- expect_silent(h_log_hr_est_via_score(
    h_lr_score_strat_cov,
    df = na.omit(surv_data),
    treatment = "ecog",
    time = "time",
    status = "status",
    strata = "sex",
    model = ~ age + meal.cal + wt.loss
  ))
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
    formula = survival::Surv(time, status) ~ 1,
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
    formula = survival::Surv(time, status) ~ 1,
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
  input <- h_prep_survival_input(
    formula = survival::Surv(time, status) ~ 1,
    data = surv_data,
    treatment = ecog ~ 1
  )
  input$data <- na.omit(input$data)
  result <- robin_surv_no_strata_no_cov(
    vars = input,
    data = input$data,
    exp_level = 2,
    control_level = 1
  )
  # These values are extracted from RobinCar (version 1.0.0) results, see
  # `tests-raw/test-survival.R`.
  robincar_result <- list(
    test_stat = -0.6188324,
    test_sigma_l2 = 0.1782103,
    estimate = -0.1131005,
    se = 0.1830198
  )
  expect_equal(result$test_stat, robincar_result$test_stat, tolerance = 1e-5)
  expect_equal(result$test_sigma_l2, robincar_result$test_sigma_l2, tolerance = 1e-4)
  expect_equal(result$estimate, robincar_result$estimate, tolerance = 1e-4)
  expect_equal(result$se, robincar_result$se, tolerance = 1e-4)
})

test_that("robin_surv_strata works as expected", {
  input <- h_prep_survival_input(
    formula = survival::Surv(time, status) ~ 1,
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

test_that("robin_surv_strata works with multiple strata variables", {
  input <- h_prep_survival_input(
    formula = survival::Surv(time, status) ~ 1,
    data = surv_data,
    treatment = sex ~ strata + ecog
  )
  result <- robin_surv_strata(
    vars = input,
    data = input$data,
    exp_level = 1,
    control_level = 2
  )
  surv_data$strata_ecog <- interaction(surv_data$strata, surv_data$ecog, drop = TRUE)
  input2 <- h_prep_survival_input(
    formula = survival::Surv(time, status) ~ 1,
    data = surv_data,
    treatment = sex ~ strata_ecog
  )
  result2 <- robin_surv_strata(
    vars = input2,
    data = input2$data,
    exp_level = 1,
    control_level = 2
  )
  expect_equal(result, result2, ignore_formula_env = TRUE)
})

test_that("robin_surv_strata gives the same results as RobinCar functions", {
  input <- h_prep_survival_input(
    formula = survival::Surv(time, status) ~ 1,
    data = surv_data,
    treatment = ecog ~ sex
  )
  input$data <- na.omit(input$data)
  result <- robin_surv_strata(
    vars = input,
    data = input$data,
    exp_level = 2,
    control_level = 1
  )
  # These values are extracted from RobinCar (version 1.0.0) results, see
  # `tests-raw/test-survival.R`.
  robincar_result <- list(
    test_stat = -0.6171326,
    test_sigma_l2 = 0.1749327,
    estimate = -0.1138251,
    se = 0.1847554
  )
  expect_equal(result$test_stat, robincar_result$test_stat, tolerance = 1e-5)
  expect_equal(result$test_sigma_l2, robincar_result$test_sigma_l2, tolerance = 1e-4)
  expect_equal(result$estimate, robincar_result$estimate, tolerance = 1e-3)
  expect_equal(result$se, robincar_result$se, tolerance = 1e-3)
})

test_that("robin_surv_cov works as expected", {
  input <- h_prep_survival_input(
    formula = survival::Surv(time, status) ~ age,
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
  input <- h_prep_survival_input(
    formula = survival::Surv(time, status) ~ age,
    data = surv_data,
    treatment = ecog ~ 1
  )
  input$data <- na.omit(input$data)
  result <- robin_surv_cov(
    vars = input,
    data = input$data,
    exp_level = 2,
    control_level = 1,
    hr_se_plugin_adjusted = FALSE # To get the exact match with RobinCar.
  )
  # These values are extracted from RobinCar (version 1.0.0) results, see
  # `tests-raw/test-survival.R`.
  robincar_result <- list(
    test_stat = -0.4309439,
    test_sigma_l2 = 0.175718,
    estimate = -0.07914235,
    se = 0.181807
  )
  expect_equal(result$test_stat, robincar_result$test_stat, tolerance = 1e-5)
  expect_equal(result$test_sigma_l2, robincar_result$test_sigma_l2, tolerance = 1e-4)
  expect_equal(result$estimate, robincar_result$estimate, tolerance = 1e-3)
  expect_equal(result$se, robincar_result$se, tolerance = 1e-3)
})

test_that("robin_surv_strata_cov works as expected", {
  input <- h_prep_survival_input(
    formula = survival::Surv(time, status) ~ age,
    data = surv_data,
    treatment = ecog ~ sex
  )
  result <- robin_surv_strata_cov(
    vars = input,
    data = input$data,
    exp_level = 2,
    control_level = 1
  )
  expect_snapshot_value(result, tolerance = 1e-4, style = "deparse")
})

test_that("robin_surv_strata_cov works with multiple strata variables", {
  input <- h_prep_survival_input(
    formula = survival::Surv(time, status) ~ age,
    data = surv_data,
    treatment = sex ~ strata + ecog
  )
  result <- robin_surv_strata_cov(
    vars = input,
    data = input$data,
    exp_level = 2,
    control_level = 1
  )
  surv_data$strata_ecog <- interaction(surv_data$strata, surv_data$ecog, drop = TRUE)
  input2 <- h_prep_survival_input(
    formula = survival::Surv(time, status) ~ age,
    data = surv_data,
    treatment = sex ~ strata_ecog
  )
  result2 <- robin_surv_strata_cov(
    vars = input2,
    data = input2$data,
    exp_level = 2,
    control_level = 1
  )
  expect_equal(result, result2, ignore_formula_env = TRUE)
})

test_that("robin_surv_strata_cov gives the same results as RobinCar functions", {
  input <- h_prep_survival_input(
    formula = survival::Surv(time, status) ~ age,
    data = surv_data,
    treatment = ecog ~ sex
  )
  input$data <- na.omit(input$data)
  result <- robin_surv_strata_cov(
    vars = input,
    data = input$data,
    exp_level = 2,
    control_level = 1,
    hr_se_plugin_adjusted = FALSE # To get the exact match with RobinCar.
  )
  # These values are extracted from RobinCar (version 1.0.0) results, see
  # `tests-raw/test-survival.R`.
  robincar_result <- list(
    test_stat = -0.4612828,
    test_sigma_l2 = 0.1734316,
    estimate = -0.08566379,
    se = 0.1840128
  )
  expect_equal(result$test_stat, robincar_result$test_stat, tolerance = 1e-5)
  expect_equal(result$test_sigma_l2, robincar_result$test_sigma_l2, tolerance = 1e-4)
  expect_equal(result$estimate, robincar_result$estimate, tolerance = 1e-3)
  expect_equal(result$se, robincar_result$se, tolerance = 1e-3)
})

test_that("h_log_hr_coef_mat works as expected", {
  x <- list(
    estimate = 0.5,
    se = 1,
    pair = structure(
      list(2L, 1L),
      levels = c("A", "B")
    )
  )
  result <- h_log_hr_coef_mat(x)
  expect_snapshot_value(result, tolerance = 1e-4, style = "deparse")
})

test_that("h_log_hr_coef_mat works as expected for multiple comparisons", {
  x <- list(
    estimate = c(0.5, 0.7, 0.9),
    se = c(1, 2, 3),
    pair = structure(
      list(c(2L, 1L, 3L), c(1L, 3L, 2L)),
      levels = c("A", "B", "C")
    )
  )
  result <- h_log_hr_coef_mat(x)
  expect_snapshot_value(result, tolerance = 1e-4, style = "serialize")
})

test_that("h_test_mat works as expected", {
  x <- list(
    test_stat = c(0.5, 0.7),
    p_value = c(0.05, 0.01),
    pair = structure(
      list(c(2L, 1L), c(1L, 2L)),
      levels = c("A", "B")
    )
  )
  result <- h_test_mat(x)
  expect_snapshot_value(result, tolerance = 1e-4, style = "deparse")
})

test_that("h_events_table works as expected with strata", {
  vars <- list(
    treatment = "sex",
    time = "time",
    status = "status",
    strata = "strata"
  )
  result <- h_events_table(surv_data, vars)
  expected <- data.frame(
    strata = structure(c(1L, 1L, 2L, 2L, 3L, 3L, 4L), levels = c("0", "1", "2", "3"), class = "factor"),
    sex = structure(c(1L, 2L, 1L, 2L, 1L, 2L, 2L), levels = c("Female", "Male"), class = "factor"),
    Patients = c(27L, 36L, 42L, 71L, 21L, 29L, 1L),
    Events = c(9L, 28L, 28L, 54L, 16L, 28L, 1L)
  )
  expect_identical(result, expected)
})

test_that("h_events_table works as expected with multiple strata", {
  vars <- list(
    treatment = "sex",
    time = "time",
    status = "status",
    strata = c("strata", "ecog")
  )
  result <- h_events_table(surv_data, vars)
  expected <- data.frame(
    strata = structure(c(1L, 1L, 3L, 3L, 4L, 2L, 2L), levels = c("0", "1", "2", "3"), class = "factor"),
    ecog = structure(c(1L, 1L, 1L, 1L, 1L, 2L, 2L), levels = c("0", "1"), class = "factor"),
    sex = structure(c(1L, 2L, 1L, 2L, 2L, 1L, 2L), levels = c("Female", "Male"), class = "factor"),
    Patients = c(27L, 36L, 21L, 29L, 1L, 42L, 71L),
    Events = c(9L, 28L, 16L, 28L, 1L, 28L, 54L)
  )
  expect_identical(result, expected)
})

test_that("h_events_table works as expected without strata", {
  vars <- list(
    treatment = "sex",
    time = "time",
    status = "status",
    strata = NULL
  )
  result <- h_events_table(surv_data, vars)
  expected <- data.frame(
    sex = structure(1:2, levels = c("Female", "Male"), class = "factor"),
    Patients = c(90L, 138L),
    Events = c(53L, 112L)
  )
  expect_identical(result, expected)
})

test_that("robin_surv works as expected without strata or covariates", {
  result <- robin_surv(
    Surv(time, status) ~ 1,
    data = surv_data,
    treatment = ecog ~ 1
  )
  expect_s3_class(result, "surv_effect")
  expect_snapshot_value(result$log_hr_coef_mat, tolerance = 1e-4, style = "deparse")
  expect_snapshot_value(result$test_mat, tolerance = 1e-4, style = "serialize")
  expect_snapshot_value(result$events_table, tolerance = 1e-4, style = "serialize")
})

test_that("robin_surv gives the same results as RobinCar functions without strata or covariates", {
  result <- robin_surv(
    Surv(time, status) ~ 1,
    data = na.omit(surv_data),
    treatment = ecog ~ 1
  )
  # These values are extracted from RobinCar (version 1.0.0) results, see
  # `tests-raw/test-survival.R`.
  robincar_result <- list(
    test_stat = -0.6188324,
    test_sigma_l2 = 0.1782103,
    test_p_val = 0.5360268,
    estimate = -0.1131005,
    se = 0.1830198
  )
  expect_equal(result$test_mat[, "Test Stat."], robincar_result$test_stat, tolerance = 1e-5)
  expect_equal(result$test_mat[, "Pr(>|z|)"], robincar_result$test_p_val, tolerance = 1e-5)
  expect_equal(result$log_hr_coef_mat[, "Estimate"], robincar_result$estimate, tolerance = 1e-4)
  expect_equal(result$log_hr_coef_mat[, "Std.Err"], robincar_result$se, tolerance = 1e-4)
})

test_that("robin_surv works as expected with strata", {
  result <- robin_surv(
    Surv(time, status) ~ 1,
    data = surv_data,
    treatment = sex ~ strata
  )
  expect_s3_class(result, "surv_effect")
  expect_snapshot_value(result$log_hr_coef_mat, tolerance = 1e-4, style = "deparse")
  expect_snapshot_value(result$test_mat, tolerance = 1e-4, style = "serialize")
  expect_snapshot_value(result$events_table, tolerance = 1e-4, style = "serialize")
})

test_that("robin_surv gives the same results as RobinCar functions with strata", {
  result <- robin_surv(
    Surv(time, status) ~ 1,
    data = na.omit(surv_data),
    treatment = ecog ~ sex
  )
  # These values are extracted from RobinCar (version 1.0.0) results, see
  # `tests-raw/test-survival.R`.
  robincar_result <- list(
    test_stat = -0.6171326,
    test_sigma_l2 = 0.1749327,
    test_p_val = 0.5371473,
    estimate = -0.1138251,
    se = 0.1847554
  )
  expect_equal(result$test_mat[, "Test Stat."], robincar_result$test_stat, tolerance = 1e-5)
  expect_equal(result$test_mat[, "Pr(>|z|)"], robincar_result$test_p_val, tolerance = 1e-5)
  expect_equal(result$log_hr_coef_mat[, "Estimate"], robincar_result$estimate, tolerance = 1e-3)
  expect_equal(result$log_hr_coef_mat[, "Std.Err"], robincar_result$se, tolerance = 1e-3)
})

test_that("robin_surv works as expected with covariates", {
  result <- robin_surv(
    Surv(time, status) ~ age,
    data = surv_data,
    treatment = ecog ~ 1
  )
  expect_s3_class(result, "surv_effect")
  expect_snapshot_value(result$log_hr_coef_mat, tolerance = 1e-4, style = "deparse")
  expect_snapshot_value(result$test_mat, tolerance = 1e-4, style = "serialize")
  expect_snapshot_value(result$events_table, tolerance = 1e-4, style = "serialize")
})

test_that("robin_surv gives the same results as RobinCar functions with covariates", {
  result <- robin_surv(
    Surv(time, status) ~ age,
    data = na.omit(surv_data),
    treatment = ecog ~ 1
  )
  # These values are extracted from RobinCar (version 1.0.0) results, see
  # `tests-raw/test-survival.R`.
  robincar_result <- list(
    test_stat = -0.4309439,
    test_sigma_l2 = 0.175718,
    test_p_val = 0.6665092,
    estimate = -0.07914235,
    se = 0.181807
  )
  expect_equal(result$test_mat[, "Test Stat."], robincar_result$test_stat, tolerance = 1e-5)
  expect_equal(result$test_mat[, "Pr(>|z|)"], robincar_result$test_p_val, tolerance = 1e-5)
  expect_equal(result$log_hr_coef_mat[, "Estimate"], robincar_result$estimate, tolerance = 1e-3)
  expect_equal(result$log_hr_coef_mat[, "Std.Err"], robincar_result$se, tolerance = 1e-3)
})

test_that("robin_surv gives the same results as RobinCar for single factor covariate", {
  result <- robin_surv(
    Surv(time, status) ~ sex,
    data = na.omit(surv_data),
    treatment = ecog ~ 1,
    hr_se_plugin_adjusted = FALSE
  )
  # These values are extracted from RobinCar (version 1.0.0) results, see
  # `tests-raw/test-survival.R`.
  robincar_result <- list(
    test_stat = -0.7582934,
    test_sigma_l2 = 0.1715611,
    test_p_val = 0.4482754,
    estimate = -0.135294,
    se = 0.1797889
  )
  expect_equal(result$test_mat[, "Test Stat."], robincar_result$test_stat, tolerance = 1e-4)
  expect_equal(result$test_mat[, "Pr(>|z|)"], robincar_result$test_p_val, tolerance = 1e-4)
  expect_equal(result$log_hr_coef_mat[, "Estimate"], robincar_result$estimate, tolerance = 1e-1)
  expect_equal(result$log_hr_coef_mat[, "Std.Err"], robincar_result$se, tolerance = 1e-3)
})

test_that("robin_surv works as expected with strata and covariates", {
  result <- robin_surv(
    Surv(time, status) ~ age + ph.karno,
    data = surv_data,
    treatment = ecog ~ sex
  )
  expect_s3_class(result, "surv_effect")
  expect_snapshot_value(result$log_hr_coef_mat, tolerance = 1e-4, style = "deparse")
  expect_snapshot_value(result$test_mat, tolerance = 1e-4, style = "serialize")
  expect_snapshot_value(result$events_table, tolerance = 1e-4, style = "serialize")
})

test_that("robin_surv gives the same results as RobinCar functions with strata and covariates", {
  result <- robin_surv(
    Surv(time, status) ~ age,
    data = na.omit(surv_data),
    treatment = ecog ~ sex
  )
  # These values are extracted from RobinCar (version 1.0.0) results, see
  # `tests-raw/test-survival.R`.
  robincar_result <- list(
    test_stat = -0.4612828,
    test_sigma_l2 = 0.1734316,
    test_p_val = 0.6445957,
    estimate = -0.08566379,
    se = 0.1840128
  )
  expect_equal(result$test_mat[, "Test Stat."], robincar_result$test_stat, tolerance = 1e-5)
  expect_equal(result$test_mat[, "Pr(>|z|)"], robincar_result$test_p_val, tolerance = 1e-5)
  expect_equal(result$log_hr_coef_mat[, "Estimate"], robincar_result$estimate, tolerance = 1e-3)
  expect_equal(result$log_hr_coef_mat[, "Std.Err"], robincar_result$se, tolerance = 1e-3)
})

test_that("robin_surv also works with multiple pairwise comparisons", {
  result <- robin_surv(
    Surv(time, status) ~ 1,
    data = surv_data,
    treatment = strata ~ 1
  )
  expect_s3_class(result, "surv_effect")
  comparisons <- c("1 v.s. 0", "2 v.s. 0", "3 v.s. 0", "2 v.s. 1", "3 v.s. 1", "3 v.s. 2")
  expect_matrix(result$log_hr_coef_mat, ncol = 4, nrow = 6)
  expect_names(rownames(result$log_hr_coef_mat), identical.to = comparisons)
  expect_matrix(result$test_mat, ncol = 2, nrow = 6)
  expect_names(rownames(result$test_mat), identical.to = comparisons)
})

test_that("robin_surv allows the user to optionally define the comparisons of interest", {
  result <- robin_surv(
    Surv(time, status) ~ 1,
    data = surv_data,
    treatment = strata ~ 1,
    comparisons = list(c(1, 2), c(3, 3))
  )
  expect_s3_class(result, "surv_effect")
  comparisons <- c("0 v.s. 2", "1 v.s. 2")
  expect_matrix(result$log_hr_coef_mat, ncol = 4, nrow = 2)
  expect_names(rownames(result$log_hr_coef_mat), identical.to = comparisons)
  expect_matrix(result$test_mat, ncol = 2, nrow = 2)
  expect_names(rownames(result$test_mat), identical.to = comparisons)
})

test_that("robin_surv allows to use unadjusted standard error", {
  result <- robin_surv(
    Surv(time, status) ~ age,
    data = surv_data,
    treatment = ecog ~ 1,
    hr_se_plugin_adjusted = FALSE
  )
  result_adjusted <- robin_surv(
    Surv(time, status) ~ age,
    data = surv_data,
    treatment = ecog ~ 1,
    hr_se_plugin_adjusted = TRUE
  )
  # Only the standard error should differ.
  expect_true(result$se != result_adjusted$se)
  expect_true(result$estimate == result_adjusted$estimate)
  expect_true(result$test_stat == result_adjusted$test_stat)
  expect_true(result$p_value == result_adjusted$p_value)
})

test_that("robin_surv gives the same results as RobinCar for strong correlation covariate with strata", {
  set.seed(2040)
  surv_data2 <- surv_data
  surv_data2$ecog <- as.factor(ifelse(
    surv_data$strata == 1,
    rbinom(nrow(surv_data), 1, prob = 0.2),
    surv_data$ecog
  ))
  surv_data2 <- surv_data2[surv_data2$strata %in% c(0, 1), ]

  result <- robin_surv(
    formula = Surv(time, status) ~ ecog,
    data = na.omit(surv_data2),
    treatment = sex ~ strata,
    hr_se_plugin_adjusted = FALSE
  )
  # These values are extracted from RobinCar (version 1.0.0) results, see
  # `tests-raw/test-survival.R`.
  robincar_result <- list(
    test_stat = 2.35142,
    test_sigma_l2 = 0.1589645,
    test_p_val = 0.01870189,
    estimate = 0.5466888,
    se = 0.2371971
  )
  expect_equal(result$test_mat[, "Test Stat."], robincar_result$test_stat, tolerance = 1e-4)
  expect_equal(result$test_mat[, "Pr(>|z|)"], robincar_result$test_p_val, tolerance = 1e-3)
  expect_equal(result$log_hr_coef_mat[, "Estimate"], robincar_result$estimate, tolerance = 1e-1)
  expect_equal(result$log_hr_coef_mat[, "Std.Err"], robincar_result$se, tolerance = 1e-2)
})
