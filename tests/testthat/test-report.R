test_that("reporting function give info message when eff_measure is NULL", {
  expect_snapshot(
    predict_counterfactual(fit_lm, "treatment", dummy_data) |>
      vcovRobinCar.prediction_cf() |>
      report_vcov_robincar(digits = 3)
  )
})

test_that("reporting function works for gaussian fitted by lm", {
  expect_snapshot(
    predict_counterfactual(fit_lm, "treatment", dummy_data) |>
      vcovRobinCar.prediction_cf(eff_measure = "diff") |>
      report_vcov_robincar(digits = 3)
  )
})

test_that("reporting function works for guassian fitted by glm", {
  expect_snapshot(
    predict_counterfactual(fit_glm, "treatment", dummy_data) |>
      vcovRobinCar.prediction_cf(eff_measure = "diff") |>
      report_vcov_robincar(digits = 3)
  )
})

test_that("reporting function works for binomial fitted by glm", {
  expect_snapshot(
    predict_counterfactual(fit_binom, "treatment", dummy_data) |>
      vcovRobinCar.prediction_cf(eff_measure = "diff") |>
      report_vcov_robincar(digits = 3)
  )
  expect_snapshot(
    predict_counterfactual(fit_binom, "treatment", dummy_data) |>
      vcovRobinCar.prediction_cf(eff_measure = "risk ratio") |>
      report_vcov_robincar(digits = 3)
  )
  expect_snapshot(
    predict_counterfactual(fit_binom, "treatment", dummy_data) |>
      vcovRobinCar.prediction_cf(eff_measure = "odds ratio") |>
      report_vcov_robincar(digits = 3)
  )
})
