test_that("robincar convariance estimation works for guassian fitted by lm", {
  expect_snapshot(
    predict_counterfactual(fit_lm, "treatment", dummy_data) |> vcovRobinCar.prediction_cf()
  )

  expect_snapshot(
    predict_counterfactual(fit_lm, "treatment", dummy_data) |> vcovRobinCar.prediction_cf(eff_measure = "diff")
  )
})

test_that("robincar convariance estimation works for guassian fitted by glm", {
  expect_snapshot(
    predict_counterfactual(fit_glm, "treatment", dummy_data) |> vcovRobinCar.prediction_cf()
  )

  expect_snapshot(
    predict_counterfactual(fit_glm, "treatment", dummy_data) |> vcovRobinCar.prediction_cf(eff_measure = "diff")
  )
})

test_that("robincar convariance estimation works for binomial fitted by glm", {
  expect_snapshot(
    predict_counterfactual(fit_binom, "treatment", dummy_data) |> vcovRobinCar.prediction_cf()
  )
  expect_snapshot(
    predict_counterfactual(fit_binom, "treatment", dummy_data) |> vcovRobinCar.prediction_cf(eff_measure = "diff")
  )
  expect_snapshot(
    predict_counterfactual(fit_binom, "treatment", dummy_data) |> vcovRobinCar.prediction_cf(eff_measure = "risk ratio")
  )
  expect_snapshot(
    predict_counterfactual(fit_binom, "treatment", dummy_data) |> vcovRobinCar.prediction_cf(eff_measure = "odds ratio")
  )
})
