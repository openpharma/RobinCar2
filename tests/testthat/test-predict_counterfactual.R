test_that("predict_counterfactual works for guassian", {
  expect_snapshot(predict_counterfactual(fit_glm, "treatment"))
})

test_that("predict_counterfactual works for guassian with lm", {
  expect_snapshot(predict_counterfactual(fit_lm, "treatment", data = dummy_data))
})

test_that("predict_counterfactual works for binomial", {
  expect_snapshot(predict_counterfactual(fit_binom, "treatment"))
})
