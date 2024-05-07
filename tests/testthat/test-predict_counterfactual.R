test_that("predict_counterfactual works for guassian", {
  expect_snapshot(predict_counterfactual(fit_glm, "Species"))
})

test_that("predict_counterfactual works for guassian with lm", {
  expect_snapshot(predict_counterfactual(fit_lm, "Species", data = iris))
})

test_that("predict_counterfactual works for binomial", {
  expect_snapshot(predict_counterfactual(fit_binom, "supp"))
})
