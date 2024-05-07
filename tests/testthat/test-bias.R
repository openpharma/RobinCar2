test_that("bias works for guassian", {
  expect_snapshot(bias(fit_lm, list(treatment = "Species"), iris))
})

test_that("predict_counterfactual works for guassian with lm", {
  expect_snapshot(bias(fit_glm, list(treatment = "Species"), data = iris))
})

test_that("predict_counterfactual works for binomial", {
  expect_snapshot(bias(fit_binom, list(treatment = "supp")))
})
