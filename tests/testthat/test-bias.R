test_that("bias works for guassian", {
  expect_snapshot(bias(residuals(fit_glm), treatment = "Species", strata = NULL, iris))
})
