test_that("bias works for guassian", {
  expect_snapshot(bias(residuals(fit), treatment = "Species", strata = NULL, iris))
})
