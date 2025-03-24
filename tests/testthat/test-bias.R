test_that("bias works for guassian", {
  testthat::expect_snapshot_value(
    bias(residuals(fit_glm), treatment = dummy_data$treatment, group_idx = list(seq_len(nrow(dummy_data)))),
    style = "deparse", tolerance = 1e-4
  )
})
