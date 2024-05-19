test_that("bias works for guassian", {
  expect_snapshot(
    bias(residuals(fit_glm), treatment = dummy_data$treatment, group_idx = list(seq_len(nrow(dummy_data))))
  )
})
