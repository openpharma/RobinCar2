test_that("predict_counterfactual works for guassian", {
  expect_snapshot(predict_counterfactual(fit_glm, "treatment"))
})

test_that("predict_counterfactual works for guassian with lm", {
  expect_snapshot(predict_counterfactual(fit_lm, "treatment", data = dummy_data))
})

test_that("predict_counterfactual works for binomial", {
  expect_snapshot(predict_counterfactual(fit_binom, "treatment"))
})

test_that("predict_counterfactual works if contrast are non-standard", {
  dummy_data2 <- dummy_data
  dummy_data2$s1 <- as.ordered(dummy_data2$s1)
  fit <- glm(y_b ~ treatment * s1, data = dummy_data2, family = binomial())
  expect_silent(pc <- predict_counterfactual(fit, treatment ~ 1, data = find_data(fit)))
  expect_snapshot(pc)
  newdf <- rbind(dummy_data2, dummy_data2, dummy_data2)
  newdf$treatment <- rep(factor(levels(dummy_data2$treatment), levels(dummy_data2$treatment)), each = nrow(dummy_data2))
  expect_identical(
    attr(pc, "predictions_linear")[, 1],
    predict(fit, newdata = newdf, type = "link")
  )
})
