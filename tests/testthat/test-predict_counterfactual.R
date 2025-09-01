test_that("predict_counterfactual works for guassian", {
  expect_snapshot(predict_counterfactual(fit_glm, treatment ~ 1))
})

test_that("predict_counterfactual works for guassian with lm", {
  expect_snapshot(predict_counterfactual(fit_lm, treatment ~ 1, data = glm_data))
})

test_that("predict_counterfactual works for binomial", {
  expect_snapshot(predict_counterfactual(fit_binom, treatment ~ 1))
})

test_that("predict_counterfactual for negative binomial", {
  fit <- glm(y_b ~ treatment * covar, data = glm_data, family = negative.binomial(theta = 1))
  pc <- predict_counterfactual(fit, treatment ~ 1, glm_data)
  predictions <- pc$predictions

  # Check that the mean of the predicted outcomes within
  # each treatment group matches with the observed outcomes
  trt_levels <- levels(glm_data$treatment)
  for (i in seq_along(trt_levels)) {
    idx <- glm_data$treatment == trt_levels[i]
    # mean of the predicted outcomes within treatment group i
    pred_mean <- mean(predictions[idx, i])
    # mean of the y's within treatment group i
    obs_mean <- mean(glm_data[idx, ][["y_b"]])
    expect_equal(pred_mean, obs_mean, tolerance = 1e-15)
  }

  # check that the mean of the residuals is zero within a treatment group
  # this test exists because previously residual and predictions attributes
  # were not aligned properly when predictions were biased
  residuals <- pc$residual
  res_mean <- mean(residuals[idx])
  expect_equal(res_mean, 0, tolerance = 1e-15)
})

test_that("predict_counterfactual works if contrast are non-standard", {
  glm_data2 <- glm_data
  glm_data2$s1 <- as.ordered(glm_data2$s1)
  fit <- glm(y_b ~ treatment * s1, data = glm_data2, family = binomial())
  expect_silent(pc <- predict_counterfactual(fit, treatment ~ 1, data = find_data(fit)))
  expect_snapshot(pc)
  newdf <- rbind(glm_data2, glm_data2, glm_data2)
  newdf$treatment <- rep(factor(levels(glm_data2$treatment), levels(glm_data2$treatment)), each = nrow(glm_data2))
  expect_identical(
    pc$predictions_linear[, 1],
    predict(fit, newdata = newdf, type = "link")
  )
})

test_that("predict_counterfactual works with provided vcov function", {
  vcov_dummy <- function(x) {
    n <- length(x$estimate)
    matrix(0.1, nrow = n, ncol = n)
  }
  expect_snapshot(predict_counterfactual(fit_lm, treatment ~ 1, data = glm_data, vcov = vcov_dummy))
  expect_snapshot(predict_counterfactual(fit_lm, treatment ~ 1, data = glm_data, vcov = NULL))
})

test_that("confint method for prediction_cf works as expected", {
  expect_snapshot(
    confint(predict_counterfactual(fit_binom, treatment = treatment ~ s1, eff_measure = h_diff))
  )
})
