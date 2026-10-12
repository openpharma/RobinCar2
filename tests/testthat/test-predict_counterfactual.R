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


test_that("predict_counterfactual works for treatment factor levels in non-alphabetical order", {
  relabel_dat <- glm_data
  levels(relabel_dat$treatment)[1] <- "trtpbo"

  fit_glm_relabel <- glm(y ~ treatment * s1 + covar, data = relabel_dat)
  expected_result <- predict_counterfactual(fit_glm, treatment ~ 1)

  result_relabel <- predict_counterfactual(fit_glm_relabel, treatment ~ 1, data = relabel_dat)

  expected <- expected_result$estimate
  names(expected)[1] <- "trtpbo"
  expect_equal(expected, result_relabel$estimate)
})

test_that("predict_counterfactual rejects fits carrying an offset", {
  # exposure time, supplied both as offset() in the formula and as the offset argument
  offset_data <- glm_data
  set.seed(123)
  offset_data$expo <- runif(nrow(offset_data), 0.5, 3)
  offset_data$os <- log(offset_data$expo)
  offset_data$y_count <- rpois(nrow(offset_data), offset_data$expo * 2)

  fits <- list(
    lm_formula = lm(y ~ treatment * s1 + offset(os), data = offset_data),
    lm_argument = lm(y ~ treatment * s1, offset = os, data = offset_data),
    glm_formula = glm(y_count ~ treatment * s1 + offset(os), family = poisson(), data = offset_data),
    glm_argument = glm(y_count ~ treatment * s1, family = poisson(), offset = os, data = offset_data),
    negbin_formula = suppressWarnings(MASS::glm.nb(y_count ~ treatment * s1 + offset(os), data = offset_data))
  )
  for (nm in names(fits)) {
    expect_error(
      predict_counterfactual(fits[[nm]], treatment ~ s1, data = offset_data),
      "Models with an offset are not supported",
      info = nm
    )
  }
})

test_that("the offset rejection advises a response shift only where that is exact", {
  offset_data <- glm_data
  set.seed(123)
  offset_data$expo <- runif(nrow(offset_data), 0.5, 3)
  offset_data$os <- log(offset_data$expo)
  offset_data$y_count <- rpois(nrow(offset_data), offset_data$expo * 2)

  # gaussian identity link: the offset is a known additive shift, so the shift is exact
  expect_error(
    predict_counterfactual(
      lm(y ~ treatment * s1 + offset(os), data = offset_data),
      treatment ~ s1,
      data = offset_data
    ),
    "Fit the shifted response instead"
  )
  # non-identity link: no restatement exists
  expect_error(
    predict_counterfactual(
      glm(y_count ~ treatment * s1 + offset(os), family = poisson(), data = offset_data),
      treatment ~ s1,
      data = offset_data
    ),
    "no established covariate-adjusted rate estimand"
  )
  # identity link but non-gaussian family: shifted values need not be in the response support,
  # so the response-shift advice must not be given here
  poisson_identity <- glm(
    y_count ~ treatment + offset(os),
    family = poisson(link = "identity"),
    data = offset_data,
    start = c(1, 0, 0)
  )
  expect_error(
    predict_counterfactual(poisson_identity, treatment ~ s1, data = offset_data),
    "no established covariate-adjusted rate estimand"
  )
})

test_that("predict_counterfactual is unaffected for fits without an offset", {
  expect_silent(predict_counterfactual(fit_lm, treatment ~ 1, data = glm_data))
  expect_silent(predict_counterfactual(fit_glm, treatment ~ 1))
  expect_silent(predict_counterfactual(fit_binom, treatment ~ 1))
})
