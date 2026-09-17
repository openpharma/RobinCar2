# h_interaction ----

test_that("h_interaction works correctly", {
  expect_false(h_interaction(y ~ trt + z, treatment = trt ~ x))
  expect_true(h_interaction(y ~ trt:z, treatment = trt ~ x))
  expect_true(h_interaction(trt * y ~ trt:z, treatment = trt ~ x))
  expect_true(h_interaction(y ~ trt:z, treatment = trt ~ 1))
})

# robin_lm ----

test_that("robin_lm works correctly", {
  expect_silent(
    robin_lm(
      y ~ treatment * s1,
      data = glm_data,
      treatment = treatment ~ s1,
    )
  )
  expect_error(
    robin_lm(
      y ~ treatment * s1,
      data = glm_data,
      treatment = treatment ~ s1,
      vcov = "vcovHC"
    ),
    "Huber-White variance estimator is ONLY supported when using a linear model
      without treatment-covariate interactions; see the 2023 FDA guidance."
  )
  expect_snapshot(
    robin_lm(y_b ~ treatment + s1, data = glm_data, treatment = treatment ~ s1)
  )
})


test_that("robin_lm give same result as robin_glm", {
  expect_silent(
    f1 <- robin_lm(
      y ~ treatment * s1,
      data = glm_data,
      treatment = treatment ~ pb(s1),
    )
  )
  expect_silent(
    f2 <- robin_glm(
      y ~ treatment * s1,
      data = glm_data,
      treatment = treatment ~ pb(s1),
    )
  )
  expect_equal(
    f1$marginal_mean$estimate,
    f2$marginal_mean$estimate
  )
  expect_equal(
    f1$contrast$estimate,
    f2$contrast$estimate
  )

  expect_silent(
    f1 <- robin_lm(
      y ~ treatment + s1,
      data = glm_data,
      treatment = treatment ~ pb(s1),
      vcov = vcovHC
    )
  )
  expect_silent(
    f2 <- robin_glm(
      y ~ treatment + s1,
      data = glm_data,
      treatment = treatment ~ pb(s1),
      vcov = vcovHC
    )
  )
  expect_equal(
    f1$marginal_mean$estimate,
    f2$marginal_mean$estimate
  )
  expect_equal(
    f1$contrast$estimate,
    f2$contrast$estimate
  )
})

test_that("robin_lm rejects models carrying an offset", {
  offset_data <- glm_data
  set.seed(123)
  offset_data$os <- log(runif(nrow(offset_data), 0.5, 3))

  expect_error(
    robin_lm(y ~ treatment * s1 + offset(os), data = offset_data, treatment = treatment ~ s1),
    "Models with an offset are not supported"
  )
})

test_that("the response shift advised for offset models works and is exact", {
  set.seed(11)
  shift_data <- glm_data
  shift_data$z <- log(runif(nrow(shift_data), 0.5, 3))

  # the literal form the offset rejection message tells users to write
  res <- robin_lm(I(y - z) ~ treatment * s1, data = shift_data, treatment = treatment ~ ps(s1))

  # g-computation on the offset model it replaces
  fit_offset <- lm(y ~ treatment * s1 + offset(z), data = shift_data)
  lvls <- levels(shift_data$treatment)
  preds <- sapply(lvls, function(a) {
    newdata <- shift_data
    newdata$treatment <- factor(a, lvls)
    predict(fit_offset, newdata = newdata)
  })
  group_idx <- split(seq_len(nrow(shift_data)), shift_data$s1)
  mm_offset <- colMeans(preds + bias(shift_data$y - fitted(fit_offset), shift_data$treatment, group_idx))

  # the two promises the message makes
  expect_equal(
    unname(mm_offset - res$marginal_mean$estimate),
    rep(mean(shift_data$z), length(lvls))
  )
  expect_equal(
    unname(res$contrast$estimate[1:2]),
    unname((mm_offset - mm_offset[1])[-1])
  )
})
