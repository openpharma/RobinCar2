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
