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
})
