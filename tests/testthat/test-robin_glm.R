# h_interaction ----

test_that("h_interaction works correctly", {
  expect_false(h_interaction(y ~ trt + z, treatment = trt ~ x))
  expect_true(h_interaction(y ~ trt:z, treatment = trt ~ x))
  expect_true(h_interaction(trt * y ~ trt:z, treatment = trt ~ x))
  expect_true(h_interaction(y ~ trt:z, treatment = "trt"))
})

# robin_glm ----

test_that("robin_glm works correctly", {
  expect_silent(robin_glm(y ~ treatment * s1, data = dummy_data, treatment = treatment ~ s1, contrast = "difference", vcov = vcovHC))
  expect_silent(robin_glm(y_b ~ treatment * s1, data = dummy_data, treatment = treatment ~ s1, contrast = "difference"))
  expect_silent(robin_glm(y_b ~ treatment * s1, data = dummy_data, treatment = treatment ~ s1, contrast = "risk_ratio"))
  expect_silent(robin_glm(y_b ~ treatment * s1, data = dummy_data, treatment = treatment ~ s1, contrast = "odds_ratio"))
  expect_error(
    robin_glm(y_b ~ treatment * s1, data = dummy_data, treatment = treatment ~ s1, contrast = "odds_ratio", vcov = vcovHC),
    "Huber-White standard error only works for difference contrasts in models without interaction term."
  )
  expect_silent(robin_glm(y_b ~ treatment * s1, data = dummy_data, treatment = treatment ~ s1, contrast = h_diff))
})
