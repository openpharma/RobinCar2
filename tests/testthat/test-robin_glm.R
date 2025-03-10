# robin_glm ----

test_that("robin_glm works correctly", {
  expect_silent(
    robin_glm(
      y ~ treatment * s1,
      data = dummy_data, treatment = treatment ~ s1,
      contrast = "difference"
    )
  )
  expect_silent(robin_glm(y_b ~ treatment * s1, data = dummy_data, treatment = treatment ~ s1, contrast = "difference"))
  expect_silent(robin_glm(y_b ~ treatment * s1, data = dummy_data, treatment = treatment ~ s1, contrast = "risk_ratio"))
  expect_silent(robin_glm(y_b ~ treatment * s1, data = dummy_data, treatment = treatment ~ s1, contrast = "odds_ratio"))
  expect_error(
    robin_glm(
      y_b ~ treatment * s1,
      data = dummy_data, treatment = treatment ~ s1,
      contrast = "odds_ratio", vcov = "vcovHC"
    ),
    "Huber-White variance estimator is ONLY"
  )
  expect_error(
    robin_glm(
      y_b ~ treatment * s1,
      data = dummy_data, treatment = treatment ~ s1,
      contrast = "difference", vcov = "vcovHC"
    ),
    "Huber-White variance estimator is ONLY"
  )
  expect_silent(robin_glm(y_b ~ treatment * s1, data = dummy_data, treatment = treatment ~ s1, contrast = h_diff))
})


test_that("robin_glm works for glm.nb", {
  dummy_data2 <- dummy_data
  dummy_data2$y_b <- rep(seq_len(10), nrow(dummy_data2) / 10)
  expect_silent(
    robin_glm(
      y_b ~ treatment * s1,
      data = dummy_data2, treatment = treatment ~ s1,
      family = MASS::negative.binomial(theta = NA),
      contrast = "difference"
    )
  )
})
