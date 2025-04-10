test_that("h_diff works as expected", {
  x <- c(1, 3, 2)
  y <- c(2, 4, 4)
  expect_identical(
    h_diff(x, y),
    c(-1, -1, -2)
  )
  x <- c(1, 2)
  y <- c(0, 1)
  expect_identical(
    h_diff(x, y),
    c(1, 1)
  )
})

test_that("h_jac_diff work as expected", {
  x <- c(1, 3, 2)
  y <- c(2, 4, 4)
  expect_identical(
    h_jac_diff(x, y),
    matrix(c(1, -1), nrow = 3, ncol = 2, byrow = TRUE),
    tolerance = 1e-8
  )
  x <- c(1, 2)
  y <- c(0, 1)
  expect_identical(
    h_jac_diff(x, y),
    matrix(c(1, -1), nrow = 2, ncol = 2, byrow = TRUE),
    tolerance = 1e-8
  )
})

test_that("h_ratio works as expected", {
  x <- c(1, 3, 2)
  y <- c(2, 3, 1)
  expect_identical(
    h_ratio(x, y),
    c(1 / 2, 1, 2)
  )
  x <- c(1, 2)
  y <- c(2, 4)
  expect_identical(
    h_ratio(x, y),
    c(1 / 2, 1 / 2)
  )
})

test_that("h_jac_ratio work as expected", {
  x <- c(0.2, 0.5, 0.6)
  y <- c(0.4, 0.2, 0.3)
  expect_identical(
    h_jac_ratio(x, y),
    matrix(c(2.5, 5, 10 / 3, -1.25, -12.5, -20 / 3), nrow = 3),
    tolerance = 1e-8
  )
  x <- c(0.2, 0.5)
  y <- c(0.5, 0.2)
  expect_identical(
    h_jac_ratio(x, y),
    matrix(c(2, 5, -0.8, -12.5), nrow = 2),
    tolerance = 1e-8
  )
})

test_that("h_odds_ratio works as expected", {
  x <- c(0.5, 0.4, 0.6)
  y <- c(0.2, 0.4, 0.3)
  expect_identical(
    h_odds_ratio(x, y),
    c(4, 1, 3.5),
    tolerance = 1e-8
  )
  x <- c(0.5, 0.4)
  y <- c(0.4, 0.5)
  expect_identical(
    h_odds_ratio(x, y),
    c(1.5, 2 / 3),
    tolerance = 1e-8
  )
})

test_that("h_jac_odds_ratio work as expected", {
  x <- c(0.2, 0.5, 0.6)
  y <- c(0.5, 0.2, 0.6)
  expect_identical(
    h_jac_odds_ratio(x, y),
    matrix(c(1.5625, 16, 25 / 6, -1, -25, -25 / 6), nrow = 3),
    tolerance = 1e-8
  )
  x <- c(0.2, 0.5)
  y <- c(0.5, 0.2)
  expect_identical(
    h_jac_odds_ratio(x, y),
    matrix(c(1.5625, 16, -1, -25), nrow = 2),
    tolerance = 1e-8
  )
})

test_that("treatment_effect works as expected", {
  pc <- predict_counterfactual(fit_binom, treatment ~ s1)
  df <- expect_silent(difference(pc))
  rr <- expect_silent(risk_ratio(pc))
  or <- expect_silent(odds_ratio(pc))
  expect_identical(
    df$estimate,
    c(0.22459921, 0.26528993, 0.04069073),
    ignore_attr = TRUE,
    tolerance = 1e-6
  )
  expect_identical(
    df$pair[[1]],
    c(2L, 3L, 3L)
  )
  expect_identical(
    diag(df$variance),
    c(0.002276376, 0.002259492, 0.002298305),
    tolerance = 1e-6
  )
  expect_identical(
    rr$estimate,
    c(1.630726, 1.744994, 1.070072),
    ignore_attr = TRUE,
    tolerance = 1e-6
  )
  expect_identical(
    rr$pair[[1]],
    c(2L, 3L, 3L)
  )
  expect_identical(
    diag(rr$variance),
    c(0.032539074, 0.035867956, 0.007316219),
    tolerance = 1e-6
  )
  expect_identical(
    or$estimate,
    c(2.504219, 2.967691, 1.185076),
    ignore_attr = TRUE,
    tolerance = 1e-6
  )
  expect_identical(
    diag(or$variance),
    c(0.25578808, 0.36889840, 0.05635703),
    tolerance = 1e-6
  )
})

test_that("treatment_effect works as expected for custom contrast", {
  pc <- predict_counterfactual(fit_binom, treatment ~ s1)
  h_contrast <- function(x, y) {
    (x - y)^3
  }
  expect_silent(treatment_effect(pc, eff_measure = h_contrast))
})

test_that("treatment_effect works for lm/glm object", {
  expect_snapshot(treatment_effect(fit_binom, treatment = treatment ~ s1, eff_measure = h_diff))
  expect_snapshot(treatment_effect(fit_lm, treatment = treatment ~ s1, eff_measure = h_diff, data = dummy_data))
})

test_that("treatment_effect works if variance is not used", {
  expect_snapshot(treatment_effect(fit_binom, treatment = treatment ~ s1, eff_measure = h_diff, variance = NULL))
})

test_that("treatment_effect works if pair is defined", {
  expect_snapshot(
    treatment_effect(
      fit_binom,
      pair = against_ref(c("pbo", "trt1", "trt2")),
      treatment = treatment ~ s1, eff_measure = h_diff
    )
  )
})
