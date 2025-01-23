test_that("h_diff works as expected", {
  x <- c(1, 3, 2)
  expect_identical(
    h_diff(x),
    c(2, 1, -1)
  )
  x <- c(1, 2)
  expect_identical(
    h_diff(x),
    c(1)
  )
})

test_that("h_jac_diff work as expected", {
  x <- c(1, 3, 2)
  expect_identical(
    h_jac_diff(x),
    matrix(c(-1, -1, 0, 1, 0, -1, 0, 1, 1), nrow = 3),
    tolerance = 1e-8
  )
  x <- c(1, 2)
  expect_identical(
    h_jac_diff(x),
    matrix(c(-1, 1), nrow = 1),
    tolerance = 1e-8
  )
})

test_that("h_ratio works as expected", {
  x <- c(1, 3, 2)
  expect_identical(
    h_ratio(x),
    c(3, 2, 2 / 3)
  )
  x <- c(1, 2)
  expect_identical(
    h_ratio(x),
    c(2)
  )
})

test_that("h_jac_ratio work as expected", {
  x <- c(0.2, 0.5, 0.6)
  expect_identical(
    h_jac_ratio(x),
    matrix(c(-12.5, -15, 0, 5, 0, -2.4, 0, 5, 2), nrow = 3),
    tolerance = 1e-8
  )
  x <- c(0.2, 0.5)
  expect_identical(
    h_jac_ratio(x),
    matrix(c(-12.5, 5), nrow = 1),
    tolerance = 1e-8
  )
})

test_that("h_odds_ratio works as expected", {
  x <- c(0.5, 0.4, 0.6)
  expect_identical(
    h_odds_ratio(x),
    c(2 / 3, 1.5, 2.25),
    tolerance = 1e-8
  )
  x <- c(0.5, 0.4)
  expect_identical(
    h_odds_ratio(x),
    2 / 3,
    tolerance = 1e-8
  )
})

test_that("h_jac_odds_ratio work as expected", {
  x <- c(0.2, 0.5, 0.6)
  expect_identical(
    h_jac_odds_ratio(x),
    matrix(c(-25, -37.5, 0, 16, 0, -6, 0, 25, 6.25), nrow = 3),
    tolerance = 1e-8
  )
  x <- c(0.2, 0.5)
  expect_identical(
    h_jac_odds_ratio(x),
    matrix(c(-25, 16), nrow = 1),
    tolerance = 1e-8
  )
})

test_that("treatment_effect works as expected", {
  pc <- predict_counterfactual(fit_binom, treatment ~ s1)
  df <- expect_silent(difference(pc))
  rr <- expect_silent(risk_ratio(pc))
  or <- expect_silent(odds_ratio(pc))
  expect_identical(
    df,
    c(0.22459921, 0.26528993, 0.04069073),
    ignore_attr = TRUE,
    tolerance = 1e-6
  )
  expect_identical(
    attr(df, "name"),
    c("trt1 v.s. pbo", "trt2 v.s. pbo", "trt2 v.s. trt1")
  )
  expect_identical(
    attr(df, "variance"),
    c(0.002276376, 0.002259492, 0.002298305),
    tolerance = 1e-6
  )
  expect_identical(
    rr,
    c(1.630726, 1.744994, 1.070072),
    ignore_attr = TRUE,
    tolerance = 1e-6
  )
  expect_identical(
    attr(rr, "name"),
    c("trt1 v.s. pbo", "trt2 v.s. pbo", "trt2 v.s. trt1")
  )
  expect_identical(
    attr(rr, "variance"),
    c(0.032539074, 0.035867956, 0.007316219),
    tolerance = 1e-6
  )
  expect_identical(
    or,
    c(2.504219, 2.967691, 1.185076),
    ignore_attr = TRUE,
    tolerance = 1e-6
  )
  expect_identical(
    attr(or, "name"),
    c("trt1 v.s. pbo", "trt2 v.s. pbo", "trt2 v.s. trt1")
  )
  expect_identical(
    attr(or, "variance"),
    c(0.25578808, 0.36889840, 0.05635703),
    tolerance = 1e-6
  )
})

test_that("treatment_effect works as expected for custom contrast", {
  pc <- predict_counterfactual(fit_binom, treatment ~ s1)
  h_contrast <- function(x) {
    v <- outer(x, x, `-`)
    v[lower.tri(v)]
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

test_that("treatment_effect works if pair is integer", {
  expect_snapshot(treatment_effect(fit_binom, pair = c(1, 2), treatment = treatment ~ s1, eff_measure = h_diff))
})
