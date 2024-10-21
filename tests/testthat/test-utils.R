test_that("h_get_vars works for formula", {
  res <- expect_silent(h_get_vars(abc ~ 1))
  expect_identical(res, list(treatment = "abc", schema = "sp", strata = character(0)))

  expect_error(
    h_get_vars("treatment"),
    "Must be a formula, not character"
  )
  expect_error(
    h_get_vars(trt ~ pb(s1) + ps(s2)),
    "only one randomization schema is allowed!"
  )
  expect_error(
    h_get_vars(~ ps(s2)),
    "treatment formula must be of type treatment ~ strata"
  )
  expect_error(
    h_get_vars(NULL),
    "Must be a formula, not 'NULL'"
  )
  expect_error(
    h_get_vars(log(a) ~ b),
    "left hand side of the treatment formula should be a single name!"
  )
  expect_error(
    h_get_vars(a + b ~ strata(b)),
    "left hand side of the treatment formula should be a single name!"
  )
})

test_that("h_get_vars works for formula with schemas", {
  res <- expect_silent(h_get_vars(a ~ b + c))
  expect_identical(res, list(treatment = "a", schema = "sp", strata = c("b", "c")))

  res <- expect_silent(h_get_vars(a ~ pb(1) + b))
  expect_identical(res, list(treatment = "a", schema = "pb", strata = "b"))

  res <- expect_silent(h_get_vars(a ~ ps(b) + c))
  expect_identical(res, list(treatment = "a", schema = "ps", strata = c("b", "c")))

  res <- expect_silent(h_get_vars(a ~ strata(b)))
  expect_identical(res, list(treatment = "a", schema = "sp", strata = "b"))
})
