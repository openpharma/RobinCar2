test_that("h_get_vars works for single character", {
  res <- expect_silent(h_get_vars("abc"))
  expect_identical(res, list(treatment = "abc", strata = character(0)))

  expect_error(
    h_get_vars(c("abc", "def")),
    "length 1 character"
  )
  expect_error(
    h_get_vars(NULL),
    "length 1 character"
  )
})

test_that("h_get_vars works for formula", {
  res <- expect_silent(h_get_vars(a ~ b + c))
  expect_identical(res, list(treatment = "a", strata = c("b", "c")))

  res <- expect_silent(h_get_vars(`~`(a, )))
  expect_identical(res, list(treatment = "a", strata = character(0)))

  expect_error(
    h_get_vars(log(a) ~ strata(b)),
    "left hand side of the treatment formula should be a single name!"
  )

  expect_error(
    h_get_vars(a + b ~ strata(b)),
    "left hand side of the treatment formula should be a single name!"
  )

  res <- expect_silent(h_get_vars(a ~ strata(b)))
  expect_identical(res, list(treatment = "a", strata = "b"))
})
