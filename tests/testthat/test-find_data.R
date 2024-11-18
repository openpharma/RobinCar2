test_that("find_data works for glm", {
  expect_identical(
    find_data(fit_glm),
    fit_glm$data
  )
})

test_that("find_data fails for lm", {
  expect_error(
    find_data(fit_lm),
    "data must be provided explicitly for lm objects"
  )
})
