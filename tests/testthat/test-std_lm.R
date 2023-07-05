test_that("std_lm works", {
  expect_silent(std_lm(Sepal.Length ~ Species, data = iris))
})
