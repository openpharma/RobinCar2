test_that("confint works correctly", {
  expect_snapshot(confint(robin_res1$marginal_mean))
  expect_snapshot(confint(robin_res1$contrast))

  expect_snapshot(confint(robin_res2$marginal_mean))
  expect_snapshot(confint(robin_res2$contrast))
})

test_that("confint works with parm argument", {
  expect_snapshot(confint(robin_res1$marginal_mean, parm = 1:2))
  expect_snapshot(confint(robin_res1$marginal_mean, parm = c("pbo", "trt2")))

  expect_snapshot(confint(robin_res1$contrast, parm = 1:2))
  expect_snapshot(confint(robin_res1$contrast, parm = c("trt1 v.s. pbo")))

  expect_error(confint(robin_res1$marginal_mean, parm = 4))
  expect_error(confint(robin_res1$marginal_mean, parm = "trt3"))
  expect_error(confint(robin_res1$contrast, parm = 4))
  expect_error(confint(robin_res1$contrast, parm = "trt v.s. pbo"))
})

test_that("confint works with level argument", {
  expect_snapshot(confint(robin_res1$marginal_mean, level = 0.8))
  expect_snapshot(confint(robin_res1$contrast, level = 0.7))
})

test_that("confint works with transform argument", {
  expect_snapshot(confint(robin_res1$contrast, transform = exp))
  expect_snapshot(confint(robin_res1$contrast, transform = identity))
})
