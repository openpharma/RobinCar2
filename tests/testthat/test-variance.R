test_that("vcovHC works", {
  pc <- predict_counterfactual(fit_binom, treatment ~ s1)
  expect_snapshot(
    vcovHC(pc)
  )
})

test_that("gvcov works", {
  pc <- predict_counterfactual(fit_binom, treatment ~ s1)
  expect_snapshot(
    gvcov(pc)
  )
  expect_snapshot(
    gvcov(pc, randomization = "permute_block")
  )
})
