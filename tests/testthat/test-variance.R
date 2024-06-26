test_that("vcovHC works", {
  pc <- predict_counterfactual(fit_binom, treatment ~ s1)
  expect_snapshot(
    vcovHC(pc)
  )
})

test_that("vcovANHECOVA works", {
  pc <- predict_counterfactual(fit_binom, treatment ~ s1)
  expect_snapshot(
    vcovANHECOVA(pc)
  )
  expect_snapshot(
    vcovANHECOVA(pc, randomization = "permute_block")
  )
})
