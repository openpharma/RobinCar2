test_that("vcovHC works", {
  pc <- predict_counterfactual(fit_binom, treatment ~ s1)
  expect_snapshot(
    vcovHC(pc)
  )
  pc <- predict_counterfactual(fit_binom, treatment ~ pb(s1))
  expect_snapshot(
    vcovHC(pc)
  )
})

test_that("vcovG works", {
  pc <- predict_counterfactual(fit_binom, treatment ~ s1)
  expect_snapshot(
    vcovG(pc)
  )
  pc <- predict_counterfactual(fit_binom, treatment ~ 1)
  expect_snapshot(
    vcovG(pc)
  )
  pc <- predict_counterfactual(fit_binom, treatment ~ pb(s1))
  expect_snapshot(
    vcovG(pc)
  )
  expect_snapshot(
    vcovG(pc, decompose = FALSE)
  )
  pc <- predict_counterfactual(fit_binom, treatment ~ ps(s1))
  expect_snapshot(
    vcovG(pc)
  )
})
