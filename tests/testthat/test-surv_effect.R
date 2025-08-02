test_that("print method for surv_effect works as expected", {
  x <- robin_surv(
    formula = Surv(time, status) ~ sex * strata + meal.cal + age,
    data = surv_data,
    treatment = sex ~ strata
  )
  expect_snapshot(print(x))
})

test_that("table method for surv_effect works as expected", {
  x <- robin_surv(
    formula = Surv(time, status) ~ sex * strata + meal.cal + age,
    data = surv_data,
    treatment = sex ~ strata
  )
  expect_snapshot(result <- table(x))
  expect_identical(result, x$events_table)
})
