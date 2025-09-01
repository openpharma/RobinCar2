test_that("print method for surv_effect works as expected", {
  x <- robin_surv(
    formula = Surv(time, status) ~ meal.cal + age,
    data = surv_data,
    treatment = sex ~ strata
  )
  expect_snapshot(print(x))
})

test_that("table method for surv_effect works as expected", {
  x <- robin_surv(
    formula = Surv(time, status) ~ meal.cal + age,
    data = surv_data,
    treatment = sex ~ strata
  )
  expect_snapshot(result <- table(x))
  expect_identical(result, x$events_table)
})

test_that("confint method for surv_effect works as expected", {
  x <- robin_surv(
    formula = Surv(time, status) ~ meal.cal + age,
    data = surv_data,
    treatment = sex ~ strata
  )
  expect_snapshot(result <- confint(x))
  expect_snapshot(
    expect_message(
      result <- confint(x, transform = exp),
      "The confidence interval is transformed.",
      fixed = TRUE
    )
  )
})
