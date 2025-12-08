test_that("print method for surv_effect works as expected (unstratified, unadjusted)", {
  x <- robin_surv(
    formula = Surv(time, status) ~ 1,
    data = surv_data,
    treatment = sex ~ sr(1)
  )
  expect_snapshot(print(x))
})

test_that("print method for surv_effect works as expected (stratified, unadjusted)", {
  x <- robin_surv(
    formula = Surv(time, status) ~ 1 + strata(strata),
    data = surv_data,
    treatment = sex ~ sr(1)
  )
  expect_snapshot(print(x))
})

test_that("print method for surv_effect works as expected (unstratified, adjusted)", {
  x <- robin_surv(
    formula = Surv(time, status) ~ meal.cal + age,
    data = surv_data,
    treatment = sex ~ sr(1)
  )
  expect_snapshot(print(x))
})

test_that("print method for surv_effect works as expected (stratified, adjusted)", {
  x <- robin_surv(
    formula = Surv(time, status) ~ meal.cal + age + strata(strata),
    data = surv_data,
    treatment = sex ~ sr(1)
  )
  expect_snapshot(print(x))
})

test_that("print method for surv_effect works as expected when no hazard ratio was estimated", {
  x <- robin_surv(
    formula = Surv(time, status) ~ meal.cal + age + strata(strata),
    data = surv_data,
    treatment = sex ~ sr(1),
    contrast = "none"
  )
  expect_snapshot(print(x))
})

test_that("table method for surv_effect works as expected", {
  x <- robin_surv(
    formula = Surv(time, status) ~ meal.cal + age + strata(strata),
    data = surv_data,
    treatment = sex ~ sr(1)
  )
  expect_snapshot(result <- table(x))
  expect_identical(result, x$events_table)
})

test_that("confint method for surv_effect works as expected", {
  x <- robin_surv(
    formula = Surv(time, status) ~ meal.cal + age + strata(strata),
    data = surv_data,
    treatment = sex ~ sr(1)
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

test_that("confint method returns error when no hazard ratio was estimated", {
  x <- robin_surv(
    formula = Surv(time, status) ~ meal.cal + age,
    data = surv_data,
    treatment = sex ~ sr(1),
    contrast = "none"
  )
  expect_error(
    confint(x),
    "No contrast was estimated; confidence interval is not available.",
    fixed = TRUE
  )
})
