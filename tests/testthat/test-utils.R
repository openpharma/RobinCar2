test_that("h_get_vars works for formula", {
  res <- expect_silent(h_get_vars(abc ~ 1))
  expect_identical(res, list(treatment = "abc", schema = "sr", strata = character(0)))
  expect_identical(res, list(treatment = "abc", schema = "sr", strata = character(0)))

  expect_error(
    h_get_vars("treatment"),
    "Must be a formula, not character"
  )
  expect_error(
    h_get_vars(trt ~ pb(s1) + ps(s2)),
    "only one randomization schema is allowed!"
  )
  expect_error(
    h_get_vars(~ ps(s2)),
    "treatment formula must be of type treatment ~ strata"
  )
  expect_error(
    h_get_vars(NULL),
    "Must be a formula, not 'NULL'"
  )
  expect_error(
    h_get_vars(log(a) ~ b),
    "left hand side of the treatment formula should be a single name!"
  )
  expect_error(
    h_get_vars(a + b ~ strata(b)),
    "left hand side of the treatment formula should be a single name!"
  )
})

test_that("h_get_vars works for formula with schemas", {
  res <- expect_silent(h_get_vars(a ~ b + c))
  expect_identical(res, list(treatment = "a", schema = "sr", strata = c("b", "c")))

  res <- expect_silent(h_get_vars(a ~ pb(1) + b))
  expect_identical(res, list(treatment = "a", schema = "pb", strata = "b"))

  res <- expect_silent(h_get_vars(a ~ ps(b) + c))
  expect_identical(res, list(treatment = "a", schema = "ps", strata = c("b", "c")))

  res <- expect_silent(h_get_vars(a ~ strata(b)))
  expect_identical(res, list(treatment = "a", schema = "sr", strata = "b"))
})

test_that("h_get_vars is backwards compatible for use of sp instead of sr", {
  res <- expect_silent(h_get_vars(a ~ sp(b) + c))
  expect_identical(res, list(treatment = "a", schema = "sr", strata = c("b", "c")))
})

test_that("h_prep_survival_input works with strata", {
  result <- expect_silent(h_prep_survival_input(
    formula = survival::Surv(time, status) ~ age +
      ph.karno +
      meal.cal +
      strata(strata) +
      interaction(I(age > 50), I(ph.karno < 20)),
    data = surv_data,
    treatment = sex ~ pb(strata)
  ))
  expected <- list(
    data = surv_data,
    time = "time",
    status = "status",
    treatment = "sex",
    randomization_strata = "strata",
    strata = "strata",
    schema = "pb",
    covariates = c("age", "ph.karno", "meal.cal"),
    model = ~ age + ph.karno + meal.cal + interaction(I(age > 50), I(ph.karno < 20)),
    n_levels = 2L,
    levels = c("Female", "Male")
  )
  expect_equal(result, expected, ignore_formula_env = TRUE)
})

test_that("h_prep_survival_input works with multiple strata", {
  result <- expect_silent(h_prep_survival_input(
    formula = survival::Surv(time, status) ~ age + ph.karno + meal.cal + strata(strata, ecog),
    data = surv_data,
    treatment = sex ~ sr(1) # We do not necessarily need to have the strata in the randomization.
  ))
  expected <- list(
    data = surv_data,
    time = "time",
    status = "status",
    treatment = "sex",
    randomization_strata = character(),
    strata = c("strata", "ecog"),
    schema = "sr",
    covariates = c("age", "ph.karno", "meal.cal"),
    model = ~ age + ph.karno + meal.cal,
    n_levels = 2L,
    levels = c("Female", "Male")
  )
  expect_equal(result, expected, ignore_formula_env = TRUE)
})

test_that("h_prep_survival_input works with multiple strata specified in separate strata() terms", {
  result <- expect_silent(h_prep_survival_input(
    formula = survival::Surv(time, status) ~ age + ph.karno + meal.cal + strata(strata) + strata(ecog),
    data = surv_data,
    treatment = sex ~ sr(1) # We do not necessarily need to have the strata in the randomization.
  ))
  expected <- list(
    data = surv_data,
    time = "time",
    status = "status",
    treatment = "sex",
    randomization_strata = character(),
    strata = c("strata", "ecog"),
    schema = "sr",
    covariates = c("age", "ph.karno", "meal.cal"),
    model = ~ age + ph.karno + meal.cal,
    n_levels = 2L,
    levels = c("Female", "Male")
  )
  expect_equal(result, expected, ignore_formula_env = TRUE)
})

test_that("h_prep_survival_input works without strata", {
  result <- expect_silent(h_prep_survival_input(
    formula = survival::Surv(time, status) ~ age + ph.karno + meal.cal,
    data = surv_data,
    # We specify strata for the randomization, but there are none in the analysis formula.
    treatment = sex ~ pb(strata)
  ))
  expected <- list(
    data = surv_data,
    time = "time",
    status = "status",
    treatment = "sex",
    randomization_strata = "strata",
    strata = character(),
    schema = "pb",
    covariates = c("age", "ph.karno", "meal.cal"),
    model = ~ age + ph.karno + meal.cal,
    n_levels = 2L,
    levels = c("Female", "Male")
  )
  expect_equal(result, expected, ignore_formula_env = TRUE)
})

test_that("h_prep_survival_input works without covariates", {
  result <- expect_silent(h_prep_survival_input(
    formula = survival::Surv(time, status) ~ 1,
    data = surv_data,
    treatment = sex ~ pb(strata)
  ))
  expected <- list(
    data = surv_data,
    time = "time",
    status = "status",
    treatment = "sex",
    randomization_strata = "strata",
    strata = character(),
    schema = "pb",
    covariates = character(),
    model = ~1,
    n_levels = 2L,
    levels = c("Female", "Male")
  )
  expect_equal(result, expected, ignore_formula_env = TRUE)
})

test_that("h_prep_survival_input works without covariates and without strata", {
  result <- expect_silent(h_prep_survival_input(
    formula = survival::Surv(time, status) ~ 1,
    data = surv_data,
    treatment = sex ~ sr(1)
  ))
  expected <- list(
    data = surv_data,
    time = "time",
    status = "status",
    treatment = "sex",
    randomization_strata = character(),
    strata = character(),
    schema = "sr",
    covariates = character(),
    model = ~1,
    n_levels = 2L,
    levels = c("Female", "Male")
  )
  expect_equal(result, expected, ignore_formula_env = TRUE)
})

test_that("h_n_events_per_time works as expected", {
  result <- expect_silent(h_n_events_per_time(
    surv_data,
    time = "time",
    status = "status"
  ))

  expect_data_frame(result)
  expect_numeric(result$time)
  expect_true(!any(duplicated(result$time)))
  expect_true(all(result$time %in% surv_data$time))
  expect_integer(result$n_events)
  expect_true(sum(result$n_events) == sum(surv_data$status))
})

test_that("h_n_events_per_time works when there are no events", {
  surv_dat_no_events <- surv_data
  surv_dat_no_events$status <- 0

  result <- expect_silent(h_n_events_per_time(
    surv_dat_no_events,
    time = "time",
    status = "status"
  ))

  expect_data_frame(result, nrows = 0L)
  expect_numeric(result$time)
  expect_integer(result$n_events)
})

test_that("sum_vectors_in_list works as expected", {
  lst <- list(a = 1:3, b = 4:6, c = 7:9)
  result <- sum_vectors_in_list(lst)
  expected <- c(12, 15, 18)
  expect_equal(result, expected)
})

test_that("h_first_fct_nested_in_second works as expected without NAs", {
  f1 <- factor(c("A", "B", "C"))
  f2 <- factor(c("C", "A", "B"))
  f3 <- factor(c("A", "B", "B"))
  f4 <- factor(c("A", "B", "C", "D"))

  expect_true(h_first_fct_nested_in_second(f1, f2))
  expect_true(h_first_fct_nested_in_second(f2, f1))
  expect_true(h_first_fct_nested_in_second(f1, f3))
  expect_false(h_first_fct_nested_in_second(f3, f1))
  expect_false(h_first_fct_nested_in_second(f1, f4))
})

test_that("h_first_fct_nested_in_second works as expected with NAs", {
  f1 <- factor(c("A", "B", "C", NA))
  f2 <- factor(c("C", "A", "B", NA))
  f3 <- factor(c(NA, "B", "C", "D"))
  f4 <- factor(c("A", "B", "C", "D", NA))
  f5 <- factor(c("E", "E", "F", "F", NA))

  expect_true(h_first_fct_nested_in_second(f1, f2))
  expect_true(h_first_fct_nested_in_second(f2, f1))
  expect_false(h_first_fct_nested_in_second(f1, f3))
  expect_false(h_first_fct_nested_in_second(f1, f4))
  expect_true(h_first_fct_nested_in_second(f4, f5))
  expect_false(h_first_fct_nested_in_second(f5, f4))
})
