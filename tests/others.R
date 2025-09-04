# Other tests which don't work within the testthat framework.
library(RobinCar2)

# test: "h_prep_survival_input works with survival::Surv objects"
surv_obj <- with(surv_data, survival::Surv(time, status))
# This works with coxph e.g.:
example <- survival::coxph(surv_obj ~ sex, data = surv_data)
result <- RobinCar2:::h_prep_survival_input(
  formula = surv_obj ~ 1,
  # We have another restriction here to avoid ambiguity, therefore
  # need to remove the time and status columns from the data.
  data = subset(surv_data, select = -c(time, status)),
  treatment = sex ~ 1
)
expected <- list(
  data = cbind(
    subset(surv_data, select = -c(time, status)),
    subset(surv_data, select = c(time, status))
  ), # The two removed columns have been added back.
  time = "time",
  status = "status",
  treatment = "sex",
  strata = character(),
  schema = "sp",
  covariates = character(),
  model = ~1,
  n_levels = 2L,
  levels = c("Female", "Male")
)
testthat::expect_equal(result, expected, ignore_formula_env = TRUE)
