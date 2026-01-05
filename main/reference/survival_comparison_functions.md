# Survival Comparison Functions

These are simple wrappers around
[`robin_surv_comparison()`](https://openpharma.github.io/RobinCar2/reference/robin_surv_comparison.md)
called with the corresponding log-rank score functions.

## Usage

``` r
robin_surv_no_strata_no_cov(
  vars,
  data,
  exp_level,
  control_level,
  contrast,
  check_rand_strat_warning = FALSE
)

robin_surv_strata(
  vars,
  data,
  exp_level,
  control_level,
  contrast,
  check_rand_strat_warning = FALSE
)

robin_surv_cov(vars, data, exp_level, control_level, contrast, ...)

robin_surv_strata_cov(vars, data, exp_level, control_level, contrast, ...)
```

## Arguments

- vars:

  (`list`) A list containing `levels`, `treatment`, and `covariates`.

- data:

  (`data.frame`) The data frame containing the survival data.

- exp_level:

  (`count`) Level of the experimental treatment arm.

- control_level:

  (`count`) Level of the control treatment arm.

- ...:

  Additional arguments passed to `score_fun`.

## Value

See
[`robin_surv_comparison()`](https://openpharma.github.io/RobinCar2/reference/robin_surv_comparison.md).

## Functions

- `robin_surv_no_strata_no_cov()`: without strata and without
  covariates, based on
  [`h_lr_score_no_strata_no_cov()`](https://openpharma.github.io/RobinCar2/reference/survival_score_functions.md).

- `robin_surv_strata()`: without strata and without covariates, based on
  [`h_lr_score_strat()`](https://openpharma.github.io/RobinCar2/reference/survival_score_functions.md).

- `robin_surv_cov()`: without strata and without covariates, based on
  [`h_lr_score_cov()`](https://openpharma.github.io/RobinCar2/reference/survival_score_functions.md)
  and
  [`h_lr_score_no_strata_no_cov()`](https://openpharma.github.io/RobinCar2/reference/survival_score_functions.md)
  (which is used to find the unadjusted log hazard ratio estimate).

- `robin_surv_strata_cov()`: with strata and covariates, based on
  [`h_lr_score_strat_cov()`](https://openpharma.github.io/RobinCar2/reference/survival_score_functions.md)
  and
  [`h_lr_score_strat()`](https://openpharma.github.io/RobinCar2/reference/survival_score_functions.md)
  (which is used to find the unadjusted log hazard ratio estimate).
