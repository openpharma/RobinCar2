# Log Hazard Ratio Estimation and Log-Rank Test via Score Function

This function combines the estimation of the log hazard ratio and the
log-rank test using a score function. Only two treatment arms are being
compared and the `data` is subset accordingly.

## Usage

``` r
robin_surv_comparison(
  score_fun,
  vars,
  data,
  exp_level,
  control_level,
  unadj_score_fun = NULL,
  ...
)
```

## Arguments

- score_fun:

  (`function`) The log-rank score function to be used for both
  estimation and testing.

- vars:

  (`list`) A list containing `levels`, `treatment`, and `covariates`.

- data:

  (`data.frame`) The data frame containing the survival data.

- exp_level:

  (`count`) Level of the experimental treatment arm.

- control_level:

  (`count`) Level of the control treatment arm.

- unadj_score_fun:

  (`function` or `NULL`) Optional unadjusted score function, see
  details.

- ...:

  Additional arguments passed to `score_fun`.

## Value

A list containing:

- `estimate`: The estimated log hazard ratio.

- `se`: The standard error of the estimated log hazard ratio.

- `hr_n`: The number of observations used in the estimation.

- `hr_sigma_l2`: The variance of the log-rank statistic used in the
  estimation.

- `test_stat`: The log-rank test statistic.

- `p_value`: The two-sided p-value of the log-rank test.

- `test_score`: The log-rank score statistic.

- `test_n`: The number of observations used in the log-rank test.

- `test_sigma_l2`: The variance of the log-rank statistic used in the
  log-rank test.

## Details

If an unadjusted score function is provided in `unadj_score_fun`, then
it is used to estimate the log hazard ratio first. This unadjusted log
hazard ratio estimate is then passed on to the adjusted score function
`score_fun` as `theta_hat`. This is required when the score function is
adjusted for covariates.
