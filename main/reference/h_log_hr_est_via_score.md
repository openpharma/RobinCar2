# Estimate Log Hazard Ratio via Score Function

This function estimates the log hazard ratio by finding the root of the
log-rank score function.

## Usage

``` r
h_log_hr_est_via_score(score_fun, interval = c(-5, 5), ...)
```

## Arguments

- score_fun:

  (`function`) The log-rank score function to be used for estimation.

- interval:

  (`numeric`) A numeric vector of length 2 specifying the interval in
  which to search for the root.

- ...:

  Additional arguments passed to `score_fun`.

## Value

A list containing:

- `theta`: The estimated log hazard ratio.

- `se`: The standard error of the estimated log hazard ratio.

- `sigma_L2`: The variance of the log-rank statistic.

- `n`: The number of observations used in the calculation.

## Details

This deactivates the ties factor correction in the score function by
passing `use_ties_factor = FALSE` to the `score_fun`. The root finding
is done without calculating the variance by passing
`calculate_variance = FALSE`, which is only calculated at the solution.
This saves computation time, and avoids spurious warnings about negative
variances during the root finding process.
