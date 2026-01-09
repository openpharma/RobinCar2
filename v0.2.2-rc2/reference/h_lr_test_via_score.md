# Log-Rank Test via Score Function

This function performs a log-rank test using the score function.

## Usage

``` r
h_lr_test_via_score(score_fun, ...)
```

## Arguments

- score_fun:

  (`function`) The log-rank score function to be used for testing.

- ...:

  Additional arguments passed to `score_fun`.

## Value

A list containing:

- `u_l`: The log-rank score statistic.

- `sigma_l2`: The variance of the log-rank statistic.

- `tau_l`: The log-rank test statistic.

- `pval`: The two-sided p-value of the log-rank test.

- `n`: The number of observations used in the calculation.

## Details

This activates the ties factor correction in the score function by
passing `use_ties_factor = TRUE` to the `score_fun`.
