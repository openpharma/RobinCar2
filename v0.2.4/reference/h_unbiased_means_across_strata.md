# Check Unbiased Means of Residuals Across Randomization Strata and Treatment Groups

This function checks whether the means of residuals are approximately
zero across specified randomization strata for each of the two treatment
groups. It is used in
[`robin_surv()`](https://openpharma.github.io/RobinCar2/reference/robin_surv.md)
downstream functions to verify the correct inclusion of randomization
strata variables in the analysis survival model.

## Usage

``` r
h_unbiased_means_across_strata(
  residuals_per_group,
  df,
  randomization_strata,
  eps = sqrt(.Machine$double.eps)
)
```

## Arguments

- residuals_per_group:

  (`list` of `numeric`) A named list of numeric vectors containing
  residuals for each of the two treatment groups.

- df:

  (`data.frame`) The data frame containing the `treatment` and
  randomization strata variables, produced by
  [`h_derived_outcome_vals()`](https://openpharma.github.io/RobinCar2/reference/derived_outcome_vals.md)
  or
  [`h_strat_derived_outcome_vals()`](https://openpharma.github.io/RobinCar2/reference/derived_outcome_vals.md).

- randomization_strata:

  (`character`) A character vector of names of the randomization strata
  variables in `df`.

- eps:

  (`numeric`) A small tolerance value to determine if means are close to
  zero.

## Value

`TRUE` if the means of residuals across randomization strata are within
the specified tolerance for both treatment groups, `FALSE` otherwise.

## See also

[`bias()`](https://openpharma.github.io/RobinCar2/reference/bias.md) for
the underlying bias (means of residuals) calculation.
