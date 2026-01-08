# Derive Outcome Values Based on Log Hazard Ratio

Compute the derived outcome values based on a given log hazard ratio.

## Usage

``` r
h_derived_outcome_vals(
  theta,
  df,
  treatment,
  time,
  status,
  covariates,
  n = nrow(df)
)

h_strat_derived_outcome_vals(
  theta,
  df,
  treatment,
  time,
  status,
  strata,
  covariates
)
```

## Arguments

- theta:

  (`number`) The assumed log hazard ratio of the second vs. the first
  level of the treatment arm variable.

- df:

  (`data.frame`) The data frame containing the survival data.

- treatment:

  (`string`) The name of the treatment arm variable in `df`. It should
  be a factor with two levels, where the first level is the reference
  group.

- time:

  (`string`) The name of the time variable in `df`, representing the
  survival time.

- status:

  (`string`) The name of the status variable in `df`, with 0 for
  censored and 1 for event.

- covariates:

  (`character`) The column names in `df` to be used for covariate
  adjustment.

- n:

  (`count`) The number of observations. Note that this can be higher
  than the number of rows when used in stratified analyses computations.

- strata:

  (`character`) The names of the strata variables in `df`, which must be
  factors.

## Value

A data frame containing the same data as the input `df`, but
restructured with standardized column names `index`, `treatment`,
`time`, `status`, the covariates, and an additional column `O_hat`
containing the derived outcome values. For the stratified version, the
list of data frames is returned, one for each stratum.

## Details

Please note that the `covariates` must not include `index`, `treatment`,
`time`, `status` to avoid naming conflicts.

## Functions

- `h_derived_outcome_vals()`: calculates the derived outcome values for
  the overall data set.

- `h_strat_derived_outcome_vals()`: calculates the derived outcome
  values for each stratum separately.
