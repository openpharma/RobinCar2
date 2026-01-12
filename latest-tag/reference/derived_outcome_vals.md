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
  randomization_strata,
  n = nrow(df)
)

h_strat_derived_outcome_vals(
  theta,
  df,
  treatment,
  time,
  status,
  strata,
  covariates,
  randomization_strata
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

- randomization_strata:

  (`character`) The names of the randomization strata variables in `df`.
  These are used to check whether the means of the covariate adjustment
  residuals are unbiased across these strata.

- n:

  (`count`) The number of observations. Note that this can be higher
  than the number of rows when used in stratified analyses computations.

- strata:

  (`character`) The names of the strata variables in `df`, which must be
  factors.

## Value

A data frame containing the same data as the input `df`, but
restructured with standardized column names `index`, `treatment`,
`time`, `status`, the covariates and randomization stratification
variables, and an additional column `O_hat` containing the derived
outcome values. For the stratified version, the computations are done
separately by stratum, and the resulting `data.frame` contains an
additional `.stratum` column indicating the stratum number.

## Details

Please note that the `covariates` and `randomization_strata` must not
include `index`, `treatment`, `time`, `status` to avoid naming
conflicts.

## Functions

- `h_derived_outcome_vals()`: calculates the derived outcome values for
  the overall data set.

- `h_strat_derived_outcome_vals()`: calculates the derived outcome
  values for each stratum separately.
