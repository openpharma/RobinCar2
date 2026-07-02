# Get Linear Model Input Data

Prepare the input data for a linear model based on the provided data
frame and model formula.

## Usage

``` r
h_get_lm_input(df, model)

h_get_strat_lm_input(df_with_stratum, model)
```

## Arguments

- df:

  (`data.frame`) Including the covariates needed for the `model`, as
  well as the derived outcome `O_hat` and the `treatment` factor.

- model:

  (`formula`) The right-hand side only model formula.

- df_with_stratum:

  (`data.frame`) A data frame with an additional column for the stratum,
  as returned by
  [`h_strat_derived_outcome_vals()`](https://openpharma.github.io/RobinCar2/reference/derived_outcome_vals.md).

## Value

A list containing for each element of the `treatment` factor a list with
the corresponding model matrix `X` and the response vector `y`. For the
stratified version, the model matrix `X` includes the `.stratum` column.

## Functions

- `h_get_lm_input()`: Get the linear model input data for the overall
  data set.

- `h_get_strat_lm_input()`: Get the linear model input data with
  attached stratum column.
