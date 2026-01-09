# Calculate Coefficient Estimates and Corresponding Residuals from Linear Model Input

Calculate the coefficient estimates for each treatment arm from the
linear model input data. Also returns the corresponding residuals.

## Usage

``` r
h_get_lm_results(lm_input)

h_get_strat_lm_results(strat_lm_input)
```

## Arguments

- lm_input:

  (`list`) A list containing the linear model input data for each
  treatment arm, as returned by
  [`h_get_lm_input()`](https://openpharma.github.io/RobinCar2/reference/get_lm_input.md).

- strat_lm_input:

  (`list`) A list containing the linear model input data for each
  treatment arm and including the `.strata` column in the design matrix,
  as returned by
  [`h_get_strat_lm_input()`](https://openpharma.github.io/RobinCar2/reference/get_lm_input.md).

## Value

A list with:

- `beta_est`: the coefficient estimates for each treatment arm.

- `residuals`: the corresponding residuals for each treatment arm.

## Functions

- `h_get_lm_results()`: Calculate the coefficient estimates for the
  overall data set.

- `h_get_strat_lm_results()`: Calculate the coefficient estimates using
  the stratified input.
