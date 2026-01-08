# Calculate Coefficient Estimates from Linear Model Input

Calculate the coefficient estimates for each treatment arm from the
linear model input data.

## Usage

``` r
h_get_beta_estimates(lm_input)

h_get_strat_beta_estimates(strat_lm_input)
```

## Arguments

- lm_input:

  (`list`) A list containing the linear model input data for each
  treatment arm, as returned by
  [`h_get_lm_input()`](https://openpharma.github.io/RobinCar2/reference/get_lm_input.md).

- strat_lm_input:

  (`list`) A list of lists, one for each stratum, containing the linear
  model input data for each treatment arm, as returned by
  [`h_get_strat_lm_input()`](https://openpharma.github.io/RobinCar2/reference/get_lm_input.md).

## Value

A list containing the coefficient estimates for each treatment arm.

## Functions

- `h_get_beta_estimates()`: Calculate the coefficient estimates for the
  overall data set.

- `h_get_strat_beta_estimates()`: Calculate the coefficient estimates
  using the stratified input.
