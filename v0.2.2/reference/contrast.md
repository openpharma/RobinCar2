# Contrast Functions and Jacobians

Contrast Functions and Jacobians

Create Contrast of Pairs

## Usage

``` r
h_diff(x, y)

h_jac_diff(x, y)

h_risk_ratio(x, y)

h_jac_risk_ratio(x, y)

h_odds_ratio(x, y)

h_jac_odds_ratio(x, y)

h_log_risk_ratio(x, y)

h_jac_log_risk_ratio(x, y)

h_log_odds_ratio(x, y)

h_jac_log_odds_ratio(x, y)

eff_jacob(f)

pairwise(levels, x = levels)

against_ref(levels, ref = levels[1], x = tail(levels, -1))

custom_contrast(levels, x, y)
```

## Arguments

- x:

  (`vector`) A vector of treatment levels.

- y:

  (`vector`) A vector of treatment levels.

- f:

  (`function`) Function with argument x and y to compute treatment
  effect.

- levels:

  (`character`) Levels of the treatment.

- ref:

  (`string` or `int`) Reference level.

## Value

Vector of contrasts, or matrix of jacobians.

A list of `contrast` object with following elements:

- Index of the treatment group.

- Index of the reference group. Additional attributes include `levels`
  and `max_levels` indicating the names of the treatment levels and the
  maximum number of levels.

## Examples

``` r
h_diff(1:3, 4:6)
#> [1] -3 -3 -3
h_jac_risk_ratio(1:3, 4:6)
#>           [,1]        [,2]
#> [1,] 0.2500000 -0.06250000
#> [2,] 0.2000000 -0.08000000
#> [3,] 0.1666667 -0.08333333
```
