# Heteroskedasticity-consistent covariance matrix for predictions

The heteroskedasticity-consistent covariance matrix for predictions is
obtained with `sandwich::vocvHC` using sandwich method.

## Usage

``` r
vcovHC(x, type = "HC3", ...)
```

## Arguments

- x:

  (`prediction_cf`) Counter-factual prediction.

- type:

  (`character`) Type of HC covariance matrix.

- ...:

  Additional arguments for
  [`sandwich::vcovHC`](https://sandwich.R-Forge.R-project.org/reference/vcovHC.html).

## Value

Matrix of the heteroskedasticity-consistent covariance for the
predictions.
