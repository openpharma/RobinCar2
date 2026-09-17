# Prediction Bias

Obtain prediction bias within each stratum.

## Usage

``` r
bias(residual, treatment, group_idx)
```

## Arguments

- residual:

  (`numeric`) residuals.

- treatment:

  (`factor`) treatment.

- group_idx:

  (`list` of `integer`) indices for each stratum group.

## Value

Numeric matrix of bias in each stratum.
