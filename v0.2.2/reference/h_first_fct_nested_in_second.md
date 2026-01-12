# Check Whether First Factor is Nested in Second Factor

This function checks whether one factor variable is nested in another in
terms of their levels and the mapping of observations to these levels:

## Usage

``` r
h_first_fct_nested_in_second(f1, f2)
```

## Arguments

- f1:

  A factor variable, supposed to be more fine grained than `f2`.

- f2:

  A factor variable, supposed to be more coarse grained than `f1`.

## Value

`TRUE` if `f1` is nested in `f2`, `FALSE` otherwise.

## Details

- Both factors must have the same length.

- The positions of `NA` values must be identical in both factors.

- After removing `NA` values, `f1` must have at least as many observed
  levels as `f2`.

- The mapping of observations to levels must be consistent, meaning that
  each level in `f1` corresponds to exactly one level in `f2`. On the
  other hand, multiple levels in `f1` can map to the same level in `f2`.
