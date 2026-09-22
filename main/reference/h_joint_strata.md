# Build the Joint Strata Factor

Unlike [`interaction()`](https://rdrr.io/r/base/interaction.html), the
stratum identity is derived from the level *codes* of the input columns
rather than from pasted labels, so levels that happen to contain the
separator (e.g. `"x:y"` crossed with `"z"` versus `"x"` crossed with
`"y:z"`) remain distinct strata instead of silently collapsing into one.
Labels are only cosmetic and are made unique for printing.

## Usage

``` r
h_joint_strata(df)
```

## Arguments

- df:

  (`data.frame`) Strata columns. No missing values are allowed.

## Value

A `factor` of observed joint strata, with
[`interaction()`](https://rdrr.io/r/base/interaction.html)-style
`:`-separated labels and the first variable varying fastest.
