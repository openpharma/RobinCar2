# Covariate adjusted glm model

Covariate adjusted glm model

## Usage

``` r
robin_glm(
  formula,
  data,
  treatment,
  contrast = c("difference", "risk_ratio", "odds_ratio", "log_risk_ratio",
    "log_odds_ratio"),
  contrast_jac = NULL,
  vcov = "vcovG",
  family = gaussian(),
  vcov_args = list(),
  pair,
  ...
)
```

## Arguments

- formula:

  (`formula`) A formula of analysis.

- data:

  (`data.frame`) Input data frame.

- treatment:

  (`formula` or `character(1)`) A formula of treatment assignment or
  assignment by stratification, or a string name of treatment
  assignment.

- contrast:

  (`function` or `character(1)`) A function to calculate the treatment
  effect, or character of "difference", "risk_ratio", "odds_ratio" for
  default contrasts.

- contrast_jac:

  (`function`) A function to calculate the Jacobian of the contrast
  function. Ignored if using default contrasts.

- vcov:

  (`function`) A function to calculate the variance-covariance matrix of
  the treatment effect, including `vcovHC` and `vcovG`.

- family:

  (`family`) A family object of the glm model.

- vcov_args:

  (`list`) Additional arguments passed to `vcov`.

- pair:

  Pairwise treatment comparison.

- ...:

  Additional arguments passed to `glm` or `glm.nb`.

## Value

A robin_output object, with `marginal_mean` and `contrast` components.

## Details

If family is `MASS::negative.binomial(NA)`, the function will use
[`MASS::glm.nb`](https://rdrr.io/pkg/MASS/man/glm.nb.html) instead of
`glm`.

## Examples

``` r
robin_glm(
  y ~ treatment * s1,
  data = glm_data,
  treatment = treatment ~ s1, contrast = "difference"
)
#> Model        :  y ~ treatment * s1 
#> Randomization:  treatment ~ s1  ( Simple )
#> Variance Type:  vcovG 
#> Marginal Mean: 
#>      Estimate  Std.Err    2.5 % 97.5 %
#> pbo  0.223048 0.068366 0.089054 0.3570
#> trt1 0.763992 0.077178 0.612727 0.9153
#> trt2 0.948275 0.077437 0.796502 1.1000
#> 
#> Contrast     :  difference
#>                Estimate Std.Err Z Value  Pr(>|z|)    
#> trt1 v.s. pbo   0.54094 0.10282  5.2611 1.432e-07 ***
#> trt2 v.s. pbo   0.72523 0.10302  7.0399 1.924e-12 ***
#> trt2 v.s. trt1  0.18428 0.10911  1.6890   0.09123 .  
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
```
