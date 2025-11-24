# Confidence Interval

Obtain the confidence interval for the marginal mean or the contrast.

## Usage

``` r
# S3 method for class 'prediction_cf'
confint(object, parm, level = 0.95, include_se = FALSE, ...)

# S3 method for class 'surv_effect'
confint(object, parm, level = 0.95, transform, ...)

# S3 method for class 'treatment_effect'
confint(object, parm, level = 0.95, transform, ...)
```

## Arguments

- object:

  Object to construct confidence interval.

- parm:

  (`character` or `integer`) Names of the parameters to construct
  confidence interval.

- level:

  (`numeric`) Confidence level.

- include_se:

  (`flag`) Whether to include the standard error as a column in the
  result matrix.

- ...:

  Not used.

- transform:

  (`function`) Transform function.

## Value

A `matrix` of the confidence interval.

## Examples

``` r
robin_res <- robin_glm(
  y_b ~ treatment * s1,
  data = glm_data, treatment = treatment ~ s1, contrast = "log_risk_ratio"
)
confint(robin_res$marginal_mean, level = 0.7)
#>       Estimate      15 %      85 %
#> pbo  0.3660066 0.3309087 0.4011045
#> trt1 0.5809844 0.5446811 0.6172876
#> trt2 0.6101537 0.5744255 0.6458818
confint(robin_res$contrast, parm = 1:3, level = 0.9)
#> The confidence interval is transformed.
#>                Estimate       5 %     95 %
#> trt1 v.s. pbo  1.587360 1.3240911 1.902975
#> trt2 v.s. pbo  1.667056 1.3951516 1.991954
#> trt2 v.s. trt1 1.050207 0.9169573 1.202820
```
