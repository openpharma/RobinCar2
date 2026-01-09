# Generalized Linear Regression in RobinCar and RobinCar2

``` r

if (!requireNamespace("RobinCar", quietly = TRUE)) {
  install.packages("RobinCar")
}
#> Installing package into '/usr/local/lib/R/site-library'
#> (as 'lib' is unspecified)
#> also installing the dependencies 'gtools', 'gplots', 'listenv', 'gargle', 'ids', 'rematch2', 'ROCR', 'future', 'truncnorm', 'conflicted', 'dtplyr', 'googledrive', 'googlesheets4', 'reprex', 'nnls', 'gam', 'cvAUC', 'future.apply', 'progressr', 'Rsolnp', 'emulator', 'tidyverse', 'fastDummies', 'SuperLearner', 'AIPW'
library(RobinCar2)
#> 
#> Attaching package: 'RobinCar2'
#> The following object is masked from 'package:base':
#> 
#>     table
library(RobinCar)
```

[RobinCar](https://marlenabannick.com/RobinCar/) was developed as a
comprehensive library of existing and future methods on covariate
adjustment. RobinCar2 aims to become a standard R package for industry
and regulatory agencies to implement covariate adjustment using selected
methods with clear documentation and validation for GxP compliance.

*See the American Statistical Association (ASA) BIOP Covariate
Adjustment Scientific Working Group (SWG) Software Subteam page
[here](https://carswg.github.io/subteam_software.html) for more
information.*

As such, RobinCar and RobinCar2 share many methods. Please use this
vignette as a guide to the similarities and differences between RobinCar
and RobinCar2.

## RobinCar2 and RobinCar produce the same results when run with the same settings on the same data for GLM

RobinCar2 has a testing suite that ensures that the outputs are
identical between RobinCar and RobinCar2. The comparisons use the
`glm_data` dataset included in RobinCar2. The outputs for the point
estimates and variance estimates are compared under a variety of
settings, including:

- Marginal means and treatment contrasts, for the difference, relative
  risk, odds ratio, and a custom contrast.
- Under Gaussian, binomial, Poisson, and negative binomial (known and
  unknown dispersion parameters) working models.
- Mean model specifications of ANOVA, ANCOVA, and ANHECOVA with up to
  three covariates.
- Under simple, Pocock-Simon, and permuted block randomization schemes.

Please see the following testing file for details:
<https://github.com/openpharma/RobinCar2/blob/main/tests/testthat/test-validate-robin_glm.R>
or view the report at RobinCar2 Validation Report.

## Differences between RobinCar and RobinCar2 argument usage for GLM

The interface for users is very similar between RobinCar and RobinCar2,
though RobinCar2 has some more streamlined use. Users of RobinCar should
find RobinCar2 at least as user-friendly and not have trouble switching
between packages.

Here we provide an example of using a *logistic regression working
model* with *stratified permuted block randomization*, and the
`glm_data` provided in the package. We output the treatment contrast.

    head(glm_data)

### RobinCar

Here is the call in RobinCar, and the output:

``` r

robincar_glm(
  df = glm_data,
  treat_col = "treatment",
  response_col = "y_b",
  formula = y_b ~ treatment * s1 + covar,
  car_strata_cols = "s1",
  car_scheme = "permuted-block",
  g_family = binomial(link = "logit"),
  contrast_h = "diff"
)
#> $main
#> Treatment group mean estimates from a GLM working model of family binomial and link logit using formula: 
#> response ~ treat * s1 + covar
#> 
#> 
#> Used HC0-type of heteroskedasticity-consistent variance estimates 
#> and adjusted variance-covariance matrix for randomization car_strata s1 
#> consistent with the permuted-block design.
#> 
#> Estimates:
#> # A tibble: 3 × 4
#>   treat estimate     se `pval (2-sided)`
#>   <chr>    <dbl>  <dbl>            <dbl>
#> 1 pbo      0.356 0.0336         3.03e-26
#> 2 trt1     0.581 0.0344         7.25e-64
#> 3 trt2     0.621 0.0340         1.54e-74
#> 
#> Variance-Covariance Matrix:
#>              [,1]         [,2]         [,3]
#> pbo  1.128902e-03 1.856234e-05 1.333885e-05
#> trt1 1.856234e-05 1.184599e-03 2.178112e-05
#> trt2 1.333885e-05 2.178112e-05 1.157268e-03
#> 
#> $contrast
#> Treatment group contrasts using linear contrast
#> 
#> Contrasts:
#> # A tibble: 2 × 4
#>   contrast         estimate     se `pval (2-sided)`
#>   <chr>               <dbl>  <dbl>            <dbl>
#> 1 treat trt1 - pbo    0.225 0.0477     0.00000251  
#> 2 treat trt2 - pbo    0.265 0.0475     0.0000000239
#> 
#> Variance-Covariance Matrix for Contrasts:
#>             [,1]        [,2]
#> [1,] 0.002276376 0.001118782
#> [2,] 0.001118782 0.002259492
```

### RobinCar2

In RobinCar2, all of the randomization information, including the name
of the treatment variable, the stratification variable, and the
randomization scheme info is included in the `treatment` argument. This
contrasts with RobinCar, where the information is required in
`treat_col`, `car_strata_cols`, and `car_scheme`.

``` r

robin_glm(
  data = glm_data,
  formula = y_b ~ treatment * s1 + covar,
  treatment = treatment ~ pb(s1),
  family = binomial(link = "logit")
)
#> Model        :  y_b ~ treatment * s1 + covar 
#> Randomization:  treatment ~ pb(s1)  ( Permuted-Block )
#> Variance Type:  vcovG 
#> Marginal Mean: 
#>      Estimate  Std.Err    2.5 % 97.5 %
#> pbo  0.356097 0.033599 0.290243 0.4219
#> trt1 0.580696 0.034418 0.513238 0.6482
#> trt2 0.621386 0.034019 0.554711 0.6881
#> 
#> Contrast     :  difference
#>                Estimate  Std.Err Z Value  Pr(>|z|)    
#> trt1 v.s. pbo  0.224599 0.047711  4.7075 2.508e-06 ***
#> trt2 v.s. pbo  0.265290 0.047534  5.5810 2.391e-08 ***
#> trt2 v.s. trt1 0.040691 0.047941  0.8488     0.396    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

The structure of the output of RobinCar2 more closely mirrors the
structure of the output that one would obtain using
[`glm()`](https://rdrr.io/r/stats/glm.html).

## Feature availability in RobinCar v. RobinCar2

Here we list major differences in the features that are available in
RobinCar and RobinCar2. This will be continually updated as the packages
are updated.

*Last Updated: February 21, 2025*

### Features available in RobinCar2 that are not available in RobinCar

#### Variance function

RobinCar only has one option for the variance, which RobinCar2 calls
`vcovG`. `vcovG` is the influence function-based variance estimator.
RobinCar2 has the option of specifying a `vcov` other than `vcovG`.

Currently, the only other `vcov` that is allowed in RobinCar2 is the
sandwich variance, called `vcovHC` and it is only valid for ANCOVA with
a linear contrast. Please see the table in the following blogpost from
the ASA-BIOP Covariate Adjustment SWG
[here](https://carswg.github.io/posts/blog_linear_model.html) for more
information about the methodology behind the sandwich variance versus
influence-based variance.

### Features available in RobinCar that are not available in RobinCar2

RobinCar supports many other methods like machine-learning methods.
Those methods is not included in RobinCar2.
