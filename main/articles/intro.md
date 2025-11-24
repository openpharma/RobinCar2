# Package Introduction

RobinCar2 provides robust covariate adjustment methods for estimating
and inferring treatment effects through generalized linear models (glm)
under different randomization schema.

## Common Usage

A minimal call of
[`robin_lm()`](https://openpharma.github.io/RobinCar2/reference/robin_lm.md)
and
[`robin_glm()`](https://openpharma.github.io/RobinCar2/reference/robin_glm.md),
consisting of only formula, data arguments and the randomization scheme,
will produce an object of class `treatment_effect`.

``` r
library(RobinCar2)
#> 
#> Attaching package: 'RobinCar2'
#> The following object is masked from 'package:base':
#> 
#>     table
head(glm_data)
#>   id treatment s1 s2      covar           y y_b
#> 1  1       pbo  b  c  0.5119022 -0.02761963   0
#> 2  2       pbo  a  c -0.7941720  0.49919508   0
#> 3  3      trt1  b  d  0.8988804  0.48037375   0
#> 4  4      trt2  b  c -0.4821317  0.67490126   1
#> 5  5      trt1  a  d -0.2285514  0.55499267   0
#> 6  6      trt2  a  c  0.2742069  2.39830584   1
```

### Data Introduction

In the `glm_data`, we have the following columns:

1.  `id` is the patient identifier.
2.  `treatment` is the treatment assignment.
3.  `s1` is the first stratification factor.
4.  `s2` is the second stratification factor.
5.  `covar` is the continuous covariate.
6.  `y` is the continuous outcome.
7.  `y_b` is the binary outcome.

### Obtain Treatment Effect for Continuous Outcomes Using the General Variance

For the continuous outcome `y`, the linear model includes `covar` as a
covariate, `s1` as a stratification factor. The randomization scheme is
a permuted-block randomization stratified by `s1`. The model formula
also includes the treatment by stratification interaction as
`y ~ treatment * s1 + covar`.

``` r
robin_lm(y ~ treatment * s1 + covar,
  data = glm_data,
  treatment = treatment ~ pb(s1)
)
#> Model        :  y ~ treatment * s1 + covar 
#> Randomization:  treatment ~ pb(s1)  ( Permuted-Block )
#> Variance Type:  vcovG 
#> Marginal Mean: 
#>      Estimate  Std.Err    2.5 % 97.5 %
#> pbo  0.200321 0.067690 0.067651 0.3330
#> trt1 0.763971 0.075929 0.615152 0.9128
#> trt2 0.971250 0.076543 0.821228 1.1213
#> 
#> Contrast     :  h_diff
#>                Estimate Std.Err Z Value  Pr(>|z|)    
#> trt1 v.s. pbo   0.56365 0.10074  5.5952 2.203e-08 ***
#> trt2 v.s. pbo   0.77093 0.10133  7.6082 2.779e-14 ***
#> trt2 v.s. trt1  0.20728 0.10683  1.9402   0.05235 .  
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

We can also use the Huber-White variance estimator by setting
`vcov = "vcovHC"`. Please note that in this case, the model formula
should not contain the treatment by stratification (covariate)
interaction.

``` r
robin_lm(y ~ treatment + s1 + covar,
  data = glm_data,
  treatment = treatment ~ pb(s1),
  vcov = "vcovHC"
)
#> Model        :  y ~ treatment + s1 + covar 
#> Randomization:  treatment ~ pb(s1)  ( Permuted-Block )
#> Variance Type:  vcovG 
#> Marginal Mean: 
#>      Estimate  Std.Err    2.5 % 97.5 %
#> pbo  0.200449 0.067690 0.067779 0.3331
#> trt1 0.763978 0.075930 0.615158 0.9128
#> trt2 0.971285 0.076539 0.821271 1.1213
#> 
#> Contrast     :  h_diff
#>                Estimate Std.Err Z Value  Pr(>|z|)    
#> trt1 v.s. pbo   0.56353 0.10074  5.5941 2.218e-08 ***
#> trt2 v.s. pbo   0.77084 0.10133  7.6074 2.796e-14 ***
#> trt2 v.s. trt1  0.20731 0.10683  1.9405   0.05232 .  
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

Note that `robin_glm` can also handle continuous outcomes using the
default family and the default link function `family = gaussian()`.

### Obtain Treatment Effect for Binary Outcomes

For binary outcomes, the logistic model includes `covar` as a covariate,
`s1` as a stratification factor. The randomization scheme is a
permuted-block randomization stratified by `s1`. The model formula also
includes the treatment by stratification interaction as
`y_b ~ treatment * s1 + covar`. Note here we need to specify `family` to
be `binomial(link = "logit")`.

``` r
robin_glm(y_b ~ treatment * s1 + covar,
  data = glm_data,
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

### Obtain Treatment Effect for Counts

For counts, the log link model includes `covar` as a covariate, `s1` as
a stratification factor. The randomization scheme is a permuted-block
randomization stratified by `s1`. The model formula also includes the
treatment by stratification interaction as
`y_count ~ treatment * s1 + covar`. Note here we need to specify
`family` to be `poisson(link = "log")` to use the Poisson model or to be
`MASS::negative.binomial(theta = NA)` to use the negative binomial
model. A fixed `theta` could be provided if it is known.

``` r
glm_data$y_count <- rpois(nrow(glm_data), lambda = 20)
robin_glm(
  y_count ~ treatment * s1 + covar,
  data = glm_data,
  treatment = treatment ~ pb(s1),
  family = MASS::negative.binomial(theta = 1)
)
#> Model        :  y_count ~ treatment * s1 + covar 
#> Randomization:  treatment ~ pb(s1)  ( Permuted-Block )
#> Variance Type:  vcovG 
#> Marginal Mean: 
#>      Estimate  Std.Err    2.5 % 97.5 %
#> pbo  19.15272  0.30047 18.56381 19.742
#> trt1 20.06258  0.33673 19.40260 20.723
#> trt2 20.59901  0.32085 19.97016 21.228
#> 
#> Contrast     :  difference
#>                Estimate Std.Err Z Value  Pr(>|z|)    
#> trt1 v.s. pbo   0.90986 0.45255  2.0105 0.0443766 *  
#> trt2 v.s. pbo   1.44629 0.43915  3.2934 0.0009898 ***
#> trt2 v.s. trt1  0.53643 0.46552  1.1523 0.2491880    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

### Using Different Covariate-Adaptive Randomization Schema

If the randomization schema is not permuted-block randomization, we can
use other randomization schema. Currently RobinCar2 supports `sr` for
the simple randomization (this was previously called `sp`), `pb` for the
permuted-block randomization, and `ps` for the Pocock-Simon
randomization.

``` r
robin_glm(y_b ~ treatment * s1 + covar,
  data = glm_data,
  treatment = treatment ~ ps(s1),
  family = binomial(link = "logit")
)
#> Model        :  y_b ~ treatment * s1 + covar 
#> Randomization:  treatment ~ ps(s1)  ( Pocock-Simon )
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

### Obtain the Confidence Intervals for the Marginal Means and contrast

To obtain the confidence interval, we can use `confint` function.

Given the following model

``` r
robin_res <- robin_glm(y_b ~ treatment * s1 + covar,
  data = glm_data,
  treatment = treatment ~ ps(s1),
  family = binomial(link = "logit"),
  contrast = "log_risk_ratio"
)
robin_res
#> Model        :  y_b ~ treatment * s1 + covar 
#> Randomization:  treatment ~ ps(s1)  ( Pocock-Simon )
#> Variance Type:  vcovG 
#> Marginal Mean: 
#>      Estimate  Std.Err    2.5 % 97.5 %
#> pbo  0.356097 0.033599 0.290243 0.4219
#> trt1 0.580696 0.034418 0.513238 0.6482
#> trt2 0.621386 0.034019 0.554711 0.6881
#> 
#> Contrast     :  log_risk_ratio
#>                Estimate  Std.Err Z Value Pr(>|z|)    
#> trt1 v.s. pbo  0.489025 0.110617  4.4209 9.83e-06 ***
#> trt2 v.s. pbo  0.556751 0.108532  5.1298 2.90e-07 ***
#> trt2 v.s. trt1 0.067726 0.079934  0.8473   0.3968    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

It is easy to obtain the confidence interval matrix for the marginal
mean, at specified level. If `parm` is not provided, the complete matrix
(all treatment groups) will be provided.

``` r
confint(robin_res$marginal_mean, parm = 1:2, level = 0.7)
#>       Estimate      15 %      85 %
#> pbo  0.3560965 0.3212733 0.3909198
#> trt1 0.5806957 0.5450238 0.6163677
confint(robin_res$marginal_mean, level = 0.7)
#>       Estimate      15 %      85 %
#> pbo  0.3560965 0.3212733 0.3909198
#> trt1 0.5806957 0.5450238 0.6163677
#> trt2 0.6213865 0.5861284 0.6566445
```

Similarly for the contrast, however it has an additional argument
`transform` to provide the confidence interval at transformed level.
Thus, standard error is removed because it does not make sense anymore.
By default, if the `log_risk_ratio` or `log_odds_ratio` is used as
contrast, the `confint` will transform it back using exponential
function. You can also specify the `transform` to be `identity` to avoid
the transformation.

``` r
confint(robin_res$contrast)
#> The confidence interval is transformed.
#>                Estimate     2.5 %   97.5 %
#> trt1 v.s. pbo  1.630726 1.3128756 2.025528
#> trt2 v.s. pbo  1.744994 1.4106235 2.158624
#> trt2 v.s. trt1 1.070072 0.9148996 1.251563
confint(robin_res$contrast, transform = identity)
#>                  Estimate      2.5 %    97.5 %
#> trt1 v.s. pbo  0.48902507  0.2722198 0.7058303
#> trt2 v.s. pbo  0.55675135  0.3440318 0.7694709
#> trt2 v.s. trt1 0.06772628 -0.0889410 0.2243936
```
