# Covariate Adjusted and Stratified Survival Analysis

Calculate log-rank test as well as hazard ratio estimates for survival
data, optionally adjusted for covariates and a stratification factor.

## Usage

``` r
robin_surv(
  formula,
  data,
  treatment,
  comparisons,
  contrast = c("hazardratio", "none"),
  test = "logrank",
  ...
)
```

## Arguments

- formula:

  (`formula`) A formula of analysis, of the form
  `Surv(time, status) ~ covariates + strata(x, y, z)`. If no covariates
  should be adjusted for, use `1` instead on the right hand side. The
  intercept must not be removed. If no stratification factors should be
  used for the analysis, do not use `strata()` in the formula.

- data:

  (`data.frame`) Input data frame.

- treatment:

  (`formula`) A formula of treatment assignment or assignment by
  stratification, of the form `treatment ~ scheme(vars)`. Note that
  currently the randomization scheme is not used in the analysis.
  However, any variables that were used in the randomization scheme must
  be included in the model formula, either as covariates, or as
  `strata()`.

- comparisons:

  (`list`) An optional list of comparisons between treatment levels to
  be performed, see details. By default, all pairwise comparisons are
  performed automatically.

- contrast:

  (`character(1)`) The contrast statistic to be used, currently only
  `"hazardratio"` is supported. Can be disabled by specifying `"none"`,
  in which case only the log-rank test is performed.

- test:

  (`character(1)`) The test to be used, currently only `"logrank"` is
  supported.

- ...:

  Additional arguments passed to the survival analysis functions, in
  particular `hr_se_plugin_adjusted` (please see
  [here](https://openpharma.github.io/RobinCar2/reference/survival_score_functions.md)
  for details).

## Value

A `surv_effect` object containing the results of the survival analysis.

## Details

The user can optionally specify a list of comparisons between treatment
levels to be performed. The list must have two elements:

- Treatment level indices of the treatment group.

- Treatment level indices of the control group.

So for example if you would like to compare level 3 with level 1, and
also level 3 with level 2 (but not level 2 with level 1) then you can
specify: `comparisons = list(c(3, 3), c(1, 2))`

## See also

[surv_effect_methods](https://openpharma.github.io/RobinCar2/reference/surv_effect_methods.md)
for S3 methods.

## Examples

``` r
# Adjusted for covariates meal.cal and age and adjusted for stratification by strata:
robin_surv(
  formula = Surv(time, status) ~ meal.cal + age + strata(strata),
  data = surv_data,
  treatment = sex ~ pb(strata)
)
#> Model        : Surv(time, status) ~ meal.cal + age + strata(strata)
#> Randomization: sex ~ pb(strata) (Permuted-Block)
#> Stratification variables:  strata 
#> Covariates adjusted for: meal.cal, age (including interactions with sex)
#> 
#> Contrast     : Covariate-adjusted Stratified Log Hazard Ratio
#> 
#>                  Estimate Std.Err Z Value Pr(>|z|)   
#> Male v.s. Female  0.55219 0.19133  2.8861   0.0039 **
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Test         : Covariate-adjusted Stratified Log-Rank
#> 
#>                  Test Stat. Pr(>|z|)   
#> Male v.s. Female     2.9496 0.003181 **
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Adjusted for stratification by strata and ecog but not for covariates:
robin_surv(
  formula = Surv(time, status) ~ 1 + strata(strata, ecog),
  data = surv_data,
  treatment = sex ~ sr(1)
)
#> Model        : Surv(time, status) ~ 1 + strata(strata, ecog)
#> Randomization: sex ~ sr(1) (Simple)
#> Stratification variables:  strata, ecog 
#> 
#> Contrast     : Stratified Log Hazard Ratio
#> 
#>                  Estimate Std.Err Z Value Pr(>|z|)   
#> Male v.s. Female  0.55482 0.17063  3.2516 0.001147 **
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Test         : Stratified Log-Rank
#> 
#>                  Test Stat. Pr(>|z|)   
#> Male v.s. Female     3.2856 0.001018 **
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Unadjusted for covariates and stratification:
robin_surv(
  formula = Surv(time, status) ~ 1,
  data = surv_data,
  treatment = sex ~ sr(1)
)
#> Model        : Surv(time, status) ~ 1
#> Randomization: sex ~ sr(1) (Simple)
#> 
#> Contrast     : Log Hazard Ratio
#> 
#>                  Estimate Std.Err Z Value Pr(>|z|)   
#> Male v.s. Female  0.53343 0.16727   3.189 0.001428 **
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Test         : Log-Rank
#> 
#>                  Test Stat. Pr(>|z|)   
#> Male v.s. Female     3.2135 0.001311 **
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
```
