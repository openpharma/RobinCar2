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
  contrast = "hazardratio",
  test = "logrank",
  ...
)
```

## Arguments

- formula:

  (`formula`) A formula of analysis, of the form
  `Surv(time, status) ~ covariates`. (If no covariates should be
  adjusted for, use `1` instead on the right hand side. The intercept
  must not be removed.)

- data:

  (`data.frame`) Input data frame.

- treatment:

  (`formula`) A formula of treatment assignment or assignment by
  stratification, of the form `treatment ~ strata`. (If no
  stratification should be adjusted for, use `1` instead on the right
  hand side.)

- comparisons:

  (`list`) An optional list of comparisons between treatment levels to
  be performed, see details. By default, all pairwise comparisons are
  performed automatically.

- contrast:

  (`character(1)`) The contrast statistic to be used, currently only
  `"hazardratio"` is supported.

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
  formula = Surv(time, status) ~ meal.cal + age,
  data = surv_data,
  treatment = sex ~ strata
)
#> Model        :  Surv(time, status) ~ meal.cal + age 
#> Randomization:  sex ~ strata  ( Simple )
#> 
#> Contrast     :  Log Hazard ratio
#> 
#>                  Estimate Std.Err Z Value Pr(>|z|)   
#> Male v.s. Female  0.55219 0.19133  2.8861   0.0039 **
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Test         :  Log-Rank
#> 
#>                  Test Stat. Pr(>|z|)   
#> Male v.s. Female     2.9496 0.003181 **
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Adjusted for stratification by strata and ecog but not for covariates:
robin_surv(
  formula = Surv(time, status) ~ 1,
  data = surv_data,
  treatment = sex ~ strata + ecog
)
#> Model        :  Surv(time, status) ~ 1 
#> Randomization:  sex ~ strata + ecog  ( Simple )
#> 
#> Contrast     :  Log Hazard ratio
#> 
#>                  Estimate Std.Err Z Value Pr(>|z|)   
#> Male v.s. Female  0.55482 0.17063  3.2516 0.001147 **
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Test         :  Log-Rank
#> 
#>                  Test Stat. Pr(>|z|)   
#> Male v.s. Female     3.2856 0.001018 **
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Unadjusted for covariates and stratification:
robin_surv(
  formula = Surv(time, status) ~ 1,
  data = surv_data,
  treatment = sex ~ 1
)
#> Model        :  Surv(time, status) ~ 1 
#> Randomization:  sex ~ 1  ( Simple )
#> 
#> Contrast     :  Log Hazard ratio
#> 
#>                  Estimate Std.Err Z Value Pr(>|z|)   
#> Male v.s. Female  0.53343 0.16727   3.189 0.001428 **
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Test         :  Log-Rank
#> 
#>                  Test Stat. Pr(>|z|)   
#> Male v.s. Female     3.2135 0.001311 **
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
```
