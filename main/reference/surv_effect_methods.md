# S3 Methods for `surv_effect`

S3 Methods for `surv_effect`

## Usage

``` r
# S3 method for class 'surv_effect'
print(x, ...)

table(x, ...)

# Default S3 method
table(x, ...)

# S3 method for class 'surv_effect'
table(x, ...)
```

## Arguments

- x:

  (`surv_effect`) the obtained result from
  [`robin_surv()`](https://openpharma.github.io/RobinCar2/reference/robin_surv.md).

- ...:

  ignored additional arguments (for compatibility).

## Functions

- `print(surv_effect)`: prints the `surv_effect` object.

- `table(surv_effect)`: prints and returns invisibly the events table of
  the `surv_effect` object.

## Examples

``` r
x <- robin_surv(
  formula = Surv(time, status) ~ meal.cal + age + strata(strata),
  data = surv_data,
  treatment = sex ~ pb(strata)
)
print(x)
#> Model        :  Surv(time, status) ~ meal.cal + age + strata(strata) 
#> Randomization:  sex ~ pb(strata)  ( Permuted-Block )
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
table(x)
#> Number of patients and events per stratum and treatment arm:
#>   strata    sex Patients Events
#> 1      0 Female       20      6
#> 2      0   Male       30     24
#> 3      1 Female       29     21
#> 4      1   Male       57     43
#> 5      2 Female       18     14
#> 6      2   Male       25     24
#> 7      3   Male        1      1
```
