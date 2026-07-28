# RobinCar2: ROBust INference for Covariate Adjustment in Randomized Clinical Trials

## Introduction

Covariate adjustment is a powerful statistical technique that can
increase efficiency in randomized clinical trials (RCTs) by reducing
variability in treatment effect estimates. The U.S. Food and Drug
Administration (FDA) finalized the guideline on covariate adjustment,
providing recommendations and best practices for using these methods in
drug development (FDA, 2023). However, a gap has existed between the
extensive statistical literature on covariate adjustment and software
that is easy to use and follows these best practices.

The **RobinCar2** R package, which stands for **ROB**ust **IN**ference
for **C**ovariate **A**djustment in **R**andomized clinical trials,
addresses this gap. It is a streamlined version of the original
**RobinCar** package (Bannick et al., 2026a), designed with minimal
dependencies and extensive validation for use in drug development,
particularly for Good Practice (GxP) purposes. The package is supported
by the ASA Biopharmaceutical Section Covariate Adjustment Scientific
Working Group Software Subteam.

This paper provides an introduction to **RobinCar2**, covering its core
functionality for three common outcome types in clinical trials:
continuous, binary, and time-to-event outcomes. It also provides best
practices of using **RobinCar** and **RobinCar2** packages for covariate
adjustment.

## Covariate Adjustment in RCTs

Covariate adjustment leverages baseline variables to improve the
precision of treatment effect estimates. Unlike traditional regression
interpretations, covariate-adjusted estimators in RCTs target the same
unconditional (marginal) treatment effect as unadjusted analyses, but
with potentially smaller variance. This is because randomization ensures
treatment assignment is independent of baseline covariates, allowing
model-assisted approaches that are robust to model misspecification.

**RobinCar2** supports three covariate-adaptive randomization schemes:

- Simple randomization (`sr`): Subjects are randomly assigned to
  treatment groups without stratification.
- Permuted-block randomization (`pb`): Treatment assignments are
  balanced within blocks defined by stratification factors.
- Pocock-Simon minimization (`ps`): An adaptive method that minimizes
  imbalance across multiple stratification factors.

The package provides two variance estimation approaches:

- `vcovG`: The default heteroskedasticity-consistent variance estimator
  that accounts for covariate-adaptive randomization.
- `vcovHC`: The Huber-White sandwich estimator, which is appropriate for
  linear covariate adjustment and only when treatment-by-covariate
  interactions are not included in the model.

## Analysis of Continuous Outcomes

For continuous outcomes, **RobinCar2** provides the
[`robin_lm()`](https://openpharma.github.io/RobinCar2/reference/robin_lm.md)
function, which fits a linear model and returns covariate-adjusted
treatment effect estimates with robust inference. The following code
fits an ANHECOVA (Analysis of Heterogeneous Covariance) model (Ye et
al., 2023a), which includes the treatment assignment (`treatment`), the
stratification factor (`s1`), the treatment-by-stratification
interaction (`treatment * s1`), and a continuous covariate (`covar`).
The randomization scheme is permuted-block randomization stratified by
`s1`, specified as `treatment ~ pb(s1)`. The variance estimation method
is `vcovG`.

``` r

library(RobinCar2)

result_lm <- robin_lm(
  y ~ treatment * s1 + covar,
  data = glm_data,
  treatment = treatment ~ pb(s1),
  vcov = "vcovG"
)

print(result_lm)
```

    ## Model        :  y ~ treatment * s1 + covar 
    ## Randomization:  treatment ~ pb(s1)  ( Permuted-Block )
    ## Variance Type:  vcovG 
    ## Marginal Mean: 
    ##      Estimate  Std.Err    2.5 % 97.5 %
    ## pbo  0.200321 0.067690 0.067651 0.3330
    ## trt1 0.763971 0.075929 0.615152 0.9128
    ## trt2 0.971250 0.076543 0.821228 1.1213
    ## 
    ## Contrast     :  h_diff
    ##                Estimate Std.Err Z Value  Pr(>|z|)    
    ## trt1 v.s. pbo   0.56365 0.10074  5.5952 2.203e-08 ***
    ## trt2 v.s. pbo   0.77093 0.10133  7.6082 2.779e-14 ***
    ## trt2 v.s. trt1  0.20728 0.10683  1.9402   0.05235 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

The output has three main parts. The first part reiterates the input
parameters and the model specification. The second part provides results
on **Marginal Mean**, which includes estimated response means for each
treatment group with standard errors and confidence intervals. The last
part provides the **Contrast** results, including pairwise treatment
comparisons (the difference in means) with test statistics and p-values.
The Huber-White variance estimator can be applied when the linear model
does not include treatment-by-stratification/covariate interactions, by
specifying `vcov = "vcovHC"` (Rosenblum and van der Laan, 2009; Lin,
2013). The confidence interval of treatment effect contrasts can be
obtained via the
[`confint()`](https://openpharma.github.io/RobinCar2/reference/confint.md)
function.

## Analysis of Binary and Count Outcomes

For binary and count outcomes, **RobinCar2** provides
[`robin_glm()`](https://openpharma.github.io/RobinCar2/reference/robin_glm.md),
which extends the framework to generalized linear models (Ye et al.,
2023b, Bannick et al. 2025). The following code fits a logistic model
(`family = binomial(link = "logit")`), which includes the treatment
assignment (`treatment`), the stratification factor (`s1`), the
treatment-by-stratification interaction (`treatment * s1`), and a
continuous covariate (`covar`). The randomization scheme is
permuted-block randomization stratified by `s1`, specified as
`treatment ~ pb(s1)`. Currently, `vcovG` is the only supported method
for variance estimation in generalized linear models.

``` r

result_binary <- robin_glm(
  y_b ~ treatment * s1 + covar,
  data = glm_data,
  treatment = treatment ~ pb(s1),
  family = binomial(link = "logit"),
  contrast = "difference"
)

print(result_binary)
```

    ## Model        :  y_b ~ treatment * s1 + covar 
    ## Randomization:  treatment ~ pb(s1)  ( Permuted-Block )
    ## Variance Type:  vcovG 
    ## Marginal Mean: 
    ##      Estimate  Std.Err    2.5 % 97.5 %
    ## pbo  0.356097 0.033599 0.290243 0.4219
    ## trt1 0.580696 0.034418 0.513238 0.6482
    ## trt2 0.621386 0.034019 0.554711 0.6881
    ## 
    ## Contrast     :  difference
    ##                Estimate  Std.Err Z Value  Pr(>|z|)    
    ## trt1 v.s. pbo  0.224599 0.047711  4.7075 2.508e-06 ***
    ## trt2 v.s. pbo  0.265290 0.047534  5.5810 2.391e-08 ***
    ## trt2 v.s. trt1 0.040691 0.047941  0.8488     0.396    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

The output of
[`robin_glm()`](https://openpharma.github.io/RobinCar2/reference/robin_glm.md)
has a similar structure to
[`robin_lm()`](https://openpharma.github.io/RobinCar2/reference/robin_lm.md).
The default contrast for binary outcomes is the difference in
probabilities (`contrast = "difference"`). **RobinCar2** supports other
contrast functions, including risk ratio, odds ratio, and their log
transformations (`"log_risk_ratio"`, `"log_odds_ratio"`). The confidence
interval of treatment effect contrasts can be obtained via the
[`confint()`](https://openpharma.github.io/RobinCar2/reference/confint.md)
function. Any family argument handled by
[`glm()`](https://rdrr.io/r/stats/glm.html) can be used with
[`robin_glm()`](https://openpharma.github.io/RobinCar2/reference/robin_glm.md).

## Analysis of Time-to-Event Outcomes

For survival outcomes, **RobinCar2** provides
[`robin_surv()`](https://openpharma.github.io/RobinCar2/reference/robin_surv.md),
which implements stratified and covariate-adjusted log-rank tests and
hazard ratio estimation (Ye et al., 2024). The following code fits a
stratified log-rank test and estimates hazard ratios, stratified by the
factor `strata`. The treatment variable (`sex`) is specified via the
`treatment` formula. The randomization scheme is permuted-block
randomization stratified by `strata`, specified as `sex ~ pb(strata)`.

``` r

result_tte <- robin_surv(
  Surv(time, status) ~ 1 + strata(strata),
  data = surv_data,
  treatment = sex ~ pb(strata)
)

print(result_tte)
```

    ## Model        : Surv(time, status) ~ 1 + strata(strata)
    ## Randomization: sex ~ pb(strata) (Permuted-Block)
    ## Stratification variables:  strata 
    ## 
    ## Contrast     : Stratified Log Hazard Ratio
    ## 
    ##                  Estimate Std.Err Z Value Pr(>|z|)   
    ## Male v.s. Female  0.55482 0.17063  3.2516 0.001147 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Test         : Stratified Log-Rank
    ## 
    ##                  Test Stat. Pr(>|z|)   
    ## Male v.s. Female     3.2856 0.001018 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

The output of
[`robin_surv()`](https://openpharma.github.io/RobinCar2/reference/robin_surv.md)
has a similar structure to
[`robin_lm()`](https://openpharma.github.io/RobinCar2/reference/robin_lm.md)
and
[`robin_glm()`](https://openpharma.github.io/RobinCar2/reference/robin_glm.md).
The **Contrast** section provides unconditional (marginal) hazard ratios
for all pairwise treatment comparisons. The **Test** section provides
the log-rank test results. The confidence interval of hazard ratios can
be obtained via the
[`confint()`](https://openpharma.github.io/RobinCar2/reference/confint.md)
function.

## Best Practices of using **RobinCar** and **RobinCar2**

**RobinCar2** is a streamlined version of the original **RobinCar**
package with the following characteristics (Bannick et al., 2026b):

| Feature | **RobinCar** | **RobinCar2** |
|----|----|----|
| Dependencies | More extensive | Minimal |
| Validation | Standard | GxP-ready |
| Methods | Comprehensive | Curated subset |
| Support | Research community | ASA BIOP Covariate Adjustment Working Group |

Methods included in **RobinCar2** have undergone additional validation
and are recommended for interactions with regulatory agencies. Previous
users of **RobinCar** should therefore consider transitioning to
**RobinCar2** when possible.

## Conclusions

**RobinCar2** provides a validated, user-friendly implementation of
covariate adjustment methods for randomized clinical trials. Its simple
interface, based on familiar R formula syntax, makes it accessible to
clinical trial statisticians while ensuring robust inference aligned
with the FDA guideline. The package covers the most common outcome types
encountered in clinical trials: continuous, binary, and time-to-event
outcomes. Future development of **RobinCar2** may include additional
variance estimation methods, the Mantel-Haenszel risk difference
estimator (currently available in **RobinCar**), and bootstrap methods.

For more information, including additional vignettes and documentation,
visit the package GitHub repository at
[github.com/openpharma/RobinCar2](https://github.com/openpharma/RobinCar2).

## Acknowledgements

This package is supported by [ASA Biopharmaceutical Section Covariate
Adjustment Scientific Working Group Software
Subteam](https://carswg.github.io/subteam_software.html).

## References

Bannick M, Shao J, Liu J, Du Y, Yi Y, Ye T (2025). A General Form of
Covariate Adjustment in Randomized Clinical Trials. *Biometrika*, 112(3)
asaf029.

Bannick M, Qian Y, Ye T, Yi Y, Bian F (2026a). RobinCar: Robust
Inference for Covariate Adjustment in Randomized Clinical Trials. R
package version 1.1.0, <https://CRAN.R-project.org/package=RobinCar>.

Bannick M, Bian Y, Chen G, Li L, Qian Y, Sabanés Bové D, Xi D, Ye T, Yi
Y (2026b). The RobinCar Family: R Tools for Robust Covariate Adjustment
in Randomized Clinical Trials. *arXiv preprint*, arXiv:2601.14498.

Food and Drug Administration (2023). Adjusting for Covariates in
Randomized Clinical Trials for Drugs and Biological Products: Final
Guidance for Industry, <https://www.fda.gov/media/148910/download>.

Lin W (2013). Agnostic Notes on Regression Adjustments to Experimental
Data: Reexamining Freedman’s Critique. *Annals of Applied Statistics*,
7(1):295-318.

Rosenblum M, van der Laan MJ (2009). Using Regression Models to Analyze
Randomized Trials: Asymptotically Valid Hypothesis Tests Despite
Incorrectly Specified Models. *Biometrics*, 65(3):937-945.

Ye T, Shao J, Yi Y, Zhao Q (2023a). Toward Better Practice of Covariate
Adjustment in Analyzing Randomized Clinical Trials. *Journal of the
American Statistical Association*, 118(544):2370-2381.

Ye T, Bannick M, Yi Y, Shao J (2023b). Robust Variance Estimation for
Covariate-Adjusted Unconditional Treatment Effect in Randomized Clinical
Trials with Binary Outcomes. *Statistical Theory and Related Fields*,
7(2):159-163.

Ye T, Shao J, Yi Y (2024). Covariate-Adjusted Log-Rank Test: Guaranteed
Efficiency Gain and Universal Applicability. *Biometrika*,
111(2):691-705.
