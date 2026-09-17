# RobinCar2 0.2.4

### Breaking Changes

* Models carrying an offset, supplied either as `offset()` in the model formula or via the `offset` argument, are now rejected with an error by `robin_glm()`, `robin_lm()`, `predict_counterfactual()` and `treatment_effect()`. Such models previously returned estimates silently, and those estimates were wrong: the offset was dropped from the counterfactual predictions but retained in the residuals used for the bias correction, so the two were on different scales and the estimate did not target the marginal mean or any other interpretable quantity (reported in #117).

  The error is deliberate rather than a repair of that mismatch. Offsets are used almost exclusively to express a rate per unit exposure time, and there is no established covariate-adjusted rate estimand for RobinCar2 to target: the theory the package implements (Bannick et al., 2024) develops marginal means of the response and does not treat exposure time at all. Beyond that, predicting at each subject's own exposure makes the prediction depend on a variable whose distribution can itself be affected by treatment, which the robustness argument for the AIPW estimator excludes, and it collapses a ratio contrast to the model coefficient `exp(beta)` rather than a marginal quantity.

  For a gaussian model with an identity link the offset is only a known additive shift, so there is an exact restatement: fit `I(y - z) ~ treatment + ...` in place of `y ~ treatment + ... + offset(z)`, which gives identical coefficients, standard errors and treatment contrasts, with marginal means shifted by `mean(z)`. For any other link there is no equivalent restatement, and users who need a rate have no supported route in this release. See #117 for discussion.

### New Features

* The new `surv_control` argument in `robin_surv()` allows to fine-control the root finding algorithm used for the hazard ratio estimation.

### Bug Fixes

* Previously `robin_surv()` gave small numerical differences to `survival::coxph()` for the hazard ratio estimate. This is now fixed.

### Misc

* Adapted a unit test to a change in `stats::model.frame.glm()` in `R-devel`, which now enforces the factor levels recorded in the model fit, consistent with `stats::model.frame.lm()`. As a consequence, `predict_counterfactual()` requires the factor levels of the treatment variable in `data` to match those of `fit`. Previously, mismatched levels were silently accepted for `glm` fits; this restriction has always applied to `lm` fits.

# RobinCar2 0.2.3

### Bug Fixes

* Fixed a bug in `predict_counterfactual` that the ordering of the treatment levels affect the order of the marginal mean.

### Misc

* Added Biometric Bulletin vignette article.

# RobinCar2 0.2.2

### New features

* The user interface for `robin_surv` has been improved for specifying analysis stratification variables directly as part of the model formula, similar to how it is done in `survival::coxph`. In addition, the randomization scheme is now specified as for the other `RobinCar2` functions for consistency. A warning will be issued when the randomization strata are not adequately included in the analysis model. See the updated vignette for details.

* It is now possible to only perform the log rank test, without estimating the log hazard ratio, in `robin_surv` by setting the argument `contrast = "none"`. This can be useful e.g. when performing simulation studies focusing only on the log-rank test operating characteristics, because the log-rank test is computationally less expensive than estimating the hazard ratio.

### Bug Fixes

* Fixed a bug in covariate-adjusted stratified survival function estimation in `robin_surv` which could occur when there are character covariates with values only appearing in one stratum, which could have failed or lead to incorrect results. 
* Fixed a issue in `robin_lm` that variance method does not apply correctly.
* Fixed a issue in `robin_glm` that `vcovHC` could previously be used for non-Gaussian family.

* Fixed another bug in covariate-adjusted stratified survival function estimation in `robin_surv`, which resulted from design matrices separately derived per stratum. Now the design matrix is created once including the stratum indicator, and then the stratum-specific parts are extracted as needed.

### Misc

* Changed from `sp` to `sr` which is easier to read as "simple randomization".
* The print output for `robin_surv` objects has been improved for better readability.

# RobinCar2 0.2.1

### New features

* Now multiple stratification variables are supported in `robin_surv` by adding them on the right-hand side of the `treatment` formula.

### Bug Fixes

* There will be no more spurious warnings in `robin_surv` during the hazard ratio estimation coming from the variance calculation: Now the variance is only calculated after the hazard ratio estimate is obtained.
* When adjusting for a single factor covariate, or when the covariate is strongly correlated with a strata variable in `robin_surv`, the function now works correctly and does not fail with a "singular design matrix" error any longer.
* Fixed a bug in covariate-adjusted survival function estimation in `robin_surv` which could occur when there are character covariates with values only appearing in one treatment group, which could have led to incorrect results. 

# RobinCar2 0.2.0

### New features

* Add survival analysis with log-rank test and hazard ratio estimation.
* Add `log_odds_ratio` and `log_risk_ratio` function for inference.
* Add `contrast_mat` to `treatment_effect` object.
* Add `confint` to obtain confidence interval for `prediction_cf` and `treatment_effect` object.

### Bug Fixes

* Previously in `robin_glm`, `vcovG` is always used regardless the variance type argument. This is fixed now.

### Misc

* Reorganized the output structure of `robin_glm` and `robin_lm`.

# RobinCar2 0.1.1

### New features

* Add unbiased counter-factual prediction.
* Add robust sandwich variance for the marginal treatment effect.
