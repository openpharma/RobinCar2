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
