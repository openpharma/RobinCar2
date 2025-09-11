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
