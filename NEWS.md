# RobinCar2 0.2.0

### New features

* Add survival analysis with log-rank test and hazard ratio estimation.
* Add `log_odds_ratio` and `log_risk_ratio` function for inference.
* Add `contrast_mat` to `treatment_effect` object.
* Add `confint` to obtain confidence interval for `prediction_cf` and `treatment_effect` object.

### Bug Fix

* Previously in `robin_glm`, `vcovG` is always used regardless the variance type argument. This is fixed now.

### Misc

* Reorganized the output structure of `robin_glm` and `robin_lm`.

# RobinCar2 0.1.1

### New features

* Add unbiased counter-factual prediction.
* Add robust sandwich variance for the marginal treatment effect.
