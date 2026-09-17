# Counterfactual Prediction

Obtain counterfactual prediction of a fit.

## Usage

``` r
predict_counterfactual(fit, treatment, data, vcov, vcov_args, ...)
```

## Arguments

- fit:

  fitted object.

- treatment:

  (`formula`) formula of form treatment ~ strata(s).

- data:

  (`data.frame`) raw dataset.

- vcov:

  (`function` or `character`) variance function or name.

- vcov_args:

  (`list`) additional arguments for variance function.

- ...:

  Additional arguments for methods.

## Value

List of class `prediction_cf` containing following elements:

- `estimate`: predicted marginal mean.

- `residual`: residual of the bias-corrected prediction.

- `predictions`: all predicted values.

- `predictions_liner`: linear predictions.

- `schema`: randomization schema.

- `response`: response value.

- `fit`: fitted model.

- `model_matrix`: model matrix.

- `treatment_formula`: treatment assignment and randomization formula.

- `treatment`: treatment value.

- `group_idx`: group index based on the stratification.

- `variance`: estimated variance of the marginal mean.

- `variance_name`: name of the variance.
