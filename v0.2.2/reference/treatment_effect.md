# Treatment Effect

Obtain treatment effect and variance from counter-factual prediction

## Usage

``` r
treatment_effect(
  object,
  pair = pairwise(names(object$estimate)),
  eff_measure,
  eff_jacobian = eff_jacob(eff_measure),
  contrast_name,
  ...
)

difference(object, ...)

risk_ratio(object, ...)

odds_ratio(object, ...)

log_risk_ratio(object, ...)

log_odds_ratio(object, ...)
```

## Arguments

- object:

  Object from which to obtain treatment effect.

- pair:

  (`contrast`) Contrast choices.

- eff_measure:

  (`function`) Treatment effect measurement function.

- eff_jacobian:

  (`function`) Treatment effect jacobian function.

- contrast_name:

  (`string`) Name of the contrast.

- ...:

  Additional arguments for variance.

## Value

A list of `treatment_effect` object with following elements:

- `estimate`: estimate of the treatment effect.

- `pair`: `contrast` object indicating the pairwise treatment effect.

- `contrast`: name of the contrast function.

- `euqal_val`: the value for no treatment effect given the contrast.

- `variance`: the variance of the treatment effect.

- `jacobian`: the Jacobian matrix.

- `contrast_mat`: contrast summary matrix.
