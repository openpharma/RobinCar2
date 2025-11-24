# Prepare Survival Input

Prepare Survival Input

## Usage

``` r
h_prep_survival_input(formula, data, treatment)
```

## Arguments

- formula:

  (`formula`) with a left hand side of the form `Surv(time, status)` and
  a right hand side defining optional covariates or just `1` if there
  are no covariates.

- data:

  (`data.frame`) containing the variables in the formula.

- treatment:

  (`string` or `formula`) string name of the treatment, or a formula.

## Value

A list containing the following elements:

- `data`: The potentially updated data set.

- `time`: Name of the time variable.

- `status`: Name of the status variable.

- `treatment`: Name of the treatment variable.

- `strata`: Name of the strata variable.

- `schema`: Randomization schema.

- `covariates`: Names of the covariates in the model.

- `model`: A formula only including the covariates, but not treatment or
  strata variables.

- `n_levels`: Number of treatment levels.

- `levels`: Names of the treatment levels.

## Details

Note that `formula` can also contain an externally defined
[survival::Surv](https://rdrr.io/pkg/survival/man/Surv.html) object. In
this case, the `time` and `status` variables are extracted and added to
the `data` input. Note that it is up to the user to ensure that in this
case the column binding is correct, i.e., that the rows of the `data`
match with the rows of the `Surv` object. In addition, the same named
variables must not appear in both the `data` and the `Surv` object, to
avoid ambiguity (this is a difference vs. the behavior of
[`survival::coxph()`](https://rdrr.io/pkg/survival/man/coxph.html) for
better transparency).
