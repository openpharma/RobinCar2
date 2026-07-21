# Control Survival Analysis Root Finding

Control parameters for the root-finding algorithm used by
[`robin_surv()`](https://openpharma.github.io/RobinCar2/reference/robin_surv.md)
to estimate the log hazard ratio.

## Usage

``` r
surv_control(tol = .Machine$double.eps^0.25, maxiter = 1000, trace = 0)
```

## Arguments

- tol:

  (`number`) The desired accuracy, passed to
  [`stats::uniroot()`](https://rdrr.io/r/stats/uniroot.html).

- maxiter:

  (`count`) The maximum number of iterations, passed to
  [`stats::uniroot()`](https://rdrr.io/r/stats/uniroot.html).

- trace:

  (`count`) Tracing level, passed to
  [`stats::uniroot()`](https://rdrr.io/r/stats/uniroot.html).

## Value

A named list with elements `tol`, `maxiter`, and `trace`.
