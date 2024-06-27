#' Treatment Effect
#' @description Obtain treatment effect and variance from counter-factual prediction
#'
#' @param object Object from which to obtain treatment effect.
#' @param pair (`integer` or `character`) Names or index of the treatment levels.
#' @param variance (`function`) Variance function.
#' @param eff_measure (`function`) Treatment effect measurement function.
#' @param eff_jacobian (`function`) Treatment effect jacobian function.
#' @param ... Additional arguments for variance.
#'
#' @export
treatment_effect <- function(object, pair, variance, eff_measure, eff_jacobian, ...) {
  UseMethod("treatment_effect")
}

#' @export
treatment_effect.prediction_cf <- function(object, pair = names(object), variance = vcovANHECOVA, eff_measure, eff_jacobian, ...) {
  assert(
    test_function(variance),
    test_null(variance)
  )
  assert_function(eff_measure)
  assert_function(eff_jacobian)
  assert_vector(pair)
  assert(
    test_subset(pair, names(object)),
    test_integerish(pair, lower = 1L, upper = length(object))
  )
  if (test_integerish(pair)) {
    pair <- names(object)[pair]
  }
  trt_effect <- unname(eff_measure(object[pair]))
  if (!is.null(variance)) {
    inner_variance <- variance(object, ...)[pair, pair]
    if (missing(eff_jacobian)) {
      trt_jac <- numDeriv::jacobian(eff_measure, object[pair])
    } else {
      trt_jac <- eff_jacobian(object[pair])
    }
    trt_var <- trt_jac %*% inner_variance %*% t(trt_jac)
  } else {
    trt_var <- diag(NULL)
  }

  pair_names <- outer(pair, pair, FUN = paste, sep = " - ")
  structure(
    .Data = trt_effect,
    name = pair_names[lower.tri(pair_names)],
    variance = diag(trt_var),
    class = "treatment_effect"
  )
}


#' @export
#' @inheritParams predict_counterfactual
treatment_effect.lm <- function(object, pair = names(object), variance = vcovANHECOVA, eff_measure, eff_jacobian, treatment, data, unbiased = TRUE, ...) {
  pc <- predict_counterfactual(object, treatment, data, unbiased)
  if (missing(pair)) {
    treatment_effect(pc, pair = , , variance = variance, eff_measure = eff_measure, eff_jacobian = eff_jacobian, ...)
  } else {
    treatment_effect(pc, pair, variance = variance, eff_measure = eff_measure, eff_jacobian = eff_jacobian, ...)
  }
}

#' @export
treatment_effect.glm <- function(object, pair, variance = vcovANHECOVA, eff_measure, eff_jacobian, treatment, data = object$data, unbiased = TRUE, ...) {
  pc <- predict_counterfactual(object, treatment, data, unbiased)
  if (missing(pair)) {
    treatment_effect(pc, pair = , , variance = variance, eff_measure = eff_measure, eff_jacobian = eff_jacobian, ...)
  } else {
    treatment_effect(pc, pair, variance = variance, eff_measure = eff_measure, eff_jacobian = eff_jacobian, ...)
  }
}

#' @rdname treatment_effect
difference <- function(object, ...) {
  treatment_effect(object, eff_measure = h_diff, eff_jacobian = h_jac_diff, ...)
}
#' @rdname treatment_effect
risk_ratio <- function(object, ...) {
  treatment_effect(object, eff_measure = h_ratio, eff_jacobian = h_jac_ratio, ...)
}
#' @rdname treatment_effect
odds_ratio <- function(object, ...) {
  treatment_effect(object, eff_measure = h_odds_ratio, eff_jacobian = h_jac_odds_ratio, ...)
}

h_diff <- function(x) {
  assert_numeric(x)
  d <- outer(x, x, `-`)
  d[lower.tri(d)]
}
h_jac_diff <- function(x) {
  assert_numeric(x)
  n <- length(x)
  if (n == 2) {
    matrix(c(-1, 1), nrow = 1L)
  } else {
    jacobian(h_diff, x)
  }
}

h_ratio <- function(x) {
  assert_numeric(x, lower = 0)
  d <- outer(x, x, `/`)
  d[lower.tri(d)]
}
h_jac_ratio <- function(x) {
  assert_numeric(x, lower = 0)
  n <- length(x)
  if (n == 2) {
    matrix(c(-x[2] / x[1]^2, 1 / x[1]), nrow = 1L)
  } else {
    jacobian(h_ratio, x)
  }
}

h_odds_ratio <- function(x) {
  assert_numeric(x, lower = 0, upper = 1)
  y <- x / (1 - x)
  h_ratio(y)
}

h_jac_odds_ratio <- function(x) {
  assert_numeric(x, lower = 0, upper = 1)
  n <- length(x)
  if (n == 2) {
    matrix(c(-x[2] / ((1 - x[2]) * x[1]^2), (1 - x[1]) / ((1 - x[2])^2 * x[1])), nrow = 1L)
  } else {
    jacobian(h_odds_ratio, x)
  }
}
