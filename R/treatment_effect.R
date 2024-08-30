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
treatment_effect.prediction_cf <- function(
    object, pair = names(object), variance = vcovANHECOVA, eff_measure, eff_jacobian, ...) {
  assert(
    test_function(variance),
    test_null(variance)
  )
  assert_function(eff_measure)
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
      assert_function(eff_jacobian)
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
    fit = attr(object, "fit"),
    vartype = deparse(substitute(variance)),
    treatment = attr(object, "treatment_formula"),
    effects = trt_effect,
    variance = diag(trt_var),
    class = "treatment_effect"
  )
}


#' @export
#' @inheritParams predict_counterfactual
treatment_effect.lm <- function(
    object, pair = names(object), variance = vcovANHECOVA, eff_measure, eff_jacobian,
    treatment, data = find_data(object), unbiased = TRUE, ...) {
  pc <- predict_counterfactual(object, data = data, treatment, unbiased)
  if (missing(pair)) {
    treatment_effect(pc, pair = , , variance = variance, eff_measure = eff_measure, eff_jacobian = eff_jacobian, ...)
  } else {
    treatment_effect(pc, pair, variance = variance, eff_measure = eff_measure, eff_jacobian = eff_jacobian, ...)
  }
}

#' @export
treatment_effect.glm <- function(
    object, pair, variance = vcovANHECOVA, eff_measure, eff_jacobian,
    treatment, data = find_data(object), unbiased = TRUE, ...) {
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

#' Contrast Functions and Jacobians
#' @rdname contrast
#' @param x (`numeric`) Vector of values.
#' @return Vector of contrasts, or matrix of jacobians.
#' @examples
#' h_diff(1:3)
#' h_jac_ratio(1:3)
#' @export
h_diff <- function(x) {
  assert_numeric(x)
  d <- outer(x, x, `-`)
  d[lower.tri(d)]
}

#' @rdname contrast
#' @export
h_jac_diff <- function(x) {
  assert_numeric(x)
  n <- length(x)
  l <- h_lower_tri_idx(n)
  ret <- matrix(0, nrow = nrow(l), ncol = n)
  ret[cbind(seq_len(nrow(ret)), l[, 1])] <- 1
  ret[cbind(seq_len(nrow(ret)), l[, 2])] <- -1
  ret
}

#' @rdname contrast
#' @export
h_ratio <- function(x) {
  assert_numeric(x, lower = 0)
  d <- outer(x, x, `/`)
  d[lower.tri(d)]
}

#' @rdname contrast
#' @export
h_jac_ratio <- function(x) {
  assert_numeric(x, lower = 0)
  n <- length(x)
  l <- h_lower_tri_idx(n)
  ret <- matrix(0, nrow = nrow(l), ncol = n)
  ret[cbind(seq_len(nrow(ret)), l[, 1])] <- 1 / x[l[, 2]]
  ret[cbind(seq_len(nrow(ret)), l[, 2])] <- -x[l[, 1]] / x[l[, 2]]^2
  ret
}

#' @rdname contrast
#' @export
h_odds_ratio <- function(x) {
  assert_numeric(x, lower = 0, upper = 1)
  y <- x / (1 - x)
  h_ratio(y)
}

#' @rdname contrast
#' @export
h_jac_odds_ratio <- function(x) {
  assert_numeric(x, lower = 0)
  n <- length(x)
  l <- h_lower_tri_idx(n)
  ret <- matrix(0, nrow = nrow(l), ncol = n)
  ret[cbind(seq_len(nrow(ret)), l[, 1])] <- (1 - x[l[, 2]]) / ((1 - x[l[, 1]])^2 * x[l[, 2]])
  ret[cbind(seq_len(nrow(ret)), l[, 2])] <- -x[l[, 1]] / ((1 - x[l[, 1]]) * x[l[, 2]]^2)
  ret
}

#' Lower Triangular Index
#' @param n (`int`) Number of rows/columns.
#' @return Matrix of lower triangular indices.
#' @keywords internal
h_lower_tri_idx <- function(n) {
  rc <- c(n, n)
  which(.row(rc) > .col(rc), arr.ind = TRUE)
}

#' @export
print.treatment_effect <- function(x, ...) {
  cat("Treatment Effect\n")
  cat("-------------\n")
  cat("Model        : ", deparse(attr(x, "fit")$formula), "\n")
  cat("Randomization: ", deparse(attr(x, "treatment")), "\n")
  cat("Variance Type: ", attr(x, "vartype"), "\n")
  trt_sd <- sqrt(attr(x, "variance"))
  z_value <- x / trt_sd
  p <- pnorm(abs(z_value), lower.tail = FALSE)
  coef_mat <- matrix(
    c(
      x,
      trt_sd,
      z_value,
      p
    ),
    nrow = length(x)
  )
  colnames(coef_mat) <- c("Estimate", "Std.Err", "Z Value", "Pr(>z)")
  row.names(coef_mat) <- attr(x, "name")
  stats::printCoefmat(
    coef_mat,
    zap.ind = 3,
    digits = 3
  )
}
