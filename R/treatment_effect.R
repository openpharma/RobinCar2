#' Treatment Effect
#' @description Obtain treatment effect and variance from counter-factual prediction
#'
#' @param object Object from which to obtain treatment effect.
#' @param pair (`contrast`) Contrast choices.
#' @param eff_measure (`function`) Treatment effect measurement function.
#' @param eff_jacobian (`function`) Treatment effect jacobian function.
#' @param ... Additional arguments for variance.
#'
#' @export
treatment_effect <- function(
    object, pair = pairwise(names(object)), eff_measure,
    eff_jacobian = eff_jacob(eff_measure), ...) {
  UseMethod("treatment_effect")
}

#' @export
treatment_effect.prediction_cf <- function(
    object, pair = pairwise(names(object)), eff_measure, eff_jacobian = eff_jacob(eff_measure), ...) {
  assert_function(eff_measure)
  assert_class(pair, "contrast")
  # make sure levels match
  pair <- update_levels(pair, names(object))
  trt_effect <- unname(eff_measure(object[pair[[1]]], object[pair[[2]]]))
  trt_jac <- eff_jacobian(object[pair[[1]]], object[pair[[2]]])
  trt_jac_mat <- jac_mat(trt_jac, pair)
  equal_val <- eff_measure(object[1], object[1])
  structure(
    .Data = trt_effect,
    pair = pair,
    contrast = deparse(substitute(eff_measure)),
    equal_val = equal_val,
    marginal_mean = object,
    fit = attr(object, "fit"),
    treatment = attr(object, "treatment_formula"),
    variance = trt_jac_mat %*% attr(object, "variance") %*% t(trt_jac_mat),
    jacobian = trt_jac_mat,
    class = "treatment_effect"
  )
}


#' @export
#' @inheritParams predict_counterfactual
treatment_effect.lm <- function(
    object, pair, eff_measure, eff_jacobian = eff_jacob(eff_measure),
    vcov = "vcovG", vcov_args = list(), treatment, data = find_data(object), ...) {
  pc <- predict_counterfactual(object, data = data, treatment, vcov = vcov, vcov_args = vcov_args)
  if (missing(pair)) {
    pair <- pairwise(names(pc))
  }
  treatment_effect(pc, pair = pair, eff_measure = eff_measure, eff_jacobian = eff_jacobian, ...)
}

#' @export
treatment_effect.glm <- function(
    object, pair, eff_measure, eff_jacobian = eff_jacob(eff_measure),
    vcov = "vcovG", vcov_args = list(), treatment, data = find_data(object), ...) {
  pc <- predict_counterfactual(object, treatment, data, vcov = vcov, vcov_args = vcov_args)
  if (missing(pair)) {
    pair <- pairwise(names(pc))
  }
  treatment_effect(pc, pair = pair, eff_measure = eff_measure, eff_jacobian = eff_jacobian, ...)
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
#' h_diff(1:3, 4:6)
#' h_jac_ratio(1:3, 4:6)
#' @export
h_diff <- function(x, y) {
  assert_numeric(x)
  assert_numeric(y, len = length(x))
  x - y
}

#' @rdname contrast
#' @export
h_jac_diff <- function(x, y) {
  assert_numeric(x)
  assert_numeric(y, len = length(x))
  n <- length(x)
  matrix(c(1, -1), nrow = n, ncol = 2, byrow = TRUE)
}

#' @rdname contrast
#' @export
h_ratio <- function(x, y) {
  assert_numeric(x, lower = 0)
  assert_numeric(y, lower = 0, len = length(x))
  x / y
}

#' @rdname contrast
#' @export
h_jac_ratio <- function(x, y) {
  assert_numeric(x, lower = 0)
  assert_numeric(y, lower = 0, len = length(x))
  cbind(1 / y, -x / y^2)
}

#' @rdname contrast
#' @export
h_odds_ratio <- function(x, y) {
  assert_numeric(x, lower = 0, upper = 1)
  assert_numeric(y, lower = 0, upper = 1, len = length(x))
  h_ratio(x / (1 - x), y / (1 - y))
}

#' @rdname contrast
#' @export
h_jac_odds_ratio <- function(x, y) {
  assert_numeric(x, lower = 0)
  assert_numeric(y, lower = 0, upper = 1, len = length(x))
  cbind(1 / (1 - x)^2 / y * (1 - y), -x / (1 - x) / y^2)
}

#' @rdname contrast
#' @param f (`function`) Function with argument x and y to compute treatment effect.
#' @export
eff_jacob <- function(f) {
  assert_function(f, args = c("x", "y"))
  function(x, y) {
    cbind(
      numDeriv::grad(\(x) f(x = x, y = y), x),
      numDeriv::grad(\(y) f(x = x, y = y), y)
    )
  }
}

#' @export
print.treatment_effect <- function(x, level = 0.95, ...) {
  print(attr(x, "marginal_mean"), signif.legend = FALSE, level = level)
  cat(sprintf("\nContrast     :  %s\n", attr(x, "contrast")))
  if (is.null(attr(x, "variance"))) {
    trt_sd <- rep(NA, length(x))
  } else {
    trt_sd <- sqrt(diag(attr(x, "variance")))
  }
  z_value <- as.numeric(x - attr(x, "equal_val")) / trt_sd
  p <- 2 * pnorm(abs(z_value), lower.tail = FALSE)
  coef_mat <- matrix(
    c(
      x,
      trt_sd,
      z_value,
      p
    ),
    nrow = length(x)
  )
  colnames(coef_mat) <- c("Estimate", "Std.Err", "Z Value", "Pr(>|z|)")
  pair <- attr(x, "pair")
  row.names(coef_mat) <- sprintf("%s v.s. %s", attr(pair, "levels")[pair[[1]]], attr(pair, "levels")[pair[[2]]])
  stats::printCoefmat(
    coef_mat
  )
}
