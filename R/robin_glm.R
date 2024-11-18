#' Covariate adjusted glm model
#' @param formula (`formula`) A formula of analysis.
#' @param data (`data.frame`) Input data frame.
#' @param treatment (`formula` or `character(1)`) A formula of treatment assignment or assignment by stratification,
#' or a string name of treatment assignment.
#' @param contrast (`function` or `character(1)`) A function to calculate the treatment effect, or character of
#' "difference", "risk_ratio", "odds_ratio" for default contrasts.
#' @param contrast_jac (`function`) A function to calculate the Jacobian of the contrast function. Ignored if using
#' default contrasts.
#' @param vcov (`function`) A function to calculate the variance-covariance matrix of the treatment effect,
#' including `vcovHC` and `gvcov`.
#' @param family (`family`) A family object of the glm model.
#' @param vcov_args (`list`) Additional arguments passed to `vcov`.
#' @param ... Additional arguments passed to `glm` or `glm.nb`.
#' @details
#' If family is `MASS::negative.binomial(NA)`, the function will use `MASS::glm.nb` instead of `glm`.
#' @export
#' @examples
#' robin_glm(
#'   y ~ treatment * s1,
#'   data = dummy_data,
#'   treatment = treatment ~ s1, contrast = "difference"
#' )
robin_glm <- function(
    formula, data, treatment, contrast = "difference",
    contrast_jac = NULL, vcov = "gvcov", family = gaussian(), vcov_args = list(), ...) {
  attr(formula, ".Environment") <- environment()
  # check if using negative.binomial family with NA as theta.
  # If so, use MASS::glm.nb instead of glm.
  assert_subset(all.vars(formula), names(data))
  assert_subset(all.vars(treatment), names(data))
  if (identical(family$family, "Negative Binomial(NA)")) {
    fit <- MASS::glm.nb(formula, data = data, ...)
  } else {
    fit <- glm(formula, family = family, data = data, ...)
  }
  pc <- predict_counterfactual(fit, treatment, data)
  has_interaction <- h_interaction(formula, treatment)
  use_vcovhc <- identical(vcov, "vcovHC") || identical(vcov, vcovHC)
  if (use_vcovhc && (has_interaction || !identical(contrast, "difference"))) {
    stop(
      "Huber-White variance estimator is ONLY supported when the expected outcome difference is estimated",
      "using a linear model without treatment-covariate interactions; see the 2023 FDA guidance."
    )
  }
  if (identical(contrast, "difference")) {
    difference(pc, variance = vcov, vcov_args = vcov_args)
  } else if (identical(contrast, "risk_ratio")) {
    risk_ratio(pc, variance = vcov, vcov_args = vcov_args)
  } else if (identical(contrast, "odds_ratio")) {
    odds_ratio(pc, variance = vcov, vcov_args = vcov_args)
  } else {
    assert_function(contrast)
    assert_function(contrast_jac, null.ok = TRUE)
    if (is.null(contrast_jac)) {
      contrast_jac <- function(x) {
        numDeriv::jacobian(contrast, x)
      }
    }
    treatment_effect(pc, eff_measure = contrast, eff_jacobian = contrast_jac, variance = vcov, vcov_args = vcov_args)
  }
}

h_interaction <- function(formula, treatment) {
  assert_formula(formula)
  treatment <- h_get_vars(treatment)
  assert_subset(treatment$treatment, all.vars(formula[[length(formula)]]))
  tms <- terms(formula)
  fct <- attr(tms, "factors")
  any(fct[treatment$treatment, ] %in% c(1, 2) & colSums(fct != 0) > 1)
}
