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
#' @param ... Additional arguments passed to `vcov`. For finer control of glm, refer to usage of `treatment_effect`,
#' `difference`, `risk_ratio`, `odds_ratio`.
#' @export
#' @examples
#' robin_glm(
#'   y ~ treatment * s1,
#'   data = dummy_data,
#'   treatment = treatment ~ s1, contrast = "difference"
#' )
robin_glm <- function(
    formula, data, treatment, contrast = "difference",
    contrast_jac = NULL, vcov = gvcov, family = gaussian, ...) {
  attr(formula, ".Environment") <- environment()
  fit <- glm(formula, family = family, data = data)
  pc <- predict_counterfactual(fit, treatment, data)
  has_interaction <- h_interaction(formula, treatment)
  if (has_interaction && identical(vcov, vcovHC) && !identical(contrast, "difference")) {
    stop(
      "Huber-White variance estimator is ONLY supported when the expected outcome difference is estimated using a linear model without treatment-covariate interactions; see the 2023 FDA guidance."
    )
  }
  if (identical(contrast, "difference")) {
    difference(pc)
  } else if (identical(contrast, "risk_ratio")) {
    risk_ratio(pc)
  } else if (identical(contrast, "odds_ratio")) {
    odds_ratio(pc)
  } else {
    assert_function(contrast)
    assert_function(contrast_jac, null.ok = TRUE)
    if (is.null(contrast_jac)) {
      contrast_jac <- function(x) {
        numDeriv::jacobian(contrast, x)
      }
    }
    treatment_effect(pc, eff_measure = contrast, eff_jacobian = contrast_jac, variance = vcov, ...)
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
