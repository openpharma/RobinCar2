#' Covariate adjusted lm model
#' @param formula (`formula`) A formula of analysis.
#' @param data (`data.frame`) Input data frame.
#' @param treatment (`formula` or `character(1)`) A formula of treatment assignment or assignment by stratification,
#' or a string name of treatment assignment.
#' @param vcov (`function`) A function to calculate the variance-covariance matrix of the treatment effect,
#' including `vcovHC` and `vcovG`. The default is 'vcovG'.
#' @param vcov_args (`list`) Additional arguments passed to `vcov`.
#' @param pair Pairwise treatment comparison.
#' @param ... Additional arguments passed to `lm`.
#' @export
#' @return A robin_output object, with `marginal_mean` and `contrast` components.
#' @examples
#' robin_lm(
#'   y ~ treatment * s1,
#'   data = glm_data,
#'   treatment = treatment ~ s1
#' )
robin_lm <- function(
  formula,
  data,
  treatment,
  vcov = "vcovG",
  vcov_args = list(),
  pair,
  ...
) {
  attr(formula, ".Environment") <- environment()
  assert_subset(all.vars(formula), names(data))
  assert_subset(all.vars(treatment), names(data))
  has_interaction <- h_interaction(formula, treatment)
  use_vcovhc <- identical(vcov, "vcovHC") || identical(vcov, vcovHC)
  if (use_vcovhc && has_interaction) {
    stop(
      "Huber-White variance estimator is ONLY supported when using a linear model
      without treatment-covariate interactions; see the 2023 FDA guidance."
    )
  }
  fit <- lm(formula, data = data, ...)
  # same as robin_glm, to allow vcov as function to be used, also allow the function name to be captured.
  pc <- eval(bquote(predict_counterfactual(fit, treatment, data, vcov = .(substitute(vcov)), vcov_args = vcov_args)))
  if (missing(pair)) {
    pair <- pairwise(names(pc$estimate))
  }
  trt_eff <- difference(pc, pair = pair)
  structure(
    list(
      marginal_mean = pc,
      contrast = trt_eff
    ),
    class = "robin_output"
  )
}

#' Evaluate if Interaction Exists
#' @keywords internal
#' @param formula (`formula`) the formula for model fitting.
#' @param treatment (`formula`) the formula for treatment assignment.
h_interaction <- function(formula, treatment) {
  assert_formula(formula)
  treatment <- h_get_vars(treatment)
  assert_subset(treatment$treatment, all.vars(formula[[length(formula)]]))
  tms <- terms(formula)
  fct <- attr(tms, "factors")
  any(fct[treatment$treatment, ] %in% c(1, 2) & colSums(fct != 0) > 1)
}
