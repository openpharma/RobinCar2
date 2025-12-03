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
#' including `vcovHC` and `vcovG`.
#' @param family (`family`) A family object of the glm model.
#' @param vcov_args (`list`) Additional arguments passed to `vcov`.
#' @param pair Pairwise treatment comparison.
#' @param ... Additional arguments passed to `glm` or `glm.nb`.
#' @details
#' If family is `MASS::negative.binomial(NA)`, the function will use `MASS::glm.nb` instead of `glm`.
#' @export
#' @return A robin_output object, with `marginal_mean` and `contrast` components.
#' @examples
#' robin_glm(
#'   y ~ treatment * s1,
#'   data = glm_data,
#'   treatment = treatment ~ s1, contrast = "difference"
#' )
robin_glm <- function(
  formula,
  data,
  treatment,
  contrast = c("difference", "risk_ratio", "odds_ratio", "log_risk_ratio", "log_odds_ratio"),
  contrast_jac = NULL,
  vcov = "vcovG",
  family = gaussian(),
  vcov_args = list(),
  pair,
  ...
) {
  attr(formula, ".Environment") <- environment()
  if (is.function(family)) {
    family <- family()
  }
  assert_subset(all.vars(formula), names(data))
  assert_subset(all.vars(treatment), names(data))
  has_interaction <- h_interaction(formula, treatment)
  use_vcovhc <- identical(vcov, "vcovHC") || identical(vcov, vcovHC)
  if (test_character(contrast)) {
    contrast <- match.arg(contrast)
  }
  if (use_vcovhc && (has_interaction || !identical(contrast, "difference") || !identical(family$family, "gaussian"))) {
    stop(
      "Huber-White variance estimator is ONLY supported when the expected outcome difference is estimated",
      "using a linear model without treatment-covariate interactions; see the 2023 FDA guidance."
    )
  }
  # check if using negative.binomial family with NA as theta.
  # If so, use MASS::glm.nb instead of glm.
  if (identical(family$family, "Negative Binomial(NA)")) {
    fit <- MASS::glm.nb(formula, data = data, ...)
  } else {
    fit <- glm(formula, family = family, data = data, ...)
  }
  pc <- eval(bquote(predict_counterfactual(fit, treatment, data, vcov = .(substitute(vcov)), vcov_args = vcov_args)))
  if (missing(pair)) {
    pair <- pairwise(names(pc$estimate))
  }

  if (test_character(contrast)) {
    if (contrast %in% c("odds_ratio", "risk_ratio")) {
      warning(
        c(
          "Consider using the log transformation `log_odds_ratio` and `log_risk_ratio` ",
          "to replace `odds_ratio` and `risk_ratio` to improve the performance of normal approximation."
        )
      )
    }
    trt_eff <- get(contrast)(pc, pair = pair, contrast_name = contrast)
  } else {
    assert_function(contrast, args = c("x", "y"))
    assert_function(contrast_jac, null.ok = TRUE, args = c("x", "y"))
    if (is.null(contrast_jac)) {
      contrast_jac <- eff_jacob(contrast)
    }
    contrast_name_full <- deparse(substitute(contrast))
    contrast_name <- paste(contrast_name_full[1], if (length(contrast_name_full) > 1) "...")
    trt_eff <- treatment_effect(
      pc,
      eff_measure = contrast, eff_jacobian = contrast_jac, pair = pair, contrast_name = contrast_name
    )
  }
  structure(
    list(
      marginal_mean = pc,
      contrast = trt_eff
    ),
    class = "robin_output"
  )
}

#' @export
print.robin_output <- function(x, ...) {
  print(x$marginal_mean)
  cat(sprintf("\nContrast     :  %s\n", x$contrast$contrast))
  print(x$contrast)
}
