#' S3 Methods for `mh_effect`
#'
#' @param x (`mh_effect`) The result of [robin_mh()].
#' @param ... Ignored additional arguments (for compatibility).
#' @name mh_effect_methods
#'
#' @examples
#' x <- robin_mh(
#'   y_b ~ s1 + s2,
#'   data = subset(glm_data, treatment != "trt2"),
#'   treatment = treatment ~ pb(s1, s2)
#' )
NULL

#' @export
#' @describeIn mh_effect_methods prints the `mh_effect` object.
#' @examples
#' print(x)
print.mh_effect <- function(x, ...) {
  cat("Model        : ", deparse(as.formula(x$formula)), "\n", sep = "")
  cat(
    "Randomization: ",
    deparse(x$randomization),
    " (",
    randomization_schema$schema[randomization_schema$id == x$schema],
    ")\n",
    sep = ""
  )
  if (length(x$vars$strata) > 0L) {
    cat("Stratification variables: ", toString(x$vars$strata), "\n", sep = "")
  }
  estimand_label <- switch(x$estimand,
    ATE = "Average Treatment Effect (risk difference)",
    MH = "Mantel-Haenszel risk difference"
  )
  ci_label <- switch(x$ci_type,
    GR = "Greenland-Robins",
    mGR = "Modified Greenland-Robins",
    Sato = "Sato"
  )
  cat(sprintf("\nEstimand     : %s\n", estimand_label))
  cat(sprintf("Variance     : %s\n\n", ci_label))
  stats::printCoefmat(x$coef_mat, has.Pvalue = TRUE, ...)
}

#' @export
#' @describeIn mh_effect_methods prints and returns invisibly the events
#'   summary table of the `mh_effect` object.
#' @examples
#' table(x)
table.mh_effect <- function(x, ...) {
  cat(
    "Number of patients and events per",
    if (length(x$vars$strata) > 0L) " stratum and " else " ",
    "treatment arm:\n",
    sep = ""
  )
  print(x$events_table)
  invisible(x$events_table)
}

#' @export
#' @rdname confint
confint.mh_effect <- function(object, parm, level = 0.95, transform, ...) {
  h_confint(object$coef_mat, parm = parm, level = level, transform = transform, ...)
}
