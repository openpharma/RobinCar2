#' S3 Methods for `surv_effect`
#' @param x (`surv_effect`) the obtained result from [robin_surv()].
#' @param ... ignored additional arguments (for compatibility).
#' @name surv_effect_methods
#'
#' @examples
#' x <- robin_surv(
#'   formula = Surv(time, status) ~ meal.cal + age + strata(strata),
#'   data = surv_data,
#'   treatment = sex ~ pb(strata)
#' )
NULL

#' @export
#' @describeIn surv_effect_methods prints the `surv_effect` object.
#' @examples
#' print(x)
print.surv_effect <- function(x, ...) {
  cat("Model        : ", deparse(as.formula(x$model)), "\n", sep = "")

  cat(
    "Randomization: ",
    deparse(x$randomization),
    " (",
    randomization_schema$schema[randomization_schema$id == x$schema],
    ")\n",
    sep = ""
  )

  strata_variables <- x$vars$strata
  uses_stratification <- length(strata_variables) > 0
  if (uses_stratification) {
    cat("Stratification variables: ", toString(strata_variables), "\n")
  }

  covariates <- x$vars$covariates
  uses_covariates <- length(covariates) > 0
  if (uses_covariates) {
    cat(
      "Covariates adjusted for: ",
      toString(covariates),
      " (including interactions with ",
      x$vars$treatment,
      ")\n",
      sep = ""
    )
  }

  contr_type <- switch(x$contrast,
    hazardratio = paste0(
      if (uses_covariates) "Covariate-adjusted ",
      if (uses_stratification) "Stratified ",
      "Log Hazard Ratio"
    ),
    none = "None"
  )
  cat(sprintf("\nContrast     : %s\n", contr_type), sep = "")

  if (x$contrast == "hazardratio") {
    cat("\n")
    stats::printCoefmat(
      x$log_hr_coef_mat
    )
  }

  cat("\n")

  test_type <- switch(x$test,
    logrank = paste0(
      if (uses_covariates) "Covariate-adjusted ",
      if (uses_stratification) "Stratified ",
      "Log-Rank"
    )
  )
  cat(sprintf("Test         : %s\n\n", test_type), sep = "")

  stats::printCoefmat(
    x$test_mat,
    tst.ind = 1L,
    has.Pvalue = TRUE
  )
}

#' @export
#' @rdname surv_effect_methods
table <- function(x, ...) UseMethod("table")

#' @export
#' @rdname surv_effect_methods
table.default <- function(x, ...) base::table(x, ...)

#' @export
#' @describeIn surv_effect_methods prints and returns invisibly the events table of the `surv_effect` object.
#' @examples
#' table(x)
table.surv_effect <- function(x, ...) {
  cat(
    "Number of patients and events per",
    ifelse(length(x$vars$strata), " stratum and ", " "),
    "treatment arm:\n",
    sep = ""
  )
  print(x$events_table)
  invisible(x$events_table)
}

#' Confidence interval function.
#' @rdname confint
#' @export
confint.surv_effect <- function(object, parm, level = 0.95, transform, ...) {
  if (object$contrast == "hazardratio") {
    h_confint(object$log_hr_coef_mat, parm = parm, level = level, transform = transform, ...)
  } else {
    stop(
      "No contrast was estimated; confidence interval is not available."
    )
  }
}
