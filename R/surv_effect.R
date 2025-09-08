#' S3 Methods for `surv_effect`
#' @param x (`surv_effect`) the obtained result from [robin_surv()].
#' @param ... ignored additional arguments (for compatibility).
#' @name surv_effect_methods
#'
#' @examples
#' x <- robin_surv(
#'   formula = Surv(time, status) ~ meal.cal + age,
#'   data = surv_data,
#'   treatment = sex ~ strata
#' )
NULL

#' @export
#' @describeIn surv_effect_methods prints the `surv_effect` object.
#' @examples
#' print(x)
print.surv_effect <- function(x, ...) {
  cat("Model        : ", deparse(as.formula(x$model)), "\n")

  cat(
    "Randomization: ",
    deparse(x$randomization),
    " (",
    randomization_schema$schema[randomization_schema$id == x$schema],
    ")\n"
  )
  contr_type <- switch(x$contrast,
    hazardratio = "Log Hazard ratio"
  )
  cat(sprintf("\nContrast     :  %s\n\n", contr_type))

  stats::printCoefmat(
    x$log_hr_coef_mat
  )

  cat("\n")

  test_type <- switch(x$test,
    logrank = "Log-Rank"
  )
  cat(sprintf("Test         :  %s\n\n", test_type))

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
  h_confint(object$log_hr_coef_mat, parm = parm, level = level, transform = transform, ...)
}
