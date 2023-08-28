#' Standardized Linear Models
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @param formula (`formula`)\cr the model formula. Must include `trt` to indicate the treatment variable.
#' @param data (`data.frame`)\cr the dataset to be used.
#' @param trt (`string`)\cr the treatment variable.
#' @param ... additional arguments for `lm()`.
#'
#' @details For linear models, we can use the conditional treatment effect to represent the marginal treatment effect
#' because it is collapsible. This function only conduct a `lm` regression and add a class `std_lm` to the returned
#' object to enable summary function.
#' To conduct the standardization method, it is required that `trt` is a factor of two levels: the first is control
#' arm and the second is treatment arm.
#'
#' @export
#'
#' @examples
#' std_lm(Sepal.Length ~ Species, data = subset(iris, Species != "virginica"), trt = "Species")
std_lm <- function(formula, data, trt, ...) {
  assert_formula(formula)
  assert_string(trt)
  assert_subset(trt, all.vars(formula[[length(formula)]]))
  trt_vars <- h_obtain_treat(formula, trt)
  if (!trt %in% trt_vars) {
    stop(sprintf("Treatment variable %s should exisit in the right hand side of formula!", trt))
  }
  if (length(trt_vars) > 1) {
    warning(
      "ANHECOVA is not implemented yet thus treatment by covariate interaction term",
      "should not be included in formula, the following terms are found",
      toString(setdiff(trt_vars, trt))
    )
  }
  fit <- lm(formula = formula, data = data, ...)
  res <- list(fit = fit)
  res$trt <- trt
  trt_levels <- unique(data[[trt]])
  trt_coef <- paste0(trt, trt_levels)
  trt_level_match <- match(trt_coef, names(coef(fit)))
  reference_arm <- trt_levels[is.na(trt_level_match)]
  res$trt_levels <- levels(data[[trt]])
  class(res) <- "std_lm"
  res
}
