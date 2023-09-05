#' Standardized Generalized Linear Models
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @param formula (`formula`)\cr the model formula. Must include `trt` to indicate the treatment variable.
#' @param data (`data.frame`)\cr the dataset to be used.
#' @param trt (`string`)\cr the treatment variable.
#' @param ... additional arguments for `glm()`.
#'
#' @note Response must be binary.
#' @export
#'
#' @examples
#' std_glm(Sepal.Length ~ Species, data = iris, trt = "Species")
std_glm <- function(formula, data, trt, ...) {
  assert_formula(formula)
  assert_string(trt)
  assert_subset(trt, all.vars(formula[[length(formula)]]))
  trt_vars <- h_obtain_treat(formula, trt)
  if (!trt %in% trt_vars) {
    stop(sprintf("Treatment variable %s should exisit in the right hand side of formula!", trt))
  }
  assert_subset(trt, colnames(data))

  data[[trt]] <- as.factor(data[[trt]])
  fit <- glm(formula = formula, data = data, family = binomial(link = "logit"), ...)
  res <- list(fit = fit)
  res$trt <- trt
  res$data <- data
  trt_levels <- levels(data[[trt]])
  trt_coef <- paste0(trt, trt_levels)
  trt_level_match <- match(trt_coef, names(coef(fit)))
  reference_arm <- trt_levels[is.na(trt_level_match)]
  res$trt_levels <- trt_levels[-1L]
  res$ref_level <- reference_arm
  class(res) <- "std_glm"
  res
}
