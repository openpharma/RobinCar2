#' Validate Treatment Variable
#' @param x (`character`)\cr or (`factor`)\cr.
#' @keywords internal
h_validate_treatment <- function(x) {
  assert_multi_class(x, c("character", "factor"))
  if (is.character(x)) {
    x <- factor(x)
  } else if (is.factor(x)) {
    x <- droplevels(x)
  }
  assert_factor(x, n.levels = 2L)
  x
}

#' Obtain Treatment Variables from Formula
h_obtain_treat <- function(formula, treatment) {
  assert_subset(treatment, all.vars(formula))
  formula <- update(formula, NULL ~ .)
  tms <- terms(formula)
  factors <- attr(tms, "factors")
  trt <- factors[treatment, ]
  names(trt)[trt == 1]
}
