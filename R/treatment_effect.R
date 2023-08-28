#' Obtain treatment effect
treatment_effect <- function(x, ...) {
  UseMethod("treatment_effect")
}

treatment_effect.std_lm <- function(x, level1, level2, vcov_method = "HC3" ...) {
  stop("Not implemented")
}

treatment_effect.std_lm <- function(x, level1, level2, statistic = c("rr", "or"), ...) {
  stop("Not implemented")
}
