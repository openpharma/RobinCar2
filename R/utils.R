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
  colnames(factors)[trt == 1]
}

h_obtain_sel <- function(x, y) {
  assert_character(x, len = 2L)
  assert_character(y)
  match_x_y <- match(x, y)
  if (all(is.na(match_x_y))) {
    stop("")
  }
  ret <- rep(0, length(y))
  if (is.na(match_x_y[1])) {
    ret[match_x_y[2]] <- -1
  } else if (is.na(match_x_y[2])) {
    ret[match_x_y[1]] <- 1
  } else {
    ret[match_x_y] <- c(1, -1)
  }
  ret
}
