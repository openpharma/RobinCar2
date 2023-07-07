#' Vlidate Treatment Variable
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
