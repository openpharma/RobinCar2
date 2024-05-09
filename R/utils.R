#' Extract Variable Names
#'
#' @param treatment (`string` or `formula`) string name of the treatment, or a formula.
#'
#' @details Extract the formula elements, including `treatment` and `strata`.
#'
#' @return A list of two elements, `treatmetn` and `strata`.
h_get_vars <- function(treatment) {
  if (test_string(treatment)) {
    ret <- list(
      treatment = treatment,
      strata = character(0)
    )
  } else if (test_formula(treatment)) {
    if (!identical(length(treatment), 3L)) {
      stop("treatment formula must be of type treatment ~ strata")
    }
    if (!is.name(treatment[[2]])) {
      stop("left hand side of the treatment formula should be a single name!")
    }
    treatvar <- as.character(treatment[[2]])
    strata <- setdiff(all.vars(treatment[[3]]), ".")
    ret <- list(
      treatment = treatvar,
      strata = strata
    )
  } else {
    stop("treatment must be a length 1 character or a formula of form treatment ~ strata")
  }
  ret
}

block_sum <- function(x, n) {
  assert_matrix(x)
  nr <- nrow(x) / n
  matrix(colSums(matrix(x, nrow = n)), nrow = nr)
}
