#' Randomization schema
#' @keywords internal
randomization_schema <- data.frame(
  schema = c("Pocock-Simon", "Permuted-Block", "Simple"),
  id = c("ps", "pb", "sp"),
  stringsAsFactors = FALSE
)

#' Extract Variable Names
#'
#' @param treatment (`string` or `formula`) string name of the treatment, or a formula.
#'
#' @details Extract the formula elements, including `treatment`, `schema` and `strata`.
#'
#' @return A list of three elements, `treatment`, `schema` and `strata`.
#' @keywords internal
h_get_vars <- function(treatment) {
  assert_formula(treatment)
  if (!identical(length(treatment), 3L)) {
    stop("treatment formula must be of type treatment ~ strata")
  }
  if (!is.name(treatment[[2]])) {
    stop("left hand side of the treatment formula should be a single name!")
  }
  treatvar <- as.character(treatment[[2]])
  tms <- terms(treatment, specials = randomization_schema$id)
  schema <- names(Filter(Negate(is.null), attr(tms, "specials")))
  if (length(schema) > 1) {
    stop("only one randomization schema is allowed!")
  } else if (length(schema) == 0) {
    schema <- "sp"
  }
  strata <- setdiff(all.vars(treatment[[3]]), ".")
  list(
    treatment = treatvar,
    schema = schema,
    strata = strata
  )
}

#' Block Sum of a matrix
#' @keywords internal
block_sum <- function(x, n) {
  assert_matrix(x)
  nr <- nrow(x) / n
  matrix(colSums(matrix(x, nrow = n)), nrow = nr)
}

#' @export
#' @rdname contrast
pairwise <- function(levels, x = levels) {
  assert(
    test_integerish(x),
    test_character(x)
  )
  all_combs <- combn(x, 2L)
  custom_contrast(levels, all_combs[2, ], all_combs[1, ])
}

#' @export
#' @rdname contrast
against_ref <- function(levels, ref = levels[1], x = tail(levels, -1)) {
  assert(
    check_string(ref),
    check_int(ref)
  )
  custom_contrast(levels, x, rep(ref, length(x)))
}
#' Create Contrast of Pairs
#' @param x (`vector`) A vector of treatment levels.
#' @param y (`vector`) A vector of treatment levels.
#' @param ref (`string` or `int`) Reference level.
#' @param levels (`character`) Levels of the treatment.
#' @export
#' @rdname contrast
#' @return A list of `contrast` object with following elements:
#' - Index of the treatment group.
#' - Index of the reference group.
#' Additional attributes include `levels` and `max_levels` indicating the
#' names of the treatment levels and the maximum number of levels.
custom_contrast <- function(levels, x, y) {
  assert_character(levels)
  if (test_integerish(x)) {
    assert_integerish(x)
  } else {
    assert_character(x)
    assert_subset(x, levels)
  }
  if (test_integerish(y)) {
    assert_integerish(y, len = length(x))
  } else {
    assert_character(y, len = length(x))
    assert_subset(y, levels)
  }
  structure(
    list(
      if (test_integerish(x)) x else match(x, levels),
      if (test_integerish(y)) y else match(y, levels)
    ),
    max_levels = length(levels),
    levels = levels,
    class = "contrast"
  )
}

#' Update levels in a contrast pair
#' @keywords internal
update_levels <- function(pair, levels) {
  assert_class(pair, "contrast")
  assert_character(levels)
  neworder <- match(attr(pair, "levels"), levels)
  custom_contrast(
    levels,
    neworder[pair[[1]]],
    neworder[pair[[2]]]
  )
}

#' Obtain the Jacobian matrix
#' @keywords internal
jac_mat <- function(jac, pair) {
  assert_matrix(jac, ncols = 2, nrows = length(pair[[1]]))
  assert_class(pair, "contrast")
  ret <- matrix(0, nrow = nrow(jac), ncol = attr(pair, "max_levels"))
  ret[cbind(seq_len(nrow(jac)), pair[[1]])] <- jac[, 1]
  ret[cbind(seq_len(nrow(jac)), pair[[2]])] <- jac[, 2]
  ret
}
