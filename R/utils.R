randomization_schema <- data.frame(
  schema = c("Pocock-Simon", "permuted-block", "simple"),
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

block_sum <- function(x, n) {
  assert_matrix(x)
  nr <- nrow(x) / n
  matrix(colSums(matrix(x, nrow = n)), nrow = nr)
}
