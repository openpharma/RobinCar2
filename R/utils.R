#' Randomization schema
#' @keywords internal
randomization_schema <- data.frame(
  schema = c("Pocock-Simon", "Permuted-Block", "Simple"),
  id = c("ps", "pb", "sr"),
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
    schema <- "sr"
  }
  strata <- setdiff(all.vars(treatment[[3]]), ".")
  list(
    treatment = treatvar,
    schema = schema,
    strata = strata
  )
}

#' Prepare Survival Input
#'
#' @param formula (`formula`) with a left hand side of the form `Surv(time, status)` and a right hand side
#'   defining optional covariates or just `1` if there are no covariates.
#' @param data (`data.frame`) containing the variables in the formula.
#' @inheritParams h_get_vars
#'
#' @details Note that `formula` can also contain an externally defined [survival::Surv]
#'   object. In this case, the `time` and `status` variables are extracted
#'   and added to the `data` input. Note that it is up to the user to ensure that in this
#'   case the column binding is correct, i.e., that the rows of the `data` match
#'   with the rows of the `Surv` object. In addition, the same named variables must not appear
#'   in both the `data` and the `Surv` object, to avoid ambiguity (this is a difference
#'   vs. the behavior of [survival::coxph()] for better transparency).
#'
#' @return A list containing the following elements:
#' - `data`: The potentially updated data set.
#' - `time`: Name of the time variable.
#' - `status`: Name of the status variable.
#' - `treatment`: Name of the treatment variable.
#' - `strata`: Name of the strata variable.
#' - `schema`: Randomization schema.
#' - `randomization_strata`: Names of the randomization strata variables.
#' - `covariates`: Names of the covariates in the model.
#' - `model`: A formula only including the covariates, but not treatment or strata variables.
#' - `n_levels`: Number of treatment levels.
#' - `levels`: Names of the treatment levels.
#'
#' @keywords internal
h_prep_survival_input <- function(formula, data, treatment) {
  # Process the treatment formula.
  trt_vars <- h_get_vars(treatment)
  assert_data_frame(data)
  assert_formula(formula)
  assert_true(identical(length(formula), 3L))
  assert_subset(c(trt_vars$treatment, trt_vars$strata), colnames(data))
  assert(
    test_character(data[[trt_vars$treatment]]),
    test_factor(data[[trt_vars$treatment]])
  )

  if (test_character(data[[trt_vars$treatment]])) {
    data[[trt_vars$treatment]] <- factor(data[[trt_vars$treatment]])
  }
  trt_lvls <- levels(data[[trt_vars$treatment]])
  n_lvls <- length(trt_lvls)

  # Process analysis model formula.
  tms <- terms(formula, specials = "strata")
  specials <- survival::untangle.specials(tms, "strata")
  analysis_strata <- if (length(specials$vars) > 0) {
    all.vars(as.formula(paste("~", paste(specials$vars, collapse = " + "))))
  } else {
    character()
  }

  covariates <- setdiff(all.vars(formula[[3]]), analysis_strata)
  assert_disjunct(trt_vars$treatment, covariates)

  # Extract survival time and censoring indicator from the left hand side of the formula.
  lhs <- formula[[2]]
  if (inherits(lhs, "call") && (lhs[[1]] == as.name("Surv") || lhs[[1]] == as.name("survival::Surv"))) {
    surv_vars <- as.character(lhs)[-1]
    assert_subset(surv_vars, colnames(data))
    time_var <- surv_vars[1]
    status_var <- surv_vars[2]
  } else if (survival::is.Surv(lhs_evaluated <- try(eval(lhs), silent = TRUE))) {
    assert_true(identical(attr(lhs_evaluated, "type"), "right"))
    lhs_matrix <- as.matrix(lhs_evaluated)
    assert_true(identical(nrow(lhs_matrix), nrow(data)))
    lhs_names <- colnames(lhs_matrix)
    time_var <- lhs_names[1]
    status_var <- lhs_names[2]
    if (any(c(time_var, status_var) %in% names(data))) {
      stop("Ambiguous names provided in Surv object and in data")
    }
    data <- cbind(data, lhs_matrix)
  } else {
    stop("Left hand side of formula must be a Surv() object.")
  }

  # Extract model without left hand side and without strata() terms.
  covariates_formula <- if (length(specials$vars) > 0) {
    stats::update(formula, as.formula(paste0("~ . -", paste(specials$vars, collapse = "-"))))
  } else {
    formula
  }
  model <- as.formula(as.call(as.list(covariates_formula)[-2L]))

  list(
    data = data,
    time = time_var,
    status = status_var,
    treatment = trt_vars$treatment,
    randomization_strata = trt_vars$strata,
    strata = analysis_strata,
    schema = trt_vars$schema,
    covariates = covariates,
    model = model,
    n_levels = n_lvls,
    levels = trt_lvls
  )
}

#' Count Number of Events per Unique Event Time
#'
#' This function counts the number of events at each unique event time point in a survival dataset.
#'
#' @details If there are no events in the dataset, it returns an empty `data.frame`.
#'
#' @param df (`data.frame`) containing the survival data.
#' @param time (`string`) name of the time variable.
#' @param status (`string`) name of the status variable, where 1 indicates an event and 0 indicates censoring.
#' @return A `data.frame` with two columns: `time` and `n_events`, where `n_events` is the
#'   number of events at each time point.
#' @keywords internal
h_n_events_per_time <- function(df, time, status) {
  assert_data_frame(df)
  assert_string(time)
  assert_string(status)
  assert_numeric(df[[time]], any.missing = FALSE)
  assert_numeric(df[[status]], any.missing = FALSE)
  assert_true(all(df[[status]] %in% c(0, 1)))

  has_event <- df[[status]] == 1
  df_events <- df[has_event, , drop = FALSE]
  if (nrow(df_events) == 0) {
    return(data.frame(time = numeric(0), n_events = integer(0)))
  }
  times_count <- stats::aggregate(
    df_events[[status]],
    by = list(time = df_events[[time]]),
    FUN = length
  )
  data.frame(
    time = times_count$time,
    n_events = times_count$x
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

#' Sum vectors in a list
#' @keywords internal
sum_vectors_in_list <- function(lst) {
  assert_list(lst, min.len = 1L, types = "numeric")
  len1 <- length(lst[[1L]])
  lapply(lst, assert_numeric, any.missing = FALSE, len = len1)
  tmp <- matrix(
    unlist(lst, recursive = FALSE, use.names = FALSE),
    nrow = len1,
    ncol = length(lst)
  )
  .rowSums(tmp, m = len1, n = length(lst))
}

#' Confidence interval calculations which are common across effect results.
#' @keywords internal
h_confint <- function(x, parm, level = 0.95, transform, include_se = FALSE, ...) {
  assert_matrix(x)
  assert_names(colnames(x), must.include = c("Estimate", "Std.Err"))
  assert_names(rownames(x), type = "unique")
  assert_number(level, lower = 0, upper = 1)
  assert_flag(include_se)
  if (!missing(parm)) {
    assert(
      check_integerish(parm, lower = 1, upper = nrow(x)),
      check_subset(parm, row.names(x))
    )
  }
  if (!missing(transform)) {
    assert_function(transform)
  }
  est <- x[, "Estimate"]
  se <- x[, "Std.Err"]
  z <- qnorm((1 + level) / 2)
  ret <- matrix(
    c(
      est,
      if (include_se) se else NULL,
      est - se * z,
      est + se * z
    ),
    nrow = nrow(x)
  )
  colnames(ret) <- c(
    "Estimate",
    if (include_se) "Std.Err" else NULL,
    sprintf("%s %%", c((1 - level) / 2, (1 + level) / 2) * 100)
  )
  rownames(ret) <- rownames(x)
  if (missing(transform)) {
    transform <- identity
  }
  if (!identical(transform, identity)) {
    message("The confidence interval is transformed.")
  }
  if (!missing(parm)) {
    ret <- ret[parm, , drop = FALSE]
  }
  transform(ret)
}

#' Check Whether First Factor is Nested in Second Factor
#'
#' This function checks whether one factor variable is nested in another in terms of their levels
#' and the mapping of observations to these levels:
#'
#' - Both factors must have the same length.
#' - The positions of `NA` values must be identical in both factors.
#' - After removing `NA` values, `f1` must have at least as many observed levels as `f2`.
#' - The mapping of observations to levels must be consistent, meaning that each level in `f1`
#'   corresponds to exactly one level in `f2`. On the other hand, multiple levels in `f1`
#'   can map to the same level in `f2`.
#'
#' @param f1 A factor variable, supposed to be more fine grained than `f2`.
#' @param f2 A factor variable, supposed to be more coarse grained than `f1`.
#' @return `TRUE` if `f1` is nested in `f2`, `FALSE` otherwise.
#'
#' @keywords internal
h_first_fct_nested_in_second <- function(f1, f2) {
  assert_factor(f1)
  assert_factor(f2)

  if (length(f1) != length(f2)) {
    return(FALSE)
  }

  f1_na <- which(is.na(f1))
  f2_na <- which(is.na(f2))
  if (!identical(f1_na, f2_na)) {
    return(FALSE)
  }

  f1 <- droplevels(stats::na.omit(f1))
  f2 <- droplevels(stats::na.omit(f2))
  if (nlevels(f1) < nlevels(f2)) {
    return(FALSE)
  }

  f1_int <- as.integer(f1)
  f2_int <- as.integer(f2)
  f1_f2_int <- paste(f1_int, f2_int, sep = "-")
  unique_combinations <- unique(f1_f2_int)
  length(unique_combinations) == nlevels(f1)
}
