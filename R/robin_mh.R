#' Mantel-Haenszel Risk Difference and Average Treatment Effect
#'
#' Estimate the (stratified) Mantel-Haenszel risk difference, or the
#' corresponding Average Treatment Effect (ATE), for a binary outcome under
#' stratified or covariate-adaptive randomization.
#'
#' @param formula (`formula`) Response formula of the form `y ~ s1 + s2` where
#'   the LHS names a binary outcome and the RHS variables define the joint
#'   analysis strata. Use `y ~ 1` for the unstratified case. The response may
#'   be `0`/`1`, `logical`, or a two-level `factor` whose second level is the
#'   event. Both sides must be bare variable names: transformed terms such as
#'   `I(1 - y)` or `factor(s1)` are rejected rather than silently ignored.
#' @param data (`data.frame`) Input data frame. Rows with missing values in the
#'   response, treatment or analysis strata are dropped with a message.
#' @param treatment (`formula`) A treatment formula `treatment ~ scheme(vars)`
#'   following the same grammar as the rest of the package (`sr`, `pb`, `ps`).
#'   The randomization scheme is informational; if the analysis strata do not
#'   cover the randomization strata a warning is emitted, paralleling
#'   [robin_surv()]. Analysis strata finer than the randomization strata are
#'   accepted without a warning.
#' @param estimand (`character(1)`) Either `"ATE"` (default) for the average
#'   treatment effect, or `"MH"` for the Mantel-Haenszel risk difference.
#' @param ci_type (`character(1)`) Variance estimator:
#'   `"mGR"` (default, modified Greenland-Robins),
#'   `"GR"` (Greenland-Robins), or `"Sato"` (Sato).
#'   `estimand = "ATE"` requires `ci_type = "mGR"` and additionally adds the
#'   Bannick-Ye nu correction term.
#' @param pair (`contrast`) Optional contrast specification (default: all
#'   pairwise comparisons, with the higher-indexed level as the experimental
#'   arm). It must only reference treatment levels observed in `data`.
#' @return An `mh_effect` object. See [mh_effect_methods] for available S3
#'   methods.
#'
#' @details
#' For each pair (`exp`, `ref`) of treatment levels and each joint analysis
#' stratum k, with \eqn{n_{1k}, n_{0k}} the counts in the two arms,
#' \eqn{n_{11k}, n_{10k}} the corresponding event counts, and weights
#' \eqn{w_k = n_{1k} n_{0k} / (n_{1k} + n_{0k})}, the Mantel-Haenszel estimator
#' is \deqn{\widehat{\Delta}_{MH} =
#'   \frac{\sum_k w_k (n_{11k}/n_{1k} - n_{10k}/n_{0k})}{\sum_k w_k}.}
#' Variance estimators: see Greenland & Robins (1985), Sato (1989), and
#' Bannick et al. (2024) for the ATE additive correction.
#'
#' @references
#' Greenland S, Robins JM (1985). Estimation of a common effect parameter from
#' sparse follow-up data. \emph{Biometrics} 41:55-68.
#'
#' Sato T (1989). On the variance estimator for the Mantel-Haenszel risk
#' difference. \emph{Biometrics} 45:1323-1324.
#'
#' Ye T, Bannick M, Yi Y, Shao J (2023). Robust variance estimation for
#' covariate-adjusted unconditional treatment effect in randomized clinical
#' trials with binary outcomes. \emph{Statistical Theory and Related Fields}.
#'
#' @seealso [mh_effect_methods] for S3 methods.
#'
#' @export
#' @examples
#' robin_mh(
#'   y_b ~ s1 + s2,
#'   data = subset(glm_data, treatment != "trt2"),
#'   treatment = treatment ~ pb(s1, s2)
#' )
#'
#' # Unstratified MH risk difference (degenerates to plain difference of proportions):
#' robin_mh(
#'   y_b ~ 1,
#'   data = subset(glm_data, treatment != "trt2"),
#'   treatment = treatment ~ sr(1),
#'   estimand = "MH",
#'   ci_type = "GR"
#' )
robin_mh <- function(
  formula,
  data,
  treatment,
  estimand = c("ATE", "MH"),
  ci_type = c("mGR", "GR", "Sato"),
  pair
) {
  assert_formula(formula)
  assert_true(identical(length(formula), 3L))
  assert_data_frame(data)
  assert_formula(treatment)
  # Unlike `robin_lm`/`robin_glm`, the formulas are never evaluated here - all
  # variables are looked up by name in `data`. Detaching the environments keeps
  # the returned object from retaining the caller's frame, which would otherwise
  # pin the whole input data set for the lifetime of the result.
  environment(formula) <- baseenv()
  environment(treatment) <- baseenv()
  estimand <- match.arg(estimand)
  ci_type <- match.arg(ci_type)
  if (estimand == "ATE" && ci_type != "mGR") {
    stop("`estimand = 'ATE'` requires `ci_type = 'mGR'`.")
  }

  trt_vars <- h_get_vars(treatment)
  trt_var <- trt_vars$treatment

  if (!is.name(formula[[2]])) {
    stop("Left hand side of `formula` must be a single response variable, without transformation.")
  }
  response_var <- as.character(formula[[2]])
  analysis_strata <- h_mh_strata_vars(formula)
  assert_disjunct(trt_var, analysis_strata)

  needed <- unique(c(response_var, trt_var, analysis_strata))
  assert_subset(c(needed, trt_vars$strata), names(data))

  # Completeness is only required of the analysis variables; the randomization
  # strata are carried along for the warning below without restricting the
  # analysis population.
  n_original <- nrow(data)
  data <- data[
    stats::complete.cases(data[, needed, drop = FALSE]),
    unique(c(needed, trt_vars$strata)),
    drop = FALSE
  ]
  if (nrow(data) == 0L) {
    stop("No complete observations remain after removing missing values.")
  }
  if (nrow(data) < n_original) {
    message(
      "Removed ", n_original - nrow(data), " of ", n_original,
      " observations with missing values in the analysis variables."
    )
  }

  y <- h_mh_response(data[[response_var]], response_var)

  if (is.character(data[[trt_var]])) {
    data[[trt_var]] <- factor(data[[trt_var]])
  }
  assert_factor(data[[trt_var]])
  trt <- droplevels(data[[trt_var]])
  trt_lvls <- levels(trt)
  if (length(trt_lvls) < 2L) {
    stop("At least two treatment levels are required.")
  }
  trt_idx <- as.integer(trt)
  n_trt <- length(trt_lvls)

  if (length(analysis_strata) == 0L) {
    strata_factor <- factor(rep("all", nrow(data)))
  } else {
    strata_factor <- h_mh_joint_strata(data[, analysis_strata, drop = FALSE])
  }
  n_strata <- nlevels(strata_factor)
  strata_idx <- as.integer(strata_factor)

  flat <- (trt_idx - 1L) * n_strata + strata_idx
  n_vec <- tabulate(flat, nbins = n_strata * n_trt)
  r_vec <- tabulate(flat[y == 1L], nbins = n_strata * n_trt)
  n_mat <- matrix(n_vec,
    nrow = n_strata, ncol = n_trt,
    dimnames = list(levels(strata_factor), trt_lvls)
  )
  r_mat <- matrix(r_vec,
    nrow = n_strata, ncol = n_trt,
    dimnames = list(levels(strata_factor), trt_lvls)
  )

  if (missing(pair)) {
    pair <- pairwise(trt_lvls)
  }
  assert_class(pair, "contrast")
  unobserved <- setdiff(attr(pair, "levels")[unlist(pair)], trt_lvls)
  if (length(unobserved) > 0L) {
    stop(
      "`pair` refers to treatment level(s) `",
      toString(unobserved),
      "` which are not observed in `data`."
    )
  }
  pair <- update_levels(pair, trt_lvls)

  mh_res <- h_mh_estimate(n_mat, r_mat, pair[[1]], pair[[2]], estimand, ci_type)
  coef_mat <- h_coef_mat(list(
    estimate = as.numeric(mh_res$estimate),
    se = as.numeric(mh_res$se),
    pair = pair
  ))
  estimate <- setNames(mh_res$estimate, rownames(coef_mat))
  se <- setNames(mh_res$se, rownames(coef_mat))

  missing_vars <- setdiff(trt_vars$strata, analysis_strata)
  # Analysis strata finer than the randomization strata are still valid, so the
  # names alone are not enough to decide - compare the joint levels as well.
  # Only the analysis variables are complete-cased above, so the randomization
  # strata may still hold missing values; restrict the comparison to the rows
  # where they are observed, otherwise the differing `NA` positions alone would
  # make the nesting check fail and warn about strata that are in fact covered.
  covers_rand_strata <- if (length(missing_vars) == 0L) {
    TRUE
  } else {
    rand_strata <- h_mh_joint_strata(data[trt_vars$strata])
    observed <- !is.na(rand_strata)
    # `h_first_fct_nested_in_second()` drops unused levels itself.
    h_first_fct_nested_in_second(strata_factor[observed], rand_strata[observed])
  }
  if (!covers_rand_strata) {
    warning(
      "It looks like you have not included all of the variables that were used ",
      "during randomization in your analysis strata. Consider adding `",
      toString(missing_vars),
      "` to the right-hand side of `formula` to ensure valid stratified inference.",
      call. = FALSE
    )
  }

  events_table <- h_mh_events_table(n_mat, r_mat, analysis_strata)

  result <- list(
    formula = formula,
    randomization = treatment,
    schema = trt_vars$schema,
    vars = list(
      treatment = trt_var,
      response = response_var,
      strata = analysis_strata,
      randomization_strata = trt_vars$strata,
      levels = trt_lvls
    ),
    estimand = estimand,
    ci_type = ci_type,
    pair = pair,
    estimate = estimate,
    se = se,
    n_per_arm_stratum = n_mat,
    events_per_arm_stratum = r_mat,
    events_table = events_table,
    coef_mat = coef_mat
  )
  class(result) <- "mh_effect"
  result
}

#' Extract the Analysis Strata Variables of an `robin_mh` Formula
#'
#' The right hand side must be `1` or a sum of bare variable names; transformed
#' terms such as `factor(x)` are rejected because they would be silently
#' ignored when the joint strata are built from the raw columns of `data`.
#'
#' @param formula (`formula`) Two-sided analysis formula.
#' @return A `character` vector of variable names, empty for `y ~ 1`.
#' @keywords internal
h_mh_strata_vars <- function(formula) {
  term_labels <- attr(terms(formula), "term.labels")
  not_names <- term_labels[!vapply(term_labels, function(x) is.name(str2lang(x)), logical(1L))]
  if (length(not_names) > 0L) {
    stop(
      "Right hand side of `formula` must be `1` or a sum of variable names. ",
      "Transformed or interaction terms are not allowed: `",
      toString(not_names),
      "`."
    )
  }
  term_labels
}

#' Coerce and Validate a Binary `robin_mh` Response
#'
#' Accepts `0`/`1` numeric, `logical`, or a two-level `factor` where the second
#' level is taken as the event.
#'
#' @param y (`vector`) Raw response column.
#' @param response_var (`string`) Column name, used in error messages.
#' @return An `integer` vector of `0`/`1`.
#' @keywords internal
h_mh_response <- function(y, response_var) {
  if (is.factor(y)) {
    if (nlevels(y) != 2L) {
      stop(
        "Response `", response_var, "` is a factor with ", nlevels(y),
        " levels; a binary response with exactly 2 levels is required."
      )
    }
    return(as.integer(y) - 1L)
  }
  if (is.logical(y)) {
    y <- as.integer(y)
  }
  assert_integerish(y, lower = 0L, upper = 1L, any.missing = FALSE, .var.name = response_var)
  as.integer(y)
}

#' Build the Joint Analysis Strata Factor
#'
#' Unlike [interaction()], the stratum identity is derived from the level
#' *codes* of the input columns rather than from pasted labels, so levels that
#' happen to contain the separator (e.g. `"x:y"` crossed with `"z"` versus
#' `"x"` crossed with `"y:z"`) remain distinct strata instead of silently
#' collapsing into one. Labels are only cosmetic and are made unique for
#' printing.
#'
#' @param df (`data.frame`) Analysis strata columns, without missing values.
#' @return A `factor` of observed joint strata, with `interaction()`-style
#'   `:`-separated labels and the first variable varying fastest.
#' @keywords internal
h_mh_joint_strata <- function(df) {
  fcts <- lapply(df, as.factor)
  n_lvls <- vapply(fcts, nlevels, integer(1L))
  # Mixed-radix index with the first variable varying fastest, matching the
  # level order that `interaction()` would produce.
  radix <- cumprod(c(1L, n_lvls[-length(n_lvls)]))
  flat <- 1L + as.integer(Reduce(`+`, Map(function(f, m) (as.integer(f) - 1L) * m, fcts, radix)))

  observed <- sort(unique(flat))
  # Decode each observed index back into its per-variable level for labelling.
  grid <- arrayInd(observed, .dim = n_lvls)
  labels <- Map(function(f, i) levels(f)[grid[, i]], fcts, seq_along(fcts))
  factor(
    flat,
    levels = observed,
    labels = make.unique(do.call(paste, c(labels, sep = ":")))
  )
}

#' Mantel-Haenszel Estimate and Variance for All Pairs
#'
#' Vectorised over `K` joint strata and `P` treatment-pair contrasts.
#'
#' @param n_mat (`matrix`) `K x J` matrix of cell sizes.
#' @param r_mat (`matrix`) `K x J` matrix of within-cell event sums (`y == 1`).
#' @param exp_idx (`integer`) Treatment-arm column indices for the experimental
#'   side of each pair.
#' @param ref_idx (`integer`) Treatment-arm column indices for the reference
#'   side of each pair.
#' @param estimand (`character(1)`) `"ATE"` or `"MH"`.
#' @param ci_type (`character(1)`) `"mGR"`, `"GR"`, or `"Sato"`.
#' @return A list with elements `estimate` and `se`, both length `P`.
#' @keywords internal
h_mh_estimate <- function(n_mat, r_mat, exp_idx, ref_idx, estimand, ci_type) {
  n1k <- n_mat[, exp_idx, drop = FALSE]
  n0k <- n_mat[, ref_idx, drop = FALSE]
  n11k <- r_mat[, exp_idx, drop = FALSE]
  n10k <- r_mat[, ref_idx, drop = FALSE]

  safe_n1k <- pmax(n1k, 1)
  safe_n0k <- pmax(n0k, 1)
  n_sum_k <- n1k + n0k
  safe_sum_k <- pmax(n_sum_k, 1)

  # Empty cells need no masking: `n1k * n0k` is already zero whenever either arm
  # is empty, which also zeroes that stratum's contribution to the numerator.
  weight_mat <- n1k * n0k / safe_sum_k
  delta_mat <- n11k / safe_n1k - n10k / safe_n0k

  total_weight <- colSums(weight_mat)
  if (any(total_weight == 0)) {
    stop("No stratum has both treatment arms represented for at least one comparison.")
  }
  estimate <- colSums(weight_mat * delta_mat) / total_weight

  var_per_pair <- switch(ci_type,
    GR = h_mh_var_gr(n11k, n10k, n1k, n0k, total_weight),
    mGR = h_mh_var_mgr(n11k, n10k, n1k, n0k, weight_mat, total_weight),
    Sato = h_mh_var_sato(n11k, n10k, n1k, n0k, total_weight, estimate)
  )

  if (estimand == "ATE") {
    var_per_pair <- var_per_pair + h_mh_var_ate_nu(
      n1k = n1k, n0k = n0k, n11k = n11k, n10k = n10k,
      weight_mat = weight_mat, total_weight = total_weight,
      estimate = estimate
    )
  }
  negative <- var_per_pair < 0
  if (any(negative)) {
    warning("Negative variance estimate produced; standard error set to NA.", call. = FALSE)
  }
  se <- sqrt(replace(var_per_pair, negative, NA_real_))

  list(estimate = estimate, se = se)
}

#' Greenland-Robins Variance for the Mantel-Haenszel Risk Difference
#'
#' Vectorised across pairs.
#' @param n11k,n10k,n1k,n0k (`matrix`) `K x P` matrices of within-stratum
#'   counts and event counts.
#' @param total_weight (`numeric`) length-`P` column sums of the MH weights.
#' @return Length-`P` numeric vector of variance estimates.
#'
#' @details Strata with an empty arm need no explicit masking: every term of the
#'   numerator already carries a factor that is zero for such a cell.
#' @keywords internal
h_mh_var_gr <- function(n11k, n10k, n1k, n0k, total_weight) {
  safe_n1k <- pmax(n1k, 1)
  safe_n0k <- pmax(n0k, 1)
  safe_sum_k <- pmax(n1k + n0k, 1)
  num <- n11k * (n1k - n11k) * n0k^3 + n10k * (n0k - n10k) * n1k^3
  den <- safe_n1k * safe_n0k * safe_sum_k^2
  colSums(num / den) / total_weight^2
}

#' Modified Greenland-Robins Variance for the Mantel-Haenszel Risk Difference
#' @inheritParams h_mh_var_gr
#' @param weight_mat (`matrix`) `K x P` matrix of MH weights.
#' @return Length-`P` numeric vector of variance estimates.
#' @keywords internal
h_mh_var_mgr <- function(n11k, n10k, n1k, n0k, weight_mat, total_weight) {
  safe_n1k <- pmax(n1k, 1)
  safe_n0k <- pmax(n0k, 1)
  n01k <- n1k - n11k
  n00k <- n0k - n10k
  help1 <- ifelse(n1k > 1, n1k / pmax(n1k - 1, 1), 1)
  help0 <- ifelse(n0k > 1, n0k / pmax(n0k - 1, 1), 1)
  var_k <- weight_mat^2 * (
    n11k * n01k / safe_n1k^3 * help1 +
      n10k * n00k / safe_n0k^3 * help0
  )
  colSums(var_k) / total_weight^2
}

#' Sato Variance for the Mantel-Haenszel Risk Difference
#' @inheritParams h_mh_var_gr
#' @param estimate (`numeric`) Length-`P` vector of MH point estimates.
#' @return Length-`P` numeric vector of variance estimates.
#' @keywords internal
h_mh_var_sato <- function(n11k, n10k, n1k, n0k, total_weight, estimate) {
  safe_sum_k <- pmax(n1k + n0k, 1)
  pk <- (n1k^2 * n10k - n0k^2 * n11k + n1k * n0k * (n0k - n1k) / 2) / safe_sum_k^2
  qk <- (n11k * (n0k - n10k) + n10k * (n1k - n11k)) / (2 * safe_sum_k)
  (estimate * colSums(pk) + colSums(qk)) / total_weight^2
}

#' ATE Additive Variance Correction
#'
#' Implements the `nu` correction from Bannick, Ye et al. used by `mGR` when
#' the target estimand is the ATE rather than the MH risk difference.
#'
#' @param n1k,n0k,n11k,n10k (`matrix`) `K x P` matrices of cell-level counts.
#' @param weight_mat (`matrix`) `K x P` MH weight matrix.
#' @param total_weight (`numeric`) length-`P` column sums of `weight_mat`.
#' @param estimate (`numeric`) Length-`P` MH point estimates.
#' @return Length-`P` numeric vector of correction terms.
#' @keywords internal
h_mh_var_ate_nu <- function(n1k, n0k, n11k, n10k, weight_mat, total_weight, estimate) {
  safe_n1k <- pmax(n1k, 1)
  safe_n0k <- pmax(n0k, 1)
  n_sum_k <- n1k + n0k
  safe_sum_k <- pmax(n_sum_k, 1)

  # `total_n > 0` is guaranteed by the `total_weight > 0` check in the caller.
  total_n <- colSums(n_sum_k)
  pi1 <- colSums(n1k) / total_n
  pi0 <- colSums(n0k) / total_n
  rho_k <- sweep(n_sum_k, 2L, total_n, "/")

  p1k <- n11k / safe_n1k
  p0k <- n10k / safe_n0k
  # `p * (1 - p) * n` is already zero where the cell holds at most one
  # observation, so no explicit guard is needed.
  y1_var_k <- p1k * (1 - p1k) * n1k / pmax(n1k - 1, 1)
  y0_var_k <- p0k * (1 - p0k) * n0k / pmax(n0k - 1, 1)
  p1k_sq <- p1k^2 - y1_var_k / safe_n1k
  p0k_sq <- p0k^2 - y0_var_k / safe_n0k
  delta_k_sq <- p1k_sq - 2 * p1k * p0k + p0k_sq
  delta_k <- p1k - p0k

  # Expand a per-pair value to a K x P matrix, constant down each column.
  per_pair <- function(x) matrix(x, nrow = nrow(n_sum_k), ncol = length(x), byrow = TRUE)
  est_mat <- per_pair(estimate)
  est_sq_mat <- est_mat^2
  pi1_pi0_mat <- per_pair(pi1 * pi0)

  tmp1 <- rho_k * (delta_k_sq - est_sq_mat)
  fac <- pi1_pi0_mat *
    (n_sum_k - 1) / safe_sum_k *
    (n_sum_k - 1 - (4 * n_sum_k - 6) * pi1_pi0_mat)
  tmp2 <- (delta_k_sq - 2 * delta_k * est_mat + est_sq_mat) * fac

  mask <- weight_mat != 0
  tmp1[!mask] <- 0
  tmp2[!mask] <- 0

  # `tmp2`'s per-pair `1 / total_n` factor is applied here rather than expanded
  # into a full `K x P` matrix above.
  numer <- pi1^2 * pi0^2 * colSums(tmp1) + colSums(tmp2) / total_n
  numer / total_n / (total_weight / total_n)^2
}

#' Build the Events / Patient Counts Table for `mh_effect`
#'
#' @param n_mat (`matrix`) `K x J` matrix of cell sizes.
#' @param r_mat (`matrix`) `K x J` matrix of within-cell event counts.
#' @param analysis_strata (`character`) Analysis strata variable names; when
#'   empty the `Stratum` column is dropped.
#' @return A `data.frame` with one row per stratum and treatment arm.
#' @keywords internal
h_mh_events_table <- function(n_mat, r_mat, analysis_strata) {
  rows <- expand.grid(
    Stratum = rownames(n_mat),
    Treatment = colnames(n_mat),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  rows$Patients <- as.integer(n_mat)
  rows$Events <- as.integer(r_mat)
  if (length(analysis_strata) == 0L) {
    rows$Stratum <- NULL
  }
  rows
}
