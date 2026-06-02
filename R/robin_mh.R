#' Mantel-Haenszel Risk Difference and Average Treatment Effect
#'
#' Estimate the (stratified) Mantel-Haenszel risk difference, or the
#' corresponding Average Treatment Effect (ATE), for a binary outcome under
#' stratified or covariate-adaptive randomization.
#'
#' @param formula (`formula`) Response formula of the form `y ~ s1 + s2` where
#'   the LHS is a binary 0/1 outcome and the RHS variables define the joint
#'   analysis strata. Use `y ~ 1` for the unstratified case.
#' @param data (`data.frame`) Input data frame.
#' @param treatment (`formula`) A treatment formula `treatment ~ scheme(vars)`
#'   following the same grammar as the rest of the package (`sr`, `pb`, `ps`).
#'   The randomization scheme is informational; if any randomization
#'   stratification variable is not part of the analysis strata a warning is
#'   emitted, paralleling [robin_surv()].
#' @param estimand (`character(1)`) Either `"ATE"` (default) for the average
#'   treatment effect, or `"MH"` for the Mantel-Haenszel risk difference.
#' @param ci_type (`character(1)`) Variance estimator:
#'   `"mGR"` (default, modified Greenland-Robins),
#'   `"GR"` (Greenland-Robins), or `"Sato"` (Sato).
#'   `estimand = "ATE"` requires `ci_type = "mGR"` and additionally adds the
#'   Bannick-Ye nu correction term.
#' @param pair (`contrast`) Optional contrast specification (default: all
#'   pairwise comparisons, with the higher-indexed level as the experimental
#'   arm).
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
  attr(formula, ".Environment") <- environment()
  assert_formula(formula)
  assert_true(identical(length(formula), 3L))
  assert_data_frame(data)
  assert_formula(treatment)
  estimand <- match.arg(estimand)
  ci_type <- match.arg(ci_type)
  if (estimand == "ATE" && ci_type != "mGR") {
    stop("`estimand = 'ATE'` requires `ci_type = 'mGR'`.")
  }

  trt_vars <- h_get_vars(treatment)
  trt_var <- trt_vars$treatment

  response_var <- all.vars(formula[[2]])
  if (length(response_var) != 1L) {
    stop("Left hand side of `formula` must be a single response variable.")
  }
  analysis_strata <- setdiff(all.vars(formula[[3]]), ".")

  needed <- unique(c(response_var, trt_var, analysis_strata))
  assert_subset(needed, names(data))

  data <- stats::na.omit(data[, needed, drop = FALSE])
  if (nrow(data) == 0L) {
    stop("No complete observations remain after removing missing values.")
  }

  y <- data[[response_var]]
  if (is.logical(y)) y <- as.integer(y)
  assert_integerish(y, lower = 0L, upper = 1L, any.missing = FALSE)
  y <- as.integer(y)

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
    strata_factor <- interaction(data[, analysis_strata, drop = FALSE], drop = TRUE, sep = ":")
  }
  n_strata <- nlevels(strata_factor)
  strata_idx <- as.integer(strata_factor)

  flat <- (trt_idx - 1L) * n_strata + strata_idx
  n_vec <- tabulate(flat, nbins = n_strata * n_trt)
  r_vec <- tabulate(flat[y == 1L], nbins = n_strata * n_trt)
  n_mat <- matrix(n_vec, nrow = n_strata, ncol = n_trt,
                  dimnames = list(levels(strata_factor), trt_lvls))
  r_mat <- matrix(r_vec, nrow = n_strata, ncol = n_trt,
                  dimnames = list(levels(strata_factor), trt_lvls))

  if (missing(pair)) {
    pair <- pairwise(trt_lvls)
  }
  pair <- update_levels(pair, trt_lvls)
  exp_idx <- pair[[1]]
  ref_idx <- pair[[2]]
  n_pair <- length(exp_idx)

  mh_res <- h_mh_estimate(n_mat, r_mat, exp_idx, ref_idx, estimand, ci_type)
  pair_labels <- sprintf("%s v.s. %s", trt_lvls[exp_idx], trt_lvls[ref_idx])
  estimate <- setNames(as.numeric(mh_res$estimate), pair_labels)
  se <- setNames(as.numeric(mh_res$se), pair_labels)

  z_value <- estimate / se
  p_value <- 2 * pnorm(-abs(z_value))
  coef_mat <- matrix(
    c(estimate, se, z_value, p_value),
    nrow = n_pair,
    dimnames = list(pair_labels, c("Estimate", "Std.Err", "Z Value", "Pr(>|z|)"))
  )

  give_rand_strat_warning <- length(setdiff(trt_vars$strata, analysis_strata)) > 0L
  if (give_rand_strat_warning) {
    missing_vars <- setdiff(trt_vars$strata, analysis_strata)
    warning(
      paste0(
        "It looks like you have not included all of the variables that were used ",
        "during randomization in your analysis strata. Consider adding `",
        toString(missing_vars),
        "` to the right-hand side of `formula` to ensure valid stratified inference."
      ),
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

  weight_mat <- n1k * n0k / safe_sum_k
  weight_mat[n_sum_k == 0] <- 0

  delta_mat <- n11k / safe_n1k - n10k / safe_n0k
  delta_mat[n1k == 0 | n0k == 0] <- 0

  total_weight <- colSums(weight_mat)
  if (any(total_weight == 0)) {
    stop("No stratum has both treatment arms represented for at least one comparison.")
  }
  estimate <- colSums(weight_mat * delta_mat) / total_weight

  var_per_pair <- switch(ci_type,
    GR = h_mh_var_gr(n11k, n10k, n1k, n0k, weight_mat, total_weight),
    mGR = h_mh_var_mgr(n11k, n10k, n1k, n0k, weight_mat, total_weight),
    Sato = h_mh_var_sato(n11k, n10k, n1k, n0k, weight_mat, total_weight, estimate)
  )

  if (estimand == "ATE") {
    var_per_pair <- var_per_pair + h_mh_var_ate_nu(
      n1k = n1k, n0k = n0k, n11k = n11k, n10k = n10k,
      weight_mat = weight_mat, total_weight = total_weight,
      estimate = estimate
    )
  }
  if (any(var_per_pair < 0)) {
    warning("Negative variance estimate produced; standard error set to NA.", call. = FALSE)
  }
  se <- ifelse(var_per_pair >= 0, sqrt(pmax(var_per_pair, 0)), NA_real_)

  list(estimate = estimate, se = se)
}

#' Greenland-Robins Variance for the Mantel-Haenszel Risk Difference
#'
#' Vectorised across pairs.
#' @param n11k,n10k,n1k,n0k (`matrix`) `K x P` matrices of within-stratum
#'   counts and event counts.
#' @param weight_mat (`matrix`) `K x P` matrix of MH weights.
#' @param total_weight (`numeric`) length-`P` column sums of `weight_mat`.
#' @return Length-`P` numeric vector of variance estimates.
#' @keywords internal
h_mh_var_gr <- function(n11k, n10k, n1k, n0k, weight_mat, total_weight) {
  safe_n1k <- pmax(n1k, 1)
  safe_n0k <- pmax(n0k, 1)
  safe_sum_k <- pmax(n1k + n0k, 1)
  num <- n11k * (n1k - n11k) * n0k^3 + n10k * (n0k - n10k) * n1k^3
  den <- safe_n1k * safe_n0k * safe_sum_k^2
  var_k <- num / den
  var_k[weight_mat == 0] <- 0
  colSums(var_k) / total_weight^2
}

#' Modified Greenland-Robins Variance for the Mantel-Haenszel Risk Difference
#' @inheritParams h_mh_var_gr
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
  var_k[weight_mat == 0] <- 0
  colSums(var_k) / total_weight^2
}

#' Sato Variance for the Mantel-Haenszel Risk Difference
#' @inheritParams h_mh_var_gr
#' @param estimate (`numeric`) Length-`P` vector of MH point estimates.
#' @return Length-`P` numeric vector of variance estimates.
#' @keywords internal
h_mh_var_sato <- function(n11k, n10k, n1k, n0k, weight_mat, total_weight, estimate) {
  safe_sum_k <- pmax(n1k + n0k, 1)
  pk <- (n1k^2 * n10k - n0k^2 * n11k + n1k * n0k * (n0k - n1k) / 2) / safe_sum_k^2
  qk <- (n11k * (n0k - n10k) + n10k * (n1k - n11k)) / (2 * safe_sum_k)
  pk[weight_mat == 0] <- 0
  qk[weight_mat == 0] <- 0
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

  total_n <- colSums(n_sum_k)
  if (any(total_n == 0)) {
    stop("No observations available for at least one treatment comparison.")
  }
  pi1 <- colSums(n1k) / total_n
  pi0 <- colSums(n0k) / total_n
  rho_k <- sweep(n_sum_k, 2L, total_n, "/")

  p1k <- n11k / safe_n1k
  p0k <- n10k / safe_n0k
  y1_var_k <- ifelse(n1k > 1, p1k * (1 - p1k) * n1k / pmax(n1k - 1, 1), 0)
  y0_var_k <- ifelse(n0k > 1, p0k * (1 - p0k) * n0k / pmax(n0k - 1, 1), 0)
  p1k_sq <- p1k^2 - ifelse(n1k > 1, y1_var_k, 0) / safe_n1k
  p0k_sq <- p0k^2 - ifelse(n0k > 1, y0_var_k, 0) / safe_n0k
  delta_k_sq <- p1k_sq - 2 * p1k * p0k + p0k_sq
  delta_k <- p1k - p0k

  est_mat <- matrix(estimate, nrow = nrow(n_sum_k), ncol = length(estimate), byrow = TRUE)
  pi1_mat <- matrix(pi1, nrow = nrow(n_sum_k), ncol = length(estimate), byrow = TRUE)
  pi0_mat <- matrix(pi0, nrow = nrow(n_sum_k), ncol = length(estimate), byrow = TRUE)
  total_n_mat <- matrix(total_n, nrow = nrow(n_sum_k), ncol = length(estimate), byrow = TRUE)

  tmp1 <- rho_k * (delta_k_sq - est_mat^2)
  fac <- pi1_mat * pi0_mat *
    (n_sum_k - 1) / safe_sum_k *
    (n_sum_k - 1 - (4 * n_sum_k - 6) * pi1_mat * pi0_mat) / total_n_mat
  tmp2 <- (delta_k_sq - 2 * delta_k * est_mat + est_mat^2) * fac

  mask <- weight_mat != 0
  tmp1[!mask] <- 0
  tmp2[!mask] <- 0

  numer <- pi1^2 * pi0^2 * colSums(tmp1) + colSums(tmp2)
  numer / total_n / (total_weight / total_n)^2
}

#' Build the Events / Patient Counts Table for `mh_effect`
#' @keywords internal
h_mh_events_table <- function(n_mat, r_mat, analysis_strata) {
  trt_lvls <- colnames(n_mat)
  strata_lvls <- rownames(n_mat)
  rows <- expand.grid(
    Stratum = strata_lvls,
    Treatment = trt_lvls,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  rows$Patients <- as.integer(c(n_mat))
  rows$Events <- as.integer(c(r_mat))
  if (length(analysis_strata) == 0L) {
    rows$Stratum <- NULL
  } else {
    names(rows)[1L] <- "Stratum"
  }
  rows
}
