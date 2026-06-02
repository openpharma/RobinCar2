# Reference Mantel-Haenszel statistics computed from textbook formulas, used as
# a self-contained ground truth (no RobinCar dependency at test time).

h_mh_reference <- function(df, treat_col, response_col, strata_cols,
                           exp_lvl, ref_lvl, estimand = "ATE", ci_type = "mGR") {
  df <- as.data.frame(df)
  if (length(strata_cols) > 0L) {
    strata <- interaction(df[, strata_cols, drop = FALSE], drop = TRUE, sep = ":")
  } else {
    strata <- factor(rep("all", nrow(df)))
  }
  is_exp <- df[[treat_col]] == exp_lvl
  is_ref <- df[[treat_col]] == ref_lvl
  keep <- is_exp | is_ref
  df <- df[keep, , drop = FALSE]
  strata <- droplevels(strata[keep])
  is_exp <- df[[treat_col]] == exp_lvl
  is_ref <- df[[treat_col]] == ref_lvl
  y <- as.integer(df[[response_col]])

  agg <- function(idx) {
    sub <- df[idx, , drop = FALSE]
    s <- strata[idx]
    list(
      n = as.integer(table(s)),
      r = as.integer(table(s[y[idx] == 1L]))
    )
  }
  s_lvls <- levels(strata)
  n1k <- as.integer(table(factor(strata[is_exp], levels = s_lvls)))
  n0k <- as.integer(table(factor(strata[is_ref], levels = s_lvls)))
  r1 <- strata[is_exp & y == 1L]
  r0 <- strata[is_ref & y == 1L]
  n11k <- as.integer(table(factor(r1, levels = s_lvls)))
  n10k <- as.integer(table(factor(r0, levels = s_lvls)))

  weight <- n1k * n0k / (n1k + n0k)
  weight[(n1k + n0k) == 0L] <- 0
  delta <- ifelse(n1k > 0L & n0k > 0L, n11k / pmax(n1k, 1) - n10k / pmax(n0k, 1), 0)
  est <- sum(weight * delta) / sum(weight)

  var_gr <- {
    num <- n11k * (n1k - n11k) * n0k^3 + n10k * (n0k - n10k) * n1k^3
    den <- pmax(n1k, 1) * pmax(n0k, 1) * pmax(n1k + n0k, 1)^2
    sum((num / den)[weight != 0]) / sum(weight)^2
  }
  var_mgr <- {
    n01k <- n1k - n11k
    n00k <- n0k - n10k
    h1 <- ifelse(n1k > 1, n1k / pmax(n1k - 1, 1), 1)
    h0 <- ifelse(n0k > 1, n0k / pmax(n0k - 1, 1), 1)
    vk <- weight^2 * (
      n11k * n01k / pmax(n1k, 1)^3 * h1 +
        n10k * n00k / pmax(n0k, 1)^3 * h0
    )
    sum(vk[weight != 0]) / sum(weight)^2
  }
  var_sato <- {
    pk <- (n1k^2 * n10k - n0k^2 * n11k + n1k * n0k * (n0k - n1k) / 2) /
      pmax(n1k + n0k, 1)^2
    qk <- (n11k * (n0k - n10k) + n10k * (n1k - n11k)) / (2 * pmax(n1k + n0k, 1))
    (est * sum(pk[weight != 0]) + sum(qk[weight != 0])) / sum(weight)^2
  }
  var_ate_extra <- {
    n_sum_k <- n1k + n0k
    total_n <- sum(n_sum_k)
    pi1 <- sum(n1k) / total_n
    pi0 <- sum(n0k) / total_n
    rho_k <- n_sum_k / total_n
    p1k <- n11k / pmax(n1k, 1)
    p0k <- n10k / pmax(n0k, 1)
    y1var <- ifelse(n1k > 1, p1k * (1 - p1k) * n1k / pmax(n1k - 1, 1), 0)
    y0var <- ifelse(n0k > 1, p0k * (1 - p0k) * n0k / pmax(n0k - 1, 1), 0)
    p1k_sq <- p1k^2 - ifelse(n1k > 1, y1var, 0) / pmax(n1k, 1)
    p0k_sq <- p0k^2 - ifelse(n0k > 1, y0var, 0) / pmax(n0k, 1)
    delta_sq <- p1k_sq - 2 * p1k * p0k + p0k_sq
    delta_k <- p1k - p0k
    tmp1 <- rho_k * (delta_sq - est^2)
    tmp2 <- (delta_sq - 2 * delta_k * est + est^2) *
      pi1 * pi0 * (n_sum_k - 1) / pmax(n_sum_k, 1) *
      (n_sum_k - 1 - (4 * n_sum_k - 6) * pi1 * pi0) / total_n
    nz <- weight != 0
    (sum(tmp1[nz]) * pi1^2 * pi0^2 + sum(tmp2[nz])) /
      total_n / (sum(weight) / total_n)^2
  }

  v <- switch(ci_type, GR = var_gr, mGR = var_mgr, Sato = var_sato)
  if (estimand == "ATE") {
    v <- v + var_ate_extra
  }
  list(estimate = est, se = sqrt(v))
}

# Build a small two-arm subset for the simple checks.
df_two <- subset(glm_data, treatment != "trt2")
df_two$treatment <- droplevels(df_two$treatment)

# robin_mh basic behaviour ----

test_that("robin_mh returns mh_effect with the expected components", {
  expect_silent(
    res <- robin_mh(
      y_b ~ s1 + s2,
      data = df_two,
      treatment = treatment ~ pb(s1, s2)
    )
  )
  expect_s3_class(res, "mh_effect")
  expect_named(
    res,
    c(
      "formula", "randomization", "schema", "vars", "estimand", "ci_type",
      "pair", "estimate", "se", "n_per_arm_stratum",
      "events_per_arm_stratum", "events_table", "coef_mat"
    )
  )
  expect_equal(rownames(res$coef_mat), "trt1 v.s. pbo")
  expect_equal(colnames(res$coef_mat), c("Estimate", "Std.Err", "Z Value", "Pr(>|z|)"))
})

test_that("robin_mh matches reference formulas across estimand and variance choices", {
  ref_specs <- expand.grid(
    estimand = c("MH", "ATE"),
    ci_type = c("GR", "mGR", "Sato"),
    stringsAsFactors = FALSE
  )
  ref_specs <- ref_specs[!(ref_specs$estimand == "ATE" & ref_specs$ci_type != "mGR"), ]
  for (i in seq_len(nrow(ref_specs))) {
    estimand <- ref_specs$estimand[i]
    ci_type <- ref_specs$ci_type[i]
    res <- robin_mh(
      y_b ~ s1 + s2,
      data = df_two,
      treatment = treatment ~ pb(s1, s2),
      estimand = estimand,
      ci_type = ci_type
    )
    ref <- h_mh_reference(
      df_two, "treatment", "y_b", c("s1", "s2"),
      exp_lvl = "trt1", ref_lvl = "pbo",
      estimand = estimand, ci_type = ci_type
    )
    expect_equal(unname(res$estimate), ref$estimate, tolerance = 1e-12,
      info = paste(estimand, ci_type))
    expect_equal(unname(res$se), ref$se, tolerance = 1e-12,
      info = paste(estimand, ci_type))
  }
})

test_that("robin_mh handles unstratified analysis (y ~ 1) and reduces to plain risk diff", {
  expect_warning(
    res <- robin_mh(
      y_b ~ 1,
      data = df_two,
      treatment = treatment ~ pb(s1, s2),
      estimand = "MH",
      ci_type = "GR"
    ),
    "not included all of the variables that were used during randomization"
  )
  # With one stratum the MH risk difference is the unweighted prevalence diff.
  p1 <- mean(df_two$y_b[df_two$treatment == "trt1"])
  p0 <- mean(df_two$y_b[df_two$treatment == "pbo"])
  expect_equal(unname(res$estimate), p1 - p0)
})

test_that("robin_mh extends to multi-arm pairwise comparisons and is consistent with two-arm fits", {
  res_all <- robin_mh(
    y_b ~ s1 + s2,
    data = glm_data,
    treatment = treatment ~ pb(s1, s2),
    estimand = "ATE", ci_type = "mGR"
  )
  expect_equal(length(res_all$estimate), 3L)
  # Compare the trt1 vs pbo pair to a two-arm fit on the corresponding subset.
  res_a <- robin_mh(
    y_b ~ s1 + s2,
    data = df_two,
    treatment = treatment ~ pb(s1, s2),
    estimand = "ATE", ci_type = "mGR"
  )
  expect_equal(res_all$estimate[["trt1 v.s. pbo"]], unname(res_a$estimate))
  expect_equal(res_all$se[["trt1 v.s. pbo"]], unname(res_a$se))
})

test_that("robin_mh accepts a custom contrast (against_ref)", {
  res <- robin_mh(
    y_b ~ s1 + s2,
    data = glm_data,
    treatment = treatment ~ pb(s1, s2),
    pair = against_ref(levels(glm_data$treatment), ref = "pbo")
  )
  expect_equal(rownames(res$coef_mat), c("trt1 v.s. pbo", "trt2 v.s. pbo"))
})

test_that("robin_mh validates inputs", {
  expect_error(
    robin_mh(y ~ s1, data = glm_data, treatment = treatment ~ pb(s1)),
    "lower|upper|integerish"
  )
  expect_error(
    robin_mh(y_b ~ s1, data = glm_data, treatment = treatment ~ pb(s1),
      estimand = "ATE", ci_type = "GR"),
    "ci_type = 'mGR'"
  )
  expect_error(
    robin_mh(~y_b, data = glm_data, treatment = treatment ~ pb(s1)),
    "treatment formula must be of type|formula"
  )
})

test_that("robin_mh emits a warning if randomization strata are not analysis strata", {
  expect_warning(
    robin_mh(y_b ~ s1, data = glm_data, treatment = treatment ~ pb(s1, s2)),
    "Consider adding `s2`"
  )
})

# Print snapshot ----

test_that("robin_mh print method produces the expected output", {
  res <- robin_mh(
    y_b ~ s1 + s2,
    data = df_two,
    treatment = treatment ~ pb(s1, s2)
  )
  expect_snapshot(print(res))
})

# table() and confint() ----

test_that("table.mh_effect returns the events table invisibly", {
  res <- robin_mh(
    y_b ~ s1 + s2,
    data = df_two,
    treatment = treatment ~ pb(s1, s2)
  )
  out <- capture.output(tab <- table(res))
  expect_s3_class(tab, "data.frame")
  expect_named(tab, c("Stratum", "Treatment", "Patients", "Events"))
  expect_equal(sum(tab$Patients), nrow(df_two))
  expect_equal(sum(tab$Events), sum(df_two$y_b))
})

test_that("confint.mh_effect returns Wald confidence intervals consistent with the SE", {
  res <- robin_mh(
    y_b ~ s1 + s2,
    data = df_two,
    treatment = treatment ~ pb(s1, s2)
  )
  ci <- confint(res, level = 0.95)
  z <- qnorm(0.975)
  expect_equal(unname(ci[, "Estimate"]), unname(res$estimate))
  expect_equal(unname(ci[, "2.5 %"]), unname(res$estimate - z * res$se))
  expect_equal(unname(ci[, "97.5 %"]), unname(res$estimate + z * res$se))
})
