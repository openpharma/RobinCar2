# Design note
# Author: X. Gregory Chen

# [Refenrece]
# Ye, T., Bannick, M., Yi, Y. and Shao, J., 2023. Robust variance estimation for
# covariate-adjusted unconditional treatment effect in randomized clinical trials
# with binary outcomes. Statistical Theory and Related Fields, 7(2), pp.159-163.
# DOI: https://doi.org/10.1080/24754269.2023.2205802

# Notation in this script tries to follow the above reference
# (_hat used to denote an estimate is dropped for brevity, but shout be clear in the context)
#
# Y is a continuous or binary random variate
#
# E(Y|A,X) = g^(-1)( beta_A'A + beta_X'X ),
#   where A is a dummy vector for treatment 1,..,or K
#         X is a covariate vector to be adjusted
#         beta is regression coef vector, in particular beta_A relates to treatment effect
#         g(.) is the link function, depending on distribution of Y and interested eff measure
#
# mu_t = g^(-1)( beta_A'a_t + beta_X'X ), an estimated/predicted E(Y|A=t,X) per subject
#   where t \in {1,...,K}
#         a_t is a vector of length K with t^th element being 1 and 0 otherwise
#
# theta_t = mean(mu_t)
#
# f(theta;t,s) =
#   1. theta_t - theta_s (diff, risk diff)
#   2. theta_t / theta_s (risk ratio)
#   3. log(odds(theta_t) / odds(theta_s)) (odds ratio)
#   where odds(p) = p/(1-p)
#         t,s are treatment index taking distinct values from the set {1,...,K}
#
# asympt.cov of estimated f(theta;t,s) = vcov_f_core/sqrt(n)
# and asympt.cov of theta vcov_f_core =
#   1. v_tt - 2*v_ts + v_ss
#   2. v_tt/(theta_t^2) - 2*v_ts/(theta_t*theta_s) + v_ss/(theta_s^2)
#   3. v_tt/(op(theta_t)^2) - 2*v_ts/(op(theta_t)*op(theta_s)) + v_ss/(op(theta_s)^2)
#   where n is the sample size
#         op(p) = p*(1-p)
#
#         let pi_t be the sample proportion of subjects received treatment t
#         I_t = a subset of all subjects {1,...,n} that received treatment t
#
#         v_tt = pi_t^(-1) * var(Y_j - mu_t_j | j \in I_t) +
#                2*cov(Y_j,mu_t_j | j \in I_t) -
#                var(mu_t)
#
#         v_ts = cov(Y_j,mu_s_j | j \in I_t) +
#                cov(Y_j,mu_t_j | j \in I_s) -
#                cov(mu_t,mu_s)


### prepare data and 'fit' -----------------------------------------------------
# quote from "design_lml.Rmd"
n <- 200
block_rand <- function(block = c(0, 0, 1, 1)) {
  function(n) {
    r <- lapply(seq_len(ceiling(n / length(block))), function(i) {
      sample(block)
    })
    unlist(r)[1:n]
  }
}
by_strata <- function(f, strata) {
  ret <- rep(NA, length(strata))
  for (x in split(seq_len(length(strata)), strata)) {
    ret[x] <- f(length(x))
  }
  return(ret)
}
sim_data <- function(n) {
  cov1 <- rnorm(n)
  z1 <- sample(size = n, factor(c("a", "b")), c(0.5, 0.5), replace = TRUE)
  z2 <- sample(size = n, factor(c("x", "y", "z")), c(0.4, 0.3, 0.3), replace = TRUE)
  permute_block <- c(0, 0, 1, 1)
  trt <- by_strata(block_rand(c("trt1", "trt1", "trt2", "trt2", "pbo", "pbo")), interaction(z1, z2))
  trt <- factor(trt, levels = c("pbo", "trt1", "trt2"))
  df <- data.frame(trt, z1, z2, cov1)
  x_mat <- model.matrix(~ trt + z1 + z2 + cov1, data = df)
  coef <- c(0.2, 0.5, 0.7, 1.5, -1, -0.5, 0.2)
  theta <- x_mat %*% coef
  y <- plogis(theta)
  y_bin <- as.integer(runif(n) < y)

  df$y <- y_bin
  df
}

d <- sim_data(500)
fit <- glm(y ~ trt:z1 + trt, family = binomial(), data = d)

### Actual start-point, with an input of
### an R object 'fit', from stat::lm() or stat::glm()

### general helper --------------------------------------------------------------

get_eff_measure <- function(fit) {
  if (identical(class(fit), "lm")) {
    out_eff <- "diff"
  } else if ("glm" %in% class(fit)) {
    use_family <- fit$family$family
    use_link <- fit$family$link
    if (use_family == "binomial" & use_link == "logit") {
      out_eff <- "odds ratio"
    } else if (use_family == "binomial" & use_link == "identity") {
      out_eff <- "diff"
    } else if (use_family %in% c("binomial", "poisson") & use_link == "log") {
      out_eff <- "risk ratio"
    } else {
      stop("pity, specified family and link function in glm() are not supported, may be in a future version...")
    }
  } else {
    stop("fit object should be either from stat::lm() or stat::glm()")
  }
  out_eff
}

### g-computation and its VCOV -------------------------------------------------

# func: prepare data
get_countfact_pred <- function(fit, trt_var_name) {
  treatments <- fit$xlevels[[trt_var_name]]
  use_dat <- fit$model

  dat_counterfact <- sapply(treatments, function(t) {
    tmp <- use_dat
    tmp$y_observed <- use_dat[, 1]
    tmp$trt_observed <- use_dat[[trt_var_name]]
    tmp$trt_counterfact <- t
    tmp
  }, simplify = FALSE)

  out_counterfact <- sapply(treatments, function(t) {
    outtmp <- tmp <- dat_counterfact[[t]]
    tmp[[trt_var_name]] <- tmp$trt_counterfact
    outtmp$mu_t <- predict(fit, newdata = tmp, type = "response")
    outtmp$theta_t <- mean(outtmp$mu_t)
    outtmp$resid_mu_t <- outtmp[, 1] - outtmp$mu_t
    outtmp$resid_theta_t <- outtmp[, 1] - outtmp$mu_t
    outtmp
  }, simplify = FALSE)

  out_counterfact
}

# Step 1: get g-computation theta_t
pred_counterfact <- get_countfact_pred(fit, "trt")
theta_t <- sapply(pred_counterfact, function(dat_t) unique(dat_t$theta_t))

# func: calculate V
calculate_V <- function(t, s, counterfact_res) {
  if (t == s) { # : calculate v_tt
    # v_tt = pi_t^(-1) * var(Y_j - mu_t_j | j \in I_t) +
    #        2*cov(Y_j,mu_t_j | j \in I_t) -
    #        var(mu_t)
    calres <- counterfact_res[[t]]

    n <- nrow(calres)
    It_ind <- (calres$trt_observed == t)
    pi_t <- sum(It_ind) / n

    var_y_mut_It <- var(calres$resid_mu_t[It_ind])
    cov_y_mut_It <- cov(calres$y_observed[It_ind], calres$mu_t[It_ind])
    var_mut <- var(calres$mu_t)

    v_out <- var_y_mut_It / pi_t + 2 * cov_y_mut_It - var_mut
  } else { # : calculate v_ts
    # v_ts = cov(Y_j,mu_s_j | j \in I_t) +
    #        cov(Y_j,mu_t_j | j \in I_s) -
    #        cov(mu_t,mu_s)

    calres_t <- counterfact_res[[t]]
    calres_s <- counterfact_res[[s]]

    n <- nrow(calres_t)
    It_ind <- (calres_s$trt_observed == t)
    Is_ind <- (calres_t$trt_observed == s)
    pi_t <- sum(It_ind) / n
    pi_s <- sum(Is_ind) / n

    cov_y_mus_It <- cov(calres_s$y_observed[It_ind], calres_s$mu_t[It_ind])
    cov_y_mut_Is <- cov(calres_t$y_observed[Is_ind], calres_t$mu_t[Is_ind])
    cov_mut_mus <- cov(calres_s$mu_t, calres_t$mu_t)

    v_out <- cov_y_mus_It + cov_y_mut_Is - cov_mut_mus
  }
  v_out
}

vectorized_calculate_V <- Vectorize(calculate_V, c("t", "s"))

# Substep (a) in Step 2: get V
treatments <- fit$xlevels[["trt"]]
V <- outer(
  X = treatments,
  Y = treatments,
  FUN = "vectorized_calculate_V",
  counterfact_res = pred_counterfact
)
rownames(V) <- colnames(V) <- treatments

# func: calculate n^(-1)f'Vf
op <- function(x, square = FALSE) (x * (1 - x))^ifelse(square, 2, 1)
odds <- function(x) (x / (1 - x))

calculate_fVf <- function(t, s, theta, V, n, eff_measure) {
  t_ind <- which(names(theta) == t)
  s_ind <- which(names(theta) == s)

  # depneding on eff_measure (1.diff; 2.risk ratio; 3.odds ratio)
  #   1. v_tt - 2*v_ts + v_ss
  #   2. v_tt/(theta_t^2) - 2*v_ts/(theta_t*theta_s) + v_ss/(theta_s^2)
  #   3. v_tt/(op(theta_t)^2) - 2*v_ts/(op(theta_t)*op(theta_s)) + v_ss/(op(theta_s)^2)
  if (eff_measure == "diff") {
    outtmp <- V[t_ind, t_ind] + V[s_ind, s_ind] - 2 * V[t_ind, s_ind]
  } else if (eff_measure == "risk ratio") {
    outtmp <- V[t_ind, t_ind] / (theta[t_ind]^2) +
      V[s_ind, s_ind] / (theta[s_ind]^2) -
      2 * V[t_ind, s_ind] / (theta[t_ind] * theta[s_ind])
  } else if (eff_measure == "odds ratio") {
    outtmp <- V[t_ind, t_ind] / op(theta[t_ind], square = T) +
      V[s_ind, s_ind] / op(theta[s_ind], square = T) -
      2 * V[t_ind, s_ind] / (op(theta[t_ind]) * op(theta[s_ind]))
  }
  outtmp / sqrt(n)
}

vectorized_calculate_fVf <- Vectorize(calculate_fVf, c("t", "s"))

# Substep (b) in Step 2: get V
n_fVf <- outer(
  X = treatments,
  Y = treatments,
  FUN = "vectorized_calculate_vcov_f",
  theta = theta_t,
  V = V,
  n = nrow(fit$model),
  eff_measure = get_eff_measure(fit)
)
rownames(n_fVf) <- colnames(n_fVf) <- treatments

# func: wrapper function given fit and pred_counterfact
calculate_f_vcov <- function(fit, trt_var_name, counterfact_pred) {
  treatments <- fit$xlevels[[trt_var_name]]

  theta_t <- sapply(counterfact_pred, function(dat_t) unique(dat_t$theta_t))

  V <- outer(
    X = treatments,
    Y = treatments,
    FUN = "vectorized_calculate_V",
    counterfact_res = counterfact_pred
  )
  rownames(V) <- colnames(V) <- treatments

  n_fVf <- outer(
    X = treatments,
    Y = treatments,
    FUN = "vectorized_calculate_fVf",
    theta = theta_t,
    V = V,
    n = nrow(fit$model),
    eff_measure = get_eff_measure(fit)
  )
  rownames(n_fVf) <- colnames(n_fVf) <- treatments

  out <- list(
    theta = theta_t,
    V = V,
    f_vcov = n_fVf,
    n = nrow(fit$model),
    eff_measure = get_eff_measure(fit),
    trt_var_name = trt_var_name,
    fit = fit
  )
  class(out) <- c(class(out), "vcov_rc")
  out
}

# func: reporting function
calculate_f <- function(theta_t, theta_s, eff_measure) {
  switch(eff_measure,
    "diff" = theta_t - theta_s,
    "risk ratio" = log(theta_t / theta_s),
    "odds ratio" = log(odds(theta_t) / odds(theta_s))
  )
}

report <- function(fit, trt_var_name, theta, f_vcov, digits = 3) {
  eff_measure <- get_eff_measure(fit)
  n <- nrow(fit$model)
  treatments <- fit$xlevels[[trt_var_name]]
  n.arm <- summary(fit$model[[trt_var_name]])
  n.arm.perc <- round(n.arm * 100 / n, 1) |> format(nsmall = 1)

  out_cases <- expand.grid(seq_along(theta), seq_along(theta))
  out_cases <- out_cases[out_cases[, 1] != out_cases[, 2], , drop = FALSE]
  t1 <- out_cases[, 1]
  t0 <- out_cases[, 2]

  outtmp <- data.frame(
    Total = n,
    Arm_1 = treatments[t1],
    N_Arm_1 = paste0(n.arm[t1], "(", n.arm.perc[t1], "%)"),
    Arm_0 = treatments[t0],
    N_Arm_0 = paste0(n.arm[t0], "(", n.arm.perc[t0], "%)"),
    Eff_measure = eff_measure,
    Estimate = calculate_f(theta[t1], theta[t0], eff_measure)
  )
  outtmp[["S.E."]] <- sqrt(f_vcov[cbind(t1, t0)])
  outtmp[["95%CI_lower"]] <- outtmp$Estimate + qnorm(0.025) * outtmp[["S.E."]]
  outtmp[["95%CI_upper"]] <- outtmp$Estimate + qnorm(0.975) * outtmp[["S.E."]]

  if (eff_measure == "diff") {
    outtmp["Estimate"] <- round(outtmp["Estimate"], digits) |> format(nsmall = digits)
    outtmp["S.E."] <- round(outtmp["S.E."], digits) |> format(nsmall = digits)
    outtmp["95%CI_lower"] <- round(outtmp["95%CI_lower"], digits) |> format(nsmall = digits)
    outtmp["95%CI_upper"] <- round(outtmp["95%CI_upper"], digits) |> format(nsmall = digits)
  } else {
    outtmp["S.E."] <- ((exp(outtmp["S.E."]) - 1) * exp(2 * outtmp["Estimate"] + outtmp["S.E."]^2)) |>
      round(digits) |>
      format(nsmall = digits)
    outtmp["Estimate"] <- exp(outtmp["Estimate"]) |>
      round(digits) |>
      format(nsmall = digits)
    outtmp["95%CI_lower"] <- exp(outtmp["95%CI_lower"]) |>
      round(digits) |>
      format(nsmall = digits)
    outtmp["95%CI_upper"] <- exp(outtmp["95%CI_upper"]) |>
      round(digits) |>
      format(nsmall = digits)
  }
  outtmp
}

report_fvcov <- function(result, digits = 3) {
  report(
    fit = result$fit,
    trt_var_name = result$trt_var_name,
    theta = result$theta,
    f_vcov = result$f_vcov,
    digits = digits
  )
}

# example: report
report(
  fit = fit,
  trt_var_name = "trt",
  theta = theta_t,
  f_vcov = n_fVf,
  digits = 3
)
