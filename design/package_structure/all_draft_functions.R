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
