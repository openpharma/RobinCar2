#### Main functions ####

#' @exportS3Method
vcovHC.prediction_cf <- function(x, type = "HC3", ...) {
  fit <- attr(x, "fit")
  vc <- vcovHC(fit, type = type)
  mm <- attr(x, "model_matrix")
  n <- nrow(mm) / length(names(x))
  md <- family(fit)$mu.eta(attr(x, "predictions_linear")) / n
  z <- block_sum(as.vector(md) * mm, n)
  ret <- z %*% vc %*% t(z)
  dimnames(ret) <- list(names(x), names(x))
  ret
}

#' Covariance matrix of Counterfactual prediction
#'
#' @param prediction_cf R object returned by \code{\line{predict_counterfactual}}
#' @param eff_measure default is NULL, the covariance matrix of the mean counterfactual
#' predictions of the response. If not NULL, a valid option is "diff", "odds ratio", or "risk ratio",
#' in which case covariance matrix of the estimated effect measure will also be returned.
#'
#' @return an R object of class 'vcov_robincar', essentially a list consisting
#' \itemize{
#'   \item theta: mean counterfactual prediction of the reponse, as the calculation result of
#'         \code{\line{predict_counterfactual}}
#'   \item V: covariance matrix of the mean counterfactual predictions of the response
#'   \item f_vcov: covariance matrix of the estimated effect measures. It is NULL
#'         if \code{eff_measure} is set to NULL (default)
#'   \item n: sample size in \code{fit} of \code{\line{predict_counterfactual}}
#'   \item eff_measure: \code{eff_measure} in the argument of this function
#'   \item fit: \code{fit} of \code{\line{predict_counterfactual}}
#'   \item trt_var_name: name of treatment variable in the data used in \code{fit}
#'         of \code{\line{predict_counterfactual}}
#' }
#' @export

vcovRobinCar.prediction_cf <- function(x, eff_measure = NULL, ...) {
  target_model <- attr(x, "fit")
  data_idx <- ifelse(identical(class(target_model), "lm"), "model", "data")
  tm_use_data <- target_model[[data_idx]]
  tm_use_trts <- attr(x, "treatment")
  trt_idx_in_data <- sapply(tm_use_data, function(ik) identical(ik, tm_use_trts))
  tm_use_trtvar <- names(trt_idx_in_data)[trt_idx_in_data]

  interim_data <- prepare_interim_vcovrc(target_model, tm_use_trtvar)

  fit.fvcov <- calculate_f_vcov(
    fit = target_model,
    trt_var_name = tm_use_trtvar,
    interim_data = interim_data,
    eff_measure = eff_measure
  )

  return(fit.fvcov)
}


#### Helper functions ####

#' helper function: wrap calculations from fit to interested covariance estimate
#' @noRd
calculate_f_vcov <- function(fit, trt_var_name, interim_data, eff_measure) {
  treatments <- fit$xlevels[[trt_var_name]]

  theta_t <- sapply(interim_data, function(dat_t) unique(dat_t$theta_t))

  V <- outer(
    X = treatments,
    Y = treatments,
    FUN = "vectorized_calculate_V",
    counterfact_res = interim_data
  )
  rownames(V) <- colnames(V) <- treatments

  out <- list(
    theta = theta_t,
    V = V,
    f_vcov = NULL,
    n = nrow(fit$model),
    eff_measure = NULL,
    trt_var_name = trt_var_name,
    fit = fit
  )

  if (!is.null(eff_measure)) {
    n_fVf <- outer(
      X = treatments,
      Y = treatments,
      FUN = "vectorized_calculate_fVf",
      theta = theta_t,
      V = V,
      n = nrow(fit$model),
      eff_measure = eff_measure
    )
    rownames(n_fVf) <- colnames(n_fVf) <- treatments

    out$eff_measure <- eff_measure
    out$f_vcov <- n_fVf
  }

  class(out) <- c(class(out), "vcov_robincar")
  out
}


#' helper function: prepare interim_data for calculate_f_vcov
#' @noRd
prepare_interim_vcovrc <- function(fit, trt_var_name) {
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

#' helper function: calculate V (covariance of the mean counterfactual prediction of response)
#' @noRd
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

#' helper function: calculate n^(-1)f'Vf (covariance of estimated effect measure)
#' @noRd
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
