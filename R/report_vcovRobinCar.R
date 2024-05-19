#' Reporting function for vcov_robincar
#'
#' Pretty reporting for the object returned by \code{\link{vcovRobinCar.prediction_cf}}.
#' Note, if eff_measure was not specified (i.e. NULL), no reporting table will be generated.
#'
#' @param x object returned by \code{\link{vcovRobinCar.prediction_cf}}
#' @param digits integer, number of decimal places of estimates in the reported table
#'
#' @return a data frame
#' @export

report_vcov_robincar <- function(x, digits = 3) {
  eff_measure <- x$eff_measure

  if(is.null(eff_measure)){
    cli::cli_alert_info("When eff_measure is NULL, nothing to report")
  }else{
    report(
      fit = x$fit,
      trt_var_name = x$trt_var_name,
      theta = x$theta,
      f_vcov = x$f_vcov,
      eff_measure = eff_measure,
      digits = digits
    )
  }

}

report <- function(fit, trt_var_name, theta, f_vcov, eff_measure, digits = 3) {
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

calculate_f <- function(theta_t, theta_s, eff_measure) {
  switch(eff_measure,
         "diff" = theta_t - theta_s,
         "risk ratio" = log(theta_t / theta_s),
         "odds ratio" = log(odds(theta_t) / odds(theta_s))
  )
}

