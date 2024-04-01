#' Counterfactual Prediction
#' @description Obtain counterfactual prediction of a fit.
#'
#' @param fit fitted object.
#' @param treatment (`string` or `formula`) treatment variable in string, or a formula of form
#' treatment ~ strata(s).
#' @param data (`data.frame`) raw dataset.
#' @param unbiased (`flag`) indicator of whether to remove potential biasness of the prediction.
#'
#' @export
predict_counterfactual <- function(fit, treatment, data, unbiased) {
  UseMethod("predict_counterfactual")
}

#' @export
predict_counterfactual.glm <- function(fit, treatment, data = fit$data, unbiased = TRUE) {
  treatment <- h_get_vars(treatment)
  assert_data_frame(data)
  assert_subset(unlist(treatment), colnames(data))
  assert_subset(treatment$treatment, all.vars(fit$formula[[3]]))
  assert(
    test_character(data[[treatment$treatment]]),
    test_factor(data[[treatment$treatment]])
  )
  assert_flag(unbiased)
  if (is.factor(data[[treatment$treatment]])) {
    trt_lvls <- levels(data[[treatment$treatment]])
  } else {
    trt_lvls <- sort(unique(data[[treatment$treatment]]))
  }
  n_lvls <- length(trt_lvls)
  df <- lapply(
    data,
    function(i) {
      rep(i, times = n_lvls)
    }
  )
  df[[treatment$treatment]] <- rep(trt_lvls, each = nrow(data))
  preds <- predict(fit, type = "response", newdata = df)
  ret <- matrix(preds, ncol = n_lvls, dimnames = list(row.names(data), trt_lvls))
  if (unbiased) {
    ret <- ret - bias(fit, treatment)
  }
  ret
}
