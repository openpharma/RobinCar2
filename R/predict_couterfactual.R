#' Counterfactual Prediction
#' @description Obtain counterfactual prediction of a fit.
#'
#' @param fit fitted object.
#' @param treatment (`string` or `formula`) treatment variable in string, or a formula of form
#' treatment ~ strata(s).
#' @param data (`data.frame`) raw dataset.
#' @param unbiased (`flag`) indicator of whether to remove potential bias of the prediction.
#'
#' @return Numeric matrix of counter factual prediction.
#'
#' @export
predict_counterfactual <- function(fit, treatment, data, unbiased) {
  UseMethod("predict_counterfactual")
}

#' @export
predict_counterfactual.lm <- function(fit, treatment, data, unbiased = TRUE) {
  treatment <- h_get_vars(treatment)
  assert_data_frame(data)
  assert_subset(unlist(treatment), colnames(data))
  formula <- formula(fit)
  assert_subset(treatment$treatment, all.vars(formula[[3]]))
  assert(
    test_character(data[[treatment$treatment]]),
    test_factor(data[[treatment$treatment]])
  )
  data[[treatment$treatment]] <- as.factor(data[[treatment$treatment]])
  assert_flag(unbiased)

  trt_lvls <- levels(data[[treatment$treatment]])
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
  y <- model.response(fit$model)
  residual <- y - fitted(fit)

  strata <- data[treatment$strata]
  if (ncol(strata) == 0L) {
    strata <- integer(nrow(strata))
  }
  group_idx <- split(seq_len(nrow(data)), strata)

  if (unbiased) {
    ret <- ret - bias(residual, data[[treatment$treatment]], group_idx)
  }

  structure(
    .Data = colMeans(ret),
    residual = residual,
    predictions = ret,
    response = y,
    fit = fit,
    group_idx = group_idx,
    class = "prediction_cf"
  )
}

#' @export
predict_counterfactual.glm <- function(fit, treatment, data = fit$data, unbiased = TRUE) {
  predict_counterfactual.lm(fit = fit, data = data, treatment = treatment, unbiased = unbiased)
}
