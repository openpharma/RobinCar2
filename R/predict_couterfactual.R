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
predict_counterfactual.lm <- function(fit, treatment, data = find_data(fit), unbiased = TRUE) {
  trt_vars <- h_get_vars(treatment)
  assert_data_frame(data)
  assert_subset(unlist(trt_vars), colnames(data))
  formula <- formula(fit)
  assert_subset(trt_vars$treatment, all.vars(formula[[3]]))
  assert(
    test_character(data[[trt_vars$treatment]]),
    test_factor(data[[trt_vars$treatment]])
  )
  data[[trt_vars$treatment]] <- as.factor(data[[trt_vars$treatment]])
  assert_flag(unbiased)

  trt_lvls <- levels(data[[trt_vars$treatment]])
  n_lvls <- length(trt_lvls)

  df <- lapply(
    data,
    function(i) {
      rep(i, times = n_lvls)
    }
  )

  df[[trt_vars$treatment]] <- rep(trt_lvls, each = nrow(data))

  mm <- model.matrix(fit, data = df)
  pred_linear <- mm %*% coefficients(fit)
  preds <- family(fit)$linkinv(pred_linear)

  ret <- matrix(preds, ncol = n_lvls, dimnames = list(row.names(data), trt_lvls))
  y <- model.response(fit$model)
  residual <- y - fitted(fit)

  strata <- data[trt_vars$strata]
  if (ncol(strata) == 0L) {
    strata <- integer(nrow(strata))
  }
  group_idx <- split(seq_len(nrow(data)), strata)

  if (unbiased) {
    ret <- ret - bias(residual, data[[trt_vars$treatment]], group_idx)
  }
  structure(
    .Data = colMeans(ret),
    residual = residual,
    predictions = ret,
    predictions_linear = pred_linear,
    response = y,
    fit = fit,
    model_matrix = mm,
    treatment_formula = treatment,
    treatment = data[[trt_vars$treatment]],
    group_idx = group_idx,
    class = "prediction_cf"
  )
}

#' @export
predict_counterfactual.glm <- function(fit, treatment, data = find_data(fit), unbiased = TRUE) {
  predict_counterfactual.lm(fit = fit, data = data, treatment = treatment, unbiased = unbiased)
}
