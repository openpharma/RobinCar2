#' Counterfactual Prediction
#' @description Obtain counterfactual prediction of a fit.
#'
#' @param fit fitted object.
#' @param treatment (`formula`) formula of form treatment ~ strata(s).
#' @param data (`data.frame`) raw dataset.
#'
#' @return Numeric matrix of counter factual prediction.
#'
#' @export
predict_counterfactual <- function(fit, treatment, data) {
  UseMethod("predict_counterfactual")
}

#' @export
predict_counterfactual.lm <- function(fit, treatment, data = find_data(fit)) {
  trt_vars <- h_get_vars(treatment)
  assert_data_frame(data)
  assert_subset(c(trt_vars$treatment, trt_vars$strata), colnames(data))
  formula <- formula(fit)
  assert_subset(trt_vars$treatment, all.vars(formula[[3]]))
  assert(
    test_character(data[[trt_vars$treatment]]),
    test_factor(data[[trt_vars$treatment]])
  )
  data[[trt_vars$treatment]] <- as.factor(data[[trt_vars$treatment]])

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

  if (identical(trt_vars$schema, "ps")) {
    ret <- ret + bias(residual, data[[trt_vars$treatment]], group_idx)
  } else {
    ret <- ret + bias(residual, data[[trt_vars$treatment]], list(seq_len(nrow(ret))))
  }
  structure(
    .Data = colMeans(ret),
    residual = residual,
    predictions = ret,
    schema = trt_vars$schema,
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
predict_counterfactual.glm <- function(fit, treatment, data = find_data(fit)) {
  predict_counterfactual.lm(fit = fit, data = data, treatment = treatment)
}
