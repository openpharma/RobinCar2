#' Counterfactual Prediction
#' @description Obtain counterfactual prediction of a fit.
#'
#' @param fit fitted object.
#' @param treatment (`formula`) formula of form treatment ~ strata(s).
#' @param data (`data.frame`) raw dataset.
#' @param vcov (`function` or `character`) variance function or name.
#' @param vcov_args (`list`) additional arguments for variance function.
#' @param ... Additional arguments for methods.
#'
#' @return List of class `prediction_cf` containing following elements:
#' - `estimate`: predicted marginal mean.
#' - `residual`: residual of the bias-corrected prediction.
#' - `predictions`: all predicted values.
#' - `predictions_liner`: linear predictions.
#' - `schema`: randomization schema.
#' - `response`: response value.
#' - `fit`: fitted model.
#' - `model_matrix`: model matrix.
#' - `treatment_formula`: treatment assignment and randomization formula.
#' - `treatment`: treatment value.
#' - `group_idx`: group index based on the stratification.
#' - `variance`: estimated variance of the marginal mean.
#' - `variance_name`: name of the variance.
#'
#' @export
predict_counterfactual <- function(fit, treatment, data, vcov, vcov_args, ...) {
  UseMethod("predict_counterfactual")
}

#' @export
predict_counterfactual.lm <- function(fit, treatment, data = find_data(fit), vcov = "vcovG", vcov_args = list(), ...) {
  # Offsets are rejected rather than supported: counterfactual prediction requires choosing a
  # value for the offset, and each choice targets a different quantity. Where the offset encodes
  # exposure time the implied estimand is a rate, for which no covariate-adjusted method is
  # established in the theory underlying RobinCar2 (Bannick et al., 2024) -- it develops marginal
  # means of the response and does not cover exposure time. `fit$offset` is set for both supply
  # routes, `offset()` in the formula and the `offset` argument.
  # A gaussian identity-link offset is the one case with an exact restatement: the offset is a
  # known additive shift, so fitting the shifted response gives bit-identical coefficients and
  # standard errors, identical treatment contrasts, and marginal means shifted by `mean(z)`.
  # That does not extend to an identity link with a non-gaussian family, where the shifted values
  # need not lie in the response support. See design/offset_rate/offset_estimand_note.md.
  if (!is.null(fit$offset)) {
    fit_family <- family(fit)
    if (identical(fit_family$family, "gaussian") && identical(fit_family$link, "identity")) {
      stop(
        "Models with an offset are not supported. ",
        "Here the offset is a known additive shift, so subtract it from the response instead: ",
        "use `I(y - z) ~ ...` in place of `y ~ ... + offset(z)`. Treatment contrasts are ",
        "identical; marginal means are shifted by `mean(z)`."
      )
    }
    stop(
      "Models with an offset are not supported. ",
      "Counterfactual prediction requires choosing a value for the offset, and each choice ",
      "targets a different quantity; where the offset encodes exposure time the implied estimand ",
      "is a rate, for which no covariate-adjusted method is established in the theory this ",
      "package implements. Giving the offset variable a free coefficient is not a substitute, ",
      "as it fits a different model."
    )
  }
  trt_vars <- h_get_vars(treatment)
  assert(
    test_string(vcov),
    test_function(vcov),
    test_null(vcov)
  )
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

  df[[trt_vars$treatment]] <- gl(n = n_lvls, k = nrow(data), labels = trt_lvls)

  mm <- model.matrix(fit, data = df)
  pred_linear <- mm %*% coefficients(fit)
  preds <- family(fit)$linkinv(pred_linear)

  ret <- matrix(preds, ncol = n_lvls, dimnames = list(row.names(data), trt_lvls))
  y <- model.response(fit$model)
  residual_raw <- y - fitted(fit)

  strata <- data[trt_vars$strata]
  if (ncol(strata) == 0L) {
    strata <- integer(nrow(strata))
  }
  group_idx <- split(seq_len(nrow(data)), strata)

  if (identical(trt_vars$schema, "ps")) {
    ret <- ret + bias(residual_raw, data[[trt_vars$treatment]], group_idx)
  } else {
    ret <- ret + bias(residual_raw, data[[trt_vars$treatment]], list(seq_len(nrow(ret))))
  }

  # Create residual based on the
  # prediction-unbiased response
  trt_idx <- match(data[[trt_vars$treatment]], trt_lvls)
  residual <- y - ret[cbind(seq_len(nrow(ret)), trt_idx)]
  ret <- structure(
    list(
      estimate = colMeans(ret),
      residual = residual,
      predictions = ret,
      schema = trt_vars$schema,
      predictions_linear = pred_linear,
      response = y,
      fit = fit,
      model_matrix = mm,
      treatment_formula = treatment,
      treatment = data[[trt_vars$treatment]],
      group_idx = group_idx
    ),
    class = "prediction_cf"
  )
  if (test_string(vcov)) {
    variance_name <- vcov
    vcov <- match.fun(vcov)
    mm_variance <- do.call(vcov, c(list(ret), vcov_args))
  } else if (test_function(vcov)) {
    variance_name_full <- deparse(substitute(vcov))
    variance_name <- paste(variance_name_full[1], if (length(variance_name_full) > 1) "...")
    mm_variance <- do.call(vcov, c(list(ret), vcov_args))
  } else {
    variance_name <- NULL
    mm_variance <- matrix(NA_real_, nrow = n_lvls, ncol = n_lvls)
  }
  ret$variance <- mm_variance
  ret$variance_name <- variance_name
  ret
}

#' @export
predict_counterfactual.glm <- function(fit, treatment, data = find_data(fit), vcov = "vcovG", vcov_args = list(), ...) {
  # use eval and bquote to allow vcov names to be further passed into inner functions.
  eval(
    bquote(
      predict_counterfactual.lm(
        fit = fit, data = data, treatment = treatment,
        vcov = .(substitute(vcov)), vcov_args = vcov_args
      )
    )
  )
}
