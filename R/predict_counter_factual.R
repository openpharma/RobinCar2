#' Predict 2
#' @export
predict_counter_factual2 <- function(x, trt, ...) {
  assert_string(trt)
  preds <- lapply(x$xlevels[[trt]], function(lvl) {
    df <- x$model
    df[trt] <- lvl
    predict(x, type = "response", newdata = df)
  })
  do.call(cbind, preds)
}


#' Predict 2
#' @export
predict_counter_factual3 <- function(x, trt, ...) {
  assert_string(trt)
  lvls <- x$xlevels[[trt]]
  df <- rbind(x$model, x$model, x$model)
  df[[trt]] <- factor(rep(lvls, each = nrow(x$model)), levels = lvls, labels = lvls)

  preds <- predict(x, type = "response", newdata = df)
  matrix(preds, ncol = length(lvls))
}

#' Remove biasness.
#' @param lvls treatment & strata interations.
unbias_prediction <- function(pred, y, trt, strat) {
  d <- matrix(NA_real_, nrow = nrow(pred), ncol = ncol(pred))
  id_strat <- split(seq_len(length(y)), strat)
  for (i in id_strat) {
    id_trt <- split(i, trt[i])
    df <- vapply(seq_len(length(id_trt)), function(xx) mean(y[id_trt[[xx]]] - pred[id_trt[[xx]], xx]), FUN.VALUE = 0)
    d[i, ] <- matrix(df, nrow = length(i), ncol = length(id_trt), byrow = TRUE)
  }
  pred + d
}

unbias_prediction3 <- function(pred, residuals, trt, strat) {
  d <- matrix(NA_real_, nrow = nrow(pred), ncol = ncol(pred))
  id_strat <- split(seq_len(length(residuals)), strat)
  for (i in id_strat) {
    id_trt <- split(i, trt[i])
    df <- vapply(id_trt, function(xx) mean(residuals[xx]), FUN.VALUE = 0)
    d[i, ] <- matrix(df, nrow = length(i), ncol = length(id_trt), byrow = TRUE)
  }
  pred + d
}

unbias_prediction4 <- function(pred, residuals, trt, strat) {
  d <- matrix(NA_real_, nrow = nrow(pred), ncol = ncol(pred))
  id_strat <- split(seq_len(length(residuals)), strat)
  for (i in id_strat) {
    df <- vapply(split(residuals[i], trt[i]), function(xx) mean(xx), FUN.VALUE = 0)
    d[i, ] <- matrix(df, nrow = length(i), ncol = length(df), byrow = TRUE)
  }
  pred + d
}


unbias_prediction2 <- function(pred, y, trt, strat) {
  id_strat <- split(seq_len(length(y)), strat)
  for (i in id_strat) {
    id_trt <- split(i, trt[i])
    df1 <- vapply(seq_len(length(id_trt)), function(xx) mean(y[id_trt[[xx]]] - pred[id_trt[[xx]], xx]), FUN.VALUE = 0)
    for (j in seq_len(length(df1))) {
      pred[i, j] <- pred[i, j] + df1[j]
    }
  }
  pred
}