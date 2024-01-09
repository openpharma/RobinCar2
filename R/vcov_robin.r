#' Obtain treatment effect
#' @export
vcov_robin <- function(x, ...) {
  UseMethod("vcov_robin")
}

#' @export
vcov_robin.glm <- function(x, trt, strata_formula = NULL, ...) {
  # errors if using negtive bonomial, however this is not found in glm families.
  # check if trt is a valid entry in data and of class factor
  # data can be dataframe or environment
  raw_data <- get_data(x, all.vars(x$terms))
  preds <- lapply(x$xlevels[[trt]], function(lvl) {
    df <- raw_data
    df[trt] <- lvl
    predict(x, type = "response", newdata = df)
  })
  pred_true <- predict(x, type = "response")
  resi <- residuals(x, type = "response")
  preds <- do.call(cbind, preds)
  est <- colMeans(preds)
  var_preds <- var(preds)
  pit <- as.numeric(table(raw_data[[trt]]) / nrow(raw_data))
  trt_lvls <- sprintf("%s%s", trt, x$xlevels[[trt]])

  idx <- split(seq_len(nrow(raw_data)), raw_data[[trt]])
  cov_ymu <- do.call(rbind, lapply(idx, function(is) {
    stats::cov(x$y[is], preds[is, ])
  }))
  vcov_sr <- vapply(split(x$y, raw_data[[trt]]), sd, FUN.VALUE = 0)^2 / pit
  diagmat <- vcov_sr + (diag(var_preds) - 2 * diag(cov_ymu)) / pit
  strata <- get_data(x, all.vars(strata_formula))
  v <- diag(diagmat) + cov_ymu + t(cov_ymu) - var_preds - get_erb(resi, strata, length(x$xlevels[[trt]]), pit)
  ret <- v / length(fit$residuals)
  dimnames(ret) <- list(trt_lvls, trt_lvls)
  attr(ret, "estimates") <- est
  return(ret)
}

#' @export
vcov_robin.lm <- function(x, trt, strata_formula = NULL, ...) {
  # errors if using negtive bonomial, however this is not found in glm families.
  # check if trt is a valid entry in data and of class factor
  raw_data <- get_data(x, all.vars(x$terms))
  preds <- lapply(x$xlevels[[trt]], function(lvl) {
    df <- x$model
    df[trt] <- lvl
    predict(x, type = "response", newdata = df)
  })
  pred_true <- predict(x, type = "response")
  resi <- residuals(x, type = "response")
  preds <- do.call(cbind, preds)
  est <- colMeans(preds)
  var_preds <- var(preds)
  pit <- table(raw_data[[trt]]) / nrow(raw_data)
  trt_lvls <- sprintf("%s%s", trt, x$xlevels[[trt]])
  idx <- split(seq_len(nrow(raw_data)), raw_data[[trt]])
  y <- fitted(x) + residuals(x)
  cov_ymu <- do.call(rbind, lapply(idx, function(is) {
    stats::cov(y[is], preds[is, ])
  }))
  vcov_sr <- vapply(split(y, raw_data[[trt]]), sd, FUN.VALUE = 0)^2 / pit
  diagmat <- vcov_sr + (diag(var_preds) - 2 * diag(cov_ymu)) / pit
  strata <- get_data(x, all.vars(strata_formula))
  v <- diag(diagmat) + cov_ymu + t(cov_ymu) - var_preds - get_erb(resi, strata, length(x$xlevels[[trt]]), pit)
  ret <- v / length(fit$residuals)
  dimnames(ret) <- list(trt_lvls, trt_lvls)
  attr(ret, "estimates") <- est
  return(ret)
}

#' Huber-White Robust Variance
#' @export
hc <- function(x, trt, vcov_method = "HC0", ...) {
  preds <- lapply(x$xlevels[[trt]], function(lvl) {
    df <- x$model
    df[trt] <- lvl
    predict(x, type = "response", newdata = df)
  })
  preds <- do.call(cbind, preds)
  est <- colMeans(preds)

  vcov <- sandwich::vcovHC(x, type = vcov_method)
  mm <- model.matrix(x)
  asg <- attr(mm, "assign")
  tms <- terms(x)
  fct <- attr(tms, "factors")[-1, , drop = FALSE]
  idx <- which(trt == colnames(fct))
  trt_lvls <- sprintf("%s%s", trt, x$xlevels[[trt]])
  if (0L %in% asg) {
    m <- diag(rep(0, length(x$xlevels[[trt]])))
    id <- which(! trt_lvls %in% names(coef(x)))
    m[-id, -id] <- diag(rep(1, length(x$xlevels[[trt]]) - 1))
    m[, id] <- 1
  } else {
    m <- diag(rep(1, length(x$xlevels[[trt]])))
  }
  ret <- m %*% vcov[asg %in% c(0L, idx), asg %in% c(0L, idx)] %*% t(m)
  dimnames(ret) <- list(trt_lvls, trt_lvls)
  attr(ret, "estimates") <- est
  return(ret)
}
