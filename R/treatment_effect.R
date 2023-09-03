#' Obtain treatment effect
#' @export
treatment_effect <- function(x, ...) {
  UseMethod("treatment_effect")
}

#' @export
treatment_effect.std_lm <- function(x, trt, ref, vcov_method = "HC3", ...) {
  if (missing(ref)) {
    ref <- x$ref_level
  }
  if (missing(trt)) {
    trt <- x$trt_levels[1]
  }
  assert_subset(c(trt, ref), c(x$trt_levels, x$ref_level))
  coefs <- coef(x$fit)
  cov <- sandwich::vcovHC(x$fit, vcov_method)
  sel_vec <- h_obtain_sel(paste0(x$trt, c(trt, ref)), names(coefs))
  ret <- list(
    trt = trt,
    ref = ref,
    stat = "diff",
    est = sum(sel_vec * coefs),
    vcov_method = vcov_method,
    var = (sel_vec %*% cov %*% sel_vec)[1, 1]
  )
  return(ret)
}
#' @export
treatment_effect.std_glm <- function(x, trt, ref, vcov_method = "HC3", statistic = "rr", ...) {
  if (missing(ref)) {
    ref <- x$ref_level
  }
  if (missing(trt)) {
    trt <- x$trt_levels[1]
  }
  if (test_string(statistic)) {
    assert_subset(statistic, c("rr", "or", "logor", "logrr", "diff"))
    stat <- match.fun(statistic)
    grad <- match.fun(paste0("grad_", statistic))
  } else {
    stop("Unknown statistic")
  }
  assert_subset(c(trt, ref), c(x$trt_levels, x$ref_level))
  df <- x$data
  df[[x$trt]] <- factor(trt, levels = sort(unique(x$data[[trt]])))
  pred1 <- predict(x$fit, data = df, type = "response")
  mat1 <- model.matrix(x$fit$formula, data = df)
  df[[x$trt]] <- factor(ref, levels = sort(unique(x$data[[trt]])))
  pred2 <- predict(x$fit, data = df, type = "response")
  mat2 <- model.matrix(x$fit$formula, data = df)
  m1 <- mean(pred1)
  m2 <- mean(pred2)
  est <- stat(m1, m2)
  gr <- grad(m1, m2)
  cov <- sandwich::vcovHC(x$fit, type = vcov_method)
  n <- nrow(df)
  mm <- rbind(c(pred1 * (1 - pred1) / n, rep(0, n)), c(rep(0, n), pred2 * (1 - pred2) / n))
  d <- rbind(mat1, mat2)
  cov_m <- mm %*% d %*% cov %*% t(d) %*% t(mm)
  var <- t(gr) %*% cov_m %*% gr
  ret <- list(
    trt = trt,
    ref = ref,
    stat = statistic,
    est = est,
    vcov_method = vcov_method,
    var = var[1, 1]
  )
  return(ret)
}

rr <- function(x, y) {
  x / y
}
grad_rr <- function(x, y) {
  c(1 / y, -x / y^2)
}
or <- function(x, y) {
  x / (1 - x) / y * (1 - y)
}
grad_or <- function(x, y) {
  c((1 - y) / ((1 - x)^2 * y), -x / ((1 - x) * y^2))
}
logrr <- function(x, y) {
  log(rr(x, y))
}
grad_logrr <- function(x, y) {
  c(1 / x, -1 / y)
}
logor <- function(x, y) {
  log(or(x, y))
}
grad_logor <- function(x, y) {
  c(1 / (x * (1 - x)), -1 / (y * (1 - y)))
}
diff <- function(x, y) {
  x - y
}
grad_diff <- function(x, y) {
  c(1, -1)
}
