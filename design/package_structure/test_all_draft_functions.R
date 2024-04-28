n <- 200
block_rand <- function(block = c(0, 0, 1, 1)) {
  function(n) {
    r <- lapply(seq_len(ceiling(n / length(block))), function(i) {
      sample(block)
    })
    unlist(r)[1:n]
  }
}
by_strata <- function(f, strata) {
  ret <- rep(NA, length(strata))
  for (x in split(seq_len(length(strata)), strata)) {
    ret[x] <- f(length(x))
  }
  return(ret)
}
sim_data <- function(n) {
  cov1 <- rnorm(n)
  z1 <- sample(size = n, factor(c("a", "b")), c(0.5, 0.5), replace = TRUE)
  z2 <- sample(size = n, factor(c("x", "y", "z")), c(0.4, 0.3, 0.3), replace = TRUE)
  permute_block <- c(0, 0, 1, 1)
  trt <- by_strata(block_rand(c("trt1", "trt1", "trt2", "trt2", "pbo", "pbo")), interaction(z1, z2))
  trt <- factor(trt, levels = c("pbo", "trt1", "trt2"))
  df <- data.frame(trt, z1, z2, cov1)
  x_mat <- model.matrix(~ trt + z1 + z2 + cov1, data = df)
  coef <- c(0.2, 0.5, 0.7, 1.5, -1, -0.5, 0.2)
  theta <- x_mat %*% coef
  y <- plogis(theta)
  y_bin <- as.integer(runif(n) < y)

  df$y <- y_bin
  df
}

d <- sim_data(500)
fit <- glm(y ~ trt:z1 + trt, family = binomial(link="logit"), data = d)

## start of the pkg
source(file.path(getwd(),"design","package_structure","all_draft_functions.R"))
pred_counterfact <- get_countfact_pred(fit,"trt")
fit.fvcov <- calculate_f_vcov(fit, "trt", pred_counterfact)
report_fvcov(fit.fvcov,3)
