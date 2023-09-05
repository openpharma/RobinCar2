set.seed(12345)
y <- rbinom(100, 1, 0.5)
x <- rbinom(100, 1, 0.4)
df <- data.frame(x = x, y = y)

fit <- std_glm(y ~ x, df, "x")

treatment_effect(fit, "1", "0", statistic = "rr", vcov_method = "HC0")
