# glm fit on iris
fit_glm <- glm(y ~ treatment * s1 + covar, data = glm_data)

# lm fit on iris
fit_lm <- lm(y ~ treatment * s1 + covar, data = glm_data)

# glm fit on ToothGrowth
fit_binom <- glm(y_b ~ treatment * s1 + covar, data = glm_data, family = binomial())

# robin glm result with log risk ratio contrast
robin_res1 <- robin_glm(y_b ~ treatment * s1, data = glm_data, treatment = treatment ~ s1, contrast = "log_risk_ratio")
# robin glm result with difference contrast
robin_res2 <- robin_glm(y_b ~ treatment * s1, data = glm_data, treatment = treatment ~ s1, contrast = "diff")
