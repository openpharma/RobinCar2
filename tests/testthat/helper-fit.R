# glm fit on iris
fit_glm <- glm(y ~ treatment * s1 + covar, data = glm_data)

# lm fit on iris
fit_lm <- lm(y ~ treatment * s1 + covar, data = glm_data)

# glm fit on ToothGrowth
fit_binom <- glm(y_b ~ treatment * s1 + covar, data = glm_data, family = binomial())
