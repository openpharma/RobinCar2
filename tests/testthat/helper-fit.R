# glm fit on iris
fit_glm <- glm(y ~ treatment * s1 + covar, data = dummy_data)

# lm fit on iris
fit_lm <- lm(y ~ treatment * s1 + covar, data = dummy_data)

# glm fit on ToothGrowth
fit_binom <- glm(y_b ~ treatment * s1 + covar, data = dummy_data, family = binomial())
