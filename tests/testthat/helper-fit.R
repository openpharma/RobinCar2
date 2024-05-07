# glm fit on iris
fit_glm <- glm(Sepal.Length ~ Sepal.Width + Species, data = iris)

# lm fit on iris
fit_lm <- lm(Sepal.Length ~ Sepal.Width + Species, data = iris)

# glm fit on ToothGrowth
grow <- ToothGrowth
grow$resp <- ifelse(grow$len < 15, 0, 1)
fit_binom <- glm(resp ~ supp + dose, data = grow, family = binomial())
