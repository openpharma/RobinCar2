head(surv_data)


# Linear model with intercept:
mod <- lm(meal.cal ~ 1 + sex + wt.loss + ecog, data = surv_data)

resids <- residuals(mod)

resids_by_ecog <- split(resids, mod$model$ecog)
sapply(resids_by_ecog, mean)


# Linear model without intercept, with centered covariates:
mod.matrix <- model.matrix(mod)

mod.matrix <- scale(mod.matrix[, -1], center = TRUE, scale = FALSE)

mod2 <- lm.fit(mod.matrix, mod$model$meal.cal)

# For the residuals, we need to adjust for the centering of the outcome:
resids2 <- residuals(mod2) - mean(mod$model$meal.cal)

resids2_by_ecog <- split(resids2, mod$model$ecog)

sapply(resids2_by_ecog, mean)
