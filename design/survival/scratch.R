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


# Linear model by strata:

# Say ecog is our stratum variable.
stratum <- mod$model$ecog
unique_strata <- unique(stratum)
x <- mod.matrix[, c("sexMale", "wt.loss")]
y <- mod$model$meal.cal

xtxs <- list()
xtys <- list()

for (stratum_index in seq_along(unique_strata)) {
  # Save the corresponding cross products
  # for this group and stratum.
  in_stratum <- stratum == unique_strata[stratum_index]

  # Get the design matrix for this treatment arm.
  this_x <- x[in_stratum, , drop = FALSE]

  # Center it.
  this_x <- scale(this_x, center = TRUE, scale = FALSE)

  # Save it.
  x[in_stratum, ] <- this_x

  # Get the derived outcome values, the response.
  this_y <- y[in_stratum]

  # Save the cross products.
  xtxs[[stratum_index]] <- crossprod(this_x)
  xtys[[stratum_index]] <- crossprod(this_x, this_y)
}

# Sum across strata.
xtx <- Reduce("+", xtxs)
xty <- Reduce("+", xtys)

# Get the coefficients.
beta_est <- solve(xtx, xty)

# Get the residuals.
resids_by_stratum <- numeric(length(y))
for (stratum_index in seq_along(unique_strata)) {
  in_stratum <- stratum == unique_strata[stratum_index]
  this_x <- x[in_stratum, , drop = FALSE]
  this_y <- y[in_stratum]

  resids_by_stratum[in_stratum] <-
    this_y - as.numeric(this_x %*% beta_est) - mean(this_y)
}

resids_by_stratum_split <- split(resids_by_stratum, stratum)
sapply(resids_by_stratum_split, mean)
