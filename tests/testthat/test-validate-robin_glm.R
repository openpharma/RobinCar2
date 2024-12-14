library(RobinCar)
library(MASS)

# Compare RobinCar2 to RobinCar results
# -------------------------------------

# Make categorical outcome to test counts
dummy_data$y_c <- ifelse(
  dummy_data$y <= 0, 1,
  ifelse(dummy_data$y <= 1, 2,
    ifelse(dummy_data$y <= 2, 3, 4)
  )
)

test_that("marginal means", {
  # Function to compare RobinCar to RobinCar2 outputs
  compare <- function(r1, r2) {
    # Estimates and variance from RobinCar
    enames <- r1$result$treat
    estimates1 <- r1$result$estimate
    names(estimates1) <- enames
    variances1 <- r1$varcov
    colnames(variances1) <- enames
    rownames(variances1) <- enames

    # Estimates and variance for the first two
    # contrast vector elements from RobinCar2
    estimates2 <- c(attributes(r2)$marginal_mean)
    variances2 <- attributes(r2)$mmvariance

    testthat::expect_equal(estimates1, estimates2)
  }

  for (family in list(gaussian(), binomial(), poisson(), negative.binomial(theta = 1))) {
    for (model in c(
      "~ treatment",
      "~ treatment * covar",
      "~ treatment * (covar + s1)"
    )) {
      print(model)
      print(family$family)

      # Set formula based on parameters
      if (family$family == "gaussian") {
        yname <- "y"
      } else if (family$family == "binomial") {
        yname <- "y_b"
      } else if (family$family %in% c("poisson", "Negative Binomial(1)")) {
        yname <- "y_c"
      }
      form <- as.formula(paste0(yname, model))

      # Use RobinCar2
      robincar2 <- robin_glm(
        form = form,
        data = dummy_data,
        treatment = treatment ~ 1,
        vcov = vcovG,
        family = family
      )

      # Use RobinCar
      robincar1 <- robincar_glm(
        df = dummy_data,
        treat_col = "treatment",
        response_col = yname,
        formula = form,
        car_scheme = "simple",
        g_family = family,
      )

      # Compare the results
      compare(robincar1, robincar2)
    }
  }
})
