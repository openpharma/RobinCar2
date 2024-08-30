library(RobinCar)

# Compare RobinCar2 to RobinCar results
# -------------------------------------

test_that("simple randomization, difference contrast", {

  # Function to compare RobinCar to RobinCar2 outputs
  compare <- function(r1, r2) {

    # Estimates and variance from RobinCar
    estimates1 <- unname(r1$contrast$result$estimate)
    variances1 <- unname(r1$contrast$result$se**2)

    # Estimates and variance for the first two
    # contrast vector elements from RobinCar2
    estimates2 <- attributes(r2)$effects[-3]
    variances2 <- attributes(r2)$variance[-3]

    testthat::expect_equal(estimates1, estimates2)
  }

  for(family in list (gaussian, binomial)){
    for(model in c ("~ treatment",
                   "~ treatment * covar",
                   "~ treatment * (covar + s1)")){

      # Set formula based on parameters
      yname <- ifelse(identical(family, binomial), "y_b", "y")
      form <- as.formula(paste0(yname, model))

      # Use RobinCar2
      robincar2 <- robin_glm(
        form,
        data = dummy_data,
        treatment = "treatment",
        vcov = vcovANHECOVA,
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
        contrast_h = "diff"
      )

      # Compare the results
      compare(robincar1, robincar2)
    }

  }

})
