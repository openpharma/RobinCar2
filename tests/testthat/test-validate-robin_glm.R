library(RobinCar)
library(MASS)

# Compare RobinCar2 to RobinCar results
# -------------------------------------

# Make categorical outcome to test counts
# Not informative at all
dummy_data$y_c <- MASS::rnegbin(n=nrow(dummy_data), theta=1)

test_that("marginal means", {
  # Function to compare RobinCar to RobinCar2 outputs
  compare_means <- function(r1, r2) {

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
    testthat::expect_equal(variances1, variances2)
  }

  families <- list(
    gaussian(),
    binomial(),
    poisson(),
    negative.binomial(theta = 1),
    "nb" # negative binomial with unspecified dispersion parameter
  )

  for (family in families) {
    for (model in c(
      # ANOVA
      "~ treatment",
      # ANCOVA
      "~ treatment + covar",
      "~ treatment + covar + s1",
      "~ treatment + covar + s1 + s2",
      # ANHECOVA
      "~ treatment * (covar)",
      "~ treatment * (covar + s1)",
      "~ treatment * (covar + s1 + s2)"
    )) {
      for (scheme in c("simple", "pocock-simon", "permuted-block")){
        for(strata in list(
          c(),
          c("s1"),
          c("s1", "s2")
          )){

          # Set formula based on parameters
          if(identical(family, "nb")){
            yname <- "y_c"
          } else if (identical(family$family, "gaussian")) {
            yname <- "y"
          } else if (identical(family$family, "binomial")) {
            yname <- "y_b"
          } else {
            yname <- "y_c"
          }

          form <- as.formula(paste0(yname, model))

          if(identical(family, "nb")){
            family2 <- MASS::negative.binomial(theta=NA)
          } else {
            family2 <- family
          }

          if(scheme == "simple"){
            scheme2 <- "sp"
          } else if(scheme == "permuted-block") {
            scheme2 <- "pb"
          } else {
            scheme2 <- "ps"
          }

          if(identical(strata, c())){
            strata2 <- "1"
          } else {
            strata2 <- strata
          }

          scheme_form <- paste0("treatment ~ ", scheme2, "(", paste(strata2, collapse=" + "), ")")

          # Skip simple randomization with strata
          # this is fine for RobinCar2, but it returns an error (intentionally) with RobinCar
          if(!(identical(strata, c()) & (scheme != "simple"))){

            set.seed(365)
            # Use RobinCar
            # We don't need to see RobinCar warnings.
            suppressWarnings(robincar1 <- robincar_glm(
              df = dummy_data,
              treat_col = "treatment",
              response_col = yname,
              formula = form,
              car_scheme = scheme,
              car_strata_cols = strata,
              g_family = family,
            ))

            set.seed(365)
            # Use RobinCar2
            robincar2 <- robin_glm(
              form = form,
              data = dummy_data,
              treatment = as.formula(scheme_form),
              vcov = vcovG,
              family = family2
            )

            # Compare the results
            compare_means(robincar1, robincar2)
          }
        }
      }
    }
  }
})

test_that("contrast -- standard options", {

  compare_contrast <- function(r1, r2, indices) {

    # Estimates and variance from RobinCar
    enames <- r1$contrast$result$treat
    estimates1 <- r1$contrast$result$estimate
    names(estimates1) <- enames
    variances1 <- diag(r1$contrast$varcov)
    names(variances1) <- enames

    # Estimates and variance for the first two
    # contrast vector elements from RobinCar2
    estimates2 <- c(r2)
    variances2 <- attr(r2, "variance")

    testthat::expect_equal(estimates1[indices], estimates2[indices])
    testthat::expect_equal(variances1[indices], variances2[indices])
  }

  for(scheme in c("simple", "pocock-simon", "permuted-block")){

    model <- "~ treatment + covar + s1"
    family <- family2 <- gaussian()
    yname <- "y"
    form <- as.formula(paste0(yname, model))
    strata <- c("s1")

    if(scheme == "simple"){
      scheme2 <- "sp"
    } else if(scheme == "permuted-block") {
      scheme2 <- "pb"
    } else {
      scheme2 <- "ps"
    }

    if(identical(strata, c())){
      strata2 <- "1"
    } else {
      strata2 <- strata
    }

    scheme_form <- paste0("treatment ~ ", scheme2, "(", paste(strata2, collapse=" + "), ")")

    run.robin1 <- function(...){
      suppressWarnings(robincar_glm(
        df = dummy_data,
        treat_col = "treatment",
        response_col = yname,
        formula = form,
        car_scheme = scheme,
        car_strata_cols = strata,
        g_family = family,
        ...))
    }

    run.robin2 <- function(...){
      robin_glm(
        form = form,
        data = dummy_data,
        treatment = as.formula(scheme_form),
        vcov = vcovG,
        family = family2,
        ...
      )
    }

    # DIFFERENCE ---------------------------------------------

    r1_diff <- run.robin1(
      contrast_h="diff"
    )
    r2_diff <- run.robin2(
      contrast="difference"
    )
    compare_contrast(r1_diff, r2_diff, indices=1:2)

    # RATIO -------------------------------------------------

    r1_ratio <- run.robin1(
      contrast_h="ratio"
    )
    r2_ratio <- run.robin2(
      contrast="risk_ratio"
    )
    compare_contrast(r1_ratio, r2_ratio, indices=1:2)

    # ODDS RATIO --------------------------------------------

    # r1_odds <- run.robin1(
    #   contrast_h=function()
    # )
    # r2_odds <- run.robin2(
    #   contrast="odds_ratio"
    # )
    # compare_contrast(r1_odds, r2_odds)

    # CUSTOM FUNCTION ---------------------------------------

    # This is a custom function for a contrast just to make sure
    # the custom function works correctly, not for anything scientifically meaningful
    # r1_cust <- run.robin1(
    #   contrast_h=function()
    # )
    # r2_cust <- run.robin2(
    #   contrast=function()
    # )
    # compare_contrast(r1_cust, r2_cust)

}})


