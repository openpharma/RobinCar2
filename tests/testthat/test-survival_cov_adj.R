test_that("h_derived_outcome_vals works as expected", {
  surv_data_full <- na.omit(surv_data)
  result <- h_derived_outcome_vals(
    theta = 0,
    df = surv_data_full,
    treatment = "sex",
    time = "time",
    status = "status",
    covariates = c("age", "ph.karno"),
    n = 400
  )
  expect_data_frame(result, ncol = 4 + 2 + 1, nrow = nrow(surv_data_full))
  head_result <- result[seq_len(6), ]
  head_expected <- data.frame(
    index = 1:6,
    treatment = c(1, 1, 1, 0, 0, 1),
    time = c(455, 210, 1022, 310, 361, 218),
    status = c(1, 1, 0, 1, 1, 1),
    age = c(68, 57, 74, 68, 71, 53),
    ph.karno = c(90, 90, 50, 70, 60, 70),
    O_hat = c(
      -0.059407303167448,
      0.280849881485069,
      -1.1574498129749,
      0.128415337453864,
      0.0645341495986705,
      0.27539351150966
    )
  )
  expect_equal(head_result, head_expected, tolerance = 1e-4, ignore_attr = TRUE)
})
