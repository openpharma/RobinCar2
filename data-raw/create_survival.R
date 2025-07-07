# Create survival example data.

library(dplyr)

surv_dat <- survival::lung |>
  dplyr::mutate(
    status = ifelse(status == 2, 1, 0),
    sex = factor(ifelse(sex == 1, "Male", "Female")),
    strata = factor(ph.ecog)
  )

usethis::use_data(surv_dat, overwrite = TRUE)
