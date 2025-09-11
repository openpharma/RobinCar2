library(dplyr)
library(survival)
library(readxl)
library(httr)
library(RobinCar)
library(RobinCar2)


# Download and prep dataset
#======================================

# Define the temporary file path for the downloaded Excel file
dpath <- "C:/Users/Daniel/Downloads/41591_2018_134_MOESM3_ESM.xlsx"

# Read in the OAK dataset
dat <- read_excel(dpath, sheet = 3) |>
  select(PtID, ECOGGR, OS, OS.CNSR, TRT01P, blSLD, ECOGGR, TC1IC1, HIST, PRIORTXC) |>
  # Create event and arm variables and select required columns
  mutate(
    event = -1 * (OS.CNSR - 1),
    time = OS,
    arm = factor(ifelse(TRT01P == "Docetaxel", "control", "experimental")),
    blSLD = as.numeric(blSLD),
    PRIORTXC = factor(PRIORTXC),
    stratum = 1
  ) |>
  select(time, event, arm, ECOGGR, blSLD, TC1IC1, HIST, PRIORTXC) |>
  as.data.frame()

## impute the 1 missing baseline sum of longest diameters using the mean
dat$blSLD[is.na(dat$blSLD)] <- mean(dat$blSLD, na.rm = TRUE)

#======================================

# Adjusted (Warnings)
#======================================

## Likely to do with change in variance estimation
## as noted in https://github.com/openpharma/RobinCar2/issues/75

# No warnings from RobinCar
RobinCar::robincar_covhr(
  dat,
  treat_col = "arm",
  response_col = "time",
  event_col = "event",
  covariate_cols = c("ECOGGR", "blSLD")
)


## Results appear consistent however 2 warnings thrown:
## Warning messages:
##  1: In sqrt(var_theta_cl) : NaNs produced
##  2: In sqrt(var_theta_cl) : NaNs produced
RobinCar2::robin_surv(
  Surv(time, event) ~ ECOGGR + blSLD,
  treatment = arm ~ 1,
  data = dat
)

## Warnings go away when setting `hr_se_plugin_adjusted = F`
RobinCar2::robin_surv(
  Surv(time, event) ~ ECOGGR + blSLD,
  treatment = arm ~ 1,
  data = dat,
  hr_se_plugin_adjusted = F
)

#======================================

# Adjusted (Error)
#======================================

## Adjusting for certain variables results in Error
## Variables impacted: `HIST`, `TC1IC1`

## Note: RobinCar returns with no error
RobinCar::robincar_covhr(dat, treat_col = "arm", response_col = "time", event_col = "event", covariate_cols = c("HIST"))


## Error in stats::lm.fit(x, y, singular.ok = FALSE) :
##    singular fit encountered
RobinCar2::robin_surv(
  Surv(time, event) ~ TC1IC1, # HIST,
  treatment = arm ~ 1,
  data = dat
)

set.seed(123)
dat <- data.frame(
  blSLD = rnorm(100),
  HIST = sample(c("A", "B", "C"), 100, replace = TRUE)
)
model <- blSLD ~ HIST
mf <- stats::model.frame(model, data = dat)
x <- stats::model.matrix(model, data = mf)
x <- x[, -1, drop = FALSE] # remove intercept
x <- scale(x, center = TRUE, scale = FALSE)
y <- stats::model.response(mf)
lm_fit <- stats::lm.fit(x, y, singular.ok = FALSE)

#======================================

# Unadjusted (OK)
#======================================

RobinCar::robincar_covhr(dat, treat_col = "arm", response_col = "time", event_col = "event", covariate_cols = NULL)


RobinCar2::robin_surv(
  Surv(time, event) ~ 1,
  treatment = arm ~ 1,
  data = dat
)
#======================================

# Stratified without adjustment (OK)
#======================================

RobinCar::robincar_covhr(
  dat,
  treat_col = "arm",
  response_col = "time",
  event_col = "event",
  car_strata_cols = "ECOGGR",
  car_scheme = "permuted-block",
  covariate_cols = NULL,
  adj_method = "CSL"
)


RobinCar2::robin_surv(
  Surv(time, event) ~ 1,
  treatment = arm ~ ECOGGR,
  data = dat |> mutate(ECOGGR = factor(ECOGGR))
)

#======================================
