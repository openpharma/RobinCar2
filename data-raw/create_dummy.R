#' Create permute-block randomization dummy data

library(dplyr)
#' Set seed to ensure reproducibility
set.seed(20240408)

n <- 400
block <- c(0L, 0L, 1L, 1L)

dummy_data <- tibble(
  s1 = sample(c("a", "b"), replace = TRUE, size = n),
  s2 = sample(c("c", "d"), replace = TRUE, size = n),
  covar = rnorm(n),
  id = seq_len(n)
) %>%
  group_by(s1, s2) %>%
  mutate(treatment = unlist(replicate(ceiling(n() / length(block)), sample(block)))[seq_len(n())]) %>%
  ungroup() %>%
  mutate(
    y = covar * 0.2 + 0.4 * (s1 == "a") - 0.1 * (s2 == "c") + 0.6 * treatment + rnorm(n),
    y_b = ifelse(y > 0.6, 1L, 0L)
  ) %>%
  mutate(
    s1 = factor(s1),
    s2 = factor(s2)
  ) %>%
  select(id, treatment, s1, s2, covar, y, y_b)

usethis::use_data(dummy_data)
