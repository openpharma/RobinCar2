omegaz <- function(pi_t, car_scheme) {
  if (car_scheme %in% c("simple", "pocock-simon")) {
    diag(pi_t) - pi_t %*% t(pi_t)
  } else {
    0
  }
}
