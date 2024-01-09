get_erb <- function(resi, strata, n_trt, pit) {
  if (is.null(strata) || ncol(strata) == 0) {
    return(0)
  }
  # Calculate Omega Z under simple
  omegaz_sr <- omegaz(pit, "simple")
  omegaz_st <- omegaz(pit, "method")

  resi_per_strata <- vapply(split(resi, strata), mean, FUN.VALUE = 0)
  # Calculate strata levels and proportions for
  # the outer expectation
  strata_levels <- length(resi_per_strata)
  strata_props <- vapply(split(resi, strata), length, FUN.VALUE = 0L)
  strata_props <- strata_props / sum(strata_props)
  # Estimate R(B) by first getting the conditional expectation
  # vector for a particular strata (vector contains
  # all treatment groups), then dividing by the pi_t

  rb_z <- resi_per_strata / as.numeric(pit)
  # Compute the R(B)[Omega_{SR} - Omega_{Z_i}]R(B) | Z_i
  # for each Z_i
  ind <- split(seq_len(strata_levels), rep(seq_len(strata_levels / n_trt), each = n_trt))
  rb_z_sum <- lapply(
    ind,
    function(x) rb_z[x] %*% t(rb_z[x]) * sum(strata_props[x])
  )
  rb_z_sum <- Reduce(`+`, rb_z_sum)
  rb_z_sum * (omegaz_sr - omegaz_st)
}