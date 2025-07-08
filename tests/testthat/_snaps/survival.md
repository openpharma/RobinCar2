# h_lr_score_no_strata_no_cov works as expected with default options

    structure(0.0895537761860842, sigma_l2 = 0.17706769289317, se_theta_l = 0.15738484553678, n = 228L)

# h_lr_score_no_strata_no_cov works as expected with custom n

    structure(0.051045652426068, sigma_l2 = 0.100928584949107, se_theta_l = 0.15738484553678, n = 400)

# h_lr_score_no_strata_no_cov works as expected when not using ties factor

    structure(0.00479534279339572, sigma_l2 = 0.158585973213507, se_theta_l = 0.166303040954794, n = 228L)

# h_log_hr_est_via_score works as expected

    list(theta = 0.530398178935407, se = 0.167180884723583, sigma_l2 = 0.156924919527366, 
        n = 228L)

# h_log_hr_est_via_score extends the search interval as needed

    list(theta = 0.530397994806822, se = 0.167180879290955, sigma_l2 = 0.156924929726076, 
        n = 228L)

# h_lr_test_via_score works as expected

    list(u_l = 0.0895537761860842, sigma_l2 = 0.17706769289317, tau_l = 3.21352484896035, 
        pval = 0.0013111645203555, n = 228L)

# robin_surv_comparison works as expected without covariate adjustment

    list(estimate = 0.530398178935407, se = 0.167180884723583, hr_n = 228L, 
        hr_sigma_l2 = 0.156924919527366, test_stat = 3.21352484896035, 
        p_value = 0.0013111645203555, test_score = 0.0895537761860842, 
        test_n = 228L, test_sigma_l2 = 0.17706769289317)

