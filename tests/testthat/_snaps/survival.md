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

# robin_surv_no_strata_no_cov works as expected

    list(estimate = -0.530398178935407, se = 0.167180884723583, hr_n = 228L, 
        hr_sigma_l2 = 0.156924919527366, test_stat = -3.21352484896035, 
        p_value = 0.0013111645203555, test_score = -0.0895537761860842, 
        test_n = 228L, test_sigma_l2 = 0.17706769289317)

# robin_surv_strata works as expected

    list(estimate = -0.553588507820212, se = 0.170594923399458, hr_n = 227L, 
        hr_sigma_l2 = 0.15137074082497, test_stat = -3.2855836062253, 
        p_value = 0.00101771334472415, test_score = -0.0896871248297144, 
        test_n = 227L, test_sigma_l2 = 0.169145720705825)

# robin_surv_cov works as expected

    list(estimate = -0.501591989853193, se = 0.165617596975653, hr_n = 228L, 
        hr_sigma_l2 = 0.157111158164578, test_stat = -3.06610317830763, 
        p_value = 0.0021686846372504, test_score = -0.0851266162073163, 
        test_n = 228L, test_sigma_l2 = 0.175748674116587)

