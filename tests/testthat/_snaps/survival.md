# h_log_hr_est_via_score works as expected

    list(theta = 0.53342923961288, se = 0.167270504465807, sigma_l2 = 0.156756810948865, 
        n = 228L)

# h_log_hr_est_via_score does not give spurious warning

    list(theta = -0.0507718560099652, se = 0.182997810903141, sigma_l2 = 0.17218348832935, 
        n = 167L)

# h_log_hr_est_via_score extends the search interval as needed

    list(theta = 0.529999478789383, se = 0.167169124542081, sigma_l2 = 0.156946999327738, 
        n = 228L)

# h_lr_test_via_score works as expected

    list(u_l = 0.0895537761860842, sigma_l2 = 0.17706769289317, tau_l = 3.21352484896035, 
        pval = 0.0013111645203555, n = 228L, give_rand_strat_warning = FALSE)

# robin_surv_comparison works as expected without covariate adjustment

    list(estimate = 0.53342923961288, se = 0.167270504465807, hr_n = 228L, 
        hr_sigma_l2 = 0.156756810948865, test_stat = 3.21352484896035, 
        p_value = 0.0013111645203555, test_score = 0.0895537761860842, 
        test_n = 228L, test_sigma_l2 = 0.17706769289317, give_rand_strat_warning = FALSE)

# robin_surv_comparison can skip the hazard ratio estimation

    list(estimate = NA_real_, se = NA_real_, hr_n = NA_integer_, 
        hr_sigma_l2 = NA_real_, test_stat = 3.21352484896035, p_value = 0.0013111645203555, 
        test_score = 0.0895537761860842, test_n = 228L, test_sigma_l2 = 0.17706769289317, 
        give_rand_strat_warning = FALSE)

# robin_surv_no_strata_no_cov works as expected

    list(estimate = -0.53342923961288, se = 0.167270504465807, hr_n = 228L, 
        hr_sigma_l2 = 0.156756810948865, test_stat = -3.21352484896035, 
        p_value = 0.0013111645203555, test_score = -0.0895537761860842, 
        test_n = 228L, test_sigma_l2 = 0.17706769289317, give_rand_strat_warning = FALSE)

# robin_surv_strata works as expected

    list(estimate = -0.554820697243091, se = 0.170628735365298, hr_n = 227L, 
        hr_sigma_l2 = 0.151310755205329, test_stat = -3.2855836062253, 
        p_value = 0.00101771334472415, test_score = -0.0896871248297144, 
        test_n = 227L, test_sigma_l2 = 0.169145720705825, give_rand_strat_warning = FALSE)

# robin_surv_cov works as expected

    list(estimate = -0.503821677843015, se = 0.1656799127954, hr_n = 228L, 
        hr_sigma_l2 = 0.156990532256188, test_stat = -3.06610317830763, 
        p_value = 0.0021686846372504, test_score = -0.0851266162073163, 
        test_n = 228L, test_sigma_l2 = 0.175748674116587, give_rand_strat_warning = FALSE)

# robin_surv_strata_cov works as expected

    list(estimate = -0.0136495961352817, se = 0.158066245456898, 
        hr_n = 227L, hr_sigma_l2 = 0.172478508438683, test_stat = -0.0835488066000914, 
        p_value = 0.933415170960403, test_score = -0.00229971887336489, 
        test_n = 227L, test_sigma_l2 = 0.171986696328211, give_rand_strat_warning = FALSE)

# h_log_hr_coef_mat works as expected

    structure(c(0.5, 1, 0.5, 0.617075077451974), dim = c(1L, 4L), dimnames = list(
        "B v.s. A", c("Estimate", "Std.Err", "Z Value", "Pr(>|z|)"
        )))

# h_log_hr_coef_mat works as expected for multiple comparisons

    WAoAAAACAAQFAQACAwAAAAIOAAAADD/gAAAAAAAAP+ZmZmZmZmY/7MzMzMzMzT/wAAAAAAAA
    QAAAAAAAAABACAAAAAAAAD/gAAAAAAAAP9ZmZmZmZmY/0zMzMzMzMz/jvxQ7mqcTP+c+KqcH
    D68/6HQjpnfpDgAABAIAAAABAAQACQAAAANkaW0AAAANAAAAAgAAAAMAAAAEAAAEAgAAAAEA
    BAAJAAAACGRpbW5hbWVzAAAAEwAAAAIAAAAQAAAAAwAEAAkAAAAIQiB2LnMuIEEABAAJAAAA
    CEEgdi5zLiBDAAQACQAAAAhDIHYucy4gQgAAABAAAAAEAAQACQAAAAhFc3RpbWF0ZQAEAAkA
    AAAHU3RkLkVycgAEAAkAAAAHWiBWYWx1ZQAEAAkAAAAIUHIoPnx6fCkAAAD+

# h_test_mat works as expected

    structure(c(0.5, 0.7, 0.05, 0.01), dim = c(2L, 2L), dimnames = list(
        c("B v.s. A", "A v.s. B"), c("Test Stat.", "Pr(>|z|)")))

# robin_surv works as expected without strata or covariates

    structure(c(-0.0374048047550865, 0.156467896653491, -0.239057375698749, 
    0.811061092843519), dim = c(1L, 4L), dimnames = list("1 v.s. 0", 
        c("Estimate", "Std.Err", "Z Value", "Pr(>|z|)")))

---

    WAoAAAACAAQFAQACAwAAAAIOAAAAAr/Oo7/aMxoGP+nyNmcckPoAAAQCAAAAAQAEAAkAAAAD
    ZGltAAAADQAAAAIAAAABAAAAAgAABAIAAAABAAQACQAAAAhkaW1uYW1lcwAAABMAAAACAAAA
    EAAAAAEABAAJAAAACDEgdi5zLiAwAAAAEAAAAAIABAAJAAAAClRlc3QgU3RhdC4ABAAJAAAA
    CFByKD58enwpAAAA/g==

---

    WAoAAAACAAQFAQACAwAAAAMTAAAAAwAAAw0AAAACAAAAAQAAAAIAAAQCAAAAAQAEAAkAAAAG
    bGV2ZWxzAAAAEAAAAAIABAAJAAAAATAABAAJAAAAATEAAAQCAAAAAQAEAAkAAAAFY2xhc3MA
    AAAQAAAAAQAEAAkAAAAGZmFjdG9yAAAA/gAAAA0AAAACAAAAcgAAAHEAAAANAAAAAgAAAFIA
    AABSAAAEAgAAAAEABAAJAAAABW5hbWVzAAAAEAAAAAMABAAJAAAABGVjb2cABAAJAAAACFBh
    dGllbnRzAAQACQAAAAZFdmVudHMAAAQCAAAAAQAEAAkAAAAJcm93Lm5hbWVzAAAADQAAAAKA
    AAAA/////gAABAIAAAL/AAAAEAAAAAEABAAJAAAACmRhdGEuZnJhbWUAAAD+

# robin_surv works as expected with strata

    structure(c(0.554820697243091, 0.170628735365298, 3.25162520870402, 
    0.00114747222174709), dim = c(1L, 4L), dimnames = list("Male v.s. Female", 
        c("Estimate", "Std.Err", "Z Value", "Pr(>|z|)")))

---

    WAoAAAACAAQFAQACAwAAAAIOAAAAAkAKSOAOyBdTP1CsmWIRFpkAAAQCAAAAAQAEAAkAAAAD
    ZGltAAAADQAAAAIAAAABAAAAAgAABAIAAAABAAQACQAAAAhkaW1uYW1lcwAAABMAAAACAAAA
    EAAAAAEABAAJAAAAEE1hbGUgdi5zLiBGZW1hbGUAAAAQAAAAAgAEAAkAAAAKVGVzdCBTdGF0
    LgAEAAkAAAAIUHIoPnx6fCkAAAD+

---

    WAoAAAACAAQFAQACAwAAAAMTAAAABAAAAw0AAAAHAAAAAQAAAAEAAAACAAAAAgAAAAMAAAAD
    AAAABAAABAIAAAABAAQACQAAAAZsZXZlbHMAAAAQAAAABAAEAAkAAAABMAAEAAkAAAABMQAE
    AAkAAAABMgAEAAkAAAABMwAABAIAAAABAAQACQAAAAVjbGFzcwAAABAAAAABAAQACQAAAAZm
    YWN0b3IAAAD+AAADDQAAAAcAAAABAAAAAgAAAAEAAAACAAAAAQAAAAIAAAACAAAEAgAAAf8A
    AAAQAAAAAgAEAAkAAAAGRmVtYWxlAAQACQAAAARNYWxlAAAEAgAAAv8AAAAQAAAAAQAEAAkA
    AAAGZmFjdG9yAAAA/gAAAA0AAAAHAAAAGwAAACQAAAAqAAAARwAAABUAAAAdAAAAAQAAAA0A
    AAAHAAAACQAAABwAAAAcAAAANgAAABAAAAAcAAAAAQAABAIAAAABAAQACQAAAAVuYW1lcwAA
    ABAAAAAEAAQACQAAAAZzdHJhdGEABAAJAAAAA3NleAAEAAkAAAAIUGF0aWVudHMABAAJAAAA
    BkV2ZW50cwAABAIAAAABAAQACQAAAAlyb3cubmFtZXMAAAANAAAAAoAAAAD////5AAAEAgAA
    Av8AAAAQAAAAAQAEAAkAAAAKZGF0YS5mcmFtZQAAAP4=

# robin_surv works as expected with covariates

    structure(c(-0.00438916915405738, 0.155227542623415, -0.0282757111262503, 
    0.977442252566086), dim = c(1L, 4L), dimnames = list("1 v.s. 0", 
        c("Estimate", "Std.Err", "Z Value", "Pr(>|z|)")))

---

    WAoAAAACAAQFAQACAwAAAAIOAAAAAr+bioZy8DyXP+9QOWI2QekAAAQCAAAAAQAEAAkAAAAD
    ZGltAAAADQAAAAIAAAABAAAAAgAABAIAAAABAAQACQAAAAhkaW1uYW1lcwAAABMAAAACAAAA
    EAAAAAEABAAJAAAACDEgdi5zLiAwAAAAEAAAAAIABAAJAAAAClRlc3QgU3RhdC4ABAAJAAAA
    CFByKD58enwpAAAA/g==

---

    WAoAAAACAAQFAQACAwAAAAMTAAAAAwAAAw0AAAACAAAAAQAAAAIAAAQCAAAAAQAEAAkAAAAG
    bGV2ZWxzAAAAEAAAAAIABAAJAAAAATAABAAJAAAAATEAAAQCAAAAAQAEAAkAAAAFY2xhc3MA
    AAAQAAAAAQAEAAkAAAAGZmFjdG9yAAAA/gAAAA0AAAACAAAAcgAAAHEAAAANAAAAAgAAAFIA
    AABSAAAEAgAAAAEABAAJAAAABW5hbWVzAAAAEAAAAAMABAAJAAAABGVjb2cABAAJAAAACFBh
    dGllbnRzAAQACQAAAAZFdmVudHMAAAQCAAAAAQAEAAkAAAAJcm93Lm5hbWVzAAAADQAAAAKA
    AAAA/////gAABAIAAAL/AAAAEAAAAAEABAAJAAAACmRhdGEuZnJhbWUAAAD+

# robin_surv works as expected with strata and covariates

    structure(c(-0.0122310580846328, 0.159010010937409, -0.0769200505837796, 
    0.938687146743022), dim = c(1L, 4L), dimnames = list("1 v.s. 0", 
        c("Estimate", "Std.Err", "Z Value", "Pr(>|z|)")))

---

    WAoAAAACAAQFAQACAwAAAAIOAAAAAr+zWeUldOI4P+4SY/wj4GMAAAQCAAAAAQAEAAkAAAAD
    ZGltAAAADQAAAAIAAAABAAAAAgAABAIAAAABAAQACQAAAAhkaW1uYW1lcwAAABMAAAACAAAA
    EAAAAAEABAAJAAAACDEgdi5zLiAwAAAAEAAAAAIABAAJAAAAClRlc3QgU3RhdC4ABAAJAAAA
    CFByKD58enwpAAAA/g==

---

    WAoAAAACAAQFAQACAwAAAAMTAAAABAAAAw0AAAAEAAAAAQAAAAEAAAACAAAAAgAABAIAAAAB
    AAQACQAAAAZsZXZlbHMAAAAQAAAAAgAEAAkAAAAGRmVtYWxlAAQACQAAAARNYWxlAAAEAgAA
    AAEABAAJAAAABWNsYXNzAAAAEAAAAAEABAAJAAAABmZhY3RvcgAAAP4AAAMNAAAABAAAAAEA
    AAACAAAAAQAAAAIAAAQCAAAB/wAAABAAAAACAAQACQAAAAEwAAQACQAAAAExAAAEAgAAAv8A
    AAAQAAAAAQAEAAkAAAAGZmFjdG9yAAAA/gAAAA0AAAAEAAAAMAAAACoAAABBAAAARwAAAA0A
    AAAEAAAAGQAAABwAAAA4AAAANgAABAIAAAABAAQACQAAAAVuYW1lcwAAABAAAAAEAAQACQAA
    AANzZXgABAAJAAAABGVjb2cABAAJAAAACFBhdGllbnRzAAQACQAAAAZFdmVudHMAAAQCAAAA
    AQAEAAkAAAAJcm93Lm5hbWVzAAAADQAAAAKAAAAA/////AAABAIAAAL/AAAAEAAAAAEABAAJ
    AAAACmRhdGEuZnJhbWUAAAD+

# robin_surv works also with character variable in the correlation case

    structure(c(0.553358244734497, 0.236938504567525, 2.33545090421045, 
    0.0195198778286913), dim = c(1L, 4L), dimnames = list("Male v.s. Female", 
        c("Estimate", "Std.Err", "Z Value", "Pr(>|z|)")))

# robin_surv gives a warning if stratified randomization was specified but simple log rank test used

    Code
      robin_surv(Surv(time, status) ~ 1, data = surv_data, treatment = sex ~ pb(ecog))
    Condition
      Warning:
      It looks like you have not included all of the variables that were used during randomization in your analysis `formula`. You can either:
      
      a. adjust for all joint levels in your `formula` using `+ ecog` or
      b. perform a stratified test by adding to your `formula` the term `+ strata(ecog)`
      
      NOTE: (b) changes the null hypothesis from your current model specification. Please see the vignette `robincar-survival` for details.
    Output
      Model        : Surv(time, status) ~ 1
      Randomization: sex ~ pb(ecog) (Permuted-Block)
      
      Contrast     : Log Hazard Ratio
      
                       Estimate Std.Err Z Value Pr(>|z|)   
      Male v.s. Female   0.5258  0.1675  3.1391 0.001695 **
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Test         : Log-Rank
      
                       Test Stat. Pr(>|z|)   
      Male v.s. Female     3.1636 0.001558 **
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# robin_surv does give a warning if strata are not sufficiently included in the analysis model

    Code
      robin_surv(Surv(time, status) ~ 1 + strata(ecog), data = surv_data, treatment = sex ~
        pb(strata))
    Condition
      Warning:
      It looks like you have not included all of the variables that were used during randomization in your analysis `formula`. You can either:
      
      a. adjust for all joint levels in your `formula` using `+ strata` or
      b. perform a stratified test by adding to your `formula` the term `+ strata(strata)`
      
      NOTE: (b) changes the null hypothesis from your current model specification. Please see the vignette `robincar-survival` for details.
    Output
      Model        : Surv(time, status) ~ 1 + strata(ecog)
      Randomization: sex ~ pb(strata) (Permuted-Block)
      Stratification variables:  ecog 
      
      Contrast     : Stratified Log Hazard Ratio
      
                       Estimate Std.Err Z Value Pr(>|z|)   
      Male v.s. Female  0.55178 0.16997  3.2463 0.001169 **
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Test         : Stratified Log-Rank
      
                       Test Stat. Pr(>|z|)   
      Male v.s. Female     3.2776 0.001047 **
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# robin_surv does give a warning if strata are not sufficiently included in the covariates

    Code
      robin_surv(Surv(time, status) ~ 1 + ph.karno, data = surv_data, treatment = sex ~
        pb(ecog))
    Condition
      Warning:
      It looks like you have not included all of the variables that were used during randomization in your analysis `formula`. You can either:
      
      a. adjust for all joint levels in your `formula` using `+ ecog` or
      b. perform a stratified test by adding to your `formula` the term `+ strata(ecog)`
      
      NOTE: (b) changes the null hypothesis from your current model specification. Please see the vignette `robincar-survival` for details.
    Output
      Model        : Surv(time, status) ~ 1 + ph.karno
      Randomization: sex ~ pb(ecog) (Permuted-Block)
      Covariates adjusted for: ph.karno (including interactions with sex)
      
      Contrast     : Covariate-adjusted Log Hazard Ratio
      
                       Estimate Std.Err Z Value Pr(>|z|)   
      Male v.s. Female  0.51573 0.16373  3.1499 0.001633 **
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Test         : Covariate-adjusted Log-Rank
      
                       Test Stat. Pr(>|z|)   
      Male v.s. Female      3.165 0.001551 **
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# robin_surv warns if strata are insufficiently included in covariate adjusted stratified model

    Code
      robin_surv(Surv(time, status) ~ 1 + strata(ecog) + ph.karno, data = surv_data,
      treatment = sex ~ pb(strata))
    Condition
      Warning:
      It looks like you have not included all of the variables that were used during randomization in your analysis `formula`. You can either:
      
      a. adjust for all joint levels in your `formula` using `+ strata` or
      b. perform a stratified test by adding to your `formula` the term `+ strata(strata)`
      
      NOTE: (b) changes the null hypothesis from your current model specification. Please see the vignette `robincar-survival` for details.
    Output
      Model        : Surv(time, status) ~ 1 + strata(ecog) + ph.karno
      Randomization: sex ~ pb(strata) (Permuted-Block)
      Stratification variables:  ecog 
      Covariates adjusted for: ph.karno (including interactions with sex)
      
      Contrast     : Covariate-adjusted Stratified Log Hazard Ratio
      
                       Estimate Std.Err Z Value Pr(>|z|)   
      Male v.s. Female  0.54011 0.16667  3.2406 0.001193 **
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Test         : Covariate-adjusted Stratified Log-Rank
      
                       Test Stat. Pr(>|z|)   
      Male v.s. Female     3.2683 0.001082 **
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

