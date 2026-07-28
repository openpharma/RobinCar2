# print method for surv_effect works as expected (unstratified, unadjusted)

    Code
      print(x)
    Output
      Model        : Surv(time, status) ~ 1
      Randomization: sex ~ sr(1) (Simple)
      
      Contrast     : Log Hazard Ratio
      
                       Estimate Std.Err Z Value Pr(>|z|)   
      Male v.s. Female  0.53037 0.16718  3.1724 0.001512 **
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Test         : Log-Rank
      
                       Test Stat. Pr(>|z|)   
      Male v.s. Female     3.2135 0.001311 **
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# print method for surv_effect works as expected (stratified, unadjusted)

    Code
      print(x)
    Output
      Model        : Surv(time, status) ~ 1 + strata(strata)
      Randomization: sex ~ sr(1) (Simple)
      Stratification variables:  strata 
      
      Contrast     : Stratified Log Hazard Ratio
      
                       Estimate Std.Err Z Value Pr(>|z|)   
      Male v.s. Female   0.5536  0.1706  3.2451 0.001174 **
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Test         : Stratified Log-Rank
      
                       Test Stat. Pr(>|z|)   
      Male v.s. Female     3.2856 0.001018 **
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# print method for surv_effect works as expected (unstratified, adjusted)

    Code
      print(x)
    Output
      Model        : Surv(time, status) ~ meal.cal + age
      Randomization: sex ~ sr(1) (Simple)
      Covariates adjusted for: meal.cal, age (including interactions with sex)
      
      Contrast     : Covariate-adjusted Log Hazard Ratio
      
                       Estimate Std.Err Z Value Pr(>|z|)   
      Male v.s. Female  0.48335 0.18631  2.5944 0.009477 **
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Test         : Covariate-adjusted Log-Rank
      
                       Test Stat. Pr(>|z|)   
      Male v.s. Female     2.6858 0.007236 **
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# print method for surv_effect works as expected (stratified, adjusted)

    Code
      print(x)
    Output
      Model        : Surv(time, status) ~ meal.cal + age + strata(strata)
      Randomization: sex ~ sr(1) (Simple)
      Stratification variables:  strata 
      Covariates adjusted for: meal.cal, age (including interactions with sex)
      
      Contrast     : Covariate-adjusted Stratified Log Hazard Ratio
      
                       Estimate Std.Err Z Value Pr(>|z|)   
      Male v.s. Female  0.54791 0.19118   2.866 0.004157 **
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Test         : Covariate-adjusted Stratified Log-Rank
      
                       Test Stat. Pr(>|z|)   
      Male v.s. Female     2.9496 0.003181 **
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# print method for surv_effect works as expected when no hazard ratio was estimated

    Code
      print(x)
    Output
      Model        : Surv(time, status) ~ meal.cal + age + strata(strata)
      Randomization: sex ~ sr(1) (Simple)
      Stratification variables:  strata 
      Covariates adjusted for: meal.cal, age (including interactions with sex)
      
      Contrast     : None
      
      Test         : Covariate-adjusted Stratified Log-Rank
      
                       Test Stat. Pr(>|z|)   
      Male v.s. Female     2.9496 0.003181 **
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# table method for surv_effect works as expected

    Code
      result <- table(x)
    Output
      Number of patients and events per stratum and treatment arm:
        strata    sex Patients Events
      1      0 Female       20      6
      2      0   Male       30     24
      3      1 Female       29     21
      4      1   Male       57     43
      5      2 Female       18     14
      6      2   Male       25     24
      7      3   Male        1      1

# confint method for surv_effect works as expected

    Code
      result <- confint(x)

---

    Code
      expect_message(result <- confint(x, transform = exp),
      "The confidence interval is transformed.", fixed = TRUE)

