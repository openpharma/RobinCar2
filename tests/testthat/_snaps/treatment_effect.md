# treatment_effect works for lm/glm object

    Code
      treatment_effect(fit_binom, treatment = treatment ~ s1, eff_measure = h_diff)
    Output
      Treatment Effect
      -------------
      Model        :  y_b ~ treatment * s1 + covar 
      Randomization:  treatment ~ s1 
      Variance Type:  variance 
                  Estimate Std.Err Z Value  Pr(>z)    
      trt1 - pbo    0.2246  0.0477    4.71 2.5e-06 ***
      trt2 - pbo    0.2653  0.0475    5.58 2.4e-08 ***
      trt2 - trt1   0.0407  0.0479    0.85     0.4    
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

---

    Code
      treatment_effect(fit_lm, treatment = treatment ~ s1, eff_measure = h_diff)
    Output
      Treatment Effect
      -------------
      Model        :  NULL 
      Randomization:  treatment ~ s1 
      Variance Type:  variance 
                  Estimate Std.Err Z Value  Pr(>z)    
      trt1 - pbo     0.564   0.101    5.60 2.2e-08 ***
      trt2 - pbo     0.771   0.101    7.61 2.8e-14 ***
      trt2 - trt1    0.207   0.107    1.94   0.052 .  
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

