# treatment_effect works for lm/glm object

    Code
      treatment_effect(fit_binom, treatment = treatment ~ s1, eff_measure = h_diff)
    Output
      Treatment Effect
      -------------
      Model        :  y_b ~ treatment * s1 + covar 
      Randomization:  treatment ~ s1 
      Marginal Mean: 
      counter-factual prediction
      
            pbo      trt1      trt2 
      0.3560965 0.5806957 0.6213865 
      
      Variance Type:  gvcov 
                  Estimate Std.Err Z Value Pr(>|z|)    
      trt1 - pbo    0.2246  0.0477    4.71  2.5e-06 ***
      trt2 - pbo    0.2653  0.0475    5.58  2.4e-08 ***
      trt2 - trt1   0.0407  0.0479    0.85      0.4    
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

---

    Code
      treatment_effect(fit_lm, treatment = treatment ~ s1, eff_measure = h_diff)
    Output
      Treatment Effect
      -------------
      Model        :  y ~ treatment * s1 + covar 
      Randomization:  treatment ~ s1 
      Marginal Mean: 
      counter-factual prediction
      
            pbo      trt1      trt2 
      0.2003208 0.7639709 0.9712499 
      
      Variance Type:  gvcov 
                  Estimate Std.Err Z Value Pr(>|z|)    
      trt1 - pbo     0.564   0.101    5.60  2.2e-08 ***
      trt2 - pbo     0.771   0.101    7.61  2.8e-14 ***
      trt2 - trt1    0.207   0.107    1.94    0.052 .  
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# treatment_effect works if variance is not used

    Code
      treatment_effect(fit_binom, treatment = treatment ~ s1, eff_measure = h_diff,
      variance = NULL)
    Output
      Treatment Effect
      -------------
      Model        :  y_b ~ treatment * s1 + covar 
      Randomization:  treatment ~ s1 
      Marginal Mean: 
      counter-factual prediction
      
            pbo      trt1      trt2 
      0.3560965 0.5806957 0.6213865 
      
      Variance Type:  none 
                  Estimate Std.Err Z Value Pr(>|z|)
      trt1 - pbo    0.2246      NA      NA       NA
      trt2 - pbo    0.2653      NA      NA       NA
      trt2 - trt1   0.0407      NA      NA       NA

# treatment_effect works if pair is integer

    Code
      treatment_effect(fit_binom, pair = c(1, 2), treatment = treatment ~ s1,
      eff_measure = h_diff)
    Output
      Treatment Effect
      -------------
      Model        :  y_b ~ treatment * s1 + covar 
      Randomization:  treatment ~ s1 
      Marginal Mean: 
      counter-factual prediction
      
            pbo      trt1      trt2 
      0.3560965 0.5806957 0.6213865 
      
      Variance Type:  gvcov 
                 Estimate Std.Err Z Value Pr(>|z|)    
      trt1 - pbo   0.2246  0.0477    4.71  2.5e-06 ***
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

