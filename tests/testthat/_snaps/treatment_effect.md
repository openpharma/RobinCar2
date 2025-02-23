# treatment_effect works for lm/glm object

    Code
      treatment_effect(fit_binom, treatment = treatment ~ s1, eff_measure = h_diff)
    Output
      Model        :  y_b ~ treatment * s1 + covar 
      Randomization:  treatment ~ s1  ( Simple )
      Variance Type:  vcovG 
      Marginal Mean: 
           Estimate  Std.Err Z Value  Pr(>|z|)    
      pbo  0.356097 0.033599  10.598 < 2.2e-16 ***
      trt1 0.580696 0.034418  16.872 < 2.2e-16 ***
      trt2 0.621386 0.034019  18.266 < 2.2e-16 ***
      
      Contrast     :  eff_measure
                     Estimate  Std.Err Z Value  Pr(>|z|)    
      trt1 v.s. pbo  0.224599 0.047711  4.7075 2.508e-06 ***
      trt2 v.s. pbo  0.265290 0.047534  5.5810 2.391e-08 ***
      trt2 v.s. trt1 0.040691 0.047941  0.8488     0.396    
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

---

    Code
      treatment_effect(fit_lm, treatment = treatment ~ s1, eff_measure = h_diff,
      data = dummy_data)
    Output
      Model        :  y ~ treatment * s1 + covar 
      Randomization:  treatment ~ s1  ( Simple )
      Variance Type:  vcovG 
      Marginal Mean: 
           Estimate  Std.Err Z Value  Pr(>|z|)    
      pbo  0.200321 0.067690  2.9594  0.003083 ** 
      trt1 0.763971 0.075929 10.0616 < 2.2e-16 ***
      trt2 0.971250 0.076543 12.6889 < 2.2e-16 ***
      
      Contrast     :  eff_measure
                     Estimate Std.Err Z Value  Pr(>|z|)    
      trt1 v.s. pbo   0.56365 0.10074  5.5952 2.203e-08 ***
      trt2 v.s. pbo   0.77093 0.10133  7.6082 2.779e-14 ***
      trt2 v.s. trt1  0.20728 0.10683  1.9402   0.05235 .  
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# treatment_effect works if variance is not used

    Code
      treatment_effect(fit_binom, treatment = treatment ~ s1, eff_measure = h_diff,
      variance = NULL)
    Output
      Model        :  y_b ~ treatment * s1 + covar 
      Randomization:  treatment ~ s1  ( Simple )
      Variance Type:  vcovG 
      Marginal Mean: 
           Estimate  Std.Err Z Value  Pr(>|z|)    
      pbo  0.356097 0.033599  10.598 < 2.2e-16 ***
      trt1 0.580696 0.034418  16.872 < 2.2e-16 ***
      trt2 0.621386 0.034019  18.266 < 2.2e-16 ***
      
      Contrast     :  eff_measure
                     Estimate  Std.Err Z Value  Pr(>|z|)    
      trt1 v.s. pbo  0.224599 0.047711  4.7075 2.508e-06 ***
      trt2 v.s. pbo  0.265290 0.047534  5.5810 2.391e-08 ***
      trt2 v.s. trt1 0.040691 0.047941  0.8488     0.396    
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# treatment_effect works if pair is defined

    Code
      treatment_effect(fit_binom, pair = against_ref(c("pbo", "trt1", "trt2")),
      treatment = treatment ~ s1, eff_measure = h_diff)
    Output
      Model        :  y_b ~ treatment * s1 + covar 
      Randomization:  treatment ~ s1  ( Simple )
      Variance Type:  vcovG 
      Marginal Mean: 
           Estimate  Std.Err Z Value  Pr(>|z|)    
      pbo  0.356097 0.033599  10.598 < 2.2e-16 ***
      trt1 0.580696 0.034418  16.872 < 2.2e-16 ***
      trt2 0.621386 0.034019  18.266 < 2.2e-16 ***
      
      Contrast     :  eff_measure
                    Estimate  Std.Err Z Value  Pr(>|z|)    
      trt1 v.s. pbo 0.224599 0.047711  4.7075 2.508e-06 ***
      trt2 v.s. pbo 0.265290 0.047534  5.5810 2.391e-08 ***
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

