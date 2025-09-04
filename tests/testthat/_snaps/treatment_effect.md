# treatment_effect works for lm/glm object

    Code
      treatment_effect(fit_binom, treatment = treatment ~ s1, eff_measure = h_diff)
    Output
                     Estimate  Std.Err Z Value  Pr(>|z|)    
      trt1 v.s. pbo  0.224599 0.047711  4.7075 2.508e-06 ***
      trt2 v.s. pbo  0.265290 0.047534  5.5810 2.391e-08 ***
      trt2 v.s. trt1 0.040691 0.047941  0.8488     0.396    
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

---

    Code
      treatment_effect(fit_lm, treatment = treatment ~ s1, eff_measure = h_diff,
      data = glm_data)
    Output
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
                    Estimate  Std.Err Z Value  Pr(>|z|)    
      trt1 v.s. pbo 0.224599 0.047711  4.7075 2.508e-06 ***
      trt2 v.s. pbo 0.265290 0.047534  5.5810 2.391e-08 ***
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# confint method for treatment_effect works as expected

    Code
      confint(treatment_effect(fit_binom, treatment = treatment ~ s1, eff_measure = h_diff))
    Output
                       Estimate      2.5 %    97.5 %
      trt1 v.s. pbo  0.22459921  0.1310866 0.3181118
      trt2 v.s. pbo  0.26528993  0.1721248 0.3584551
      trt2 v.s. trt1 0.04069073 -0.0532712 0.1346527

