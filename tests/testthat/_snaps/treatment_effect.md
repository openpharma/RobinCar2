# treatment_effect works for lm/glm object

    Code
      treatment_effect(fit_binom, treatment = treatment ~ s1, eff_measure = h_diff)
    Output
      Model        :  y_b ~ treatment * s1 + covar 
      Randomization:  treatment ~ s1  ( Simple )
      Variance Type:  vcovG 
      Marginal Mean: 
           Estimate  Std.Err    2.5 % 97.5 %
      pbo  0.356097 0.033599 0.290243 0.4219
      trt1 0.580696 0.034418 0.513238 0.6482
      trt2 0.621386 0.034019 0.554711 0.6881
      
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
           Estimate  Std.Err    2.5 % 97.5 %
      pbo  0.200321 0.067690 0.067651 0.3330
      trt1 0.763971 0.075929 0.615152 0.9128
      trt2 0.971250 0.076543 0.821228 1.1213
      
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
           Estimate  Std.Err    2.5 % 97.5 %
      pbo  0.356097 0.033599 0.290243 0.4219
      trt1 0.580696 0.034418 0.513238 0.6482
      trt2 0.621386 0.034019 0.554711 0.6881
      
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
           Estimate  Std.Err    2.5 % 97.5 %
      pbo  0.356097 0.033599 0.290243 0.4219
      trt1 0.580696 0.034418 0.513238 0.6482
      trt2 0.621386 0.034019 0.554711 0.6881
      
      Contrast     :  eff_measure
                    Estimate  Std.Err Z Value  Pr(>|z|)    
      trt1 v.s. pbo 0.224599 0.047711  4.7075 2.508e-06 ***
      trt2 v.s. pbo 0.265290 0.047534  5.5810 2.391e-08 ***
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

