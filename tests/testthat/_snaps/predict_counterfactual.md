# predict_counterfactual works for guassian

    Code
      predict_counterfactual(fit_glm, treatment ~ 1)
    Output
      Model        :  y ~ treatment * s1 + covar 
      Randomization:  treatment ~ 1  ( Simple )
      Variance Type:  vcovG 
      Marginal Mean: 
           Estimate  Std.Err Z Value  Pr(>|z|)    
      pbo  0.200321 0.067690  2.9594  0.003083 ** 
      trt1 0.763971 0.075929 10.0616 < 2.2e-16 ***
      trt2 0.971250 0.076543 12.6889 < 2.2e-16 ***
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# predict_counterfactual works for guassian with lm

    Code
      predict_counterfactual(fit_lm, treatment ~ 1, data = dummy_data)
    Output
      Model        :  y ~ treatment * s1 + covar 
      Randomization:  treatment ~ 1  ( Simple )
      Variance Type:  vcovG 
      Marginal Mean: 
           Estimate  Std.Err Z Value  Pr(>|z|)    
      pbo  0.200321 0.067690  2.9594  0.003083 ** 
      trt1 0.763971 0.075929 10.0616 < 2.2e-16 ***
      trt2 0.971250 0.076543 12.6889 < 2.2e-16 ***
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# predict_counterfactual works for binomial

    Code
      predict_counterfactual(fit_binom, treatment ~ 1)
    Output
      Model        :  y_b ~ treatment * s1 + covar 
      Randomization:  treatment ~ 1  ( Simple )
      Variance Type:  vcovG 
      Marginal Mean: 
           Estimate  Std.Err Z Value  Pr(>|z|)    
      pbo  0.356097 0.033599  10.598 < 2.2e-16 ***
      trt1 0.580696 0.034418  16.872 < 2.2e-16 ***
      trt2 0.621386 0.034019  18.266 < 2.2e-16 ***
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# predict_counterfactual works if contrast are non-standard

    Code
      pc
    Output
      Model        :  y_b ~ treatment * s1 
      Randomization:  treatment ~ 1  ( Simple )
      Variance Type:  vcovG 
      Marginal Mean: 
           Estimate  Std.Err Z Value  Pr(>|z|)    
      pbo  0.366007 0.033864  10.808 < 2.2e-16 ***
      trt1 0.580984 0.035027  16.587 < 2.2e-16 ***
      trt2 0.610154 0.034472  17.700 < 2.2e-16 ***
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

