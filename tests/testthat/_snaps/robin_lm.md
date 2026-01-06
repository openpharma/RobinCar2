# robin_lm works correctly

    Code
      robin_lm(y_b ~ treatment + s1, data = glm_data, treatment = treatment ~ s1)
    Output
      Model        :  y_b ~ treatment + s1 
      Randomization:  treatment ~ s1  ( Simple )
      Variance Type:  vcovG 
      Marginal Mean: 
           Estimate  Std.Err    2.5 % 97.5 %
      pbo  0.366010 0.033864 0.299638 0.4324
      trt1 0.580976 0.035027 0.512324 0.6496
      trt2 0.610163 0.034473 0.542598 0.6777
      
      Contrast     :  h_diff
                     Estimate  Std.Err Z Value  Pr(>|z|)    
      trt1 v.s. pbo  0.214966 0.048633  4.4202 9.862e-06 ***
      trt2 v.s. pbo  0.244153 0.048244  5.0608 4.175e-07 ***
      trt2 v.s. trt1 0.029187 0.049064  0.5949    0.5519    
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

