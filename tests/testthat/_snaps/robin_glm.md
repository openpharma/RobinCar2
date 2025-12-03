# robin_glm can be printed correctly

    Code
      robin_res1
    Output
      Model        :  y_b ~ treatment * s1 
      Randomization:  treatment ~ s1  ( Simple )
      Variance Type:  vcovG 
      Marginal Mean: 
           Estimate  Std.Err    2.5 % 97.5 %
      pbo  0.366007 0.033864 0.299634 0.4324
      trt1 0.580984 0.035027 0.512332 0.6496
      trt2 0.610154 0.034472 0.542589 0.6777
      
      Contrast     :  log_risk_ratio
                     Estimate  Std.Err Z Value  Pr(>|z|)    
      trt1 v.s. pbo  0.462072 0.110251  4.1911 2.776e-05 ***
      trt2 v.s. pbo  0.511059 0.108251  4.7211 2.346e-06 ***
      trt2 v.s. trt1 0.048987 0.082488  0.5939    0.5526    
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

---

    Code
      robin_res2
    Output
      Model        :  y_b ~ treatment * s1 
      Randomization:  treatment ~ s1  ( Simple )
      Variance Type:  vcovG 
      Marginal Mean: 
           Estimate  Std.Err    2.5 % 97.5 %
      pbo  0.366007 0.033864 0.299634 0.4324
      trt1 0.580984 0.035027 0.512332 0.6496
      trt2 0.610154 0.034472 0.542589 0.6777
      
      Contrast     :  difference
                     Estimate  Std.Err Z Value  Pr(>|z|)    
      trt1 v.s. pbo  0.214978 0.048633  4.4204 9.850e-06 ***
      trt2 v.s. pbo  0.244147 0.048244  5.0607 4.177e-07 ***
      trt2 v.s. trt1 0.029169 0.049064  0.5945    0.5522    
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

---

    Code
      robin_glm(y_b ~ treatment + s1, data = glm_data, treatment = treatment ~ s1)
    Output
      Model        :  y_b ~ treatment + s1 
      Randomization:  treatment ~ s1  ( Simple )
      Variance Type:  vcovG 
      Marginal Mean: 
           Estimate  Std.Err    2.5 % 97.5 %
      pbo  0.366010 0.033864 0.299638 0.4324
      trt1 0.580976 0.035027 0.512324 0.6496
      trt2 0.610163 0.034473 0.542598 0.6777
      
      Contrast     :  difference
                     Estimate  Std.Err Z Value  Pr(>|z|)    
      trt1 v.s. pbo  0.214966 0.048633  4.4202 9.862e-06 ***
      trt2 v.s. pbo  0.244153 0.048244  5.0608 4.175e-07 ***
      trt2 v.s. trt1 0.029187 0.049064  0.5949    0.5519    
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

