# robin_mh print method produces the expected output

    Code
      print(res)
    Output
      Model        : y_b ~ s1 + s2
      Randomization: treatment ~ pb(s1, s2) (Permuted-Block)
      Stratification variables: s1, s2
      
      Estimand     : Average Treatment Effect (risk difference)
      Variance     : Modified Greenland-Robins
      
                    Estimate  Std.Err Z Value  Pr(>|z|)    
      trt1 v.s. pbo 0.214683 0.048667  4.4113 1.028e-05 ***
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

