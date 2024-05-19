# robincar convariance estimation works for guassian fitted by lm

    Code
      vcovRobinCar.prediction_cf(predict_counterfactual(fit_lm, "treatment",
        dummy_data))
    Output
      $theta
            pbo      trt1      trt2 
      0.2003208 0.7639709 0.9712499 
      
      $V
                  pbo       trt1       trt2
      pbo  2.74152393 0.05972904 0.05201403
      trt1 0.05972904 3.44436398 0.06333053
      trt2 0.05201403 0.06333053 3.53553897
      
      $f_vcov
      NULL
      
      $n
      [1] 600
      
      $eff_measure
      NULL
      
      $trt_var_name
      [1] "treatment"
      
      $fit
      
      Call:
      lm(formula = y ~ treatment * s1 + covar, data = dummy_data)
      
      Coefficients:
            (Intercept)      treatmenttrt1      treatmenttrt2                s1b  
                0.33108            0.54454            0.73872           -0.27656  
                  covar  treatmenttrt1:s1b  treatmenttrt2:s1b  
                0.20396            0.03796            0.06399  
      
      
      attr(,"class")
      [1] "list"          "vcov_robincar"

---

    Code
      vcovRobinCar.prediction_cf(predict_counterfactual(fit_lm, "treatment",
        dummy_data), eff_measure = "diff")
    Output
      $theta
            pbo      trt1      trt2 
      0.2003208 0.7639709 0.9712499 
      
      $V
                  pbo       trt1       trt2
      pbo  2.74152393 0.05972904 0.05201403
      trt1 0.05972904 3.44436398 0.06333053
      trt2 0.05201403 0.06333053 3.53553897
      
      $f_vcov
                 pbo      trt1      trt2
      pbo  0.0000000 0.2476610 0.2520131
      trt1 0.2476610 0.0000000 0.2797824
      trt2 0.2520131 0.2797824 0.0000000
      
      $n
      [1] 600
      
      $eff_measure
      [1] "diff"
      
      $trt_var_name
      [1] "treatment"
      
      $fit
      
      Call:
      lm(formula = y ~ treatment * s1 + covar, data = dummy_data)
      
      Coefficients:
            (Intercept)      treatmenttrt1      treatmenttrt2                s1b  
                0.33108            0.54454            0.73872           -0.27656  
                  covar  treatmenttrt1:s1b  treatmenttrt2:s1b  
                0.20396            0.03796            0.06399  
      
      
      attr(,"class")
      [1] "list"          "vcov_robincar"

# robincar convariance estimation works for guassian fitted by glm

    Code
      vcovRobinCar.prediction_cf(predict_counterfactual(fit_glm, "treatment",
        dummy_data))
    Output
      $theta
            pbo      trt1      trt2 
      0.2003208 0.7639709 0.9712499 
      
      $V
                  pbo       trt1       trt2
      pbo  2.74152393 0.05972904 0.05201403
      trt1 0.05972904 3.44436398 0.06333053
      trt2 0.05201403 0.06333053 3.53553897
      
      $f_vcov
      NULL
      
      $n
      [1] 600
      
      $eff_measure
      NULL
      
      $trt_var_name
      [1] "treatment"
      
      $fit
      
      Call:  glm(formula = y ~ treatment * s1 + covar, data = dummy_data)
      
      Coefficients:
            (Intercept)      treatmenttrt1      treatmenttrt2                s1b  
                0.33108            0.54454            0.73872           -0.27656  
                  covar  treatmenttrt1:s1b  treatmenttrt2:s1b  
                0.20396            0.03796            0.06399  
      
      Degrees of Freedom: 599 Total (i.e. Null);  593 Residual
      Null Deviance:	    724.3 
      Residual Deviance: 632.3 	AIC: 1750
      
      attr(,"class")
      [1] "list"          "vcov_robincar"

---

    Code
      vcovRobinCar.prediction_cf(predict_counterfactual(fit_glm, "treatment",
        dummy_data), eff_measure = "diff")
    Output
      $theta
            pbo      trt1      trt2 
      0.2003208 0.7639709 0.9712499 
      
      $V
                  pbo       trt1       trt2
      pbo  2.74152393 0.05972904 0.05201403
      trt1 0.05972904 3.44436398 0.06333053
      trt2 0.05201403 0.06333053 3.53553897
      
      $f_vcov
                 pbo      trt1      trt2
      pbo  0.0000000 0.2476610 0.2520131
      trt1 0.2476610 0.0000000 0.2797824
      trt2 0.2520131 0.2797824 0.0000000
      
      $n
      [1] 600
      
      $eff_measure
      [1] "diff"
      
      $trt_var_name
      [1] "treatment"
      
      $fit
      
      Call:  glm(formula = y ~ treatment * s1 + covar, data = dummy_data)
      
      Coefficients:
            (Intercept)      treatmenttrt1      treatmenttrt2                s1b  
                0.33108            0.54454            0.73872           -0.27656  
                  covar  treatmenttrt1:s1b  treatmenttrt2:s1b  
                0.20396            0.03796            0.06399  
      
      Degrees of Freedom: 599 Total (i.e. Null);  593 Residual
      Null Deviance:	    724.3 
      Residual Deviance: 632.3 	AIC: 1750
      
      attr(,"class")
      [1] "list"          "vcov_robincar"

# robincar convariance estimation works for binomial fitted by glm

    Code
      vcovRobinCar.prediction_cf(predict_counterfactual(fit_binom, "treatment",
        dummy_data))
    Output
      $theta
            pbo      trt1      trt2 
      0.3560965 0.5806957 0.6213865 
      
      $V
                   pbo       trt1        trt2
      pbo  0.676245627 0.01113740 0.008003312
      trt1 0.011137402 0.70765788 0.013068673
      trt2 0.008003312 0.01306867 0.698427494
      
      $f_vcov
      NULL
      
      $n
      [1] 600
      
      $eff_measure
      NULL
      
      $trt_var_name
      [1] "treatment"
      
      $fit
      
      Call:  glm(formula = y_b ~ treatment * s1 + covar, family = binomial(), 
          data = dummy_data)
      
      Coefficients:
            (Intercept)      treatmenttrt1      treatmenttrt2                s1b  
               -0.42927            0.97415            1.10707           -0.42043  
                  covar  treatmenttrt1:s1b  treatmenttrt2:s1b  
                0.41049           -0.01731            0.06947  
      
      Degrees of Freedom: 599 Total (i.e. Null);  593 Residual
      Null Deviance:	    831 
      Residual Deviance: 773.7 	AIC: 787.7
      
      attr(,"class")
      [1] "list"          "vcov_robincar"

---

    Code
      vcovRobinCar.prediction_cf(predict_counterfactual(fit_binom, "treatment",
        dummy_data), eff_measure = "diff")
    Output
      $theta
            pbo      trt1      trt2 
      0.3560965 0.5806957 0.6213865 
      
      $V
                   pbo       trt1        trt2
      pbo  0.676245627 0.01113740 0.008003312
      trt1 0.011137402 0.70765788 0.013068673
      trt2 0.008003312 0.01306867 0.698427494
      
      $f_vcov
                  pbo       trt1       trt2
      pbo  0.00000000 0.05558826 0.05546733
      trt1 0.05558826 0.00000000 0.05633614
      trt2 0.05546733 0.05633614 0.00000000
      
      $n
      [1] 600
      
      $eff_measure
      [1] "diff"
      
      $trt_var_name
      [1] "treatment"
      
      $fit
      
      Call:  glm(formula = y_b ~ treatment * s1 + covar, family = binomial(), 
          data = dummy_data)
      
      Coefficients:
            (Intercept)      treatmenttrt1      treatmenttrt2                s1b  
               -0.42927            0.97415            1.10707           -0.42043  
                  covar  treatmenttrt1:s1b  treatmenttrt2:s1b  
                0.41049           -0.01731            0.06947  
      
      Degrees of Freedom: 599 Total (i.e. Null);  593 Residual
      Null Deviance:	    831 
      Residual Deviance: 773.7 	AIC: 787.7
      
      attr(,"class")
      [1] "list"          "vcov_robincar"

---

    Code
      vcovRobinCar.prediction_cf(predict_counterfactual(fit_binom, "treatment",
        dummy_data), eff_measure = "risk ratio")
    Output
      $theta
            pbo      trt1      trt2 
      0.3560965 0.5806957 0.6213865 
      
      $V
                   pbo       trt1        trt2
      pbo  0.676245627 0.01113740 0.008003312
      trt1 0.011137402 0.70765788 0.013068673
      trt2 0.008003312 0.01306867 0.698427494
      
      $f_vcov
                 pbo      trt1      trt2
      pbo  0.0000000 0.2989941 0.2886095
      trt1 0.2989941 0.0000000 0.1565623
      trt2 0.2886095 0.1565623 0.0000000
      
      $n
      [1] 600
      
      $eff_measure
      [1] "risk ratio"
      
      $trt_var_name
      [1] "treatment"
      
      $fit
      
      Call:  glm(formula = y_b ~ treatment * s1 + covar, family = binomial(), 
          data = dummy_data)
      
      Coefficients:
            (Intercept)      treatmenttrt1      treatmenttrt2                s1b  
               -0.42927            0.97415            1.10707           -0.42043  
                  covar  treatmenttrt1:s1b  treatmenttrt2:s1b  
                0.41049           -0.01731            0.06947  
      
      Degrees of Freedom: 599 Total (i.e. Null);  593 Residual
      Null Deviance:	    831 
      Residual Deviance: 773.7 	AIC: 787.7
      
      attr(,"class")
      [1] "list"          "vcov_robincar"

---

    Code
      vcovRobinCar.prediction_cf(predict_counterfactual(fit_binom, "treatment",
        dummy_data), eff_measure = "odds ratio")
    Output
      $theta
            pbo      trt1      trt2 
      0.3560965 0.5806957 0.6213865 
      
      $V
                   pbo       trt1        trt2
      pbo  0.676245627 0.01113740 0.008003312
      trt1 0.011137402 0.70765788 0.013068673
      trt2 0.008003312 0.01306867 0.698427494
      
      $f_vcov
                 pbo      trt1      trt2
      pbo  0.0000000 0.9961186 1.0281436
      trt1 0.9961186 0.0000000 0.9838131
      trt2 1.0281436 0.9838131 0.0000000
      
      $n
      [1] 600
      
      $eff_measure
      [1] "odds ratio"
      
      $trt_var_name
      [1] "treatment"
      
      $fit
      
      Call:  glm(formula = y_b ~ treatment * s1 + covar, family = binomial(), 
          data = dummy_data)
      
      Coefficients:
            (Intercept)      treatmenttrt1      treatmenttrt2                s1b  
               -0.42927            0.97415            1.10707           -0.42043  
                  covar  treatmenttrt1:s1b  treatmenttrt2:s1b  
                0.41049           -0.01731            0.06947  
      
      Degrees of Freedom: 599 Total (i.e. Null);  593 Residual
      Null Deviance:	    831 
      Residual Deviance: 773.7 	AIC: 787.7
      
      attr(,"class")
      [1] "list"          "vcov_robincar"

