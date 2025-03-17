# predict_counterfactual works for guassian

    Code
      predict_counterfactual(fit_glm, treatment ~ 1)
    Output
      Model        :  y ~ treatment * s1 + covar 
      Randomization:  treatment ~ 1  ( Simple )
      Variance Type:  vcovG 
      Marginal Mean: 
           Estimate  Std.Err    2.5 % 97.5 %
      pbo  0.200321 0.067690 0.067651 0.3330
      trt1 0.763971 0.075929 0.615152 0.9128
      trt2 0.971250 0.076543 0.821228 1.1213

# predict_counterfactual works for guassian with lm

    Code
      predict_counterfactual(fit_lm, treatment ~ 1, data = dummy_data)
    Output
      Model        :  y ~ treatment * s1 + covar 
      Randomization:  treatment ~ 1  ( Simple )
      Variance Type:  vcovG 
      Marginal Mean: 
           Estimate  Std.Err    2.5 % 97.5 %
      pbo  0.200321 0.067690 0.067651 0.3330
      trt1 0.763971 0.075929 0.615152 0.9128
      trt2 0.971250 0.076543 0.821228 1.1213

# predict_counterfactual works for binomial

    Code
      predict_counterfactual(fit_binom, treatment ~ 1)
    Output
      Model        :  y_b ~ treatment * s1 + covar 
      Randomization:  treatment ~ 1  ( Simple )
      Variance Type:  vcovG 
      Marginal Mean: 
           Estimate  Std.Err    2.5 % 97.5 %
      pbo  0.356097 0.033599 0.290243 0.4219
      trt1 0.580696 0.034418 0.513238 0.6482
      trt2 0.621386 0.034019 0.554711 0.6881

# predict_counterfactual works if contrast are non-standard

    Code
      pc
    Output
      Model        :  y_b ~ treatment * s1 
      Randomization:  treatment ~ 1  ( Simple )
      Variance Type:  vcovG 
      Marginal Mean: 
           Estimate  Std.Err    2.5 % 97.5 %
      pbo  0.366007 0.033864 0.299634 0.4324
      trt1 0.580984 0.035027 0.512332 0.6496
      trt2 0.610154 0.034472 0.542589 0.6777

