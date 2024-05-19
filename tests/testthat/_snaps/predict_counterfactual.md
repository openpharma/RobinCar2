# predict_counterfactual works for guassian

    Code
      predict_counterfactual(fit_glm, "treatment")
    Output
      counter-factual prediction
      
            pbo      trt1      trt2 
      0.2003208 0.7639709 0.9712499 
      

# predict_counterfactual works for guassian with lm

    Code
      predict_counterfactual(fit_lm, "treatment", data = dummy_data)
    Output
      counter-factual prediction
      
            pbo      trt1      trt2 
      0.2003208 0.7639709 0.9712499 
      

# predict_counterfactual works for binomial

    Code
      predict_counterfactual(fit_binom, "treatment")
    Output
      counter-factual prediction
      
            pbo      trt1      trt2 
      0.3560965 0.5806957 0.6213865 
      

