# predict_counterfactual works for guassian

    Code
      predict_counterfactual(fit_glm, "Species")
    Output
          setosa versicolor  virginica 
        4.708147   6.166890   6.654963 

# predict_counterfactual works for guassian with lm

    Code
      predict_counterfactual(fit_lm, "Species", data = iris)
    Output
          setosa versicolor  virginica 
        4.708147   6.166890   6.654963 

# predict_counterfactual works for binomial

    Code
      predict_counterfactual(fit_binom, "supp")
    Output
             OJ        VC 
      0.7666667 0.6000000 

