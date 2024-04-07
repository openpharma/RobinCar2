# predict_counterfactual works for guassian

    Code
      predict_counterfactual(fit, "Species")
    Output
          setosa versicolor  virginica 
        4.708147   6.166890   6.654963 

# predict_counterfactual works for guassian with lm

    Code
      predict_counterfactual(fit, "Species", data = iris)
    Output
          setosa versicolor  virginica 
        4.708147   6.166890   6.654963 

# predict_counterfactual works for binomial

    Code
      predict_counterfactual(fit, "supp")
    Output
             OJ        VC 
      0.8666667 0.8000000 

