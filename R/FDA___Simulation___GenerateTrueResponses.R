FDA___Simulation___GenerateTrueResponses = function(Smoothed_Results.list, True_Coef_Functions.list, cutoff = 0.5){
  ## 🟨 Linear Predictor by Inner product ==========================================================================
  Linear_Predictor = list()
  for(i in seq_along(True_Coef_Functions.list)){

    Linear_Predictor[[i]] = fda::inprod(Smoothed_Results.list[[i]]$smoothing$fd,
                                        True_Coef_Functions.list[[i]])

  }

  Linear_Predictor.df = do.call(cbind, Linear_Predictor)


  ## 🟨 probabilities by a sigmoid function ==========================================================================
  p = apply(Linear_Predictor.df, MARGIN=1, function(ith_record){
    E = sum(ith_record) %>% exp()
    E/(1+E)
  })


  ## 🟨 Decide category by probabilities ==========================================================================
  Category = ifelse(p > cutoff, 1, 0)



  Results = data.frame(Category = Category, p = p)
  return(Results)

}


