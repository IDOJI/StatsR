Classification___Logistic___Binomial___MLE = function(Logistic){
  #=============================================================================
  # Data combining
  #=============================================================================
  Binded_Data = dplyr::bind_cols(Logistic$Train_y, Logistic$Train_X)





  #=============================================================================
  # Fit proportional odds model
  #=============================================================================
  Formula = SUB___as.formula(y = Logistic$Train_y %>% names,
                             x = Logistic$Train_X %>% names)

  Logistic$Best_Model = glm(Formula, data = Binded_Data, family = "binomial")






  #=============================================================================
  # Results
  #=============================================================================
  Results = Classification___Logistic___Results(Logistic)




  return(Results)

}
