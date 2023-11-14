Classification___Logistic___Multinomial___Ordinal___MLE = function(Logistic){
  #=============================================================================
  # Data combining
  #=============================================================================
  Binded_Data = dplyr::bind_cols(Logistic$Train_y, Logistic$Train_X)






  #=============================================================================
  # Install.pacakges
  #=============================================================================
  install_packages("MASS")






  #=============================================================================
  # Fit proportional odds model
  #=============================================================================
  Formula = SUB___as.formula(y = Logistic$Train_y %>% names,
                             x = Logistic$Train_X %>% names)

  Fit = MASS::polr(formula = Formula,
                   data = Binded_Data,
                   Hess=TRUE,
                   method = Logistic$Link) %>% suppressWarnings()







  #=============================================================================
  # Extract results
  #=============================================================================
  Results = Classification___Logistic___Results(Fit,
                                                 Best_alpha=NULL,
                                                 X_Test,
                                                 y_Test,
                                                 x_varname,
                                                 y_varname,
                                                 AUC_in_Legend,
                                                 path_Export)






  #=============================================================================
  # interpretaion warning
  #=============================================================================
  # 음수 부호가 디폴트 모델이므로, 해석에 주의
  # warning(paste0("Since polr uses the following logit ", "logit[P(Y <= j | X = x_i)] = a_j - bx_i", " the interpretation of the coefficiets should be careful!"))





  #=============================================================================
  # return results
  #=============================================================================
  return(Results)
}
