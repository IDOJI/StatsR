Classification___Logistic___Binomial___Penalty___Grouped = function(Logistic){
  #=============================================================================
  # Load the glmnet
  #=============================================================================
  install_packages(c("grpreg", "pROC"))





  #=============================================================================
  # CV glmnet
  #=============================================================================
  tictoc::tic()
  Logistic$cv_fit = cv.grpreg(X = Logistic$Train_X,
                              y = Logistic$Train_y %>% unlist,
                              group = Logistic$Grouped_Vars_Index,
                              penalty = Logistic$Fitting_Method,
                              family = "binomial")
  tictoc::toc()








  #=============================================================================
  # Best Model
  #=============================================================================
  # Fit the final model using the best lambda found in cross-validations
  Logistic$Best_Model = grpreg(X = Logistic$Train_X,
                               y = Logistic$Train_y %>% unlist,
                               group = Logistic$Grouped_Vars_Index,
                               penalty = Logistic$Fitting_Method,
                               lambda = Logistic$cv_fit$lambda.min, # Use the best lambda from CV
                               family = "binomial")







  #=============================================================================
  # plotting
  #=============================================================================
  # plot(cv_fit)
  # summary(Logistic$Best_Model)
  # plot(Logistic$Best_Model)






  #=============================================================================
  # Extract Results
  #=============================================================================
  Logistic = Classification___Logistic___Results(Logistic)





  return(Logistic)
}








