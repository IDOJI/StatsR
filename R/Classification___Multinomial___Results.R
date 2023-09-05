Classification___Multinomial___Results = function(Best_Fit, Best_alpha=NULL, X_Test=NULL, y_Test=NULL, AUC_in_Legend=FALSE, title=NULL, path_Export=NULL){
  #=============================================================================
  # Extracting Results
  #=============================================================================
  # summary
  Fit_Summary = summary(Best_Fit)


  # Coefficients
  Fit_Coef = coef(Best_Fit, matrix=TRUE)



  #=============================================================================
  # Cumulative Probability plot for most effective variable
  #=============================================================================
  Cum_p = Classification___Multinomial___Results___Cumulative.Probability.Plot(Best_Fit, X_Test, y_Test)





  #=============================================================================
  # Prediction
  #=============================================================================
  if(!is.null(X_Test) && !is.null(y_Test)){
    Prediction = Classification___Multinomial___Results___Predict(fit = Best_Fit, X_Test, y_Test, AUC_in_Legend, title, path_Export)
  }else{
    Prediction = NULL
  }






  #=============================================================================
  # Combined Results
  #=============================================================================
  Combined.list = c(list(Fit_Summary = Fit_Summary, Best_alpha = Best_alpha, Fit_Coef = Fit_Coef), Prediction)





  #=============================================================================
  # Return
  #=============================================================================
  return(Combined.list)


}
