Classification___Logistic___Results = function(fit,
                                                  Best_alpha=NULL,
                                                  X_Test=NULL,
                                                  y_Test=NULL,
                                                  x_varname=NULL,
                                                  y_varname=NULL,
                                                  AUC_in_Legend=FALSE,
                                                  path_Export=NULL){
  #=============================================================================
  # Extracting Results
  #=============================================================================
  Fit_Coef = Classification___Logistic___Results___Coefficients(fit)




  #=============================================================================
  # Cumulative Probability plot for most effective variable
  #=============================================================================
  if(!is.null(x_varname)){
    Cumulative_plot = Classification___Logistic___Results___Cumulative.Probability.Plot(Fit_Coef, Data=X_Test, x_varname, path_Export)
  }else{
    Cumulative_plot = NULL
  }





  #=============================================================================
  # Prediction
  #=============================================================================
  if(!is.null(X_Test) && !is.null(y_Test)){
    Prediction = Classification___Logistic___Results___Predict(fit, X_Test, y_Test, x_varname, y_varname, AUC_in_Legend, path_Export)
  }else{
    Prediction = NULL
  }





  #=============================================================================
  # Combined Results
  #=============================================================================
  Combined.list = c(list(Fit=fit, Best_alpha = Best_alpha, Fit_Coef = Fit_Coef, Cumulative_plot = Cumulative_plot),
                    Prediction,
                    Cumulative_plot)








  #=============================================================================
  # Export results
  #=============================================================================
  Classification___Logistic___Results___Export(Combined.list, path_Export)






  #=============================================================================
  # Return
  #=============================================================================
  return(Combined.list)
}
























