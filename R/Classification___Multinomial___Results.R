Classification___Multinomial___Results = function(fit,
                                                  Best_alpha=NULL,
                                                  X_Test=NULL,
                                                  y_Test=NULL,
                                                  x_varname=NULL,
                                                  y_varname=NULL,
                                                  AUC_in_Legend=FALSE,
                                                  title=NULL,
                                                  path_Export=NULL){
  #=============================================================================
  # Extracting Results
  #=============================================================================
  # summary
  Fit_Summary = summary(fit)


  # Coefficients
  Fit_Coef = coef(fit, matrix=TRUE)
  if(!is.null(fit$zeta)){
    Fit_Coef = c(fit$zeta, Fit_Coef)
    Fit_Coef = data.frame(Coef = names(Fit_Coef), value = Fit_Coef)
  }







  #=============================================================================
  # Cumulative Probability plot for most effective variable
  #=============================================================================
  if(!is.null(x_varname)){
    Cumulative_plot = Classification___Multinomial___Results___Cumulative.Probability.Plot(fit, Data=X_Test, x_varname, title_cum.plot = "", path_Export)
  }else{
    Cumulative_plot = NULL
  }







  #=============================================================================
  # Prediction
  #=============================================================================
  if(!is.null(X_Test) && !is.null(y_Test)){
    Prediction = Classification___Multinomial___Results___Predict(fit, X_Test, y_Test, x_varname, y_varname, AUC_in_Legend, title, path_Export)
  }else{
    Prediction = NULL
  }





  #=============================================================================
  # Combined Results
  #=============================================================================
  Combined.list = c(list(Fit=fit, Fit_Summary = Fit_Summary, Best_alpha = Best_alpha, Fit_Coef = Fit_Coef, Cumulative_plot = Cumulative_plot),
                    Prediction,
                    Cumulative_plot)








  #=============================================================================
  # Export results
  #=============================================================================
  Classification___Multinomial___Results___Export(Combined.list, path_Export)






  #=============================================================================
  # Return
  #=============================================================================
  return(Combined.list)
}
























