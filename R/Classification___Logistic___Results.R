Classification___Logistic___Results = function(Logistic){
  #=============================================================================
  # Extracting Results
  #=============================================================================
  Logistic$Best_Model_Coef = Classification___Logistic___Results___Coefficients(Logistic$Best_Model)





  #=============================================================================
  # Cumulative Probability plot for most effective variable
  #=============================================================================
  if(!is.null(Logistic$Plot_x_varname)){
    Logistic$Cumulative_plot = Classification___Logistic___Results___Cumulative.Probability.Plot(Logistic)
  }else{
    Logistic$Cumulative_plot = NULL
  }






  #=============================================================================
  # Prediction
  #=============================================================================
  Logistic$Prediction = Classification___Logistic___Results___Predict(Logistic)





  #=============================================================================
  # Export results
  #=============================================================================
  Classification___Logistic___Results___Export(Logistic)





  #=============================================================================
  # Return
  #=============================================================================
  return(Logistic)
}
























