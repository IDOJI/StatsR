Classification___Logistic___Results = function(Logistic){
  #=============================================================================
  # Extracting Results
  #=============================================================================
  Logistic = Classification___Logistic___Results___Coefficients(Logistic)








  #=============================================================================
  # Cumulative Probability plot for most effective variable
  #=============================================================================
  if(Logistic$Test_y %>% unlist %>% levels %>% length > 2){
    if(!is.null(Logistic$Plot_x_varname)){

      Logistic$Cumulative_plot = Classification___Logistic___Results___Cumulative.Probability.Plot(Logistic)

    }else{

      Logistic$Cumulative_plot = NULL

    }
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
























