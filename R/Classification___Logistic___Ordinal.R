Classification___Logistic___Ordinal = function(X_Train,
                                                  y_Train,
                                                  X_Test=NULL,
                                                  y_Test=NULL,
                                                  y_varname=NULL,
                                                  x_varname=NULL,
                                                  standardize=TRUE,
                                                  #=======================================
                                                  fitting.method = c("MLE", "Elastic"),
                                                  penalty_alpha = NULL,
                                                  penalty_lambda = NULL,
                                                  family = "cumulative",
                                                  link = "logistic",
                                                  tuneMethod = "cvMisclass",
                                                  best.model.criterion = "misclass",
                                                  Train_Folds_Index,
                                                  #=======================================
                                                  AUC_in_Legend = T,
                                                  path_Export=NULL) {
  #=============================================================================
  # arguments
  #=============================================================================
  fitting.method_choices = c("MLE", "Elastic") %>% tolower()
  fitting.method = match.arg(tolower(fitting.method), fitting.method_choices)





  #=============================================================================
  # Analysis by method
  #=============================================================================
  # Code for MLE method
  if(fitting.method == "mle"){
    Results = Classification___Logistic___Ordinal___MLE(X_Train, y_Train,
                                                           X_Test, y_Test,
                                                           y_varname, x_varname,
                                                           link, AUC_in_Legend, path_Export)


    # Code for Elastic method
  }else if(fitting.method == "elastic"){
    Results = Classification___Logistic___Ordinal___Elastic(X_Train,
                                                               y_Train,
                                                               X_Test,
                                                               y_Test,
                                                               y_varname,
                                                               x_varname,
                                                               standardize,
                                                               penalty_alpha,
                                                               penalty_lambda,
                                                               family,
                                                               link,
                                                               tuneMethod,
                                                               best.model.criterion,
                                                               Train_Folds_Index,
                                                               AUC_in_Legend,
                                                               path_Export)

  }




  #=============================================================================
  # Return
  #=============================================================================
  return(Results)
}



















