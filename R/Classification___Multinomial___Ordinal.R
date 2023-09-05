Classification___Multinomial___Ordinal = function(X_Train,
                                                  y_Train,
                                                  X_Test = NULL,
                                                  y_Test = NULL,
                                                  y_varname = NULL,
                                                  standardize = T,
                                                  method = c("MLE", "Elastic"),
                                                  penatly_alpha = NULL,
                                                  penalty_lambda = NULL,
                                                  family = "cumulative",
                                                  link = c("logit", "logistic", "probit", "loglog", "cloglog", "cauchit"),
                                                  tuneMethod = "cvMisclass",
                                                  best.model.criterion = "misclass",
                                                  folds,
                                                  AUC_in_Legend = T,
                                                  title = "",
                                                  path_Export,
                                                  ...) {
    #=============================================================================
  # arguments
  #=============================================================================
  # Convert choices to lowercase for case-insensitive comparison
  method_choices = tolower(method)

  method = match.arg(method_choices)

  if(!method %in% method_choices) {
    stop("Invalid method choice.")
  }





  #=============================================================================
  # Analysis by method
  #=============================================================================
  # Code for MLE method
  if(method == "mle"){
    Results = Classification___Multinomial___Ordinal___MLE(X_Train, y_Train, ...)



  # Code for Elastic method
  }else if(method == "elastic"){
    Results = Classification___Multinomial___Ordinal___Elastic(X_Train, y_Train, ...)
  }





  #=============================================================================
  # Return
  #=============================================================================
  return(Results)
}













