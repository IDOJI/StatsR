Classification___Logistic___Multinomial___Nominal = function(X_Train,
                                                  y_Train,
                                                  X_Test = NULL,
                                                  y_Test = NULL,
                                                  standardize = T,
                                                  method = c("MLE", "Elastic"),
                                                  family = "cumulative",
                                                  link = "logit",
                                                  tuneMethod = "cvMisclass",
                                                  best.model.criterion = "misclass",
                                                  folds,
                                                  AUC_in_Legend = T,
                                                  title = "",
                                                  path_Export,
                                                  ...){
  #=============================================================================
  # arguments
  #=============================================================================
  response_type_choices = tolower(method)

  response_type = match.arg(response_type_choices)

  if(!response_type %in% response_type_choices) {
    stop("Invalid reponse_type choice.")
  }
  Results = Classification___Logistic___Multinomial___Nominal___MLE()
  #glmnet



  return(Results)

}
