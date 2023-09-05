Classification___Multinomial = function(X_Train,
                                        y_Train,
                                        X_Test = NULL,
                                        y_Test = NULL,
                                        y_varname = NULL,
                                        standardize = T,
                                        method = c("MLE", "Elastic"),
                                        response_type = c("Nominal", "Ordinal"),
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
  response_type_choices = tolower(response_type)

  response_type = match.arg(response_type_choices)

  if(!response_type %in% response_type_choices) {
    stop("Invalid reponse_type choice.")
  }






  #=============================================================================
  # Fitting
  #=============================================================================
  if(response_type == "nominal"){
    Results = Classification___Multinomial___Nominal(X_Train, y_Train, ...)
  }else if(response_type == "ordinal"){
    Results = Classification___Multinomial___Ordinal(X_Train, y_Train, ...)
  }




  #=============================================================================
  # Return
  #=============================================================================
  return(Results)
}









