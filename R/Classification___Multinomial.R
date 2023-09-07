Classification___Multinomial = function(X_Train,
                                        y_Train,
                                        X_Test = NULL,
                                        y_Test = NULL,
                                        y_varname = NULL,
                                        x_varname = NULL,
                                        standardize = T,
                                        #=======================================
                                        fitting.method = c("MLE", "Elastic"),
                                        penatly_alpha=NULL,
                                        penalty_lambda=NULL,
                                        response_type = c("Nominal", "Ordinal"),
                                        family = "cumulative",
                                        link = "logit",
                                        tuneMethod = "cvMisclass",
                                        best.model.criterion = "misclass",
                                        folds,
                                        #=======================================
                                        AUC_in_Legend = T,
                                        title = "",
                                        path_Export=NULL,
                                        ...){
  #=============================================================================
  # arguments
  #=============================================================================
  reponse_type_choices = c("Nominal", "Ordinal") %>% tolower
  response_type = match.arg(tolower(response_type), reponse_type_choices)






  #=============================================================================
  # path
  #=============================================================================
  if(!is.null(path_Export)){
    dir.create(path_Export, F)
  }






  #=============================================================================
  # standardize data
  #=============================================================================
  if(standardize & fitting.method%in%c("elastic", "Elastic")){
    X_Train = X_Train %>% mutate_all(scale)
    X_Test = X_Test %>% mutate_all(scale)
  }





  #=============================================================================
  # Fitting
  #=============================================================================
  if(response_type == "nominal"){

    Results = Classification___Multinomial___Nominal(X_Train, y_Train)



  }else if(response_type == "ordinal"){

    Results = Classification___Multinomial___Ordinal(X_Train,
                                                     y_Train,
                                                     X_Test,
                                                     y_Test,
                                                     y_varname,
                                                     x_varname,
                                                     standardize,#
                                                     fitting.method,
                                                     penatly_alpha,
                                                     penalty_lambda,
                                                     family,
                                                     link,
                                                     tuneMethod,
                                                     best.model.criterion,
                                                     folds,#
                                                     AUC_in_Legend,
                                                     title,
                                                     path_Export)
  }





  #=============================================================================
  # Return
  #=============================================================================
  cat("\n", crayon::green("Fitting a model is done!"), "\n")
  return(Results)


}





























