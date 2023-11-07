Classification___Logistic = function(Logistic){
  #=============================================================================
  # Arguments Setting
  #=============================================================================
  # Logistic = list(#----------------------------------------
  #                 # Data Setting
  #                 #----------------------------------------
  #                 Train_X = Train_X,
  #                 Train_y = Train_y,
  #                 Test_X = Test_X,
  #                 Test_y = Test_y,
  #                 Train_Folds_Index.vec = Train_Folds_Index.vec,
  #                 Train_Folds_Index.list = Train_Folds_Index.list,
  #                 Standardize = TRUE,
  #                 #----------------------------------------
  #                 # Modeling Fitting
  #                 #----------------------------------------
  #                 # Method
  #                 Response_Type = c("Nominal", "Ordinal"),
  #                 Fitting_Method = c("MLE", "ElasticNet"), #
  #                 # Model
  #                 Family = c(#Classification___Logistic___Ordinal___Elastic___NonGroupedPenalty
  #                            "cumulative", "sratio", "cratio", "acat",
  #                            #Classification___Logistic___Nominal___Elastic___NonGroupedPenalty
  #                            "gaussian", "binomial", "poisson", "multinomial", "cox", "mgaussian"),
  #                 Link = c(#Classification___Logistic___Ordinal___Elastic___NonGroupedPenalty
  #                          "logit", "probit", "cloglog", "cauchit"),
  #                 # Penalty
  #                 penalty_alpha = seq(0, 1, 0.01),
  #                 penalty_lambda = exp(seq(-2,2,0.01)),
  #                 #----------------------------------------
  #                 # Tuning measures
  #                 #----------------------------------------
  #                 Tune_Method = c(#Classification___Logistic___Ordinal___Elastic___NonGroupedPenalty
  #                                 "cvLoglik", "cvMisclass", "cvBrier", "cvDevPct", "aic", "bic",
  #                                 #Classification___Logistic___Nominal___Elastic___NonGroupedPenalty
  #                                 "default", "mse", "deviance", "class", "auc", "mae", "C"),
  #                 # Best_Model_Criterion = c(#Classification___Logistic___Ordinal___Elastic___NonGroupedPenalty
  #                 #                          "cvLoglik", "cvMisclass", "cvBrier", "cvDevPct", "aic", "bic"),
  #                 #----------------------------------------
  #                 # Grouping variables
  #                 #----------------------------------------
  #                 Grouped_Vars_Index = NULL, # NULL이 아니면 그룹 정보를 사용, 그룹 위치 벡터를 넣어야 함.
  #                 #----------------------------------------
  #                 # Plotting
  #                 #----------------------------------------
  #                 Plot_y_varname = NULL, # proportional logit plot은 하나의 변수만 가능하므로 한 변수 지정
  #                 Plot_x_varname = NULL, # 지정하지 않으면 plot 안 그려짐
  #                 AUC_in_Legend = TRUE,
  #                 #----------------------------------------
  #                 # Export Results
  #                 #----------------------------------------
  #                 path_Export = NULL)
  # Logistic$Plot_x_varname = "pregnant"
  # Logistic$Plot_y_varname = "age group"
  # Logistic$Family = "cumulative"
  # Logistic$Link = "logit"
  # Logistic$Tune_Method = "cvMisclass"
  # Logistic$Fitting_Method = "ElasticNet"





  #=============================================================================
  # Generated path
  #=============================================================================
  if(!is.null(Logistic$path_Export)){
    dir.create(Logistic$path_Export, FALSE)
  }







  #=============================================================================
  # Check the data
  #=============================================================================
  # only numeric input
  Logistic$Train_X = apply(Logistic$Train_X, 2, as.numeric) %>% dplyr::as_tibble()
  Logistic$Test_X = apply(Logistic$Test_X, 2, as.numeric) %>% dplyr::as_tibble()


  # standardization for Penalization
  if(match.arg(tolower(Logistic$Fitting_Method), c("elasticnet")) %in% c("elasticnet") || Logistic$Standardize){
    Logistic$Train_X = Logistic$Train_X %>% mutate_if(is.numeric, scale)
    Logistic$Test_X = Logistic$Test_X %>% mutate_if(is.numeric, scale)
  }


  # Compare input dimension
  if(!is.null(Logistic$Train_X) && ncol(Logistic$Train_X)!=ncol(Logistic$Test_X)){
    stop("Check input Test_X and Train_X. Its ncols are different!")
  }


  # as.matrix
  Logistic$Train_X = Logistic$Train_X %>% as.matrix
  Logistic$Test_X = Logistic$Test_X %>% as.matrix






  #=============================================================================
  # Fitting Models
  #=============================================================================
  Response_Type = Logistic$Response_Type
  Reponse_Type_Choices = c("Nominal", "Ordinal") %>% tolower
  Response_Type = match.arg(tolower(Response_Type), Reponse_Type_Choices)

  if(Response_Type == "nominal"){

    Results = Classification___Logistic___Nominal(Logistic)

  }else if(response_type == "ordinal"){

    Results = Classification___Logistic___Ordinal(Logistic)

  }







  #=============================================================================
  # Return
  #=============================================================================
  cat("\n", crayon::green("Fitting a Logistic model is done!"), "\n")
  return(Results)
}







