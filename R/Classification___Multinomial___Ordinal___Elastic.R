Classification___Multinomial___Ordinal___Elastic = function (X_Train,
                                                            y_Train,
                                                            X_Test = NULL,
                                                            y_Test = NULL,
                                                            y_varname = NULL,
                                                            x_varname = NULL,
                                                            standardize = T,
                                                            #=======================================
                                                            penalty_alpha = NULL,
                                                            penalty_lambda = NULL,
                                                            family = "cumulative",
                                                            link = c("logit", "logistic", "probit", "loglog", "cloglog", "cauchit"),
                                                            tuneMethod = "cvMisclass",
                                                            best.model.criterion = "misclass",
                                                            Train_Folds_Index,
                                                            #=======================================
                                                            AUC_in_Legend = T,
                                                            path_Export){
  # Train_Folds_Index = Train_Fold_Index
  # penalty_alpha =
  #=============================================================================
  # install.packages
  #=============================================================================
  install_packages(packages = c("ordinalNet", "fs"), load = T)







  #=============================================================================
  # path
  #=============================================================================
  if(!is.null(path_Export)){
    fs::dir_create(path_Export, recurse=T)
  }




  #=============================================================================
  # Compare input
  #=============================================================================
  if(!is.null(X_Test) && ncol(X_Train)!=ncol(X_Test)){
    stop("Check input X_Test and X_Train. Its dimensions are different!")
  }




  #=============================================================================
  # Input
  #=============================================================================
  X_Train_New = apply(X_Train, 2, as.numeric) %>% as.matrix()







  #=============================================================================
  # fitting by CV
  #=============================================================================
  Fit_CV.list = lapply(penalty_alpha, function(ith_alpha, ...){
    tictoc::tic()
    # Fitting CV for ith_alpha
    ith_Fit_CV = ordinalNetCV(x = X_Train_New,
                              y = y_Train %>% unlist,
                              standardize = standardize,
                              family = family,
                              link = link,
                              tuneMethod = tuneMethod,
                              lambdaVals = penalty_lambda,
                              alpha = ith_alpha,
                              fold = Train_Folds_Index,
                              printProgress = TRUE,
                              warn = TRUE)
    saveRDS(ith_Fit_CV, file = paste0(path_Export, "/", "Fit_CV", "___", ith_alpha, ".rds"))
    tictoc::toc()

    # Averaging
    ith_Fit_CV_Mean = summary(ith_Fit_CV) %>% colMeans()
    return(ith_Fit_CV_Mean)
  })


  # Fit_CV.list = lapply(list.files(path_Export, full.names=T, pattern = "Fit_CV_"), function(y){
  #   readRDS(y) %>% summary %>% colMeans
  # })








  #=============================================================================
  # Select best parameters
  #=============================================================================
  # best.model.criterion = match.arg(best.model.criterion)
  if(best.model.criterion %in% c("misclass", "brier")){
    which_best = which.min
  }else{
    which_best = which.max
  }


  Combined_Criteria = sapply(Fit_CV.list, function(ith_Fit_CV){
    ith_Fit_CV[names(ith_Fit_CV)==best.model.criterion]
  })
  best_ind = which_best(Combined_Criteria)
  best_alpha = penalty_alpha[best_ind]
  # best_lambda = Fit_CV.list[[best_ind]][1]









  #=============================================================================
  # Fit again with best parameters
  #=============================================================================
  Best_Fit = ordinalNet(x = X_Train %>% as.matrix,
                        y = y_Train %>% unlist,
                        alpha = best_alpha,
                        standardize = standardize,
                        family = family,
                        link = link)

  Best_Fit_Summary = summary(Best_Fit)
  if(which.min(Best_Fit_Summary$aic) == which.min(Best_Fit_Summary$bic)){
    best_lambda = Best_Fit$lambdaVals[which.min(Best_Fit_Summary$aic)]
  }else{
    stop("Best indice of AIC and BIC are different!")
  }

  # Fit again using best parameters
  Best_Fit_Final = ordinalNet(x = X_Train %>% as.matrix,
                              y = y_Train %>% unlist,
                              alpha = best_alpha,
                              lambdaVals = best_lambda,
                              standardize = standardize,
                              family = family,
                              link = link)


  # Best_Fit_Final = Fitting_3$Best_Fit
  # best_alpha = Fitting_3$Best_alpha





  #=============================================================================
  # Extract results and prediction & Exporting
  #=============================================================================
  Results.list = Classification___Multinomial___Results(fit = Best_Fit_Final,
                                                        Best_alpha = best_alpha,
                                                        X_Test = X_Test,
                                                        y_Test = y_Test,
                                                        x_varname,
                                                        y_varname,
                                                        AUC_in_Legend = AUC_in_Legend,
                                                        path_Export = path_Export)




  cat("\n", crayon::green("Congratulation! The fitting is done!"),"\n")
  return(Results.list)
}





