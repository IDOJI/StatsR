Classification___Multinomial___Oridinal___Elastic.Net = function (X_Train,
                                                                  y_Train,
                                                                  X_Test=NULL,
                                                                  y_Test=NULL,
                                                                  standardize = TRUE,
                                                                  #=============================== Modeling
                                                                  family = c("cumulative", "sratio", "cratio", "acat"),
                                                                  link = c("logit", "probit", "cloglog", "cauchit"),
                                                                  tuneMethod = c("cvMisclass", "cvLoglik", "cvBrier", "cvDevPct", "aic", "bic"),
                                                                  best.model.criterion = c("misclass", "brier", "loglik", "devPct"),
                                                                  folds = NULL,
                                                                  alpha_seq = seq(0, 1, 0.1),
                                                                  lambdaVals = NULL,
                                                                  printProgress = TRUE,
                                                                  warn = TRUE,
                                                                  #=============================== Prediction & AUC
                                                                  AUC_in_Legend = FALSE,
                                                                  title = NULL,
                                                                  path_Export = NULL){
  # folds = Train_Fold_Index
  # alpha_seq =
  #=============================================================================
  # path
  #=============================================================================
  if(!is.null(path_Export)){
    dir.create(path_Export, FALSE)
  }




  #=============================================================================
  # Compare input
  #=============================================================================
  if(!is.null(X_Test) && ncol(X_Train)!=ncol(X_Test)){
    stop("Check input X_Test and X_Train. Its dimensions are different!")
  }





  #=============================================================================
  # install.packages
  #=============================================================================
  install_packages(packages = "ordinalNet", load = T)









  #=============================================================================
  # fitting by CV
  #=============================================================================
  Fit_CV.list = lapply(alpha_seq, function(ith_alpha, ...){
    tictoc::tic()
    # Fitting CV for ith_alpha
    ith_Fit_CV = ordinalNetCV(x = X_Train,
                              y = y_Train,
                              standardize = standardize,
                              family = family,
                              link = link,
                              tuneMethod = tuneMethod,
                              lambdaVals = lambdaVals,
                              alpha = ith_alpha,
                              folds = folds,
                              printProgress = printProgress,
                              warn = warn)
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
  best_alpha = alpha_seq[best_ind]
  # best_lambda = Fit_CV.list[[best_ind]][1]







  #=============================================================================
  # Fit again with best parameters
  #=============================================================================
  Best_Fit = ordinalNet(x = X_Train,
                        y = y_Train,
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


  Best_Fit_Final = ordinalNet(x = X_Train,
                              y = y_Train,
                              alpha = best_alpha,
                              lambdaVals = best_lambda,
                              standardize = standardize,
                              family = family,
                              link = link)









  #=============================================================================
  # Extract results and prediction
  #=============================================================================
  Results.list = Classification___Multinomial___Results(Best_Fit = Best_Fit_Final,
                                                        Best_alpha = best_alpha,
                                                        X_Test = X_Test %>% as.matrix,
                                                        y_Test = y_Test,
                                                        AUC_in_Legend = AUC_in_Legend,
                                                        title = title,
                                                        path_Export = path_Export)

  Final_Results.list = c(list(Best_Fit = Best_Fit_Final), Results.list)
  if(!is.null(path_Export)){
    saveRDS(Final_Results.list, file=paste0(path_Export, "/Best_Model_Fitting_Results.RDS"))
  }


  cat("\n", crayon::green("Congratulation! The fitting is done!"),"\n")
  return(Final_Results.list)
}





