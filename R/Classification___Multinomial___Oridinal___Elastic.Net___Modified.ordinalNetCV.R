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
                                                                  alpha_seq = seq(0, 1, 0.01),
                                                                  lambdaVals = NULL,
                                                                  printProgress = TRUE,
                                                                  warn = TRUE,
                                                                  #=============================== Prediction & AUC
                                                                  AUC_in_Legend = FALSE,
                                                                  path_Export = NULL){
  # folds = Train_Fold_Index
  # alpha_seq =
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
    ith_Fit_CV = ordinalNetCV(x = x,
                              y = y,
                              standardize = standardize,
                              family = family,
                              link = link,
                              tuneMethod = tuneMethod,
                              lambdaVals = lambdaVals,
                              alpha = ith_alpha,
                              folds = folds,
                              printProgress = printProgress,
                              warn = warn)
    tictoc::toc()

    # Averaging
    ith_Fit_CV_Mean = summary(ith_Fit_CV) %>% colMeans()
    return(ith_Fit_CV_Mean)
  })







  #=============================================================================
  # Select best parameters
  #=============================================================================
  best.model.criterion = match.arg(best.model.criterion)

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
  best_lambda = Fit_CV.list[[best_ind]][1]








  #=============================================================================
  # Fit again with best parameters
  #=============================================================================
  Best_Fit = ordinalNet(x = x,
                        y = y,
                        alpha = best_alpha,
                        standardize = standardize,
                        family = family,
                        link = link,
                        lambdaVals = best_lambda)





  #=============================================================================
  # Extract results and prediction
  #=============================================================================
  Resulst.list = Classification___Multinomial___Results(Best_Fit, X_Test, y_Test, AUC_in_Legend, path_Export)






}










