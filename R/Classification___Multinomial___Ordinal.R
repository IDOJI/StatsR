Classification___Multinomial___Oridinal = function(X, y, method = "MLE"){
  
  
  
}

#===============================================================================
# Elastic net
#===============================================================================
Classification___Multinomial___Oridinal___Elastic.Net = function(X, 
                                                                 y, 
                                                                 family = "cumulative", 
                                                                 link = "logit",
                                                                 alpha = seq(0, 1, 0.1),
                                                                 n_fold = NULL,
                                                                 seed = 123,
                                                                 standardize=FALSE){
  #=============================================================================
  # install.packages
  #=============================================================================
  install_packages(packages = "ordinalNet", load = T)
  
  
  
  #=============================================================================
  # Data
  #=============================================================================
  X = as.matrix(X)
  
  
  
  
  #=============================================================================
  # Tuning Parameters by CV
  #=============================================================================
  base::RNGkind(sample.kind = "Rounding") %>% suppressWarnings()
  set.seed(seed)
  Results = lapply(alpha, function(ith_alpha, ...){
    tictoc::tic()
    ith_fit = ordinalNet::ordinalNetTune(x = X,
                                         y = y, 
                                         family = family,
                                         link = link,
                                         alpha = ith_alpha,
                                         lambdaMinRatio = 1e-04,
                                         nFolds = n_fold,
                                         printProgress = TRUE,
                                         warn = FALSE)
    tictoc::toc()  
    return(ith_fit)
  })
  
  
  
  
  
  
  
  
  
  # Fit parallel cumulative logit model; select lambda by cross validation
  tunefit <- ordinalNetTune(x, y)
  summary(tunefit)
  plot(tunefit)
  
  bestLambdaIndex <- which.max(rowMeans(tunefit$loglik))
  coef(tunefit$fit, whichLambda=bestLambdaIndex, matrix=TRUE)
  predict(tunefit$fit, whichLambda=bestLambdaIndex)
  
  
  
  
  
  
  
  
  
  
  #=============================================================================
  # Fitting
  #=============================================================================
  fit_1 = ordinalNet::ordinalNet(x = X, y = y, 
                                 family = family, 
                                 link = link,
                                 lambdaVals = 9.357452e-05,
                                 alpha = seq(0,1,0.01),
                                 parallelTerms = TRUE,
                                 nonparallelTerms = FALSE)
  
  
  
  
  
  
  
  
  
  #=============================================================================
  # Fitting CV
  #=============================================================================
  set.seed(seed)
  tictoc::tic()
  fit1_cv = ordinalNet::ordinalNetCV(x = X,
                                     y = y,
                                     family = family, 
                                     link = link,
                                     lambdaMinRatio = 1e-04,
                                     nFolds = n_fold,
                                     printProgress = TRUE,
                                     warn = FALSE)
  tictoc::toc()
  
  summary(fit1_cv)
  
  summary(fit1_cv)
  class(fit1_cv)
  coef(fit1_cv)
  print.ordinalNetCV(fit1_cv)
  
  summary(fit1_tune)
  
}

















