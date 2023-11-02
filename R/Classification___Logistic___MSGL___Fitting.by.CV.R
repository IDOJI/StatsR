Classification___Multinomial___MSGL___Fitting.by.CV = function(X,
                                                               y,
                                                               features.grouping,
                                                               n_fold,
                                                               alpha_seq,
                                                               lambda.min,
                                                               d,
                                                               lambda.min.rel,
                                                               sparse.data,
                                                               standardize,
                                                               parallel.cores,
                                                               path_Export){
  #=============================================================================
  # Data-derived lambdas
  #=============================================================================
  lambda_seq = msgl::lambda(x = as.matrix(X),
                            classes = y,
                            sampleWeights = NULL,
                            grouping = features.grouping,
                            groupWeights = NULL,
                            parameterWeights = NULL,
                            alpha = alpha,
                            d = d,
                            standardize,
                            lambda.min,
                            intercept = TRUE,
                            sparse.data = sparse.data,
                            lambda.min.rel = lambda.min.rel,
                            algorithm.config = msgl::msgl.standard.config)








  #=========================================================================
  # Model fitting
  #=========================================================================
  # packages
  install_packages(c("parallel", "msgl"))

  # parallel starts
  cl = makeCluster(parallel.cores) ; registerDoParallel(cl)

  # Fitting models
  All_Fit_CV = lapply(alpha_seq, function(ith_alpha, ...){
    # < fitting by cv >
    # information about models
    # and cross validation errors (estimated expected generalization error)
    tictoc::tic()
    ith_fit.cv = msgl::cv(x = X,
                          classes = y,
                          grouping = features.grouping,
                          fold = n_fold,
                          alpha = ith_alpha,
                          lambda = lambda_seq,
                          sparse.data = sparse.data,
                          use_parallel = TRUE)

    # Exporting
    saveRDS(ith_fit.cv, file=paste0(path_Export, "/Fitted_CV", "___", ith_alpha, ".rds"))


    tictoc::toc()
    return(ith_fit.cv)
  })

  # parallel stop
  stopCluster(cl)







  #=========================================================================
  # Returning
  #=========================================================================
  return(All_Fit_CV)
}
