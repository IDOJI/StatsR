FDA___Smoothing___Bspline___Multiple.Functions___Fitting.Each.Element___Single.Data.Frame = function(data.df,
                                                                             rangeval,
                                                                             norder,
                                                                             breaks,
                                                                             lambdas,
                                                                             argvals,
                                                                             Lfdobj = int2Lfd(2),
                                                                             path_Export,
                                                                             filename_prefix){
  # Create a B-spline basis object pbasis with order 4 (cubic) and breakpoints ranging from 0 to 150.
  pbasis = fda::create.bspline.basis(rangeval = rangeval, norder = norder, breaks = breaks)



  # argvals vector has the same length as the number of rows in Signals_AD.list[[1]].



  # Define a range of lambda candidates for the smoothing parameter.
  lambdas = lambdas





  #=============================================================================
  # Find gcv
  #=============================================================================
  # Initialize an array gcvs to store the Generalized Cross-Validation (GCV) values for each lambda candidate.
  # Iterate over each lambda candidate, perform the smoothing, and compute the GCV value.
  gcvs = sapply(lambdas, function(ith_lambda, ...){
    # tictoc::tic()
    pPar = fdPar(fdobj = pbasis, Lfdobj = Lfdobj, lambda = ith_lambda)
    mean_gcv = mean(smooth.basis(argvals = argvals, y = as.matrix(data.df), fdParobj = pPar)$gcv)
    # gcvs[i] = try(mean(smooth.basis(rangeval, data.df %>% as.matrix, pPar)$gcv), silent = T)
    # 20 gcv values will be calculaed for  the 20 replications.
    # So, we average them to get a summary measurement.
    # tictoc::toc()
    return(mean_gcv)
  })




  #=============================================================================
  # Best GCV & lambda
  #=============================================================================
  # Identify the best lambda (the one with the smallest GCV value) and assign it to lambda.
  which_best = which.min(gcvs)
  best_lambda = lambdas[which_best]
  best_gcv = gcvs[which_best]
  main = paste0(filename_prefix, "___","Best_gcv=", round(best_gcv, 4),  "___Best_Log_Lambda=", log(best_lambda),".png")
  main_path = paste0(filename_prefix, "/","Best_gcv=", round(best_gcv, 4),  "___Best_Log_Lambda=", log(best_lambda),".png")
  dir.create(paste0(path_Export, filename_prefix), F)

  png(filename = paste0(path_Export, main_path))
  plot(gcvs, main = main, ylab = "gcv")
  abline(v = which_best, col="red")
  dev.off()






  # Perform the final smoothing with the best lambda and create a functional data object pfd.
  pPar = fdPar(fdobj = pbasis, Lfdobj = int2Lfd(2), lambda = best_lambda)
  pfd = smooth.basis(argvals = argvals, y = as.matrix(data.df), fdParobj = pPar)

  # plot(pfd)
  return(list(fdSmooth = pfd, lambda = best_lambda))
}
