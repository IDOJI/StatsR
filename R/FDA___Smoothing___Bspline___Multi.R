FDA___Smoothing___Bspline___Multi = function(data.df,
                                             rangeval,
                                             norder,
                                             breaks,
                                             lambdas,
                                             argvals){
  # Create a B-spline basis object pbasis with order 4 (cubic) and breakpoints ranging from 0 to 150.
  pbasis = create.bspline.basis(rangeval = rangeval, norder = norder, breaks = breaks)

  # argvals vector has the same length as the number of rows in Signals_AD.list[[1]].

  # Define a range of lambda candidates for the smoothing parameter.
  lambdas = lambdas

  # Initialize an array gcvs to store the Generalized Cross-Validation (GCV) values for each lambda candidate.
  gcvs = rep(0,length(lambdas)) #initialization

  # Iterate over each lambda candidate, perform the smoothing, and compute the GCV value.
  tictoc::tic()
  for(i in 1:length(lambdas)){
    pPar = fdPar(pbasis, int2Lfd(2), lambdas[i])
    gcvs[i] = mean(smooth.basis(argvals, data.df %>% as.matrix, pPar)$gcv)

    # gcvs[i] = try(mean(smooth.basis(rangeval, data.df %>% as.matrix, pPar)$gcv), silent = T)
    # 20 gcv values will be calculaed for  the 20 replications.
    # So, we average them to get a summary measurement.
  }
  tictoc::toc()

  best = which.min(gcvs)

  # Identify the best lambda (the one with the smallest GCV value) and assign it to lambda.
  lambda = lambdas[best]


  # Perform the final smoothing with the best lambda and create a functional data object pfd.
  pPar = fdPar(fdobj = pbasis, Lfdobj = int2Lfd(2), lambda = lambda)
  pfd = smooth.basis(argvals = argvals, y = data.df %>% as.matrix, fdParobj = pPar)

  # plot(pfd)
  return(list(pfd, lambda=lambda))
}
