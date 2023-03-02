FDA___Smoothing___Fourier___Pen.Second = function(y, x=NULL, nbasis=NULL, lambda){
  # int2Lfd(2) : the second derivative information about basis function
  # int2Lfd(2) is used when the second derivative is used for penalty
  # the lambda is selected randomly

  if(is.null(x)){
    x = 1:length(y)
  }
  if(is.null(nbasis)){
    nbasis = length(y)
  }

  # Basis functions
  F_basis = create.fourier.basis(rangeval = c(min(x), max(x)), nbasis = nbasis)


  # Evaluation
  F_eval = eval.basis(evalarg  = x, basisobj = F_basis)


  # fd Paramerte Object
  FD_Par_Obj = fdPar(fdobj  = F_basis,
                     Lfdobj = int2Lfd(2),
                     lambda = lambda)


  # Construct a functional data object by smoothing data using a roughness penalty
  FD_Smooth = smooth.basis(argvals  = x,
                           y        = y,
                           fdParobj = FD_Par_Obj)


  # y2cMap: the matrix mapping the data to the coefficients.
  Hat.mat = F_eval %*% FD_Smooth$y2cMap

  # # plotting
  # plot(y)
  # lines(x = x, y = Hat.mat %*% y, col = "red")

  # eval.fd : Values of a Functional Data Object
  OCV = mean( (y-eval.fd(x, FD_Smooth$fd))^2 / (1-diag(Hat.mat))^2 )

  # results
  # results
  list(lambda = lambda, DF=FD_Smooth$df, GCV=FD_Smooth$gcv, SSE=FD_Smooth$SSE, OCV=OCV, y.hat = Hat.mat %*% y, y = y, FD_Smooth=FD_Smooth) %>% return
}
