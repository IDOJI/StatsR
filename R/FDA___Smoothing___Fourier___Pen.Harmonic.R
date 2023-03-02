FDA___Smoothing___Fourier___Pen.Harmonic = function(y, x=NULL, nbasis=NULL, lambda){
  if(is.null(x)){
    x = 1:length(y)
  }
  if(is.null(nbasis)){
    nbasis = length(y)
  }


  # three coeff required for the linear diff operator obj
  Lcoef = c(0, (2*pi/diff(c(min(x), max(x))))^2, 0)


  # to use the harmonic accelerated penalty
  # vec2Lfd is used.
  # convert Lcoef to the vector to the linear diff operator obj
  harmonic_Lfd = vec2Lfd(Lcoef, rangeval = c(min(x), max(x)))


  # Basis functions
  F_basis = create.fourier.basis(rangeval = c(min(x), max(x)), nbasis = nbasis)


  # Evaluation
  F_eval = eval.basis(evalarg  = x, basisobj = F_basis)


  # construct a functional paramter obj with a roughness penalty
  # fdPar(basis obj, a derivative order m or a diff operator L to be penalized, smoothing paramter)
  # -> OCV/GCV를 이용해서 최적의 람다값을 찾는다.
  FD_Par_Obj = fdPar(fdobj = F_basis, # basis functions
                     Lfdobj = harmonic_Lfd, # what penalty
                     lambda = lambda) # tuning parameter

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
  list(lambda = lambda, DF=FD_Smooth$df, GCV=FD_Smooth$gcv, SSE=FD_Smooth$SSE, OCV=OCV, y.hat = Hat.mat %*% y, y = y, FD_Smooth=FD_Smooth) %>% return
}












