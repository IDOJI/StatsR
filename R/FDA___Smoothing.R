FDA___Smoothing = function(Bslpline=NULL, Fourier=NULL, best.criterion = "gcv", path_Export=NULL){
  #=============================================================================
  # Bspline
  #=============================================================================
  if(!is.null(Bspline)){
    Results = FDA___Smoothing___Bspline(Bslpline, best.criterion, path_Export)



  #=============================================================================
  # Fourier
  #=============================================================================
  }else if(!is.null(Fourier)){
    Results = FDA___Smoothing___Fourier(Fourier, best.criterion, path_Export)
  }

  return(Results)
}
