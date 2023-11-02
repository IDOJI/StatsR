FDA___Smoothing = function(Bspline=NULL, Fourier=NULL, best.criterion = "gcv", path_Export=NULL, file.name=NULL){
  #=============================================================================
  # Bspline
  #=============================================================================
  if(!is.null(Bspline)){
    tictoc::tic()
    Results = FDA___Smoothing___Bspline(Bspline, best.criterion, path_Export, file.name)
    tictoc::toc()

  #=============================================================================
  # Fourier
  #=============================================================================
  }else if(!is.null(Fourier)){

    Results = FDA___Smoothing___Fourier(Fourier, best.criterion, path_Export, file.name)

  }

  return(Results)
}
