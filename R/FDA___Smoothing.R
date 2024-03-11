FDA___Smoothing = function(Bspline=NULL, Fourier=NULL, best.criterion = "gcv", path_Export=NULL, file.name=NULL){
  # 🟥 path ##########################################################################
  fs::dir_create(path_Export, recurse = T)






  # 🟥 Smoothing ##########################################################################
  ## 🟨 Bspline ==========================================================================
  if(!is.null(Bspline)){
    tictoc::tic()
    Results = FDA___Smoothing___Bspline(Bspline, best.criterion, path_Export, file.name)
    tictoc::toc()



  ## 🟨 Fourier ==========================================================================
  }else if(!is.null(Fourier)){
    tictoc::tic()
    Results = FDA___Smoothing___Fourier(Fourier, best.criterion, path_Export, file.name)
    tictoc::toc()
  }

  return(Results)
}
