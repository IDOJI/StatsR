# Bspline = list(y = y,
#                x = x,
#                range_vals = NULL,
#                nbasis = NULL,
#                norder = NULL,
#                breaks = NULL,
#                labmdas = NULL,
#                best_criterion = "gcv",
#                m_int2Lfd = NULL,
#                argvals = NULL)
FDA___Smoothing = function(Bspline=NULL,
                           Fourier=NULL,
                           path_Export=NULL,
                           file.name=NULL,
                           save_rds=T,
                           save_plot=T){
  # 🟥 path ##########################################################################
  fs::dir_create(path_Export, recurse = T)






  # 🟥 Smoothing ##########################################################################
  if(!is.null(Bspline)){
    ## 🟨 Bspline ==========================================================================
    tictoc::tic()
    Results = FDA___Smoothing___Bspline(Bspline, path_Export, file.name, save_rds, save_plot)
    tictoc::toc()




  }else if(!is.null(Fourier)){
    ## 🟨 Fourier ==========================================================================
    tictoc::tic()
    Results = FDA___Smoothing___Fourier(Fourier, path_Export, file.name, save_rds, save_plot)
    tictoc::toc()
  }

  return(Results)
}
