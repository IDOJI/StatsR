FDA___Smoothing___Bspline___RS.fMRI.Signals = function(Signals.list, initial_lambdas = exp(seq(-3,3,1)), path_Export){
  path_Export = path_Export %>% path_tail_slash()
  #=============================================================================
  # Initial lambdas
  #=============================================================================
  smoothed_1.list = lapply(Signals.list, FUN=function(ith_ROI, ...){
    # gcv와 튜닝 파라미터를 이용해서 추정
    FDA___Smoothing___Bspline___Multi(data.df = ith_ROI,
                                      rangeval = c(1, nrow(ith_ROI)),
                                      norder = 4,
                                      breaks = seq(1, nrow(ith_ROI), by = 3),
                                      lambdas = initial_lambdas,
                                      argvals = 1:nrow(ith_ROI)) %>% suppressWarnings()
  })




  #=============================================================================
  # Second lambdas
  #=============================================================================
  smoothed_2.list = list()
  for(i in 1:length(Signals.list)){
    ith_ROI = Signals.list[[i]]
    ith_log_lambda = log(smoothed_1.list[[i]][[2]])
    smoothed_2.list[[i]] = FDA___Smoothing___Bspline___Multi(data.df = ith_ROI,
                                                             rangeval = c(1,nrow(ith_ROI)),
                                                             norder = 4,
                                                             breaks = seq(1, nrow(ith_ROI), by = 3),
                                                             lambdas = exp(seq(ith_log_lambda-3, ith_log_lambda+3, 0.1)),
                                                             argvals = 1:nrow(ith_ROI)) %>% suppressWarnings()
  }






  #=============================================================================
  # Third lambdas
  #=============================================================================
  smoothed_3.list = list()
  for(i in 1:length(Signals.list)){
    ith_ROI = Signals.list[[i]]
    ith_log_lambda = log(smoothed_2.list[[i]][[2]])
    smoothed_3.list[[i]] = FDA___Smoothing___Bspline___Multi(data.df = ith_ROI,
                                                             rangeval = c(1,nrow(ith_ROI)),
                                                             norder = 4,
                                                             breaks = seq(1, nrow(ith_ROI), by = 3),
                                                             lambdas = exp(seq(ith_log_lambda-0.5, ith_log_lambda+0.5, 0.01)),
                                                             argvals = 1:nrow(ith_ROI)) %>% suppressWarnings()
  }






  #=============================================================================
  # Exporting png
  #=============================================================================
  for(i in 1:length(smoothed_3.list)){
    png(filename = paste0(path_Export, "ROI_", fit_length(i, 3), "_log_lambda=", log(smoothed_3.list[[i]][[2]]), ".png"))
    plot(smoothed_3.list[[i]][[1]])
    dev.off()
  }



  return(smoothed_3.list)
}
