FDA___Smoothing___Fourier___RS.fMRI___Each.Subject = function(Signals.list , lambdas, nbasis){
  Smoothed_Signals = lapply(Signals.list, FUN=function(ith_Signals, ...){
    ith_Smoothed_Signals = apply(ith_Signals[1:2], MARGIN=2, FUN=function(kth_ROI, ...){
      kth_ROI_Smoothing_results.list = FDA___Smoothing___Fourier___Total(y = kth_ROI,
                                                                 n_basis = nbasis,
                                                                 lambdas = lambdas,
                                                                 penalty.type = "harmonic",
                                                                 criterion = "OCV")
      return(kth_ROI_Smoothing_results.list)
    })
    names(ith_Smoothed_Signals) = paste0("ROI_", fit_length(1:length(ith_Smoothed_Signals), 3))
    # y = ith_Signals_AD[,2]
    # plot(y)
    # lines(x = 1:length(y), y = ith_Smoothing_AD[[2]]$y.hat, col="red")
    # log(Smoothing_results.list$lambda)
    return(ith_Smoothed_Signals)
  })
  names(Smoothed_Signals) = names(Signals.list)
  return(Smoothed_Signals)
}
