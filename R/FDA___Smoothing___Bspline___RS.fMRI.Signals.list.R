FDA___Smoothing___Bspline___RS.fMRI.Signals.list = function(Signals.list,
                                                            knots_length.out = NULL,
                                                            norders = 12:20,
                                                            lambdas = exp(seq(-3, 3, 1)),
                                                            penalty.type = "second",
                                                            file.name_prefix = "AD",
                                                            save.path = paste0(which_OS, "/Dropbox/Github/Rpkgs/Papers/figures/Functional_Data_Analysis"),
                                                            save.each = T){
  if(is.null(knots_length.out)){
    knots_length.out = 45:floor((length(Signals.list[[1]][,1])/3))
  }


  Smoothed_ROI_Signals.list = list()
  for(i in 1:length(Signals.list)){
    ith_ROI_Signals.df = Signals.list[[i]]
    ith_Smoothed_ROI_Signals.list = list()
    for(j in 1:ncol(ith_ROI_Signals.df)){
      # tictoc::tic("sleeping")         # 시작
      # print("falling asleep...")
      ith_Smoothed_ROI_Signals.list[[j]] = FDA___Smoothing___Bspline___Total(y = ith_ROI_Signals.df[,j],
                                                                             knots_length.out,
                                                                             norders,
                                                                             lambdas,
                                                                             penalty.type,
                                                                             file.name_prefix = paste0(file.name_prefix, "_",
                                                                                                       names(Signals.list)[i], "_",
                                                                                                       names(ith_ROI_Signals.df)[j]),
                                                                             save.path = save.path)
      # print("...waking up")
      # tictoc::toc()                    # 종료
      if(save.each){

      }
    }
    names(ith_Smoothed_ROI_Signals.list) = names(ith_ROI_Signals.df)
    Smoothed_ROI_Signals.list[[i]] = ith_Smoothed_ROI_Signals.list
  }
  names(Smoothed_ROI_Signals.list) = names(Signals.list)
  return(Smoothed_ROI_Signals.list)
}
