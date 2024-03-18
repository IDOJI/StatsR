FDA___CV___Smoothing___Bspline = function(Curves.list,
                                          Bspline.list,
                                          path_Export){
  # 🟥 Check arguments #############################################################################
  ## 🟨 length of list ==========================================================================
  if(length(Curves.list)!=length(Bspline.list)){
    stop("The length between Curves.list and Bspline.list is different!")
  }





  # 🟥 Smoothing ##################################################################################
  tictoc::tic()
  Smoothed_Curves.list = lapply(seq_along(Curves.list), function(k){

    FDA___Smoothing(Bspline = c(list(y = kth_Curves), Bspline.list[[k]]),
                    path_Export = paste0(path_Export , "/Smoothed FC Curves using Bspline"),
                    file.name = paste0(fit_length(k, 3), "_", names(Curves.list)[k]),
                    save_rds = F,
                    save_plot = F)

    cat("\n", crayon::green("Smoothing signals from "), crayon::bgRed(names(Curves.list)[k]), crayon::green("is done!"),"\n")

  }) %>% setNames(Brain_Regions)
  tictoc::toc()

  cat("\n", crayon::green("Smoothing"), crayon::green("is done!"),"\n")


  return(Smoothed_Data.list)
}

