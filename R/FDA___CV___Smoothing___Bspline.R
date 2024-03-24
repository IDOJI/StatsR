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
  # Extract Brain regions
  Curves_Names = names(Curves.list)

  # Smoothing
  Smoothed_Curves.list = lapply(seq_along(Curves.list), function(k){

    kth_region_name = names(Curves_Names)[k]
    kth_Curves = Curves.list[[k]]
    kth_Bspline_Setting = Bspline.list[[k]]
    kth_Bspline_Setting$
    test = FDA___Smoothing(Bspline = list(y = kth_curves[,-1],
                                   x = kth_Bspline_Setting$x,
                                   range_vals = c(min(kth_x), max(kth_x)),
                                   nbasis = NULL,
                                   norder = 4,
                                   breaks = kth_x,
                                   labmdas =  ,
                                   m_int2Lfd = 2,
                                   argvals = kth_x),
                    best.criterion = ,
                    path_Export = paste0(path_Export , "/Smoothed FC Curves using Bspline"),
                    file.name = paste0(fit_length(k, 3), "_", kth_region_name),
                    save_rds = F,
                    save_plot = F)

  }) %>% setNames(Brain_Regions)

  tictoc::toc()

  cat("\n", crayon::green("Exporting"), crayon::bgRed("Smoothed Data"), crayon::green("is done!"),"\n")
  return(Smoothed_Data.list)
}

