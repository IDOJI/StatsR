FDA_CV = function(Demographics.df,
                  Curves.list,
                  Fold_Arguments = list(Data.df = Demographics.df, Var_1 = "Group", Train_K_Folds = 3, Return_Validation = TRUE, seed = 123),
                  Bspline.list=NULL,
                  Fourier.list=NULL,
                  path_save=NULL){
  # 🟥 TMP #################################################################
  # Demographics.df = Sampled_Data$Demographics
  # Curves.list = Sampled_Data$FC_Curves
  # Fold_Arguments = list(Data.df = Demographics.df, Var_1 = "DIAGNOSIS_NEW", Train_K_Folds = 3, Return_Validation = TRUE, seed = seed)






  # 🟥 Folding data #################################################################
  # Demographics
  Folded_Data.list =  SUB___Fold(Data.df = Fold_Arguments$Data.df,
                                 Var_1 = Fold_Arguments$Var_1,
                                 Train_K_Folds = Fold_Arguments$Train_K_Folds,
                                 Return_Validation = Fold_Arguments$Return_Validation,
                                 seed = Fold_Arguments$seed)



  # Curves
  Train_Curves.list = list()
  Test_Curves.list = list()

  for(k in seq_along(Curves.list)){

    Train_Curves.list[[k]] = Curves.list[[k]][,Folded_Data.list$Combined_Train_Index]

    Test_Curves.list[[k]] = Curves.list[[k]][,Folded_Data.list$Index$Validation]

  }

  names(Train_Curves.list) = names(Curves.list)
  names(Test_Curves.list) = names(Curves.list)
  Test_Curves.list %>% names




  # 🟥 Smoothing #################################################################
  FDA_CV___SmoothingByBspline = function(Curves.list, Bspline.list, path_Export){
    if(length(Curves.list)!=length(Bspline.list)){
      stop("The length between Curves.list and Bspline.list is different!")
    }

    tictoc::tic()
    # Extract Brain regions
    Curves_Names = names(Curves.list)

    # Smoothing
    Smoothed_Curves.list = lapply(seq_along(Curves.list), function(k){

      kth_Curves = Curves.list[[k]]
      kth_Bspline_Setting = Bspline.list[[k]]



      FDA___Smoothing(Bspline = list(y = kth_curves[,-1],
                                     x = kth_Bspline_Setting$x,
                                     range_vals = c(min(kth_x), max(kth_x)),
                                     nbasis = NULL,
                                     norder = 4,
                                     breaks = kth_x,
                                     labmdas =  ,
                                     m_int2Lfd = 2,
                                     argvals = kth_x),
                      best.criterion = "gcv",
                      path_Export = paste0(path_Export , "/Smoothed FC Curves using Bspline"),
                      file.name = paste0(fit_length(k, 3), "_", Brain_Regions[k]),
                      save_rds = F,
                      save_plot = F)

    }) %>% setNames(Brain_Regions)

    tictoc::toc()

    cat("\n", crayon::green("Exporting"), crayon::bgRed("Smoothed Data"), crayon::green("is done!"),"\n")
    return(Smoothed_Data.list)
  }


  Smoothed_Results = Smoothing_by_Bspline(Sampled_Data, path_save)









  # 🟥 Bspline #################################################################
  ## 🟧 Smoothing using Bspline ====================================================================



  Smoothing_by_Bspline = function(Sampled_Data, path_save){
    # ✅ Load path of the sampled data list --------------------------------------------------------
    # Folders = list.files(path_save, full.names = T)
    # path_Sampled_Data_List = sapply(Folders, function(y){
    #   list.files(y, full.names=T, pattern = "Sampled Data.rds")
    # }) %>% unname




    # ✅ Smoothing --------------------------------------------------------
    tictoc::tic()
    # Extract FC curves only
    FC_Curves = Sampled_Data$FC_Curves

    # Extract Brain regions
    Brain_Regions = names(FC_Curves)

    # Setting path
    path_Export = paste0(path_save, "/", Sampled_Data$save_folder_name)


    Smoothed_Data.list = lapply(seq_along(FC_Curves), function(k){

      kth_Region = FC_Curves[[k]]

      kth_x = kth_Region[,1]

      FDA___Smoothing(Bspline = list(y = kth_Region[,-1],
                                     x = kth_x,
                                     range_vals = c(min(kth_x), max(kth_x)),
                                     nbasis = NULL,
                                     norder = 4,
                                     breaks = kth_x,
                                     labmdas =  exp(seq(-5, -4, 0.1)),
                                     m_int2Lfd = 2,
                                     argvals = kth_x),
                      best.criterion = "gcv",
                      path_Export = paste0(path_Export , "/Smoothed FC Curves using Bspline"),
                      file.name = paste0(fit_length(k, 3), "_", Brain_Regions[k]),
                      save_rds = F,
                      save_plot = F)

    }) %>% setNames(Brain_Regions)

    tictoc::toc()

    cat("\n", crayon::green("Exporting"), crayon::bgRed("Smoothed Data"), crayon::green("is done!"),"\n")
    return(Smoothed_Data.list)
  }







  # 🟥 FPCA #################################################################




}
