  FDA___CV = function(Demographics.df,
                    Curves.list,
                    Fold_Arguments = list(Data.df = Demographics.df,
                                          Var_1 = "Group",
                                          Train_K_Folds = 3,
                                          Return_Validation = TRUE,
                                          seed = 123),
                    Bspline.list = lapply(Domain_x.list, function(x){
                                    list(x = x,
                                         range_vals = c(min(x), max(x)),
                                         nbasis = NULL,
                                         norder = 4,
                                         breaks = x,
                                         lambdas = exp(seq(-5, -4, 0.1)),
                                         int2Lfd = 2,
                                         argvals = x,
                                         best_criterion = "gcv")
                                   }) %>% setNames(names(Curves.list)),
                    Fourier.list=NULL,
                    FPCA.list = NULL,
                    path_save=NULL){
  # 🟥 TMP #################################################################
  # Demographics.df = Sampled_Data$Demographics
  # Curves.list = Sampled_Data$FC_Curves
  # Fold_Arguments = list(Data.df = Demographics.df, Var_1 = "DIAGNOSIS_NEW", Train_K_Folds = 3, Return_Validation = TRUE, seed = seed)
  # FPCA.list = list(threshold = 0.9),




  # 🟥 Folding data #################################################################
  ## 🟨 Fold Index from Demographics =====================================================
  Folded_Data.list =  SUB___Fold(Data.df = Fold_Arguments$Data.df,
                                 Var_1 = Fold_Arguments$Var_1,
                                 Train_K_Folds = Fold_Arguments$Train_K_Folds,
                                 Return_Validation = Fold_Arguments$Return_Validation,
                                 seed = Fold_Arguments$seed)




  ## 🟨 Extract Train Curves =====================================================
  Train_Curves.list = lapply(seq_along(Curves.list), function(k){

    Curves.list[[k]][,Folded_Data.list$Combined_Train_Index] %>% as.data.frame

  }) %>% setNames(names(Curves.list))




  ## 🟨 Extract Test Curves =====================================================
  Test_Curves.list = lapply(seq_along(Curves.list), function(k){

    Curves.list[[k]][,Folded_Data.list$Index$Validation] %>% as.data.frame

  }) %>% setNames(names(Curves.list))




  # 🟥 Smoothing #################################################################
  if(!is.null(Bspline.list) && is.null(Fourier.list)){
    ## 🟨 Bspline ==================================================================== fix
    Smoothed_Train = FDA___CV___Smoothing___Bspline(Curves.list = Train_Curves.list,
                                                    Bspline.list = Bspline.list,
                                                    path_Export = path_Export)
    Smoothed_Test = FDA___CV___Smoothing___Bspline(Curves.list = Test_Curves.list,
                                                    Bspline.list = Bspline.list,
                                                    path_Export = path_Export)

  }else if(is.null(Bspline) && !is.null(Fourier.list)){
    ## 🟨 Fourier ====================================================================

    stop("Fourier cannot be used!")

  }else{

    stop("Either Bspline.list or Fourier list should be NULL!")

  }



  # 🟥 FPCA #################################################################
  if(!is.null(FPCA)){

    FPCA_Train = FDA___CV___FPCA(FPCA.list, Smoothed_Train, path_save)
    FPCA_Test = FDA___CV___FPCA(FPCA.list, Smoothed_Test, path_save)

  }else{

    FPCA_Train = NULL
    FPCA_Test = NULL

  }




  # 🟥 Return Results #################################################################
  Train_Result = list(Smoothed_Train = Smoothed_Train, FPCA_Train = FPCA_Train)
  Test_Result = list(Smoothed_Test = Smoothed_Test, FPCA_Test = FPCA_Test)
  list(Folded_Data = Folded_Data.list, Train_Result = Train_Result, Test_Result=Test_Result) %>% return
}
