FDA___Simulation___GenerateTrueCoefFunctions = function(Smoothed_Results.list,
                                                        num_p_nonzero,
                                                        path_save = NULL){
  #### ✅ Generate functions by Total Number of Functions ============================================================================
  # How many functions to generate
  Total_Num_Functions = tail(num_p_nonzero, 1)

  Braion_Regions = names(Smoothed_Results.list)

  Generated_Coef_Functions = list()
  Plot_Titles = list()

  for(k in 1:Total_Num_Functions){
    # smoothed results of kth region
    kth_Region_Smoothing = Smoothed_Results.list[[k]]$smoothing

    # Domain
    Domain = kth_Region_Smoothing$argvals %>% as.vector

    # Range
    Range = kth_Region_Smoothing$fd$basis$rangeval

    # Basis Expansion
    nbasis = k + 3  # nbasis 설정
    if(k %% 2 == 0){
      what_basis = "fourier"
      Basis = create.fourier.basis(rangeval = Range, nbasis = nbasis, period = (2 + k) * pi)
    } else {
      what_basis = "bspline"
      Basis = fda::create.bspline.basis(rangeval = Range, nbasis = nbasis)
    }


    # Functional Coef : length(coef) = nbassi of Basis
    set.seed(k)
    Generated_Coef_Functions[[k]] = fd_obj = fd(coef = matrix(runif(nbasis), nrow = nbasis), basisobj = Basis)

    # Saving plots
    Plot_Titles[[k]] = plot_title = paste0("Region___",Braion_Regions[k], "___k=", k , "___nbasis=", nbasis, "___Basis=", what_basis)
    if(!is.null(path_save)){
      path_Export = paste0(path_save, "/", "Regression Coefficient Functions")


      dir.create(path_Export, F)
      png(filename = paste0(path_Export, "/", sprintf("%02d", k), "th_Coefficient.png"), width = 800, height = 500, bg = "white")
      plot(fd_obj, main = plot_title)
      dev.off()
      cat("\n", crayon::green("Saving"),crayon::bgMagenta("regression coefficient function plots"),crayon::green("is done!"), "\n")
    }
  }



  names(Generated_Coef_Functions) = unlist(Plot_Titles)

  return(Generated_Coef_Functions)
}


# path_Folders = list.files(path_save, full.names=T)
#
# path_Smoothed = sapply(path_Folders, function(y){
#   list.files(y, pattern = "Smoothed FC Curves using Bspline.rds", full.names = T)
# }) %>% unname
#
# ith_Smoothed = readRDS(path_Smoothed[1])
#

#### ✅ Inner product with all zero fd ===========================================================================
# # 기저 함수 객체 가져오기
# basisobj <- mth_Smoothed_Result$smoothing$fd$basis
#
# # 계수를 0으로 하는 벡터 생성
# coefs_zero <- matrix(0, nrow = basisobj$nbasis, ncol = 1)
#
# # 모든 점에서 0의 값을 갖는 fd 객체 생성
# fd_zero <- fd(coefs_zero, basisobj)
#
# # fd_zero와 mth_Smoothed_Result$smoothing$fd 사이의 내적 계산
# inner_product <- inprod(kth_Region_Smoothing, mth_Smoothed_Result$smoothing$fd)

