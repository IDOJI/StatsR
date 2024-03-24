FDA___CV___FPCA = function(FPCA.list, Smoothed_Train, Smoothed_Test, path_save){
  # 🟥 Train #########################################################################
  tictoc::tic()
  FPCA_Train = lapply(seq_along(Smoothed_Train), function(k){


    FDA___fPCA(fdobj = Smoothed_Train[[k]]$smoothing$fd,
               threshold = FPCA.list$threshold,
               path_Export = paste0(path_save, "/", save_folder, "/FPCA"),
               file.name = names(Smoothed_Train)[k],
               score.name = paste0("FPC_", names(Smoothed_Train)[k]),
               export_result = F,
               export_plot = F)


  }) %>% setNames(Smoothed_Train)
  cat("\n", crayon::green("FPCA is done:"),"\n")
  tictoc::toc()





  # 🟥 Test #########################################################################
  # Extracting scores from Train
  FPCA_Scores = mapply(Extract_fPCA_Scores_with_GroupNums,
                       FPCA = FPCA_Train.list,
                       File.Name = Names_Smoothed_Files,
                       path_Export = path_FPCA,
                       SIMPLIFY = FALSE)








  ## 🟨 Exporting fPCA scores for Test ==================================================================================
  for(i in seq_along(FPCA_Train.list)){
    tictoc::tic()
    ith_FPCA_Train = FPCA_Train.list[[i]]
    ith_Smoothing_Test = Smoothing_Test.list[[i]]

    ith_BrainRegion = names(ith_FPCA_Train)

    ith_Scores_Group_Num = c()

    # Computer inner product for each Brain Region
    ith_Scores_Test = lapply(seq_along(ith_FPCA_Train), function(j){
      # FPCA results from Train
      ijth_Region_FCPA_Train = ith_FPCA_Train[[j]]

      # centering before eigenvalue decomposition
      ijth_Region_Smoothing_FD_Centered_Test = center.fd(ith_Smoothing_Test[[j]]$smoothing$fd)

      ijth_Scores_Test = fda::inprod(fdobj1 = ijth_Region_Smoothing_FD_Centered_Test,
                                     fdobj2 = ijth_Region_FCPA_Train$harmonics)

      colnames(ijth_Scores_Test) = paste0(ith_BrainRegion[j], "___",  1:ncol(ijth_Scores_Test))
      rownames(ijth_Scores_Test) = colnames(ijth_Region_Smoothing_Test$smoothing$y)
      ith_Scores_Group_Num <<- c(ith_Scores_Group_Num, rep(j, times = ncol(ijth_Scores_Test)))

      return(ijth_Scores_Test)
    }) %>% setNames(names(ith_FPCA_Train)) %>% do.call(cbind, .) %>% as.data.frame()


    saveRDS(list(Scores = ith_Scores_Test, Features_Group_Nums = ith_Scores_Group_Num),
            file = paste0(path_FPCA, "/Scores___", Names_Smoothed_Files[i], "___Test.rds"))
    tictoc::toc()
  }









  # 🟥 Extrac Scores #########################################################################
  Extract_fPCA_Scores_with_GroupNums = function(FPCA, path_Export, File.Name){
    # Export directory
    fs::dir_create(path_Export, recurse = T)

    # Group lasso를 위한 numbering
    FPCA_Scores_GroupNum = c()

    # Extract FPCA scores
    FPCA_Scores = lapply(seq_along(FPCA), function(i){

      ith_Region = FPCA[[i]]

      ith_PC_Scores = ith_Region$scores %>% as.data.frame

      names(ith_PC_Scores) = paste0(names(FPCA)[i], "___", 1:ncol(ith_PC_Scores))

      FPCA_Scores_GroupNum <<- c(FPCA_Scores_GroupNum, rep(i, times = ncol(ith_PC_Scores)))

      return(ith_PC_Scores)
    })

    FPCA_Scores = do.call(cbind, FPCA_Scores)

    FPCA_Combined = list(fPCA_Scores = FPCA_Scores, Features_Group_Nums = FPCA_Scores_GroupNum)

    saveRDS(FPCA_Combined, file = paste0(path_Export, "/Scores___", File.Name, "___Train.rds"))

  }


  return(FPCA_Results)

}
