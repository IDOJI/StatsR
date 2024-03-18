FDA___CV___FPCA = function(FPCA.list, Smoothed_Result, path_save){

  tictoc::tic()
  FPCA_Results = lapply(seq_along(Smoothed_Train), function(k){


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


  return(FPCA_Results)

}
