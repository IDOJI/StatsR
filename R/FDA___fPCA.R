FDA___fPCA = function(fdobj, threshold=0.9, path_Export, file.name, score.name = "FPC",export_result=F,export_plot = T){
  tictoc::tic()
  # 🟥 path ####################################################################
  fs::dir_create(path_Export, recurse = T)




  # 🟥 Fitting & Exporting by Optimal nharm ####################################################################
  fPCA_Results = FDA___fPCA___Optimal.nharm.by.Threshold(fdobj, threshold, path_Export, file.name, export_result)



  # 🟥 Exporting Plots ####################################################################
  if(export_plot){
    FDA___fPCA___Plots(fPCA_Results, threshold, path_Export, file.name)
  }




  # 🟥Scores ####################################################################
  n_col = fPCA_Results$scores %>% as.data.frame %>% ncol
  if(n_col < 10){
    n_length = 2
  }else if(n_col < 100){
    n_length = 3
  }
  FPC_Scores = fPCA_Results$scores %>% as.data.frame()
  names(FPC_Scores) = paste(score.name, fit_length(x.vec = 1:n_col, fit.num = n_length), sep = "_")



  # 🟥Combined results####################################################################
  FPCA_Results = list(fPCA_Results = fPCA_Results, FPC_Scores = FPC_Scores)
  tictoc::toc()
  cat("\n", crayon::green("FPCA is done: "), crayon::bgMagenta(file.name),"\n")

  return(FPCA_Results)
}













