FDA___fPCA = function(fdobj, threshold=0.9, path_Export, file.name, export_result=F,export_plot = T){
  # 🟥 path ####################################################################
  fs::dir_create(path_Export, recurse = T)




  # 🟥 Fitting & Exporting by Optimal nharm ####################################################################
  fPCA_Results = FDA___fPCA___Optimal.nharm.by.Threshold(fdobj, threshold, path_Export, file.name, export_result)



  # 🟥 Exporting Plots ####################################################################
  if(export_plot){
    FDA___fPCA___Plots(fPCA_Results, threshold, path_Export, file.name)
  }


  # 🟥 Results ####################################################################
  fPCA_Results$scores


  return(fPCA_Results)
}













