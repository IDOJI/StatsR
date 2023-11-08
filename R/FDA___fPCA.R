FDA___fPCA = function(fdobj, threshold=0.9, path_Export, file.name){
  #=============================================================================
  # path
  #=============================================================================
  fs::dir_create(path_Export, recurse = T)




  #=============================================================================
  # Fitting & Exporting by Optimal nharm
  #=============================================================================
  fPCA_Results = FDA___fPCA___Optimal.nharm.by.Threshold(fdobj, threshold, path_Export, file.name)




  #=============================================================================
  # Exporting Plots
  #=============================================================================
  FDA___fPCA___Plots(fPCA_Results, threshold, path_Export, file.name)



  return(fPCA_Results)
}













