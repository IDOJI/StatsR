FDA___fPCA___Optimal.nharm.by.Threshold = function(fdobj, threshold=0.9, path_Export, file.name){
  #=============================================================================
  # Extract Sample ID
  #=============================================================================
  ID = fdobj$coefs %>% colnames






  #=============================================================================
  # fPCA by threshold
  #=============================================================================
  # init values
  nharm = 0
  cumulative_var = 0

  while(cumulative_var < threshold){
    # increase nharm
    nharm = nharm + 1

    # pca.fd
    fPCA_results = pca.fd(fdobj = fdobj, nharm = nharm)

    # varprop
    cumulative_var = fPCA_results$varprop %>% sum
  }





  #=============================================================================
  # Add ID
  #=============================================================================
  rownames(fPCA_results$scores) = ID





  #=============================================================================
  # Export rds data
  #=============================================================================
  saveRDS(fPCA_results, file = paste0(path_Export, "/", file.name, ".rds"))
  cat("\n", paste0(crayon::green("Exported RDS of fPCA results :"), crayon::red(file.name)),"\n")







  #=============================================================================
  # Return
  #=============================================================================
  return(fPCA_results)
}





