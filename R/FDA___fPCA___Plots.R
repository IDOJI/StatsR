FDA___fPCA___Plots = function(fPCA_Results, threshold, path_Export, file.name){
  #===========================================================================
  # Directory
  #===========================================================================
  dir.create(path_Export, F)



  #===========================================================================
  # Explained Var.prop plots
  #===========================================================================
  nharm = fPCA_Results$scores %>% ncol
  cumulative_var = fPCA_Results$varprop %>% sum

  png(filename = paste0(path_Export, "/", file.name, "___nharm=", nharm, "___prop.var=", round(cumulative_var, 4),".png"))
  plot(cumsum(fPCA_Results$values)/sum(fPCA_Results$values),
       xlab ='component',
       ylab ='cumulative variance explained (%)',
       main = paste0(file.name, "___ nharm = ", nharm, "___prop.var=", round(cumulative_var,4)),
       col=4,
       cex.lab=1.5,
       cex.axis=1.5,
       cex=2)
  abline(h=threshold, col = "red")
  dev.off()
  cat("\n", crayon::green("Exporting the explained var.prop :"), crayon::red(file.name),"\n")




  #===========================================================================
  # Varimax rotation
  #===========================================================================
  # Varmax rotation
  # FDA_fPCA___Plotting = function(){
  #   daytemppcaobj <- pca.fd(daytempfd, nharm=4, harmfdPar)
  #   daytemppcaVarmx <- varmx.pca.fd(daytemppcaobj)
  #
  #
  # }


}
