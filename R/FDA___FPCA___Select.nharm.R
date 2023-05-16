FDA___FPCA___Select.nharm = function(pca.fd.obj, threshold = 0.9, save.path, file.name){
  # pca.fd.obj = FPCA_AD.list[[1]]
  #=============================================================================
  # Finding optimum by threshold
  #=============================================================================
  cum_proportion = cumsum(pca.fd.obj$values)/sum(pca.fd.obj $values)
  selected_nharm = which_cut_by_threshold = which(cum_proportion >= threshold)[1]



  #=============================================================================
  # Save plotting cumulative variance plot
  #=============================================================================
  save.path = save.path %>% path_tail_slash()
  dir.create(save.path, showWarnings = F)
  png(filename = paste0(save.path, file.name, "_selected_nharm=", selected_nharm, ".png"))
  plot(cum_proportion,
       xlab='component',
       ylab='cumulative variance explained',
       main = paste0(file.name, "_", "Threshold : ", threshold, " ", "Selected_Nharm : ", selected_nharm),
       col=4,
       cex.lab=1.5,
       cex.axis=1.5,
       cex=2)
  abline(h=threshold)
  abline(v=selected_nharm, col="red")
  dev.off()


  return(selected_nharm)
}
