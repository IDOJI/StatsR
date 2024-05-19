# 🟥 fda fpca ##############################################################################################################################
fda___fpca = function(fdobj, threshold=0.9, path_export=NULL, file.name=NULL, score.name = "FPC", export_result=F, export_plot = T){
  tictoc::tic()

  # 🟧 fit length ########################################################################################
  fit_length = function(x.vec, fit.num){
    if(class(x.vec)=="numeric"){
      x.vec = as.character(x.vec)
    }

    New_x.vec = sapply(x.vec, function(y){
      if(nchar(y)>fit.num){
        stop("fit.num should larger!")
      }else{
        while(nchar(y) != fit.num){
          y = paste("0", y, collapse = "", sep = "")
        }
        return(y)
      }
    })

    return(New_x.vec)
  }





  # 🟧 path ##################################################################################################
  if(!is.null(path_export)){
    fs::dir_create(path_export, recurse = T)
  }





  # 🟧 Fitting & Exporting by Optimal nharm ####################################################################
  fpca_results = fda___fpca___optimal.nharm.by.threshold(fdobj, threshold, path_export, file.name, export_result)



  # 🟧 Exporting Plots ####################################################################
  if(export_plot){
    fda___fpca___plots(fpca_results, threshold, path_export, file.name)
  }




  # 🟧Scores ####################################################################
  n_col = fpca_results$scores %>% as.data.frame %>% ncol
  if(n_col < 10){
    fit_num = 2
  }else if(n_col < 100){
    fit_num = 3
  }
  FPC_Scores = fpca_results$scores %>% as.data.frame()
  names(FPC_Scores) = paste(score.name, fit_length(x.vec = 1:n_col, fit.num = fit_num), sep = "_")



  # 🟧Combined results#########################################################################################
  fpca_results = list(fpca_results = fpca_results, FPC_Scores = FPC_Scores)
  tictoc::toc()
  cat("\n", crayon::green("FPCA is done: "), crayon::bgMagenta(file.name),"\n")

  return(fpca_results)
}








# 🟥 fda fpca plots ##############################################################################################################################
fda___fpca___plots = function(fpca_results, threshold, path_export = NULL, file.name){
  # 🟧 Directory ==============================================================
  if(!is.null(path_export)){
    fs::dir_create(path_export, recurse = T)
  }






  # 🟧 Explained Var.prop plots ==============================================================
  nharm = fpca_results$scores %>% ncol
  cumulative_var = fpca_results$varprop %>% sum

  # png(filename = paste0(path_export, "/", file.name, "___nharm=", nharm, "___prop.var=", round(cumulative_var, 4),".png"))
  # plot(cumsum(fpca_results$values)/sum(fpca_results$values),
  #      xlab ='component',
  #      ylab ='cumulative variance explained (%)',
  #      main = paste0(file.name, "___ nharm = ", nharm, "___prop.var=", round(cumulative_var,4)),
  #      col=4,
  #      cex.lab=1.5,
  #      cex.axis=1.5,
  #      cex=2)
  # abline(h=threshold, col = "red")
  # dev.off()
  # cat("\n", crayon::green("Exporting the explained var.prop :"), crayon::red(file.name),"\n")

  library(ggplot2)

  # Assuming 'fpca_results$values' contains the eigenvalues of PCA components
  # and 'cumulative_var' and 'threshold' are previously defined
  data <- data.frame(
    component = 1:length(fpca_results$values),
    cum_var_explained = cumsum(fpca_results$values) / sum(fpca_results$values)
  )

  # Define the filename for saving the plot
  filename <- paste0(path_export, "/", file.name, "___nharm=", nharm, "___prop.var=", round(cumulative_var, 4), ".png")

  # Create the plot
  p <- ggplot(data, aes(x = component, y = cum_var_explained)) +
    geom_line(color = "blue", size = 1) +
    geom_hline(aes(yintercept = threshold), color = "red", linetype = "dashed") +
    labs(x = "Component",
         y = "Cumulative Variance Explained (%)",
         title = paste0(file.name, "___ nharm = ", nharm, "___prop.var=", round(cumulative_var, 4))) +
    theme_minimal() +
    theme(text = element_text(size = 16),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14))

  # Save the plot
  if(!is.null(path_export)){
    ggsave(filename, plot = p, width = 10, height = 8, dpi = 300)
  }


  # Print success message
  cat("\n", crayon::green("Exporting the explained var.prop:"), crayon::red(file.name), "\n")




  # 🟧 Varimax rotation ==============================================================
  # Varmax rotation
  # FDA_fPCA___Plotting = function(){
  #   daytemppcaobj <- pca.fd(daytempfd, nharm=4, harmfdPar)
  #   daytemppcaVarmx <- varmx.pca.fd(daytemppcaobj)
  #
  #
  # }
  return(p)
}









# 🟥 fda___fpca___optimal.nharm.by.threshold ##############################################################################################################################
fda___fpca___optimal.nharm.by.threshold = function(fdobj, threshold=0.9, path_export = NULL, file.name = NULL, export_result=F){
  # 🟧 Extract Sample ID ##########################################################
  ID = fdobj$coefs %>% colnames



  # 🟧 fPCA by threshold ##########################################################
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





  # 🟧 Add ID #####################################################################
  rownames(fPCA_results$scores) = ID





  # 🟧 Export rds data #####################################################################
  if(export_result){
    saveRDS(fPCA_results, file = paste0(path_export, "/", file.name, ".rds"))
    cat("\n", paste0(crayon::green("Exported RDS of fPCA results :"), crayon::red(file.name)),"\n")
  }




  # 🟧 Return #####################################################################
  return(fPCA_results)
}
