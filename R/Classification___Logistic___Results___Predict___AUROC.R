Classification___Logistic___Results___Predict___AUROC = function(Predicted_Probs, y_Test_unlist, AUC_in_Legend = FALSE, path_Export=NULL){
  #=============================================================================
  # pacakges
  #=============================================================================
  install_packages(c("pROC", "ggplot2", "dplyr"), load = TRUE)




  #=============================================================================
  # Computing ROC
  #=============================================================================
  # Categories
  Categories = levels(y_Test_unlist)
  if(length(Categories) > 2){
    Extracted_ROC.list = Classification___Logistic___Results___Predict___AUROC___Multi(Predicted_Probs, y_Test_unlist, Categories)
  }else{
    Extracted_ROC.list = Classification___Logistic___Results___Predict___AUROC___Binary(Predicted_Probs, y_Test_unlist, Categories)
  }






  #=============================================================================
  # Export plot
  #=============================================================================
  if(!is.null(path_Export)){
    ggsave(filename = paste0(path_Export, "/ROC_plot.png"), plot = Extracted_ROC.list$p, width = 10, height = 8, dpi = 300, bg  = "white")
  }





  #=============================================================================
  # Returning results
  #=============================================================================
  Extracted_ROC.list %>% return()


}
























