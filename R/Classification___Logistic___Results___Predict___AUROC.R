Classification___Logistic___Results___Predict___AUROC = function(Predicted_Probs, Logistic){
  #=============================================================================
  # pacakges
  #=============================================================================
  install_packages(c("pROC", "ggplot2", "dplyr", "ROCR", "caTools"), load = TRUE)




  #=============================================================================
  # Computing ROC
  #=============================================================================
  # Categories
  Categories = levels(Logistic$Test_y %>% unlist)
  if(length(Categories) > 2){
    Extracted_ROC.list = Classification___Logistic___Results___Predict___AUROC___Multi(Predicted_Probs, Logistic$Test_y %>% unlist, Categories)

    if(!is.null(path_Export)){
      ggsave(filename = paste0(path_Export, "/ROC_plot.png"), plot = Extracted_ROC.list$p, width = 10, height = 8, dpi = 300, bg  = "white")
    }

  }else{

    Extracted_ROC.list = Classification___Logistic___Results___Predict___AUROC___Binary(Predicted_Probs, Logistic, Categories)

  }



  #=============================================================================
  # Returning results
  #=============================================================================
  Extracted_ROC.list %>% return()


}
























