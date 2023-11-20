Classification___Logistic___Results___Predict___AUROC___Binary = function(Predicted_Probs, Logistic, Categories){
  library(pROC)

  # Assuming you have a data frame `data` with predictor variables and a binary outcome `response`
  # Fit the logistic model
  # Calculate the AUC
  roc_obj <- roc(Logistic$Test_y %>% unlist %>% unname, Predicted_Probs)
  auc_value <- auc(roc_obj)


  # ROC 커브와 AUC 값 출력
  fs::dir_create(Logistic$path_Export)
  png(filename = paste0(Logistic$path_Export, "/AUC.png"))
  plot(roc_obj, main = paste("ROC curve (AUC = ", auc_value, ")", sep = ""))
  dev.off()



  return(auc_value)
}
