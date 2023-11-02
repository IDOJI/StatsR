Classification___Logistic___Results___Predict = function(fit, X_Test, y_Test, x_varname, y_varname, AUC_in_Legend, path_Export){
  #===========================================================================
  # Prediction
  #===========================================================================
  # Predicted Probabilities
  newdata = X_Test %>% as_tibble %>% dplyr::select(all_of(x_varname))
  if(class(fit)=="ordinalNet"){
    Predicted_Probs = predict(fit, newx = X_Test %>% as.matrix, type = "response") # Ordinal_Elastic인 경우에는 잘 동작함.
  }else{
    Predicted_Probs = predict(fit, newdata = X_Test, type = "probs")
  }




  # Get the predicted classes from the probabilities
  Predicted_Classes = apply(Predicted_Probs, 1, which.max) %>% unname





  #===========================================================================
  # Compare the classification & Extract the misclassified subjects
  #===========================================================================
  y_Test_unlist = y_Test %>% unlist()
  y_Test_New = y_Test_unlist %>% as.integer()
  Index_Misclassified = which(Predicted_Classes != y_Test_New)
  Misclassified.df = cbind(Group = as.character(y_Test_unlist[Index_Misclassified]),
                           Predicted_Class = levels(y_Test)[Predicted_Classes[Index_Misclassified]],
                           X_Test[Index_Misclassified, ] %>% as.data.frame,
                           Index = Index_Misclassified)





  #===========================================================================
  # Confusion matrix
  #===========================================================================
  # Create a confusion matrix
  Confusion_matrix = table(Predicted = Predicted_Classes, Actual = y_Test_unlist)
  if(nrow(Confusion_matrix) != levels(y_Test_unlist) %>% length){
    rows_not_defined = unique(as.integer(y_Test_unlist))[! y_Test %>% as.integer %>% unique %in% rownames(Confusion_matrix)] %>% sort
    New_matrix = matrix(0, nrow = rows_not_defined %>% length, ncol = y_Test_unlist %>% unique %>% length)
    rownames(New_matrix) = rows_not_defined
    Combined_Matrix = rbind(New_matrix, Confusion_matrix)

    row_names = rownames(Combined_Matrix)
    Confusion_matrix = Combined_Matrix[row_names %>% as.numeric %>% order,]
  }
  rownames(Confusion_matrix) = levels(y_Test_unlist)

  # Calculate the number of correct predictions (diagonal of the confusion matrix)
  Correct_predictions = sum(diag(Confusion_matrix))

  # Calculate the total number of predictions (sum of all elements in the confusion matrix)
  Total_predictions = sum(Confusion_matrix)

  # Calculate the misclassification rate
  Misclassification_rate = 1 - (Correct_predictions / Total_predictions)








  #===========================================================================
  # ROAUC
  #===========================================================================
  ROAUC.list = Classification___Logistic___Results___Predict___AUROC(Predicted_Probs, y_Test_unlist, AUC_in_Legend, path_Export) %>% suppressWarnings()







  #===========================================================================
  # return
  #===========================================================================
  c(list(Confusion_Matrix = Confusion_matrix, Misclassification_Rate = Misclassification_rate, Misclassified_Subjects = Misclassified.df), ROAUC.list) %>% return()

}



























