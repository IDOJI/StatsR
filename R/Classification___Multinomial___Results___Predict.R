Classification___Multinomial___Results___Predict = function(fit, X_Test, y_Test, AUC_in_Legend, title, path_Export){
  #===========================================================================
  # Prediction
  #===========================================================================
  # Predicted Probabilities
  # Predicted_Prob = predict(fit, newx = X_Test, type = "response") # Ordinal_Elastic인 경우에는 잘 동작함.
  Predicted_Prob = predict(fit, newdata = X_Test, type = "probs")
  class(fit)
  dim(Predicted_Prob)

  # Convert predicted probabilities to predicted classes
  Predicted_classes = apply(Predicted_Prob, 1, which.max) %>% unname




  #===========================================================================
  # Compare the classification & Extract the misclassified subjects
  #===========================================================================
  y_Test_New = as.integer(y_Test)
  Index_Misclassified = which(Predicted_classes != y_Test_New)
  Misclassified.df = cbind(Group = as.character(y_Test[Index_Misclassified]), X_Test[Index_Misclassified, ] %>% as.data.frame)






  #===========================================================================
  # Confusion matrix
  #===========================================================================
  # Create a confusion matrix
  Confusion_matrix = table(Predicted = Predicted_classes, Actual = y_Test)
  if(nrow(Confusion_matrix) != levels(y_Test) %>% length){
    rows_not_defined = unique(as.integer(y_Test))[! y_Test %>% as.integer %>% unique %in% rownames(Confusion_matrix)] %>% sort
    New_matrix = matrix(0, nrow = rows_not_defined %>% length, ncol = y_Test %>% unique %>% length)
    rownames(New_matrix) = rows_not_defined
    Combined_Matrix = rbind(New_matrix, Confusion_matrix)

    row_names = rownames(Combined_Matrix)
    Confusion_matrix = Combined_Matrix[row_names %>% as.numeric %>% order,]
  }
  rownames(Confusion_matrix) = levels(y_Test)

  # Calculate the number of correct predictions (diagonal of the confusion matrix)
  Correct_predictions = sum(diag(Confusion_matrix))

  # Calculate the total number of predictions (sum of all elements in the confusion matrix)
  Total_predictions = sum(Confusion_matrix)

  # Calculate the misclassification rate
  Misclassification_rate = 1 - (Correct_predictions / Total_predictions)








  #===========================================================================
  # ROAUC
  #===========================================================================
  ROAUC.list = Classification___Multinomial___Results___Predict___AUROC(fit, X_Test, y_Test, AUC_in_Legend, title, path_Export)







  #===========================================================================
  # return
  #===========================================================================
  c(list(Confusion_Matrix = Confusion_matrix, Misclassification_Rate = Misclassification_rate, Misclassified_Subjects = Misclassified.df), ROAUC.list) %>% return()

}



























