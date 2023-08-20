Classification___Multinomial___Results___Predict = function(fit, X_Test, y_Test, AUC_in_Legend, title, path_Export){
  #===========================================================================
  # Prediction
  #===========================================================================
  # Predicted Probabilities
  Predicted_Prob = predict(Best_Fit, newx = X_Test, type = "response")

  # Convert predicted probabilities to predicted classes
  Predicted_classes = apply(Predicted_Prob, 1, which.max)









  #===========================================================================
  # Confusion matrix
  #===========================================================================
  # Create a confusion matrix
  Confusion_matrix = table(Predicted = Predicted_classes, Actual = y_Test)
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
  c(list(Confusion_Matrix = Confusion_matrix, Misclassification_Rate = Misclassification_rate), ROAUC.list) %>% return()



}



























