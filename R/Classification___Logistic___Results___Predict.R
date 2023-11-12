Classification___Logistic___Results___Predict = function(Logistic){
  #===========================================================================
  # Arguments
  #===========================================================================
  fit = Logistic$Best_Model
  Test_X = Logistic$Test_X
  Test_y = Logistic$Test_y
  x_varname = Logistic$Plot_x_varname
  y_varname = Logistic$Plot_y_varname
  AUC_in_Legend = Logistic$AUC_in_Legend
  path_Export = Logistic$path_Export







  #===========================================================================
  # Prediction
  #===========================================================================
  # Predicted Probabilities
  newdata = Test_X %>% as_tibble %>% dplyr::select(all_of(x_varname))
  if(class(fit)=="ordinalNet"){
    Predicted_Probs = predict(fit, newx = Test_X %>% as.matrix, type = "response") # Ordinal_Elastic인 경우에는 잘 동작함.
  }else{
    Predicted_Probs = predict(fit, newdata = Test_X, type = "probs")
  }

  # Get the predicted classes from the probabilities
  Predicted_Classes = apply(Predicted_Probs, 1, which.max) %>% unname

  # Predicted_Classes 벡터를 실제 레이블로 변환
  # fit 객체의 클래스 레벨을 가져옵니다
  class_levels = levels(Test_y)

  # 인덱스를 레이블로 변환합니다
  Predicted_Classes = factor(class_levels[Predicted_Classes], levels = class_levels)






  #===========================================================================
  # Confusion matrix
  #===========================================================================
  # Confusion matrix 생성
  Confusion_Matrix = table(Predicted = Predicted_Classes, Actual = Test_y)

  # Calculate the number of correct predictions (diagonal of the confusion matrix)
  Correct_Predictions = sum(diag(Confusion_Matrix))

  # Calculate the total number of predictions (sum of all elements in the confusion matrix)
  Total_predictions = sum(Confusion_Matrix)

  # Calculate the misclassification rate
  Misclassification_rate = 1 - (Correct_predictions / Total_predictions)






  #===========================================================================
  # ROAUC
  #===========================================================================
  ROAUC.list = Classification___Logistic___Results___Predict___AUROC(Predicted_Probs = Predicted_Probs,
                                                                     y_Test_unlist = Test_y,
                                                                     AUC_in_Legend = Logistic$AUC_in_Legend,
                                                                     path_Export = Logistic$path_Export) %>% suppressWarnings()







  #===========================================================================
  # return
  #===========================================================================
  c(list(Confusion_Matrix = Confusion_matrix, Misclassification_Rate = Misclassification_rate, Misclassified_Subjects = Misclassified.df), ROAUC.list) %>% return()


}





















#
# #===========================================================================
# # Compare the classification & Extract the misclassified subjects
# #===========================================================================
# Test_y_unlist = Test_y %>% unlist() %>% unname()
# Test_y_New = Test_y_unlist
# Index_Misclassified = which(Predicted_Classes != Test_y_New)
# Misclassified.df = cbind(Group = as.character(Test_y_unlist[Index_Misclassified]),
#                          Predicted_Class = levels(unlist(Test_y))[Predicted_Classes[Index_Misclassified]],
#                          Test_X[Index_Misclassified, ] %>% as.data.frame,
#                          Index = Index_Misclassified)
#
#
#
#
#
# #===========================================================================
# # Confusion matrix
# #===========================================================================
# # Create a confusion matrix
# Confusion_matrix = table(Predicted = Predicted_Classes, Actual = Test_y_unlist)
# if(nrow(Confusion_matrix) != levels(Test_y_unlist) %>% length){
#   rows_not_defined = unique(Test_y_unlist)[! Test_y %>% unique %in% rownames(Confusion_matrix)] %>% sort
#
#   New_matrix = matrix(0, nrow = rows_not_defined %>% length, ncol = Test_y_unlist %>% unique %>% length)
#   rownames(New_matrix) = rows_not_defined
#
#   Combined_Matrix = rbind(New_matrix, Confusion_matrix)
#
#   row_names = rownames(Combined_Matrix)
#   Confusion_matrix = Combined_Matrix[row_names %>% as.numeric %>% order,]
# }
# rownames(Confusion_matrix) = levels(Test_y_unlist)
#

#






