Classification___Logistic___Results___Predict = function(Logistic){
  #===========================================================================
  # Arguments
  #===========================================================================
  fit = Logistic$Best_Model

  Test_X = Logistic$Test_X
  Test_y = Logistic$Test_y

  Levels = Test_y %>% unlist %>% levels

  x_varname = Logistic$Plot_x_varname
  y_varname = Logistic$Plot_y_varname

  AUC_in_Legend = Logistic$AUC_in_Legend
  path_Export = Logistic$path_Export


  Results.list = list()




  #===========================================================================
  # Prediction
  #===========================================================================
  # Predicted Probabilities
  if("glm" %in% class(fit)){

    Predicted_Probs = predict(fit, newdata = Test_X %>% as.data.frame, type = "response")

  }else if("ordinalNet" %in% class(fit)){

    Predicted_Probs = predict(fit, newx = Test_X %>% as.matrix, type = "response") # Ordinal_Elastic인 경우에는 잘 동작함.

  }else if("glmnet" %in% class(fit)){

    Predicted_Probs = predict(fit, newx = Test_X %>% as.matrix, type = "response", s = fit$lambda.min)

  }else if("grpreg" %in% class(fit)){

    Predicted_Probs = predict(fit, X = Test_X %>% as.matrix, type = "response")


  }else{

    stop("??????????????????Prediction Probs")

  }






  #===========================================================================
  # Predict class
  #===========================================================================)
  if(Levels %>% length == 2){
    if(Logistic$Cut_Off %>% is.null){
      Predicted_Classes = NULL
    }else{
      # 확률을 바탕으로 클래스 레이블 결정 (예: 확률이 0.5 이상이면 1, 그렇지 않으면 0)
      Predicted_Classes = ifelse(Predicted_Probs > Logistic$Cut_Off, Levels[2], Levels[1])
      Predicted_Classes = factor(Predicted_Classes, levels = Levels)
    }
  }else{
    # Get the predicted classes from the probabilities
    Predicted_Classes = apply(Predicted_Probs, 1, which.max) %>% unname

    # Predicted_Classes 벡터를 실제 레이블로 변환
    # fit 객체의 클래스 레벨을 가져옵니다
    class_levels = levels(Test_y)

    # 인덱스를 레이블로 변환합니다
    Predicted_Classes = factor(class_levels[Predicted_Classes], levels = class_levels)
  }
  Results.list$Predicted_Classes = Predicted_Classes







  #===========================================================================
  # Confusion matrix
  #===========================================================================
  if(!is.null(Results.list$Predicted_Classes)){
    # Confusion matrix 생성
    Results.list$Confusion_Matrix = Confusion_Matrix = table(Predicted = Predicted_Classes, Actual = Test_y %>% unlist)

    # Calculate the number of correct predictions (diagonal of the confusion matrix)
    Correct_Predictions = sum(diag(Confusion_Matrix))

    # Calculate the total number of predictions (sum of all elements in the confusion matrix)
    Total_Predictions = sum(Confusion_Matrix)

    # Calculate the misclassification rate
    Results.list$Misclassification_Rate = 1 - (Correct_Predictions / Total_Predictions)
  }











  #===========================================================================
  # Misclassified subjects
  #===========================================================================









  #===========================================================================
  # ROAUC
  #===========================================================================
  Results.list$ROAUC = Classification___Logistic___Results___Predict___AUROC(Predicted_Probs,
                                                                             Logistic) %>% suppressWarnings()






  #===========================================================================
  # return
  #===========================================================================
  return(Results.list)
}
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




