Classification___Logistic___Binomial___Penalty___NonGrouped = function(Logistic){
  #=============================================================================
  # Load the glmnet
  #=============================================================================
  install_packages(c("glmnet", "pROC"))





  #=============================================================================
  # CV glmnet
  #=============================================================================
  # 각 alpha 값에 대한 교차 검증을 수행하여 최적의 lambda를 찾습니다.
  if(Logistic$Train_Folds_Index.vec %>% unique %>% length < 3){
    Combined = cbind(Logistic$Train_y, Logistic$Train_X)

    Logistic$Train_Folds_Index.vec = caret::createFolds(Combined$DEMO___DIAGNOSIS_NEW, k = 3, list = F)
    Logistic$Train_Folds_Index.list = caret::createFolds(Combined$DEMO___DIAGNOSIS_NEW, k = 3, list = T)

  }
  CV_Results.list = lapply(Logistic$penalty_alpha, function(ith_alpha_val) {
    tictoc::tic()
    ith_Result = cv.glmnet(x = as.matrix(Logistic$Train_X),
                           y = Logistic$Train_y %>% unlist,
                           family = "binomial",
                           foldid = Logistic$Train_Folds_Index.vec,
                           type.measure = "deviance",
                           alpha = ith_alpha_val,
                           parallel = T,
                           penalty.factor = Logistic$penalty_factor)
    tictoc::toc()
    return(ith_Result)
  }) %>% suppressWarnings()






  #=============================================================================
  # CV glmnet : find optimal lambda
  #=============================================================================
  # 각 alpha 값에 대해 최소 CV 오류를 가지는 lambda를 추출합니다.
  Best_Parameters = lapply(seq_along(CV_Results.list), function(k) {
    kth_CV = CV_Results.list[[k]]
    data.frame(
      alpha = Logistic$penalty_alpha[k],
      lambda = kth_CV$lambda.min,
      cvm = min(kth_CV$cvm)
    )
  }) %>% do.call(rbind, .)
  Best_Parameters = Best_Parameters[which.min(Best_Parameters$cvm), ]






  #=============================================================================
  # Fit best model
  #=============================================================================
  Logistic$Best_Model = glmnet(x = Logistic$Train_X,
                               y = Logistic$Train_y %>% unlist,
                               family = Logistic$Family,
                               alpha = Best_Parameters$alpha,
                               lambda = Best_Parameters$lambda,
                               penalty.factor = Logistic$penalty_factor)





  #=============================================================================
  # Extract Results
  #=============================================================================
  Logistic = Classification___Logistic___Results(Logistic)



  return(Logistic)
}
