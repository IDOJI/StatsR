Split.Data___CV.Fold___Stratified___One.Var = function(Data, Var_1, n_fold, seed){
  # Var_1의 prop을 유지한 채 n-fold
  #=============================================================================
  # packages
  #=============================================================================
  install_packages("caret", load=T)






  #=============================================================================
  # Define N_fold for test set
  #=============================================================================
  n_fold_new = n_fold + 1






  #=============================================================================
  # Use the `createFolds` function to generate stratified n-fold partitions:
  #=============================================================================
  set.seed(seed)
  Folds_Index = createFolds(Data[,Var_1] %>% unlist, k = n_fold_new, list = TRUE, returnTrain = FALSE)




  #=============================================================================
  # Split test & train
  #=============================================================================
  Test = Data[Folds_Index[[1]], ] %>% as_tibble
  Train = Data[-Folds_Index[[1]], ] %>% as_tibble





  #=============================================================================
  # Split Train again
  #=============================================================================
  set.seed(seed)
  Train_Folds_Index = createFolds(Train[,Var_1] %>% unlist, k = n_fold, list = TRUE, returnTrain = FALSE)






  #=============================================================================
  # Extract Train Data by Index
  #=============================================================================
  Folded_Train_Data.list = lapply(Train_Folds_Index, function(ith_Index, ...){
    return(Train[ith_Index, ])
  })






  #=============================================================================
  # New Var_1
  #=============================================================================
  Which_Fold = rep(NA, times = nrow(Train))
  for(k in 1:length(Train_Folds_Index)){
    Which_Fold[Train_Folds_Index[[k]]] = k
  }
  Train$Which_Fold = Which_Fold
  Train = Train %>% relocate(Which_Fold)









  #=============================================================================
  # Results
  #=============================================================================
  Results = list(X_Train = Train %>% dplyr::select(-all_of(c(Var_1, "Which_Fold"))),
                 y_Train = Train %>% dplyr::select(all_of(Var_1)),
                 Train_Folds_Index.vec = Train$Which_Fold,
                 Train_Folds_Index.list = Train_Folds_Index,
                 X_Test = Test %>% dplyr::select(-all_of(Var_1)),
                 y_Test = Test %>% dplyr::select(all_of(Var_1)))





  #=============================================================================
  # returning
  #=============================================================================
  Results %>% return
}








#
#
# #=============================================================================
# # Extract test set
# #=============================================================================
# if(return_test.set){
#   if(return_index){
#     test.set = Folds_Index[[1]]
#     train.set = Folds_Index[-1]
#   }else{
#     test.set = Folded_Data.list[[1]]
#     train.set = Folded_Data.list[-1]
#   }
# }else{
#   if(return_index){
#     train.set = Folds_Index
#   }else{
#     train.set = Folded_Data.list
#   }
# }
# names(train.set) = paste0(rep("Fold_", times = length(train.set)), 1:length(train.set))







