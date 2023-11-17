SUB___Fold = function(Data, Var_1, Var_2 = NULL, y_Var = NULL, Train_percentage=0.8, Train_K_Fold=5, Fold_N = 70, seed = 1123, Return_Test=T){
  #=============================================================================
  # Arguments
  #=============================================================================
  # y_Var : if given, Test_y, Train_y data are exported





  #=============================================================================
  # Results list
  #=============================================================================
  Results.list = list()






  #=============================================================================
  # Check categorical data
  #=============================================================================
  # test
  if (!is.factor(Data[[Var_1]]) && !is.character(Data[[Var_1]])) {
    stop(paste(Var_1, "is not a categorical variable."))
  }
  if (!is.null(Var_2) && !is.factor(Data[[Var_2]]) && !is.character(Data[[Var_2]])) {
    stop(paste(Var_2, "is not a categorical variable."))
  }


  # 범주형 변수를 factor로 변환합니다.
  Data[[Var_1]] <- as.factor(Data[[Var_1]]) %>% droplevels()
  if (!is.null(Var_2)) {
    Data[[Var_2]] <- as.factor(Data[[Var_2]]) %>% droplevels()
  }








  #===============================================================================
  # Packages
  #===============================================================================
  install_packages(c("caret", "dplyr", "tidyr"), load=T)







  #===============================================================================
  # Split data by Test & Train
  #===============================================================================
  set.seed(seed)
  if(is.null(Var_2)){
    # Split data by Var_1
    Train_Index = createDataPartition(Data[[Var_1]], p = Train_percentage, list = FALSE)
  }else{
    # Split data by Var_1 & Var_2
    # Generate new variable by combination of Var_1 & Var_2
    Strata = interaction(Data[[Var_1]], Data[[Var_2]])
    Data = cbind(Strata, Data)
    # The code which works the same thing
    # Data_Var_1 = Data[[Var_1]]
    # Data_Var_2 = Data[[Var_2]]
    # Data = Data %>% unite("Strata", !!sym(Var_2), !!sym(Var_1), sep = "_")
    # Data = cbind(Data_Var_1, Data_Var_2, Data) %>% dplyr::relocate("Strata")

    Train_Index <- createDataPartition(Data$Strata, p = Train_percentage, list = FALSE)
    Data$Strata = NULL
  }

  Results.list$Train = Data[Train_Index, ]
  Results.list$Test = Data[-Train_Index, ]
  # Test$Strata %>% table %>% prop.table
  # Train$Strata %>% table %>% prop.table






  #===============================================================================
  # Define K fold by "Train_Fold_N"
  #===============================================================================
  # Compute the number of fold by the minimum sample size
  if(!is.null(Fold_N)) {
    # 총 샘플 수를 폴드 당 샘플 수로 나누어 필요한 폴드 수를 계산합니다.
    # ceil 함수를 사용하여 소수점을 올림하여 항상 최소 샘플 수를 만족하는 폴드 수를 보장합니다.
    N_in_Train = nrow(Results.list$Train)
    Train_K_Fold = ceiling(N_in_Train / Fold_N)
  }







  #===============================================================================
  # Split Train by n_fold
  #===============================================================================
  set.seed(seed)
  if(is.null(Var_2)){
    # Var_1을 기준으로 트레이닝 데이터를 나눕니다.
    Results.list$Folds.list = createFolds(Results.list$Train[[Var_1]], k = Train_K_Fold, list = TRUE)
    Results.list$Folds.vec = createFolds(Results.list$Train[[Var_1]], k = Train_K_Fold, list = FALSE)
  }else{
    # combinedVar를 기준으로 트레이닝 데이터를 나눕니다.
    Results.list$Folds.list = createFolds(Results.list$Train$Strata, k = Train_K_Fold, list = TRUE)
    Results.list$Folds.vec = createFolds(Results.list$Train$Strata, k = Train_K_Fold, list = FALSE)
  }








  #===============================================================================
  # y_Var
  #===============================================================================
  if(!is.null(y_Var)){
    Results.list$Train_X = Results.list$Train %>% dplyr::select(-!!y_Var) %>% as_tibble
    Results.list$Train_y = Results.list$Train %>% dplyr::select(!!y_Var) %>% as_tibble
    Results.list$Train = NULL


    Results.list$Test_X = Results.list$Test %>% dplyr::select(-!!y_Var) %>% as_tibble
    Results.list$Test_y = Results.list$Test %>% dplyr::select(!!y_Var) %>% as_tibble
    Results.list$Test = NULL
  }






  #===============================================================================
  # Return Results
  #===============================================================================
  return(Results.list)

}











