SUB___Fold = function(Data.df,
                      Var_1,
                      Var_2 = NULL,
                      Train_K_Folds = 5,
                      Return_Validation=T,
                      seed = 1123){
  # 🟥 Packages =============================================================================
  install_packages(c("caret", "dplyr", "tidyr"), load=T)




  # 🟥 Results list =============================================================================
  Results.list = list()



  # 🟥 Check categorical data =============================================================================
  if (!is.factor(Data.df[[Var_1]]) && !is.character(Data.df[[Var_1]])) {
    stop(paste(Var_1, "is not a categorical variable."))
  }
  if (!is.null(Var_2) && !is.factor(Data.df[[Var_2]]) && !is.character(Data.df[[Var_2]])) {
    stop(paste(Var_2, "is not a categorical variable."))
  }

  # 범주형 변수를 factor로 변환합니다.
  Data.df[[Var_1]] <- as.factor(Data.df[[Var_1]]) %>% droplevels()
  if (!is.null(Var_2)) {
    Data.df[[Var_2]] <- as.factor(Data.df[[Var_2]]) %>% droplevels()
  }





  # 🟥 Extract Group Var =============================================================================
  if(is.null(Var_2)){
    Data.df$Group_Var = Data.df[[Var_1]]
  }else{
    # stratification
    Data.df$Group_Var = interaction(Data.df[[Var_1]], Data.df[[Var_2]], drop=T)
  }





  # 🟥 Split Data by n_fold ===============================================================
  if(Return_Validation){
    K_Folds = Train_K_Folds + 1
  }else{
    K_Folds = Train_K_Folds
  }

  # Generate folds (Train_K_Folds + 1 (for validation set))
  Folds = createFolds(Data.df$Group_Var, k = K_Folds, list = TRUE)

  # split data
  Splitted_Data.list = lapply(Folds, function(ith_fold){
    Data.df[ith_fold,]
  })


  # Train + Test
  Train = Splitted_Data.list[1:Train_K_Folds]
  if(Return_Validation){
    Validation = Splitted_Data.list[[K_Folds]]

    # Folds
    names(Folds)[K_Folds] = "Validation"
    Combined_Train_Folds = Folds[-K_Folds] %>% unlist %>% unname

  }else{
    Validation = NULL
    Combined_Train_Folds  = Folds %>% unlist %>% unname
  }







  # 🟥 Check results ===============================================================
  # The number of subjects for each Fold
  Number_of_Each_Fold_Elements = sapply(Folds, length) %>%
    as.matrix %>%
    as.data.frame %>%
    setNames("N_Subjects")


  # Proportion for groups of each element
  Proportions = sapply(Folds, function(ith_fold){
    Data.df$Group_Var[ith_fold] %>% table %>% prop.table
  }) %>% t() %>% as.data.frame
  names(Proportions) = paste0("PropGroup_", names(Proportions))

  # Combine
  Combined = cbind(Folds = rownames(Proportions), Number_of_Each_Fold_Elements, Proportions)
  rownames(Combined) = NULL
  Combined$Folds[1:Train_K_Folds] = paste0("Train_", Combined$Folds[1:Train_K_Folds])
  Combined$Folds[Train_K_Folds+1] = "Validation"





  # 🟥 Results ===============================================================
  Results = list(Train = Train,
                 Validation = Validation,
                 Index = Folds,
                 Combined_Train_Index = Combined_Train_Folds,
                 Structure = Combined)
  return(Results)

}











