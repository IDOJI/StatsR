Split.Data___CV.Fold___Stratified___Two.Var = function(Data, Var_1, Var_2, n_fold, seed){
  # 두 변수의 조합의 비율은 유지한 채 n_fold
  # var의 prop을 유지한 채 n-fold
  #=============================================================================
  # packages
  #=============================================================================
  install_packages(c("caret", "dplyr", "tidyr"), load=T)







  #=============================================================================
  # Create a combined stratification variable
  #=============================================================================
  Data_Var_1 = Data[,Var_1]
  Data_Var_2 = Data[,Var_2]
  Data = Data %>% unite("Strata", !!sym(Var_2), !!sym(Var_1), sep = "_")
  Data = cbind(Data_Var_1, Data_Var_2, Data)







  #=============================================================================
  # Perform n_fold splitting by "Strata"
  #=============================================================================
  Splitted_Data = Split.Data___CV.Fold___Stratified___One.Var(Data, Var_1 = "Strata", n_fold, seed)






  #=============================================================================
  # returning
  #=============================================================================
  return(Splitted_Data)
}


