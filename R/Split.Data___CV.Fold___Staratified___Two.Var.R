Split.Data___CV.Fold___Staratified___One.Var = function(Data, Var, n_fold=10, return_test.set = TRUE, return_index = FALSE){
  # var의 prop을 유지한 채 n-fold
  #=============================================================================
  # packages
  #=============================================================================
  install_packages("caret", load=T)






  #=============================================================================
  # Define N_fold
  #=============================================================================
  if(return_test.set){
    n_fold = n_fold + 1
  }






  #=============================================================================
  # Use the `createFolds` function to generate stratified n-fold partitions:
  #=============================================================================
  Folds_Index = createFolds(Data[,Var] %>% unlist, k = n_fold, list = TRUE, returnTrain = FALSE)








  #=============================================================================
  # Extract Data by Index
  #=============================================================================
  Folded_Data.list = lapply(Folds_Index, function(ith_Index, ...){
    return(Data[ith_Index, ])
  })







  #=============================================================================
  # Extract test set
  #=============================================================================
  if(return_test.set){
    if(return_index){
      test.set = Folds_Index[[1]]
      train.set = Folds_Index[-1]
    }else{
      test.set = Folded_Data.list[[1]]
      train.set = Folded_Data.list[-1]
    }
  }else{
    if(return_index){
      train.set = Folds_Index
    }else{
      train.set = Folded_Data.list
    }
  }






  #=============================================================================
  # returning
  #=============================================================================
  if(return_test.set){
    list(Train = train.set, Test = test.set)
  }else{
    return(train.set)
  }
}

#-------------------------------------------------------------------------------

Split.Data___CV.Fold___Keep.Prop = function(Data, Var_1, Var_2=NULL){
  #=============================================================================
  # packages
  #=============================================================================
  install_packages(c("dplyr", "tidyr"), load=T)


  #=============================================================================
  # packages
  #=============================================================================
  # Create a combined stratification variable
  data <- data %>%
    unite("strata", response, age_group, sep = "_")

  # Perform stratified sampling
  sample_data <- data %>%
    group_by(strata) %>%
    sample_frac(0.2)

  # This 'sample_data' dataframe now contains 20% of the original data
  # with the proportions of the response variable and age groups maintained.



}

