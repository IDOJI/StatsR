Split.Data___CV.Fold___Staratified = function(Data, Var, n_fold=10, return_test.set = TRUE, return_index = TRUE){
  #=============================================================================
  # One var
  #=============================================================================
  if(length(Var)==1){
    test = Split.Data___CV.Fold___Staratified___One.Var(Data,...)
  }
  return(test)
}

test  = Split.Data___CV.Fold___Staratified(Data=Data_New,Var="AHI_group",
                                           n_fold=10,
                                           return_test.set=TRUE,
                                           return_index=TRUE)


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

