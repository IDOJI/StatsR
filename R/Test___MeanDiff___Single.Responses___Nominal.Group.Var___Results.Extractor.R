Test___MeanDiff___Single.Responses___Nominal.Group.Var___Results.Extractor = function(p, Data, Group_Var, Response_Var){
  #=============================================================================
  # Grouping
  #=============================================================================
  n_Group = Data %>% dplyr::select(!!Group_Var) %>% unlist() %>% table %>% names %>% length





  #=============================================================================
  # Decision
  #=============================================================================
  if(n_Group == 2){
    Extracted_Results = Test___MeanDiff___Single.Responses___Nominal.Group.Var___Results.Extractor___Two.Groups(p)
  }else{
    Extracted_Results = Test___MeanDiff___Single.Responses___Nominal.Group.Var___Results.Extractor___More.Groups(p, Group_Var, Response_Var)
  }





  #=============================================================================
  # Returning
  #=============================================================================
  return(Extracted_Results)
}

