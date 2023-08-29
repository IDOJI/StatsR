Test___MeanDiff___Single.Responses___Ordinal.Group.Var = function(Data,
                                                                  Response_Vars,
                                                                  Group_Var,
                                                                  alpha_ANOVA = 0.05,
                                                                  is.Normal=NULL,
                                                                  is.Equal.Var=NULL,
                                                                  type){
  #==================================================================================
  # Test for each response
  #==================================================================================
  Results.list = lapply(seq_along(Response_Vars), function(i){
    if(is.Normal[i]){
      Test___MeanDiff___Single.Responses___Ordinal.Group.Var___Parametric()
    }else{
      Test___MeanDiff___Single.Responses___Ordinal.Group.Var___Nonparametric(Data, Group_Var, Response_Vars[i])
    }
  })
  names(Results.list) = Response_Vars







  #=============================================================================
  # Select min p.vals
  #=============================================================================
  Minimal_p.vals = lapply(Results.list, function(y){
    if(y$p.value[1] < alpha_ANOVA){
      index_min_p.val = which.min(y$p.value[2:3]) + 1
    }else{
      index_min_p.val = which.min(y$p.value)
    }
    return(y[index_min_p.val, ])
  })






  #=============================================================================
  # Combining Resulst
  #=============================================================================
  Combined_Results = do.call(rbind, Minimal_p.vals)
  Combined_Results = cbind(Group = Group_Var, Response = Response_Vars, Combined_Results)
  rownames(Combined_Results) = NULL
  Combined_Results.list = split(Combined_Results, seq_len(nrow(Combined_Results)))
  names(Combined_Results.list) = Response_Vars



  return(Combined_Results.list)
}














