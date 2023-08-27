Test___MeanDiff___Ordinal.Group.Var = function(Data,
                                               Response_Vars,
                                               Group_Var,
                                               alpha = 0.05,
                                               p.adjust.method = ){
  #==================================================================================
  # Test for each response
  #==================================================================================
  Results.list = lapply(seq_along(Response_Vars), function(i){
    if(is.Normal[i]){
      Test___MeanDiff___Ordinal.Group.Var___Parametric()
    }else{
      Test___MeanDiff___Ordinal.Group.Var___Nonparametric(Data, Group_Var, Response_Vars[i])
    }
  })
  names(Results.list) = Response_Vars





  #=============================================================================
  # Select min p.vals
  #=============================================================================
  Minimal_p.vals = lapply(Results.list, function(y){
    index_min_p.val = which.min(y$p.vals)
    return(y[index_min_p.val, ])
  })




  #=============================================================================
  # Combining Resulst
  #=============================================================================
  Combined_Results = do.call(rbind, Minimal_p.vals)
  Combined_Results = cbind(Group = Group_Var, Response = Response_Vars, Combined_Results)
  rownames(Combined_Results) = NULL


  return(Combined_Results)
}






