Test___MeanDiff___Single.Responses___Ordinal.Group.Var = function(Data,
                                                                  Response_Vars,
                                                                  Group_Var,
                                                                  alpha_ANOVA = 0.05,
                                                                  alpha_PostHoc = 0.05,
                                                                  p.adjust.method = c("TSBH", "Bonferroni", "Holm", "Hochberg", "SidakSS", "SidakSD", "BH", "BY","ABH"),
                                                                  is.Normal=NULL,
                                                                  is.Equal.Var=NULL){
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
    if(y$p.vals[1] < alpha_ANOVA){
      index_min_p.val = which.min(y$p.vals[2:3]) + 1
    }else{
      index_min_p.val = which.min(y$p.vals)
    }
    return(y[index_min_p.val, ])
  })






  #=============================================================================
  # Combining Resulst
  #=============================================================================
  Combined_Results = do.call(rbind, Minimal_p.vals)
  Combined_Results = cbind(Group = Group_Var, Response = Response_Vars, Combined_Results)
  rownames(Combined_Results) = NULL





  #=============================================================================
  # Adjust p.vals
  #=============================================================================
  Adjusted_P.vals = Test___Adjust.p.values(raw_p.vals = Combined_Results$p.vals, proc =  p.adjust.method, alpha_ANOVA)
  # Combine
  Combined_Results = cbind(Combined_Results, Adjusted_P.vals)


  return(Combined_Results)
}






