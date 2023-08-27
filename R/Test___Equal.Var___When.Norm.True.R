Test___Equal.Var___When.Norm.True = function(Data, Group_Var, Response_Var, is.normal, alpha){
  data.list = as_list_by(Data, Group_Var)

  results.list = list(NA, NA, NA, NA)
  names(results.list) = c("Equal.Var_results", "Equal.Var_What.Test","Equal.Var_p.val", "is.Equal.Var")

  if(length(data.list)==2){
    results.list[[1]] = var.test(data.list[[1]][,Response_Var] %>% unlist,
                                 data.list[[2]][,Response_Var] %>% unlist,
                                 conf.level = 1-alpha)
    results.list[[2]] = "F.test"
    results.list[[3]] = results.list[[1]]$p.value
  }else if(length(data.list)>=3){
    results.list[[1]] = bartlett.test(SUB___as.formula(Response_Var, Group_Var), Data)
    results.list[[2]] = "Bartlett"
    results.list[[3]] = results.list[[1]]$p.value
  }

  results.list[[4]] = results.list[[3]] > alpha
  return(results.list)
}

