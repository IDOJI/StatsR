Test___Equal.Var___When.Norm.False = function(Data, Group_Var, Response_Var, is.normal, alpha, outlier_method = c("IQR")){
  #===========================================================================
  # Check outliers
  #===========================================================================
  Have_Outliers = tapply(
    Data %>% select(!!Response_Var) %>% unlist(),
    Data %>% select(!!Group_Var) %>% unlist(),
    function(x){
      ith_Outliers = SUB___Which.Outliers(x, method = outlier_method)
      ith_Have_Outliers = is.numeric(ith_Outliers)
      return(ith_Have_Outliers)
    }
  ) %>% sum %>% as.logical





  #===========================================================================
  # test
  #===========================================================================
  results.list = list(NA, NA, NA, NA)
  names(results.list) = c("Equal.Var_results", "Equal.Var_What.Test", "Equal.Var_p.val", "is.Equal.Var")


  if(Have_Outliers){
    results.list[[1]] = fligner.test(SUB___as.formula(y=Response_Var, x=Group_Var), data=Data)
    results.list[[2]] = "Fligner-Killeen"
    results.list[[3]] = results.list[[1]]$p.value %>% as.numeric
  }else{
    results.list[[1]] =car::leveneTest(y=Data[,Response_Var] %>% unlist,  group=Data[,Group_Var] %>% unlist) %>% suppressWarnings()
    results.list[[2]] = "Levene"
    results.list[[3]] = results.list[[1]]$`Pr(>F)`[1] %>% as.numeric
  }

  results.list[[4]] = results.list[[3]] > alpha
  return(results.list)
}
