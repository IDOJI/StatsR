Test___MeanDiff___Single.Responses = function(Data,
                                              Response_Vars,
                                              Group_Var,
                                              Group_Var_Type = c("Nominal", "nominal","Ordinal", "ordinal"),
                                              alpha_ANOVA = 0.05,
                                              p.adjust.method = c("Bonferroni", "Holm", "Hochberg", "SidakSS", "SidakSD", "BH", "BY","ABH","TSBH"),
                                              is.Normal,
                                              is.Equal.Var,
                                              type = c("parametric", "nonparametric", "robust", "bayes")){
  #==================================================================================
  # ANOVA
  #==================================================================================
  if(Group_Var_Type %in% c("Nominal", "nominal")){
    Results_ANOVA = Test___MeanDiff___Single.Responses___Nominal.Group.Var(Data,
                                                                           Response_Vars,
                                                                           Group_Var,
                                                                           alpha_ANOVA,
                                                                           is.Normal,
                                                                           is.Equal.Var,
                                                                           type)
    p.value_colname = "p.value_Comparison"
  }else if(Group_Var_Type %in% c("Ordinal", "ordinal")){
    Results_ANOVA = Test___MeanDiff___Single.Responses___Ordinal.Group.Var(Data,
                                                                           Response_Vars,
                                                                           Group_Var,
                                                                           alpha_ANOVA,
                                                                           is.Normal,
                                                                           is.Equal.Var,
                                                                           type)
    p.value_colname = "p.value"
  }




  #==================================================================================
  # Adjust p.vals & Significance
  p.adjust.method = match.arg(p.adjust.method)
  test = do.call(rbind, Results_ANOVA)
  p.vals = sapply(Results_ANOVA, function(x){x %>% select(!!p.value_colname) %>% unlist() %>% unname()}) %>% as.vector()
  Adjusted_p.vals = Test___Adjust.p.values(p.vals, method = p.adjust.method, alpha = alpha_ANOVA)
  # Adjusted_p.vals.list = split(Adjusted_p.vals, seq_len(nrow(Adjusted_p.vals)))
  for(i in 1:length(Results_ANOVA)){
    Results_ANOVA[[i]] = cbind(Results_ANOVA[[i]], Adjusted_p.vals[i,])
  }









  #==================================================================================
  # Add group1 & group2
  #==================================================================================
  Groups = Data %>% select(!!Group_Var) %>% unlist() %>% unique
  n_Groups = Groups %>% length
  if(n_Groups == 2){
    for(i in 1:length(Results_ANOVA)){
      Results_ANOVA[[i]]$group1 = Groups[1]
      Results_ANOVA[[i]]$group2 = Groups[2]
      Results_ANOVA[[i]] = Results_ANOVA[[i]] %>% relocate(starts_with("group"), .after=Group)
    }
  }






  #==================================================================================
  # return
  #==================================================================================
  return(Results_ANOVA)
}
