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
  # Adjust p-values
  #==================================================================================
  # adjust method
  p.adjust.method = match.arg(p.adjust.method)

  # Combine results
  # Combined_Results_ANOVA = do.call(rbind, Results_ANOVA)

  # Extract p.vals
  p.vals = sapply(Results_ANOVA, function(x){x %>% select(!!p.value_colname) %>% unlist() %>% unname()}) %>% as.vector()

  # Adjust p.vals
  Adjusted_p.vals = Test___Adjust.p.values(p.vals, method = p.adjust.method, alpha = alpha_ANOVA)


  # Replace p.vals and method
  Results_ANOVA_New = Results_ANOVA
  Adjusted_p.vals_New = Adjusted_p.vals

  for(m in 1:length(Results_ANOVA_New)){
    # mth ANOVA results
    mth_Results_ANOVA = Results_ANOVA_New[[m]]

    # mth nrow
    mth_nrow = mth_Results_ANOVA %>% nrow

    # replace pvals
    mth_Results_ANOVA$p.value_Comparison = Adjusted_p.vals_New[1:mth_nrow]


    # Decide significance
    mth_Results_ANOVA$Significance = SUB___P.vals.Signif.Stars(mth_Results_ANOVA$p.value_Comparison)


    # replace adjustment method
    mth_Results_ANOVA$p.adjust.method = p.adjust.method

    # remove the added p.vals
    Adjusted_p.vals_New = Adjusted_p.vals_New[-c(1:mth_nrow)]

    # replace new results
    Results_ANOVA_New[[m]] = mth_Results_ANOVA
  }
  Results_ANOVA = Results_ANOVA_New








  #==================================================================================
  # Add group1 & group2 for 2 groups results only
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
