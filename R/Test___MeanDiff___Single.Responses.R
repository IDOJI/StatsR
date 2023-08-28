Test___MeanDiff___Single.Responses = function(Data,
                                              Response_Vars,
                                              Group_Var,
                                              Group_Var_Type = c("Nominal", "nominal","Ordinal", "ordinal"),
                                              alpha_ANOVA = 0.05,
                                              alpha_PostHoc = 0.05,
                                              p.adjust.method = c("Bonferroni", "Holm", "Hochberg", "SidakSS", "SidakSD", "BH", "BY","ABH","TSBH"),
                                              is.Normal,
                                              is.Equal.Var){


  if(Group_Var_Type %in% c("Nominal", "nominal")){
    Results_ANOVA = Test___MeanDiff___Single.Responses___Nominal.Group.Var(Data,
                                                                           Response_Vars,
                                                                           Group_Var,
                                                                           alpha_ANOVA,
                                                                           alpha_PostHoc,
                                                                           p.adjust.method,
                                                                           is.Normal,
                                                                           is.Equal.Var)
  }else if(Group_Var_Type %in% c("Ordinal", "ordinal")){
    Results_ANOVA = Test___MeanDiff___Single.Responses___Ordinal.Group.Var(Data,
                                                                           Response_Vars,
                                                                           Group_Var,
                                                                           alpha_ANOVA,
                                                                           alpha_PostHoc,
                                                                           p.adjust.method,
                                                                           is.Normal,
                                                                           is.Equal.Var)
  }

  return(Results_ANOVA)
}
