Test___MeanDiff = function(##############################
                           # data & variables
                           ##############################
                           Data,
                           Response_Vars,
                           Response_Vars_Milti = FALSE,
                           Group_Var,
                           Group_Var_Type = c("Nominal", "Ordinal"),
                           ##############################
                           # Significance level
                           ##############################
                           alpha_Norm  = 0.05,
                           alpha_Equal.Var  = 0.05,
                           alpha_ANOVA = 0.05,
                           alpha_PostHoc = 0.05,
                           type = c("parametric", "nonparametric", "robust", "bayes"),
                           p.adjust.method = c("Bonferroni", "Holm", "Hochberg", "SidakSS", "SidakSD", "BH", "BY","ABH","TSBH"),
                           ##############################
                           # Figure
                           ##############################
                           label.as.p.val=F,
                           group.comparison=F,
                           lines.connecting.medians=F,
                           # title="",
                           # results.subtitle=T,
                           ##############################
                           # exporting
                           ##############################
                           save.path = NULL,
                           file.name = NULL){
  #==================================================================================
  # path
  #==================================================================================
  dir.create(save.path, showWarnings = F)






  #==================================================================================
  # type
  #==================================================================================
  type = match.arg(type)






  #==================================================================================
  # 1) Normality
  #==================================================================================
  Results_Normality = Test___Normality(Data, Group_Var, Response_Vars, alpha = alpha_Norm)
  is.Normal = sapply(Results_Normality, function(x) x[[2]])







  #==================================================================================
  # 2) Homogeneity
  #==================================================================================
  Results_Homogeneity = Test___Equal.Var(Data, Group_Var, Response_Vars, is.Normal, alpha = alpha_Equal.Var)
  is.Equal.Var = sapply(Results_Homogeneity, function(x) x[[4]])







  #==================================================================================
  # 3) ANOVA
  #==================================================================================
  Group_Var_Type = match.arg(Group_Var_Type)
  if(Response_Vars_Milti){
    Results_ANOVA = Test___MeanDiff___Multi.Reponses(Data, Response_Vars, Group_Var, Group_Var_Type, alpha_ANOVA, is.Normal, is.Equal.Var, type)
  }else{
    Results_ANOVA = Test___MeanDiff___Single.Responses(Data, Response_Vars, Group_Var, Group_Var_Type, alpha_ANOVA, p.adjust.method, is.Normal, is.Equal.Var, type)
  }







  #==================================================================================
  # 4) Export ANOVA results
  #==================================================================================
  if(!is.null(save.path)){
    if(is.null(file.name)){
      file.name = paste0("[ANOVA] Results_", "`", Group_Var, "`")
    }
    Test___MeanDiff___Export.xlsx.Highlight(Results_ANOVA, save.path, file.name)
  }







  #==================================================================================
  # 5) Boxplot
  #==================================================================================
  Boxplot.list = lapply(Results_ANOVA, function(ith_Results){
    Test___MeanDiff___Single.Responses___Box.Plot(Data,
                                                  ith_Results,
                                                  label.as.p.val,
                                                  group.comparison,
                                                  lines.connecting.medians,
                                                  save.path)
  })





  return(Results_ANOVA)
}



