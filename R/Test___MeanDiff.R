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
                           Boxplot_label.as.p.val=F,
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
  type = mathc.arg(type)






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
  # 3) ANOVA & Boxplot
  #==================================================================================
  Group_Var_Type = match.arg(Group_Var_Type)
  if(Response_Vars_Milti){
    Results_ANOVA = Test___MeanDiff___Multi.Reponses(Data, Response_Vars, Group_Var, Group_Var_Type, alpha_ANOVA, alpha_PostHoc, is.Normal, is.Equal.Var, type)
  }else{
    Results_ANOVA = Test___MeanDiff___Single.Responses(Data, Response_Vars, Group_Var, Group_Var_Type, alpha_ANOVA, alpha_PostHoc, p.adjust.method, is.Normal, is.Equal.Var, type)
  }





  #==================================================================================
  # 5) Export
  #==================================================================================
  if(!is.null(save.path)){
    if(is.null(file.name)){
      file.name = paste0("[ANOVA] Results_", "`", Group_Var, "`")
    }
    Test___MeanDiff___Export.xlsx.Highlight(Results_ANOVA, save.path, file.name)
  }



  return(Results_ANOVA)
}




#
#
# #==================================================================================
# # Add excluded subjects
# #==================================================================================
# MeanDiff_Multi_Responses_New.list = lapply(seq_along(MeanDiff_Multi_Responses.list), FUN=function(i, ...){
#   ith_results.list = MeanDiff_Multi_Responses.list[[i]]
#   if(length(ith_results.list)==3){
#     ith_Results.df = ith_results.list[[1]]
#     ith_Results_Reporting.df = ith_results.list[[2]]
#
#
#     ith_tmp.df = matrix(NA, nrow=length(ith_results.list[[3]]), ncol=ncol(ith_Results.df)) %>% as.data.frame;names(ith_tmp.df) = names(ith_Results.df)
#     ith_tmp.df[,1] = ith_results.list[[3]]
#     ith_tmp.df[,2] = rep(1, times = length(ith_results.list[[3]]))
#     ith_tmp.df[,3] = rep("Excluded Group (Only One data)", times = length(ith_results.list[[3]]))
#     ith_Results_New.df = rbind(ith_Results.df, ith_tmp.df)
#
#
#     ith_tmp.df = matrix(NA, nrow=length(ith_results.list[[3]]), ncol=ncol(ith_Results_Reporting.df)) %>% as.data.frame;names(ith_tmp.df) = names(ith_Results_Reporting.df)
#     ith_tmp.df[,1] = ith_results.list[[3]]
#     ith_tmp.df[,2] = rep(1, times = length(ith_results.list[[3]]))
#     ith_tmp.df[,3] = rep("Excluded Group (Only One data)", times = length(ith_results.list[[3]]))
#     ith_Results_Reporting_New.df = rbind(ith_Results_Reporting.df, ith_tmp.df)
#
#     return(list(Resuts = ith_Results_New.df, Results_Reporting = ith_Results_Reporting_New.df))
#   }else{
#     ith_results.list
#   }
# })
#












#==================================================================================
# 5) Combining Results
#==================================================================================
# Expanded_Response_Vars = rep(Response_Vars, each = 3)
# # Expanded_Response_Vars[c(FALSE, TRUE, TRUE)] <- NA
# Expanded_Group_Var = rep(Group_Var, times = length(Response_Vars))
# Results.df = do.call(rbind, Results)
# Results.df = cbind(Group=Expanded_Group_Var, Response = Expanded_Response_Vars, Results.df)



