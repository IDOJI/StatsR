Test___MeanDiff = function(##############################
                           # data & variables
                           ##############################
                           Data,
                           Response_Vars,
                           Group_Var,
                           Group_Var_Type = c("Nominal", "Ordinal"),
                           ##############################
                           # Significance level
                           ##############################
                           alpha_Norm  = 0.05,
                           alpha_Equal.Var  = 0.05,
                           alpha_ANOVA = 0.05,
                           alpha_PostHoc = 0.05,
                           p.adjust.method = c("bonferroni", "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"),
                           ##############################
                           # Figure
                           ##############################
                           Boxplot_label.as.p.val=F,
                           # title="",
                           # results.subtitle=T,
                           ##############################
                           # exporting
                           ##############################
                           save.path){
  #==================================================================================
  # path
  #==================================================================================
  dir.create(save.path, showWarnings = F)







  #==================================================================================
  # 1) Normality
  #==================================================================================
  Results_Normality = Test___Normality(Data, Group_Var, Response_Vars, alpha = alpha_Norm)
  is.Normal = sapply(Results_Normality, function(x) x[[2]])







  #==================================================================================
  # 2) Homogeneity
  #==================================================================================
  Results_Homogeneity = Test___Equal.Var(Data, Group_Var, Response_Vars, is.Normal)
  is.Equal.Var = sapply(Results_Homogeneity, function(x) x[[4]])







  #==================================================================================
  # 3) ANOVA
  #==================================================================================
  Group_Var_Type = match.arg(Group_Var_Type)

  if(Group_Var_Type == "Nominal"){
    Results = Test___MeanDiff___Nominal.Group.Var(Data, Response_Vars, Group_Var)
  }else if(Group_Var_Type == "Ordinal"){
    Results = Test___MeanDiff___Ordinal.Group.Var(Data, Response_Vars, Group_Var)
  }







  #==================================================================================
  # 4) Exporting results
  #==================================================================================









  return(Results)
}






#==================================================================================
# Add excluded subjects
#==================================================================================
MeanDiff_Multi_Responses_New.list = lapply(seq_along(MeanDiff_Multi_Responses.list), FUN=function(i, ...){
  ith_results.list = MeanDiff_Multi_Responses.list[[i]]
  if(length(ith_results.list)==3){
    ith_Results.df = ith_results.list[[1]]
    ith_Results_Reporting.df = ith_results.list[[2]]


    ith_tmp.df = matrix(NA, nrow=length(ith_results.list[[3]]), ncol=ncol(ith_Results.df)) %>% as.data.frame;names(ith_tmp.df) = names(ith_Results.df)
    ith_tmp.df[,1] = ith_results.list[[3]]
    ith_tmp.df[,2] = rep(1, times = length(ith_results.list[[3]]))
    ith_tmp.df[,3] = rep("Excluded Group (Only One data)", times = length(ith_results.list[[3]]))
    ith_Results_New.df = rbind(ith_Results.df, ith_tmp.df)


    ith_tmp.df = matrix(NA, nrow=length(ith_results.list[[3]]), ncol=ncol(ith_Results_Reporting.df)) %>% as.data.frame;names(ith_tmp.df) = names(ith_Results_Reporting.df)
    ith_tmp.df[,1] = ith_results.list[[3]]
    ith_tmp.df[,2] = rep(1, times = length(ith_results.list[[3]]))
    ith_tmp.df[,3] = rep("Excluded Group (Only One data)", times = length(ith_results.list[[3]]))
    ith_Results_Reporting_New.df = rbind(ith_Results_Reporting.df, ith_tmp.df)

    return(list(Resuts = ith_Results_New.df, Results_Reporting = ith_Results_Reporting_New.df))
  }else{
    ith_results.list
  }
})











#==================================================================================
# Export Full Results
#==================================================================================
Combined_Final.list = lapply(MeanDiff_Multi_Responses_New.list, FUN=function(x){
  # x = MeanDiff_Multi_Responses.list[[1]]
  return(x[[1]])
})
Combined_Final.df = do.call(dplyr::bind_rows, Combined_Final.list) %>% as.data.frame
Test___MeanDiff___Exporting.XLSX.Highlight(Reporting.df = Combined_Final.df,
                                           alpha_ANOVA = alpha_ANOVA,
                                           alpha_PostHoc = alpha_PostHoc,
                                           save.path = save.path,
                                           filename = paste0("[ANOVA] Results_", "`", var_group_filename, "`"))











#==================================================================================
# Export Resulf for Reporting
#==================================================================================
Combined_Reporting.list = lapply(MeanDiff_Multi_Responses_New.list, FUN=function(x){
  # x = MeanDiff_Multi_Responses.list[[1]]
  return(x[[2]])
})
Combined_Reporting.df = do.call(dplyr::bind_rows, Combined_Reporting.list) %>% as.data.frame
Test___MeanDiff___Exporting.XLSX.Highlight(Reporting.df = Combined_Reporting.df,
                                           alpha_ANOVA = alpha_ANOVA,
                                           alpha_PostHoc = alpha_PostHoc,
                                           save.path = save.path,
                                           filename = paste0("[ANOVA_Reporting] Results_", "`", var_group_filename, "`"))




# Example p-values
set.seed(123)
pvals <- runif(20)  # Generate 20 random p-values between 0 and 1


adjusted_pvals <- p.adjust(pvals, method = "BH")
print(adjusted_pvals)
?p.adjust




#==================================================================================
# 4) Plotting Boxplot
#==================================================================================






#==================================================================================
# 5) Combining Results
#==================================================================================
# Expanded_Response_Vars = rep(Response_Vars, each = 3)
# # Expanded_Response_Vars[c(FALSE, TRUE, TRUE)] <- NA
# Expanded_Group_Var = rep(Group_Var, times = length(Response_Vars))
# Results.df = do.call(rbind, Results)
# Results.df = cbind(Group=Expanded_Group_Var, Response = Expanded_Response_Vars, Results.df)



