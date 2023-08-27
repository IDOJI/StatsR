Test___MeanDiff___Single.Response = function(##############################
                                          # data & variables
                                          ##############################
                                          df, var_group, var_response,
                                          ##############################
                                          # Significance level
                                          ##############################
                                          alpha_Norm  = 0.05,
                                          alpha_Equal.Var  = 0.05,
                                          alpha_ANOVA = 0.05,
                                          alpha_PostHoc = 0.05,
                                          p.adjust.method = c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"),
                                          ##############################
                                          # Figure
                                          ##############################
                                          # title="",
                                          # results.subtitle=T,
                                          ##############################
                                          # exporting
                                          ##############################
                                          save.path = NULL,
                                          filename = NULL,
                                          export.xlsx = F,
                                          Boxplot_label.as.p.val=F){
  #============================================================================
  # Having only one subject in the group?
  #============================================================================
  group_tab = table(df[,var_group])
  groups_with_1_subjects = names(group_tab)[group_tab == 1]
  if(length(groups_with_1_subjects) > 0){
    df = df %>% filter(.data[[var_group]] != groups_with_1_subjects)
    excluded_group = names(group_tab)[group_tab == 1]
  }else{
    excluded_group = NULL
  }






  #============================================================================
  # Exclude NA
  #============================================================================
  which_NA = df[,var_group] %>% unlist %>% is.na %>% which
  if(length(which_NA)>0){
    df = df[-which_NA, ]
  }





  #============================================================================
  # Normality
  #============================================================================
  Norm.Test_combined.list  = Test___Normality(df, var_group, var_response, alpha_Norm)
  Norm.Test_combined.df   = Norm.Test_combined.list[[1]]
  is.Normal               = Norm.Test_combined.list[[2]]






  #============================================================================
  # Equal.Var
  #============================================================================
  Equal.Var.Test_results.list = Test___Equal.Var(df, var_group, var_response, is.Normal, alpha_Equal.Var)
  Equal.Var.Test_combined.df  = Test___Equal.Var___Extract.Results(Equal.Var.Test_results.list)
  is.Equal.Var = Equal.Var.Test_combined.df$is.Equal.Var





  #============================================================================
  # Meandiff results
  #============================================================================
  p = Test___MeanDiff___Single.Response___ggstats(# data & variables
                                                  df,
                                                  var_group,
                                                  var_response,
                                                  # test results
                                                  alpha_ANOVA,
                                                  is.Normal,
                                                  is.Equal.Var,
                                                  # plotting options
                                                  results.subtitle = T,
                                                  pairwise.comparisons = T,
                                                  p.adjust.method,
                                                  title="")
  Mean.Diff.Results = Test___MeanDiff___Single.Response___Results.Extractor(p, var_group, var_response)



  #============================================================================
  # Boxplot & save
  #============================================================================
  if(is.null(Mean.Diff.Results$Post.Hoc)){
    if(Mean.Diff.Results[[1]]$MeanDiff_p.value <= alpha_ANOVA){
      p = Test___MeanDiff___Single.Response___Box.Plot(df, var_group, var_response, Mean.Diff.Results, alpha_ANOVA=alpha_ANOVA, palette = "jco", label.as.p.val=Boxplot_label.as.p.val)
      ggsave(filename = paste0(filename, ".png"), plot = p, path = save.path)
      cat("\n", crayon::red(paste0(filename, ".png")),  crayon::blue("is saved !"),"\n")
    }
  }else{
    is.significant_ANOVA = Mean.Diff.Results[[1]]$MeanDiff_p.value <= alpha_ANOVA
    is.significant_PostHoc = sum(unlist(Mean.Diff.Results[[2]]$PostHoc_p.value_adj) <= alpha_PostHoc) != 0
    if(is.significant_ANOVA & is.significant_PostHoc){
      p = Test___MeanDiff___Single.Response___Box.Plot(df, var_group, var_response, Mean.Diff.Results, alpha_PostHoc=alpha_PostHoc, palette = "jco", label.as.p.val = Boxplot_label.as.p.val)
      if(!is.null(filename) && !is.null(save.path)){
        ggsave(filename = paste0(filename, ".png"), plot = p, path = save.path)
        cat("\n", crayon::red(paste0(filename, ".png")),  crayon::blue("is saved !"),"\n")
      }
    }
  }



  #============================================================================
  # final results
  #============================================================================
  Final.list = Test___MeanDiff___Single.Response___Final.Results(save.path,
                                                                 filename,
                                                                 Norm.Test_combined.df,
                                                                 Equal.Var.Test_combined.df,
                                                                 Mean.Diff.Results)





  #============================================================================
  # Excluded group?
  #============================================================================
  if(length(excluded_group)>0){
    Final.list = c(Final.list, excluded_group)
    names(Final.list)[3] = "Excluded_Group (only having one subject)"
  }








  #============================================================================
  # export final results
  #============================================================================
  if(export.xlsx){
    Test___MeanDiff___Exporting.XLSX.Highlight(Final.df,
                                               Reporting.df,
                                               alpha_ANOVA,
                                               alpha_PostHoc,
                                               save.path,
                                               filename)
  }


  return(Final.list)
}
