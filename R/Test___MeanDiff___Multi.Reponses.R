Test___MeanDiff___Multi.Reponses = function(##############################
                                            # data & variables
                                            ##############################
                                            df, var_group, var_responses,
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
  dir.create(save.path, showWarnings = F)






  #==================================================================================
  var_group_filename = gsub(pattern = "/", replacement = ".", x = var_group)
  var_responses_filename = gsub(pattern = "/", replacement = ".", x = var_responses)







  #==================================================================================
  # ANOVA results for each groups
  #==================================================================================
  MeanDiff_Multi_Responses.list = lapply(seq_along(var_responses), FUN=function(k, ...){
    # k = 1
    ith_results = Test___MeanDiff___Single.Response(df,
                                                    var_group,
                                                    var_response = var_responses[k],
                                                    alpha_Norm, alpha_Equal.Var, alpha_ANOVA, alpha_PostHoc,
                                                    p.adjust.method,
                                                    save.path,
                                                    filename = paste0("[ANOVA_Boxplot] ", '`', var_responses_filename[k],"`"," by `", var_group_filename, "`"),
                                                    export.xlsx = F,
                                                    Boxplot_label.as.p.val = Boxplot_label.as.p.val)
    cat("\n",  crayon::blue("The response variable"), crayon::red(y), crayon::blue("is done!"), "\n")
    return(ith_results)
  })




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



  return(list(Combined_Final.df, Combined_Reporting.df))
}

#
#
# Test___MeanDiff_Multi = function(#################################################
#                                  # data & variabels
#                                  #################################################
#                                  dataset.df,
#                                  variables,
#                                  group_variables,
#                                  #################################################
#                                  # significance level
#                                  #################################################
#                                  norm_alpha=0.05,
#                                  anova_alpha ,
#                                  posthoc_alpha,
#                                  p.adjust.method="bonferroni",
#                                  #################################################
#                                  # path & file names
#                                  #################################################
#                                  group_filenames,
#                                  path,
#                                  file_name = "MeanDiff"){
#
#
#   #==============================================================================
#   # Meandiff results for each group variable
#   #==============================================================================
#
#   MeanDiff_results.list = lapply(group_variables, FUN=function(ith_group_variable, ...){
#     ind = which(ith_group_variable==group_variables)
#
#     ith_filename = group_filenames[ind]
#
#     Final.list = lapply(variables, FUN=function(v, ...){
#       title = paste(v, "by", g,sep=" ")
#       filename = paste(v, "_", f, ".png", sep="")
#       Test___MeanDiff(#################################################
#                       # dataset
#                       #################################################
#                       X                 =    dataset.df,
#                       group             =    ith_group_variable,
#                       variable          =    v,
#                       #################################################
#                       # significance level
#                       #################################################
#                       norm_alpha        =    norm_alpha,
#                       anova_alpha       =    anova_alpha,
#                       posthoc_alpha     =    posthoc_alpha,
#                       p.adjust.method   =    p.adjust.method,
#                       #################################################
#                       # filename
#                       #################################################
#                       title             =    title,
#                       path              =    path,
#                       filename          =    filename)
#     })
#
#
#
#     return(Final_results.df)
#   })
#
#
#
#
#   #################################################
#   # combining results
#   #################################################
#   for(i in 1:length(Final.list)){
#     if(i==1){
#       Final_results.df = Final.list[[i]]
#     }else{
#       Final_results.df = rrbind(Final_results.df, Final.list[[i]])
#     }
#   }
#
#
#   ### combining
#   for(i in 1:length(MeanDiff_results.list)){
#     if(i==1){
#       MeanDiff.df = MeanDiff_results.list[[i]]
#     }else{
#       MeanDiff.df = rrbind(MeanDiff.df, MeanDiff_results.list[[i]])
#     }
#   }
#
#
#
#
#
#   #==============================================================================
#   # Highlighting results & Exporting
#   #==============================================================================
#   ### highlighting
#   which_meandiff_sig = which(MeanDiff.df$MeanDiff_p.val <= anova_alpha)
#   which_posthoc_sig = which(MeanDiff.df$PostHoc_p.val <= posthoc_alpha)
#   coloring_index.list = c(rep(list(which_meandiff_sig),3),
#                           rep(list(which_posthoc_sig),3))
#   colors.list = c(rep(list("#F4FA58"), 3),
#                   rep(list("#FE9A2E"), 3))
#   which_cols.list = which_cols(MeanDiff.df, c("Group Name","Response", "MeanDiff_p.val",
#                                               "Group_1","Group_2","PostHoc_p.val")) %>% as.list
#   coloring_xlsx_cells(data.df = MeanDiff.df,
#                       colors.list = colors.list,
#                       which_cols.list = which_cols.list,
#                       coloring_index.list = coloring_index.list,
#                       save_path = path,
#                       file_name = file_name) %>% suppressWarnings
#
#
#
#   return(MeanDiff.df)
# }


