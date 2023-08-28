Test___MeanDiff___Single.Responses___Nominal.Group.Var = function(Data,
                                                                  Response_Vars,
                                                                  Group_Var,
                                                                  alpha_ANOVA = 0.05,
                                                                  alpha_PostHoc = 0.05,
                                                                  p.adjust.method = c("Bonferroni", "Holm", "Hochberg", "SidakSS", "SidakSD", "BH", "BY","ABH","TSBH"),
                                                                  is.Normal,
                                                                  is.Equal.Var){
  #==================================================================================
  # file names
  #==================================================================================
  # var_group_filename = gsub(pattern = "/", replacement = ".", x = var_group)
  # var_responses_filename = gsub(pattern = "/", replacement = ".", x = var_responses)




  Test___MeanDiff___Single.Responses___Nominal.Group.Var



  return(list(Combined_Final.Data, Combined_Reporting.Data))





  }






#==================================================================================
# ANOVA results for each groups
#==================================================================================
# MeanDiff_Multi_Responses.list = lapply(seq_along(var_responses), FUN=function(k, ...){
#   ith_results = Test___MeanDiff___Single.Responses___Nominal.Group.Var(Data,
#                                                                        var_group,
#                                                                        var_response = var_responses[k],
#                                                                        alpha_Norm,
#                                                                        alpha_Equal.Var,
#                                                                        alpha_ANOVA,
#                                                                        alpha_PostHoc,
#                                                                        p.adjust.method,
#                                                                        save.path,
#                                                                        filename = paste0("[ANOVA_Boxplot] ", '`', var_responses_filename[k],"`"," by `", var_group_filename, "`"),
#                                                                        export.xlsx = T,
#                                                                        Boxplot_label.as.p.val = Boxplot_label.as.p.val)
#   cat("\n",  crayon::blue("The response variable"), crayon::red(var_responses[k]), crayon::blue("is done!"), "\n")
#   return(ith_results)
# })


#
#
# Test___MeanDiff_Multi = function(#################################################
#                                  # data & variabels
#                                  #################################################
#                                  dataset.Data,
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
#                       X                 =    dataset.Data,
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
#     return(Final_results.Data)
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
#       Final_results.Data = Final.list[[i]]
#     }else{
#       Final_results.Data = rrbind(Final_results.Data, Final.list[[i]])
#     }
#   }
#
#
#   ### combining
#   for(i in 1:length(MeanDiff_results.list)){
#     if(i==1){
#       MeanDiff.Data = MeanDiff_results.list[[i]]
#     }else{
#       MeanDiff.Data = rrbind(MeanDiff.Data, MeanDiff_results.list[[i]])
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
#   which_meandiff_sig = which(MeanDiff.Data$MeanDiff_p.val <= anova_alpha)
#   which_posthoc_sig = which(MeanDiff.Data$PostHoc_p.val <= posthoc_alpha)
#   coloring_index.list = c(rep(list(which_meandiff_sig),3),
#                           rep(list(which_posthoc_sig),3))
#   colors.list = c(rep(list("#F4FA58"), 3),
#                   rep(list("#FE9A2E"), 3))
#   which_cols.list = which_cols(MeanDiff.Data, c("Group Name","Response", "MeanDiff_p.val",
#                                               "Group_1","Group_2","PostHoc_p.val")) %>% as.list
#   coloring_xlsx_cells(data.Data = MeanDiff.Data,
#                       colors.list = colors.list,
#                       which_cols.list = which_cols.list,
#                       coloring_index.list = coloring_index.list,
#                       save_path = path,
#                       file_name = file_name) %>% suppressWarnings
#
#
#
#   return(MeanDiff.Data)
# }


