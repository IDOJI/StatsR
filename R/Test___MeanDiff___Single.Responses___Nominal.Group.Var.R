Test___MeanDiff___Single.Responses___Nominal.Group.Var = function(Data,
                                                                  Response_Vars,
                                                                  Group_Var,
                                                                  alpha_ANOVA = 0.05,
                                                                  is.Normal,
                                                                  is.Equal.Var,
                                                                  type = c("parametric", "nonparametric", "robust", "bayes"),
                                                                  # plot
                                                                  plot_title,
                                                                  outlier.tagging = FALSE,
                                                                  tr=0.2,
                                                                  filename = NULL,
                                                                  path_save = NULL){
  # 🟥 ggstats ###########################################################################
  type = match.arg(type)
  ggstats.list = lapply(seq_along(Response_Vars), function(k){
    Test___MeanDiff___Single.Responses___Nominal.Group.Var___ggstats(Data,
                                                                     Response_Var = Response_Vars[k],
                                                                     Group_Var,
                                                                     alpha_ANOVA,
                                                                     is.Normal = is.Normal[k],
                                                                     is.Equal.Var = is.Equal.Var[k],
                                                                     type,
                                                                     plot_title,
                                                                     outlier.tagging,
                                                                     tr)
  }) %>% setNames(Response_Vars)







  # 🟥 Extract Results ###########################################################################
  Extracted_Results.list = lapply(seq_along(ggstats.list), function(k){

    kth_ANOVA = Test___MeanDiff___Single.Responses___Nominal.Group.Var___Results.Extractor(p = ggstats.list[[k]]$plot_with_results,
                                                                                           Data,
                                                                                           Group_Var,
                                                                                           Response_Var = Response_Vars[k])
    kth_ANOVA$Used.Significance.Level = alpha_ANOVA
    kth_ANOVA$Group.Difference = kth_ANOVA$p.value < alpha_ANOVA
    return(kth_ANOVA)
  }) %>% setNames(Response_Vars)




  # 🟥 save boxplots ###########################################################################
  if(!is.null(path_save)){

    for(b in seq_along(Response_Vars)){

      file.name = paste0("[Boxplot] Results_", "`", Group_Var, "`___", "`", Response_Vars[b], "`")

      # plot_height, plot_width에 대한 옵션 넣어야 함
      dir.create(paste0(path_save, "/Boxplot"),showWarnings = F)
      ggsave(plot = ggstats.list[[b]]$plot_without_results,
             path = paste0(path_save, "/Boxplot"),
             filename = paste0(file.name, ".png"),
             bg = "white",
             width = plot_width,
             height = plot_height,
             units = plot_units,
             dpi = plot_dpi)
    }

    cat("\n", crayon::green("Exporting"), crayon::red("Boxplots"), crayon::green("is done!"),"\n")
  }



  # 🟥 return ###########################################################################
  list(Boxplots = ggstats.list, Results = Extracted_Results.list)
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

#
#
#
#   return(MeanDiff.Data)
# }


