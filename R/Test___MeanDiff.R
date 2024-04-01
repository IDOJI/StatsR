Test___MeanDiff = function(# data & variables
                           Data,
                           Response_Vars,
                           Group_Var,
                           Group_Var_Type = c("Nominal", "Ordinal"),
                           # Significance level
                           alpha_Norm  = 0.05,
                           alpha_Equal.Var  = 0.05,
                           alpha_ANOVA = 0.05,
                           alpha_PostHoc = 0.05,
                           outlier_method = c("IQR"),
                           p.adjust.method = c("Bonferroni", "Holm", "Hochberg", "SidakSS", "SidakSD", "BH", "BY","ABH","TSBH"),
                           type = c("parametric", "nonparametric", "robust", "bayes"),
                           MANOVA = F,
                           # Figure
                           label.as.p.val=F,
                           group.comparison=F,
                           lines.connecting.medians=F,
                           plot_title="",
                           plot_width = 10,
                           plot_height = 7.5,
                           plot_units = "in",
                           plot_dpi = 200,
                           # results.subtitle=T,
                           # exporting
                           path_save = NULL,
                           export.separately=TRUE,
                           ...){
  # 🟥 packages ===================================================================
  install_packages = function(packages, load=TRUE) {
    # load : load the packages after installation?
    for(pkg in packages) {
      if (!require(pkg, character.only = TRUE)) {
        install.packages(pkg)
      }

      if(load){
        library(pkg, character.only = TRUE)
      }
    }
  }
  install_packages(c("ggpubr", "ggplot2", "ggstatsplot"))





  # 🟥 path ===================================================================
  if(!is.null(path_save)){
    dir.create(path_save, showWarnings = F)
  }





  # 🟥 Results list ===================================================================
  Results.list = list()



  # 🟥 type ===================================================================
  type = match.arg(type)





  # 🟥 1) Normality ===================================================================
  Results.list$Normality = Test___Normality(Data, Group_Var, Response_Vars, outlier_method, alpha = alpha_Norm)
  is.Normal = sapply(Results.list$Normality, function(x){
   x$Norm_Test_Result$is.normal
  })





  # 🟥 2) Homoscedasticity ===========================================================
  Results.list$Homoscedasticity = Test___Equal.Var(Data, Group_Var, Response_Vars, is.Normal, outlier_method, alpha = alpha_Equal.Var)
  is.Equal.Var = sapply(Results.list$Homoscedasticity, function(x) x[,3])





  # 🟥 3) ANOVA & Boxplot ===========================================================
  # Group var type
  Group_Var_Type = match.arg(Group_Var_Type)
  if(MANOVA){
    Results.list$ANOVA = Test___MeanDiff___Multi.Reponses(Data,
                                                          Response_Vars,
                                                          Group_Var,
                                                          Group_Var_Type,
                                                          alpha_ANOVA,
                                                          p.adjust.method,
                                                          is.Normal,
                                                          is.Equal.Var,
                                                          type,
                                                          plot_title="")
  }else{

    Results.list$ANOVA = lapply(Response_Vars, function(ith_Response_Var){
      # Consider only one response variable for each ANOVA
      Test___MeanDiff___Single.Responses(Data,
                                         ith_Response_Var,
                                         Group_Var,
                                         Group_Var_Type,
                                         alpha_ANOVA,
                                         p.adjust.method,
                                         is.Normal,
                                         is.Equal.Var,
                                         type,
                                         plot_title="")

    }) %>% setNames(Response_Vars)

  }
  cat("\n", crayon::green("Testing"), crayon::red("Mean Differece"), crayon::green("is done!"),"\n")






  # 🟥 4) Combine results ===========================================================
  if(! 3 * length(Response_Vars) == length(Results.list$Normality) + length(Results.list$Homoscedasticity) + length(Results.list$ANOVA)){
    stop("There is a variable which is not done yet")
  }

  # Combine by each variable for all results
  Combined_Results.list = lapply(seq_along(Response_Vars), function(k){

    kth_Norm = Results.list$Normality[[k]]
    kth_Homo = Results.list$Homoscedasticity[[k]]
    kth_ANOVA = Results.list$ANOVA[[k]][[1]]


    kth_plots = list(Normality = kth_Norm$Norm_Plots, Boxplot = kth_ANOVA$Boxplot)
    kth_results = ccbind(X = kth_Norm$Norm_Test_Result$Norm_results, Y = kth_Homo) %>%
      ccbind(., kth_ANOVA$Result) %>%
      ccbind(data.frame(Response_Var = Response_Vars[k], Group_Var = Group_Var), .)

    kth_results_ReplaceNA = kth_results %>% dplyr::mutate_all(~ifelse(is.na(.), "", .))


    list(plots = kth_plots, results = kth_results, results_kable = kth_results_ReplaceNA)

  }) %>% setNames(Response_Vars)





  # 🟥 5) Export ANOVA Results as data frame ===========================================================
  # if(!is.null(path_save)){
  #   file.name = paste0("[ANOVA] Results_", "`", Group_Var, "`")
  #
  #   # save a combined df
  #   # Test___MeanDiff___Export.xlsx.Highlight(Combined_Results.df,
  #   #                                         path_save,
  #   #                                         file.name,
  #   #                                         Group_Var_Type)
  #   cat("\n", crayon::green("Exporting"), crayon::red("Mean Difference Results"), crayon::green("is done!"),"\n")
  # }




  # 🟥 7) Return ===========================================================
  cat("\n", crayon::bgCyan("Analaysis is done!"),"\n")
  return(Combined_Results.list)
}


#==================================================================================
# 6) Save combined result
#==================================================================================
# if(!is.null(path_save)){
#   file.name = paste0("[ANOVA] Combined_Results_", "`", Group_Var, "`")
#
#   # save RDS file
#   saveRDS(object = Combined_Results.list, file = paste0(path_save, "/", file.name, ".rds"))
#
#   cat("\n", crayon::green("Exporting"), crayon::red("Combined Results"), crayon::green("is done!"),"\n")
# }


# # Combining tables for LaTeX
# Combined.list = lapply(list.files(path_save, pattern = "@_Combined Results for Latex Table", full.names=T), read.csv)
# First_Cols = Combined.list[[1]][,1:3]
# Second_Cols = lapply(Combined.list, function(x) x[,-c(1:3)])
# Combined_New.list = c(First_Cols, Second_Cols)
# Combined_New.df = do.call(cbind, Combined_New.list)
# Which_rows_to_highlight = apply(Combined_New.df, 1, function(x){
#   x = Combined_New.df[1,] %>% unlist
#   only_having_ns = sum(x[-c(1:3)] %in% c("none", "HNS", "NS")) == length(x)
#   return(!only_having_ns)
# }) %>% which
# Export___xlsx___Highlighting(Combined_New.df, colors.list = "red", which_cols.list = 1:ncol(Combined_New.df), coloring_index.list = Which_rows_to_highlight, path_save = path_save, file.name = "@@_Combined Results for Latex Table", sheet.name = "")
# write.csv(Combined_New.df, paste0(path_save, "/@@_Combined Results for Latex Table", ".csv"), row.names=F)
#




# Combining p.values
# Combined.list = lapply(list.files(path_save, pattern = "@_Only p.values Combined Results", full.names=T), read.csv)
# write.csv(do.call(rbind, Combined.list), paste0(path_save, "/@@_Only p.values Combined Results", ".csv"), row.names=F)








#==================================================================================
# 5) Boxplot
#==================================================================================
# Boxplot.list = lapply(Results_ANOVA, function(ith_Results){
#   Test___MeanDiff___Single.Responses___Box.Plot(Data,
#                                                 ith_Results,
#                                                 label.as.p.val,
#                                                 group.comparison,
#                                                 lines.connecting.medians,
#                                                 path_save)
# })
